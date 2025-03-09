use std::{
    fmt,
    ops::{self, RangeBounds},
};

slotmap::new_key_type! {
    pub struct SpanRef;
}

pub mod footnote;

pub const PREFIX: &str = "az";

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn len(self) -> usize {
        self.end - self.start
    }

    pub fn join(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl RangeBounds<usize> for Span {
    fn start_bound(&self) -> ops::Bound<&usize> {
        ops::Bound::Included(&self.start)
    }

    fn end_bound(&self) -> ops::Bound<&usize> {
        ops::Bound::Excluded(&self.end)
    }
}

pub struct ProcessorDriver {
    output: Buffer,
    processors: Vec<Box<dyn Processor>>,
}

impl ProcessorDriver {
    pub fn new(processors: Vec<Box<dyn Processor>>) -> Self {
        Self {
            processors,
            output: Default::default(),
        }
    }

    pub fn parse(&mut self, input: &str) -> Result<(), Error> {
        use htmlparser::{ElementEnd, Token, Tokenizer};
        fn find_map_token<'t, T>(
            tokens: &mut Tokenizer<'t>,
            mut map: impl FnMut(Token<'t>) -> Option<T>,
        ) -> Result<Option<T>, htmlparser::Error> {
            tokens
                .find_map(|res| res.map(&mut map).transpose())
                .transpose()
        }

        let mut tokens = htmlparser::Tokenizer::from_fragment(input, 0..input.len());
        let mut last = 0;
        while let Some(input_start) = find_map_token(&mut tokens, |tok| match tok {
            Token::ElementStart { span, .. } => Some(span.start()),
            _ => None,
        })? {
            self.output.buffer.push_str(&input[last..input_start]);
            let (input_end, kind) = find_map_token(&mut tokens, |tok| match tok {
                Token::ElementEnd { end, span } => Some((span.end(), end)),
                _ => None,
            })?
            .expect("TODO: return an error for no element end");
            let start = self.output.len();
            self.output.buffer.push_str(&input[input_start..input_end]);
            let end = self.output.len();
            last = input_end;
            let span = Span { start, end };
            match kind {
                ElementEnd::Open => self.handle_tag(span, Processor::start_tag),
                ElementEnd::Close(..) => self.handle_tag(span, Processor::end_tag),
                ElementEnd::Empty => todo!(),
            }
        }
        Ok(())
    }

    #[allow(clippy::type_complexity)]
    fn handle_tag(
        &mut self,
        span: Span,
        filter: for<'p> fn(
            &'p mut (dyn Processor + 'static),
            &str,
            SpanRef,
        ) -> Option<Box<dyn Visitor + 'p>>,
    ) {
        let span_ref = self.output.spans.insert(span);
        self.processors.iter_mut().find_map(|p| {
            filter(&mut **p, &self.output[span], span_ref).map(|v| v.visit(&mut self.output))
        });
    }
}

// TODO: replace this with our own error type
pub type Error = htmlparser::Error;

#[derive(Clone, Default, Debug)]
pub struct Buffer {
    buffer: String,
    spans: slotmap::HopSlotMap<SpanRef, Span>,
}

impl Buffer {
    pub fn replace_span(&mut self, span: SpanRef) -> Option<InsertWriter<'_>> {
        Some(InsertWriter {
            replace_span: *self.spans.get(span)?,
            output: self,
            buffer: String::new(),
        })
    }

    pub fn join_spans(&mut self, first: SpanRef, second: SpanRef) -> Option<SpanRef> {
        let first = self.spans.get(first)?;
        let second = self.spans.get(second)?;
        let joined = first.join(*second);
        Some(self.spans.insert(joined))
    }

    pub fn outer_and_inner_spans(
        &mut self,
        first: SpanRef,
        second: SpanRef,
    ) -> Option<(SpanRef, SpanRef)> {
        let first = self.spans.get(first)?;
        let second = self.spans.get(second)?;
        let outer = Span {
            start: first.start,
            end: second.end,
        };
        let inner = Span {
            start: first.end,
            end: second.start,
        };
        Some((self.spans.insert(outer), self.spans.insert(inner)))
    }

    fn len(&self) -> usize {
        self.buffer.len()
    }
}

impl fmt::Write for Buffer {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buffer.push_str(s);
        Ok(())
    }
}

impl ops::Index<Span> for Buffer {
    type Output = str;

    fn index(&self, span: Span) -> &Self::Output {
        &self.buffer[span.start..span.end]
    }
}

impl ops::Index<SpanRef> for Buffer {
    type Output = str;

    fn index(&self, span: SpanRef) -> &Self::Output {
        let span = self.spans[span];
        &self[span]
    }
}

pub struct InsertWriter<'a> {
    buffer: String,
    output: &'a mut Buffer,
    replace_span: Span,
}

impl Drop for InsertWriter<'_> {
    fn drop(&mut self) {
        let old_len = self.buffer.len();
        let new_len = self.replace_span.len();
        if old_len != new_len {
            self.output.spans.retain(|_, span| {
                if self.replace_span.contains(&span.start) || self.replace_span.contains(&span.end)
                {
                    return false;
                }
                if span.start >= self.replace_span.end {
                    span.start -= old_len;
                    span.start += new_len;
                }
                if span.end >= self.replace_span.end {
                    span.end -= old_len;
                    span.end += new_len;
                }
                true
            });
        }
        self.output
            .buffer
            .replace_range(self.replace_span, &self.buffer);
    }
}

impl fmt::Write for InsertWriter<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buffer.push_str(s);
        Ok(())
    }
}

pub trait Visitor {
    fn visit(self: Box<Self>, output: &mut Buffer);
}

pub trait Processor {
    fn start_tag(&mut self, tag_contents: &str, tag_span: SpanRef)
        -> Option<Box<dyn Visitor + '_>>;

    fn end_tag(&mut self, tag_contents: &str, tag_span: SpanRef) -> Option<Box<dyn Visitor + '_>>;
}
