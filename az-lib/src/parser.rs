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
        use htmlparser::{ElementEnd, StrSpan, Token, Tokenizer};
        fn find_map_token<'t, T>(
            tokens: &mut Tokenizer<'t>,
            mut map: impl FnMut(Token<'t>) -> Option<T>,
        ) -> Result<Option<T>, htmlparser::Error> {
            tokens
                .find_map(|res| res.map(&mut map).transpose())
                .transpose()
        }

        struct StartTag<'a> {
            span: StrSpan<'a>,
            prefix: StrSpan<'a>,
            local: StrSpan<'a>,
            is_closed: bool,
        }

        impl StartTag<'_> {
            fn new(token: Token<'_>) -> Option<StartTag<'_>> {
                match token {
                    Token::ElementStart {
                        prefix,
                        local,
                        span,
                    } => Some(StartTag {
                        span,
                        prefix,
                        local,
                        is_closed: false,
                    }),
                    Token::ElementEnd {
                        end: ElementEnd::Close(prefix, local),
                        span,
                    } => Some(StartTag {
                        span,
                        prefix,
                        local,
                        is_closed: true,
                    }),
                    _ => None,
                }
            }
        }

        type ProcessorFn =
            for<'p> fn(&'p mut (dyn Processor + 'static), Tag<'_>) -> Option<Box<dyn Visitor + 'p>>;

        let mut tokens = htmlparser::Tokenizer::from_fragment(input, 0..input.len());
        let mut last = 0;

        while let Some(start_tag) = find_map_token(&mut tokens, StartTag::new)? {
            let input_start = start_tag.span.start();
            self.output.buffer.push_str(&input[last..input_start]);
            let (input_end, filter): (_, ProcessorFn) = if start_tag.is_closed {
                (start_tag.span.end(), Processor::end_tag)
            } else {
                find_map_token::<(_, ProcessorFn)>(&mut tokens, |tok| match tok {
                    Token::ElementEnd {
                        end: ElementEnd::Open,
                        span,
                    } => Some((span.end(), Processor::start_tag)),
                    Token::ElementEnd {
                        end: ElementEnd::Empty,
                        span,
                    } => Some((span.end(), Processor::empty_tag)),
                    _ => None,
                })?
                .expect("TODO: return an error for no element end")
            };
            let start = self.output.len();
            self.output.buffer.push_str(&input[input_start..input_end]);
            let end = self.output.len();
            last = input_end;
            let span = Span { start, end };
            let span_ref = self.output.spans.insert(span);
            self.processors.iter_mut().find_map(|p| {
                filter(
                    &mut **p,
                    Tag {
                        contents: &self.output[span],
                        prefix: &start_tag.prefix,
                        local: &start_tag.local,
                        span: span_ref,
                    },
                )
                .map(|v| v.visit(&mut self.output))
            });
        }
        Ok(())
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
        let old_len = self.replace_span.len();
        let new_len = self.buffer.len();
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

#[derive(Clone, Copy, Debug)]
pub struct Tag<'a> {
    pub contents: &'a str,
    pub prefix: &'a str,
    pub local: &'a str,
    pub span: SpanRef,
}

pub trait Processor {
    fn start_tag(&mut self, tag: Tag<'_>) -> Option<Box<dyn Visitor + '_>>;

    fn end_tag(&mut self, tag: Tag<'_>) -> Option<Box<dyn Visitor + '_>>;

    fn empty_tag(&mut self, tag: Tag<'_>) -> Option<Box<dyn Visitor + '_>>;
}

#[cfg(test)]
mod tests {
    #[test]
    fn footnotes() {
        let footnote_processor = Box::new(super::footnote::FootnoteProcessor::new());
        let mut driver = super::ProcessorDriver::new(vec![footnote_processor]);
        driver
            .parse(
                r#"<az:footnote></az:footnote>
<az:footnote-ref name="foo" />
<az:footnote name="foo"></az:footnote>"#,
            )
            .unwrap();
        assert_eq!(
            driver.output.buffer,
            r##"<a id="footnote-ref-0" href="#footnote-0" class="footnote-ref ">0</a>
<a id="footnote-ref-foo-1" href="#footnote-foo" class="footnote-ref ">1</a>
<a id="footnote-ref-foo-2" href="#footnote-foo" class="footnote-ref ">1</a>"##
        );
    }
}
