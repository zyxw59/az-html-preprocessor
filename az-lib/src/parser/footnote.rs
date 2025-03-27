use std::{collections::BTreeMap, fmt, rc::Rc};

use htmlparser::Token;

use super::{Buffer, PREFIX, Processor, SpanRef, Tag, Visitor};

const NOTE_TAG: &str = "footnote";
const REF_TAG: &str = "footnote-ref";

const EXPECT_SPAN: &str = "span should be valid";
const EXPECT_WRITE: &str = "writing to buffer should not fail";

#[derive(Default, Debug)]
pub struct FootnoteProcessor {
    next_index: usize,
    stack: Vec<Pending>,
    footnotes: BTreeMap<usize, Footnote>,
    by_name: BTreeMap<Rc<str>, Metadata>,
}

impl FootnoteProcessor {
    pub fn new() -> Self {
        Default::default()
    }

    fn metadata(&mut self, name: Option<Rc<str>>) -> Metadata {
        if let Some(name) = name {
            let refcount = self.by_name.entry(name).or_insert_with(|| {
                let index = self.next_index;
                self.next_index += 1;
                Metadata { index, refcount: 0 }
            });
            refcount.refcount += 1;
            *refcount
        } else {
            let index = self.next_index;
            self.next_index += 1;
            Metadata { index, refcount: 0 }
        }
    }

    fn finish_inner(self, output: &mut Buffer) -> fmt::Result {
        use std::fmt::Write;

        writeln!(output, r#"<footer><ol class="footnote-container">"#)?;
        let mut last = 0;
        for (idx, footnote) in self.footnotes {
            write!(output, r#"<li id="footnote-{}""#, footnote.id())?;
            if idx != last + 1 {
                write!(output, r#" value="{idx}""#)?;
            }
            write!(output, r#" class="footnote {}">"#, footnote.class)?;
            write!(output, "{}", footnote.text)?;
            if let Some(name) = footnote.name {
                let refcount = self
                    .by_name
                    .get(&name)
                    .map(|metadata| metadata.refcount)
                    .unwrap_or(0);
                for i in 0..refcount {
                    write!(
                        output,
                        r##"<a href="#footnote-ref-{name}-{i}" class="footnote-return-link"></a>"##
                    )?;
                }
            } else {
                write!(
                    output,
                    r##"<a href="#footnote-ref-{}" class="footnote-return-link"></a>"##,
                    footnote.index,
                )?;
            }
            writeln!(output, "</li>")?;
            last = idx;
        }
        writeln!(output, r#"</ol></footer>"#)?;
        Ok(())
    }
}

impl Processor for FootnoteProcessor {
    fn start_tag(&mut self, tag: Tag) -> Option<Box<dyn Visitor + '_>> {
        if tag.prefix != PREFIX || tag.local != NOTE_TAG {
            return None;
        }
        let mut tokens = htmlparser::Tokenizer::from_fragment(tag.contents, 0..tag.contents.len());
        // skip the start tag token
        tokens.next();
        let mut name: Option<Rc<str>> = None;
        let mut class = Vec::new();
        let mut ref_class = Vec::new();
        while let Some(Ok(Token::Attribute { local, value, .. })) = tokens.next() {
            match (local.as_str(), value) {
                ("name", Some(value)) => name = Some(value.as_str().into()),
                ("class", Some(value)) => class.push(value.as_str()),
                ("ref-class", Some(value)) => ref_class.push(value.as_str()),
                // TODO: warn on unused attributes?
                _ => {}
            }
        }
        let metadata = self.metadata(name.clone());
        let footnote = Pending {
            opening_span: tag.span,
            name,
            index: metadata.index,
            ref_number: metadata.refcount,
            class: class.join(" ").into(),
            ref_class: ref_class.join(" ").into(),
        };
        self.stack.push(footnote);
        // this processor doesn't modify output on start tags
        None
    }

    fn end_tag(&mut self, tag: Tag) -> Option<Box<dyn Visitor + '_>> {
        if tag.prefix != PREFIX || tag.local != NOTE_TAG {
            return None;
        }
        let pending = self.stack.pop()?;
        Some(Box::new(EndVisitor {
            processor: self,
            pending,
            end_span: tag.span,
        }))
    }

    fn empty_tag(&mut self, tag: Tag) -> Option<Box<dyn Visitor + '_>> {
        if tag.prefix != PREFIX || tag.local != REF_TAG {
            return None;
        }
        let mut tokens = htmlparser::Tokenizer::from_fragment(tag.contents, 0..tag.contents.len());
        // skip the start tag token
        tokens.next();
        let mut name: Option<Rc<str>> = None;
        let mut ref_class = Vec::new();
        while let Some(Ok(Token::Attribute { local, value, .. })) = tokens.next() {
            match (local.as_str(), value) {
                ("name", Some(value)) => name = Some(value.as_str().into()),
                // TODO: do we want to use `class`, `ref-class`, or both for footnote refs?
                ("class", Some(value)) => ref_class.push(value.as_str()),
                ("ref-class", Some(value)) => ref_class.push(value.as_str()),
                // TODO: warn on unused attributes?
                _ => {}
            }
        }
        let metadata = self.metadata(name.clone());

        Some(Box::new(RefVisitor {
            footnote_ref: FootnoteRef {
                name_and_ref_number: name.map(|name| (name, metadata.refcount)),
                index: metadata.index,
                class: ref_class.join(" ").into(),
            },
            tag_span: tag.span,
        }))
    }

    fn finish(self: Box<Self>, output: &mut Buffer) {
        self.finish_inner(output).expect(EXPECT_WRITE);
    }
}

struct EndVisitor<'a> {
    processor: &'a mut FootnoteProcessor,
    pending: Pending,
    end_span: SpanRef,
}

impl Visitor for EndVisitor<'_> {
    fn visit(self: Box<Self>, output: &mut Buffer) {
        let Some((outer, inner)) =
            output.outer_and_inner_spans(self.pending.opening_span, self.end_span)
        else {
            // TODO: error handling
            return;
        };
        let text = output[inner].into();
        let footnote = Footnote {
            name: self.pending.name.clone(),
            index: self.pending.index,
            class: self.pending.class,
            text,
        };
        self.processor
            .footnotes
            .insert(self.pending.index, footnote);
        let footnote_ref = FootnoteRef {
            name_and_ref_number: self
                .pending
                .name
                .map(|name| (name, self.pending.ref_number)),
            index: self.pending.index,
            class: self.pending.ref_class,
        };
        use std::fmt::Write;
        write!(
            output.replace_span(outer).expect(EXPECT_SPAN),
            "{footnote_ref}"
        )
        .expect(EXPECT_WRITE);
    }
}

struct RefVisitor {
    footnote_ref: FootnoteRef,
    tag_span: SpanRef,
}

impl Visitor for RefVisitor {
    fn visit(self: Box<Self>, output: &mut Buffer) {
        use std::fmt::Write;
        write!(
            output.replace_span(self.tag_span).expect(EXPECT_SPAN),
            "{}",
            self.footnote_ref
        )
        .expect(EXPECT_WRITE);
    }
}

#[derive(Debug)]
struct Pending {
    opening_span: SpanRef,
    name: Option<Rc<str>>,
    index: usize,
    ref_number: usize,
    class: Box<str>,
    ref_class: Box<str>,
}

#[derive(Debug)]
struct Footnote {
    name: Option<Rc<str>>,
    index: usize,
    class: Box<str>,
    text: Box<str>,
}

impl Footnote {
    fn id(&self) -> &dyn fmt::Display {
        if let Some(name) = &self.name {
            name
        } else {
            &self.index
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Metadata {
    index: usize,
    refcount: usize,
}

#[derive(Debug)]
struct FootnoteRef {
    name_and_ref_number: Option<(Rc<str>, usize)>,
    index: usize,
    class: Box<str>,
}

impl fmt::Display for FootnoteRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("<a ")?;
        if let Some((name, ref_number)) = &self.name_and_ref_number {
            write!(f, r#"id="footnote-ref-{name}-{ref_number}" "#)?;
            write!(f, r##"href="#footnote-{name}" "##)?;
        } else {
            write!(f, r#"id="footnote-ref-{}" "#, self.index)?;
            write!(f, r##"href="#footnote-{}" "##, self.index)?;
        }
        write!(
            f,
            r#"class="footnote-ref {}">{}</a>"#,
            self.class, self.index
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{
        super::{Buffer, PREFIX, Processor, Span, Tag},
        FootnoteProcessor, NOTE_TAG,
    };

    #[test]
    fn awawa() {
        const EMPTY_SPAN: Span = Span { start: 0, end: 0 };
        let mut buffer = Buffer::default();
        let mut processor = FootnoteProcessor::new();
        let empty_span = buffer.spans.insert(EMPTY_SPAN);

        let start_tag = Tag {
            contents: "<az:footnote>",
            prefix: PREFIX,
            local: NOTE_TAG,
            span: empty_span,
        };
        assert!(processor.start_tag(start_tag).is_none());
        assert_eq!(processor.stack.len(), 1);
        let end_tag = Tag {
            contents: "</az:footnote>",
            prefix: PREFIX,
            local: NOTE_TAG,
            span: empty_span,
        };
        let visitor = processor.end_tag(end_tag).unwrap();
        visitor.visit(&mut buffer);
        assert_eq!(
            buffer.buffer,
            r##"<a id="footnote-ref-0" href="#footnote-0" class="footnote-ref ">0</a>"##
        );
    }
}
