use std::{collections::BTreeMap, fmt, rc::Rc};

use htmlparser::Token;

use super::{Buffer, Processor, SpanRef, Visitor, PREFIX};

const NOTE_TAG: &str = "footnote";
const REF_TAG: &str = "footnote-ref";

pub struct FootnoteProcessor {
    stack: Vec<FootnotePending>,
    footnotes: BTreeMap<usize, Footnote>,
    ref_counts: BTreeMap<Rc<str>, usize>,
}

impl FootnoteProcessor {
    fn is_element_start(token: Token, tag_name: &str) -> bool {
        matches!(token, Token::ElementStart { prefix, local, .. } if prefix == PREFIX && local == tag_name)
    }

    fn next_index(&self) -> usize {
        self.stack.len() + self.footnotes.len() + 1
    }
}

impl Processor for FootnoteProcessor {
    fn start_tag(
        &mut self,
        tag_contents: &str,
        opening_span: SpanRef,
    ) -> Option<Box<dyn Visitor + '_>> {
        let mut tokens = htmlparser::Tokenizer::from_fragment(tag_contents, 0..tag_contents.len());
        if !Self::is_element_start(tokens.next()?.ok()?, NOTE_TAG) {
            return None;
        }
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
        let footnote = FootnotePending {
            opening_span,
            name,
            class: class.join(" ").into(),
            ref_class: ref_class.join(" ").into(),
            index: self.next_index(),
        };
        self.stack.push(footnote);
        // this processor doesn't modify output on start tags
        None
    }

    fn end_tag(&mut self, tag_contents: &str, end_span: SpanRef) -> Option<Box<dyn Visitor + '_>> {
        let mut tokens = htmlparser::Tokenizer::from_fragment(tag_contents, 0..tag_contents.len());
        if !Self::is_element_start(tokens.next()?.ok()?, NOTE_TAG) {
            return None;
        }
        let pending = self.stack.pop()?;
        Some(Box::new(EndVisitor {
            processor: self,
            pending,
            end_span,
        }))
    }
}

struct EndVisitor<'a> {
    processor: &'a mut FootnoteProcessor,
    pending: FootnotePending,
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
        let index_or_refcount = if let Some(ref name) = self.pending.name {
            let refcount = self
                .processor
                .ref_counts
                .entry(Rc::clone(name))
                .or_default();
            *refcount += 1;
            *refcount
        } else {
            self.pending.index
        };
        let footnote = Footnote {
            name: self.pending.name.clone(),
            index: self.pending.index,
            class: self.pending.class,
            text,
        };
        self.processor.footnotes.insert(self.pending.index, footnote);
        let footnote_ref = FootnoteRef {
            name: self.pending.name.as_deref(),
            index_or_refcount,
            class: &self.pending.ref_class,
        };
        use std::fmt::Write;
        write!(
            output.replace_span(outer).expect("span should be valid"),
            "{footnote_ref}"
        ).expect("writing footnote ref should not fail");
    }
}

struct FootnotePending {
    opening_span: SpanRef,
    name: Option<Rc<str>>,
    index: usize,
    class: Box<str>,
    ref_class: Box<str>,
}

struct Footnote {
    name: Option<Rc<str>>,
    index: usize,
    class: Box<str>,
    text: Box<str>,
}

#[derive(Clone, Copy, Debug)]
struct FootnoteRef<'a> {
    name: Option<&'a str>,
    index_or_refcount: usize,
    class: &'a str,
}

impl fmt::Display for FootnoteRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("<a ")?;
        if let Some(name) = self.name {
            write!(f, r#"id="footnote-ref-{name}-{}" "#, self.index_or_refcount)?;
            write!(f, r##"href="#footnote-{name}" "##)?;
        } else {
            write!(f, r#"id="footnote-ref-{}" "#, self.index_or_refcount)?;
            write!(f, r##"href="#footnote-{}" "##, self.index_or_refcount)?;
        }
        write!(f, r#"class="footnote-ref {}" />"#, self.class)?;
        Ok(())
    }
}
