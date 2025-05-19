use std::collections::HashMap;

use htmlparser::Token;

use crate::parser::{Buffer, PREFIX, Processor, SpanRef, Tag, Visitor};

pub type FileMetadata = HashMap<Box<str>, Vec<Box<str>>>;

const META_TAG: &str = "meta";

#[derive(Debug)]
pub struct MetadataProcessor<'a> {
    pub values: &'a mut FileMetadata,
    stack: Vec<Pending>,
}

impl<'a> MetadataProcessor<'a> {
    pub fn new(values: &'a mut FileMetadata) -> Self {
        Self {
            values,
            stack: Vec::new(),
        }
    }
}

impl Processor for MetadataProcessor<'_> {
    fn start_tag(&mut self, tag: Tag<'_>) -> Option<Box<dyn Visitor + '_>> {
        if tag.prefix != PREFIX || tag.local != META_TAG {
            return None;
        }
        let mut tokens = htmlparser::Tokenizer::from_fragment(tag.contents, 0..tag.contents.len());
        // skip the start tag token
        tokens.next();
        while let Some(Ok(Token::Attribute { local, value, .. })) = tokens.next() {
            if local == "key" {
                if let Some(value) = value {
                    self.stack.push(Pending {
                        opening_span: tag.span,
                        key: value.as_str().into(),
                    });
                    break;
                }
            }
        }
        // this processor doesn't modify output on start tags
        None
    }

    fn end_tag(&mut self, tag: Tag<'_>) -> Option<Box<dyn Visitor + '_>> {
        if tag.prefix != PREFIX || tag.local != META_TAG {
            return None;
        }
        let pending = self.stack.pop()?;
        Some(Box::new(MetadataVisitor {
            values: self.values,
            pending,
            end_span: tag.span,
        }))
    }

    fn empty_tag(&mut self, _tag: Tag<'_>) -> Option<Box<dyn Visitor + '_>> {
        None
    }

    fn finish(self: Box<Self>, _output: &mut Buffer) {}
}

#[derive(Debug)]
struct Pending {
    opening_span: SpanRef,
    key: Box<str>,
}

struct MetadataVisitor<'a> {
    values: &'a mut FileMetadata,
    pending: Pending,
    end_span: SpanRef,
}

impl Visitor for MetadataVisitor<'_> {
    fn visit(self: Box<Self>, output: &mut Buffer) {
        let Some((outer, inner)) =
            output.outer_and_inner_spans(self.pending.opening_span, self.end_span)
        else {
            // TODO: error handling
            return;
        };
        self.values
            .entry(self.pending.key)
            .or_default()
            .push(output[inner].into());
        output.replace_span(outer);
    }
}
