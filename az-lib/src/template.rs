use std::{collections::BTreeMap, rc::Rc};

use htmlparser::Token;

use crate::parser::{Buffer, PREFIX, Processor, SpanRef, Tag, Visitor};

const TEMPLATE_TAG: &str = "template";

#[derive(Default, Debug)]
pub struct TemplateProcessor {
    templates: BTreeMap<Box<str>, (Rc<str>, Rc<str>)>,
    stack: Vec<Rc<str>>,
}

impl Processor for TemplateProcessor {
    fn start_tag(&mut self, tag: Tag) -> Option<Box<dyn Visitor + '_>> {
        if tag.prefix != PREFIX || tag.local != TEMPLATE_TAG {
            return None;
        }
        let mut tokens = htmlparser::Tokenizer::from_fragment(tag.contents, 0..tag.contents.len());
        // skip the start tag token
        tokens.next();
        let Some(name) = tokens.find_map(|res| {
            if let Ok(Token::Attribute {
                local,
                value: Some(value),
                ..
            }) = res
                && local.as_str() == "name"
            {
                Some(value.as_str())
            } else {
                None
            }
        }) else {
            // TODO: error on missing name
            return None;
        };
        let Some((pre, post)) = self.templates.get(name) else {
            // TODO: error on unknown templates
            return None;
        };
        self.stack.push(Rc::clone(post));
        Some(Box::new(TemplateVisitor {
            span: tag.span,
            replacement: Rc::clone(pre),
        }))
    }

    fn end_tag(&mut self, tag: Tag) -> Option<Box<dyn Visitor + '_>> {
        if tag.prefix != PREFIX || tag.local != TEMPLATE_TAG {
            return None;
        }
        let replacement = self.stack.pop()?;
        Some(Box::new(TemplateVisitor {
            span: tag.span,
            replacement,
        }))
    }

    fn empty_tag(&mut self, _tag: Tag) -> Option<Box<dyn Visitor + '_>> {
        None
    }

    fn finish(self: Box<Self>, _output: &mut Buffer) {}
}

struct TemplateVisitor {
    span: SpanRef,
    replacement: Rc<str>,
}

impl Visitor for TemplateVisitor {
    fn visit(self: Box<Self>, output: &mut Buffer) {
        output.replace_span(self.span);
    }

    fn add_input(&self) -> Option<Rc<str>> {
        Some(Rc::clone(&self.replacement))
    }
}
