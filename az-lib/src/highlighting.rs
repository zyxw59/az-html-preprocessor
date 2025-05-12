use std::collections::HashMap;

use htmlparser::Token;
use itertools::Itertools;
use tree_sitter_highlight::{Highlight, HighlightConfiguration, HighlightEvent, Highlighter};

use crate::parser::{Buffer, Processor, SpanRef, Tag, Visitor};

const CODE_TAG: &str = "code";

const EXPECT_SPAN: &str = "span should be valid";
const EXPECT_WRITE: &str = "writing to buffer should not fail";

pub struct HighlightProcessor {
    highlighter: Highlighter,
    highlight_names: Box<[Box<str>]>,
    configs: HashMap<Box<str>, HighlightConfiguration>,
    stack: Vec<Pending>,
}

#[cfg(test)]
impl HighlightProcessor {
    pub fn new_test() -> Self {
        let highlight_names = [
            "attribute",
            "comment",
            "constant",
            "constructor",
            "embedded",
            "function",
            "function.macro",
            "keyword",
            "module",
            "number",
            "operator",
            "property",
            "string",
            "variable",
            "variable.builtin",
            "variable.parameter",
        ]
        .into_iter()
        .map(Into::into)
        .collect::<Box<[_]>>();
        let configs = [(
            "rust",
            tree_sitter_rust::LANGUAGE,
            tree_sitter_rust::HIGHLIGHTS_QUERY,
            tree_sitter_rust::INJECTIONS_QUERY,
            "",
        )]
        .into_iter()
        .map(|(name, language, highlights, injections, locals)| {
            let mut config =
                HighlightConfiguration::new(language.into(), name, highlights, injections, locals)
                    .unwrap();
            config.configure(&highlight_names);
            (name.into(), config)
        })
        .collect();
        Self {
            highlighter: Highlighter::new(),
            highlight_names,
            configs,
            stack: Vec::new(),
        }
    }
}

impl Processor for HighlightProcessor {
    fn start_tag(&mut self, tag: Tag) -> Option<Box<dyn Visitor + '_>> {
        if !tag.prefix.is_empty() || tag.local != CODE_TAG {
            return None;
        }
        let mut tokens = htmlparser::Tokenizer::from_fragment(tag.contents, 0..tag.contents.len());
        // skip the start tag token
        tokens.next();
        while let Some(Ok(Token::Attribute { local, value, .. })) = tokens.next() {
            if local == "data-lang" {
                if let Some(value) = value {
                    self.stack.push(Pending {
                        start_span: tag.span,
                        lang: value.as_str().into(),
                    });
                    break;
                }
            }
        }
        // this processor doesn't modify output on start tags
        None
    }

    fn end_tag(&mut self, tag: Tag) -> Option<Box<dyn Visitor + '_>> {
        if !tag.prefix.is_empty() || tag.local != CODE_TAG {
            return None;
        }
        let pending = self.stack.pop()?;
        Some(Box::new(HighlightVisitor {
            processor: self,
            pending,
            end_span: tag.span,
        }))
    }

    fn empty_tag(&mut self, _tag: Tag) -> Option<Box<dyn Visitor + '_>> {
        None
    }

    fn finish(self: Box<Self>, _output: &mut Buffer) {}
}

struct Pending {
    start_span: SpanRef,
    lang: Box<str>,
}

struct HighlightVisitor<'a> {
    processor: &'a mut HighlightProcessor,
    pending: Pending,
    end_span: SpanRef,
}

impl Visitor for HighlightVisitor<'_> {
    fn visit(self: Box<Self>, output: &mut Buffer) {
        let Some(config) = self.processor.configs.get(&self.pending.lang) else {
            return;
        };
        let Some((_outer, inner)) =
            output.outer_and_inner_spans(self.pending.start_span, self.end_span)
        else {
            // TODO: error handling
            return;
        };
        let original_text = &output[inner];
        // steps:
        // 1. parse out the source text, resolving html entities and stripping tags
        // 2. get highlighting events for the source text
        // 3. output the highlighted text, inserting spans as needed for highlighting, while also
        //    reinserting the stripped tags and re-encoding html entities
        let (stripped_text, html_events) = strip_html(original_text);
        let Ok(highlights) =
            self.processor
                .highlighter
                .highlight(config, stripped_text.as_bytes(), None, |lang| {
                    self.processor.configs.get(lang)
                })
        else {
            // TODO: error handling
            return;
        };
        let mut idx = 0;
        let combined_highlights = highlights.filter_map(Result::ok).filter_map(|ev| match ev {
            HighlightEvent::Source { end, .. } => {
                idx = end;
                None
            }
            HighlightEvent::HighlightStart(hl) => Some(CombinedEvent {
                start: idx,
                end: idx,
                kind: CombinedEventKind::HighlightStart(hl),
            }),
            HighlightEvent::HighlightEnd => Some(CombinedEvent {
                start: idx,
                end: idx,
                kind: CombinedEventKind::HighlightEnd,
            }),
        });
        let combined_html_events = html_events.into_iter().map(CombinedEvent::from);
        let combined_events = combined_highlights.merge_by(combined_html_events, |left, right| {
            if left.start != right.start {
                return left.start < right.start;
            }
            match (left.kind, right.kind) {
                // non-breaking spans come after highlight events
                (_, CombinedEventKind::Html(_, false)) => true,
                // highlight start comes after breaking spans
                (CombinedEventKind::HighlightStart(_), _) => false,
                // higlight end comes before breaking spans
                (CombinedEventKind::HighlightEnd, _) => true,
                (CombinedEventKind::Html(..), _) => {
                    unreachable!("HTML events in highlight event stream")
                }
            }
        });

        let hl_names = &self.processor.highlight_names;
        let mut hl_stack = Vec::new();
        let mut out_buffer = String::with_capacity(original_text.len() * 2);
        let mut idx = 0;
        let mut broken = false;
        for event in combined_events {
            if broken && idx != event.start {
                unbreak_highlight_spans(hl_names, &hl_stack, &mut out_buffer);
                broken = false;
            }
            out_buffer.push_str(&stripped_text[idx..event.start]);
            match event.kind {
                CombinedEventKind::Html(text, break_spans) => {
                    if !broken && break_spans {
                        for _ in &hl_stack {
                            out_buffer.push_str("</span>");
                        }
                    } else if broken && !break_spans {
                        unbreak_highlight_spans(hl_names, &hl_stack, &mut out_buffer);
                    }
                    broken = break_spans;
                    out_buffer.push_str(text);
                }
                CombinedEventKind::HighlightStart(hl) => {
                    hl_stack.push(hl);
                    write_highlight_span(&self.processor.highlight_names, hl, &mut out_buffer);
                }
                CombinedEventKind::HighlightEnd => {
                    out_buffer.push_str("</span>");
                    hl_stack.pop();
                }
            }
            idx = event.end;
        }
        std::fmt::Write::write_str(
            &mut output.replace_span(inner).expect(EXPECT_SPAN),
            &out_buffer,
        )
        .expect(EXPECT_WRITE);
    }
}

fn strip_html(source: &str) -> (String, Vec<HtmlEvent<'_>>) {
    let mut stripped_text = String::with_capacity(source.len());
    let mut html_events = Vec::new();
    let mut tokens = htmlparser::Tokenizer::from_fragment(source, 0..source.len());
    let mut last_idx = 0;
    while let Some(Ok(token)) = tokens.next() {
        match token {
            // text: handle any html entities and push to the output
            Token::Text { text } => {
                strip_entities(text.as_str(), &mut stripped_text, &mut html_events)
            }
            // element start: look for the corresponding element end, and throw the whole thing
            // in the events list
            Token::ElementStart {
                span: element_start,
                ..
            } => {
                if let Some((element_end, break_spans)) = tokens.find_map(|res| match res {
                    Ok(Token::ElementEnd {
                        span,
                        end: htmlparser::ElementEnd::Empty,
                    }) => Some((span, false)),
                    Ok(Token::ElementEnd {
                        span,
                        end: htmlparser::ElementEnd::Open,
                    }) => Some((span, true)),
                    _ => None,
                }) {
                    let start = element_start.start();
                    let end = element_end.end();
                    last_idx = end;
                    html_events.push(HtmlEvent {
                        start: stripped_text.len(),
                        end: stripped_text.len(),
                        replacement: &source[start..end],
                        break_spans,
                    });
                }
            }
            // element end: add to the event list
            Token::ElementEnd {
                span,
                end: htmlparser::ElementEnd::Close(..),
            } => {
                last_idx = span.end();
                html_events.push(HtmlEvent {
                    start: stripped_text.len(),
                    end: stripped_text.len(),
                    replacement: span.as_str(),
                    break_spans: true,
                });
            }
            // other HTML tokens: add the events list.
            _ => {
                let span = token.span();
                last_idx = span.end();
                html_events.push(HtmlEvent {
                    start: stripped_text.len(),
                    end: stripped_text.len(),
                    replacement: span.as_str(),
                    break_spans: false,
                });
            }
        }
    }
    stripped_text.push_str(&source[last_idx..]);
    (stripped_text, html_events)
}

fn strip_entities<'a>(source: &'a str, dest: &mut String, events: &mut Vec<HtmlEvent<'a>>) {
    let mut prev = 0;
    for (i, _m) in source.match_indices('&') {
        dest.push_str(&source[prev..i]);
        let start = dest.len();
        prev = i;
        if let Some(len) =
            source[i + 1..].find(|c: char| !(c == '#' || c == ';' || c.is_ascii_alphanumeric()))
        {
            let replacement = &source[i..][..len + 1];
            if html_escape::decode_html_entities_to_string(replacement, dest) != replacement {
                events.push(HtmlEvent {
                    start,
                    end: dest.len(),
                    replacement,
                    break_spans: false,
                });
                prev += len + 1;
            }
        }
    }
    dest.push_str(&source[prev..])
}

#[derive(Clone, Copy, Debug)]
struct HtmlEvent<'a> {
    /// Byte index into the stripped text where this event occurs
    start: usize,
    end: usize,
    replacement: &'a str,
    break_spans: bool,
}

#[derive(Clone, Copy, Debug)]
struct CombinedEvent<'a> {
    start: usize,
    end: usize,
    kind: CombinedEventKind<'a>,
}

impl<'a> From<HtmlEvent<'a>> for CombinedEvent<'a> {
    fn from(event: HtmlEvent<'a>) -> Self {
        Self {
            start: event.start,
            end: event.end,
            kind: CombinedEventKind::Html(event.replacement, event.break_spans),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum CombinedEventKind<'a> {
    Html(&'a str, bool),
    HighlightStart(Highlight),
    HighlightEnd,
}

fn write_highlight_span(
    highlight_names: &[Box<str>],
    highlight: Highlight,
    out_buffer: &mut String,
) {
    out_buffer.push_str(r#"<span class=""#);
    for class in highlight_names[highlight.0].split('.') {
        out_buffer.push_str(class);
        out_buffer.push(' ');
    }
    out_buffer.push_str(r#"">"#);
}

fn unbreak_highlight_spans(
    highlight_names: &[Box<str>],
    highlight_stack: &[Highlight],
    out_buffer: &mut String,
) {
    for &hl in highlight_stack {
        write_highlight_span(highlight_names, hl, out_buffer);
    }
}
