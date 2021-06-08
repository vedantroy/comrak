/*!
  In many of these cases the AST will be scanned and then it
  is found there is no match. In many of these cases the scan
  turns up False. It can be see that in the very simplest cases,
  usually by doing a char check at the very begginning of the
  line, we can eliminate these checks without the same allocations
  that are done otherwise and cause the program considerable
  slowdown.

*/

use crate::nodes::{LatexArgs,NodeCommand};
use pest::{iterators::Pairs, Parser};
use std::str;
use twoway::find_bytes;

#[cfg(debug_assertions)]
const _LEXER: &str = include_str!("lexer.pest");

#[derive(Parser)]
#[grammar = "lexer.pest"]
struct Lexer;

#[inline(always)]
fn search(rule: Rule, line: &[u8]) -> Option<usize> {
    if let Ok(pairs) = Lexer::parse(rule, unsafe { str::from_utf8_unchecked(line) }) {
        Some(pairs.last().unwrap().as_span().end())
    } else {
        None
    }
}

#[inline(always)]
fn is_match(rule: Rule, line: &[u8]) -> bool {
    Lexer::parse(rule, unsafe { str::from_utf8_unchecked(line) }).is_ok()
}

#[inline(always)]
pub fn atx_heading_start(line: &[u8]) -> Option<usize> {
    if line[0] != b'#' {
        return None;
    }
    search(Rule::atx_heading_start, line)
}

#[inline(always)]
pub fn html_block_end_1(line: &[u8]) -> bool {
    // XXX: should be case-insensitive
    find_bytes(line, b"</script>").is_some()
        || find_bytes(line, b"</pre>").is_some()
        || find_bytes(line, b"</style>").is_some()
}

#[inline(always)]
pub fn html_block_end_2(line: &[u8]) -> bool {
    find_bytes(line, b"-->").is_some()
}

#[inline(always)]
pub fn html_block_end_3(line: &[u8]) -> bool {
    find_bytes(line, b"?>").is_some()
}

#[inline(always)]
pub fn html_block_end_4(line: &[u8]) -> bool {
    line.contains(&b'>')
}

#[inline(always)]
pub fn html_block_end_5(line: &[u8]) -> bool {
    find_bytes(line, b"]]>").is_some()
}

#[inline(always)]
pub fn open_code_fence(line: &[u8]) -> Option<usize> {
    if line[0] != b'`' && line[0] != b'~' {
        return None;
    }
    search(Rule::open_code_fence, line)
}

fn latex_env(rule: Rule, line: &[u8]) -> Option<(&str, usize)> {
    if let Ok(pairs) = Lexer::parse(rule, unsafe { str::from_utf8_unchecked(line) }) {
        let end_stmt = pairs.last().unwrap();
        let end_pos = end_stmt.as_span().end();
        let env_name = end_stmt.into_inner().last().unwrap().as_span().as_str();
        Some((env_name, end_pos))
    } else {
        None
    }
}

pub fn get_args(args: Pairs<'_, Rule>) -> (LatexArgs, LatexArgs) {
    let mut optional = vec![];
    let mut required = vec![];

    for (idx, arg) in args.enumerate() {
        if let Rule::EOI = arg.as_rule() {
            continue;
        }
        let s = arg.as_str();
        // trim surrounding braces
        let s = &s[1..s.len() - 1];
        match arg.as_rule() {
            // Doing the copy to `String` is fine here b/c we only call this function
            // if we expect to get the arguments & collect them into a struct
            Rule::latex_arg_curly => required.push((idx, s.to_string())),
            Rule::latex_arg_square => optional.push((idx, s.to_string())),
            // This should never happen at runtime insofar as lexer.pest is correct
            _ => panic!("Unexpected rule type: {:?}", arg.as_rule()),
        }
    }

    (required, optional)
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct OpenLatexEnvInfo {
    pub name: String,
    pub optional: LatexArgs,
    pub required: LatexArgs,
}

// We only support arguments on a single line
#[inline(always)]
pub fn open_latex_env(line: &[u8]) -> Option<(OpenLatexEnvInfo, usize)> {
    // latex blocks start with \begin{evname}
    if line[0] != b'\\' {
        return None;
    }
    if let Ok(pairs) = Lexer::parse(Rule::open_latex_env, unsafe {
        str::from_utf8_unchecked(line)
    }) {
        let full_stmt = pairs.last().unwrap();
        let end_pos = full_stmt.as_span().end();
        let mut children = full_stmt.into_inner();
        let name = children.next().unwrap().as_span().as_str().to_string();
        let (req, opt) = get_args(children);
        Some((
            OpenLatexEnvInfo {
                name,
                required: req,
                optional: opt,
            },
            end_pos,
        ))
    } else {
        None
    }
}

#[inline(always)]
pub fn close_latex_env(line: &[u8]) -> Option<(&str, usize)> {
    // latex environments end with \end{...args}
    if line[0] != b'\\' {
        return None;
    }
    latex_env(Rule::close_latex_env, line)
}

#[inline(always)]
pub fn latex_command(input: &[u8]) -> Option<(NodeCommand, usize)> {
    if let Ok(pairs) = Lexer::parse(Rule::latex_command, unsafe {
        str::from_utf8_unchecked(input)
    }) {
        let cmd = pairs.last().unwrap();
        let endpos = cmd.as_span().end();
        let mut children = cmd.into_inner();
        let name = children.next().unwrap().as_span().as_str().to_string();
        let (req, opt) = get_args(children);
        Some((NodeCommand {
            name,
            required: req,
            optional: opt,
        }, endpos))
    } else {
        None
    }
}

#[inline(always)]
pub fn close_code_fence(line: &[u8]) -> Option<usize> {
    if line[0] != b'`' && line[0] != b'~' {
        return None;
    }
    search(Rule::close_code_fence, line)
}

#[inline(always)]
pub fn html_block_start(line: &[u8]) -> Option<usize> {
    lazy_static! {
        static ref STR2: &'static [u8] = b"<!--";
        static ref STR3: &'static [u8] = b"<?";
        static ref STR5: &'static [u8] = b"<![CDATA[";
    }

    if !line.starts_with(b"<") {
        return None;
    }

    if is_match(Rule::html_block_start_1, line) {
        Some(1)
    } else if line.starts_with(*STR2) {
        Some(2)
    } else if line.starts_with(*STR3) {
        Some(3)
    } else if is_match(Rule::html_block_start_4, line) {
        Some(4)
    } else if line.starts_with(*STR5) {
        Some(5)
    } else if is_match(Rule::html_block_start_6, line) {
        Some(6)
    } else {
        None
    }
}

#[inline(always)]
pub fn html_block_start_7(line: &[u8]) -> Option<usize> {
    if is_match(Rule::html_block_start_7, line) {
        Some(7)
    } else {
        None
    }
}

pub enum SetextChar {
    Equals,
    Hyphen,
}

#[inline(always)]
pub fn setext_heading_line(line: &[u8]) -> Option<SetextChar> {
    if (line[0] == b'=' || line[0] == b'-') && is_match(Rule::setext_heading_line, line) {
        if line[0] == b'=' {
            Some(SetextChar::Equals)
        } else {
            Some(SetextChar::Hyphen)
        }
    } else {
        None
    }
}

#[inline(always)]
pub fn thematic_break(line: &[u8]) -> Option<usize> {
    if line[0] != b'*' && line[0] != b'-' && line[0] != b'_' {
        return None;
    }
    search(Rule::thematic_break, line)
}

#[inline(always)]
pub fn footnote_definition(line: &[u8]) -> Option<usize> {
    search(Rule::footnote_definition, line)
}

#[inline(always)]
pub fn scheme(line: &[u8]) -> Option<usize> {
    search(Rule::scheme_rule, line)
}

#[inline(always)]
pub fn autolink_uri(line: &[u8]) -> Option<usize> {
    search(Rule::autolink_uri, line)
}

#[inline(always)]
pub fn autolink_email(line: &[u8]) -> Option<usize> {
    search(Rule::autolink_email, line)
}

#[inline(always)]
pub fn html_tag(line: &[u8]) -> Option<usize> {
    search(Rule::html_tag, line)
}

#[inline(always)]
pub fn spacechars(line: &[u8]) -> Option<usize> {
    search(Rule::spacechars, line)
}

#[inline(always)]
pub fn link_title(line: &[u8]) -> Option<usize> {
    search(Rule::link_title, line)
}

#[inline(always)]
pub fn table_start(line: &[u8]) -> Option<usize> {
    search(Rule::table_start, line)
}

#[inline(always)]
pub fn table_cell(line: &[u8]) -> Option<usize> {
    search(Rule::table_cell, line)
}

#[inline(always)]
pub fn table_cell_end(line: &[u8]) -> Option<usize> {
    search(Rule::table_cell_end, line)
}

#[inline(always)]
pub fn table_row_end(line: &[u8]) -> Option<usize> {
    search(Rule::table_row_end, line)
}

#[inline(always)]
pub fn dangerous_url(line: &[u8]) -> Option<usize> {
    search(Rule::dangerous_url, line)
}

#[cfg(test)]
mod tests {
    use crate::{nodes::NodeCommand, scanners::OpenLatexEnvInfo};

    use super::{close_latex_env, open_code_fence, open_latex_env, latex_command};

    // This is for understanding how `open_code_fence` works
    #[test]
    fn test_open_code_fence() {
        assert_eq!(open_code_fence(b"```rust\nfn foo();"), Some(3));
    }

    #[test]
    fn test_latex_command() {
        // Here the `endpos` that is returned is "7" b/c the
        // rule matches EOI (end of input). This seems fine?
        assert_eq!(latex_command(b"sqrt{3}"), Some((NodeCommand {
            name: "sqrt".into(),
            required: vec![(0, "3".into())],
            optional: vec![],
        }, 7)));

        assert_eq!(latex_command(b"sqrt{3} hello!"), Some((NodeCommand {
            name: "sqrt".into(),
            required: vec![(0, "3".into())],
            optional: vec![],
        }, 8)));
    }

    #[test]
    fn test_open_latex_env() {
        assert_eq!(open_latex_env(b"\\begin{}\n"), None);
        assert_eq!(open_latex_env(b"\\begin{tabbed}\n"), Some((OpenLatexEnvInfo {
            name: "tabbed".into(),
            optional: vec![],
            required: vec![],
        }, 15)));
        assert_eq!(
            open_latex_env(b"\\begin{tabbed}[arg]{arg2}\n"),
            Some((
                OpenLatexEnvInfo {
                    name: "tabbed".into(),
                    required: vec![(1, "arg2".into())],
                    optional: vec![(0, "arg".into())]
                },
                26
            ))
        );
    }

    #[test]
    fn test_close_latex_env() {
        assert_eq!(close_latex_env(b"\\end{a}\n"), Some(("a", 8)));
    }
}
