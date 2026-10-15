// Program status (OSC 7501): what a terminal's programs say they are doing.
//
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: GPL-3.0-or-later
//
// A program reports its state to its terminal, which keeps one record per
// id, see https://superlogical.com/rex/docs/build/program-status:
//
//   ESC ] 7501 ; state=blocked : kind=permission : app=codex : msg=<base64> ST
//
// alacritty_terminal ignores this OSC, so the program's output is shown to
// a `Scanner' on its way to alacritty.  The scanner picks out the reports,
// the prompts a shell starts (OSC 133 A) and full resets (RIS), and
// `Records' keeps what they say.  A report is untrusted: one that breaks a
// rule of the protocol changes nothing.

use std::collections::BTreeMap;

use base64::alphabet::STANDARD;
use base64::engine::{DecodePaddingMode, GeneralPurpose, GeneralPurposeConfig};
use base64::Engine;

/// The longest sequence taken, from ESC ] to its terminator.
const MAX_SEQUENCE: usize = 4096;
const MAX_KEY: usize = 16;
const MAX_MSG: (usize, usize) = (2732, 2048);
const MAX_TITLE: (usize, usize) = (256, 192);
const MAX_APP: usize = 32;
const MAX_ID: usize = 128;
const MAX_SEGMENT: usize = 32;
const MAX_LEVELS: usize = 8;
const MAX_RECORDS: usize = 256;

/// The answer to a feature query, without its terminator.
pub const QUERY_REPLY: &[u8] =
    b"\x1b]7501;?:states=idle,working,done,blocked,error:kinds=permission,question,auth";

/// How an OSC ended, so an answer can end alike.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Terminator {
    Bel,
    St,
}

impl Terminator {
    pub fn bytes(self) -> &'static [u8] {
        match self {
            Terminator::Bel => b"\x07",
            Terminator::St => b"\x1b\\",
        }
    }
}

/// What a `Scanner' found in a program's output.
#[derive(Debug, PartialEq, Eq)]
pub enum Found<'a> {
    /// An OSC 7501: its text after "7501;", and how it ended.
    Status(&'a [u8], Terminator),
    /// A shell starts a prompt: OSC 133 A.
    Prompt,
    /// A full reset: RIS.
    Reset,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum Mode {
    #[default]
    Ground,
    /// After ESC.
    Escape,
    /// In an OSC, after ESC ].
    Osc,
    /// In an OSC, after an ESC that may begin its terminator.
    OscEscape,
}

/// Finds OSC 7501, OSC 133 A and RIS in a stream of output, however it is
/// cut into reads.  It follows alacritty's parser (vte): an OSC starts at
/// ESC ] wherever it appears and drops C0 controls, so both see the same
/// sequences.  Only BEL or ESC \ ends a sequence that counts; CAN, SUB or
/// another escape abandons it.
#[derive(Default)]
pub struct Scanner {
    mode: Mode,
    body: Vec<u8>,
    /// The OSC is longer than any sequence taken.
    overflow: bool,
}

impl Scanner {
    /// Scan BYTES, the next of the output, calling FOUND with what is found.
    pub fn scan(&mut self, bytes: &[u8], found: &mut impl FnMut(Found)) {
        let mut i = 0;
        while i < bytes.len() {
            if self.mode == Mode::Ground {
                match bytes[i..].iter().position(|&b| b == 0x1b) {
                    Some(at) => {
                        i += at + 1;
                        self.mode = Mode::Escape;
                    }
                    None => return,
                }
                continue;
            }
            let byte = bytes[i];
            i += 1;
            self.mode = match (self.mode, byte) {
                (Mode::Escape | Mode::OscEscape, 0x1b) => Mode::Escape,
                (Mode::Escape, b']') => {
                    self.body.clear();
                    self.overflow = false;
                    Mode::Osc
                }
                (Mode::Escape, b'c') => {
                    found(Found::Reset);
                    Mode::Ground
                }
                (Mode::Escape, 0x00..=0x17 | 0x19 | 0x1c..=0x1f) => Mode::Escape,
                (Mode::Escape, _) => Mode::Ground,
                (Mode::Osc, 0x07) => {
                    self.end(Terminator::Bel, found);
                    Mode::Ground
                }
                (Mode::Osc, 0x18 | 0x1a) => Mode::Ground,
                (Mode::Osc, 0x1b) => Mode::OscEscape,
                (Mode::Osc, 0x00..=0x1f) => Mode::Osc,
                (Mode::Osc, _) => {
                    if self.body.len() < MAX_SEQUENCE {
                        self.body.push(byte);
                    } else {
                        self.overflow = true;
                    }
                    Mode::Osc
                }
                (Mode::OscEscape, b'\\') => {
                    self.end(Terminator::St, found);
                    Mode::Ground
                }
                // The OSC is abandoned; this byte follows a plain ESC.
                (Mode::OscEscape, b']') => {
                    self.body.clear();
                    self.overflow = false;
                    Mode::Osc
                }
                (Mode::OscEscape, b'c') => {
                    found(Found::Reset);
                    Mode::Ground
                }
                (Mode::OscEscape, 0x00..=0x17 | 0x19 | 0x1c..=0x1f) => Mode::Escape,
                (Mode::OscEscape, _) => Mode::Ground,
                (Mode::Ground, _) => Mode::Ground,
            };
        }
    }

    fn end(&mut self, terminator: Terminator, found: &mut impl FnMut(Found)) {
        let length = 2 + self.body.len() + terminator.bytes().len();
        if let Some(text) = self.body.strip_prefix(b"7501;") {
            if !self.overflow && length <= MAX_SEQUENCE {
                found(Found::Status(text, terminator));
            }
        } else if let Some(text) = self.body.strip_prefix(b"133;A") {
            if text.is_empty() || text[0] == b';' {
                found(Found::Prompt);
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum State {
    Idle,
    Working,
    Done,
    Blocked,
    Error,
}

impl State {
    pub fn name(self) -> &'static str {
        match self {
            State::Idle => "idle",
            State::Working => "working",
            State::Done => "done",
            State::Blocked => "blocked",
            State::Error => "error",
        }
    }
}

/// Why a program is blocked.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Permission,
    Question,
    Auth,
}

impl Kind {
    pub fn name(self) -> &'static str {
        match self {
            Kind::Permission => "permission",
            Kind::Question => "question",
            Kind::Auth => "auth",
        }
    }
}

/// What a program said of itself, or of one of its tasks.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Record {
    /// The path of ids naming the task, empty for the program itself.
    pub id: String,
    pub state: State,
    pub kind: Option<Kind>,
    /// Percent done, when known.
    pub progress: Option<u8>,
    /// The program's identifier, perhaps inherited from a parent task.
    pub app: Option<String>,
    pub title: Option<String>,
    pub msg: Option<String>,
}

/// A report a program sent.
#[derive(Debug, PartialEq, Eq)]
pub enum Report {
    /// The program asks whether the terminal takes reports.
    Query,
    Set(Record),
    /// Remove the record of the id, and those beneath it; of every id when
    /// none is given.
    Clear(Option<String>),
}

fn is_id_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || matches!(b, b'_' | b'.' | b'+' | b'-')
}

fn is_value_char(b: u8) -> bool {
    is_id_char(b) || matches!(b, b',' | b'/' | b'=')
}

/// The text of base64 VALUE, nil if empty, or an error when VALUE is not
/// base64 of text without controls, at most LIMITS (encoded, decoded) long:
/// the report must be refused.  Characters a terminal must not show outside
/// its grid as they are, those that reorder or hide text, are left out.
fn decode_text(value: &[u8], (encoded, decoded): (usize, usize)) -> Result<Option<String>, ()> {
    const BASE64: GeneralPurpose = GeneralPurpose::new(
        &STANDARD,
        GeneralPurposeConfig::new().with_decode_padding_mode(DecodePaddingMode::Indifferent),
    );
    if value.len() > encoded {
        return Err(());
    }
    let bytes = BASE64.decode(value).map_err(|_| ())?;
    if bytes.len() > decoded {
        return Err(());
    }
    let text = String::from_utf8(bytes).map_err(|_| ())?;
    if text.chars().any(char::is_control) {
        return Err(());
    }
    let text: String = text.chars().filter(|&c| !is_disarmed(c)).collect();
    Ok((!text.is_empty()).then_some(text))
}

/// Bidirectional overrides and invisible formatting characters.
fn is_disarmed(c: char) -> bool {
    matches!(c,
        '\u{061c}' | '\u{200b}'..='\u{200f}' | '\u{202a}'..='\u{202e}'
        | '\u{2060}'..='\u{2064}' | '\u{2066}'..='\u{2069}' | '\u{feff}')
}

fn valid_id(id: &str) -> bool {
    id.len() <= MAX_ID
        && id.split('/').count() <= MAX_LEVELS
        && id.split('/').all(|s| !s.is_empty() && s.len() <= MAX_SEGMENT && s.bytes().all(is_id_char))
}

/// Read the report in TEXT, an OSC 7501's text after "7501;", or nil when
/// the protocol says to ignore it.
pub fn parse(text: &[u8]) -> Option<Report> {
    if text.trim_ascii() == b"?" {
        return Some(Report::Query);
    }
    let mut pairs: BTreeMap<&str, &str> = BTreeMap::new();
    for pair in text.split(|&b| b == b':') {
        let Some(at) = pair.iter().position(|&b| b == b'=') else { continue };
        let (key, value) = (pair[..at].trim_ascii(), pair[at + 1..].trim_ascii());
        if key.len() > MAX_KEY {
            return None;
        }
        if key.is_empty() || !key.iter().all(u8::is_ascii_lowercase) || !value.iter().all(|&b| is_value_char(b)) {
            continue;
        }
        // Both are ASCII.
        let (Ok(key), Ok(value)) = (std::str::from_utf8(key), std::str::from_utf8(value)) else { continue };
        pairs.insert(key, value);
    }
    let id = match pairs.get("id") {
        None => None,
        Some(id) if valid_id(id) => Some(id.to_string()),
        Some(_) => return None,
    };
    let state = match *pairs.get("state")? {
        "idle" => State::Idle,
        "working" => State::Working,
        "done" => State::Done,
        "blocked" => State::Blocked,
        "error" => State::Error,
        "clear" => return Some(Report::Clear(id)),
        _ => return None,
    };
    let kind = match pairs.get("kind").copied() {
        Some("permission") if state == State::Blocked => Some(Kind::Permission),
        Some("question") if state == State::Blocked => Some(Kind::Question),
        Some("auth") if state == State::Blocked => Some(Kind::Auth),
        _ => None,
    };
    let progress = pairs
        .get("progress")
        .filter(|p| matches!(state, State::Working | State::Blocked) && p.len() <= 3 && p.bytes().all(|b| b.is_ascii_digit()))
        .and_then(|p| p.parse::<u8>().ok())
        .filter(|&p| p <= 100);
    let app = match pairs.get("app") {
        Some(app) if app.len() > MAX_APP => return None,
        Some(app) if !app.is_empty() && app.bytes().all(is_id_char) => Some(app.to_string()),
        _ => None,
    };
    let decode = |key: &str, limits| match pairs.get(key) {
        None => Ok(None),
        Some(value) => decode_text(value.as_bytes(), limits),
    };
    let (Ok(title), Ok(msg)) = (decode("title", MAX_TITLE), decode("msg", MAX_MSG)) else { return None };
    Some(Report::Set(Record { id: id.unwrap_or_default(), state, kind, progress, app, title, msg }))
}

/// The records a terminal keeps, one per id.
#[derive(Default)]
pub struct Records {
    /// Each record, by id, with when it was last set.
    records: BTreeMap<String, (Record, u64)>,
    clock: u64,
}

impl Records {
    /// Take REPORT; return whether a record changed.  Queries are the
    /// caller's to answer.
    pub fn apply(&mut self, report: Report) -> bool {
        match report {
            Report::Query => false,
            Report::Set(record) => {
                self.clock += 1;
                let id = record.id.clone();
                let changed = self.records.get(&id).is_none_or(|(old, _)| *old != record);
                self.records.insert(id.clone(), (record, self.clock));
                if self.records.len() > MAX_RECORDS {
                    // Make room by the record set longest ago.
                    if let Some(oldest) = self
                        .records
                        .iter()
                        .filter(|(other, _)| **other != id)
                        .min_by_key(|(_, (_, stamp))| *stamp)
                        .map(|(other, _)| other.clone())
                    {
                        self.records.remove(&oldest);
                    }
                }
                changed
            }
            Report::Clear(None) => self.retain(|_| false),
            Report::Clear(Some(id)) => {
                let beneath = format!("{id}/");
                self.retain(|record| record.id != id && !record.id.starts_with(&beneath))
            }
        }
    }

    /// The command ended: its program exited, or a shell starts a prompt.
    /// What still ran, waited or idled is over; what finished stays to be
    /// seen.  Return whether a record changed.
    pub fn end_command(&mut self) -> bool {
        self.retain(|record| matches!(record.state, State::Done | State::Error))
    }

    /// The user took up the terminal: what finished was seen.
    pub fn dismiss(&mut self) -> bool {
        self.retain(|record| !matches!(record.state, State::Done | State::Error))
    }

    /// The terminal was reset: forget every record.
    pub fn reset(&mut self) -> bool {
        self.retain(|_| false)
    }

    fn retain(&mut self, mut keep: impl FnMut(&Record) -> bool) -> bool {
        let before = self.records.len();
        self.records.retain(|_, (record, _)| keep(record));
        self.records.len() != before
    }

    /// Every record, the program's own first, then by id; a task without an
    /// app has that of its nearest ancestor that names one.
    pub fn list(&self) -> Vec<Record> {
        self.records
            .values()
            .map(|(record, _)| {
                let mut record = record.clone();
                if record.app.is_none() {
                    record.app = self.inherited_app(&record.id);
                }
                record
            })
            .collect()
    }

    fn inherited_app(&self, id: &str) -> Option<String> {
        let mut path = id;
        while !path.is_empty() {
            path = path.rfind('/').map_or("", |at| &path[..at]);
            if let Some((record, _)) = self.records.get(path) {
                if record.app.is_some() {
                    return record.app.clone();
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn found(chunks: &[&[u8]]) -> Vec<String> {
        let mut scanner = Scanner::default();
        let mut out = Vec::new();
        for chunk in chunks {
            scanner.scan(chunk, &mut |f| {
                out.push(match f {
                    Found::Status(text, t) => format!("{}{:?}", String::from_utf8_lossy(text), t),
                    Found::Prompt => "prompt".into(),
                    Found::Reset => "reset".into(),
                })
            });
        }
        out
    }

    fn set(text: &str) -> Record {
        match parse(text.as_bytes()) {
            Some(Report::Set(record)) => record,
            other => panic!("{text}: {other:?}"),
        }
    }

    fn b64(s: &str) -> String {
        base64::engine::general_purpose::STANDARD.encode(s)
    }

    #[test]
    fn scanner_finds_sequences_however_cut() {
        let stream = b"a\x1b]7501;state=working\x07b\x1b]133;A\x1b\\\x1b]133;B\x07\x1bc\x1b]7501;?\x1b\\";
        let whole = found(&[stream]);
        assert_eq!(whole, ["state=workingBel", "prompt", "reset", "?St"]);
        for cut in 1..stream.len() {
            assert_eq!(found(&[&stream[..cut], &stream[cut..]]), whole, "cut at {cut}");
        }
        let bytes: Vec<&[u8]> = stream.chunks(1).collect();
        assert_eq!(found(&bytes), whole);
    }

    #[test]
    fn scanner_ignores_other_and_abandoned_sequences() {
        assert_eq!(found(&[b"\x1b]0;title\x07\x1b]1337;x\x1b\\\x1b[31mred"]), Vec::<String>::new());
        // CAN cancels; an ESC not followed by \ abandons the OSC.
        assert_eq!(found(&[b"\x1b]7501;state=done\x18\x1b]7501;state=idle\x1bX"]), Vec::<String>::new());
        // An abandoning ESC may begin the next OSC.
        assert_eq!(found(&[b"\x1b]7501;state=done\x1b]7501;state=idle\x07"]), ["state=idleBel"]);
        // C0 controls are dropped from the text, as alacritty drops them.
        assert_eq!(found(&[b"\x1b]7501;state=\nidle\x07"]), ["state=idleBel"]);
        assert_eq!(found(&[b"\x1b]133;AB\x07\x1b]133;A;cl=m\x07"]), ["prompt"]);
    }

    #[test]
    fn scanner_drops_sequences_too_long() {
        let fits = format!("\x1b]7501;state=idle:msg={}\x07", "A".repeat(MAX_SEQUENCE - 23));
        assert_eq!(fits.len(), MAX_SEQUENCE);
        assert_eq!(found(&[fits.as_bytes()]).len(), 1);
        let long = format!("\x1b]7501;state=idle:msg={}\x07", "A".repeat(MAX_SEQUENCE - 22));
        assert!(found(&[long.as_bytes()]).is_empty());
        let huge = format!("\x1b]7501;state=idle:msg={}\x07\x1b]7501;state=done\x07", "A".repeat(3 * MAX_SEQUENCE));
        assert_eq!(found(&[huge.as_bytes()]), ["state=doneBel"]);
    }

    #[test]
    fn parse_reads_every_key() {
        let record = set(&format!(
            " state = blocked : kind=permission : progress=40 : app=codex : id=a/b : title={} : msg={}",
            b64("Edit λ"),
            b64("Run make test?").trim_end_matches('=')
        ));
        assert_eq!(
            record,
            Record {
                id: "a/b".into(),
                state: State::Blocked,
                kind: Some(Kind::Permission),
                progress: Some(40),
                app: Some("codex".into()),
                title: Some("Edit λ".into()),
                msg: Some("Run make test?".into()),
            }
        );
        assert_eq!(parse(b"?"), Some(Report::Query));
        assert_eq!(parse(b"state=clear:id=a"), Some(Report::Clear(Some("a".into()))));
        assert_eq!(parse(b"state=clear"), Some(Report::Clear(None)));
    }

    #[test]
    fn parse_skips_what_does_not_apply() {
        // Malformed pairs and unknown keys are skipped; the last value wins.
        let record = set("state=done:junk:BAD=1:app=a b:future=x:state=working:kind=permission:progress=101");
        assert_eq!((record.state, record.kind, record.progress, record.app), (State::Working, None, None, None));
        assert_eq!(set("state=done:progress=50").progress, None);
        assert_eq!(set("state=blocked:kind=other").kind, None);
        assert_eq!(set("state=done:msg=*").msg, None);
    }

    #[test]
    fn parse_refuses_reports_breaking_rules() {
        for text in [
            "app=x".to_string(),
            "state=sleeping".into(),
            "state=done:id=a//b".into(),
            "state=done:id=".into(),
            format!("state=done:id={}", "a/".repeat(8) + "a"),
            format!("state=done:id={}", "a".repeat(33)),
            format!("state=done:app={}", "a".repeat(33)),
            format!("state=done:{}=1", "k".repeat(17)),
            "state=done:msg=A".into(),
            "state=done:msg=-_".into(),
            format!("state=done:msg={}", b64("two\nlines")),
            format!("state=done:msg={}", b64("\u{85}")),
            format!("state=done:msg={}", base64::engine::general_purpose::STANDARD.encode([0xff, 0xfe])),
            format!("state=done:msg={}", b64(&"x".repeat(2049))),
            format!("state=done:title={}", b64(&"x".repeat(193))),
        ] {
            assert_eq!(parse(text.as_bytes()), None, "{text}");
        }
        assert_eq!(set(&format!("state=done:msg={}", b64(&"x".repeat(2048)))).msg.map(|m| m.len()), Some(2048));
    }

    #[test]
    fn parse_disarms_hidden_characters() {
        assert_eq!(set(&format!("state=done:msg={}", b64("a\u{202e}b\u{200b}c"))).msg, Some("abc".into()));
        assert_eq!(set(&format!("state=done:msg={}", b64("\u{2066}"))).msg, None);
    }

    fn apply(records: &mut Records, text: &str) -> bool {
        records.apply(parse(text.as_bytes()).expect(text))
    }

    fn states(records: &Records) -> Vec<(String, &'static str)> {
        records.list().into_iter().map(|r| (r.id, r.state.name())).collect()
    }

    #[test]
    fn records_replace_and_clear_by_id() {
        let mut records = Records::default();
        assert!(apply(&mut records, "state=working:app=cargo"));
        assert!(apply(&mut records, "state=working:id=build"));
        assert!(apply(&mut records, "state=done:id=build/a"));
        assert!(apply(&mut records, "state=working:id=test"));
        assert!(!apply(&mut records, "state=working:id=test"));
        // A task inherits its ancestor's app; a report replaces its record whole.
        assert_eq!(records.list()[2].app.as_deref(), Some("cargo"));
        assert!(apply(&mut records, "state=working"));
        assert_eq!(records.list()[2].app, None);
        assert!(apply(&mut records, "state=clear:id=build"));
        assert_eq!(states(&records), [("".into(), "working"), ("test".into(), "working")]);
        assert!(!apply(&mut records, "state=clear:id=nothing"));
        assert!(apply(&mut records, "state=clear"));
        assert!(records.list().is_empty());
    }

    #[test]
    fn records_end_with_their_command() {
        let mut records = Records::default();
        for text in ["state=idle", "state=working:id=a", "state=blocked:id=b", "state=done:id=c", "state=error:id=d"] {
            apply(&mut records, text);
        }
        assert!(records.end_command());
        assert_eq!(states(&records), [("c".into(), "done"), ("d".into(), "error")]);
        assert!(!records.end_command());
        apply(&mut records, "state=working:id=e");
        assert!(records.dismiss());
        assert_eq!(states(&records), [("e".into(), "working")]);
        assert!(records.reset());
        assert!(!records.reset());
    }

    #[test]
    fn records_are_bounded() {
        let mut records = Records::default();
        for i in 0..MAX_RECORDS + 10 {
            apply(&mut records, &format!("state=working:id=t{i}"));
        }
        let list = records.list();
        assert_eq!(list.len(), MAX_RECORDS);
        assert!(!list.iter().any(|r| r.id == "t0" || r.id == "t9"));
        assert!(list.iter().any(|r| r.id == "t10"));
    }
}
