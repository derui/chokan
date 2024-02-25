use std::fmt::Display;

use dic::base::speech::{Speech, VerbForm};

use crate::note_grammer::{Note, NoteEntry, NoteSpeech, Okuri};

/// 変換した後のNoteEntry。SKKから変換できる形式とでは互換性を保つのが難しいので、
/// 独自の型で表現する
pub struct ConvertedEntry {
    pub headword: String,
    /// 見出し
    pub word: String,
    /// 変換後
    pub speech: Speech,
}

impl Display for ConvertedEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\t{}\t{}", self.headword, self.word, self.speech)
    }
}

impl Okuri {
    fn to_kana(&self, default: &str) -> String {
        match self {
            Okuri::Fixed(v) => v.to_string(),
            Okuri::CharClass(_) => default.to_string(),
        }
    }
}

/// 動詞に対応する送り仮名を返す。SKKのnotesでは、辞書系が見出しから確定できないものに限っては
/// Fixedで指定されているため、Fixed以外であればVerbFormから取得できるものをそのまま利用して良い
fn form_to_okuri_kana(form: &VerbForm, okuri: &Option<Okuri>) -> String {
    match okuri {
        Some(Okuri::Fixed(v)) => v.to_string(),
        _ => form.to_dictionary_okuri().to_string(),
    }
}

impl NoteSpeech {
    /// 各品詞と元々ある送りの定義から、語に対応する送り仮名を返す
    fn to_okuri_kana(&self) -> String {
        match self {
            NoteSpeech::Verb(form, okuri) => form_to_okuri_kana(form, okuri),
            NoteSpeech::Adjective(okuri) => okuri
                .clone()
                .map(|v| v.to_kana("い"))
                .unwrap_or("い".to_string()),
            NoteSpeech::AdjectivalVerb(okuri) => okuri.to_kana("だ"),
            NoteSpeech::Adverb(okuri) => okuri.to_kana(""),
            NoteSpeech::Noun(_, okuri) => okuri
                .clone()
                .map(|v| v.to_kana(""))
                .unwrap_or("".to_string()),
            NoteSpeech::Counter(_) => "".to_string(),
            NoteSpeech::Verbatim(okuri) => okuri.to_kana(""),
            NoteSpeech::PreNounAdjectival(okuri) => okuri
                .clone()
                .map(|v| v.to_kana(""))
                .unwrap_or("".to_string()),
        }
    }
}

impl NoteEntry {
    /// 辞書形を返す
    fn get_dictionary_form(&self, headword: &str) -> (String, String) {
        let okuri = self.speech.to_okuri_kana();

        (
            format!("{}{}", self.stem, okuri),
            format!("{}{}", headword, okuri),
        )
    }

    /// NoteEntry自体を単独の[Entry]に変換する
    fn to_entry(&self, headword: &str) -> ConvertedEntry {
        let speech = match &self.speech {
            NoteSpeech::Verb(form, _) => Speech::Verb(form.clone()),
            NoteSpeech::Adjective(_) => Speech::Adjective,
            NoteSpeech::AdjectivalVerb(_) => Speech::AdjectivalVerb,
            NoteSpeech::Adverb(_) => Speech::Adverb,
            NoteSpeech::Noun(_, _) => Speech::Noun,
            NoteSpeech::Counter(_) => Speech::Counter,
            NoteSpeech::Verbatim(_) => Speech::Verbatim,
            NoteSpeech::PreNounAdjectival(_) => Speech::PreNounAdjectival,
        };
        let (word, headword) = self.get_dictionary_form(headword);

        ConvertedEntry {
            headword,
            word,
            speech,
        }
    }
}

impl Note {
    /// NoteからbaseにあるEntryの一覧に変換する
    pub fn to_entries(&self) -> Vec<ConvertedEntry> {
        let headword = &self.headword;
        self.entries.iter().map(|v| v.to_entry(headword)).collect()
    }
}
