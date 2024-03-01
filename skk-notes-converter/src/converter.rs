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
        _ => form_to_skk_okuri(form).to_string(),
    }
}

/// 対応する辞書形の送り仮名を返す
/// 返した送り仮名は、語幹のみを取り出すのに利用される
fn form_to_skk_okuri(form: &VerbForm) -> &str {
    match form {
        VerbForm::Godan(row) => match row.as_str() {
            "カ" => "く",
            "ガ" => "ぐ",
            "サ" => "す",
            "ザ" => "ず",
            "タ" => "つ",
            "ナ" => "ぬ",
            "バ" => "ぶ",
            "マ" => "む",
            "ラ" => "る",
            "ワ" => "う",
            _ => panic!("Can not get okuri for godan verb with {}", row),
        },

        VerbForm::Yodan(row) => match row.as_str() {
            "カ" => "く",
            "ガ" => "ぐ",
            "サ" => "す",
            "タ" => "つ",
            "ダ" => "づ",
            "ナ" => "ぬ",
            "ハ" => "ふ",
            "バ" => "ぶ",
            "マ" => "む",
            "ラ" => "る",
            _ => panic!("Can not get okuri for yodan verb with {}", row),
        },
        VerbForm::SimoIchidan(row) => match row.as_str() {
            "ア" => "える",
            "カ" => "ける",
            "ガ" => "げる",
            "サ" => "せる",
            "ザ" => "ぜる",
            "タ" => "てる",
            "ダ" => "でる",
            "ナ" => "ねる",
            "ハ" => "へる",
            "バ" => "べる",
            "マ" => "める",
            "ラ" => "れる",
            _ => panic!("Can not get okuri for shimoichidan verb with {}", row),
        },
        VerbForm::KamiIchidan(row) => match row.as_str() {
            "ア" => "いる",
            "カ" => "きる",
            "ガ" => "ぎる",
            "ザ" => "じる",
            "タ" => "ちる",
            // ナ行上一は、「にる」のみ
            "ナ" => "る",
            // ハ行上一は、「ひる」のみ
            "ハ" => "る",
            "バ" => "びる",
            // 語幹自体が「み」のものも含む
            "マ" => "みる",
            "ラ" => "りる",
            "ワ" => "ゐる",
            _ => panic!("Can not get okuri for kamiichidan verb with {}", row),
        },
        VerbForm::SimoNidan(row) => match row.as_str() {
            // ア行下二は、「得る」のみ
            "ア" => "る",
            "カ" => "く",
            "ガ" => "ぐ",
            "サ" => "す",
            "ザ" => "ず",
            "タ" => "つ",
            "ダ" => "づ",
            "ナ" => "ぬ",
            "ハ" => "ふ",
            "マ" => "む",
            "ラ" => "る",
            "ヤ" => "ゆ",
            "ワ" => "う",
            _ => panic!("Can not get okuri for godan verb with {}", row),
        },
        VerbForm::KamiNidan(row) => match row.as_str() {
            "カ" => "く",
            "ガ" => "ぐ",
            "タ" => "つ",
            "ダ" => "づ",
            "ハ" => "ふ",
            "バ" => "ぶ",
            "マ" => "む",
            "ヤ" => "ゆ",
            "ラ" => "る",
            "ワ" => "る",
            _ => panic!("Can not get okuri for godan verb with {}", row),
        },
        VerbForm::Hen(row) => match row.as_str() {
            // カ行変格活用では、基本的に語幹自体が無いという考え方がある。
            "カ" => "くる",
            "サ" => "する",
            "ラ" => "り",
            "ナ" => "ぬ",
            _ => panic!("Can not get okuri for henkaku verb with {}", row),
        },
    }
}

/// 辞書形の送り仮名を削除して語幹のみを返す
/// SKKのnotesでは、辞書形が見出しから確定できないものに限っては、固定されたものが設定されているため、
/// 迂遠だがこういう形をとらないといけない
fn drop_dictionary_okuri(word: &str, speech: &NoteSpeech) -> String {
    match speech {
        NoteSpeech::Verb(form, _) => {
            let okuri = form_to_skk_okuri(form);

            if word.len() == okuri.len() {
                word.chars().next().unwrap().to_string()
            } else {
                word[..(word.len() - okuri.len())].to_string()
            }
        }
        NoteSpeech::Adjective(_) => word[..(word.len() - "い".len())].to_string(),
        _ => word.to_string(),
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

        let (stem_with_okuri, headword_with_okuri) = (
            format!("{}{}", self.stem, okuri),
            format!("{}{}", headword, okuri),
        );
        println!("{} -> {}", &stem_with_okuri, headword_with_okuri);

        (
            drop_dictionary_okuri(&stem_with_okuri, &self.speech),
            drop_dictionary_okuri(&headword_with_okuri, &self.speech),
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
