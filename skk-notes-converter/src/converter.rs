use std::fmt::Display;

use dic::base::speech::{AffixVariant, NounVariant, ParticleType, Speech, VerbForm};

use crate::note_grammer::{Note, NoteEntry, NoteSpeech, Okuri};

/// 変換した後のNoteEntry。SKKから変換できる形式とでは互換性を保つのが難しいので、
/// 独自の型で表現する
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ConvertedEntry {
    pub headword: String,
    /// 見出し
    pub word: String,
    /// 変換後
    pub speech: Speech,
}

impl ConvertedEntry {
    pub fn is_ancillary(&self) -> bool {
        match &self.speech {
            // ここでは接辞を対象にする
            Speech::Affix(_) => true,
            _ => false,
        }
    }
}

impl Display for ConvertedEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\t{}\t/{}/", self.headword, self.word, self.speech)
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
            "バ" => "ぶ",
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
        NoteSpeech::AdjectivalVerb(_) => word[..(word.len() - "だ".len())].to_string(),
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
            NoteSpeech::ConjuctiveParticle(okuri) => okuri
                .clone()
                .map(|v| v.to_kana(""))
                .unwrap_or("".to_string()),
            NoteSpeech::Conjunction(okuri) => okuri
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

        (
            drop_dictionary_okuri(&stem_with_okuri, &self.speech),
            drop_dictionary_okuri(&headword_with_okuri, &self.speech),
        )
    }

    /// 可能なら接辞を返す
    fn to_affix_if_possible(&self, headword: &str) -> Option<ConvertedEntry> {
        self.speech
            .okuri()
            .and_then(|o| {
                if o.has_prefix() {
                    Some(Speech::Affix(AffixVariant::Prefix))
                } else if o.has_suffix() {
                    Some(Speech::Affix(AffixVariant::Suffix))
                } else {
                    None
                }
            })
            .map(|speech| {
                let (stem, headword) = self.get_dictionary_form(headword);
                ConvertedEntry {
                    headword,
                    word: stem,
                    speech,
                }
            })
    }

    /// NoteEntry自体を単独の[Entry]に変換する
    fn to_entries(&self, headword: &str) -> Vec<ConvertedEntry> {
        let speech = match &self.speech {
            NoteSpeech::Verb(form, _) => Speech::Verb(form.clone()),
            NoteSpeech::Adjective(_) => Speech::Adjective,
            NoteSpeech::AdjectivalVerb(_) => Speech::AdjectivalVerb,
            NoteSpeech::Adverb(_) => Speech::Adverb,
            NoteSpeech::Noun(typ, _) => match typ.as_str() {
                // notesではサ変名詞くらいしか区別されていない
                "サ変名詞" => Speech::Noun(NounVariant::Sahen),
                _ => Speech::Noun(NounVariant::Common),
            },
            NoteSpeech::Counter(_) => Speech::Counter,
            NoteSpeech::Verbatim(_) => Speech::Verbatim,
            NoteSpeech::PreNounAdjectival(_) => Speech::PreNounAdjectival,
            NoteSpeech::ConjuctiveParticle(_) => Speech::Particle(ParticleType::Conjunctive),
            NoteSpeech::Conjunction(_) => Speech::Conjunction,
        };
        let (word, dic_headword) = self.get_dictionary_form(headword);
        let base = ConvertedEntry {
            headword: dic_headword,
            word,
            speech,
        };

        vec![Some(base), self.to_affix_if_possible(headword)]
            .into_iter()
            .flatten()
            .collect()
    }
}

impl Note {
    /// NoteからbaseにあるEntryの一覧に変換する
    pub fn to_entries(&self) -> Vec<ConvertedEntry> {
        let headword = &self.headword;
        self.entries
            .iter()
            .flat_map(|v| v.to_entries(headword))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::note_grammer::parse_note;

    #[test]
    fn convert_adjactive_verb() {
        let note = parse_note("おんみつ /隠密;∥形容動詞[φdn(s)]/");

        let converted = note.unwrap().unwrap().to_entries();

        assert_eq!(converted.len(), 1);
        assert_eq!(converted[0].to_string(), "おんみつ\t隠密\t/形容動詞/")
    }

    #[test]
    fn parse_affix() {
        let note = parse_note("ふ /不;∥副詞[>]/");

        let converted = note.unwrap().unwrap().to_entries();
        assert_eq!(converted.len(), 2);
        assert_eq!(converted[0].to_string(), "ふ\t不\t/副詞/");
        assert_eq!(converted[1].to_string(), "ふ\t不\t/接頭辞/");
    }
}
