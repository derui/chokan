use std::fmt::{self, Display};

use super::{speech::Speech, support, word::Word};

/// 辞書における一単語を表現する型
/// 語幹と活用形は辞書でのみ必要になる属性であるため、基本的にはWordを利用する必要がある
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Entry {
    /// 語幹
    stem: String,
    /// 活用形
    forms: Vec<String>,
    /// 語幹の読み。活用形はすべてひらがななので、語幹の読みだけを持つ
    pub stem_reading: String,
    /// 品詞。動詞の活用形はformsで表現されているため、ここでは不要としている
    pub speech: Speech,
}

impl Entry {
    /// 新しいEntryを生成する
    pub fn new(stem: &str, forms: Vec<String>, stem_reading: &str, speech: Speech) -> Entry {
        Entry {
            stem: stem.to_string(),
            forms,
            stem_reading: stem_reading.to_string(),
            speech,
        }
    }

    /// 読みと漢字、品詞からEntryを返す
    pub fn from_jisyo(reading: &str, kanji: &str, speech: Speech) -> Entry {
        let stem_reading = support::reading_to_stem_reading(reading, &speech);
        let stem = support::kanji_to_stem(kanji, &speech);
        Entry {
            stem: stem.to_string(),
            forms,
            stem_reading: stem_reading.to_string(),
            speech,
        }
    }

    /// 辞書形のかなを返す
    fn get_dictionary_kana(&self) -> String {
        match &self.speech {
            // 動詞の場合、活用形次第で変わってくるため、ここでは判定しない
            Speech::Verb(form) => form.to_dictionary_form(&self.stem_reading),
            Speech::Adjective => format!("{}い", self.stem_reading),
            Speech::AdjectivalVerb => format!("{}だ", self.stem_reading),
            _ => self.stem.clone(),
        }
    }

    /// 辞書形の変換語を返す
    fn get_dictionary_kanji(&self) -> String {
        match &self.speech {
            // 動詞の場合、活用形次第で変わってくるため、ここでは判定しない
            Speech::Verb(form) => form.to_dictionary_form(&self.stem),
            Speech::Adjective => format!("{}い", self.stem),
            Speech::AdjectivalVerb => format!("{}だ", self.stem),
            _ => self.stem.clone(),
        }
    }
}

impl Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kana = self.get_dictionary_kana();
        let kanji = self.get_dictionary_kanji();
        write!(f, "{}\t{}\t/{}/", kana, kanji, self.speech)
    }
}

impl From<Entry> for Vec<Word> {
    /// Entryから活用した形式一覧をWordのリストに変換する
    fn from(val: Entry) -> Self {
        let mut words = Vec::new();
        let forms = &val.forms;

        for form in forms {
            let reading = format!("{}{}", val.stem_reading, form);
            words.push(Word::new(
                &format!("{}{}", val.stem, form),
                &reading,
                val.speech.clone(),
            ));
        }

        if forms.is_empty() {
            words.push(Word::new(&val.stem, &val.stem_reading, val.speech.clone()));
        }

        words
    }
}

#[cfg(test)]
mod tests {
    use crate::base::speech::VerbForm;

    use super::*;

    #[test]
    fn test_entry_to_word() {
        // Arrange
        let verb = Speech::Verb(VerbForm::SimoIchidan("バ".to_string()));
        let entry = Entry::new(
            "食べ",
            vec!["る".to_string(), "ない".to_string()],
            "たべ",
            verb.clone(),
        );

        // Act
        let words: Vec<Word> = entry.into();

        // Assert
        assert_eq!(words.len(), 2);
        assert_eq!(words[0], Word::new("食べる", "たべる", verb.clone()));
        assert_eq!(words[1], Word::new("食べない", "たべない", verb.clone()));
    }

    #[test]
    fn contains_only_stem() {
        // Arrange
        let entry = Entry::new("大きな", vec![], "おおきな", Speech::Adjective);

        // Act
        let words: Vec<Word> = entry.into();

        // Assert
        assert_eq!(words.len(), 1);
        assert_eq!(words[0], Word::new("大きな", "おおきな", Speech::Adjective));
    }

    #[test]
    fn display_adjective_entry() {
        // Arrange
        let entry = Entry::new("大き", vec!["い".to_string()], "おおき", Speech::Adjective);

        // Act
        let actual = format!("{}", entry);

        // Assert
        assert_eq!(actual, "おおきい\t大きい\t/形容詞/");
    }

    #[test]
    fn display_verb_entry() {
        // Arrange
        let entry = Entry::new(
            "引",
            vec!["く".to_string()],
            "ひ",
            Speech::Verb(VerbForm::Godan("カ".to_string())),
        );

        // Act
        let actual = format!("{}", entry);

        // Assert
        assert_eq!(actual, "ひく\t引く\t/カ行五段/");
    }
}
