use super::{speech::Speech, word::Word};

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
                val.speech,
            ));
        }

        if forms.is_empty() {
            words.push(Word::new(&val.stem, &val.stem_reading, val.speech));
        }

        words
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_entry_to_word() {
        // Arrange
        let entry = Entry::new(
            "食べ",
            vec!["る".to_string(), "ない".to_string()],
            "たべ",
            Speech::Verb,
        );

        // Act
        let words: Vec<Word> = entry.into();

        // Assert
        assert_eq!(words.len(), 2);
        assert_eq!(words[0], Word::new("食べる", "たべる", Speech::Verb));
        assert_eq!(words[1], Word::new("食べない", "たべない", Speech::Verb));
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
}
