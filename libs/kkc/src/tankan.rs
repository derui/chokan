use std::collections::HashMap;

use dic::base::word::Word;

/**
単漢字変換用の辞書
*/
pub struct TankanDictionary {
    pub kanji_map: HashMap<String, Vec<Word>>,
}

impl TankanDictionary {
    /**
    指定された文字列に対する単漢字の候補を返す

    # Arguments
    * `input` - 変換対象の文字列

    # Returns
    単漢字の候補
    */
    pub fn get_candidates(&self, input: &str) -> Vec<String> {
        self.kanji_map
            .get(input)
            .map(|v| v.iter().cloned().map(|w| w.word.iter().collect()).collect())
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use dic::base::speech::{NounVariant, Speech};

    use super::*;
    #[test]
    fn should_be_able_to_return_matched_kanjis() {
        // arrange
        let map = HashMap::from([(
            "くるま".to_string(),
            vec![Word::new("車", "くるま", Speech::Noun(NounVariant::Common))],
        )]);
        let dic = TankanDictionary { kanji_map: map };

        // act
        let ret = dic.get_candidates("くるま");

        // assert
        assert_eq!(ret, vec!["車"])
    }

    #[test]
    fn should_return_empty_vec_if_no_entries() {
        // arrange
        let map = HashMap::from([(
            "くるま".to_string(),
            vec![Word::new("車", "くるま", Speech::Noun(NounVariant::Common))],
        )]);
        let dic = TankanDictionary { kanji_map: map };

        // act
        let ret = dic.get_candidates("むち");

        // assert
        assert!(ret.is_empty(), "Expected empty vec, got {:?}", ret)
    }
}
