use dic::base::{
    entry::Entry,
    speech::{NounVariant, Speech, VerbForm},
};
// SKKの辞書におけるnotesの一行分の文法を定義する
//
// notesの構成は以下のようになっている。
//
// [見出しかな]+[送りアルファベット]space*/語幹;∥品詞[[送り仮名情報][活用]](/...)/\n
//
use peg::{error::ParseError, str::LineCol};

/*
単漢字のエントリ。送り有りはすべて無視する
*/
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Tankan {
    reading: String,
    entries: Vec<String>,
}

impl Tankan {
    /*
    単漢字を、全体の辞書で利用するentry形式に変換する
     */
    pub(crate) fn to_entries(&self) -> Vec<Entry> {
        self.entries
            .iter()
            .map(|e| Entry::from_jisyo(&self.reading, e, Speech::Noun(NounVariant::Common)))
            .collect()
    }
}

peg::parser! {

  grammar tankan_parser() for str {
      rule eof() = ![_]
      rule any() = [_]
      rule space() = [' ' | '\t' ]
      rule kana() -> String = n:$(['あ'..='ん' | 'ゐ' | 'ゃ' | 'ゅ' | 'ょ' | 'ぁ' | 'ぃ' | 'ぅ' | 'ぇ' | 'ぉ' | 'っ' | 'ー']) { n.to_string() }

      rule reading() -> String = n:kana()+ { n.concat() }
      rule kanji() -> String = n:$([^ ' ' | '/']+) "/" { n.to_string() }
      rule entry() -> Tankan = r:reading() space()+ "/" s:kanji()+ {
          Tankan {reading: r, entries: s}
      }

      pub rule root() -> Option<Tankan> = comment() {None} / n:entry() { Some(n) }

       rule comment() = ";" any()* "\n"
  }
}

/// 対象の一行を解析して、Noteを返す
pub fn parse_tankan(s: &str) -> Result<Option<Tankan>, ParseError<LineCol>> {
    if let Some(tankan) = tankan_parser::root(s)? {
        // 一文字ではない場合は単漢字とみなさない
        let entries = tankan
            .entries
            .iter()
            .cloned()
            .filter(|v| v.chars().collect::<Vec<char>>().len() == 1)
            .collect::<Vec<_>>();

        if entries.is_empty() {
            Ok(None)
        } else {
            Ok(Some(Tankan {
                reading: tankan.reading,
                entries,
            }))
        }
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn parse_entry() {
        assert_eq!(
            tankan_parser::root("さる /猿/"),
            Ok(Some(Tankan {
                reading: "さる".to_string(),
                entries: vec!["猿".to_string()]
            }))
        );

        assert_eq!(
            tankan_parser::root("さつ /冊/札/"),
            Ok(Some(Tankan {
                reading: "さつ".to_string(),
                entries: vec!["冊".to_string(), "札".to_string()]
            }))
        );
    }

    #[test]
    fn ignore_multi_character_word() {
        assert_eq!(
            parse_tankan("むち /鞭/無知/"),
            Ok(Some(Tankan {
                reading: "むち".to_string(),
                entries: vec!["鞭".to_string()]
            }))
        );

        assert_eq!(parse_tankan("むち /無知/"), Ok(None));
    }
}
