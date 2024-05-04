// SKKの辞書についての文法を定義する
//
// [見出しかな]+[送りアルファベット]space*/[漢字](/...)/\n
//
use peg::{error::ParseError, str::LineCol};

// 一行分の辞書エントリー
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SkkEntry {
    reading: String,
    okuri: Option<String>,
    words: Vec<String>,
}

impl SkkEntry {
    pub fn reading(&self) -> String {
        self.reading.clone()
    }

    pub fn okuri(&self) -> Option<String> {
        self.okuri.clone()
    }

    pub fn words(&self) -> Vec<String> {
        self.words.clone()
    }
}

peg::parser! {

  grammar skk_dic_parser() for str {
      rule eof() = ![_]
      rule any() = [_]
      rule space() = [' ' | '\t' ]
      rule kana() -> String = n:$(['あ'..='ん' | 'ゐ' | 'ゃ' | 'ゅ' | 'ょ' | 'ぁ' | 'ぃ' | 'ぅ' | 'ぇ' | 'ぉ' | 'っ' | 'ー']) { n.to_string() }
      rule alphabet() -> String = n:$(['a'..='z']) { n.to_string() }
      rule annotation() = ";" [^ '/']*

      rule reading() -> String = n:kana()+ { n.concat() }
      rule okuri() -> Option<String> = n:alphabet()* { if n.is_empty() { None } else { Some(n.concat()) } }
      rule kanji() -> String = n:$([^ ' ' | '/' | ';']+) annotation()? "/" { n.to_string() }
      rule entry() -> SkkEntry = r:reading() o:okuri() space()+ "/" s:kanji()+ {
          SkkEntry {reading: r, okuri: o, words: s}
      }

      pub rule root() -> Option<SkkEntry> = comment() {None} / n:entry() { Some(n) }

      rule comment() = ";" any()* "\n"
  }
}

/// 対象の一行を解析して、SkkEntryを返す
pub fn parse_skk_entry(s: &str) -> Result<Option<SkkEntry>, ParseError<LineCol>> {
    skk_dic_parser::root(s)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn parse_entry() {
        assert_eq!(
            skk_dic_parser::root("さる /猿/"),
            Ok(Some(SkkEntry {
                reading: "さる".to_string(),
                okuri: None,
                words: vec!["猿".to_string()]
            }))
        );

        assert_eq!(
            skk_dic_parser::root("さつn /冊/札/"),
            Ok(Some(SkkEntry {
                reading: "さつ".to_string(),
                okuri: Some("n".to_string()),
                words: vec!["冊".to_string(), "札".to_string()]
            }))
        );
    }

    #[test]
    fn parse_entry_with_root_function() {
        assert_eq!(
            parse_skk_entry("むち /鞭/無知/"),
            Ok(Some(SkkEntry {
                reading: "むち".to_string(),
                okuri: None,
                words: vec!["鞭".to_string(), "無知".to_string()]
            }))
        );
    }

    #[test]
    fn ignore_annotation() {
        assert_eq!(
            parse_skk_entry("むち /鞭;名詞/無知/"),
            Ok(Some(SkkEntry {
                reading: "むち".to_string(),
                okuri: None,
                words: vec!["鞭".to_string(), "無知".to_string()]
            }))
        );
    }
}
