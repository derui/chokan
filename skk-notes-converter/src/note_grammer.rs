// SKKの辞書におけるnotesの一行分の文法を定義する
//
// notesの構成は以下のようになっている。
//
// [見出しかな]+[送りアルファベット]space*/語幹;∥品詞[[送り仮名情報][活用]](/...)/\n
//

peg::parser! {

  grammar note_parser() for str {
      rule kana() -> String = n:$(['あ'..='ん' | 'ゃ' | 'ゅ' | 'ょ' | 'ぁ' | 'ぃ' | 'ぅ' | 'ぇ' | 'ぉ' | 'っ']) { n.to_string() }

    pub rule midashi() -> String
      = n:kana()+ { n.concat() }

      pub rule okuri() -> String = n:$(['a'..='z']) { n.to_string() }
  }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_midashi() {
        assert_eq!(note_parser::midashi("あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわをん"),
            Ok("あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわをん".to_string())
        );
        assert_eq!(
            note_parser::midashi("がぎぐげござじずぜぞだぢづでどばびぶべぼ"),
            Ok("がぎぐげござじずぜぞだぢづでどばびぶべぼ".to_string())
        );
        assert_eq!(
            note_parser::midashi("しゃしゅしょぁぃぅぇぉっ"),
            Ok("しゃしゅしょぁぃぅぇぉっ".to_string())
        );
    }

    #[test]
    fn parse_okuri() {
        for (_, c) in "abcdefghijklmnopqrstuvwxyz".chars().enumerate() {
            assert_eq!(note_parser::okuri(&c.to_string()), Ok(c.to_string()));
        }

        // 送りのアルファベットは一つしかつかない
        assert_ne!(note_parser::okuri("abc"), Ok("a".to_string()));
    }
}
