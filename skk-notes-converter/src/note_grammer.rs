use dic::base::speech::VerbForm;
// SKKの辞書におけるnotesの一行分の文法を定義する
//
// notesの構成は以下のようになっている。
//
// [見出しかな]+[送りアルファベット]space*/語幹;∥品詞[[送り仮名情報][活用]](/...)/\n
//
use peg::{error::ParseError, str::LineCol};

#[derive(Debug, PartialEq, Eq)]
pub struct Note {
    pub headword: String,
    pub okuri: String,
    pub entries: Vec<NoteEntry>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NoteEntry {
    pub stem: String,
    pub speech: NoteSpeech,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum NoteSpeech {
    Verb(VerbForm, Option<Okuri>), // 活用形と、キャラクタクラス,を含めた動詞
    Adjective(Option<Okuri>),      // 活用形を含む形容詞
    AdjectivalVerb(Okuri),         // 形容動詞
    Adverb(Okuri),                 // 副詞
    Noun(String, Option<Okuri>),   // 名詞の種類と、語幹の後に続く文字を含めた名詞
    Counter(Okuri),                // 助数詞
    Verbatim(Okuri),               // 感動詞
    PreNounAdjectival(Option<Okuri>), // 連体詞
}

// 送り仮名の情報
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Okuri {
    Fixed(String),     // -いる、-えるなどの固定の送り仮名
    CharClass(String), // []で囲まれたキャラクタクラス
}

peg::parser! {

  grammar note_parser() for str {
      rule eof() = ![_]
      rule any() = [_]
      rule space() = [' ' | '\t' ]
      rule kana() -> String = n:$(['あ'..='ん' | 'ゐ' | 'ゃ' | 'ゅ' | 'ょ' | 'ぁ' | 'ぃ' | 'ぅ' | 'ぇ' | 'ぉ' | 'っ']) { n.to_string() }
      rule katakana() -> String = n:$(['ア' |'カ'| 'サ' | 'タ' |'ナ' | 'ハ'|'マ'|'ヤ'|'ワ' | 'ラ' | 'ダ' | 'バ' | 'ガ' | 'ザ']) { n.to_string() }

    pub rule headword() -> String
      = n:kana()+ { n.concat() }

      pub rule okuri_alpha() -> String = n:$(['a'..='z']) { n.to_string() }
      // 語幹の漢字部分
      pub rule stem() -> String = n:$([^ ';']+) { n.to_string() }
      // 品詞部分
      rule fixed_okuri() -> Okuri = "(-" n:$(kana()+) ")" { Okuri::Fixed(n.to_string()) }
      rule char_class() -> Okuri = "[" n:$(['a'..='z' | '>' | '<' | '#' | '*' | '-' | '(' | ')' | 'φ' | '.']+) "]" { Okuri::CharClass(n.to_string()) }
      rule okuri() -> Okuri = n:(fixed_okuri() / char_class()) { n }
            rule noun() -> NoteSpeech = t:$("サ変名詞" / "名詞" / "人称代名詞" / "疑問代名詞" / "連語" / "複合語" / "成句")  o:okuri()? { NoteSpeech::Noun(t.to_string(), o) }
      rule verb_form() -> VerbForm = k:katakana() n:$("行五段" / "行四段" / "行上一" / "行下一" /"行上二" / "行下二" / "変" ) {?
          match n {
              "行五段" => Ok(VerbForm::Godan(k)),
              "行四段" => Ok(VerbForm::Yodan(k)),
              "行上一" => Ok(VerbForm::KamiIchidan(k)),
              "行下一" => Ok(VerbForm::SimoIchidan(k)),
              "行上二" => Ok(VerbForm::KamiNidan(k)),
              "行下二" => Ok(VerbForm::SimoNidan(k)),
              "変" => Ok(VerbForm::Hen(k)),
              _ => Err("Invalid verb form")
          }
      }
      rule note_in_entry() = space()* "¶" [^ '/']+
      rule verb() -> NoteSpeech = t:verb_form() o:okuri()?  { NoteSpeech::Verb(t, o) }
      rule adjective() -> NoteSpeech = "形容詞" o:okuri()? { NoteSpeech::Adjective(o) }
      rule adverb() -> NoteSpeech =  "副詞"  o:okuri() { NoteSpeech::Adverb(o) }
      rule adjectival_verb() -> NoteSpeech = "形容動詞" o:okuri() { NoteSpeech::AdjectivalVerb(o) }
      rule counter() -> NoteSpeech = "助数詞" o:okuri() { NoteSpeech::Counter(o) }
      rule verbatim() -> NoteSpeech = "感動詞" o:okuri() { NoteSpeech::Verbatim(o) }
      rule pre_noun_adjectival() -> NoteSpeech = "連体詞" o:okuri()? { NoteSpeech::PreNounAdjectival(o) }
      rule speech() -> Vec<NoteSpeech> = "∥" "<base>"? "(文語)"? n:(noun() / verb() / adjective() / adjectival_verb() / counter() / verbatim() / pre_noun_adjectival()) ** "," note_in_entry()? { n }
      rule annotation() = ";" [^ '∥']*
      // 送りなしエントリは今回取り扱わない
      rule okuri_nasi() = "∥<okuri-nasi>" [^ '/']*
      rule okuri_nasi_entry() -> Vec<NoteEntry> = "/" s:stem() annotation() okuri_nasi() {vec![]}
      rule entry() -> Vec<NoteEntry> = "/" s:stem() annotation() ss:speech() { ss.iter().map(|v| NoteEntry { stem: s.clone(), speech: v.clone() }).collect() }
      pub rule note() -> Note = h:headword() o:okuri_alpha()? space()+ entries:(okuri_nasi_entry() / entry())+ "/" eof() {
          let entries = entries.iter().filter(|v| v.len() > 0).flatten().cloned().collect();
          let okuri = o.map(|v|v.to_string()).unwrap_or("".to_string());
          Note { headword: h, okuri , entries  } }

      pub rule root() -> Option<Note> = comment() {None} / n:note() { if n.entries.len() > 0 { Some(n) } else { None } }

      pub rule comment() = ";" any()*
  }
}

/// 対象の一行を解析して、Noteを返す
pub fn parse_note(s: &str) -> Result<Option<Note>, ParseError<LineCol>> {
    note_parser::root(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_headword() {
        assert_eq!(note_parser::headword("あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわをん"),
            Ok("あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわをん".to_string())
        );
        assert_eq!(
            note_parser::headword("がぎぐげござじずぜぞだぢづでどばびぶべぼ"),
            Ok("がぎぐげござじずぜぞだぢづでどばびぶべぼ".to_string())
        );
        assert_eq!(
            note_parser::headword("しゃしゅしょぁぃぅぇぉっ"),
            Ok("しゃしゅしょぁぃぅぇぉっ".to_string())
        );
    }

    #[test]
    fn parse_okuri() {
        for (_, c) in "abcdefghijklmnopqrstuvwxyz".chars().enumerate() {
            assert_eq!(note_parser::okuri_alpha(&c.to_string()), Ok(c.to_string()));
        }

        // 送りのアルファベットは一つしかつかない
        assert_ne!(note_parser::okuri_alpha("abc"), Ok("a".to_string()));
    }

    #[test]
    fn parse_comment() {
        assert_eq!(note_parser::comment(";; abc"), Ok(()));
        assert_ne!(note_parser::comment("abc ; comments in back"), Ok(()));
    }

    #[test]
    fn parse_note() {
        assert_eq!(
            note_parser::note("をs /惜;∥形容詞(-しい)/"),
            Ok(Note {
                headword: "を".to_string(),
                okuri: "s".to_string(),
                entries: vec![NoteEntry {
                    stem: "惜".to_string(),
                    speech: NoteSpeech::Adjective(Some(Okuri::Fixed("しい".to_string())))
                }]
            })
        );

        assert_eq!(
            note_parser::note("ゐr /居;∥ラ変/"),
            Ok(Note {
                headword: "ゐ".to_string(),
                okuri: "r".to_string(),
                entries: vec![NoteEntry {
                    stem: "居".to_string(),
                    speech: NoteSpeech::Verb(VerbForm::Hen("ラ".to_string()), None)
                }]
            })
        );

        assert_eq!(note_parser::note("わらu /笑;∥<base>ワ行五段[wiueot(c)]/嗤;(jeer) 人の失敗を嗤う∥<base>ワ行五段[wiueot(c)]/"), Ok(Note {
            headword: "わら".to_string(),
            okuri: "u".to_string(),
            entries: vec![
                NoteEntry {
                    stem: "笑".to_string(),
                    speech: NoteSpeech::Verb(VerbForm::Godan("ワ".to_string()), Some(Okuri::CharClass("wiueot(c)".to_string())))
                },
                NoteEntry {
                    stem: "嗤".to_string(),
                    speech: NoteSpeech::Verb(VerbForm::Godan("ワ".to_string()), Some(Okuri::CharClass("wiueot(c)".to_string())))
                },
            ]
        }));

        assert_eq!(
            note_parser::note("わせだどおr /早稲田通;∥名詞(-り)/"),
            Ok(Note {
                headword: "わせだどお".to_string(),
                okuri: "r".to_string(),
                entries: vec![NoteEntry {
                    stem: "早稲田通".to_string(),
                    speech: NoteSpeech::Noun(
                        "名詞".to_string(),
                        Some(Okuri::Fixed("り".to_string()))
                    )
                }]
            })
        );

        assert_eq!(
            note_parser::note("わけあたe /分け与;∥ア行下一(-える)/"),
            Ok(Note {
                headword: "わけあた".to_string(),
                okuri: "e".to_string(),
                entries: vec![NoteEntry {
                    stem: "分け与".to_string(),
                    speech: NoteSpeech::Verb(
                        VerbForm::SimoIchidan("ア".to_string()),
                        Some(Okuri::Fixed("える".to_string()))
                    )
                }]
            })
        );

        assert_eq!(
            note_parser::note("えんきょう /円強;∥助数詞[#]/"),
            Ok(Note {
                headword: "えんきょう".to_string(),
                okuri: "".to_string(),
                entries: vec![NoteEntry {
                    stem: "円強".to_string(),
                    speech: NoteSpeech::Counter(Okuri::CharClass("#".to_string()))
                }]
            })
        );

        assert_eq!(
            note_parser::note("おんみつ /隠密;∥形容動詞[φdn(s)],名詞/"),
            Ok(Note {
                headword: "おんみつ".to_string(),
                okuri: "".to_string(),
                entries: vec![
                    NoteEntry {
                        stem: "隠密".to_string(),
                        speech: NoteSpeech::AdjectivalVerb(Okuri::CharClass("φdn(s)".to_string()))
                    },
                    NoteEntry {
                        stem: "隠密".to_string(),
                        speech: NoteSpeech::Noun("名詞".to_string(), None)
                    }
                ]
            })
        );

        assert_eq!(
            note_parser::note("ありがとu /有り難;∥感動詞(-う)/有難;∥感動詞(-う)/"),
            Ok(Note {
                headword: "ありがと".to_string(),
                okuri: "u".to_string(),
                entries: vec![
                    NoteEntry {
                        stem: "有り難".to_string(),
                        speech: NoteSpeech::Verbatim(Okuri::Fixed("う".to_string()))
                    },
                    NoteEntry {
                        stem: "有難".to_string(),
                        speech: NoteSpeech::Verbatim(Okuri::Fixed("う".to_string()))
                    }
                ]
            })
        );

        assert_eq!(
            note_parser::note("ゆめn /夢;∥連体詞(-の)/"),
            Ok(Note {
                headword: "ゆめ".to_string(),
                okuri: "n".to_string(),
                entries: vec![NoteEntry {
                    stem: "夢".to_string(),
                    speech: NoteSpeech::PreNounAdjectival(Some(Okuri::Fixed("の".to_string())))
                }]
            })
        );
        assert_eq!(
            note_parser::note("わるよi /悪酔;∥<okuri-nasi> -い/"),
            Ok(Note {
                headword: "わるよ".to_string(),
                okuri: "i".to_string(),
                entries: vec![]
            })
        )
    }

    #[test]
    fn parse_error_derived() {
        assert!(
            note_parser::note("わらe /笑;∥<derived>/").is_err(),
            "should be error if derived"
        )
    }

    #[test]
    fn ignore_note_in_entry() {
        assert_eq!(
            note_parser::note("わけなi /訳無;∥形容詞[iks] ¶実質kのみ/"),
            Ok(Note {
                headword: "わけな".to_string(),
                okuri: "i".to_string(),
                entries: vec![NoteEntry {
                    stem: "訳無".to_string(),
                    speech: NoteSpeech::Adjective(Some(Okuri::CharClass("iks".to_string())))
                }]
            })
        )
    }
}
