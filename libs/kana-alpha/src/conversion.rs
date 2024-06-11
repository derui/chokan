// ひらがな・カタカナ・アルファベットそれぞれを変換するための情報をまとめる構造体
pub(crate) struct Conversion {
    hiragana: char,
    katakana: char,
    alphabets: Vec<String>,
}

impl Conversion {
    /// 渡された文字が[Conversion::hiragana]または[Conversion::katakana]と一致する場合、
    /// ローマ字を展開した文字列を返す
    pub(crate) fn expand_roma(&self, c: char) -> Option<String> {
        if self.hiragana == c || self.katakana == c {
            Some(self.alphabets[0].clone())
        } else {
            None
        }
    }
}

pub(crate) fn get_conversions() -> Vec<Conversion> {
    vec![
        Conversion {
            hiragana: 'あ',
            katakana: 'ア',
            alphabets: vec!["a".to_string()],
        },
        Conversion {
            hiragana: 'い',
            katakana: 'イ',
            alphabets: vec!["i".to_string()],
        },
        Conversion {
            hiragana: 'う',
            katakana: 'ウ',
            alphabets: vec!["u".to_string()],
        },
        Conversion {
            hiragana: 'え',
            katakana: 'エ',
            alphabets: vec!["e".to_string()],
        },
        Conversion {
            hiragana: 'お',
            katakana: 'オ',
            alphabets: vec!["o".to_string()],
        },
        Conversion {
            hiragana: 'か',
            katakana: 'カ',
            alphabets: vec!["ka".to_string()],
        },
        Conversion {
            hiragana: 'き',
            katakana: 'キ',
            alphabets: vec!["ki".to_string()],
        },
        Conversion {
            hiragana: 'く',
            katakana: 'ク',
            alphabets: vec!["ku".to_string()],
        },
        Conversion {
            hiragana: 'け',
            katakana: 'ケ',
            alphabets: vec!["ke".to_string()],
        },
        Conversion {
            hiragana: 'こ',
            katakana: 'コ',
            alphabets: vec!["ko".to_string()],
        },
        Conversion {
            hiragana: 'さ',
            katakana: 'サ',
            alphabets: vec!["sa".to_string()],
        },
        Conversion {
            hiragana: 'し',
            katakana: 'シ',
            alphabets: vec!["si".to_string()],
        },
        Conversion {
            hiragana: 'す',
            katakana: 'ス',
            alphabets: vec!["su".to_string()],
        },
        Conversion {
            hiragana: 'せ',
            katakana: 'セ',
            alphabets: vec!["se".to_string()],
        },
        Conversion {
            hiragana: 'そ',
            katakana: 'ソ',
            alphabets: vec!["so".to_string()],
        },
        Conversion {
            hiragana: 'た',
            katakana: 'タ',
            alphabets: vec!["ta".to_string()],
        },
        Conversion {
            hiragana: 'ち',
            katakana: 'チ',
            alphabets: vec!["ti".to_string()],
        },
        Conversion {
            hiragana: 'つ',
            katakana: 'ツ',
            alphabets: vec!["tu".to_string()],
        },
        Conversion {
            hiragana: 'て',
            katakana: 'テ',
            alphabets: vec!["te".to_string()],
        },
        Conversion {
            hiragana: 'と',
            katakana: 'ト',
            alphabets: vec!["to".to_string()],
        },
        Conversion {
            hiragana: 'な',
            katakana: 'ナ',
            alphabets: vec!["na".to_string()],
        },
        Conversion {
            hiragana: 'に',
            katakana: 'ニ',
            alphabets: vec!["ni".to_string()],
        },
        Conversion {
            hiragana: 'ぬ',
            katakana: 'ヌ',
            alphabets: vec!["nu".to_string()],
        },
        Conversion {
            hiragana: 'ね',
            katakana: 'ネ',
            alphabets: vec!["ne".to_string()],
        },
        Conversion {
            hiragana: 'の',
            katakana: 'ノ',
            alphabets: vec!["no".to_string()],
        },
        Conversion {
            hiragana: 'は',
            katakana: 'ハ',
            alphabets: vec!["ha".to_string()],
        },
        Conversion {
            hiragana: 'ひ',
            katakana: 'ヒ',
            alphabets: vec!["hi".to_string()],
        },
        Conversion {
            hiragana: 'ふ',
            katakana: 'フ',
            alphabets: vec!["hu".to_string()],
        },
        Conversion {
            hiragana: 'へ',
            katakana: 'ヘ',
            alphabets: vec!["he".to_string()],
        },
        Conversion {
            hiragana: 'ほ',
            katakana: 'ホ',
            alphabets: vec!["ho".to_string()],
        },
        Conversion {
            hiragana: 'ま',
            katakana: 'マ',
            alphabets: vec!["ma".to_string()],
        },
        Conversion {
            hiragana: 'み',
            katakana: 'ミ',
            alphabets: vec!["mi".to_string()],
        },
        Conversion {
            hiragana: 'む',
            katakana: 'ム',
            alphabets: vec!["mu".to_string()],
        },
        Conversion {
            hiragana: 'め',
            katakana: 'メ',
            alphabets: vec!["me".to_string()],
        },
        Conversion {
            hiragana: 'も',
            katakana: 'モ',
            alphabets: vec!["mo".to_string()],
        },
        Conversion {
            hiragana: 'や',
            katakana: 'ヤ',
            alphabets: vec!["ya".to_string()],
        },
        Conversion {
            hiragana: 'ゆ',
            katakana: 'ユ',
            alphabets: vec!["yu".to_string()],
        },
        Conversion {
            hiragana: 'よ',
            katakana: 'ヨ',
            alphabets: vec!["yo".to_string()],
        },
        Conversion {
            hiragana: 'ら',
            katakana: 'ラ',
            alphabets: vec!["ra".to_string()],
        },
        Conversion {
            hiragana: 'り',
            katakana: 'リ',
            alphabets: vec!["ri".to_string()],
        },
        Conversion {
            hiragana: 'る',
            katakana: 'ル',
            alphabets: vec!["ru".to_string()],
        },
        Conversion {
            hiragana: 'れ',
            katakana: 'レ',
            alphabets: vec!["re".to_string()],
        },
        Conversion {
            hiragana: 'ろ',
            katakana: 'ロ',
            alphabets: vec!["ro".to_string()],
        },
        Conversion {
            hiragana: 'わ',
            katakana: 'ワ',
            alphabets: vec!["wa".to_string()],
        },
        Conversion {
            hiragana: 'を',
            katakana: 'ヲ',
            alphabets: vec!["wo".to_string()],
        },
        Conversion {
            hiragana: 'ん',
            katakana: 'ン',
            alphabets: vec!["n".to_string()],
        },
        Conversion {
            hiragana: 'が',
            katakana: 'ガ',
            alphabets: vec!["ga".to_string()],
        },
        Conversion {
            hiragana: 'ぎ',
            katakana: 'ギ',
            alphabets: vec!["gi".to_string()],
        },
        Conversion {
            hiragana: 'ぐ',
            katakana: 'グ',
            alphabets: vec!["gu".to_string()],
        },
        Conversion {
            hiragana: 'げ',
            katakana: 'ゲ',
            alphabets: vec!["ge".to_string()],
        },
        Conversion {
            hiragana: 'ご',
            katakana: 'ゴ',
            alphabets: vec!["go".to_string()],
        },
        Conversion {
            hiragana: 'ざ',
            katakana: 'ザ',
            alphabets: vec!["za".to_string()],
        },
        Conversion {
            hiragana: 'じ',
            katakana: 'ジ',
            alphabets: vec!["zi".to_string()],
        },
        Conversion {
            hiragana: 'ず',
            katakana: 'ズ',
            alphabets: vec!["zu".to_string()],
        },
        Conversion {
            hiragana: 'ぜ',
            katakana: 'ゼ',
            alphabets: vec!["ze".to_string()],
        },
        Conversion {
            hiragana: 'ぞ',
            katakana: 'ゾ',
            alphabets: vec!["zo".to_string()],
        },
        Conversion {
            hiragana: 'だ',
            katakana: 'ダ',
            alphabets: vec!["da".to_string()],
        },
        Conversion {
            hiragana: 'ぢ',
            katakana: 'ヂ',
            alphabets: vec!["di".to_string()],
        },
        Conversion {
            hiragana: 'づ',
            katakana: 'ヅ',
            alphabets: vec!["du".to_string()],
        },
        Conversion {
            hiragana: 'で',
            katakana: 'デ',
            alphabets: vec!["de".to_string()],
        },
        Conversion {
            hiragana: 'ど',
            katakana: 'ド',
            alphabets: vec!["do".to_string()],
        },
        Conversion {
            hiragana: 'ば',
            katakana: 'バ',
            alphabets: vec!["ba".to_string()],
        },
        Conversion {
            hiragana: 'び',
            katakana: 'ビ',
            alphabets: vec!["bi".to_string()],
        },
        Conversion {
            hiragana: 'ぶ',
            katakana: 'ブ',
            alphabets: vec!["bu".to_string()],
        },
        Conversion {
            hiragana: 'べ',
            katakana: 'ベ',
            alphabets: vec!["be".to_string()],
        },
        Conversion {
            hiragana: 'ぼ',
            katakana: 'ボ',
            alphabets: vec!["bo".to_string()],
        },
        Conversion {
            hiragana: 'ぱ',
            katakana: 'パ',
            alphabets: vec!["pa".to_string()],
        },
        Conversion {
            hiragana: 'ぴ',
            katakana: 'ピ',
            alphabets: vec!["pi".to_string()],
        },
        Conversion {
            hiragana: 'ぷ',
            katakana: 'プ',
            alphabets: vec!["pu".to_string()],
        },
        Conversion {
            hiragana: 'ぺ',
            katakana: 'ペ',
            alphabets: vec!["pe".to_string()],
        },
        Conversion {
            hiragana: 'ぽ',
            katakana: 'ポ',
            alphabets: vec!["po".to_string()],
        },
        Conversion {
            hiragana: 'ぁ',
            katakana: 'ァ',
            alphabets: vec!["la".to_string()],
        },
        Conversion {
            hiragana: 'ぃ',
            katakana: 'ィ',
            alphabets: vec!["li".to_string()],
        },
        Conversion {
            hiragana: 'ぅ',
            katakana: 'ゥ',
            alphabets: vec!["lu".to_string()],
        },
        Conversion {
            hiragana: 'ぇ',
            katakana: 'ェ',
            alphabets: vec!["le".to_string()],
        },
        Conversion {
            hiragana: 'ぉ',
            katakana: 'ォ',
            alphabets: vec!["lo".to_string()],
        },
        Conversion {
            hiragana: 'ゃ',
            katakana: 'ャ',
            alphabets: vec!["lya".to_string()],
        },
        Conversion {
            hiragana: 'ゅ',
            katakana: 'ュ',
            alphabets: vec!["lyu".to_string()],
        },
        Conversion {
            hiragana: 'ょ',
            katakana: 'ョ',
            alphabets: vec!["lyo".to_string()],
        },
        Conversion {
            hiragana: 'っ',
            katakana: 'ッ',
            alphabets: vec!["ltu".to_string()],
        },
    ]
}
