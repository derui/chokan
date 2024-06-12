// ひらがな・カタカナ・アルファベットそれぞれを変換するための情報をまとめる構造体
pub(crate) struct Conversion {
    hiragana: String,
    katakana: String,
    alphabets: Vec<String>,
}

impl Conversion {
    /// 渡された文字が[Conversion::hiragana]または[Conversion::katakana]と一致する場合、
    /// ローマ字を展開した文字列と、その文字列の長さを返す
    pub(crate) fn expand_roma(&self, c: &str) -> Option<(String, usize)> {
        if c.starts_with(&self.hiragana) || c.starts_with(&self.katakana) {
            let size = self.hiragana.chars().collect::<Vec<_>>().len();
            Some((self.alphabets[0].clone(), size))
        } else {
            None
        }
    }
}

pub(crate) fn get_conversions() -> Vec<Conversion> {
    let mut vec = vec![
        Conversion {
            hiragana: "あ".into(),
            katakana: "ア".into(),
            alphabets: vec!["a".to_string()],
        },
        Conversion {
            hiragana: "い".into(),
            katakana: "イ".into(),
            alphabets: vec!["i".to_string()],
        },
        Conversion {
            hiragana: "う".into(),
            katakana: "ウ".into(),
            alphabets: vec!["u".to_string()],
        },
        Conversion {
            hiragana: "え".into(),
            katakana: "エ".into(),
            alphabets: vec!["e".to_string()],
        },
        Conversion {
            hiragana: "お".into(),
            katakana: "オ".into(),
            alphabets: vec!["o".to_string()],
        },
        Conversion {
            hiragana: "か".into(),
            katakana: "カ".into(),
            alphabets: vec!["ka".to_string()],
        },
        Conversion {
            hiragana: "き".into(),
            katakana: "キ".into(),
            alphabets: vec!["ki".to_string()],
        },
        Conversion {
            hiragana: "く".into(),
            katakana: "ク".into(),
            alphabets: vec!["ku".to_string()],
        },
        Conversion {
            hiragana: "け".into(),
            katakana: "ケ".into(),
            alphabets: vec!["ke".to_string()],
        },
        Conversion {
            hiragana: "こ".into(),
            katakana: "コ".into(),
            alphabets: vec!["ko".to_string()],
        },
        Conversion {
            hiragana: "さ".into(),
            katakana: "サ".into(),
            alphabets: vec!["sa".to_string()],
        },
        Conversion {
            hiragana: "し".into(),
            katakana: "シ".into(),
            alphabets: vec!["si".to_string()],
        },
        Conversion {
            hiragana: "す".into(),
            katakana: "ス".into(),
            alphabets: vec!["su".to_string()],
        },
        Conversion {
            hiragana: "せ".into(),
            katakana: "セ".into(),
            alphabets: vec!["se".to_string()],
        },
        Conversion {
            hiragana: "そ".into(),
            katakana: "ソ".into(),
            alphabets: vec!["so".to_string()],
        },
        Conversion {
            hiragana: "た".into(),
            katakana: "タ".into(),
            alphabets: vec!["ta".to_string()],
        },
        Conversion {
            hiragana: "ち".into(),
            katakana: "チ".into(),
            alphabets: vec!["ti".to_string()],
        },
        Conversion {
            hiragana: "つ".into(),
            katakana: "ツ".into(),
            alphabets: vec!["tu".to_string()],
        },
        Conversion {
            hiragana: "て".into(),
            katakana: "テ".into(),
            alphabets: vec!["te".to_string()],
        },
        Conversion {
            hiragana: "と".into(),
            katakana: "ト".into(),
            alphabets: vec!["to".to_string()],
        },
        Conversion {
            hiragana: "な".into(),
            katakana: "ナ".into(),
            alphabets: vec!["na".to_string()],
        },
        Conversion {
            hiragana: "に".into(),
            katakana: "ニ".into(),
            alphabets: vec!["ni".to_string()],
        },
        Conversion {
            hiragana: "ぬ".into(),
            katakana: "ヌ".into(),
            alphabets: vec!["nu".to_string()],
        },
        Conversion {
            hiragana: "ね".into(),
            katakana: "ネ".into(),
            alphabets: vec!["ne".to_string()],
        },
        Conversion {
            hiragana: "の".into(),
            katakana: "ノ".into(),
            alphabets: vec!["no".to_string()],
        },
        Conversion {
            hiragana: "は".into(),
            katakana: "ハ".into(),
            alphabets: vec!["ha".to_string()],
        },
        Conversion {
            hiragana: "ひ".into(),
            katakana: "ヒ".into(),
            alphabets: vec!["hi".to_string()],
        },
        Conversion {
            hiragana: "ふ".into(),
            katakana: "フ".into(),
            alphabets: vec!["hu".to_string()],
        },
        Conversion {
            hiragana: "へ".into(),
            katakana: "ヘ".into(),
            alphabets: vec!["he".to_string()],
        },
        Conversion {
            hiragana: "ほ".into(),
            katakana: "ホ".into(),
            alphabets: vec!["ho".to_string()],
        },
        Conversion {
            hiragana: "ま".into(),
            katakana: "マ".into(),
            alphabets: vec!["ma".to_string()],
        },
        Conversion {
            hiragana: "み".into(),
            katakana: "ミ".into(),
            alphabets: vec!["mi".to_string()],
        },
        Conversion {
            hiragana: "む".into(),
            katakana: "ム".into(),
            alphabets: vec!["mu".to_string()],
        },
        Conversion {
            hiragana: "め".into(),
            katakana: "メ".into(),
            alphabets: vec!["me".to_string()],
        },
        Conversion {
            hiragana: "も".into(),
            katakana: "モ".into(),
            alphabets: vec!["mo".to_string()],
        },
        Conversion {
            hiragana: "や".into(),
            katakana: "ヤ".into(),
            alphabets: vec!["ya".to_string()],
        },
        Conversion {
            hiragana: "ゆ".into(),
            katakana: "ユ".into(),
            alphabets: vec!["yu".to_string()],
        },
        Conversion {
            hiragana: "よ".into(),
            katakana: "ヨ".into(),
            alphabets: vec!["yo".to_string()],
        },
        Conversion {
            hiragana: "ら".into(),
            katakana: "ラ".into(),
            alphabets: vec!["ra".to_string()],
        },
        Conversion {
            hiragana: "り".into(),
            katakana: "リ".into(),
            alphabets: vec!["ri".to_string()],
        },
        Conversion {
            hiragana: "る".into(),
            katakana: "ル".into(),
            alphabets: vec!["ru".to_string()],
        },
        Conversion {
            hiragana: "れ".into(),
            katakana: "レ".into(),
            alphabets: vec!["re".to_string()],
        },
        Conversion {
            hiragana: "ろ".into(),
            katakana: "ロ".into(),
            alphabets: vec!["ro".to_string()],
        },
        Conversion {
            hiragana: "わ".into(),
            katakana: "ワ".into(),
            alphabets: vec!["wa".to_string()],
        },
        Conversion {
            hiragana: "を".into(),
            katakana: "ヲ".into(),
            alphabets: vec!["wo".to_string()],
        },
        Conversion {
            hiragana: "ん".into(),
            katakana: "ン".into(),
            alphabets: vec!["n".to_string()],
        },
        Conversion {
            hiragana: "が".into(),
            katakana: "ガ".into(),
            alphabets: vec!["ga".to_string()],
        },
        Conversion {
            hiragana: "ぎ".into(),
            katakana: "ギ".into(),
            alphabets: vec!["gi".to_string()],
        },
        Conversion {
            hiragana: "ぐ".into(),
            katakana: "グ".into(),
            alphabets: vec!["gu".to_string()],
        },
        Conversion {
            hiragana: "げ".into(),
            katakana: "ゲ".into(),
            alphabets: vec!["ge".to_string()],
        },
        Conversion {
            hiragana: "ご".into(),
            katakana: "ゴ".into(),
            alphabets: vec!["go".to_string()],
        },
        Conversion {
            hiragana: "ざ".into(),
            katakana: "ザ".into(),
            alphabets: vec!["za".to_string()],
        },
        Conversion {
            hiragana: "じ".into(),
            katakana: "ジ".into(),
            alphabets: vec!["zi".to_string()],
        },
        Conversion {
            hiragana: "ず".into(),
            katakana: "ズ".into(),
            alphabets: vec!["zu".to_string()],
        },
        Conversion {
            hiragana: "ぜ".into(),
            katakana: "ゼ".into(),
            alphabets: vec!["ze".to_string()],
        },
        Conversion {
            hiragana: "ぞ".into(),
            katakana: "ゾ".into(),
            alphabets: vec!["zo".to_string()],
        },
        Conversion {
            hiragana: "だ".into(),
            katakana: "ダ".into(),
            alphabets: vec!["da".to_string()],
        },
        Conversion {
            hiragana: "ぢ".into(),
            katakana: "ヂ".into(),
            alphabets: vec!["di".to_string()],
        },
        Conversion {
            hiragana: "づ".into(),
            katakana: "ヅ".into(),
            alphabets: vec!["du".to_string()],
        },
        Conversion {
            hiragana: "で".into(),
            katakana: "デ".into(),
            alphabets: vec!["de".to_string()],
        },
        Conversion {
            hiragana: "ど".into(),
            katakana: "ド".into(),
            alphabets: vec!["do".to_string()],
        },
        Conversion {
            hiragana: "ば".into(),
            katakana: "バ".into(),
            alphabets: vec!["ba".to_string()],
        },
        Conversion {
            hiragana: "び".into(),
            katakana: "ビ".into(),
            alphabets: vec!["bi".to_string()],
        },
        Conversion {
            hiragana: "ぶ".into(),
            katakana: "ブ".into(),
            alphabets: vec!["bu".to_string()],
        },
        Conversion {
            hiragana: "べ".into(),
            katakana: "ベ".into(),
            alphabets: vec!["be".to_string()],
        },
        Conversion {
            hiragana: "ぼ".into(),
            katakana: "ボ".into(),
            alphabets: vec!["bo".to_string()],
        },
        Conversion {
            hiragana: "ぱ".into(),
            katakana: "パ".into(),
            alphabets: vec!["pa".to_string()],
        },
        Conversion {
            hiragana: "ぴ".into(),
            katakana: "ピ".into(),
            alphabets: vec!["pi".to_string()],
        },
        Conversion {
            hiragana: "ぷ".into(),
            katakana: "プ".into(),
            alphabets: vec!["pu".to_string()],
        },
        Conversion {
            hiragana: "ぺ".into(),
            katakana: "ペ".into(),
            alphabets: vec!["pe".to_string()],
        },
        Conversion {
            hiragana: "ぽ".into(),
            katakana: "ポ".into(),
            alphabets: vec!["po".to_string()],
        },
        Conversion {
            hiragana: "ぁ".into(),
            katakana: "ァ".into(),
            alphabets: vec!["la".to_string()],
        },
        Conversion {
            hiragana: "ぃ".into(),
            katakana: "ィ".into(),
            alphabets: vec!["li".to_string()],
        },
        Conversion {
            hiragana: "ぅ".into(),
            katakana: "ゥ".into(),
            alphabets: vec!["lu".to_string()],
        },
        Conversion {
            hiragana: "ぇ".into(),
            katakana: "ェ".into(),
            alphabets: vec!["le".to_string()],
        },
        Conversion {
            hiragana: "ぉ".into(),
            katakana: "ォ".into(),
            alphabets: vec!["lo".to_string()],
        },
        Conversion {
            hiragana: "ゃ".into(),
            katakana: "ャ".into(),
            alphabets: vec!["lya".to_string()],
        },
        Conversion {
            hiragana: "ゅ".into(),
            katakana: "ュ".into(),
            alphabets: vec!["lyu".to_string()],
        },
        Conversion {
            hiragana: "ょ".into(),
            katakana: "ョ".into(),
            alphabets: vec!["lyo".to_string()],
        },
        Conversion {
            hiragana: "っ".into(),
            katakana: "ッ".into(),
            alphabets: vec!["ltu".to_string()],
        },
        Conversion {
            hiragana: "ゎ".into(),
            katakana: "ヮ".into(),
            alphabets: vec!["lwa".to_string()],
        },
        Conversion {
            hiragana: "ゐ".into(),
            katakana: "ヰ".into(),
            alphabets: vec!["wi".to_string()],
        },
        Conversion {
            hiragana: "ゑ".into(),
            katakana: "ヱ".into(),
            alphabets: vec!["we".to_string()],
        },
        Conversion {
            hiragana: "ゔ".into(),
            katakana: "ヴ".into(),
            alphabets: vec!["vu".to_string()],
        },
        Conversion {
            hiragana: "ゕ".into(),
            katakana: "ヵ".into(),
            alphabets: vec!["lka".to_string()],
        },
        Conversion {
            hiragana: "ゖ".into(),
            katakana: "ヶ".into(),
            alphabets: vec!["lke".to_string()],
        },
        Conversion {
            hiragana: "きゃ".into(),
            katakana: "キャ".into(),
            alphabets: vec!["kya".to_string()],
        },
        Conversion {
            hiragana: "きゅ".into(),
            katakana: "キュ".into(),
            alphabets: vec!["kyu".to_string()],
        },
        Conversion {
            hiragana: "きょ".into(),
            katakana: "キョ".into(),
            alphabets: vec!["kyo".to_string()],
        },
        Conversion {
            hiragana: "しゃ".into(),
            katakana: "シャ".into(),
            alphabets: vec!["sya".to_string()],
        },
        Conversion {
            hiragana: "しゅ".into(),
            katakana: "シュ".into(),
            alphabets: vec!["syu".to_string()],
        },
        Conversion {
            hiragana: "しょ".into(),
            katakana: "ショ".into(),
            alphabets: vec!["syo".to_string()],
        },
        Conversion {
            hiragana: "ちゃ".into(),
            katakana: "チャ".into(),
            alphabets: vec!["tya".to_string()],
        },
        Conversion {
            hiragana: "ちゅ".into(),
            katakana: "チュ".into(),
            alphabets: vec!["tyu".to_string()],
        },
        Conversion {
            hiragana: "ちょ".into(),
            katakana: "チョ".into(),
            alphabets: vec!["tyo".to_string()],
        },
        Conversion {
            hiragana: "にゃ".into(),
            katakana: "ニャ".into(),
            alphabets: vec!["nya".to_string()],
        },
        Conversion {
            hiragana: "にゅ".into(),
            katakana: "ニュ".into(),
            alphabets: vec!["nyu".to_string()],
        },
        Conversion {
            hiragana: "にょ".into(),
            katakana: "ニョ".into(),
            alphabets: vec!["nyo".to_string()],
        },
        Conversion {
            hiragana: "ひゃ".into(),
            katakana: "ヒャ".into(),
            alphabets: vec!["hya".to_string()],
        },
        Conversion {
            hiragana: "ひゅ".into(),
            katakana: "ヒュ".into(),
            alphabets: vec!["hyu".to_string()],
        },
        Conversion {
            hiragana: "ひょ".into(),
            katakana: "ヒョ".into(),
            alphabets: vec!["hyo".to_string()],
        },
        Conversion {
            hiragana: "みゃ".into(),
            katakana: "ミャ".into(),
            alphabets: vec!["mya".to_string()],
        },
        Conversion {
            hiragana: "みゅ".into(),
            katakana: "ミュ".into(),
            alphabets: vec!["myu".to_string()],
        },
        Conversion {
            hiragana: "みょ".into(),
            katakana: "ミョ".into(),
            alphabets: vec!["myo".to_string()],
        },
        Conversion {
            hiragana: "りゃ".into(),
            katakana: "リャ".into(),
            alphabets: vec!["rya".to_string()],
        },
        Conversion {
            hiragana: "りゅ".into(),
            katakana: "リュ".into(),
            alphabets: vec!["ryu".to_string()],
        },
        Conversion {
            hiragana: "りょ".into(),
            katakana: "リョ".into(),
            alphabets: vec!["ryo".to_string()],
        },
        Conversion {
            hiragana: "ぎゃ".into(),
            katakana: "ギャ".into(),
            alphabets: vec!["gya".to_string()],
        },
        Conversion {
            hiragana: "ぎゅ".into(),
            katakana: "ギュ".into(),
            alphabets: vec!["gyu".to_string()],
        },
        Conversion {
            hiragana: "ぎょ".into(),
            katakana: "ギョ".into(),
            alphabets: vec!["gyo".to_string()],
        },
        Conversion {
            hiragana: "じゃ".into(),
            katakana: "ジャ".into(),
            alphabets: vec!["zya".to_string()],
        },
        Conversion {
            hiragana: "じゅ".into(),
            katakana: "ジュ".into(),
            alphabets: vec!["zyu".to_string()],
        },
        Conversion {
            hiragana: "じょ".into(),
            katakana: "ジョ".into(),
            alphabets: vec!["zyo".to_string()],
        },
        Conversion {
            hiragana: "びゃ".into(),
            katakana: "ビャ".into(),
            alphabets: vec!["bya".to_string()],
        },
        Conversion {
            hiragana: "びゅ".into(),
            katakana: "ビュ".into(),
            alphabets: vec!["byu".to_string()],
        },
        Conversion {
            hiragana: "びょ".into(),
            katakana: "ビョ".into(),
            alphabets: vec!["byo".to_string()],
        },
        Conversion {
            hiragana: "ぴゃ".into(),
            katakana: "ピャ".into(),
            alphabets: vec!["pya".to_string()],
        },
        Conversion {
            hiragana: "ぴゅ".into(),
            katakana: "ピュ".into(),
            alphabets: vec!["pyu".to_string()],
        },
        Conversion {
            hiragana: "ぴょ".into(),
            katakana: "ピョ".into(),
            alphabets: vec!["pyo".to_string()],
        },
    ];

    vec.sort_by(|a, b| b.hiragana.len().cmp(&a.hiragana.len()));

    return vec;
}
