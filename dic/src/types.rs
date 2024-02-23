
// 辞書全体を構成する型。
// この型は、辞書ファイルを読み込んで構築され、またimmutableな編集ができる。
#[derive(Debug)]
pub struct Dictionary {
    pub words: Vec<Word>,
}

// 単語における品詞
#[derive(Debug)]
pub enum Speech {
    Noun,                       // 名詞
    Verb,                       // 動詞
    Adjective,                  // 形容詞
    Adverb,                     // 副詞
    AdjectivalVerb,             // 形容動詞
    Verbatim,                   // 感動詞
    Conjunction,                // 接続詞
    Particle,                   // 助詞
    AuxiliaryVerb,              // 助動詞
    PreNounAdjectival,          // 連体詞
}

#[derive(Debug)]
pub struct Word {
    pub word: String,
    pub speech: Speech,
}
