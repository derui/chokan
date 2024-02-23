// 単語における品詞
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Speech {
    Noun,              // 名詞
    Verb,              // 動詞
    Adjective,         // 形容詞
    Adverb,            // 副詞
    AdjectivalVerb,    // 形容動詞
    Verbatim,          // 感動詞
    Conjunction,       // 接続詞
    Particle,          // 助詞
    AuxiliaryVerb,     // 助動詞
    PreNounAdjectival, // 連体詞
}
