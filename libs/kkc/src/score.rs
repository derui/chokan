use dic::base::{
    speech::{AffixVariant, ParticleType, Speech},
    word::Word,
};

use crate::{context::Context, graph::Node};

/**
node自体のscoreと、2node間 = edgeのscoreを計算する関数を定義する

ここで定義する関数は、基本的に決定論的な手法を前提としていて、頻度分析などは対象外である。
ただし、ユーザーの入力してきた単語や繋がりの頻度などは考慮する。
*/

/// 単語同士が、文法上接続可能かどうかを返す
///
/// ここでの接続可能性は、基本的に文節を構成可能かどうか、に主眼が置かれている。
///
/// # Arguments
/// * `context` - かな漢字変換を実施するときのcontext
/// * `current` - 接続可能かどうかを調べる単語
/// * `prev` - currentの一つ前の単語
///
/// # Returns
/// 接続可能ならtrue、そうでないならfalse
pub fn get_edge_score(context: &Context, prev: &Option<Node>, current: &Node) -> u32 {
    match prev {
        Some(prev) => get_edge_score_impl(context, prev, current),
        None => 0,
    }
}

fn get_edge_score_impl(context: &Context, prev: &Node, current: &Node) -> u32 {
    match (prev, current) {
        (Node::WordNode(prev, _), Node::WordNode(current, _)) => {
            get_edge_score_between_words(context, prev, current)
        }
        (Node::WordNode(prev, _), Node::Virtual(_, _)) => {
            get_edge_score_allow_virtual_word(context, prev)
        }
        _ => 0,
    }
}

fn get_edge_score_allow_virtual_word(context: &Context, prev: &Word) -> u32 {
    // 仮想nodeが続く場合、対象が文節末を構成しうる場合は許容する
    match &prev.speech {
        Speech::Verb(_) => 1,
        Speech::Noun(_) => 1,
        Speech::Noun(_) => 1,
        _ => 0,
    }
}

fn get_edge_score_between_words(context: &Context, prev: &Word, current: &Word) -> u32 {
    match (&prev.speech, &current.speech) {
        // 格助詞は体言の後につく
        (Speech::Noun(_), Speech::Particle(ParticleType::Case)) => 2,
        // 接続助詞は用言または助動詞の後につく
        (Speech::Verb(_), Speech::Particle(ParticleType::Conjunctive)) => 1,
        ((Speech::AuxiliaryVerb), Speech::Particle(ParticleType::Conjunctive)) => 1,
        // 副助詞は色々つくことができる
        ((_), Speech::Particle(ParticleType::Adverbial)) => 1,
        // 終助詞は文末につくが、文末を保証するのがむずかしいので、副助詞と同様にする
        ((_), Speech::Particle(ParticleType::SentenceFinal)) => 1,
        // 助動詞は用言の後につく
        ((Speech::Verb(_)), Speech::AuxiliaryVerb) => 2,
        // 接頭辞は動詞または名詞の前につく
        ((Speech::Affix(AffixVariant::Prefix)), Speech::Noun(_)) => 2,
        ((Speech::Affix(AffixVariant::Prefix)), Speech::Verb(_)) => 2,
        // 接尾辞は動詞または名詞の後につく。ただしかな漢字変換では、基本的に接尾辞は名詞の後につく
        ((Speech::Noun(_)), Speech::Affix(AffixVariant::Suffix)) => 1,
        ((Speech::Verb(_)), Speech::Affix(AffixVariant::Suffix)) => 1,

        // 上記以外は接続しないものとして扱う
        _ => -1,
    }
}
