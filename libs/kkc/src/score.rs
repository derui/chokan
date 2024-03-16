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

/// node自体のscoreを返す
///
/// # Arguments
/// * `context` - かな漢字変換を実施するときのcontext
/// * `current` - scoreを計算するnode
///
/// # Returns
/// nodeに対するscore
pub fn get_node_score(_context: &Context, current: &Node) -> i32 {
    // 読みが長い方が選択される可能性は高いものの、score自体はある程度の影響しかしないようにしておく
    match current {
        Node::WordNode(_, w, _) => w.reading.len() as i32,
        Node::Virtual(_, _, _) => 0,
        Node::BOS => 0,
        Node::EOS => 0,
    }
}

/// node間のscoreを返す
///
/// ここでの接続可能性は、基本的に文節を構成可能かどうか、に主眼が置かれている。
///
/// # Arguments
/// * `context` - かな漢字変換を実施するときのcontext
/// * `current` - 接続可能かどうかを調べるnode
/// * `prev` - currentの一つ前のnode。BOSが一個前である場合はNone
///
/// # Returns
/// edgeに対するscore
pub fn get_edge_score(context: &Context, prev: &Node, current: &Node) -> i32 {
    match prev {
        Node::BOS => 0,
        _ => get_edge_score_impl(context, prev, current),
    }
}

fn get_edge_score_impl(context: &Context, prev: &Node, current: &Node) -> i32 {
    match (prev, current) {
        (Node::WordNode(_, prev, _), Node::WordNode(_, current, _)) => {
            get_edge_score_between_words(context, prev, current)
        }
        (Node::WordNode(_, prev, _), Node::Virtual(_, _, _)) => {
            get_edge_score_allow_virtual_word(context, prev)
        }
        // BOS/EOSとの接続関係は影響しない
        _ => 0,
    }
}

/// 現在のnodeが仮想ノードであった場合に、対象のノードが文節を構成しうるかどうかを判定したscoreを返す
///
/// 文節末を構成できないノードであった場合は、scoreをマイナスする
fn get_edge_score_allow_virtual_word(_context: &Context, prev: &Word) -> i32 {
    // 仮想nodeが続く場合、対象が文節末を構成しうる場合は許容する
    match &prev.speech {
        Speech::Verb(_) => 1,
        Speech::Noun(_) => 1,
        v if !v.is_ancillary() => 1,
        _ => -1,
    }
}

/// 単語間のscoreを返す
///
/// 単語間では、原則としては接続可能性があるものに対して高いスコアを与えるものとしている
///
/// # Arguments
/// * `context` - かな漢字変換を実施するときのcontext
/// * `prev` - 接続可能かどうかを調べる単語
/// * `current` - currentの一つ前の単語
///
/// # Returns
/// edgeに対するscore
fn get_edge_score_between_words(_context: &Context, prev: &Word, current: &Word) -> i32 {
    match (&prev.speech, &current.speech) {
        // 格助詞は体言の後につく
        (Speech::Noun(_), Speech::Particle(ParticleType::Case)) => 2,
        // 接続助詞は用言または助動詞の後につく
        (Speech::Verb(_), Speech::Particle(ParticleType::Conjunctive)) => 2,
        (Speech::AuxiliaryVerb, Speech::Particle(ParticleType::Conjunctive)) => 2,
        // 副助詞は色々つくことができる
        (_, Speech::Particle(ParticleType::Adverbial)) => 2,
        // 終助詞は文末につくが、文末を保証するのがむずかしいので、副助詞と同様にする
        (_, Speech::Particle(ParticleType::SentenceFinal)) => 2,
        // 助動詞は用言の後につく
        (Speech::Verb(_), Speech::AuxiliaryVerb) => 2,
        // 接頭辞は動詞または名詞の前につく
        (Speech::Affix(AffixVariant::Prefix), Speech::Noun(_)) => 2,
        (Speech::Affix(AffixVariant::Prefix), Speech::Verb(_)) => 2,
        // 接尾辞は動詞または名詞の後につく。ただしかな漢字変換では、基本的に接尾辞は名詞の後につく
        (Speech::Noun(_), Speech::Affix(AffixVariant::Suffix)) => 2,
        (Speech::Verb(_), Speech::Affix(AffixVariant::Suffix)) => 2,

        // 上記以外は接続しないものとして扱う
        _ => -1,
    }
}
