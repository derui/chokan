use std::{cmp::Ordering, ops};

use dic::base::{
    speech::{AffixVariant, ParticleType, Speech},
    word::Word,
};

use crate::{context::Context, frequency::ConversionFrequency, graph::Node};

/// 計算されたscore
///
/// scoreは、計算の過程で、接続不可能なケースが存在しうる。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Score(i32);

impl Score {
    /// scoreが有効かどうかを返す
    pub fn is_valid(&self) -> bool {
        let var_name = self.0 >= 0;
        var_name
    }

    /**
     * 接続できないことを表すScoreを返す
     */
    pub fn non_connect() -> Self {
        Self(-1)
    }
}

/// Scoreとして成立しないScore
pub const MIN_SCORE: Score = Score(i32::MIN);

impl PartialOrd for Score {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.0 < 0 && other.0 < 0 {
            Some(Ordering::Equal)
        } else if self.0 < 0 {
            return Some(Ordering::Less);
        } else if other.0 < 0 {
            return Some(Ordering::Greater);
        } else {
            return self.0.partial_cmp(&other.0);
        }
    }
}

impl ops::Add for Score {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        if self.0 < 0 || rhs.0 < 0 {
            return Score(-1);
        }
        Score(self.0 + rhs.0)
    }
}

impl From<Score> for Option<i32> {
    /// 0未満の場合は対象とすることが出来ない
    fn from(value: Score) -> Self {
        if value.0 < 0 {
            None
        } else {
            Some(value.0)
        }
    }
}

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
pub fn get_node_score(
    context: &Context,
    current: &Node,
    frequencies: &ConversionFrequency,
) -> Score {
    // 読みが長い方が選択される可能性は高いものの、score自体はある程度の影響しかしないようにしておく
    match current {
        Node::Word(_, w, _) => {
            let proper_priority = if context.is_proper() {
                if w.speech.is_noun_proper() {
                    10
                } else {
                    0
                }
            } else {
                0
            };
            let frequency =
                frequencies.get_frequency_of_word(&w.word.iter().collect::<String>(), context);
            Score(
                (frequency.div_ceil(10) + (w.reading.len() as u64) * 2 + proper_priority)
                    .try_into()
                    .unwrap(),
            )
        }
        Node::Virtual(_, _, _) => Default::default(),
        Node::Bos => Default::default(),
        Node::Eos => Default::default(),
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
pub fn get_edge_score(context: &Context, prev: &Node, current: &Node) -> Score {
    match prev {
        Node::Bos => get_edge_score_of_head(context, current),
        _ => get_edge_score_impl(context, prev, current),
    }
}

/// 先頭のnodeに対するscoreを算出する
///
/// 主に、contextがNormal以外の場合、数詞や接辞を優先するようなscoreを構成する
fn get_edge_score_of_head(context: &Context, current: &Node) -> Score {
    match current {
        Node::Word(_, w, _) => match w.speech {
            Speech::Counter if context.is_numeral() => Score(2),
            Speech::Affix(AffixVariant::Prefix) if context.is_foreign_word() => Score(2),
            _ => Default::default(),
        },
        _ => Default::default(),
    }
}

fn get_edge_score_impl(context: &Context, prev: &Node, current: &Node) -> Score {
    match (prev, current) {
        (Node::Word(_, prev, _), Node::Word(_, current, _)) => {
            get_edge_score_between_words(context, prev, current)
        }
        (Node::Word(_, prev, _), Node::Virtual(_, _, _)) => {
            get_edge_score_allow_virtual_word(context, prev)
        }
        // BOS/EOSとの接続関係は影響しない
        _ => Default::default(),
    }
}

/// 現在のnodeが仮想ノードであった場合に、対象のノードが文節を構成しうるかどうかを判定したscoreを返す
///
/// 文節末を構成できないノードであった場合は、scoreをマイナスする
fn get_edge_score_allow_virtual_word(_context: &Context, prev: &Word) -> Score {
    // 仮想nodeが続く場合、対象が文節末を構成しうる場合は許容する
    match &prev.speech {
        Speech::Verb(_) => Score(0),
        Speech::Noun(_) => Score(0),
        // 接頭・接尾辞の場合はそもそも文末を構成することがない
        Speech::Affix(_) => Score::non_connect(),
        v if !v.is_ancillary() => Score(0),
        _ => Score::non_connect(),
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
fn get_edge_score_between_words(context: &Context, prev: &Word, current: &Word) -> Score {
    match (&prev.speech, &current.speech) {
        // 格助詞は体言の後につく
        (Speech::Noun(_), Speech::Particle(ParticleType::Case)) => Score(1),
        // 接続助詞は用言または助動詞の後につく
        (Speech::Verb(_), Speech::Particle(ParticleType::Conjunctive)) => Score(1),
        (Speech::AuxiliaryVerb, Speech::Particle(ParticleType::Conjunctive)) => Score(1),
        // 副助詞は色々つくことができる
        (_, Speech::Particle(ParticleType::Adverbial)) => Score(0),
        // 終助詞は文末につくが、文末を保証するのがむずかしいので、副助詞と同様にする
        (_, Speech::Particle(ParticleType::SentenceFinal)) => Score(0),
        // 助動詞は用言の後につく
        (Speech::Verb(_), Speech::AuxiliaryVerb) => Score(1),
        // 接頭辞は動詞または名詞の前につく
        (Speech::Affix(AffixVariant::Prefix), Speech::Noun(_)) => Score(1),
        (Speech::Affix(AffixVariant::Prefix), Speech::Verb(_)) => Score(1),
        // 接尾辞は動詞または名詞の後につく。ただしかな漢字変換では、基本的に接尾辞は名詞の後につくため、
        // 動詞に対してはつかないものとする
        (Speech::Noun(_), Speech::Affix(AffixVariant::Suffix)) => Score(1),

        // 上記以外は接続しないものとして扱う
        _ => Score::non_connect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn score_calculation() {
        // arrange

        // act
        let ret = Score(0) + Score(1);

        // assert
        assert_eq!(ret, Score(1))
    }

    #[test]
    fn score_with_minus() {
        // arrange

        // act

        // assert
        assert_eq!(Score(-1) + Score(2), Score(-1))
    }

    #[test]
    fn score_comparison() {
        // arrange

        // act

        // assert
        assert!(Score(1) < Score(2), "1 < 2");
        assert!(Score(2) < Score(3), "2 < 3");
        assert!(Score(1) < Score(3), "1 < 3");
        assert!(Score(1) <= Score(1), "1 <= 1");
        assert!(MIN_SCORE == MIN_SCORE, "-1 == -1");
        assert!(MIN_SCORE < Score(1), "-1 < 1");
        assert!(MIN_SCORE <= Score(1), "-1 > 1");
    }
}
