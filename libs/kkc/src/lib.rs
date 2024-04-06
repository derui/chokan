use std::collections::{BinaryHeap, HashMap};

use dic::base::word::Word;
use score::{Score, MIN_SCORE};
use serde::{Deserialize, Serialize};

pub mod context;
pub mod frequency;
mod graph;
mod score;
mod tankan;

#[cfg(test)]
mod test_dic;

/// 解析グラフ上で利用する辞書の形式。
///
/// ここで利用される辞書は、 `chokan-dic` プログラムから生成されるものが利用される
#[derive(Serialize, Deserialize, Debug)]
pub struct GraphDictionary {
    pub standard_trie: trie::Trie,
    pub standard_dic: HashMap<String, Vec<Word>>,
    // 付属語
    pub ancillary_trie: trie::Trie,
    pub ancillary_dic: HashMap<String, Vec<Word>>,
}

/// currentとその一つ前のノードから、currentの最適なスコアを計算する
///
/// ここではビタビアルゴリズムにおけるscore計算を実施している。
/// 計算されるscoreは、 `一つ前のnodeのscore + 今注目しているnodeのscore + ２つのnode間のscore ` となる
///
/// # Arguments
/// * `current` - 現在注目しているノード
/// * `prev_nodes` - 現在注目しているノードの一つ前のノードの一覧
/// * `context` - 解析グラフ上で利用するコンテキスト
///
fn calculate_best_score(
    current: &graph::Node,
    prev_nodes: &[graph::Node],
    context: &context::Context,
    frequency: &frequency::ConversionFrequency,
) -> Score {
    let mut best_score = MIN_SCORE;

    for prev_node in prev_nodes {
        let prev_score: Score = prev_node.get_score().into();
        let current_score = score::get_node_score(context, current, frequency);
        let edge_score = score::get_edge_score(context, prev_node, current);

        let score: Score = prev_score + current_score + edge_score;

        if score > best_score {
            best_score = score;
        }
    }

    best_score
}

/// graphに対して前向きDPを行う。
///
/// この関数は、graphに対して前向きDPを行い、各ノードに対する最適なスコアを計算する。ちょうどビタビアルゴリズムを実装することと
/// 同義である
fn forward_dp(
    input: &str,
    graph: &mut graph::Graph,
    context: &context::Context,
    frequency: &frequency::ConversionFrequency,
) {
    let input = input.chars().collect::<Vec<_>>();

    for i in 0..input.len() {
        for node in graph.nodes_at(i) {
            // 各nodeに対して、一つ前のnodeからの遷移scoreを計算する
            let prev_nodes = graph.previsous_nodes(&node);

            let score = calculate_best_score(&node, &prev_nodes, context, frequency);

            if let Some(node) = graph.get_node_mut(&node) {
                node.set_score(score);
            }
        }
    }
}

/// Pathとして定義するデータ。
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Candidate {
    // 現在のnode
    current_node: graph::Node,
    // 次の候補情報
    //
    // Candidateは、全体をcloneして連鎖的に保持することで、先頭から取得することが可能になる
    next: Option<Box<Candidate>>,
    score: Score,
    priority: i32,
}

impl Candidate {
    /// 自立語を取得する
    ///
    /// # Returns
    /// 自立語のみを取得した文字列
    pub fn to_string_only_independent(&self) -> Option<String> {
        match &self.current_node {
            graph::Node::WordNode(_, w, _) if !w.speech.is_ancillary() => {
                Some(w.word.iter().collect())
            }
            _ => self
                .next
                .as_ref()
                .and_then(|v| v.to_string_only_independent()),
        }
    }
}

impl ToString for Candidate {
    fn to_string(&self) -> String {
        match &self.next {
            Some(next) => format!("{}{}", self.current_node.to_string(), next.to_string()),
            None => self.current_node.to_string(),
        }
    }
}

impl Ord for Candidate {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.priority.cmp(&other.priority)
    }
}

impl PartialOrd for Candidate {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// 指定した数以下の最適な候補を取得する
///
/// この関数は、前向きDPを行った後に、最適なパスを取得する。最適なパスは、最適なスコアを持つノードをたどることで取得することができる
fn get_n_best_candidates(
    context: &context::Context,
    graph: &graph::Graph,
    frequency: &frequency::ConversionFrequency,
    n: usize,
) -> Vec<Candidate> {
    let mut queue: BinaryHeap<Candidate> = BinaryHeap::new();
    let mut result = vec![];

    queue.push(Candidate {
        current_node: graph::Node::EOS,
        next: None,
        score: Default::default(),
        priority: 0,
    });

    // A*アルゴリズムを利用して、順次最適な候補を取得する
    while let Some(candidate) = queue.pop() {
        let Candidate {
            current_node,
            score,
            ..
        } = &candidate;

        if *current_node == graph::Node::BOS {
            result.push(candidate.clone());
            if result.len() >= n {
                break;
            }
        }

        // 各ノードについて、その一つ前のノードからの遷移scoreと、現在のnodeからゴールまでのコストを設定する。
        for prev_node in graph.previsous_nodes(current_node) {
            let edge_score = score::get_edge_score(context, &prev_node, current_node);
            let node_score = score::get_node_score(context, current_node, frequency);
            let next_score = edge_score + node_score + *score;

            if let Some(priority) = Option::from(next_score + prev_node.get_score().into()) {
                queue.push(Candidate {
                    current_node: prev_node.clone(),
                    next: Some(Box::new(candidate.clone())),
                    score: next_score,
                    priority,
                });
            }
        }
    }

    result
}

/// 指定した文字列に対する候補を取得する
///
/// # Arguments
/// * `input` - 変換対象の文字列
/// * `dic` - 変換に利用する辞書
/// * `context` - 変換に利用するコンテキスト
/// * `n` - 取得する候補の数
///
/// # Returns
/// 変換候補のリスト
pub fn get_candidates(
    input: &str,
    dic: &GraphDictionary,
    context: &context::Context,
    frequency: &frequency::ConversionFrequency,
    n: usize,
) -> Vec<Candidate> {
    let mut graph = graph::Graph::from_input(input, dic, context);
    forward_dp(input, &mut graph, context, frequency);
    get_n_best_candidates(context, &graph, frequency, n)
}

// re-export tankan
pub use tankan::TankanDictionary;

/// 指定した文字列に対する単漢字の候補を取得する
///
/// # Arguments
/// * `input` - 変換対象の文字列
/// * `dic` - 変換に利用する辞書
///
/// # Returns
/// 変換候補のリスト
pub fn get_tankan_candidates(input: &str, dic: &TankanDictionary) -> Vec<String> {
    dic.get_candidates(input)
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use dic::base::speech::{NounVariant, Speech};

    use crate::{context::Context, test_dic::LABELS};

    use super::*;

    #[test]
    fn get_best_candidates_from_input() {
        // arrange
        let dic = test_dic::new_dic();
        let context = Context::normal();
        let freq = frequency::ConversionFrequency::new();

        // act
        let result = get_candidates("くるまではしらなかった", &dic, &context, &freq, 1);

        // assert
        assert_eq!(1, result.len());
        assert_eq!("車ではしらなかった", result[0].to_string())
    }

    #[test]
    fn get_best_n_candidates_from_input() {
        // arrange
        let dic = test_dic::new_dic();
        let context = Context::normal();
        let freq = frequency::ConversionFrequency::new();

        // act
        let result = get_candidates("くるまではしらなかった", &dic, &context, &freq, 3);

        // assert
        assert_eq!(3, result.len());
        assert_eq!(
            HashSet::from_iter(result.iter().map(|c| c.to_string())),
            HashSet::from([
                "車ではしらなかった".to_string(),
                "来るまではしらなかった".to_string(),
                "繰るまではしらなかった".to_string(),
            ])
        )
    }

    #[test]
    fn virtual_node_with_1_node() {
        // arrange
        let keys = LABELS.to_vec();
        let ancillary_trie = trie::Trie::from_keys(&keys);
        let mut standard_trie = trie::Trie::from_keys(&keys);

        standard_trie.insert("これ").unwrap();

        let dic = GraphDictionary {
            standard_trie,
            standard_dic: HashMap::from([(
                "これ".to_string(),
                vec![Word::new("此れ", "これ", Speech::Noun(NounVariant::Common))],
            )]),
            ancillary_trie,
            ancillary_dic: HashMap::from([]),
        };
        let context = Context::normal();
        let freq = frequency::ConversionFrequency::new();

        // act
        let result = get_candidates("これでいける", &dic, &context, &freq, 3);

        // assert
        assert_eq!(
            HashSet::from_iter(result.iter().map(|c| c.to_string())),
            HashSet::from(["此れでいける".to_string(),])
        )
    }
}
