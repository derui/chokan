use std::collections::HashMap;

use dic::base::word::Word;
use serde::{Deserialize, Serialize};

mod context;
mod graph;
mod score;

/// 解析グラフ上で利用する辞書の形式。
///
/// ここで利用される辞書は、 `chokan-dic` プログラムから生成されるものが利用される
#[derive(Serialize, Deserialize)]
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
) -> i32 {
    let mut best_score = i32::MIN;

    for prev_node in prev_nodes {
        let prev_score = prev_node.get_score();
        let current_score = score::get_node_score(context, current);
        let edge_score = score::get_edge_score(context, prev_node, current);

        let score = i32::from(prev_score) + current_score + edge_score;

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
fn forward_dp(input: &str, graph: &mut graph::Graph, context: &context::Context) {
    let input = input.chars().collect::<Vec<_>>();

    for i in 1..input.len() {
        for node in graph.nodes_at(i) {
            // 各nodeに対して、一つ前のnodeからの遷移scoreを計算する
            let prev_nodes = graph.previsous_nodes(&node);

            let score = calculate_best_score(&node, &prev_nodes, context);

            if let Some(node) = graph.get_node_mut(&node) {
                node.set_score(score);
            }
        }
    }
}
