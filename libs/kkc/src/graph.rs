use std::collections::HashSet;

use dic::base::{
    speech::{AffixVariant, Speech},
    word::Word,
};

use crate::GraphDictionary;

/// 解析で利用するグラフと、それを入力文字列から構築する処理を提供する

/// 各Nodeにおけるキー
/// graph内のindexをそのまま指す
struct NodeKey(usize);

/// Graphの中で使われるNode
///
/// BOS/EOS に相当するNodeも含めている
enum Node {
    WordNode(Word),
    /// 仮想Nodeに対応する型である。この型は必ずしも存在するものではない
    Virtual(String),
    BOS,
    EOS,
}

/// node間を結ぶエッジ
///
/// エッジに設定されているコストは、 `from -> to` における遷移コストを表す
struct Edge {
    from: NodeKey,
    to: NodeKey,
    cost: i32,
}

pub struct Graph {
    /// graphの中に含まれるNode
    ///
    /// EOS/BOSも含んでいる
    /// このNodesは、長さが入力文字 + 2であり、先頭がBOS、最後尾がEOSになる
    nodes: Vec<Vec<Node>>,
}

impl Graph {
    /// 解析結果からグラフを返す
    ///
    /// # Arguments
    /// * `input` - 解析対象の文字列
    /// * `dic` - 辞書
    ///
    /// # Returns
    /// 構築されたグラフ
    pub fn from_input(input: &str, dic: &GraphDictionary) -> Graph {
        let mut nodes: Vec<Vec<Node>> = Vec::with_capacity(input.len() + 2);
        nodes[0] = vec![Node::BOS];
        nodes[input.len() + 1] = vec![Node::EOS];
        let mut graph = Graph { nodes };

        // まず付属語を検索し、nodesに追加する
        graph.find_auxiliary(input, dic);
        // 先頭から始まりうる単語をnodesに追加する
        graph.find_word_only_first(input, dic);

        graph
    }

    /// 先頭から始まりうる単語をnodesに追加する
    ///
    /// ここでは、あくまで先頭から発見可能である単語に限って検索する。但し、該当する単語が接頭語である場合には
    /// その後ろから再度検索を実施する場合がある。
    fn find_word_only_first(&mut self, input: &str, dic: &GraphDictionary) {
        let mut need_re_search_indices: HashSet<usize> = HashSet::new();

        for i in 1..input.len() {
            let key = &input[0..i];

            let words = dic
                .standard_trie
                .search(key, &|_, _| {})
                .and_then(|_| dic.standard_dic.get(key));

            if let Some(words) = words {
                for word in words {
                    // 接頭語が見つかった場合は、続く単語も検索する
                    if word.speech == Speech::Affix(AffixVariant::Prefix) {
                        need_re_search_indices.insert(i + key.len());
                    }
                    self.nodes[i + key.len()].push(Node::WordNode(word.clone()));
                }
            }
        }

        // 接頭語があったindexの後ろから単語のみを探索する
        for idx in need_re_search_indices {
            for i in (idx + 1)..input.len() {
                let key = &input[idx..i];

                let words = dic
                    .standard_trie
                    .search(key, &|_, _| {})
                    .and_then(|_| dic.standard_dic.get(key));

                if let Some(words) = words {
                    for word in words {
                        self.nodes[i + key.len()].push(Node::WordNode(word.clone()));
                    }
                }
            }
        }
    }

    /// 付属語を検索して graph のnodeに追加する
    ///
    /// この段階では、仮想ノードへの追加なども実施はせずに、単純にあり得る付属語を検索して追加するのみである
    fn find_auxiliary(&mut self, input: &str, dic: &GraphDictionary) {
        for i in 0..input.len() {
            let mut j = i;

            while j < input.len() {
                let key = &input[i..j];

                let words = dic
                    .ancillary_trie
                    .search(key, &|_, _| {})
                    .and_then(|_| dic.ancillary_dic.get(key));

                if let Some(words) = words {
                    for word in words {
                        self.nodes[i + key.len()].push(Node::WordNode(word.clone()));
                    }
                }

                j += 1
            }
        }
    }
}
