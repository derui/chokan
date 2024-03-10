use std::collections::{HashSet, VecDeque};

use dic::base::{
    speech::{AffixVariant, Speech},
    word::Word,
};

use crate::GraphDictionary;

/// 解析で利用するグラフと、それを入力文字列から構築する処理を提供する

/// Graphの中で使われるNode
#[derive(Debug, PartialEq, Clone)]
enum Node {
    WordNode(Word),
    /// 仮想Nodeに対応する型である。この型は必ずしも存在するものではない
    Virtual(usize),
}

#[derive(Debug)]
pub struct Graph {
    /// graphの中に含まれるNode
    ///
    /// EOS/BOSも含んでいる
    /// このNodesは、長さが入力文字であり、先頭がBOSとなる。BOSは、対応するindexが末尾であればそのまま末尾となる
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
        let input = input.chars().collect::<Vec<_>>();
        let nodes: Vec<Vec<Node>> = vec![Default::default(); input.len()];
        let mut graph = Graph { nodes };

        // まず付属語を検索し、nodesに追加する
        graph.find_ancillary(&input, dic);
        // 先頭から始まりうる単語をnodesに追加する
        graph.find_word_only_first(&input, dic);
        graph.remove_unconnected_nodes();

        graph
    }

    /// 指定したindexにあるnodeのうち、現時点でつながるものがないnodeを排除する
    ///
    /// # Arguments
    /// * `input` - 解析対象の文字列
    /// * `index` - 探索するindex
    ///
    /// # Returns
    /// 不要なものが削除されたnodeのリスト
    fn get_filter_unconnected_nodes(&self, index: usize) -> Vec<Node> {
        // 0が終点になる単語は存在しない。
        self.nodes[index]
            .iter()
            .filter(|v| {
                if let Node::WordNode(word) = v {
                    let prev_node_idx = index + word.reading.len();

                    // 先頭を指している場合は対象にしておく
                    if prev_node_idx == 0 {
                        true
                    } else {
                        self.nodes
                            .get(prev_node_idx)
                            .map(|v| v.is_empty())
                            .unwrap_or(false)
                    }
                } else {
                    false
                }
            })
            .cloned()
            .collect::<Vec<_>>()
    }

    /// 先頭からつながらないnodeを削除する
    fn remove_unconnected_nodes(&mut self) {
        // 0が終点になる単語は存在しない。
        for i in (1..self.nodes.len()).rev() {
            self.nodes[i] = self.get_filter_unconnected_nodes(i);
        }
    }

    /// 先頭から始まりうる単語をnodesに追加する
    ///
    /// ここでは、あくまで先頭から発見可能である単語に限って検索する。但し、該当する単語が接頭語である場合には
    /// その後ろから再度検索を実施する場合がある。
    fn find_word_only_first(&mut self, input: &[char], dic: &GraphDictionary) {
        let mut need_re_search_indices: HashSet<usize> = HashSet::new();

        for i in 1..input.len() {
            let key = input[0..i].iter().collect::<String>();

            let words = dic
                .standard_trie
                .search(&key, &|_, _| {})
                .and_then(|_| dic.standard_dic.get(&key));

            if let Some(words) = words {
                for word in words {
                    // 接頭語が見つかった場合は、続く単語も検索する
                    if word.speech == Speech::Affix(AffixVariant::Prefix) {
                        need_re_search_indices.insert(i + key.len());
                    }
                    self.nodes[i].push(Node::WordNode(word.clone()));
                }
            }
        }

        // 接頭語があったindexの後ろから単語のみを探索する
        for idx in need_re_search_indices {
            for i in (idx + 1)..input.len() {
                let key = input[idx..i].iter().collect::<String>();

                let words = dic
                    .standard_trie
                    .search(&key, &|_, _| {})
                    .and_then(|_| dic.standard_dic.get(&key));

                if let Some(words) = words {
                    for word in words {
                        self.nodes[i].push(Node::WordNode(word.clone()));
                    }
                }
            }
        }
    }

    /// 付属語を検索して graph のnodeに追加する
    ///
    /// この段階では、仮想ノードへの追加なども実施はせずに、単純にあり得る付属語を検索して追加するのみである
    fn find_ancillary(&mut self, input: &[char], dic: &GraphDictionary) {
        for i in 0..input.len() {
            let mut j = i;

            while j < input.len() {
                let key = input[i..j].iter().collect::<String>();

                let words = dic
                    .ancillary_trie
                    .search(&key, &|_, _| {})
                    .and_then(|_| dic.ancillary_dic.get(&key));

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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use dic::base::speech::{NounVariant, ParticleType, VerbForm};

    use super::*;

    // テスト用の辞書
    fn dic() -> GraphDictionary {
        let keys = vec![
            'あ', 'い', 'う', 'え', 'お', 'か', 'き', 'く', 'け', 'こ', 'さ', 'し', 'す', 'せ',
            'そ', 'た', 'ち', 'つ', 'て', 'と', 'な', 'に', 'ぬ', 'ね', 'の', 'は', 'ひ', 'ふ',
            'へ', 'ほ', 'ま', 'み', 'む', 'め', 'も', 'や', 'ゆ', 'よ', 'ら', 'り', 'る', 'れ',
            'ろ', 'わ', 'を', 'ん', 'が', 'ぎ', 'ぐ', 'げ', 'ご', 'ざ', 'じ', 'ず', 'ぜ', 'ぞ',
            'だ', 'ぢ', 'づ', 'で', 'ど', 'ば', 'び', 'ぶ', 'べ', 'ぼ', 'ぱ', 'ぴ', 'ぷ', 'ぺ',
            'ぽ', 'っ',
        ];
        let mut standard_trie = trie::Trie::from_keys(&keys);
        let mut ancillary_trie = trie::Trie::from_keys(&keys);

        standard_trie.insert("くるま").unwrap();
        standard_trie.insert("くる").unwrap();
        ancillary_trie.insert("まで").unwrap();
        ancillary_trie.insert("で").unwrap();

        GraphDictionary {
            standard_trie,
            standard_dic: HashMap::from([
                (
                    "くるま".to_string(),
                    vec![Word::new("くるま", "車", Speech::Noun(NounVariant::Common))],
                ),
                (
                    "くる".to_string(),
                    vec![Word::new(
                        "くる",
                        "来る",
                        Speech::Verb(VerbForm::Hen("カ".to_string())),
                    )],
                ),
                (
                    "くる".to_string(),
                    vec![Word::new(
                        "くる",
                        "繰る",
                        Speech::Verb(VerbForm::Godan("ラ".to_string())),
                    )],
                ),
            ]),
            ancillary_trie,
            ancillary_dic: HashMap::from([
                (
                    "まで".to_string(),
                    vec![Word::new(
                        "まで",
                        "まで",
                        Speech::Particle(ParticleType::Adverbial),
                    )],
                ),
                (
                    "で".to_string(),
                    vec![Word::new("で", "で", Speech::Particle(ParticleType::Case))],
                ),
            ]),
        }
    }

    #[test]
    fn name() {
        // arrange
        let dic = dic();

        // act
        let graph = Graph::from_input("くるまではしらかった", &dic);

        // assert
    }
}
