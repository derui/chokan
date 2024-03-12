use std::collections::{HashSet, VecDeque};

use dic::base::{
    speech::{AffixVariant, Speech},
    word::Word,
};

use crate::GraphDictionary;

/// 解析で利用するグラフと、それを入力文字列から構築する処理を提供する

/// 各Nodeに対して設定されるconst
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
struct NodeCost {
    // startからこのnodeまでのコスト
    cost_from_start: u32,
    // このノードから末尾までのコスト
    cost_to_end: u32,
}

impl Default for NodeCost {
    fn default() -> Self {
        NodeCost {
            cost_from_start: 0,
            cost_to_end: 0,
        }
    }
}

/// Graphの中で使われるNode
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
enum Node {
    WordNode(Word, NodeCost),
    /// 仮想Nodeに対応する型である。この型は必ずしも存在するものではない
    Virtual(usize, NodeCost),
}

impl Node {
    /// このノードの末尾から、最初のindexを返す
    ///
    /// # Arguments
    /// * `end_at` - 末尾のindex。0 origin
    ///
    /// # Returns
    /// このノードの先頭のindex。あれば、一つ前のnodeのend_at + 1となる
    #[inline]
    fn start_at(&self, end_at: usize) -> usize {
        match self {
            Node::WordNode(word, _) => end_at - (word.reading.len() - 1),
            Node::Virtual(s, _) => end_at - (s - 1),
        }
    }
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
        let ancillaries = graph.find_ancillary(&input, dic);
        // 先頭から始まりうる単語をnodesに追加する
        graph.find_word_only_first(&input, dic);
        // 付属語を追加する
        graph.merge_ancillaries(&ancillaries);
        // 仮想ノードを追加する
        graph.complete_virtual_nodes(&input);

        graph
    }

    /// 付属語の一覧から、単語間で接続しうるものをnodesに追加する
    ///
    /// sa
    ///
    /// # Arguments
    /// * `ancillaries` - 付属語の一覧
    fn merge_ancillaries(&mut self, ancillaries: &[Vec<Node>]) {
        // 付属語が設定されているindexは、その付属語の末尾のindexであるため、
        // start_atの位置に単語がある場合のみをmerge対象にする
        for (i, ancillary) in ancillaries.iter().enumerate() {
            for node in ancillary {
                let start_at = node.start_at(i);

                match self.nodes.get(start_at) {
                    Some(v) if !v.is_empty() => {
                        self.nodes[i].push(node.clone());
                        continue;
                    }
                    _ => {}
                }
            }
        }
    }

    /// 仮想ノードを補完する
    ///
    /// 補完する仮想ノードは、対象となる単語の末尾に対して設定する。呼び出し時点で、
    /// 単語はつながっているしか存在していないため、単語の存在するindex + 1文字目から
    /// 末尾までを仮想ノードとして設定する
    fn complete_virtual_nodes(&mut self, input: &[char]) {
        let end_of_input = input.len() - 1;

        // 末尾に到達している単語には設定する必要がないので無視する
        for i in (1..(input.len() - 1)).rev() {
            match self.nodes.get(i) {
                Some(v) if !v.is_empty() => {
                    let virtual_node = Node::Virtual(end_of_input - i, Default::default());
                    self.nodes[end_of_input].push(virtual_node);
                }
                _ => {}
            }
        }
    }

    /// 先頭から始まりうる単語をnodesに追加する
    ///
    /// ここでは、あくまで先頭から発見可能である単語に限って検索する。但し、該当する単語が接頭語である場合には
    /// その後ろから再度検索を実施する場合がある。
    ///
    /// # Returns
    /// 発見した単語の末尾indexの集合
    fn find_word_only_first(&mut self, input: &[char], dic: &GraphDictionary) -> HashSet<usize> {
        let mut need_re_search_indices: HashSet<usize> = HashSet::new();
        let mut found_indices: HashSet<usize> = HashSet::new();

        for i in 1..input.len() {
            let key = input[0..=i].iter().collect::<String>();

            let words = dic
                .standard_trie
                .search(&key, &|_, _| {})
                .and_then(|_| dic.standard_dic.get(&key));

            if let Some(words) = words {
                for word in words {
                    // 接頭語が見つかった場合は、続く単語も検索する
                    if word.speech == Speech::Affix(AffixVariant::Prefix) {
                        need_re_search_indices.insert(i);
                    } else {
                        found_indices.insert(i);
                    }
                    self.nodes[i].push(Node::WordNode(word.clone(), Default::default()));
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
                        self.nodes[i].push(Node::WordNode(word.clone(), Default::default()));
                        found_indices.insert(i);
                    }
                }
            }
        }

        found_indices
    }

    /// 付属語を検索して graph のnodeに追加する
    ///
    /// この段階では、仮想ノードへの追加なども実施はせずに、単純にあり得る付属語を検索して追加するのみである
    ///
    /// # Returns
    /// 付属語が設定されたnodes
    fn find_ancillary(&self, input: &[char], dic: &GraphDictionary) -> Vec<Vec<Node>> {
        let mut nodes: Vec<Vec<Node>> = vec![Default::default(); input.len()];

        for i in 0..input.len() {
            let mut j = i;

            while j < input.len() {
                let key = input[i..=j].iter().collect::<String>();

                let words = dic
                    .ancillary_trie
                    .search(&key, &|_, _| {})
                    .and_then(|_| dic.ancillary_dic.get(&key));

                if let Some(words) = words {
                    for word in words {
                        nodes[j].push(Node::WordNode(word.clone(), Default::default()));
                    }
                }

                j += 1
            }
        }

        nodes
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
                    vec![
                        Word::new(
                            "くる",
                            "来る",
                            Speech::Verb(VerbForm::Hen("カ".to_string())),
                        ),
                        Word::new(
                            "くる",
                            "繰る",
                            Speech::Verb(VerbForm::Godan("ラ".to_string())),
                        ),
                    ],
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
    fn should_be_able_to_construct() {
        // arrange
        let dic = dic();

        // act
        let graph = Graph::from_input("くるまではしらなかった", &dic);

        // assert
        assert_eq!(graph.nodes.len(), 11);
        assert_eq!(graph.nodes[1].len(), 2);
        assert_eq!(graph.nodes[2].len(), 1);
        assert_eq!(graph.nodes[3].len(), 2);
        assert_eq!(graph.nodes[10].len(), 3);
    }

    #[test]
    fn contains_virtual_nodes() {
        // arrange
        let dic = dic();

        // act
        let graph = Graph::from_input("くるまではしらなかった", &dic);

        // assert
        assert_eq!(
            graph.nodes[1].iter().cloned().collect::<HashSet<_>>(),
            HashSet::from([
                Node::WordNode(
                    Word::new(
                        "くる",
                        "来る",
                        Speech::Verb(VerbForm::Hen("カ".to_string()))
                    ),
                    Default::default()
                ),
                Node::WordNode(
                    Word::new(
                        "くる",
                        "繰る",
                        Speech::Verb(VerbForm::Godan("ラ".to_string()))
                    ),
                    Default::default()
                )
            ])
        );
        assert_eq!(
            graph.nodes[2].iter().cloned().collect::<HashSet<_>>(),
            HashSet::from([Node::WordNode(
                Word::new("くるま", "車", Speech::Noun(NounVariant::Common)),
                Default::default()
            )])
        );
        assert_eq!(
            graph.nodes[3].iter().cloned().collect::<HashSet<_>>(),
            HashSet::from([
                Node::WordNode(
                    Word::new("まで", "まで", Speech::Particle(ParticleType::Adverbial)),
                    Default::default()
                ),
                Node::WordNode(
                    Word::new("で", "で", Speech::Particle(ParticleType::Case)),
                    Default::default()
                )
            ])
        );
        assert_eq!(
            graph.nodes[10].iter().cloned().collect::<HashSet<_>>(),
            HashSet::from([
                Node::Virtual(7, Default::default()),
                Node::Virtual(8, Default::default()),
                Node::Virtual(9, Default::default()),
            ])
        );
    }
}
