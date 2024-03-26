use std::collections::HashSet;

use dic::base::{
    speech::{AffixVariant, Speech},
    word::Word,
};

use crate::GraphDictionary;

/// 解析で利用するグラフと、それを入力文字列から構築する処理を提供する

/// 各Nodeに対して設定されるconst
#[derive(Debug, PartialEq, Clone, Eq, Hash, Default)]
pub struct NodeScore {
    // startからこのnodeまでのコスト
    cost_from_start: i32,
}

impl From<NodeScore> for i32 {
    fn from(value: NodeScore) -> Self {
        value.cost_from_start
    }
}

/// 構築が完了したgraphにおいて、安全なアクセスを提供するnew type
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct NodePointer(usize, usize);

impl NodePointer {
    ///
    /// # Arguments
    /// * `index_at_input` - 入力文字列におけるindex
    /// * `index_at_words` - 指定された位置内のindex
    ///
    /// # Returns
    /// 新しいNodePointer
    fn new(index_at_input: usize, index_at_words: usize) -> NodePointer {
        NodePointer(index_at_input, index_at_words)
    }

    /**
    node pointerにおける終点のindexを返す
    */
    #[inline]
    fn end_at(&self) -> usize {
        self.0
    }
}

/// Graphの中で使われるNode
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Node {
    WordNode(NodePointer, Word, NodeScore),
    /// 仮想Nodeに対応する型である。構築したグラフ内に存在しない場合もある
    Virtual(NodePointer, Vec<char>, NodeScore),

    // 開始地点を表すnode
    BOS,

    // 終了地点を表すnode
    EOS,
}

impl ToString for Node {
    fn to_string(&self) -> String {
        match self {
            Node::WordNode(_, w, _) => w.word.iter().collect::<String>(),
            Node::Virtual(_, v, _) => v.iter().collect::<String>(),
            Node::BOS => String::new(),
            Node::EOS => String::new(),
        }
    }
}

impl Node {
    /// このノードの末尾から、最初のindexを返す
    ///
    /// # Returns
    /// このノードの先頭のindex。あれば、一つ前のnodeのend_at + 1となる
    #[inline]
    fn start_at(&self) -> usize {
        match self {
            Node::WordNode(NodePointer(end_at, _), word, _) => end_at - (word.reading.len() - 1),
            Node::Virtual(NodePointer(end_at, _), s, _) => end_at - (s.len() - 1),
            // EOS/BOSは固定された位置にあるので、定義しない
            Node::EOS => 0,
            Node::BOS => 0,
        }
    }

    #[inline]
    fn end_at(&self) -> usize {
        match self {
            Node::WordNode(NodePointer(end_at, _), _, _) => *end_at,
            Node::Virtual(NodePointer(end_at, _), _, _) => *end_at,
            // EOS/BOSは固定された位置にあるので、定義しない
            Node::EOS => 0,
            Node::BOS => 0,
        }
    }

    /// Nodeにおける現時点のscoreを返す
    ///
    /// EOS/BOSのついてはスコアという概念はないので、あくまで
    pub(crate) fn get_score(&self) -> NodeScore {
        match self {
            Node::WordNode(_, _, score) => score.clone(),
            Node::Virtual(_, _, score) => score.clone(),
            Node::EOS => Default::default(),
            Node::BOS => Default::default(),
        }
    }

    /// Nodeに対してscoreを設定する
    ///
    /// # Arguments
    /// * `score` - 設定するscore
    pub(crate) fn set_score(&mut self, score: i32) {
        match self {
            Node::WordNode(_, _, s) => s.cost_from_start = score,
            Node::Virtual(_, _, s) => s.cost_from_start = score,
            // EOSとBOSはscoreがどうか？という判定自体しない
            Node::EOS => {}
            Node::BOS => {}
        }
    }

    /// Nodeに対してpointerだけ変更したNodeを返す
    ///
    /// # Arguments
    /// * `pointer` - 設定するpointer
    ///
    /// # Returns
    /// 新しいNode
    fn clone_with(&self, pointer: NodePointer) -> Self {
        match self {
            Self::WordNode(_, w, s) => Self::WordNode(pointer, w.clone(), s.clone()),
            Self::Virtual(_, w, s) => Self::Virtual(pointer, w.clone(), s.clone()),
            // EOSとBOSはscoreがどうか？という判定自体しない
            Self::EOS => Self::EOS,
            Self::BOS => Self::BOS,
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
        // 接頭語の後ろから単語をnodesに追加する
        graph.find_word_after_prefix(&input, dic, &ancillaries);
        // 付属語を追加する
        graph.merge_ancillaries(&ancillaries);
        // 仮想ノードを追加する
        graph.complete_virtual_nodes(&input);

        graph
    }

    /// 指定した位置で終わるnodeの一覧を返す
    ///
    /// # Arguments
    /// * `index` - 0 origin。inputにおける **文字単位** でのindex指定となる。0は常にBOSのみが含まれる
    ///
    /// # Returns
    /// 指定した位置にあるnodeの一覧。
    pub fn nodes_at(&mut self, index: usize) -> Vec<Node> {
        self.nodes[index].to_vec()
    }

    /// 指定したnodeの前のnodeを返す
    ///
    /// # Arguments
    /// * `node` - 取得するnode
    ///
    /// # Returns
    /// nodeの前のnodeの一覧
    pub fn previsous_nodes(&self, node: &Node) -> Vec<Node> {
        let index = match node {
            Node::WordNode(NodePointer(i, _), w, _) => Some(*i as i32 - w.reading.len() as i32),
            Node::Virtual(NodePointer(i, _), w, _) => Some(*i as i32 - w.len() as i32),
            Node::EOS => Some(self.nodes.len() as i32 - 1_i32),
            Node::BOS => None,
        };

        match index {
            None => vec![],
            Some(i) if i < 0 => vec![Node::BOS],
            Some(i) => self.nodes[i as usize].to_vec(),
        }
    }

    /// 指定したNodeに対するmutableな参照を返す
    ///
    /// # Arguments
    /// * `node` - 取得するNode
    ///
    /// # Returns
    /// mutableなNode
    ///
    /// # Panics
    /// 指定されたNodeが存在しない場合にはpanicする
    pub fn get_node_mut(&mut self, node: &Node) -> Option<&mut Node> {
        match node {
            Node::WordNode(NodePointer(i, j), _, _) => Some(&mut self.nodes[*i][*j]),
            Node::Virtual(NodePointer(i, j), _, _) => Some(&mut self.nodes[*i][*j]),
            Node::EOS => None,
            Node::BOS => None,
        }
    }

    /// 付属語の一覧から、単語間で接続しうるものをnodesに追加する
    ///
    /// # Arguments
    /// * `ancillaries` - 付属語の一覧
    fn merge_ancillaries(&mut self, ancillaries: &[Vec<Node>]) {
        // 付属語が設定されているindexは、その付属語の末尾のindexであるため、
        // start_atの位置に単語がある場合のみをmerge対象にする
        for ancillary in ancillaries {
            for node in ancillary {
                if self.is_mergeable_ancillary(node) {
                    let end_at = node.end_at();
                    let pointer = NodePointer(end_at, self.nodes[end_at].len());
                    self.nodes[end_at].push(node.clone_with(pointer));
                }
            }
        }
    }

    /// 対象の付属語がマージ可能かどうかを返す
    fn is_mergeable_ancillary(&self, aicillary: &Node) -> bool {
        let start_at = aicillary.start_at();

        // 先頭である場合、接頭辞の場合だけはmergeできる
        if start_at == 0 {
            match aicillary {
                Node::WordNode(_, w, _) => w.speech == Speech::Affix(AffixVariant::Prefix),
                _ => false,
            }
        } else {
            // 接頭辞以外は、先頭ではない場所でだけ追加できる
            match self.nodes.get(start_at) {
                Some(v) => !v.is_empty(),
                None => false,
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
        for i in (0..(input.len() - 1)).rev() {
            match self.nodes.get(i) {
                Some(v) if !v.is_empty() => {
                    let current_node_size = self.nodes[end_of_input].len();

                    // ここから末尾に追加するので、indexとしてはcurrent_node_sizeのままでよい
                    let virtual_node = Node::Virtual(
                        NodePointer(end_of_input, current_node_size),
                        input[(i + 1)..=end_of_input].to_vec(),
                        Default::default(),
                    );
                    self.nodes[end_of_input].push(virtual_node);
                }
                _ => {}
            }
        }
    }

    /// 先頭から始まりうる単語をnodesに追加する
    ///
    /// ここでは、あくまで先頭から発見可能である単語に限って検索する。接頭語が存在するかどうかは、
    /// find_word_after_prefixで行う。
    ///
    /// # Returns
    /// 発見した単語の末尾indexの集合
    fn find_word_only_first(&mut self, input: &[char], dic: &GraphDictionary) -> HashSet<usize> {
        let mut found_indices: HashSet<usize> = HashSet::new();

        for i in 0..input.len() {
            let key = input[0..=i].iter().collect::<String>();

            let words = dic
                .standard_trie
                .search(&key, &|_, _| {})
                .and_then(|_| dic.standard_dic.get(&key));

            if let Some(words) = words {
                for word in words {
                    found_indices.insert(i);
                    let pointer = NodePointer(i, self.nodes[i].len());
                    self.nodes[i].push(Node::WordNode(pointer, word.clone(), Default::default()));
                }
            }
        }

        found_indices
    }

    /// 接頭語がある場合に、その後ろに続きうる単語をgraphに追加する
    ///
    /// # Arguments
    /// * `input` - 入力文字列
    /// * `dic` - 辞書
    /// * `ancillaries` - 付属語の一覧
    ///
    fn find_word_after_prefix(
        &mut self,
        input: &[char],
        dic: &GraphDictionary,
        ancillaries: &[Vec<Node>],
    ) {
        // 接頭語があったindexの後ろから単語のみを探索する
        let mut prefix_nodes: Vec<(NodePointer, Node)> = Vec::new();

        for nodes in ancillaries {
            for node in nodes {
                match node {
                    Node::WordNode(
                        p,
                        Word {
                            speech: Speech::Affix(AffixVariant::Prefix),
                            ..
                        },
                        _,
                    ) if node.start_at() == 0 => prefix_nodes.push((p.clone(), node.clone())),
                    _ => (),
                }
            }
        }

        // 接頭語のnode pointer + 1が開始なので、それ以降を探索する
        for (p, _) in prefix_nodes {
            for i in (p.end_at() + 1)..input.len() {
                let key = input[(p.end_at() + 1)..=i].iter().collect::<String>();

                let words = dic
                    .standard_trie
                    .search(&key, &|_, _| {})
                    .and_then(|_| dic.standard_dic.get(&key));

                if let Some(words) = words {
                    for word in words {
                        let pointer = NodePointer(i, self.nodes[i].len());
                        self.nodes[i].push(Node::WordNode(
                            pointer,
                            word.clone(),
                            Default::default(),
                        ));
                    }
                }
            }
        }
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
                        let pointer = NodePointer(j, nodes[j].len());
                        nodes[j].push(Node::WordNode(pointer, word.clone(), Default::default()));
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

    use crate::test_dic::{self, LABELS};

    use super::*;

    #[test]
    fn should_be_able_to_construct() {
        // arrange
        let dic = test_dic::new_dic();

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
        let dic = test_dic::new_dic();

        // act
        let graph = Graph::from_input("くるまではしらなかった", &dic);

        // assert
        assert_eq!(
            graph.nodes[1].iter().cloned().collect::<HashSet<_>>(),
            HashSet::from([
                Node::WordNode(
                    NodePointer(1, 0),
                    Word::new(
                        "来る",
                        "くる",
                        Speech::Verb(VerbForm::Hen("カ".to_string()))
                    ),
                    Default::default()
                ),
                Node::WordNode(
                    NodePointer(1, 1),
                    Word::new(
                        "繰る",
                        "くる",
                        Speech::Verb(VerbForm::Godan("ラ".to_string()))
                    ),
                    Default::default()
                )
            ])
        );
        assert_eq!(
            graph.nodes[2].iter().cloned().collect::<HashSet<_>>(),
            HashSet::from([Node::WordNode(
                NodePointer(2, 0),
                Word::new("車", "くるま", Speech::Noun(NounVariant::Common)),
                Default::default()
            )])
        );
        assert_eq!(
            graph.nodes[3].iter().cloned().collect::<HashSet<_>>(),
            HashSet::from([
                Node::WordNode(
                    NodePointer(3, 0),
                    Word::new("まで", "まで", Speech::Particle(ParticleType::Adverbial)),
                    Default::default()
                ),
                Node::WordNode(
                    NodePointer(3, 1),
                    Word::new("で", "で", Speech::Particle(ParticleType::Case)),
                    Default::default()
                )
            ])
        );
        assert_eq!(
            graph.nodes[10]
                .iter()
                .cloned()
                .filter_map(|v| {
                    match v {
                        Node::Virtual(_, w, _) => Some(w.iter().collect::<String>()),
                        _ => None,
                    }
                })
                .collect::<HashSet<_>>(),
            HashSet::from([
                "ではしらなかった".to_string(),
                "まではしらなかった".to_string(),
                "はしらなかった".to_string()
            ])
        );
    }

    #[test]
    fn virtual_node_with_1_node() {
        // arrange
        let keys = LABELS.iter().cloned().collect::<Vec<_>>();
        let ancillary_trie = trie::Trie::from_keys(&keys);
        let mut standard_trie = trie::Trie::from_keys(&keys);

        standard_trie.insert("こ").unwrap();

        let dic = GraphDictionary {
            standard_trie,
            standard_dic: HashMap::from([(
                "こ".to_string(),
                vec![Word::new(
                    "来",
                    "こ",
                    Speech::Verb(VerbForm::Hen("カ".to_string())),
                )],
            )]),
            ancillary_trie,
            ancillary_dic: HashMap::from([]),
        };

        // act
        let graph = Graph::from_input("これでいける", &dic);

        // assert
        println!("{:?}", graph);
        assert_eq!(
            graph.nodes[5]
                .iter()
                .cloned()
                .filter_map(|v| {
                    match v {
                        Node::Virtual(_, w, _) => Some(w.iter().collect::<String>()),
                        _ => None,
                    }
                })
                .collect::<HashSet<_>>(),
            HashSet::from(["れでいける".to_string(),])
        );
    }

    #[test]
    fn find_node_after_prefix() {
        // arrange
        let keys = LABELS.iter().cloned().collect::<Vec<_>>();
        let mut ancillary_trie = trie::Trie::from_keys(&keys);
        let mut standard_trie = trie::Trie::from_keys(&keys);

        standard_trie.insert("こ").unwrap();
        ancillary_trie.insert("しん").unwrap();

        let dic = GraphDictionary {
            standard_trie,
            standard_dic: HashMap::from([(
                "こ".to_string(),
                vec![Word::new(
                    "来",
                    "こ",
                    Speech::Verb(VerbForm::Hen("カ".to_string())),
                )],
            )]),
            ancillary_trie,
            ancillary_dic: HashMap::from([(
                "しん".to_string(),
                vec![Word::new("新", "しん", Speech::Affix(AffixVariant::Prefix))],
            )]),
        };

        // act
        let graph = Graph::from_input("しんこか", &dic);

        // assert
        println!("{:?}", graph);
        assert_eq!(
            graph.nodes[1].iter().cloned().collect::<HashSet<_>>(),
            HashSet::from([Node::WordNode(
                NodePointer(1, 0),
                Word::new("新", "しん", Speech::Affix(AffixVariant::Prefix)),
                Default::default()
            ),])
        );
        assert_eq!(
            graph.nodes[2].iter().cloned().collect::<HashSet<_>>(),
            HashSet::from([Node::WordNode(
                NodePointer(2, 0),
                Word::new("来", "こ", Speech::Verb(VerbForm::Hen("カ".to_string())),),
                Default::default()
            ),])
        );
        assert_eq!(
            graph.nodes[3]
                .iter()
                .cloned()
                .filter_map(|v| {
                    match v {
                        Node::Virtual(_, w, _) => Some(w.iter().collect::<String>()),
                        _ => None,
                    }
                })
                .collect::<HashSet<_>>(),
            HashSet::from(["か".to_string(), "こか".to_string(),])
        );
    }
}
