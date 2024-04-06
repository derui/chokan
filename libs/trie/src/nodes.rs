use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use crate::types::{
    base::Base,
    check::Check,
    empties,
    empty::Empty,
    label::{Label, Labels},
    node::Node,
    node_idx::NodeIdx,
};

/// ダブル配列を構成するための基本的なデータ構造。
///
/// この配列アクセスが非常にわかりづらいため、型でラップしている。
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub(crate) struct Nodes {
    nodes: Vec<Node>,
    empties: HashSet<Empty>,
}

impl Nodes {
    pub fn new() -> Self {
        Nodes {
            nodes: vec![Node::new_root()],
            empties: HashSet::new(),
        }
    }

    /// [NodeIdx]が指すcheckを返す
    pub fn check_of(&self, check_idx: &NodeIdx) -> Option<Check> {
        self.nodes.get(usize::from(*check_idx)).map(|v| v.check)
    }

    /// [NodeIdx]が指すbaseを返す
    pub fn base_of(&self, check_idx: &NodeIdx) -> Option<Base> {
        self.nodes.get(usize::from(*check_idx)).map(|v| v.base)
    }

    /// [NodeIdx]が指すbaseを返す
    fn base_unchecked(&self, check_idx: &NodeIdx) -> &Base {
        &self.nodes[usize::from(*check_idx)].base
    }

    /// 指定したnodeからの遷移を、指定したbaseから遷移するようにrebaseする。
    ///
    /// あくまでnodeからの遷移のみを扱うため、遷移先から更に遷移しているnodeについてはサポートしない。
    ///
    /// # Arguments
    /// - `node` - 現在注目しているnode。
    /// - `new_base` - 新しい遷移先。conflictが発生しないことは、呼び出し元が保証すること。
    ///
    pub fn rebase(&mut self, node: &NodeIdx, new_base: &Base, labels: &Labels) {
        let old_base = *self.base_unchecked(node);
        let transitions = self.find_labels_of(node, labels);

        // まず元々のbase自体を更新する
        self.record_transition_base_at(node, new_base);

        for l in transitions.iter() {
            // 各labelを新しいbaseからの遷移として再登録する
            let idx = self.record_transition_at(node, l);

            // 移動したlabelからの遷移がある場合、baseをコピーしてくる
            let old_transitted_node_idx = old_base + *l;
            let old_transitted_node_base = *self.base_unchecked(&old_transitted_node_idx);
            if old_transitted_node_base.is_used() {
                self.record_transition_base_at(&idx, &old_transitted_node_base);

                // 遷移している先のidxは、新しいbaseを元にしたindexである必要があるので、入れ直す
                let labels_of_moved = self.find_labels_of(&old_transitted_node_idx, labels);
                for l in labels_of_moved {
                    self.record_transition_at(&idx, &l);
                }
            }

            // 移動が完了したら、元々のlabelがあった位置を未使用とする。
            // baseについても移動は終わっているので、初期化してしまって構わない
            empties::push_unused(&mut self.empties, &old_transitted_node_idx);
            assert!(*node != old_transitted_node_idx, "should be different");
            self.nodes[usize::from(old_transitted_node_idx)] = Node::new_empty();
        }
    }

    /// 指定した位置のidxからの遷移に `label` を追加する。追加されたlabelのnodeを指す[NodeIdx]を返す
    ///
    /// # Arguments
    /// - `idx` - 遷移元のindex
    /// - `label` - 設定するlabel
    ///
    /// # Returns
    /// 遷移を書き込んだ先の[NodeIdx]
    pub fn record_transition_at(&mut self, idx: &NodeIdx, label: &Label) -> NodeIdx {
        let check_idx = self.nodes[usize::from(*idx)].base + *label;
        let current_len = self.nodes.len();
        let next_size = usize::from(check_idx);
        // 必要なら領域を拡張する
        if current_len <= next_size {
            let new_size = next_size - current_len + 1;
            empties::expand_empties(&mut self.nodes, &mut self.empties, new_size);
        }

        // 対象が未使用だった場合は、未使用領域から外してから、対象の位置に書き込む
        empties::delete_at(&mut self.empties, &check_idx);
        self.nodes[usize::from(check_idx)].check = Check::from(*idx);

        check_idx
    }

    /// xcheckアルゴリズムの実装。
    ///
    /// # Arguments
    /// - `labels` - 遷移先ラベルの集合
    ///
    /// # Returns
    /// すべてのlabelが設定可能と判定されたbase
    pub fn xcheck(&self, labels: &[Label]) -> Base {
        assert!(!labels.is_empty(), "should not be empty");

        let ary_size = self.nodes.len() as u32;
        let mut labels = labels.to_owned();
        labels.sort();

        let min_label = labels.first().unwrap();
        let other_labels = &labels[1..];

        for e in self.empties.iter() {
            // 最小のlabelを基準にしてbaseを抽出することで、これが対象として利用できるか？を判定できる
            if let Some(t) = *e - *min_label {
                let mut is_ok = true;

                // 一つでも利用中のcheckがある場合は対象外
                for label in other_labels {
                    let i = t + *label;
                    if !self.empties.contains(&Empty::from(i)) {
                        is_ok = false;
                        break;
                    }
                }
                if is_ok {
                    return t;
                }
            }
        }

        Base::new(ary_size)
    }

    /// 指定したnodeの遷移として利用するbaseを登録する
    ///
    /// # Arguments
    /// - `idx` - 遷移元のindex
    /// - `base` - 設定するbase
    ///
    pub fn record_transition_base_at(&mut self, idx: &NodeIdx, base: &Base) {
        assert!(base.is_used(), "should be used");
        self.nodes[usize::from(*idx)].base = *base;
    }

    /// 現在指している `idx` から遷移するlabelの一覧を返す
    ///
    /// # Arguments
    /// - `idx` - 遷移元のindex
    /// - `labels` - labelの一覧
    ///
    /// # Returns
    /// 遷移先のlabelの一覧
    pub fn find_labels_of(&self, idx: &NodeIdx, labels: &Labels) -> Vec<Label> {
        let mut result = Vec::new();
        let base = &self.nodes[usize::from(*idx)].base;

        if base.is_empty() {
            return result;
        }

        // 遷移先の数の上限は、 `labels` から返されるlabelの数が上限になる
        for l in labels.label_set().iter() {
            let l_idx = *base + *l;

            match self.check_of(&l_idx) {
                Some(ck) if ck.is_transition_from(idx) => {
                    result.push(*l);
                }
                _ => (),
            }
        }

        result
    }
}
