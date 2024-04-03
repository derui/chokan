/// Trieのデータ型と操作を提供するmodule
mod types;

use std::{collections::HashSet, usize};

use serde::{Deserialize, Serialize};
use types::{
    base::Base,
    check::Check,
    empties,
    empty::Empty,
    label::{Label, Labels},
    node::Node,
    node_idx::NodeIdx,
};

/// 最も基本的なtrie構造を表現する
/// 任意のデータ構造を保有する必要がある場合は、[HoldableTrie<T>]を利用すること
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Trie {
    /// ダブル配列を構成するための基本的なデータ構造。
    nodes: Vec<Node>,

    /// 内部で値として利用する、文字とデータIDのマッピングを管理する
    /// 今回は255種類のみ許容する
    labels: Labels,

    /// 未使用の領域を管理する
    empties: HashSet<Empty>,
}

impl Trie {
    /// 新しいTrieを生成する
    pub fn from_keys(keys: &[char]) -> Trie {
        let labels = Labels::from_chars(keys);

        let nodes = vec![
            // 仕組み上、0のcheckが利用されることはないため、初期化時点では常に1を指しておく
            Node::new_root(),
        ];

        Trie {
            nodes,
            labels,
            empties: HashSet::new(),
        }
    }

    /// 現在指している `idx` から遷移するlabelの一覧を返す
    ///
    /// # Arguments
    /// - `idx` - 遷移元のindex
    ///
    /// # Returns
    /// 遷移先のlabelの一覧
    fn find_labels_of(&self, idx: &NodeIdx) -> Vec<Label> {
        let mut labels = Vec::new();
        let base = self.nodes[usize::from(*idx)].base;

        if base.is_empty() {
            return labels;
        }

        // 遷移先の数の上限は、 `labels` から返されるlabelの数が上限になる
        for l in self.labels.label_set() {
            let l_idx = base + l;

            match self.nodes.get(usize::from(l_idx)) {
                Some(ck) if ck.is_transit(idx) => {
                    labels.push(l);
                }
                _ => (),
            }
        }

        labels
    }

    /// xcheckアルゴリズムの実装。
    ///
    /// # Arguments
    /// - `labels` - 遷移先ラベルの集合
    ///
    /// # Returns
    /// 返却される値は、すべてのlabelが設定可能と判定されたbaseのindexである
    fn xcheck(&self, labels: &[Label]) -> Base {
        let ary_size = self.nodes.len() as u32;
        let mut labels = labels.to_owned();

        if labels.is_empty() {
            return Base::new(ary_size);
        }

        labels.sort();

        let min_label = labels.first().unwrap();
        let other_labels = &labels[1..];

        for e in self.empties.iter() {
            // 実際に空として認識するのは、最小のlabelのみが基準となる
            if let Some(t) = *e - *min_label {
                let mut is_ok = true;

                // 一つでも利用中のcheckがある場合は、何もしない
                for label in other_labels {
                    let i = t + *label;
                    match self.nodes.get(usize::from(i)) {
                        Some(n) if n.check.is_used() => {
                            is_ok = false;
                            break;
                        }
                        None => {
                            // Noneになる場合、範囲を超えてしまっているので、領域の追加が必要であるが、空として認識できる
                        }
                        _ => (),
                    }
                }

                if is_ok {
                    return t;
                }
            }
        }

        Base::new(ary_size)
    }

    /// 指定されているnodeとlabelから、衝突しているnodeの移動を行う
    ///
    /// 此処で移動対象になる木は、labelの数が少ない方を優先する。
    ///
    /// # Arguments
    /// - `node` - 現在注目しているnode。baseを取得したもとのindexになる
    /// - `label` - 設定対象のlabel
    ///
    /// # Returns
    /// 現在注目していた[NodeIdx]に対して衝突を回避した後、基準になるBase
    fn move_conflicted(&mut self, node: &NodeIdx, label: &Label, base: &Base) -> NodeIdx {
        // 移動する対象を確定する
        let detect_to_move_base: (NodeIdx, Vec<Label>);

        // できるだけ遷移先が少ない方を移動する
        {
            let check_idx = *base + *label;
            let conflicted_node = NodeIdx::from(self.nodes[usize::from(check_idx)].check);

            assert!(*node != conflicted_node, "each node should be different");

            let current_labels = self.find_labels_of(node);
            let conflicted_labels = self.find_labels_of(&conflicted_node);

            // 今注目しているnodeのlabelsは、本来移動するものより一個少ない
            // ただし、この後の処理で移動するときに問題になるので、labelsにはくわえていない
            if current_labels.len() + 1 < conflicted_labels.len() {
                detect_to_move_base = (*node, current_labels);
            } else {
                detect_to_move_base = (conflicted_node, conflicted_labels);
            }
        }

        // 全体が入る先を探索して、そこにラベル一式を移動する
        let new_base = self.xcheck(&detect_to_move_base.1);
        let current_base = self.nodes[usize::from(detect_to_move_base.0)].base;
        for l in detect_to_move_base.1.iter() {
            let idx = self.write_at(&detect_to_move_base.0, l, &new_base);

            // 移動したlabelからの遷移がある場合、その遷移先に置き換える
            if self.nodes[usize::from(current_base + *l)].base.is_used() {
                self.nodes[usize::from(idx)].base = self.nodes[usize::from(current_base + *l)].base;

                let labels_of_moved = self.find_labels_of(&(current_base + *l));
                let transition_base = self.nodes[usize::from(current_base + *l)].base;
                for l in labels_of_moved {
                    self.nodes[usize::from(transition_base + l)].check = Check::from(idx);
                }
            }

            // 移動が完了したら、元々のlabelがあった位置を未使用とする。baseについても初期化する
            empties::push_unused(&mut self.empties, current_base + *l);
            self.nodes[usize::from(current_base + *l)].base = Base::empty();
        }

        // 現在注目しているnodeが移動した場合は、新しいbaseに対して書き込み、そうではない場合はbaseを起点にして書き込む
        if detect_to_move_base.0 == *node {
            self.write_at(node, label, &new_base)
        } else {
            self.write_at(node, label, base)
        }
    }

    /// 指定した位置に `label` を書き込み、書き込んだ位置を [NodeIdx] として返す
    ///
    /// # Arguments
    /// - `idx` - 遷移元のindex
    /// - `label` - 設定するlabel
    /// - `base` - 設定もとのbase
    ///
    /// # Returns
    /// checkを書き込んだ先の[NodeIdx]
    fn write_at(&mut self, idx: &NodeIdx, label: &Label, base: &Base) -> NodeIdx {
        // 必要なら領域を拡張してから書き込む
        let next_idx = *base + *label;
        let current_len = self.nodes.len();
        if current_len <= usize::from(next_idx) {
            empties::expand_empties(
                &mut self.nodes,
                &mut self.empties,
                usize::from(next_idx) - current_len + 1,
            );
        }

        // 先に未使用領域から外してから、対象の位置に書き込む
        empties::delete_at(&mut self.empties, &next_idx);
        self.nodes[usize::from(next_idx)].check = Check::from(*idx);
        self.nodes[usize::from(*idx)].base = *base;

        next_idx
    }

    /// Trieに新しいキーを追加する
    ///
    /// 追加は、以下のアルゴリズムで行う。
    /// 1. キーをlabelに変換する
    /// 2. 先頭のlabelから、baseを計算する
    /// 3. 計算したbaseから遷移先を取得する
    /// 4. 今回追加するlabelが衝突していないかcheckする
    /// 5. 衝突していない場合、label含めて対応するbaseを設定する
    ///   5.1 衝突していた場合、移動可能な場所を探索し、書き込む
    ///   5.2 衝突していた方の元々の位置を初期化する
    ///   5.3 移動していない方を、起点の場所に書き込む
    /// 6. 次のlabelの位置を起点にして、2-5を繰り返す
    ///
    /// # Arguments
    /// - `key` - 追加するキー
    ///
    /// # Returns
    /// すでに存在するキーの場合は、何も行わない。
    /// 利用できない文字が含まれている場合は、Errを返す
    pub fn insert(&mut self, key: &str) -> Result<(), ()> {
        let labels = self.labels.key_to_labels(key)?;
        let mut current = NodeIdx::head();

        for label in labels.iter() {
            let node = &self.nodes[usize::from(current)];

            // baseが未使用の場合は、xcheck経由で新しいbaseを計算する
            let mut base = node.base;
            if base.is_empty() {
                let mut labels = self.find_labels_of(&current);
                labels.push(*label);
                base = self.xcheck(&labels);
            }

            match self.nodes.get(usize::from(base + *label)) {
                Some(n) if n.is_transit(&current) => {
                    // labelに対して遷移が記録されている場合、遷移先から進める
                    current = node.next_node(label);
                    continue;
                }
                Some(n) if !n.can_transit() => {
                    // 使用中の場合は衝突しているので、衝突を解消してから書き込みを行う
                    current = self.move_conflicted(&current, label, &base);
                }
                _ => {
                    current = self.write_at(&current, label, &base);
                }
            }
        }

        Ok(())
    }

    /// Trieから指定したキーを検索する
    ///
    /// 指定されたキーの部分文字列が発見された場合は、その都度 `callback` が呼び出される。
    ///
    /// # Arguments
    /// - `key` - 検索するキー
    /// - `callback` - 部分文字列が発見された場合に呼び出される関数
    ///
    /// # Returns
    /// 発見されたnodeのindex。見つからなかった場合はNone
    pub fn search(&self, key: &str, callback: &dyn Fn(NodeIdx, &str)) -> Option<usize> {
        let labels = self.labels.key_to_labels(key).expect(key);
        let mut current = NodeIdx::head();
        let mut result = Vec::new();
        let mut found_labels = String::new();

        for label in labels.iter() {
            let node = &self.nodes[usize::from(current)];
            if node.base.is_empty() {
                return None;
            }

            let transition = node.base + *label;
            match self.nodes.get(usize::from(transition)) {
                Some(n) if !n.is_transit(&current) => {
                    return None;
                }
                None => {
                    return None;
                }
                _ => (),
            }

            found_labels.push(self.labels.label_to_char(label));
            current = transition;
            callback(current, &found_labels);
            result.push(current);
        }

        Some(usize::from(current))
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, collections::HashSet};

    use crate::Trie;

    fn labels() -> Vec<char> {
        vec!['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
    }

    #[test]
    fn write_key_and_search_it() {
        // arrange
        let mut trie = Trie::from_keys(&labels());
        let _ = trie.insert("abc");

        // act
        let index = trie.search("ab", &|_, _| {});
        // assert
        assert_ne!(index, None);
    }

    #[test]
    fn return_not_found_path() {
        // arrange
        let mut trie = Trie::from_keys(&labels());
        let _ = trie.insert("abc");

        // act
        let index = trie.search("ac", &|_, _| {});
        // assert
        assert_eq!(index, None);
    }

    #[test]
    fn get_first_part_match_label() {
        // arrange
        let mut trie = Trie::from_keys(&labels());
        let _ = trie.insert("abc");
        let _ = trie.insert("aac");

        // act
        let chars = RefCell::new(vec![]);
        let _ = trie.search("aac", &|_, k| {
            let mut c = chars.borrow_mut();
            c.push(k.to_string())
        });

        // assert
        assert_eq!(
            chars.borrow().iter().cloned().collect::<HashSet<_>>(),
            HashSet::from_iter(
                ["a".to_string(), "aa".to_string(), "aac".to_string()]
                    .iter()
                    .cloned()
            )
        );
    }

    #[test]
    fn get_conflicted_path() {
        // arrange
        let mut trie = Trie::from_keys(&labels());
        let _ = trie.insert("abc");
        let _ = trie.insert("aac");

        // act
        let chars = RefCell::new(vec![]);
        let _ = trie.search("abc", &|_, k| {
            let mut c = chars.borrow_mut();
            c.push(k.to_string())
        });

        // assert
        assert_eq!(
            chars.borrow().iter().cloned().collect::<HashSet<_>>(),
            HashSet::from_iter(
                ["a".to_string(), "ab".to_string(), "abc".to_string()]
                    .iter()
                    .cloned()
            )
        );
    }

    #[test]
    fn japanese_label_case_1() {
        // arrange
        let mut trie = Trie::from_keys(&"じっしつてきになさい".chars().collect::<Vec<_>>());
        let _ = trie.insert("じっしつ");
        let _ = trie.insert("じっしつてき");
        let _ = trie.insert("じっしつてきに");
        let _ = trie.insert("じっしつてきな");
        let _ = trie.insert("じって");
        let _ = trie.insert("じっさい");

        // act

        // assert
        assert!(
            trie.search("じっしつてきに", &|_, _| {}).is_some(),
            "じっしつてきに"
        );
        assert!(
            trie.search("じっしつて", &|_, _| {}).is_some(),
            "じっしつて"
        );
        assert!(trie.search("じっしつ", &|_, _| {}).is_some(), "じっしつ");
        assert!(
            trie.search("じっしつてき", &|_, _| {}).is_some(),
            "じっしつてき"
        );
        assert!(
            trie.search("じっしつてきな", &|_, _| {}).is_some(),
            "じっしつてきな"
        );
        assert!(trie.search("じって", &|_, _| {}).is_some(), "じって");
        assert!(trie.search("じっさい", &|_, _| {}).is_some(), "じっさい");
    }
}
