mod nodes;
mod types;

use nodes::Nodes;
use serde::{Deserialize, Serialize};
use std::usize;
use types::{
    base::Base,
    label::{Label, Labels},
    node_idx::NodeIdx,
};

/// 最も基本的なtrie構造を表現する
/// 任意のデータ構造を保有する必要がある場合は、[HoldableTrie<T>]を利用すること
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Trie {
    /// ダブル配列を構成するための基本的なデータ構造。
    nodes: Nodes,

    /// 内部で値として利用する、文字とデータIDのマッピングを管理する
    /// 今回は255種類のみ許容する
    labels: Labels,
}

impl Trie {
    /// 新しいTrieを生成する
    pub fn from_keys(keys: &[char]) -> Trie {
        let labels = Labels::from_chars(keys);

        Trie {
            nodes: Nodes::new(),
            labels,
        }
    }

    /// 指定されているnodeとlabelから、衝突しているnodeの移動を行う
    ///
    /// 此処で移動対象になる木は、labelの数が少ない方を優先する。
    ///
    /// # Arguments
    /// - `node` - 現在注目しているnode。baseを取得したもとのindexになる
    /// - `label` - 設定対象のlabel
    ///
    fn move_conflicted(&mut self, node: &NodeIdx, label: &Label) {
        // 移動する対象を確定する
        let detect_to_move_base: (NodeIdx, Vec<Label>);

        // できるだけ遷移先が少ない方を移動する
        {
            let check_idx = self.nodes.base_of(node).expect("should be a base") + *label;
            let conflicted_node: NodeIdx = self
                .nodes
                .check_of(&check_idx)
                .expect("Can not get check")
                .into();

            assert!(*node != conflicted_node, "each node should be different");

            let mut current_labels = self.nodes.find_labels_of(node, &self.labels);
            let conflicted_labels = self.nodes.find_labels_of(&conflicted_node, &self.labels);
            current_labels.push(*label);

            // conflictしたlabel自体も対象に加えて、移動する方を決定する
            if current_labels.len() < conflicted_labels.len() {
                detect_to_move_base = (*node, current_labels);
            } else {
                detect_to_move_base = (conflicted_node, conflicted_labels);
            }
        }

        // 全体が入る先を探索して、移動が必要なものを移動する
        let new_base = self.nodes.xcheck(&detect_to_move_base.1);
        self.nodes
            .rebase(&detect_to_move_base.0, &new_base, &self.labels);
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
        let _need_restart = false;

        for (_i, label) in labels.iter().enumerate() {
            // baseが未使用の場合は、xcheck経由で新しいbaseを計算する
            let base: Base;
            if let Some(b) = self.nodes.base_of(&current) {
                if b.is_empty() {
                    let labels = vec![*label];
                    base = self.nodes.xcheck(&labels);
                    self.nodes.record_transition_base_at(&current, &base);
                } else {
                    base = b;
                }
            } else {
                // 取得できない場合はどこかの処理がおかしいので、継続できない。
                return Err(());
            }

            match self.nodes.check_of(&(base + *label)) {
                Some(n) if n.is_transition_from(&current) => {
                    // labelに対して遷移が記録されている場合、遷移先から進める
                    current = base + *label;
                }
                Some(n) if n.is_used() => {
                    // 使用中の場合は衝突しているので、衝突を解消してから書き込みを行う
                    // ただし、衝突を回避した時点で、currentが使い物にならなくなる場合があるため、再度先頭から挿入し直す
                    self.move_conflicted(&current, label);
                    current = self.nodes.record_transition_at(&current, label);
                }
                _ => {
                    current = self.nodes.record_transition_at(&current, label);
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
        let labels = match self.labels.key_to_labels(key) {
            Ok(it) => it,
            Err(()) => return None,
        };
        let mut current = NodeIdx::head();
        let mut result = Vec::new();
        let mut found_labels = String::new();

        for label in labels.iter() {
            if let Some(base) = self.nodes.base_of(&current) {
                if base.is_empty() {
                    return None;
                }

                let transition = base + *label;
                match self.nodes.check_of(&transition) {
                    Some(n) if !n.is_transition_from(&current) => {
                        return None;
                    }
                    None => {
                        return None;
                    }
                    _ => (),
                }

                if let Some(ch) = self.labels.label_to_char(label) {
                    found_labels.push(ch);
                }
                current = transition;
                callback(current, &found_labels);
                result.push(current);
            } else {
                return None;
            }
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
        let index = trie.search("abc", &|_, _| {});
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
            trie.search("じっしつて", &|_, _| {}).is_none(),
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
