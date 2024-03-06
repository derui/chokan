/// Trieのデータ型と操作を提供するmodule
mod types;

use types::{empties, Base, Check, Label, Labels, Node, NodeIdx};

/// 最も基本的なtrie構造を表現する
/// 任意のデータ構造を保有する必要がある場合は、[HoldableTrie<T>]を利用すること
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Trie {
    /// ダブル配列を構成するための基本的なデータ構造。
    nodes: Vec<Node>,

    /// 内部で値として利用する、文字とデータIDのマッピングを管理する
    /// 今回は255種類のみ許容する
    labels: Labels,
}

impl Trie {
    /// 新しいTrieを生成する
    pub fn from_keys(keys: &Vec<char>) -> Trie {
        let labels = Labels::from_chars(keys);

        let mut nodes = Vec::new();
        // 仕組み上、0のcheckが利用されることはないため、初期化時点では常に1を指しておく
        nodes.push(Node {
            base: Base::root(),
            check: Check::root(),
        });

        Trie { nodes, labels }
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
                Some(ck) if ck.is_transit(&l_idx) => {
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
    fn xcheck(&mut self, labels: &Vec<Label>) -> Base {
        let ary_size = self.nodes.len() as u32;
        let mut labels = labels.clone();

        labels.sort();

        let min_label = labels.first().unwrap();
        let other_labels = &labels[1..];

        for e in empties::as_empties(&self.nodes) {
            // 実際に空として認識するのは、最小のlabelのみが基準となる
            if let Some(t) = e - *min_label {
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
                            // Noneになる場合、範囲を超えてしまっているので、領域の追加が必要である
                            is_ok = false;
                            break;
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

    /// 指定したnodeから参照しているcheckを未使用状態に戻す
    fn clear(&mut self, _node_idx: &i32, _new_base: &i32) {}

    /// 指定されているbaseとlabelから、衝突しているbaseの移動を行う
    ///
    /// 此処で移動対象になる木は、labelの数が少ない方を優先する。
    ///
    /// # Arguments
    /// - `node` - 現在注目しているnode。baseを取得したもとのindexになる
    /// - `check` - 設定先のcheck
    /// - `label` - 対象のlabel
    ///
    fn modify(&mut self, _node: &i32, _check_idx: &i32, _label: &i32) {
        unimplemented!();
        // let check_node = self.nodes[*check_idx as usize];

        // assert!(
        //     check_node.base != dbary::calc_base(*check_idx, *label),
        //     "should be different base"
        // );

        // let current_base = dbary::calc_base(*check_idx, *label);
        // let mut current_labels = self.get_labels_of(current_base);
        // let mut other_labels = self.get_labels_of(check_node.base);

        // // 対象も含めて移動することになるので、ここのタイミングで追加しておく
        // current_labels.insert(*label);

        // // それぞれ新しい場所に書き込むのだが、移動した方は、元々の場所を初期化しなければならない
        // if current_labels.len() < other_labels.len() {
        //     let new_base = self.xcheck(&current_labels);
        //     self.write_labels(*node, new_base, &current_labels);
        //     self.clear(node, &new_base);
        // } else {
        //     // 元々あった方を動かす場合は、baseがcheckが指している場所になる。
        //     let new_base = self.xcheck(&other_labels);
        //     self.write_labels(*check_node, new_base, &other_labels);
        //     self.clear(&current_base, &new_base);
        // }
    }

    /// 指定した位置に `label` を書き込み、書き込んだ位置を [NodeIdx] として返す
    ///
    /// # Arguments
    /// - `idx` - 遷移元のindex
    /// - `label` - 設定するlabel
    /// - `base` - 設定もとのbase
    fn write_at(&mut self, idx: &NodeIdx, label: &Label, base: &Base) -> NodeIdx {
        // 必要なら領域を拡張してから書き込む
        let transition = *base + *label;
        let current_len = self.nodes.len();
        if current_len <= usize::from(transition) {
            empties::expand_empties(&mut self.nodes, usize::from(transition) - current_len + 1);
        }

        // 先に未使用領域から外してから、対象の位置に書き込む
        let next_idx = transition;
        empties::delete_at(&mut self.nodes, &next_idx);
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
            let mut base = self.nodes[usize::from(current)].base;

            // baseが未使用の場合は、xcheck経由で新しいbaseを計算する
            if base.is_empty() {
                base = self.xcheck(&vec![*label]);
            }

            current = self.write_at(&current, label, &base);
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
        let labels = self.labels.key_to_labels(key).unwrap();
        let mut current = NodeIdx::head();
        let mut result = Vec::new();
        let mut found_labels = String::new();

        for label in labels.iter() {
            let node = &self.nodes[usize::from(current)];
            let transition = node.base + *label;

            if node.is_transit(&transition) {
                return None;
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
    use std::{borrow::BorrowMut, cell::RefCell, collections::HashSet};

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
    fn get_first_part_match_label() {
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
}
