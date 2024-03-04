/// Trieのデータ型と操作を提供するmodule
mod types;

use std::collections::HashSet;

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

        let value_len = labels.len();
        empties::expand_empties(&mut nodes, value_len);

        Trie { nodes, labels }
    }

    /// checkを変更する。
    ///
    /// checkの変更では、対象が未使用である場合には、一つ前のindexが、更新しようとしているcheckが指している
    /// indexを指すようにしなければならない
    fn write_check(&mut self, _base: i32, _label: i32) {}

    /// nodeから遷移する先のラベルを挿入する
    ///
    /// ラベル集合が挿入できる場所に挿入するが、挿入できない場合は新しい領域を確保する。
    /// この処理では、衝突回避は行わず、挿入と拡張のみを行う。
    ///
    /// # Arguments
    /// - `node` - 現在注目しているnode
    /// - `base` - baseとして設定する値
    /// - `labels` - 遷移先ラベルの集合
    ///
    fn write_labels(&mut self, _node: usize, _base: i32, _labels: &HashSet<i32>) {}

    /// xcheckアルゴリズムの実装。
    ///
    /// # Arguments
    /// - `labels` - 遷移先ラベルの集合
    ///
    /// # Returns
    /// 返却される値は、すべてのlabelが設定可能と判定されたbaseのindexである
    fn xcheck(&self, labels: &Vec<Label>) -> Base {
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
                    if self.nodes[usize::from(i)].check.is_used() {
                        is_ok = false;
                        break;
                    }
                }

                if is_ok {
                    // 見つけたbaseを利用する。
                    return t;
                }
            }
        }

        // 一切見つからなかった場合は、新しい領域を追加する必要がある
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
    /// すでに存在するキーの場合は、そのIDを返す。
    /// 利用できない文字が含まれている場合は、Errを返す
    pub fn insert(&mut self, key: &str) -> Result<i32, ()> {
        let labels = self.labels.key_to_labels(key)?;
        let mut current = NodeIdx::head();

        for label in labels.iter() {
            let base = self.nodes[usize::from(current)].base;
            let transition = base + *label;

            // 対象の場所が空いていればそのままそこに書き込み、空いていなければ衝突しているので解消する
            self.nodes[usize::from(transition)].check.is_empty();

            // 次の位置はcheckの場所からになる
            current = transition.into();
        }

        Ok(i32::from(current))
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn it_works() {}
}
