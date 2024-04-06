pub(crate) mod node {
    use std::fmt::{self, Display};

    use serde::{Deserialize, Serialize};

    use super::{base::Base, check::Check};

    /// 内部で利用するbase/checkのペア
    /// ここで定義されるbase/checkは、内部的には未使用領域を負の値で管理している。
    /// baseにおける負の値は **前の未使用baseのindex** を負の値としており、 checkにおける負の値は、 **次の未使用checkのindex** を負の値としている
    #[derive(Clone, Eq, PartialEq, Serialize, Deserialize)]
    pub struct Node {
        pub base: Base,
        pub check: Check,
    }

    impl Node {
        pub fn new_root() -> Self {
            Self {
                base: Base::root(),
                check: Check::root(),
            }
        }

        pub fn new_empty() -> Self {
            Self {
                base: Base::empty(),
                check: Check::empty(),
            }
        }
    }

    impl fmt::Debug for Node {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "[b={}:c={}]", self.base, self.check)
        }
    }

    impl Display for Node {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "[b={}:c={}]", self.base, self.check)
        }
    }
}

pub(crate) mod empty {
    use std::ops;

    use serde::{Deserialize, Serialize};

    use super::{base::Base, label::Label, node_idx::NodeIdx};

    /// 未使用領域そのものを与える型
    #[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord, Serialize, Deserialize, Copy)]
    pub struct Empty(usize);

    impl From<&NodeIdx> for Empty {
        fn from(value: &NodeIdx) -> Self {
            Empty(usize::from(*value))
        }
    }

    impl From<&Base> for Empty {
        fn from(value: &Base) -> Self {
            Empty(usize::from(*value))
        }
    }

    impl ops::Sub<Label> for Empty {
        type Output = Option<Base>;

        fn sub(self, rhs: Label) -> Self::Output {
            let rhs = usize::from(rhs);
            if self.0 >= rhs {
                Some(Base::new((self.0 - rhs) as u32))
            } else {
                None
            }
        }
    }

    impl From<NodeIdx> for Empty {
        fn from(value: NodeIdx) -> Self {
            Self(usize::from(value))
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        use crate::types::{empty::Empty, label::Label};

        #[test]
        fn get_base_if_label_was_valid() {
            // arrange
            let label = Label::new(3);
            let empty = Empty::from(NodeIdx::from(5));

            // act
            let base = empty - label;

            // assert
            assert_eq!(base, Some(Base::new(2)));
        }

        #[test]
        fn do_not_return_base_if_base_is_invalid() {
            // arrange
            let label = Label::new(3);
            let empty = Empty::from(NodeIdx::from(2));

            // act
            let base = empty - label;

            // assert
            assert_eq!(None, base);
        }
    }
}

pub mod empties {
    use std::collections::HashSet;

    use super::{empty::Empty, node::Node, node_idx::NodeIdx};

    pub(crate) type Empties = HashSet<Empty>;

    /// 指定したindexを未使用領域から削除する
    pub fn delete_at<T>(empties: &mut Empties, idx: T)
    where
        Empty: From<T>,
    {
        empties.remove(&Empty::from(idx));
    }

    /// 対象の位置を未使用に変更する
    pub fn push_unused<T>(empties: &mut Empties, idx: T)
    where
        Empty: From<T>,
    {
        empties.insert(Empty::from(idx));
    }

    /// 未使用領域を `size` だけ広げる
    ///
    /// 拡張するというのは未使用領域に閉じている。
    pub fn expand_empties(vec: &mut Vec<Node>, empties: &mut Empties, size: usize) {
        let current_len = vec.len();

        // 拡張領域を作成する。末尾は常に先頭から最初に見つかった未使用領域を指す
        let expanded: Vec<Node> = vec![Node::new_empty(); size];

        // 追加する全体のindexを未使用領域に追加する
        empties.extend(
            (current_len..current_len + size).map(|v| -> Empty { NodeIdx::from(v).into() }),
        );

        // 直接追加する
        vec.extend_from_slice(&expanded)
    }

    #[cfg(test)]
    mod tests {
        use crate::types::{base::Base, check::Check, label::Label};

        use super::*;

        use std::collections::HashSet;

        #[test]
        fn expandable_empty_node() {
            // arrange
            let mut nodes = vec![Node {
                base: Base::root(),
                check: Check::root(),
            }];
            let mut empties: Empties = HashSet::new();

            // act
            expand_empties(&mut nodes, &mut empties, 4);

            // assert
            assert_eq!(
                nodes,
                vec![
                    Node {
                        base: Base::root(),
                        check: Check::root()
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty()
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty()
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty()
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty()
                    },
                ]
            );

            assert_eq!(
                empties,
                HashSet::from([
                    Empty::from(NodeIdx::from(2)),
                    Empty::from(NodeIdx::from(3)),
                    Empty::from(NodeIdx::from(4)),
                    Empty::from(NodeIdx::from(1)),
                ])
            );
        }

        #[test]
        fn expandable_with_having_empty() {
            // arrange
            let mut nodes = vec![
                Node {
                    base: Base::root(),
                    check: Check::root(),
                },
                Node {
                    base: Base::root(),
                    check: Check::empty(),
                },
                Node {
                    base: Base::root(),
                    check: Check::from(NodeIdx::from(1)),
                },
            ];
            let mut empties: Empties = HashSet::from([NodeIdx::from(1).into()]);

            // act
            expand_empties(&mut nodes, &mut empties, 4);

            // assert
            assert_eq!(
                nodes,
                vec![
                    Node {
                        base: Base::root(),
                        check: Check::root()
                    },
                    Node {
                        base: Base::root(),
                        check: Check::empty()
                    },
                    Node {
                        base: Base::root(),
                        check: Check::from(NodeIdx::from(1))
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty()
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty()
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty()
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty()
                    },
                ]
            );
            assert_eq!(
                empties,
                HashSet::from([
                    NodeIdx::from(1).into(),
                    NodeIdx::from(3).into(),
                    NodeIdx::from(4).into(),
                    NodeIdx::from(5).into(),
                    NodeIdx::from(6).into(),
                ])
            );
        }

        #[test]
        fn push_unused_to_vec() {
            // arrange
            let mut empties = HashSet::new();

            // act
            push_unused(&mut empties, &(Base::new(1) + Label::new(1)));

            // assert
            assert_eq!(empties, HashSet::from([NodeIdx::from(2).into(),]))
        }
    }
}

pub(crate) mod label {
    use std::collections::HashMap;

    use serde::{Deserialize, Serialize};

    /// 内部で利用する値に対して型での表明を行うための型を提供する
    ///
    /// このmoduleでは、今回定義するダブル配列において必要になる各種演算のみを許容するように定義している

    /// Labelの集合を表す型
    ///
    /// charからlabelへの変換を提供する。
    #[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
    pub struct Labels {
        labels: HashMap<char, u8>,
    }

    impl Labels {
        /// labelの集合の数を返す
        /// labelをキーになる値から生成する
        pub fn from_chars(keys: &[char]) -> Labels {
            assert!(!keys.is_empty(), "can not accept empty keys");
            let mut key_map = HashMap::new();

            for (i, key) in keys.iter().enumerate() {
                key_map.insert(*key, (i + 1) as u8);
            }

            Self { labels: key_map }
        }

        /// labelから文字に変換する
        ///
        /// # Arguments
        /// - `label` - 変換するlabel
        ///
        /// # Returns
        /// 対応する文字。labelが存在しない場合はpanic
        pub fn label_to_char(&self, label: &Label) -> Option<char> {
            if label.is_terminal() {
                return None;
            }

            self.labels
                .iter()
                .find(|(_, v)| **v == label.0)
                .map(|(k, _)| *k)
        }

        /// keyからLabelに変換する
        ///
        /// 変換後の順序は、 `key` の文字順と一致する
        pub fn key_to_labels(&self, key: &str) -> Result<Vec<Label>, ()> {
            let mut labels: Vec<Label> = Vec::new();

            for c in key.chars() {
                let id = self.label_of(c)?;
                labels.push(id);
            }

            // 終端しておく
            labels.push(Label::new(self.labels.len() as u8 + 1));

            Ok(labels)
        }

        /// 文字に対応するlabelを返す
        ///
        /// # Arguments
        /// - `c` - 変換する文字
        ///
        /// # Returns
        /// 対応するlabel。labelが存在しないcが渡されるとErr
        fn label_of(&self, c: char) -> Result<Label, ()> {
            if let Some(v) = self.labels.get(&c) {
                Ok(Label(*v))
            } else {
                Err(())
            }
        }

        /// 内部で保持しているラベルの集合を返す
        pub fn label_set(&self) -> Vec<Label> {
            let mut labels: Vec<_> = self.labels.values().map(|v| Label(*v)).collect();
            labels.push(Label::new(self.labels.len() as u8 + 1));

            labels
        }
    }

    /// labelを表す型
    #[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
    pub struct Label(u8);

    impl Label {
        pub fn new(v: u8) -> Self {
            assert!(v > 0, "All label must be greater than 0");
            Self(v)
        }

        fn is_terminal(&self) -> bool {
            self.0 == 0
        }
    }

    impl From<Label> for usize {
        fn from(value: Label) -> Self {
            value.0 as usize
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        #[should_panic]
        fn do_not_allow_empty_labels() {
            Labels::from_chars(&[]);
        }

        #[test]
        #[should_panic]
        fn do_not_allow_unnecessary_label() {
            Label::new(0);
        }

        #[test]
        fn get_labeled_keys_from_key() {
            // arrange
            let ls = Labels::from_chars(&['a', 'b', 'c']);

            // act
            let ret = ls.key_to_labels("aabc");

            // assert
            assert_eq!(
                ret,
                Ok(vec![
                    Label::new(1),
                    Label::new(1),
                    Label::new(2),
                    Label::new(3),
                    Label::new(4),
                ])
            )
        }

        #[test]
        fn should_return_error_if_key_contains_char_that_is_not_contained_labels() {
            // arrange
            let ls = Labels::from_chars(&['a', 'b', 'c']);

            // act
            let ret = ls.key_to_labels("adabc");

            // assert
            assert_eq!(ret, Err(()))
        }
    }
}

pub(crate) mod base {
    use std::{fmt::Display, ops};

    use serde::{Deserialize, Serialize};

    use super::{label::Label, node_idx::NodeIdx};

    /// Base自体を表す型
    ///
    /// Baseはそもそもoffset自体を表現しているので、実際はindexで利用することがほとんどである
    #[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize, PartialOrd, Ord, Hash)]
    pub struct Base(i32);

    impl Base {
        /// Baseを新規に作成する
        pub fn new(v: u32) -> Base {
            Self(v as i32)
        }

        /// rootとして扱うBase
        pub fn root() -> Base {
            Self(0)
        }

        /// 未使用のBaseとして扱う
        pub fn empty() -> Base {
            Self(-1)
        }

        /// 使用中であるかを返す
        pub fn is_used(&self) -> bool {
            self.0 >= 0
        }

        /// 未使用であるかを返す
        pub fn is_empty(&self) -> bool {
            self.0 < 0
        }
    }

    impl ops::Add<Label> for Base {
        type Output = NodeIdx;

        fn add(self, rhs: Label) -> Self::Output {
            assert!(self.0 >= 0, "base should be used");
            let rhs = usize::from(rhs);
            let idx = self.0 as usize + rhs;
            NodeIdx::from(idx)
        }
    }

    impl From<Base> for usize {
        fn from(value: Base) -> Self {
            value.0 as usize
        }
    }

    impl Display for Base {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        #[test]
        fn root_is_not_empty() {
            assert!(Base::root().is_used(), "should be used");
            assert!(!Base::root().is_empty(), "should be used");
        }

        #[test]
        fn make_transition_with_label() {
            // arrange
            let label = Label::new(3);
            let base = Base::new(2);

            // act
            let ret = base + label;

            // assert
            assert_eq!(ret, NodeIdx::from(5))
        }
    }
}

pub(crate) mod check {
    use std::fmt::Display;

    use serde::{Deserialize, Serialize};

    use super::node_idx::NodeIdx;

    /// Check自体を表す型
    #[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
    pub struct Check(i32);

    impl Check {
        /// rootに設定するためのcheckを返す
        pub fn root() -> Self {
            Check(0)
        }

        /// 内部用のfactory
        pub fn empty() -> Self {
            Check(-1)
        }

        /// Checkが未使用かどうか返す
        #[inline]
        pub fn is_empty(&self) -> bool {
            self.0 < 0
        }

        /// Checkが使用中かどうか返す
        #[inline]
        pub fn is_used(&self) -> bool {
            !self.is_empty()
        }

        /// checkが対象のnodeからの遷移であるかを返す
        pub fn is_transition_from(&self, idx: &NodeIdx) -> bool {
            self.is_used() && NodeIdx::from(*self) == *idx
        }
    }

    impl From<Check> for NodeIdx {
        /// NodeIdxに変換する
        fn from(val: Check) -> Self {
            assert!(val.is_used(), "should be used");
            NodeIdx::from(val.0 as usize)
        }
    }

    impl From<NodeIdx> for Check {
        /// NodeIdxから[Check]に変換する
        fn from(val: NodeIdx) -> Self {
            let val = i32::from(val);
            assert!(val >= 0, "node index should be greater equal 0");
            Self(val)
        }
    }

    impl Display for Check {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }
}

pub(crate) mod node_idx {
    use serde::{Deserialize, Serialize};

    /// Nodeを指し示すindex
    #[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
    pub struct NodeIdx(usize);

    impl NodeIdx {
        /// 先頭を表す[NodeIdx]を返す
        pub fn head() -> Self {
            Self(0)
        }
    }

    impl From<usize> for NodeIdx {
        fn from(value: usize) -> Self {
            Self(value)
        }
    }

    impl From<NodeIdx> for i32 {
        fn from(value: NodeIdx) -> Self {
            value.0 as i32
        }
    }

    impl From<NodeIdx> for usize {
        fn from(value: NodeIdx) -> Self {
            value.0
        }
    }
}
