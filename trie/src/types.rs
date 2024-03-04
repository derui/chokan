use std::{collections::HashMap, ops};

/// 内部で利用するbase/checkのペア
/// ここで定義されるbase/checkは、内部的には未使用領域を負の値で管理している。
/// baseにおける負の値は **前の未使用baseのindex** を負の値としており、 checkにおける負の値は、 **次の未使用checkのindex** を負の値としている
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Node {
    pub base: Base,
    pub check: Check,
}

/// 未使用領域そのものを与える型
#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Empty(usize);

impl ops::Sub<Label> for Empty {
    type Output = Option<Base>;

    fn sub(self, rhs: Label) -> Self::Output {
        if self.0 >= rhs.0 as usize {
            Some(Base::new((self.0 - rhs.0 as usize) as u32))
        } else {
            None
        }
    }
}

impl Empty {
    fn new(v: usize) -> Self {
        Empty(v)
    }
}

pub mod empties {
    use std::collections::HashSet;

    use super::{Base, Check, Empty, Node, Transition};

    /// 空配列の集合を返す
    pub fn as_empties(nodes: &Vec<Node>) -> Vec<Empty> {
        let mut set: HashSet<Empty> = HashSet::new();

        let mut root = nodes
            .iter()
            .find(|v| v.check.is_empty())
            .and_then(|v| v.check.next_empty());

        loop {
            if let Some(rt) = root {
                if set.contains(&rt) {
                    break;
                }
                set.insert(rt.clone());

                root = nodes[rt.0].check.next_empty()
            } else {
                break;
            }
        }

        let mut vec = set.iter().cloned().collect::<Vec<_>>();
        vec.sort();
        vec
    }

    /// 対象の遷移位置を未使用に変更する
    pub fn push_unused(vec: &mut Vec<Node>, transiton: Transition) {
        let base_idx: usize = transiton.into();

        for i in 1..base_idx {
            if let Some(old) = vec[base_idx - i].check.next_empty() {
                vec[base_idx - i].check = Check::into_chain(transiton);
                vec[base_idx].check = Check::from(old);
                break;
            }
        }
    }

    /// 対象の遷移位置がemptyかどうかを返す
    pub fn is_empty_at(vec: &Vec<Node>, transition: Transition) -> bool {
        if let Some(v) = vec.get(usize::from(transition)) {
            v.check.is_empty()
        } else {
            true
        }
    }

    /// 未使用領域を `size` だけ広げる
    ///
    /// 拡張するというのは未使用領域に閉じている。
    pub fn expand_empties(vec: &mut Vec<Node>, size: usize) {
        let current_len = vec.len();

        // 拡張領域を作成する。末尾は常に先頭から最初に見つかった未使用領域を指す
        let mut expanded: Vec<Node> = (0..size)
            .map(|idx| Node {
                base: Base::empty(),
                check: Check::empty_at(current_len + idx + 1),
            })
            .collect();

        expanded[size - 1].check = vec
            .iter()
            .position(|v| v.check.is_empty())
            .map(|i| Check::empty_at(i))
            .unwrap_or(Check::empty_at(current_len));

        // 現在ある末尾のcheckの値を、今追加する先頭のindexに向ける
        // 追加するcheckの末尾は、未使用領域の先頭に向けておく
        // ここではSomeではあるが、実際にはrootの位置が常に未使用になるため、Noneになることはない。
        if let Some((idx, _)) = vec.iter().enumerate().rfind(|(_, v)| v.check.is_empty()) {
            vec[idx].check = Check::empty_at(current_len);
        }

        // 直接追加する
        vec.extend_from_slice(&expanded)
    }
}

/// 内部で利用する値に対して型での表明を行うための型を提供する
///
/// このmoduleでは、今回定義するダブル配列において必要になる各種演算のみを許容するように定義している

/// Labelの集合を表す型
///
/// charからlabelへの変換も実施するが、
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Labels(HashMap<char, u8>);

impl Labels {
    /// labelの集合の数を返す
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn from_chars(keys: &Vec<char>) -> Labels {
        assert!(!keys.is_empty(), "can not accept empty keys");
        let mut key_map = HashMap::new();

        for (i, key) in keys.iter().enumerate() {
            key_map.insert(*key, (i + 1) as u8);
        }

        Self(key_map)
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
        if let Some(v) = self.0.get(&c) {
            Ok(Label(*v))
        } else {
            Err(())
        }
    }

    /// 内部で保持しているラベルの集合を返す
    pub fn labels(&self) -> Vec<Label> {
        self.0.values().map(|v| Label(*v)).collect()
    }
}

/// Base自体を表す型
///
/// Baseはそもそもoffset自体を表現しているので、実際はindexで利用することがほとんどである
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Base(i32);

impl Base {
    /// Baseから利用されうるtransitionの範囲を返す
    pub fn transition_range(self, labels: &Labels) -> Vec<Transition> {
        let labels = labels.labels();
        labels.iter().map(|v| self + *v).collect()
    }

    /// Baseを新規に作成する
    pub fn new(v: u32) -> Base {
        assert!(v > 0, "All bases are should greater than 0");
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
    type Output = Transition;

    fn add(self, rhs: Label) -> Self::Output {
        assert!(self.0 >= 0, "base should be used");
        Transition(self.0 as usize + rhs.0 as usize)
    }
}

/// Check自体を表す型
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Check(i32);

impl Check {
    /// rootに設定するためのcheckを返す
    pub fn root() -> Self {
        Check(0)
    }

    /// 内部用のfactory
    fn empty_at(s: usize) -> Self {
        Check(-(s as i32))
    }

    /// 指定したTransitionを未使用のCheckに変換する
    ///
    /// # Returns
    /// 対象の位置を未使用としてマークした[Check]
    pub fn into_chain(transition: Transition) -> Self {
        Self(-(transition.0 as i32))
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

    /// 次の未使用checkを返す
    pub fn next_empty(&self) -> Option<Empty> {
        if self.is_used() {
            None
        } else {
            Some(Empty((-self.0) as usize))
        }
    }
}

impl From<Check> for Base {
    /// Baseに変換する
    fn from(val: Check) -> Self {
        assert!(val.is_used(), "should be used");
        Base(val.0)
    }
}

impl From<Empty> for Check {
    fn from(value: Empty) -> Self {
        Self(-(value.0 as i32))
    }
}

/// Baseから変換する。
impl From<Base> for Check {
    fn from(value: Base) -> Self {
        assert!(value.is_used(), "Can not convert empty base to check");
        Self(value.0)
    }
}

/// labelを表す型
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct Label(u8);

impl Label {
    fn new(v: u8) -> Self {
        Self(v)
    }
}

/// Nodeを指し示すindex
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct NodeIdx(usize);

impl NodeIdx {
    /// 先頭を表す[NodeIdx]を返す
    pub fn head() -> Self {
        Self(0)
    }
}

impl From<NodeIdx> for usize {
    fn from(value: NodeIdx) -> Self {
        value.0
    }
}

impl From<NodeIdx> for i32 {
    fn from(value: NodeIdx) -> Self {
        value.0 as i32
    }
}

/// 遷移を表す型
///
/// 実際には、Checkを指し示すindex
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct Transition(usize);

impl Transition {
    fn is_root(&self) -> bool {
        self.0 == 0
    }
}

impl From<Transition> for NodeIdx {
    fn from(val: Transition) -> Self {
        NodeIdx(val.0)
    }
}

impl From<Transition> for usize {
    fn from(value: Transition) -> Self {
        value.0
    }
}

impl ops::Sub<Label> for Transition {
    type Output = Base;

    fn sub(self, rhs: Label) -> Self::Output {
        Base((self.0 - rhs.0 as usize) as i32)
    }
}

#[cfg(test)]
mod tests {

    mod base {
        use crate::types::{Base, Label, Labels, Transition};

        #[test]
        fn root_is_not_empty() {
            assert!(Base::root().is_used(), "should be used");
            assert!(!Base::root().is_empty(), "should be used");
        }

        #[test]
        #[should_panic]
        fn should_panic_when_new_base_with_root_index() {
            Base::new(0);
        }

        #[test]
        fn get_transition_range() {
            // arrange
            let labels = Labels::from_chars(&vec!['a', 'b']);

            // act
            let base = Base::new(12);
            let mut range = base.transition_range(&labels);
            range.sort();

            // assert
            assert_eq!(range.len(), 2);
            assert_eq!(range, vec![base + Label::new(1), base + Label::new(2)]);
        }

        #[test]
        fn make_transition_with_label() {
            // arrange
            let label = Label::new(3);
            let base = Base::new(2);

            // act
            let ret = base + label;

            // assert
            assert_eq!(ret, Transition(5))
        }
    }

    mod empty {
        use crate::types::{Base, Empty, Label};

        #[test]
        fn get_base_if_label_was_valid() {
            // arrange
            let label = Label(3);
            let empty = Empty(5);

            // act
            let base = empty - label;

            // assert
            assert_eq!(base, Some(Base::new(2)));
        }

        #[test]
        fn do_not_return_base_if_base_is_invalid() {
            // arrange
            let label = Label(3);
            let empty = Empty(2);

            // act
            let base = empty - label;

            // assert
            assert_eq!(None, base);
        }
    }

    mod empties {
        use crate::types::{empties, Base, Check, Empty, Label, Node, Transition};

        #[test]
        fn return_empties_in_nodes() {
            // arrange
            let nodes = vec![
                Node {
                    base: Base::root(),
                    check: Check::root(),
                },
                Node {
                    base: Base::root(),
                    check: Check::empty_at(2),
                },
                Node {
                    base: Base::root(),
                    check: Check::empty_at(1),
                },
                Node {
                    base: Base::root(),
                    check: Check::from(Base(3)),
                },
            ];

            // act
            let empties = empties::as_empties(&nodes);

            // assert
            assert_eq!(empties, vec![Empty(1), Empty(2)])
        }

        #[test]
        fn return_empties_as_empty_if_no_empty() {
            // arrange
            let nodes = vec![
                Node {
                    base: Base::root(),
                    check: Check::root(),
                },
                Node {
                    base: Base::root(),
                    check: Check::from(Base(2)),
                },
                Node {
                    base: Base::root(),
                    check: Check::from(Base(2)),
                },
                Node {
                    base: Base::root(),
                    check: Check::from(Base(3)),
                },
            ];

            // act
            let empties = empties::as_empties(&nodes);

            // assert
            assert!(empties.is_empty(), "should be empty")
        }

        #[test]
        fn expandable_empty_node() {
            // arrange
            let mut nodes = vec![Node {
                base: Base::root(),
                check: Check::root(),
            }];

            // act
            empties::expand_empties(&mut nodes, 4);

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
                        check: Check::empty_at(2)
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty_at(3)
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty_at(4)
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty_at(1)
                    },
                ]
            )
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
                    check: Check::empty_at(1),
                },
                Node {
                    base: Base::root(),
                    check: Check::from(Base(1)),
                },
            ];

            // act
            empties::expand_empties(&mut nodes, 4);

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
                        check: Check::empty_at(3)
                    },
                    Node {
                        base: Base::root(),
                        check: Check::from(Base(1))
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty_at(4)
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty_at(5)
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty_at(6)
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty_at(1)
                    },
                ]
            )
        }

        #[test]
        fn push_unused_to_vec() {
            // arrange
            let mut nodes = vec![
                Node {
                    base: Base::root(),
                    check: Check::root(),
                },
                Node {
                    base: Base::new(1),
                    check: Check::empty_at(1),
                },
                Node {
                    base: Base::empty(),
                    check: Check::from(Base(1)),
                },
            ];

            // act
            empties::push_unused(&mut nodes, Base::new(1) + Label::new(1));

            // assert
            assert_eq!(
                nodes,
                vec![
                    Node {
                        base: Base::root(),
                        check: Check::root()
                    },
                    Node {
                        base: Base::new(1),
                        check: Check::empty_at(2)
                    },
                    Node {
                        base: Base::empty(),
                        check: Check::empty_at(1)
                    },
                ]
            )
        }
    }

    mod labels {
        use crate::types::{Label, Labels};

        #[test]
        #[should_panic]
        fn do_not_allow_empty_labels() {
            Labels::from_chars(&vec![]);
        }

        #[test]
        fn get_labeled_keys_from_key() {
            // arrange
            let ls = Labels::from_chars(&vec!['a', 'b', 'c']);

            // act
            let ret = ls.key_to_labels("aabc");

            // assert
            assert_eq!(
                ret,
                Ok(vec![
                    Label::new(1),
                    Label::new(1),
                    Label::new(2),
                    Label::new(3)
                ])
            )
        }

        #[test]
        fn should_return_error_if_key_contains_char_that_is_not_contained_labels() {
            // arrange
            let ls = Labels::from_chars(&vec!['a', 'b', 'c']);

            // act
            let ret = ls.key_to_labels("adabc");

            // assert
            assert_eq!(ret, Err(()))
        }
    }
}
