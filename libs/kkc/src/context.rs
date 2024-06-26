use serde::{Deserialize, Serialize};

/// かな漢字変換を実施するときのcontext

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Hash, Clone)]
pub enum ContextKind {
    Normal,      // 通常のかな漢字変換
    ForeignWord, // 外来語
    Numeral,     // 数詞
    Proper,      // 固有名詞優先
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Hash, Clone)]
pub struct Context {
    kind: ContextKind,
}

impl Context {
    /// 通常のコンテキストを生成する.
    pub fn normal() -> Context {
        Context {
            kind: ContextKind::Normal,
        }
    }

    /// 固有名詞優先のコンテキストを生成する.
    pub fn proper() -> Context {
        Context {
            kind: ContextKind::Proper,
        }
    }

    /// 通常のコンテキストを生成する.
    pub fn foreign_word() -> Context {
        Context {
            kind: ContextKind::ForeignWord,
        }
    }

    /// 通常のコンテキストを生成する.
    pub fn numeral() -> Context {
        Context {
            kind: ContextKind::Numeral,
        }
    }

    /// 現在のコンテキストが外来語かどうかを返す
    pub fn is_foreign_word(&self) -> bool {
        self.kind == ContextKind::ForeignWord
    }

    /// 現在のコンテキストが数詞かどうかを返す
    pub fn is_numeral(&self) -> bool {
        self.kind == ContextKind::Numeral
    }

    /// 現在のコンテキストが固有名詞優先かどうかを返す
    pub fn is_proper(&self) -> bool {
        self.kind == ContextKind::Proper
    }
}
