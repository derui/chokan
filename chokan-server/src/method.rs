use chokan_dic::ChokanDictionary;
use jsonrpsee::{core::RpcResult, RpcModule};
use kkc::{context::Context, get_candidates, get_tankan_candidates};
use serde::{Deserialize, Serialize};

/**
chokan-serverで提供するmethodの実装を行う。
*/

/**
実行時の文脈を表す
*/
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
enum GetCandidatesContextKind {
    Normal,      // 通常のかな漢字変換
    ForeignWord, // 外来語
    Counter,     // 数詞
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct GetCandidatesContext {
    r#type: GetCandidatesContextKind,
    value: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct GetCandidatesRequest {
    /// かな漢字変換を行う文字列
    input: String,
    /// 実行時の文脈
    context: Option<GetCandidatesContext>,
}

impl From<GetCandidatesContext> for Context {
    fn from(val: GetCandidatesContext) -> Self {
        match val.r#type {
            GetCandidatesContextKind::Normal => Context::normal(),
            GetCandidatesContextKind::ForeignWord => Context::foreign_word(),
            GetCandidatesContextKind::Counter => Context::counter(),
        }
    }
}

impl Default for GetCandidatesContext {
    fn default() -> Self {
        GetCandidatesContext {
            r#type: GetCandidatesContextKind::Normal,
            value: None,
        }
    }
}

/// 候補の一つを表す
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct CandidateResponse {
    /// 候補のID。頻度更新時に利用される
    id: String,
    /// input全体に対応する変換候補
    candidate: String,
}

/// `GetCanddiates`メソッドのレスポンス
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct GetCandidatesResponse {
    /// 実行した変換のsession。このsessionは、頻度更新時に利用される
    session_id: String,
    /// 候補の一覧
    candidates: Vec<CandidateResponse>,
}

/// GetCandidates の実装を登録する
///
/// # Arguments
/// * `module` - 登録するmodule
pub(crate) fn make_get_candidates_method(
    module: &mut RpcModule<ChokanDictionary>,
) -> anyhow::Result<()> {
    module.register_method("GetCandidates", |params, dictionary| {
        let params = params.parse::<GetCandidatesRequest>()?;
        let context = params.context.unwrap_or_default().into();
        let candidates = get_candidates(&params.input, &dictionary.graph, &context, 100);

        let candidates = candidates
            .into_iter()
            .enumerate()
            .map(|(idx, candidate)| CandidateResponse {
                id: idx.to_string(),
                candidate: candidate.to_string(),
            })
            .collect();

        RpcResult::Ok(GetCandidatesResponse {
            session_id: "dummy".to_string(),
            candidates,
        })
    })?;

    Ok(())
}

/// GetTankanCandidates の実装を登録する
///
/// # Arguments
/// * `module` - 登録するmodule
pub(crate) fn make_get_tankan_candidates_method(
    module: &mut RpcModule<ChokanDictionary>,
) -> anyhow::Result<()> {
    module.register_method("GetTankanCandidates", |params, dictionary| {
        let params = params.parse::<GetCandidatesRequest>()?;
        let candidates = get_tankan_candidates(&params.input, &dictionary.tankan);

        let candidates = candidates
            .into_iter()
            .enumerate()
            .map(|(idx, candidate)| CandidateResponse {
                id: idx.to_string(),
                candidate: candidate.to_string(),
            })
            .collect();

        RpcResult::Ok(GetCandidatesResponse {
            session_id: "dummy".to_string(),
            candidates,
        })
    })?;

    Ok(())
}
