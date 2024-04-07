use std::sync::{mpsc::Sender, Arc, Mutex};

use dic::base::entry::Entry;
use jsonrpsee::{core::RpcResult, RpcModule};
use kkc::{context::Context, get_candidates, get_tankan_candidates, Candidate};
use serde::{Deserialize, Serialize};

use crate::{
    method_context::MethodContext,
    session::{RespondedCandidate, SessionId, SessionStore},
};

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
    Numeral,     // 数詞
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
            GetCandidatesContextKind::Numeral => Context::numeral(),
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
/// * `tx` - sessionを送信するためのchannel
pub(crate) fn make_get_candidates_method(
    module: &mut RpcModule<MethodContext>,
    tx: Sender<(SessionId, Vec<RespondedCandidate>, Context)>,
) -> anyhow::Result<()> {
    module.register_method("GetCandidates", move |params, ctx| {
        let params = params.parse::<GetCandidatesRequest>()?;
        let context = params.context.unwrap_or_default().into();
        let candidates: Vec<Candidate>;
        {
            let dict = ctx.dictionary.lock().unwrap();
            let user_pref = ctx.user_pref.lock().unwrap();
            candidates = get_candidates(
                &params.input,
                &dict.graph,
                &context,
                user_pref.frequency(),
                100,
            );
        }

        let candidates = candidates
            .into_iter()
            .enumerate()
            .map(|(idx, candidate)| RespondedCandidate {
                id: idx.to_string(),
                body: candidate.clone(),
            })
            .collect::<Vec<_>>();
        let session_id = SessionId::new();

        // sessionを送信して記録しておく
        tx.send((session_id.clone(), candidates.clone(), context.clone()))
            .unwrap();

        RpcResult::Ok(GetCandidatesResponse {
            session_id: session_id.to_string(),
            candidates: candidates
                .iter()
                .map(|candidate| CandidateResponse {
                    id: candidate.id.to_string(),
                    candidate: candidate.body.to_string(),
                })
                .collect(),
        })
    })?;

    Ok(())
}

/// GetTankanCandidates の実装を登録する
///
/// # Arguments
/// * `module` - 登録するmodule
pub(crate) fn make_get_tankan_candidates_method(
    module: &mut RpcModule<MethodContext>,
) -> anyhow::Result<()> {
    module.register_method("GetTankanCandidates", |params, ctx| {
        let params = params.parse::<GetCandidatesRequest>()?;
        let dict = ctx.dictionary.lock().unwrap();
        let candidates = get_tankan_candidates(&params.input, &dict.tankan);

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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct UpdateFrequencyRequest {
    session_id: String,
    candidate_id: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct UpdateFrequencyResponse {}

/// UpdateFrequency の実装を登録する
///
/// 内部でMutexを利用しているため、ここ以外でlockを取得している場所と同時に呼び出すとデッドロックが発生する可能性がある
///
/// # Arguments
/// * `module` - 登録するmodule
/// * `store` - sessionを管理するstore
/// * `notifier` - 変換が完了したことを通知する
pub(crate) fn make_update_frequency_method(
    module: &mut RpcModule<MethodContext>,
    store: Arc<Mutex<SessionStore>>,
    notifier: Sender<()>,
    entry_updater: Sender<Entry>,
) -> anyhow::Result<()> {
    module.register_method("UpdateFrequency", move |params, ctx| {
        let params = params.parse::<UpdateFrequencyRequest>()?;
        {
            let session_id = params.session_id;
            let mut store = store.lock().unwrap();

            let session = store.pop_session(&SessionId::from(session_id));
            let candidate = session.clone().and_then(|session| {
                session
                    .find_candidate(params.candidate_id)
                    .map(|v| (v, session.context))
            });

            if let Some((c, context)) = candidate {
                let mut user_pref = ctx.user_pref.lock().unwrap();

                if let Some(word) = c.body.to_string_only_independent() {
                    user_pref.update_frequency(&word, &context);
                }
                let _ = user_pref
                    .update_compound_words(&c.body)
                    .inspect(|v| entry_updater.send(v.clone()).unwrap());
            }
        }
        // 成否に関わらずに送っておく
        notifier.send(()).unwrap();

        RpcResult::Ok(UpdateFrequencyResponse {})
    })?;

    Ok(())
}
