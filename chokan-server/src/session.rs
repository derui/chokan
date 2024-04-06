use std::collections::HashMap;

use kkc::{context::Context, Candidate};
use uuid::Uuid;

/// 各セッションのID
#[derive(Eq, Hash, PartialEq, Clone, Debug)]
pub struct SessionId(String);

impl SessionId {
    pub fn new() -> Self {
        let uuid = Uuid::new_v4();
        SessionId(uuid.to_string())
    }
}

impl From<String> for SessionId {
    fn from(value: String) -> Self {
        Self(value.clone())
    }
}

impl ToString for SessionId {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

/// 変換した候補にidを付与したもの
#[derive(PartialEq, Debug, Clone)]
pub struct RespondedCandidate {
    pub id: String,
    pub body: Candidate,
}

/// 単漢字以外の変換が行われたときに使われるsession
#[derive(PartialEq, Debug, Clone)]
pub struct ConversionSession {
    pub context: Context,
    candidates: Vec<RespondedCandidate>,
}

impl ConversionSession {
    /// 指定したidに対応するcandidateを返す
    ///
    /// # Arguments
    /// * `id` - 対象のid
    ///
    /// # Returns
    /// 対応するcandidate
    pub fn find_candidate(&self, id: String) -> Option<RespondedCandidate> {
        self.candidates.iter().find(|v| v.id == id).cloned()
    }
}

/// セッションを管理するStore
///
/// sessionが発行されるのは、単漢字変換以外である。
#[derive(Debug)]
pub struct SessionStore {
    sessions: HashMap<SessionId, ConversionSession>,
}

impl SessionStore {
    pub fn new() -> Self {
        SessionStore {
            sessions: HashMap::new(),
        }
    }

    /// 指定したIDのセッションを取得を取り出す。
    ///
    /// # Arguments
    /// * `id` - セッションのID
    pub fn pop_session(&mut self, id: &SessionId) -> Option<ConversionSession> {
        self.sessions.remove(id)
    }

    pub fn add_session(
        &mut self,
        id: &SessionId,
        candidates: &[RespondedCandidate],
        context: &Context,
    ) {
        let candidates = candidates.to_owned();

        self.sessions.insert(
            id.clone(),
            ConversionSession {
                candidates,
                context: context.clone(),
            },
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_not_remove_session_if_not_exists() {
        // arrange
        let mut store = SessionStore::new();

        // act
        let ret = store.pop_session(&SessionId::new());

        // assert
        assert_eq!(ret, None);
    }

    #[test]
    fn get_session() {
        // arrange
        let mut store = SessionStore::new();
        let id = SessionId::new();
        store.add_session(&id, &[], &Context::normal());

        // act
        let ret = store.pop_session(&id);

        // assert
        assert_eq!(
            ret,
            Some(ConversionSession {
                candidates: vec![],
                context: Context::normal()
            })
        );
    }
}
