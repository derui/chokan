use std::collections::HashMap;

use kkc::Candidate;
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
        SessionId(value)
    }
}

/// 単漢字以外の変換が行われたときに使われるsession
#[derive(PartialEq, Debug)]
struct ConversionSession {
    candidates: Vec<Candidate>,
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

    pub fn add_session(&mut self, candidates: &Vec<Candidate>) -> SessionId {
        let candidates = candidates.clone();
        let session_id = SessionId::new();

        self.sessions
            .insert(session_id.clone(), ConversionSession { candidates });

        session_id
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
        let id = store.add_session(&vec![]);

        // act
        let ret = store.pop_session(&id);

        // assert
        assert_eq!(ret, Some(ConversionSession { candidates: vec![] }));
    }
}
