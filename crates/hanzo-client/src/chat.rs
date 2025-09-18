#[derive(Clone)]
pub struct ChatRequest {
    pub model: String,
}

impl ChatRequest {
    pub fn new(message: String) -> Self {
        Self { model: "gpt-5".to_string() }
    }

    pub fn to_wire(&self, _model: &crate::Model) -> crate::WireRequest {
        crate::WireRequest {
            model: self.model.clone(),
            messages: vec![],
        }
    }
}

#[derive(Clone)]
pub struct ChatResponse {
    pub content: String,
}

impl ChatResponse {
    pub fn text(&self) -> String {
        self.content.clone()
    }

    pub fn from_wire(_response: crate::WireResponse) -> Self {
        Self { content: String::new() }
    }
}
