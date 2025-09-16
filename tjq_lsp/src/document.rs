use ropey::Rope;
use tower_lsp::lsp_types::Position;
use tree_sitter::Tree;

pub struct DocumentData {
    pub rope: Rope,
    pub version: i32,
    pub tree: Option<Tree>,
    pub text: String,
}

impl DocumentData {
    pub fn offset_at_position(&self, position: Position) -> Option<usize> {
        let line = position.line as usize;
        let character = position.character as usize;
        
        if line >= self.rope.len_lines() {
            return None;
        }
        
        let line_start = self.rope.line_to_char(line);
        let line_text = self.rope.line(line);
        
        // Handle UTF-16 to byte offset conversion
        let mut byte_offset = 0;
        let mut utf16_offset = 0;
        
        for ch in line_text.chars() {
            if utf16_offset >= character {
                break;
            }
            byte_offset += ch.len_utf8();
            utf16_offset += ch.len_utf16();
        }
        
        Some(line_start + byte_offset)
    }
    
    pub fn position_at_offset(&self, offset: usize) -> Position {
        let line = self.rope.char_to_line(offset);
        let line_start = self.rope.line_to_char(line);
        let character = offset - line_start;
        
        Position::new(line as u32, character as u32)
    }
}
