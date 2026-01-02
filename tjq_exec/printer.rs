use tree_sitter::Node;

pub fn print_ast(node: Node, source: &str, indent: usize) {
    let indent_str = "  ".repeat(indent);
    let node_text = &source[node.byte_range()];

    let kind = if node.kind() == "identifier" {
        let text = &source[node.range().start_byte..node.range().end_byte];
        if text.is_empty() {
            "hole"
        } else {
            node.kind()
        }
    } else {
        node.kind()
    };
    tracing::trace!(
        "{}{}[{}..{}]: {:?}",
        indent_str,
        kind,
        node.start_byte(),
        node.end_byte(),
        node_text
    );

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        print_ast(child, source, indent + 1);
    }
}

pub fn print_node_details(node: Node, source: &str, indent: usize) {
    let indent_str = "  ".repeat(indent);
    let node_text = &source[node.byte_range()];

    println!("{}Node: {}", indent_str, node.kind());
    println!(
        "{}  Range: {}..{}",
        indent_str,
        node.start_byte(),
        node.end_byte()
    );
    println!(
        "{}  Position: {:?} - {:?}",
        indent_str,
        node.start_position(),
        node.end_position()
    );
    println!("{}  Text: {:?}", indent_str, node_text);
    println!("{}  Named: {}", indent_str, node.is_named());

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        print_node_details(child, source, indent + 1);
    }
}
