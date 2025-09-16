# TJQ Language Server

A minimal Language Server Protocol (LSP) implementation for the TJQ language, providing intelligent code assistance features.

## Features

### ✅ Completions
- **Context-aware completions** - Different suggestions based on cursor position
- **Built-in functions** - All TJQ built-in functions with documentation
- **Keywords** - Language keywords with snippets
- **Operators** - Common operators and patterns
- **Smart triggers** - Activates on `.`, `|`, `[`, `{`, `(`, `$`

### 🔍 Hover Information
- **Function documentation** - Detailed info for built-in functions
- **Keyword help** - Syntax and usage for language constructs
- **Operator explanations** - Clear descriptions of TJQ operators
- **Examples** - Code examples in hover tooltips

### 🛠️ Code Assists
- **Extract to variable** - Extract expressions to variables
- **Wrap in try-catch** - Add error handling
- **Convert to pipeline** - Refactor nested calls to pipelines
- **Add type check** - Quick type checking
- **Simplify expression** - Optimize redundant patterns
- **Safe array access** - Add optional chaining to array access

### 📝 Document Formatting
- Automatic spacing around operators
- Consistent code style

## Installation

### Prerequisites
- Rust toolchain (1.70+)
- Tree-sitter TJQ grammar (included in project)

### Building from Source

```bash
# Clone the repository
git clone <repository-url>
cd tjq/tjq_lsp

# Build the LSP server
./build.sh

# Or manually with cargo
cargo build --release
```

The compiled binary will be at: `target/release/tjq-lsp`

## Usage

### Standalone
```bash
# Run the LSP server (communicates via stdin/stdout)
./target/release/tjq-lsp
```

### VS Code Integration
1. Build the LSP server using the build script
2. Configure the extension to use the built binary:
   ```json
   {
     "tjq.lspBinaryPath": "/path/to/tjq_lsp/target/release/tjq-lsp"
   }
   ```

### Other Editors

#### Neovim (with nvim-lspconfig)
```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

configs.tjq_lsp = {
  default_config = {
    cmd = {'/path/to/tjq-lsp'},
    filetypes = {'tjq', 'jq'},
    root_dir = lspconfig.util.root_pattern('.git'),
    settings = {},
  },
}

lspconfig.tjq_lsp.setup{}
```

#### Emacs (with lsp-mode)
```elisp
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "/path/to/tjq-lsp")
                  :major-modes '(jq-mode)
                  :server-id 'tjq-lsp))
```

## Development

### Running Tests
```bash
cargo test
```

### Debug Logging
Set the environment variable for detailed logging:
```bash
RUST_LOG=tjq_lsp=debug,tower_lsp=debug ./target/release/tjq-lsp
```

### Project Structure
```
tjq_lsp/
├── src/
│   ├── main.rs          # Entry point
│   ├── server.rs         # LSP server implementation
│   ├── completions.rs    # Completion provider
│   ├── hover.rs          # Hover provider
│   ├── assists.rs        # Code actions/assists
│   ├── document.rs       # Document management
│   ├── builtins.rs       # TJQ built-in functions/keywords
│   └── tests.rs          # Test suite
├── Cargo.toml            # Dependencies
├── build.sh              # Build script
└── README.md             # This file
```

## Architecture

### Document Management
- Uses `ropey` for efficient rope-based text storage
- Tree-sitter for syntax parsing
- Maintains document state with version tracking

### Completion System
- Context detection based on cursor position
- Different completion sets for different contexts:
  - After `.` - field and method completions
  - After `|` - filter and function completions
  - In expressions - all available completions

### Hover System
- Word detection at cursor position
- Fallback to syntax tree analysis
- Rich markdown documentation

### Code Assists
- Pattern-based transformations
- Safe text manipulation with range tracking
- Non-destructive suggestions

## Contributing

Contributions are welcome! Areas for improvement:

1. **More built-in functions** - Add missing TJQ functions
2. **Better error handling** - Improve error recovery in parsing
3. **Semantic analysis** - Add type inference and checking
4. **More code assists** - Additional refactoring options
5. **Performance** - Optimize for large files
6. **Diagnostics** - Add syntax and semantic error reporting

## License

[Same as parent TJQ project]

## Acknowledgments

- Built with `tower-lsp` for LSP protocol handling
- Uses `tree-sitter` for robust parsing
- Inspired by the original jq language
