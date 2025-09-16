#!/bin/bash

# Build script for TJQ Language Server

set -e

echo "Building TJQ Language Server..."

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Navigate to the LSP directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# Check if cargo is installed
if ! command -v cargo &> /dev/null; then
    echo "Error: Cargo is not installed. Please install Rust from https://rustup.rs/"
    exit 1
fi

# Build the LSP server in release mode
echo -e "${YELLOW}Building LSP server...${NC}"
cargo build --release

# Check if build was successful
if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Build successful!${NC}"
    echo ""
    echo "LSP binary location:"
    echo "  $SCRIPT_DIR/target/release/tjq-lsp"
    echo ""
    echo "To run the LSP server:"
    echo "  ./target/release/tjq-lsp"
    echo ""
    echo "To run tests:"
    echo "  cargo test"
    echo ""
    echo "To use with VS Code extension:"
    echo "  Configure 'tjq.lspBinaryPath' to point to:"
    echo "  $SCRIPT_DIR/target/release/tjq-lsp"
else
    echo "Build failed!"
    exit 1
fi
