#!/bin/bash

set -e

echo "Building TJQ VS Code Extension..."

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Navigate to extension directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# Check if npm is installed
if ! command -v npm &> /dev/null; then
    echo "Error: npm is not installed. Please install Node.js and npm."
    exit 1
fi

# Install dependencies
echo -e "${YELLOW}Installing dependencies...${NC}"
npm install

# Compile TypeScript
echo -e "${YELLOW}Compiling TypeScript...${NC}"
npm run compile

# Check if compilation was successful
if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Extension built successfully!${NC}"
    echo ""
    echo "To use the extension:"
    echo "1. Open VS Code"
    echo "2. Press F5 to launch a new VS Code window with the extension"
    echo "3. Open a .tjq or .jq file to test"
    echo ""
    echo "Make sure the LSP server is built:"
    echo "  cd ../tjq_lsp && cargo build --release"
else
    echo "Build failed!"
    exit 1
fi
