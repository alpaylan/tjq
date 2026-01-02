#!/bin/bash

set -e

echo "Building and packaging TJQ VS Code Extension as VSIX..."

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

# Navigate to extension directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# Check if npm is installed
if ! command -v npm &> /dev/null; then
    echo -e "${RED}Error: npm is not installed. Please install Node.js and npm.${NC}"
    exit 1
fi

# Install dependencies
echo -e "${YELLOW}Installing dependencies...${NC}"
npm install

# Install vsce if not already installed
if ! npm list @vscode/vsce &>/dev/null; then
    echo -e "${YELLOW}Installing vsce...${NC}"
    npm install --save-dev @vscode/vsce
fi

# Compile TypeScript
echo -e "${YELLOW}Compiling TypeScript...${NC}"
npm run compile

# Create syntaxes directory if it doesn't exist
if [ ! -d "syntaxes" ]; then
    echo -e "${YELLOW}Creating syntaxes directory...${NC}"
    mkdir -p syntaxes
    echo '{"scopeName": "source.tjq", "patterns": []}' > syntaxes/tjq.tmLanguage.json
fi

# Package the extension
echo -e "${YELLOW}Creating VSIX package...${NC}"
npx vsce package

# Check if packaging was successful
if [ $? -eq 0 ]; then
    VSIX_FILE=$(ls -t *.vsix | head -1)
    echo -e "${GREEN}✓ Extension packaged successfully!${NC}"
    echo ""
    echo "VSIX file created: ${VSIX_FILE}"
    echo ""
    echo "To install the extension:"
    echo "  1. Open VS Code"
    echo "  2. Go to Extensions view (Ctrl+Shift+X)"
    echo "  3. Click '...' menu > 'Install from VSIX...'"
    echo "  4. Select: $SCRIPT_DIR/${VSIX_FILE}"
    echo ""
    echo "Or install via command line:"
    echo "  code --install-extension $SCRIPT_DIR/${VSIX_FILE}"
else
    echo -e "${RED}Packaging failed!${NC}"
    exit 1
fi
