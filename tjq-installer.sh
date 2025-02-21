#!/bin/sh
set -e

REPO="alpaylan/tjq"
INSTALL_DIR="/usr/local/bin"
BINARY_NAME="tjq"

OS=$(uname -s)
ARCH=$(uname -m)

if [ "$OS" = "Linux" ]; then
    PLATFORM="x86_64-unknown-linux-gnu"
elif [ "$OS" = "Darwin" ]; then
    if [ "$ARCH" = "arm64" ]; then
        PLATFORM="aarch64-apple-darwin"
    else
        PLATFORM="x86_64-apple-darwin"
    fi
else
    echo "Unsupported OS: $OS"
    exit 1
fi

LATEST_RELEASE=$(curl -s https://api.github.com/repos/$REPO/releases/latest | grep "tag_name" | cut -d '"' -f 4)

URL="https://github.com/$REPO/releases/download/$LATEST_RELEASE/${BINARY_NAME}-${PLATFORM}.tar.gz"
echo "Downloading $BINARY_NAME from $URL..."
curl -L $URL | tar xz

chmod +x "$BINARY_NAME"
sudo mv "$BINARY_NAME" "$INSTALL_DIR/$BINARY_NAME"

echo "Installation complete! You can now run '$BINARY_NAME'."
