#!/bin/bash
binary_name="TTS-Mod-Backup"
destination_dir="/usr/local/bin"


# Exit immediately if a command exits with a non-zero status
set -e

# Remove old builds
echo "Removing old builds..."
cabal clean

# Build the project
echo "Running cabal build..."
cabal build

# Locate the binary
BINARY=$(find dist-newstyle/build/ -type f -name "$binary_name")
if [ -z "$BINARY" ]; then
  echo "Error: Could not find the binary after building."
  exit 1
fi
echo "Binary found: $BINARY"


if [ -f "$destination_dir/$binary_name" ]; then
  echo "Removing old binary..."
  sudo rm $destination_dir/$binary_name
fi

# Move the binary to destination (requires sudo for root privileges)
echo "Moving binary to $destination_dir..."
sudo cp "$BINARY" /usr/local/bin

echo "Binary moved successfully to $destination_dir."