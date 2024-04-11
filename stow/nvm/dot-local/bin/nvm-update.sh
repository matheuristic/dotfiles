#!/usr/bin/env bash

# update node to latest lts version
# see https://github.com/nvm-sh/nvm/issues/1706

# load NVM
NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" || \
        {
          echo "Bash NVM is not installed on the system."; exit 1
        }

# update node.js
CURR_NODE_VER=$(nvm current)
NEXT_NODE_VER=$(nvm version-remote --lts)
echo "Current node.js version: $CURR_NODE_VER"
echo "Newest node.js version:  $NEXT_NODE_VER"
if [ "$CURR_NODE_VER" != "$NEXT_NODE_VER" ]; then
    PREV_NODE_VER=$CURR_NODE_VER
    nvm install --lts
    nvm reinstall-packages "$PREV_NODE_VER"
    nvm uninstall "$PREV_NODE_VER"
    nvm cache clear
else
    echo "Current node.js is already the latest LTS version."
fi
