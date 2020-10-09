#!/usr/bin/env bash

set -Eeux

# modified files
git diff --name-status origin/master \
  | grep .hs                         \
  | grep "^M"                        \
  | cut -f 2                         \
  | xargs stylish-haskell --inplace

# added files
git diff --name-status origin/master \
  | grep .hs                         \
  | grep "^A"                        \
  | cut -f 2                         \
  | xargs stylish-haskell --inplace

# renamed files
git diff --name-status origin/master \
  | grep .hs                         \
  | grep "^R"                        \
  | cut -f 3                         \
  | xargs stylish-haskell --inplace

