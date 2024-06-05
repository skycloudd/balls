#!/bin/sh

set -e

for file in examples/*.bl; do
    cargo run -- $file
done
