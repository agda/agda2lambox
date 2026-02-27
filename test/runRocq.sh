#!/bin/zsh

# PRE:
#   - path to agda2lambox compilation output
#   - `coqc` command should be available
BUILD_DIR=dist

echo "Running λ-box output..."

cd $BUILD_DIR

for f in *.v; do
  coqc -R ../../theories Agda2Lambox $f &> $f.result
done

echo "...done!"
