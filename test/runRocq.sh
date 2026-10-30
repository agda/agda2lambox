#!/bin/zsh

# PRE:
#   - path to agda2lambox compilation output
#   - `coqc` command should be available
BUILD_DIR=dist
# POST:
#   - $BUILD_DIR/$f.result holds what Rocq printed for each file
#   - $BUILD_DIR/rocq.log holds the value of each entry point, one file per line

echo "Running λ-box output..."

cd $BUILD_DIR

REPORT=rocq.log
: > $REPORT

for f in *.v; do
  coqc -R ../../theories Agda2Lambox $f &> $f.result
  if (( $? )); then
    out="ERROR $(grep -m1 '^Error' $f.result)"
  else
    out=$(sed -n 's/^ *= "\(.*\)"%bs$/\1/p' $f.result | paste -sd ';')
    grep -q '^ *= false$' $f.result && out="ILL-FORMED $out"
  fi
  echo " * ${f%.v}: $out"
  printf "%s\t%s\n" ${f%.v} "$out" >> $REPORT
done

echo "...done!"
