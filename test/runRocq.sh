#!/bin/zsh

# PRE:
#   - path to agda2lambox compilation output: untyped λ□ under dist/, typed λ□ under dist/typed/
BUILD_DIR=dist
TYPED_DIR=$BUILD_DIR/typed
#   - `coqc` command should be available
THEORIES=$PWD/../theories
# POST:
#   - $dir/$f.result holds what Rocq printed for each file
#   - $dir/rocq.log holds the value of each entry point, one file per line

for dir in $BUILD_DIR $TYPED_DIR; do
  echo "Running λ-box output in $dir..."
  pushd -q $dir

  REPORT=rocq.log
  : > $REPORT

  for f in *.v(N); do
    coqc -R $THEORIES Agda2Lambox $f &> $f.result
    if (( $? )); then
      out="ERROR $(grep -m1 '^Error' $f.result)"
    else
      out=$(sed -n 's/^ *= "\(.*\)"%bs$/\1/p' $f.result | paste -sd ';')
      grep -q '^ *= false$' $f.result && out="ILL-FORMED $out"
    fi
    echo " * ${f%.v}: $out"
    printf "%s\t%s\n" ${f%.v} "$out" >> $REPORT
  done

  popd -q
  echo "...done!"
done
