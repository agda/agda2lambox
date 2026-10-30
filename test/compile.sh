#!/bin/zsh

# PRE:
#   - generated target files are placed under dist/
BUILD_DIR=dist
#   - Peregrine compilation output is placed under gen/
GEN_DIR=gen
#   - `peregrine` command should be available

echo "Compiling λ-box..."

# remove the output of earlier runs, keeping Cargo's lockfile and build cache
setopt extendedglob
rm -f $GEN_DIR/*/^Cargo.lock(.N)

for f in $BUILD_DIR/*.ast; do
  echo " * $f"
  ext=${f##*.}
  fn=${${f#"$BUILD_DIR"/}%."$ext"}

  for target in wasm c ocaml cakeml rust elm; do
    ext=$(case $target in
      "ocaml") echo "mlf";;
      "cakeml") echo "sml";;
      "rust") echo "rs";;
      *) echo $target;;
    esac)
    mkdir -p $GEN_DIR/$target
    peregrine $target $f -o $GEN_DIR/$target/$fn.$ext
    # decompile WASM binaries to text (WAT), keeping them for `runNative.sh`
    if [[ $target == "wasm" && -f $GEN_DIR/$target/$fn.$ext ]]; then
        wasm2wat --enable-all $GEN_DIR/$target/$fn.$ext > $GEN_DIR/$target/$fn.wat
    fi
  done
done

echo "...done!"
