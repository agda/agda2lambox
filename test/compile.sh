#!/bin/zsh

# PREREQUISITES:
#   - generated target files are placed under dist/
BUILD_DIR=dist
#   - Peregrine compilation output is placed under gen/
GEN_DIR=gen
#   - `peregrine` binary should be available

echo "Compiling λ-box..."

for f in $BUILD_DIR/*.ast; do
  echo " * $f"
  ext=${f##*.}
  fn=${${f#"$BUILD_DIR"/}%."$ext"}

  for target in wasm c ocaml cakeml rust elm; do
    ext=$(case $target in
      "ocaml") echo "ml";;
      "cakeml") echo "sml";;
      "rust") echo "rs";;
      *) echo $target;;
    esac)
    mkdir -p $GEN_DIR/$target
    peregrine $target $f -o $GEN_DIR/$target/$fn.$ext
    if [[ $target == "wasm" ]]; then # decompile WASM binaries to text (WAT)
        wasm2wat $GEN_DIR/$target/$fn.$ext > $GEN_DIR/$target/$fn.wat
        rm $GEN_DIR/$target/$fn.$ext
    fi
  done
done

echo "...done!"
