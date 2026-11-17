#!/bin/zsh

# PRE:
#   - untyped λ□ is placed under dist/, typed λ□ under dist/typed/
BUILD_DIR=dist
TYPED_DIR=$BUILD_DIR/typed
#   - Peregrine compilation output is placed under gen/
GEN_DIR=gen
#   - `peregrine` command should be available
# POST:
#   - $GEN_DIR/$target/peregrine.log holds, for each file, OK or the error Peregrine reported

echo "Compiling λ-box..."

# remove the output of earlier runs, keeping Cargo's lockfile and build cache
setopt extendedglob
rm -f $GEN_DIR/*/^Cargo.lock(.N)

targets=(wasm c llvm ocaml cakeml rust koka koka-typed lean fsharp)
for target in $targets; do
  mkdir -p $GEN_DIR/$target
  : > $GEN_DIR/$target/peregrine.log
done

fns=($BUILD_DIR/*.ast(N:t:r) $TYPED_DIR/*.ast(N:t:r))
for fn in ${(uo)fns}; do
  echo " * $fn"

  for target in $targets; do
    ext=$(case $target in
      "llvm") echo "ll";;
      "ocaml") echo "mlf";;
      "cakeml") echo "sml";;
      "rust") echo "rs";;
      "koka"|"koka-typed") echo "kk";;
      "fsharp") echo "fs";;
      *) echo $target;;
    esac)
    out=$GEN_DIR/$target/$fn.$ext

    # Rust and typed Koka only get typed λ□, so that Peregrine never infers types
    src=$BUILD_DIR/$fn.ast
    [[ $target == (rust|koka-typed) ]] && src=$TYPED_DIR/$fn.ast
    if [[ ! -f $src ]]; then
      res="ERROR no $src"
    elif msg=$(peregrine $target $src -o $out 2>&1); then
      res=OK
    else
      res="ERROR $(grep -v -e '^Compiling:$' -e '^Could not compile:$' <<< $msg | paste -sd ' ' | tr -s ' ')"
    fi
    [[ $res == OK ]] || echo "   $target: $res"
    printf "%s\t%s\n" $fn $res >> $GEN_DIR/$target/peregrine.log

    # decompile WASM binaries to text (WAT), keeping them for `runNative.sh`
    if [[ $target == "wasm" && -f $out ]]; then
        wasm2wat --enable-all $out > $GEN_DIR/$target/$fn.wat
    fi
  done
done

echo "...done!"
