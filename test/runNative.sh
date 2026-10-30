#!/bin/zsh

# PRE:
#   - path to Peregrine compilation output
GEN_DIR=gen
#   - printers for each target's runtime values
SHOW=$PWD/show
#   - `coqc`, `gcc`, `malfunction`, `ocamlopt` and `node` should be available
# POST:
#   - $GEN_DIR/$target/$fn.result holds what building and running each file printed
#   - $GEN_DIR/$target/$fn.value holds the runtime value of its entry point, with
#     chains of unary blocks `(0 (0 … 0))` printed in decimal, as `Nat`
#   - $GEN_DIR/$target/$target.log holds the value of each entry point, one file per line

RT=$(coqc -where)/user-contrib/CertiRocq/Plugin/runtime

c() {
  gcc -w -O2 -I$RT -I../.. -o _build/$1 $RT/gc_stack.c $1.c $SHOW/show.c \
    && timeout 30 _build/$1
}

ocaml() {
  local d=_build/$1 m=${(U)1[1]}${1[2,-1]}
  local axioms=$(grep -o '(global \$Axioms \$[^)]*)' $1.mlf | sort -u \
    | sed 's/(global \$Axioms \$\(.*\))/let \1 = Obj.repr absorb/')
  mkdir -p $d && cp $1.mlf $d/$m.mlf
  ( cd $d
    echo 'val main : Obj.t' > $m.mli
    echo "let () = Show.show $m.main; print_newline ()" > Main.ml
    if [[ -n $axioms ]]; then
      printf 'let rec absorb _ = Obj.repr absorb\n%s\n' $axioms > Axioms.ml
      ocamlopt -c Axioms.ml || exit
    fi
    ocamlopt -c $m.mli && malfunction cmx $m.mlf && ocamlopt -I .. -c Main.ml \
      && ocamlopt -o main ../Show.cmx ${axioms:+Axioms.cmx} $m.cmx Main.cmx
  ) && timeout 30 $d/main
}

# blocks do not record their size, so follow the shape C printed for the same program
wasm() {
  timeout 30 node $SHOW/show.mjs $1.wasm "$(tail -1 ../c/$1.c.result)"
}

for target ext in c c ocaml mlf wasm wasm; do
  echo "Running Peregrine ($target) output..."
  pushd -q $GEN_DIR/$target
  mkdir -p _build
  [[ $target == ocaml ]] && cp $SHOW/Show.ml _build \
    && ( cd _build && ocamlopt -c Show.ml )

  REPORT=$target.log
  : > $REPORT

  for fn in *.$ext; do
    f=${fn%.*}
    $target $f &> $fn.result
    code=$?
    if (( code )); then
      err=$(grep -m1 -i -e error -e exception -e timeout $fn.result)
      out="ERROR ${err:-exit $code}"
    else
      out=$(tail -1 $fn.result | perl -pe '1 while s/\(0 (\d+)\)/$1+1/e')
    fi
    echo " * $f: $out"
    printf "%s\n" $out > $fn.value
    printf "%s\t%s\n" $f $out >> $REPORT
  done

  popd -q
  echo "...done!"
done
