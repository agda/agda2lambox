#!/bin/zsh

# PRE:
#   - test modules, each declaring `test : T` and stating its expected value as `_ : test ≡ v`
TEST_DIRS=(agda2lambox agda2rust agda2hs)
#   - path to λ-box evaluation results
BUILD_DIR=dist
TYPED_DIR=$BUILD_DIR/typed
#   - path to Peregrine output
GEN_DIR=gen
# POST:
#   - $GEN_DIR/stats.log holds, for each target, how many modules pass and why the others fail:
#     * peregrine: Peregrine did not compile the module
#     * error:     ill-formed λ-box; Rocq eval failed; target build/run failed
#     * wrong:     returned value `w : Nat != v : Nat`
#     * checked:   returned value `w : Nat == v : Nat`

STATS=$GEN_DIR/stats.log

expected=$(for f in $^TEST_DIRS/*.agda; do
  printf "%s\t%s\t%s\n" ${f:t:r} "$(sed -n 's/^_ *: *test *[[:alnum:].]*≡ *//p' $f)" \
    "$(sed -n 's/^ *test *: *//p' $f | head -1)"
done)

tally() { # target log
  awk -F'\t' -v target=$1 '
    FNR == NR { want[$1] = $2; nat[$1] = $3 ~ /^([[:alnum:]]+\.)*(Nat|ℕ)$/; next }
    { total++ }
    $2 ~ /^ERROR peregrine: / { peregrine++; next }
    $2 ~ /^(ERROR|ILL-FORMED|<error: )/ { error++; next }
    nat[$1] && want[$1] ~ /^[0-9]+$/ && $2 != want[$1] { wrong++; next }
    { pass++; if ($2 == want[$1]) checked++ }
    END {
      printf "%-10s %5d %5d %5d %10d %6d %6d %8d\n", target, total, pass, total - pass,
        peregrine, error, wrong, checked
    }' <(print -r -- $expected) $2
}

{
  printf "%-10s %5s %5s %5s %10s %6s %6s %8s\n" \
    target total pass fail peregrine error wrong checked
  tally rocq $BUILD_DIR/rocq.log
  tally rocq-typed $TYPED_DIR/rocq.log
  for target in rust c llvm ocaml wasm cakeml koka koka-typed lean fsharp; do tally $target $GEN_DIR/$target/$target.log; done
} | tee $STATS
