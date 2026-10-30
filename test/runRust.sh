#!/bin/zsh

# PRE:
#   - path to Peregrine compilation output
GEN_DIR=gen
#   - `cargo` command should be available
# POST:
#   - $GEN_DIR/rust/$f.rs.result holds what cargo printed for each file
#   - $GEN_DIR/rust/$f.rs.value holds the value of its entry point
#   - $GEN_DIR/rust/rust.log holds the value of each entry point, one file per line

echo "Running Peregrine (Rust) output..."

cd $GEN_DIR/rust

REPORT=rust.log
: > $REPORT

# 1. make everything public
sed -i -e 's/^fn/pub fn/g' -e 's/^struct/pub struct/g' *.rs

# 2. Create Cargo package
rustPackage="\
[package]
name = \"main\"
[dependencies]
bumpalo = \"3.20.1\"
"
echo $rustPackage > Cargo.toml

for fn in *.rs; do
  f=${fn%.*}
  rustBin="\
[[bin]]
name = \"$f\"
path = \"$f.rs\"
"
  echo $rustBin >> Cargo.toml

# 3. Attach an individual main function to each test
  rustMain="\
pub fn main() {
  println!(\"{:?}\", Program::new().${f}_test());
}
"
  grep -q '^pub fn main()' $fn || echo $rustMain >> $fn

# 4. Run and record the result of evaluation, dropping phantom fields and
#    printing `Nat` in decimal
  cargo run --bin $f &> $fn.result
  if (( $? )); then
    out="ERROR $(grep -m1 '^error' $fn.result)"
  else
    out=$(tail -1 $fn.result | sed -e 's/PhantomData<[^>]*>, //g' -e 's/(PhantomData<[^>]*>)//g' \
          | perl -pe 's/\bzero\b/0/g; 1 while s/\bsuc\((\d+)\)/$1+1/e')
  fi
  echo " * $f: $out"
  printf "%s\n" "$out" > $fn.value
  printf "%s\t%s\n" $f "$out" >> $REPORT
done

echo "...done!"
