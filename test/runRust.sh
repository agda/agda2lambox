#!/bin/zsh

# PRE:
#   - path to Peregrine compilation output
GEN_DIR=gen
#   - `cargo` command should be available

echo "Running Peregrine (Rust) output..."

cd $GEN_DIR/rust

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
  echo $rustMain >> $f.rs

# 4. Run and record the result of evaluation
  cargo run --bin $f &> $f.rs.result
done

echo "...done!"
