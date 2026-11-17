#!/bin/zsh

# PRE:
#   - path to Peregrine compilation output
GEN_DIR=gen
# POST:
#   - $GEN_DIR/rust/$f.rs.result printed `cargo` output
#   - $GEN_DIR/rust/$f.rs.value returned value of its entry point
#   - $GEN_DIR/rust/rust.log

echo "Running Peregrine (Rust) output..."

cd $GEN_DIR/rust

REPORT=rust.log
: > $REPORT

# 1. make everything public
sed -i -e 's/^fn/pub fn/g' -e 's/^struct/pub struct/g' *.rs(N)
perl -CSD -Mutf8 -0pi -e '
  my %body; $body{$1} = $2 while /^pub enum (\w+)[^\n]* \{\n(.*?)^\}/msg;
  my %fn = map { $_ => 1 } grep { $body{$_} =~ /dyn Fn/ } keys %body;
  for (my $more = 1; $more;) { $more = 0;
    for my $e (grep { !$fn{$_} } keys %body) {
      $fn{$e} = $more = 1 if grep { $body{$e} =~ /\b\Q$_\E\b/ } keys %fn } }
  s/^#\[derive\(Debug, Clone\)\]\n(?=pub enum (\w+))/$fn{$1} ? "#[derive(Clone)]\n" : $&/mge' *.rs(N)
  # ^ remove [deriveDebug] on functions

# 2. Create Cargo package
rustPackage="\
[package]
name = \"main\"
[dependencies]
bumpalo = \"3.20.1\"
"
echo $rustPackage > Cargo.toml

while IFS=$'\t' read -r -u 3 f res; do
  fn=$f.rs
  if [[ $res != OK ]]; then
    out="ERROR peregrine: ${res#ERROR }"
    echo " * $f: $out"
    printf "%s\t%s\n" $f "$out" >> $REPORT
    continue
  fi
  rustBin="\
[[bin]]
name = \"$f\"
path = \"$f.rs\"
"
  echo $rustBin >> Cargo.toml

# 3. Attach an individual main function to each test
  call=$(perl -ne 'next unless /^pub fn \Q'$f'\E_test(?:<([^>]*)>)?\(&\x27a self((?:, \w+: \(\))*)\)/;
    my ($gs, $as) = ($1 // "", $2);
    my ($g, $a) = (scalar(() = $gs =~ /:/g), scalar(() = $as =~ /:/g));
    print $g ? "::<" . join(", ", ("()") x $g) . ">" : "", "(", join(", ", ("()") x $a), ")"; last' $fn)
  rustMain="\
pub fn main() {
  println!(\"{:?}\", Program::new().${f}_test${call:-()});
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
done 3< peregrine.log

echo "...done!"
