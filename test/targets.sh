#!/bin/zsh
# Usage: test/targets.sh gen|run|html…
#   gen   compiles the λ□ of every test module (`cabal test` puts it in dist/, typed in dist/typed/)
#         with Peregrine to every target in gen/<target>/; peregrine.log holds, per module,
#         OK or the error Peregrine reported
#   run   evaluates λ□ with Rocq, then builds and runs each target's output: X.<ext>.result holds
#         what that printed, X.<ext>.value the value of the entry point (`ERROR …` on failure),
#         <target>.log the values of all modules, and gen/stats.log tallies them
#   html  renders the Agda pages of the test modules in html/, each with a menu of its outputs,
#         and html/index.html, the page of the index module Tests with the statistics
# Needs `peregrine`, `wasm2wat`, `coqc` (with CertiRocq), `gcc`, `malfunction`, `ocamlopt`,
# `node`, `curl`, `cargo`, `koka`, `lean`/`lake`, `dotnet`, `pandoc` and `perl`.
setopt extendedglob
cd ${0:h}
SHOW=$PWD/show THEORIES=${PWD:h}/theories

#        target     directory      extension label
TARGETS=(rocq       dist           v         Rocq
         rocq-typed dist/typed     v         'Rocq (typed)'
         c          gen/c          c         C
         llvm       gen/llvm       ll        LLVM
         ocaml      gen/ocaml      mlf       OCaml
         wasm       gen/wasm       wasm      WASM
         cakeml     gen/cakeml     sml       CakeML
         rust       gen/rust       rs        Rust
         koka       gen/koka       kk        Koka
         koka-typed gen/koka-typed kk        'Koka (typed)'
         lean       gen/lean       lean      Lean
         fsharp     gen/fsharp     fs        'F#')

gen() {
  local t d e l m src res msg mods=(dist/*.ast(N:t:r) dist/typed/*.ast(N:t:r))
  rm -f gen/*/^Cargo.lock(.N)
  for t d e l in $TARGETS; [[ $d == gen/* ]] && mkdir -p $d && : > $d/peregrine.log
  for m in ${(uo)mods}; do
    echo " * $m"
    for t d e l in $TARGETS; do
      [[ $d == gen/* ]] || continue
      src=dist/$m.ast
      [[ $t == (rust|koka-typed) ]] && src=dist/typed/$m.ast
      if [[ ! -f $src ]]; then res="ERROR no $src"
      elif msg=$(peregrine $t $src -o $d/$m.$e 2>&1); then res=OK
      else res="ERROR $(grep -v -e '^Compiling:$' -e '^Could not compile:$' <<< $msg | paste -sd ' ' | tr -s ' ')"
      fi
      [[ $res == OK ]] || echo "   $t: $res"
      print -r -- $m$'\t'$res >> $d/peregrine.log
    done
    [[ -f gen/wasm/$m.wasm ]] && wasm2wat --enable-all gen/wasm/$m.wasm > gen/wasm/$m.wat
  done
}

# Each target X (and X-typed) has `run_X m`, which prints the value of the entry
# point of module m on its last line, run in the target's directory after `build_X`.
# We then pretty-print the output in decimal etc. in a best-effort fashion.
typeset -A SUC=(koka 1 lean 1 fsharp 1)

ok() { awk -F'\t' '$2 == "OK" { print $1 }' peregrine.log }

# the number of □ the entry point of $1 is applied to, which the Koka, Lean and F# output leave out
erased() { grep -m1 '^main program' ../../dist/$1.txt | grep -o □ | wc -l }

run_rocq() { coqc -R $THEORIES Agda2Lambox $1.v }

run_c() { gcc -w -O2 -I$RT -I../.. -o _build/$1 $RT/gc_stack.c $1.c $SHOW/show.c && timeout 30 _build/$1 }

run_llvm() {
  $CLANG -c -O2 -w -o _build/$1.o $1.ll \
    && gcc -w -O2 -I$RT -Dbody=certirocq_main -o _build/$1 $RT/gc_stack.c _build/$1.o $SHOW/show.c \
    && timeout 30 _build/$1
}

build_ocaml() { cp $SHOW/Show.ml _build && ( cd _build && ocamlopt -c Show.ml ) }
run_ocaml() {
  local d=_build/$1 m=${(U)1[1]}${1[2,-1]}
  mkdir -p $d && cp $1.mlf $d/$m.mlf
  ( cd $d
    grep -o '(export [^)]*)' $m.mlf | tail -1 \
      | perl -pe 's/^\(export |\)$//g; s/\$(\S+) ?/val $1 : Obj.t\n/g' > $m.mli
    echo "let () = Show.show $m.main; print_newline ()" > Main.ml
    ocamlopt -c $m.mli && malfunction cmx $m.mlf && ocamlopt -I .. -c Main.ml \
      && ocamlopt -o main ../Show.cmx $m.cmx Main.cmx
  ) && timeout 30 $d/main
}

run_wasm() { timeout 30 node $SHOW/show.mjs $1.wasm "$(tail -1 ../c/$1.c.result)" }

build_cakeml() {
  [[ -x _build/cake-x64-64/cake ]] || ( cd _build \
    && curl -sSL https://github.com/CakeML/cakeml/releases/download/v3213/cake-x64-64.tar.gz | tar zx \
    && make -s -C cake-x64-64 cake )
}
run_cakeml() {
  local cake=_build/cake-x64-64
  perl $SHOW/cakeml.pl $1.sml > _build/$1.cml \
    && $cake/cake --sexp=true --skip_type_inference=true < _build/$1.cml > _build/$1.cake.S \
    && gcc -w -o _build/$1 _build/$1.cake.S $cake/basis_ffi.c -lm \
    && timeout 30 _build/$1 && echo
}

build_rust() {
  local mods=($(ok)) m call
  # derive Debug only for the enums that hold no function, directly or through another enum:
  # Peregrine derives it for every enum (as its `--attributes` would override), which rustc
  # rejects for `dyn Fn`
  perl -CSD -Mutf8 -0pi -e '
    my %body; $body{$1} = $2 while /^pub enum (\w+)[^\n]* \{\n(.*?)^\}/msg;
    my %fn = map { $_ => 1 } grep { $body{$_} =~ /dyn Fn/ } keys %body;
    for (my $more = 1; $more;) { $more = 0;
      for my $e (grep { !$fn{$_} } keys %body) {
        $fn{$e} = $more = 1 if grep { $body{$e} =~ /\b\Q$_\E\b/ } keys %fn } }
    s/^#\[derive\(Debug, Clone\)\]\n(?=pub enum (\w+))/$fn{$1} ? "#[derive(Clone)]\n" : $&/mge' *.rs(N)
  { print '[package]\nname = "main"\n[dependencies]\nbumpalo = "3.20.1"'
    for m in $mods; print "\n[[bin]]\nname = \"$m\"\npath = \"$m.rs\"" } > Cargo.toml
  # the entry point gets `()` for the erased module parameters that λ□'s main program applies it to
  for m in $mods; do
    grep -q '^fn main()' $m.rs && continue
    call=$(m=$m perl -ne 'next unless /^fn \Q$ENV{m}\E_test(?:<([^>]*)>)?\(&\x27a self((?:, \w+: \(\))*)\)/;
      my ($g, $a) = (scalar(() = ($1 // "") =~ /:/g), scalar(() = $2 =~ /:/g));
      print $g ? "::<" . join(", ", ("()") x $g) . ">" : "", "(", join(", ", ("()") x $a), ")"; last' $m.rs)
    print "fn main() {\n  println!(\"{:?}\", Program::new().${m}_test${call:-()});\n}" >> $m.rs
  done
}
run_rust() { cargo run --bin $1 }

build_koka() {
  local mods=($(ok)) force=pg_force box=pg_boxv m e n args
  [[ $t == koka-typed ]] && force=pgk_force box='()'
  cp $SHOW/koka.c _build/show.c
  for m in $mods; do
    e=pg_${m}_test
    if grep -q "^val $e : delayed" $m.kk; then e="$force($e)"
    elif grep -q "^fun $e()" $m.kk; then e="$e()"
    elif (( $(erased $m) )); then
      n=$(e=$e perl -ne 'if (/^fun \Q$ENV{e}\E\((.*?)\) : pure/) { print scalar(() = $1 =~ /\bv_\w+ :/g); exit }' $m.kk)
      args=()
      repeat $(( n < $(erased $m) ? n : $(erased $m) )) args+=($box)
      e="$e(${(j:, :)args})"
    fi
    { cat $m.kk; sed "s/ENTRY/$e/" $SHOW/koka.kk } > _build/m${(L)m}.kk
  done
  ( cd _build
    print -l m${(L)^mods} | head -1 | xargs -I{} sh -c 'koka -O2 -o {} {}.kk > {}.log 2>&1'
    print -l m${(L)^mods} | tail -n +2 | xargs -P $(nproc) -I{} sh -c 'koka -O2 -o {} {}.kk > {}.log 2>&1'
    for m in m${(L)^mods}; [[ -x $m ]] || koka -O2 -o $m $m.kk &> $m.log )
}
run_koka() {
  local m=_build/m${(L)1}
  [[ -x $m ]] || { grep -m3 -i error $m.log; return 1 }
  timeout 30 $m
}

build_lean() {
  local mods=($(ok)) m e
  cp -r $SHOW/lean/. _build/
  { print 'name = "agda2lambox"\n\n[[lean_lib]]\nname = "Peregrine"'
    print 'roots = ["Peregrine.Runtime", "Peregrine.Show"]'
    for m in $mods; do
      print "\n[[lean_exe]]\nname = \"$m\"\nroot = \"$m\""
      print 'moreLeanArgs = ["-Dlinter.unusedVariables=false"]'
    done } > _build/lakefile.toml
  for m in $mods; do
    e="Peregrine.reflect Generated.${m}_test"
    grep -q -E "^unsafe def ${m}_test : Thunk" $m.lean && e="Generated.${m}_test.get"
    repeat $(erased $m) e="Peregrine.apply ($e) (Peregrine.reflect ())"
    { sed 's/^import Peregrine.Runtime$/import Peregrine.Show/' $m.lean
      print "\nunsafe def main : IO Unit := IO.println (Peregrine.pp ($e))" } > _build/$m.lean
  done
  ( cd _build && lake build $mods &> lake.log )
}
run_lean() {
  local exe=_build/.lake/build/bin/$1
  [[ -x $exe ]] || { grep -A3 "error: $1.lean" _build/lake.log; return 1 }
  timeout 30 $exe
}

build_fsharp() {
  export DOTNET_CLI_TELEMETRY_OPTOUT=1 DOTNET_NOLOGO=1
  cp $SHOW/Show.fs _build/
  local mods=($(ok)) failed rest m e
  for m in $mods; sed "1s/^module Generated$/module P_$m/" $m.fs > _build/$m.fs
  : > _build/errors.log
  while (( $#mods )); do
    { print '<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType>'
      print '<TargetFramework>net10.0</TargetFramework><AssemblyName>main</AssemblyName>'
      print '<Tailcalls>true</Tailcalls><InvariantGlobalization>true</InvariantGlobalization>'
      print '<NoWarn>$(NoWarn);FS0020;FS0025;FS0026;FS0040;FS0049;FS0058;FS0064</NoWarn>'
      print '</PropertyGroup><ItemGroup><Compile Include="Show.fs" />'
      for m in $mods; print "<Compile Include=\"$m.fs\" />"
      print '<Compile Include="Main.fs" /></ItemGroup></Project>' } > _build/main.fsproj
    { print 'module Main\n\n[<EntryPoint>]\nlet main argv =\n  let run () =\n    match argv.[0] with'
      for m in $mods; do
        if grep -q -E "^let ${m}_test : Lazy<obj>" $m.fs; then
          e="P_$m.${m}_test.Force()"
          repeat $(erased $m) e="P_$m.PG.apply ($e) P_$m.PG.boxv"
        else
          e="P_$m.${m}_test"
          repeat $(erased $m) e+=" P_$m.PG.boxv"
          e="box ($e)"
        fi
        print "    | \"$m\" -> Show.show ($e)"
      done
      print '    | m -> failwith m\n  let mutable out = ""'
      print '  let t = System.Threading.Thread((fun () -> out <- run ()), 512 * 1024 * 1024)'
      print '  t.Start(); t.Join(); printfn "%s" out; 0' } > _build/Main.fs
    ( cd _build && dotnet build -c Release -nologo -v:q -o out &> build.log ) && break
    grep ': error' _build/build.log >> _build/errors.log
    failed=($(grep -o -E '/[A-Za-z0-9_]+\.fs\([0-9,]+\): error' _build/build.log | sed -E 's|^/(.+)\.fs\(.*|\1|' | sort -u))
    rest=(${mods:|failed})
    (( $#rest < $#mods )) || break
    mods=($rest)
  done
}
run_fsharp() {
  grep -q "\"$1\" ->" _build/Main.fs && [[ -f _build/out/main.dll ]] \
    || { grep -m3 "/$1.fs(" _build/errors.log | sed -E 's|^[^ (]*/||; s| \[[^]]*\]$||'; return 1 }
  timeout 30 dotnet _build/out/main.dll $1
}

run() {
  local RT=$(coqc -where)/user-contrib/CertiRocq/Plugin/runtime CLANG=${CLANG:-$(lean --print-prefix)/bin/clang}
  local t d e l b m res out code
  for t d e l in $TARGETS; do
    echo "Running $l..."
    b=${t%-typed}
    pushd -q $d
    [[ $d == gen/* ]] && mkdir -p _build
    (( $+functions[build_$b] )) && build_$b
    : > $t.log
    while IFS=$'\t' read -r -u 3 m res; do
      if [[ ${res:-OK} != OK ]]; then
        out="ERROR peregrine: ${res#ERROR }"
      else
        run_$b $m &> $m.$e.result
        code=$?
        if (( code )); then
          out="ERROR ${$(grep -m1 -i -e error -e exception -e timeout $m.$e.result):-exit $code}"
        elif [[ $b == rocq ]]; then
          out=$(sed -n 's/^ *= "\(.*\)"%bs$/\1/p' $m.$e.result | paste -sd ';')
          grep -q '^ *= false$' $m.$e.result && out="ILL-FORMED $out"
        else
          out=$(tail -1 $m.$e.result | k=${SUC[$b]:-0} perl -pe 's/PhantomData<[^>]*>, |\(PhantomData<[^>]*>\)//g;
            s/\bzero\b/0/g; 1 while s{\((?:$ENV{k}|suc) (\d+)\)|\bsuc\((\d+)\)}{($1 // $2) + 1}e')
        fi
        print -r -- $out > $m.$e.value
      fi
      echo " * $m: $out"
      print -r -- $m$'\t'$out >> $t.log
    done 3< <(if [[ $b == rocq ]]; then print -l *.v(N:r); else cat peregrine.log; fi)
    popd -q
  done
  stats | tee gen/stats.log
}

# for each target, how many modules pass, and why the others fail:
#   peregrine  Peregrine did not compile the module
#   error      building or running the output failed, λ□ is ill-formed, or Rocq's evaluation failed
#   wrong      the module states `_ : test ≡ v` for a numeral v and `test : Nat`, but the value is not v
# a passing module is `checked` when its value is exactly v
stats() {
  local f t d e l expected=$(for f in agda2*/*.agda; do
    print -r -- ${f:t:r}$'\t'"$(sed -n 's/^_ *: *test *[[:alnum:].]*≡ *//p' $f)"$'\t'"$(sed -n 's/^ *test *: *//p' $f | head -1)"
  done)
  printf "%-10s %5s %5s %5s %10s %6s %6s %8s\n" target total pass fail peregrine error wrong checked
  for t d e l in $TARGETS; do
    awk -F'\t' -v target=$t '
      FNR == NR { want[$1] = $2; nat[$1] = $3 ~ /^([[:alnum:]]+\.)*(Nat|ℕ)$/; next }
      { total++ }
      $2 ~ /^ERROR peregrine: / { peregrine++; next }
      $2 ~ /^(ERROR|ILL-FORMED|<error: )/ { error++; next }
      nat[$1] && want[$1] ~ /^[0-9]+$/ && $2 != want[$1] { wrong++; next }
      { pass++; if ($2 == want[$1]) checked++ }
      END {
        printf "%-10s %5d %5d %5d %10d %6d %6d %8d\n", target, total, pass, total - pass,
          peregrine, error, wrong, checked
      }' <(print -r -- $expected) $d/$t.log
  done
}

html() {
  local f m t d e l g grp src opts panes ARROW='<div style="font-size:50px;">&nbsp; ↪ &nbsp;</div>'
  typeset -A HL=(txt agda ast scheme wat scheme sml scheme mlf scheme result scheme
                 v ocaml rs rust ll llvm fs fsharp kk kotlin lean haskell)
  render() {
    [[ -f $1 ]] || return
    local code
    mkdir -p html/$1:h
    { [[ $1 == *.result && -f ${1%.result}.value ]] && printf '```\n%s\n```\n\n---\n\n' "$(<${1%.result}.value)"
      print '```'${HL[${1:e}]:-${1:e}}
      # Peregrine prints its CakeML and OCaml output on one line, without a final newline
      if [[ $1 == *.(sml|mlf) ]] && code=$(malfunction fmt < $1 2> /dev/null); then
        print -r -- $code
      else
        sed '$a\' $1
      fi
      print '```'
    } | pandoc --quiet -f markdown -o html/$1.html -s --highlight-style=tango \
          -V header-includes='<style>pre > code.sourceCode { white-space: pre-wrap }</style>'
  }
  pane() {
    [[ $1 == $g ]] || { opts+="${g:+</optgroup>}<optgroup label=\"$1\">"; g=$1 }
    opts+="<option value=\"$2\">$3</option>"
    panes+="<div class=\"tabs__tab\" id=\"$2\" data-tab-info><embed data-src=\"$4.html\"/>${5:+$ARROW<embed data-src=\"$5.html\"/>}</div>"
    render $4
    [[ -n $5 ]] && render $5
  }
  split() {
    sed -i 's/class="Agda"/class="split left Agda"/g' $1
    NEW="<div class=\"split right\">$2</div>" perl -0pi -e 's{</body>}{$ENV{NEW}</body>}' $1
  }

  mkdir -p html && cp Agda.css favicon.ico html/
  cabal run -v0 -- agda2lambox --html --css=Agda.css Tests.agda
  for f in dist/*.txt; do
    m=${f:t:r}
    [[ -f html/$m.html ]] || { echo " No HTML for $f (should be at html/$m.html)"; exit 1 }
    echo " * html/$m.html"
    g= opts=() panes=()
    pane agda2lambox debug Debug dist/$m.txt
    pane agda2lambox debug-typed 'Debug (typed)' dist/typed/$m.txt
    pane agda2lambox ast 'λ&#9744;' dist/$m.ast
    pane agda2lambox ast-typed 'λ&#9744; (typed)' dist/typed/$m.ast
    for t d e l in $TARGETS; do
      src=$d/$m.$e
      [[ $e == wasm ]] && src=$d/$m.wat
      [[ $d == gen/* ]] && grp=Peregrine || grp=agda2lambox
      pane $grp $t $l $src $d/$m.$e.result
    done
    # the first option is selected initially
    panes[1]=${${panes[1]/tabs__tab/tabs__tab active}//data-src=/src=}
    split html/$m.html "<select autocomplete=\"off\" onchange=\"\
document.querySelectorAll('[data-tab-info]').forEach(t => t.classList.toggle('active', t.id == this.value)\
 && t.querySelectorAll('embed:not([src])').forEach(e => e.src = e.dataset.src))\
\">${(j::)opts}</optgroup></select><div class=\"tab-content\">${(j::)panes}</div>"
  done
  cp html/Tests.html html/index.html
  [[ -f gen/stats.log ]] && split html/index.html "<pre class=\"code\">$(<gen/stats.log)</pre>"
}

for cmd; $cmd
