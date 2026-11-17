#!/bin/zsh

# PRE:
#   - path to Peregrine compilation output
GEN_DIR=gen
SHOW=$PWD/show
CLANG=${CLANG:-$(lean --print-prefix)/bin/clang}
CAKE=https://github.com/CakeML/cakeml/releases/download/v3213/cake-x64-64.tar.gz
# POST:
#   - $GEN_DIR/$target/$fn.result printed output during build/run
#   - $GEN_DIR/$target/$fn.value best-effort pretty-printed returned value
#   - $GEN_DIR/$target/$target.log

RT=$(coqc -where)/user-contrib/CertiRocq/Plugin/runtime
typeset -A SUC=(koka 1 koka-typed 1 lean 1 fsharp 1)

c() {
  gcc -w -O2 -I$RT -I../.. -o _build/$1 $RT/gc_stack.c $1.c $SHOW/show.c \
    && timeout 30 _build/$1
}

llvm() {
  $CLANG -c -O2 -w -o _build/$1.o $1.ll \
    && gcc -w -O2 -I$RT -Dbody=certirocq_main -o _build/$1 $RT/gc_stack.c _build/$1.o $SHOW/show.c \
    && timeout 30 _build/$1
}

ocaml() {
  local d=_build/$1 m=${(U)1[1]}${1[2,-1]}
  mkdir -p $d && cp $1.mlf $d/$m.mlf
  ( cd $d
    # a module's values are positional, so the signature lists every export in order
    grep -o '(export [^)]*)' $m.mlf | tail -1 \
      | perl -pe 's/^\(export |\)$//g; s/\$(\S+) ?/val $1 : Obj.t\n/g' > $m.mli
    echo "let () = Show.show $m.main; print_newline ()" > Main.ml
    ocamlopt -c $m.mli && malfunction cmx $m.mlf && ocamlopt -I .. -c Main.ml \
      && ocamlopt -o main ../Show.cmx $m.cmx Main.cmx
  ) && timeout 30 $d/main
}

wasm() {
  timeout 30 node $SHOW/show.mjs $1.wasm "$(tail -1 ../c/$1.c.result)"
}

cakeml() {
  local cake=_build/cake-x64-64
  perl $SHOW/cakeml.pl $1.sml > _build/$1.cml \
    && $cake/cake --sexp=true --skip_type_inference=true < _build/$1.cml > _build/$1.cake.S \
    && gcc -w -o _build/$1 _build/$1.cake.S $cake/basis_ffi.c -lm \
    && timeout 30 _build/$1 && echo
}

erased() { grep -m1 '^main program' ../../dist/$1.txt | grep -o □ | wc -l }

koka_build() {
  local mods=($(awk -F'\t' '$2 == "OK" { print $1 }' peregrine.log)) force=pg_force box=pg_boxv m e n
  [[ $target == koka-typed ]] && force=pgk_force box='()'
  cp $SHOW/koka.c _build/show.c
  for m in $mods; do
    e=pg_${m}_test
    if grep -q "^val $e : delayed" $m.kk; then e="$force($e)"
    elif grep -q "^fun $e()" $m.kk; then e="$e()"
    elif (( $(erased $m) )); then
      # typed Koka has no parameter for an erased type argument
      n=$(e=$e perl -ne 'if (/^fun \Q$ENV{e}\E\((.*?)\) : pure/) { print scalar(() = $1 =~ /\bv_\w+ :/g); exit }' $m.kk)
      local args=()
      repeat $(( n < $(erased $m) ? n : $(erased $m) )) args+=($box)
      e="$e(${(j:, :)args})"
    fi
    { cat $m.kk; sed "s/ENTRY/$e/" $SHOW/koka.kk } > _build/m${(L)m}.kk
  done
  ( cd _build
    print -l m${(L)^mods} | head -1 | xargs -I{} sh -c 'koka -O2 -o {} {}.kk > {}.log 2>&1'
    print -l m${(L)^mods} | tail -n +2 | xargs -P $(nproc) -I{} sh -c 'koka -O2 -o {} {}.kk > {}.log 2>&1' )
}

koka() {
  local m=_build/m${(L)1}
  [[ -x $m ]] || { grep -m3 -i error $m.log; return 1 }
  timeout 30 $m
}
koka-typed() { koka $1 }

lean_build() {
  cp -r $SHOW/lean/. _build/
  local mods=($(awk -F'\t' '$2 == "OK" { print $1 }' peregrine.log))
  { print 'name = "agda2lambox"\n\n[[lean_lib]]\nname = "Peregrine"'
    print 'roots = ["Peregrine.Runtime", "Peregrine.Show"]'
    for m in $mods; do
      print "\n[[lean_exe]]\nname = \"$m\"\nroot = \"$m\""
      print 'moreLeanArgs = ["-Dlinter.unusedVariables=false"]'
    done } > _build/lakefile.toml
  for m in $mods; do
    local e="Peregrine.reflect Generated.${m}_test"
    grep -q -E "^unsafe def ${m}_test : Thunk" $m.lean && e="Generated.${m}_test.get"
    repeat $(erased $m) e="Peregrine.apply ($e) (Peregrine.reflect ())"
    { sed 's/^import Peregrine.Runtime$/import Peregrine.Show/' $m.lean
      print "\nunsafe def main : IO Unit := IO.println (Peregrine.pp ($e))" } > _build/$m.lean
  done
  ( cd _build && lake build $mods &> lake.log )
}

lean() {
  local exe=_build/.lake/build/bin/$1
  [[ -x $exe ]] || { grep -A3 "error: $1.lean" _build/lake.log; return 1 }
  timeout 30 $exe
}

fsharp_build() {
  export DOTNET_CLI_TELEMETRY_OPTOUT=1 DOTNET_NOLOGO=1
  cp $SHOW/Show.fs _build/
  local mods=($(awk -F'\t' '$2 == "OK" { print $1 }' peregrine.log)) failed rest m e
  for m in $mods; do sed "1s/^module Generated$/module P_$m/" $m.fs > _build/$m.fs; done
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

fsharp() {
  grep -q "\"$1\" ->" _build/Main.fs && [[ -f _build/out/main.dll ]] \
    || { grep -m3 "/$1.fs(" _build/errors.log | sed -E 's|^[^ (]*/||; s| \[[^]]*\]$||'; return 1 }
  timeout 30 dotnet _build/out/main.dll $1
}

for target ext in c c llvm ll ocaml mlf wasm wasm cakeml sml koka kk koka-typed kk lean lean fsharp fs; do
  echo "Running Peregrine ($target) output..."
  pushd -q $GEN_DIR/$target
  mkdir -p _build
  [[ $target == ocaml ]] && cp $SHOW/Show.ml _build \
    && ( cd _build && ocamlopt -c Show.ml )
  [[ $target == cakeml && ! -x _build/cake-x64-64/cake ]] \
    && ( cd _build && curl -sSL $CAKE | tar zx && make -s -C cake-x64-64 cake )
  [[ $target == (koka|koka-typed) ]] && koka_build
  [[ $target == lean ]] && lean_build
  [[ $target == fsharp ]] && fsharp_build

  REPORT=$target.log
  : > $REPORT

  while IFS=$'\t' read -r -u 3 f res; do
    fn=$f.$ext
    if [[ $res != OK ]]; then
      out="ERROR peregrine: ${res#ERROR }"
      echo " * $f: $out"
      printf "%s\t%s\n" $f $out >> $REPORT
      continue
    fi
    $target $f &> $fn.result
    code=$?
    if (( code )); then
      err=$(grep -m1 -i -e error -e exception -e timeout $fn.result)
      out="ERROR ${err:-exit $code}"
    else
      out=$(tail -1 $fn.result | k=${SUC[$target]:-0} perl -pe 's/\bzero\b/0/g; 1 while s/\((?:$ENV{k}|suc) (\d+)\)/$1+1/e')
    fi
    echo " * $f: $out"
    printf "%s\n" $out > $fn.value
    printf "%s\t%s\n" $f $out >> $REPORT
  done 3< peregrine.log

  popd -q
  echo "...done!"
done
