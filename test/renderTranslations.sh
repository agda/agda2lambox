#!/bin/zsh

# PRE:
#   - path to generated λ-box files, untyped under dist/ and typed under dist/typed/
BUILD_DIR=dist
TYPED_DIR=$BUILD_DIR/typed
#   - path to Peregrine compilation output
GEN_DIR=gen
# POST:
#   - path to Agda generated HTML
AGDA_HTML_DIR=html

echo "Rendering translations..."

# skip what earlier runs rendered, Rocq and WASM binaries, and build directories
setopt extendedglob
for f in $BUILD_DIR/*~*.(html|md|vo|vok|vos|glob)(.) $TYPED_DIR/*~*.(html|md|vo|vok|vos|glob)(.) \
         $GEN_DIR/*/*~*.(html|md|wasm)(.); do

  echo " * $f"
  ext=${f##*.}
  fn=${${f#"$BUILD_DIR"/}%."$ext"}

  mdFn="$f".md
  lang=$(case $ext in
    "txt") echo "agda";;
    "ast"|"wat"|"sml"|"mlf") echo "scheme";; # S-expressions
    "result"|"value") echo "scheme";; # mostly constructor trees
    "v") echo "ocaml";;
    "rs") echo "rust";;
    "ll") echo "llvm";;
    "fs") echo "fsharp";;
    "kk") echo "kotlin";;
    "lean") echo "haskell";;
    *) echo "$ext";;
  esac)
  : > $mdFn
  # show the value of the entry point above the raw output that contains it
  value=${f%.result}.value
  [[ $ext == "result" && -f $value ]] && \
    printf '```\n%s\n```\n\n---\n\n' "$(<$value)" >> $mdFn
  echo "\`\`\`$lang" >> $mdFn
  if [[ $ext == (sml|mlf) ]] && code=$(malfunction fmt < $f 2> /dev/null); then
    print -r -- $code >> $mdFn
  else
    sed '$a\' $f >> $mdFn
  fi
  echo "\`\`\`" >> $mdFn

  targetHtml="$f".html
  pandoc --quiet -i "$mdFn" -o "$targetHtml" -s --highlight-style=tango \
    -V header-includes='<style>pre > code.sourceCode { white-space: pre-wrap }</style>'
done

mkdir -p "$AGDA_HTML_DIR/$BUILD_DIR" "$AGDA_HTML_DIR/$TYPED_DIR"
cp $BUILD_DIR/*.html "$AGDA_HTML_DIR/$BUILD_DIR"
cp $TYPED_DIR/*.html "$AGDA_HTML_DIR/$TYPED_DIR"
for d in $GEN_DIR/*(/N); do
  mkdir -p "$AGDA_HTML_DIR/$d" && cp $d/*.html "$AGDA_HTML_DIR/$d"
done

ARROW='<div style="font-size:50px;">&nbsp; ↪ &nbsp;</div>'
# an option showing pane $1, labelled $2
opt() {
  print -rn "<option value=\"$1\">$2</option>"
}
# a pane with the rendering of file $2, then that of file $3 (its value) if given
pane() {
  print -rn "<div class=\"tabs__tab\" id=\"$1\" data-tab-info><embed src=\"$2.html\"/>${3:+$ARROW<embed src=\"$3.html\"/>}</div>"
}

for f in $BUILD_DIR/*.txt; do
  fn=${${f#$BUILD_DIR/}%.txt}
  sourceHtml=$AGDA_HTML_DIR/$(echo $fn | tr '/' '.').html
  [ ! -f $sourceHtml ] && \
    echo " No corresponding HTML for $f (should be at $sourceHtml)" && \
    exit 1
  echo " * $sourceHtml"

  u=$BUILD_DIR/$fn t=$TYPED_DIR/$fn g=$GEN_DIR
  opts=(
    '<optgroup label="agda2lambox">'
    "$(opt debug Debug)" "$(opt debugT 'Debug (typed)')" "$(opt ast 'λ&#9744;')" "$(opt astT 'λ&#9744; (typed)')"
    "$(opt rocq Rocq)" "$(opt rocqT 'Rocq (typed)')"
    '</optgroup><optgroup label="Peregrine">'
    "$(opt wasm WASM)" "$(opt c C)" "$(opt llvm LLVM)" "$(opt ocaml OCaml)" "$(opt cakeml CakeML)"
    "$(opt rust Rust)" "$(opt koka Koka)" "$(opt kokaT 'Koka (typed)')" "$(opt lean Lean)" "$(opt fsharp 'F#')"
    '</optgroup>'
  )
  panes=(
    "$(pane debug $u.txt)" "$(pane debugT $t.txt)"
    "$(pane ast $u.ast)" "$(pane astT $t.ast)"
    "$(pane rocq $u.v $u.v.result)" "$(pane rocqT $t.v $t.v.result)"
    "$(pane wasm $g/wasm/$fn.wat $g/wasm/$fn.wasm.result)"
    "$(pane c $g/c/$fn.c $g/c/$fn.c.result)"
    "$(pane llvm $g/llvm/$fn.ll $g/llvm/$fn.ll.result)"
    "$(pane ocaml $g/ocaml/$fn.mlf $g/ocaml/$fn.mlf.result)"
    "$(pane cakeml $g/cakeml/$fn.sml $g/cakeml/$fn.sml.result)"
    "$(pane rust $g/rust/$fn.rs $g/rust/$fn.rs.result)"
    "$(pane koka $g/koka/$fn.kk $g/koka/$fn.kk.result)"
    "$(pane kokaT $g/koka-typed/$fn.kk $g/koka-typed/$fn.kk.result)"
    "$(pane lean $g/lean/$fn.lean $g/lean/$fn.lean.result)"
    "$(pane fsharp $g/fsharp/$fn.fs $g/fsharp/$fn.fs.result)"
  )
  # the first option is selected initially
  panes[1]=${panes[1]/tabs__tab/tabs__tab active}
  sed -i "s%class=\"Agda\"%class=\"split left Agda\"%g" $sourceHtml
  NEW="<div class=\"split right\"><select autocomplete=\"off\" onchange=\"\
document.querySelectorAll('[data-tab-info]').forEach(t => t.classList.toggle('active', t.id == this.value))\
\">${(j::)opts}</select><div class=\"tab-content\">${(j::)panes}</div></div>" perl -0pi -e 's{</body>}{$ENV{NEW}</body>}' $sourceHtml
done

echo "...done!"
