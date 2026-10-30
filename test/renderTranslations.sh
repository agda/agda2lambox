#!/bin/zsh

# PRE:
#   - path to generated λ-box files
BUILD_DIR=dist
#   - path to Peregrine compilation output
GEN_DIR=gen
# POST:
#   - path to Agda generated HTML
AGDA_HTML_DIR=html

echo "Rendering translations..."

# skip what earlier runs rendered, WASM binaries, and build directories
setopt extendedglob
for f in $BUILD_DIR/*~*.(html|md)(.) $GEN_DIR/*/*~*.(html|md|wasm)(.); do

  echo " * $f"
  ext=${f##*.}
  fn=${${f#"$BUILD_DIR"/}%."$ext"}

  mdFn="$f".md
  lang=$(case $ext in
    "ast") echo "";;
    "txt") echo "";;
    "v") echo "coq";; # alas, pandoc has no coq/rocq syntax-highlighting support
    "rs") echo "rust";;
    "sml") echo "ocaml";;
    "mlf") echo "scheme";; # Malfunction
    "result"|"value") echo "";;
    *) echo "$ext";;
  esac)
  : > $mdFn
  # show the value of the entry point above the raw output that contains it
  value=${f%.result}.value
  [[ $ext == "result" && -f $value ]] && \
    printf '```\n%s\n```\n\n---\n\n' "$(<$value)" >> $mdFn
  echo "\`\`\`$lang" >> $mdFn
  sed '$a\' $f >> $mdFn # Peregrine's OCaml and CakeML output lacks a final newline
  echo "\`\`\`" >> $mdFn

  targetHtml="$f".html
  pandoc --quiet -i "$mdFn" -o "$targetHtml" -s --highlight-style=tango
done

mkdir -p "$AGDA_HTML_DIR/$BUILD_DIR" "$AGDA_HTML_DIR/$GEN_DIR"
cp $BUILD_DIR/*.html "$AGDA_HTML_DIR/$BUILD_DIR"
cp $GEN_DIR/**/*.html "$AGDA_HTML_DIR/$GEN_DIR"

for f in $BUILD_DIR/**/**.txt; do

  ext=${f##*.}
  fn=${${f#"$BUILD_DIR"/}%."$ext"}

  fTxt="$f"
  fAst="$BUILD_DIR"/"$fn".ast
  fCoq="$BUILD_DIR"/"$fn".v

  fWasm="$GEN_DIR"/"$fn".wat
  fWasmBin="$GEN_DIR"/"$fn".wasm
  fC="$GEN_DIR"/"$fn".c
  fOcaml="$GEN_DIR"/"$fn".mlf
  fCakeml="$GEN_DIR"/"$fn".sml
  fRust="$GEN_DIR"/"$fn".rs
  fElm="$GEN_DIR"/"$fn".elm

  sourceHtml=$AGDA_HTML_DIR/$(echo $fn | tr '/' '.').html
  [ ! -f $sourceHtml ] && \
    echo " No corresponding HTML for $f (should be at $sourceHtml)" && \
    exit 1
  echo " * $sourceHtml"
  sed -i "s%class=\"Agda\"%class=\"split left Agda\"%g" $sourceHtml
  newHtml="\
<div class=\"split right\">\
<div class=\"tabs\">\
    <span data-tab-value=\"#in_1\">Debug</span>\
    <span data-tab-value=\"#in_2\">λ\&\#9744;</span>\
    <span data-tab-value=\"#in_3\">Rocq</span>\
    <div style=\"font-size:50px;\">\&nbsp; ↪ \&nbsp;</div>\
    <span data-tab-value=\"#out_1\">WASM</span>\
    <span data-tab-value=\"#out_2\">C</span>\
    <span data-tab-value=\"#out_3\">OCaml</span>\
    <span data-tab-value=\"#out_4\">CakeML</span>\
    <span data-tab-value=\"#out_5\">Rust</span>\
    <span data-tab-value=\"#out_6\">Elm</span>\
</div>\
<div class=\"tab-content\">\
    <div class=\"tabs__tab active\" id=\"in_1\" data-tab-info>\
        <embed src=\"$fTxt.html\"/>\
    </div>\
    <div class=\"tabs__tab\" id=\"in_2\" data-tab-info>\
        <embed src=\"$fAst.html\"/>\
    </div>\
    <div class=\"tabs__tab\" id=\"in_3\" data-tab-info>\
        <embed src=\"$fCoq.html\"/>\
        <div style=\"font-size:50px;\">\&nbsp; ↪ \&nbsp;</div>\
        <embed src=\"$fCoq.result.html\"/>\
    </div>\
    <div class=\"tabs__tab\" id=\"out_1\" data-tab-info>\
        <embed src=\"$fWasm.html\"/>\
        <div style=\"font-size:50px;\">\&nbsp; ↪ \&nbsp;</div>\
        <embed src=\"$fWasmBin.result.html\"/>\
    </div>\
    <div class=\"tabs__tab\" id=\"out_2\" data-tab-info>\
        <embed src=\"$fC.html\"/>\
        <div style=\"font-size:50px;\">\&nbsp; ↪ \&nbsp;</div>\
        <embed src=\"$fC.result.html\"/>\
    </div>\
    <div class=\"tabs__tab\" id=\"out_3\" data-tab-info>\
        <embed src=\"$fOcaml.html\"/>\
        <div style=\"font-size:50px;\">\&nbsp; ↪ \&nbsp;</div>\
        <embed src=\"$fOcaml.result.html\"/>\
    </div>\
    <div class=\"tabs__tab\" id=\"out_4\" data-tab-info>\
        <embed src=\"$fCakeml.html\"/>\
    </div>\
    <div class=\"tabs__tab\" id=\"out_5\" data-tab-info>\
        <embed src=\"$fRust.html\"/>\
        <div style=\"font-size:50px;\">\&nbsp; ↪ \&nbsp;</div>\
        <embed src=\"$fRust.result.html\"/>\
    </div>\
    <div class=\"tabs__tab\" id=\"out_6\" data-tab-info>\
        <embed src=\"$fElm.html\"/>\
    </div>\
</div>\
</div>\
<script type="text/javascript">\
  document.querySelectorAll('[data-tab-value]').forEach(tab => {tab.addEventListener('click', () => {\
    document.querySelectorAll('[data-tab-info]').forEach(tabInfo => {tabInfo.classList.remove('active') });\
    document.querySelector(tab.dataset.tabValue).classList.add('active');\
  })})\
</script>"
  sed -i "s%</body>%$newHtml</body>%g" $sourceHtml
done

echo "...done!"
