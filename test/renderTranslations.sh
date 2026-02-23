 #!/usr/bin/bash

shopt -s globstar nullglob

# PREREQUISITES:
#   - generated target files are placed under dist/
#   - Agda generated HTML is placed under html/
BUILD_DIR=dist
AGDA_HTML_DIR=html

echo "Rendering translations..."

# coqdoc --html --no-Index --no-externals --short --body-only
# for f in "$BUILD_DIR"/**/*; do
#   [[ -f $f ]] || continue
# 
#   prefix=${f#"$BUILD_DIR"/}
#   ext=${f##*.}
#   fn=${prefix%."$ext"}
# 
#   mdFn="$f".md
#   lang=$(case $ext in
#     "ast") echo "";;
#     "txt") echo "";;
#     "v") echo "coq";; # alas, pandoc has no coq/rocq syntax-highlighting support
#     *) echo "";;
#   esac)
#   echo "\`\`\`$lang" > $mdFn
#   cat $f >> $mdFn
#   echo "\`\`\`" >> $mdFn
# 
#   targetHtml="$f".html
#   pandoc --quiet -i "$mdFn" -o "$targetHtml" --syntax-highlighting=tango
# done


echo "Copying Agda HTML to $AGDA_HTML_DIR/$BUILD_DIR"
mkdir -p "$AGDA_HTML_DIR/$BUILD_DIR"
cp $BUILD_DIR/*.html "$AGDA_HTML_DIR/$BUILD_DIR"

for f in "$BUILD_DIR"/**/*.txt; do
  [[ -f $f ]] || continue

  prefix=${f#"$BUILD_DIR"/}
  ext=${f##*.}
  fn=${prefix%."$ext"}

  fTxt="$f"
  fAst="$BUILD_DIR"/"$fn".ast
  fCoq="$BUILD_DIR"/"$fn".v
  sourceHtml=$AGDA_HTML_DIR/$(echo $fn | tr '/' '.').html

  [ ! -f $sourceHtml ] && \
    echo " No corresponding HTML for $f (should be at $sourceHtml)" && \
    exit 1

  echo " * $sourceHtml"

  srcAgda=$(<"$sourceHtml")
  srcTxt=$(<"$fTxt")
  srcAst=$(<"$fAst")
  srcCoq=$(<"$fCoq")

  cat > "$sourceHtml" <<EOF
  <!doctype html>
  <html>
    <head>
      <meta charset="utf-8">
      <link rel="stylesheet" href="Agda.css">
      <style>
        body {
          margin: 0;
          box-sizing: border-box;
          height: 100vh;
          display: grid;
          grid-template-columns: 1fr 1fr;
          grid-template-rows: 1fr;
          gap: 2em;
          overflow: hidden;
        }

        main {
          padding: 1em;
          border-right: 1px solid #000;
          height: 100%;
        }
        section {
          height: 100%;
          display: grid;
          grid-template-rows: auto 1fr;
        }

        pre {
          overflow: scroll;
          grid: 1;
        }

        nav {
          padding-top: .5em;
        }

        nav a {
          display: inline-block;
          background: #ddd;
          padding: .5em;
          border-radius: .5em .5em 0 0;
          border: 1px solid #000;
          text-decoration: none;
          border-bottom: none;

        }
        nav:has(+ :target) a { background: #fff }

        section pre {
          overflow: scroll;
        }

        section pre:target ~ :first-child,
        section pre:not(:target) {
          display: none;
        }

        section pre:first-child,
        section pre:target {
          display: inherit;
        }
      </style>
    </head>
    <body>
      <main>
        <pre class="Agda"><code>$srcAgda</code></pre>
      </main>
      <section>
        <nav>
          <a href="#txt">Debug</a>
          <a href="#ast">λ☐</a>
          <a href="#rocq">Rocq</a>
        </nav>
        <div>
          <pre id="txt"><code>$srcTxt</code></pre>
          <pre id="ast"><code>$srcAst</code></pre>
          <pre id="rocq"><code>$srcCoq</code></pre>
        </div>
      </section>
    </body>
  </html>
EOF


#   sed -i "s%class=\"Agda\"%class=\"split left Agda\"%g" $sourceHtml
#   newHtml="\
# <div class=\"split right\">\
# <div class=\"tabs\">\
#     <span data-tab-value=\"#\">Debug</span>\
#     <span data-tab-value=\"#tab_2\">λ\&\#9744;</span>\
#     <span data-tab-value=\"#tab_3\">Rocq</span>\
# </div>\
# <div class=\"tab-content\">\
#     <div class=\"tabs__tab active\" id=\"tab_1\" data-tab-info>\
#         <embed src=\"$fTxt.html\"/>\
#     </div>\
#     <div class=\"tabs__tab\" id=\"tab_2\" data-tab-info>\
#         <embed src=\"$fAst.html\"/>\
#     </div>\
#     <div class=\"tabs__tab\" id=\"tab_3\" data-tab-info>\
#         <embed src=\"$fCoq.html\"/>\
#     </div>\
# </div>\
# </div>\
# <script type="text/javascript">\
#   document.querySelectorAll('[data-tab-value]').forEach(tab => {tab.addEventListener('click', () => {\
#     document.querySelectorAll('[data-tab-info]').forEach(tabInfo => {tabInfo.classList.remove('active') });\
#     document.querySelector(tab.dataset.tabValue).classList.add('active');\
#   })})\
# </script>"
#   sed -i "s%</body>%$newHtml</body>%g" $sourceHtml
done

echo "...done!"
