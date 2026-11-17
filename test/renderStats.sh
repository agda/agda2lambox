#!/bin/zsh

# PRE:
#   - statistics, as written by stats.sh
GEN_DIR=gen
STATS=$GEN_DIR/stats.log
#   - path to Agda generated HTML, holding the page of index module $1
AGDA_HTML_DIR=html
# POST:
#   - $AGDA_HTML_DIR/index.html is the page of $1, with $STATS alongside it

echo "Rendering statistics..."

index=$AGDA_HTML_DIR/$1.html
if [[ ! -f $STATS ]]; then
  echo " No $STATS, so $AGDA_HTML_DIR/index.html has no statistics"
  cp $index $AGDA_HTML_DIR/index.html
  exit
fi

awk '
  FNR == NR { pane = pane $0 "\n"; next }
  { sub(/class="Agda"/, "class=\"split left Agda\"") }
  i = index($0, "</body>") {
    $0 = substr($0, 1, i - 1) "<div class=\"split right\"><pre class=\"code\">" pane \
         "</pre></div>" substr($0, i)
  }
  1
' $STATS $index > $AGDA_HTML_DIR/index.html

echo "...done!"
