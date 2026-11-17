
extern import
  c file "show.c"

extern show-value( x : any ) : io () { c inline "(show_value(#1, kk_context()), putchar('\\n'), kk_Unit)" }
extern to-any( x : a ) : any { c inline "#1" }

pub fun main() : io () { show-value(to-any(ENTRY)) }
