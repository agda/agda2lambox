#include <stdio.h>
#include "gc_stack.h"

extern value body(struct thread_info *);
extern char __executable_start, etext;

/* a closure pairs a code pointer with its environment */
static int closure(value v) {
  if (Wosize_hd(Hd_val(v)) != 2) return 0;
  char *f = (char *) ((value *) v)[0];
  return f >= &__executable_start && f < &etext;
}

static void show(value v) {
  if (Is_long(v)) { printf("%ld", (long) Long_val(v)); return; }
  if (closure(v)) { printf("<fun>"); return; }
  printf("(%u", Tag_hd(Hd_val(v)));
  for (mlsize_t i = 0; i < Wosize_hd(Hd_val(v)); i++) {
    putchar(' ');
    show(((value *) v)[i]);
  }
  putchar(')');
}

int main(void) {
  show(body(make_tinfo()));
  putchar('\n');
  return 0;
}
