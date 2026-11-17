static void show_value(kk_box_t b, kk_context_t* ctx) {
  if (kk_box_is_value(b)) {
    long r = (long) b.box;
    printf("%ld", (r & 3) == 3 || r == 1 ? r >> 1 : (r >> 2) - 1);
    return;
  }
  kk_block_t* p = kk_ptr_decode(b.box, ctx);
  kk_tag_t t = kk_block_tag(p);
  if (t == KK_TAG_FUNCTION) { printf("<fun>"); return; }
  if (t == KK_TAG_BOX || t == KK_TAG_BOX_ANY) { show_value(kk_block_field(p, 0), ctx); return; }
  printf("(%ld", (long) t - 1);
  for (kk_ssize_t i = 0; i < kk_block_scan_fsize(p); i++) {
    putchar(' ');
    show_value(kk_block_field(p, i), ctx);
  }
  putchar(')');
}
