let rec show (v : Obj.t) =
  let t = Obj.tag v in
  if Obj.is_int v then print_int (Obj.obj v)
  else if t = Obj.closure_tag || t = Obj.infix_tag then print_string "<fun>"
  else begin
    print_string "("; print_int t;
    for i = 0 to Obj.size v - 1 do print_char ' '; show (Obj.field v i) done;
    print_string ")"
  end
