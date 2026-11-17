import Peregrine.Runtime

set_option linter.unusedVariables false

namespace Peregrine

@[extern c inline "lean_is_scalar(#1)"] unsafe def isScalar (o : @& Obj) : Bool := false
@[extern c inline "lean_is_ctor(#1)"] unsafe def isCtor (o : @& Obj) : Bool := false
@[extern c inline "lean_unbox(#1)"] unsafe def unbox (o : @& Obj) : USize := 0
@[extern c inline "lean_ptr_tag(#1)"] unsafe def tag (o : @& Obj) : UInt32 := 0
@[extern c inline "lean_ctor_num_objs(#1)"] unsafe def numObjs (o : @& Obj) : UInt32 := 0
@[extern c inline "(lean_inc(lean_ctor_get(#1, #2)), lean_ctor_get(#1, #2))"]
unsafe def field (o : @& Obj) (i : UInt32) : Obj := o

unsafe def pp (o : Obj) : String :=
  if isScalar o then toString (unbox o)
  else if isCtor o then
    (List.range (numObjs o).toNat).foldl (fun s i => s ++ " " ++ pp (field o i.toUInt32))
      ("(" ++ toString (tag o)) ++ ")"
  else "<fun>"

end Peregrine
