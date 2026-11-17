module Show

open Microsoft.FSharp.Reflection

let rec show (o: obj) : string =
  if isNull o then "<fun>"
  else
    let t = o.GetType()
    if FSharpType.IsUnion(t, true) then
      let case, fs = FSharpValue.GetUnionFields(o, t, true)
      if fs.Length = 0 then string case.Tag
      else "(" + string case.Tag + String.concat "" [ for f in fs -> " " + show f ] + ")"
    else "<fun>"
