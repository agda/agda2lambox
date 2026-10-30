(* Evaluate λ-box programs.

CertiRocq's LambdaBoxMut expects constructors as blocks without parameters,
which Peregrine produces from our spines with the following transformations:
- `remove_params`
- `constructors_as_blocks_transformation`

*)

From Stdlib                 Require Import Nat.
From MetaRocq.Utils         Require Import utils.
From MetaRocq.Utils         Require Import ReflectEq.
From MetaRocq.Erasure       Require Import EAst EWellformed EEnvMap EProgram EConstructorsAsBlocks.
From MetaRocq.Erasure       Require ERemoveParams.
From CertiRocq.Common       Require Import Common.
From CertiRocq.LambdaBoxMut Require Import compile term program wcbvEval.
From Agda2Lambox            Require Import CheckWF.

(* TODO: eta-expand here *)

Definition from_reflect {P b} (r : reflectProp P b) : b = true -> P :=
  match r in reflectProp _ b' return b' = true -> P with
  | reflectP p  => fun _ => p
  | reflectF np => fun e => False_rect _ (diff_false_true e)
  end.

Definition on_wf (f : GlobalContextMap.t -> term -> EAst.program) (p : EAst.program) : EAst.program :=
  match inspect (@check_wf_glob eflags p.1) with
  | exist true H => f (GlobalContextMap.make p.1 (wf_glob_fresh _ (from_reflect (check_wf_globP p.1) H))) p.2
  | exist false _ => p
  end.

Definition blocks (p : EAst.program) : EAst.program :=
  on_wf (fun Σ t => (transform_blocks_env Σ, transform_blocks Σ t))
        (on_wf (fun Σ t => ERemoveParams.strip_program (Σ, t)) p).

(* convert a lambda box program to certicoq lambda box mut, and run it *)
Definition eval_program (p : EAst.program) : exception Term :=
  let p := blocks p in
  let prog := {| env  := LambdaBoxMut.compile.compile_ctx (fst p);
      main := compile (snd p)
  |}
  in wcbvEval (env prog) (2 ^ 14) (main prog).

(* Render an evaluated program as an Agda-like value: constructors are named as in
   the environment, and `Agda.Builtin.Nat` is printed in decimal. *)
Section Show.
  Local Open Scope bs_scope.
  Context (Σ : global_declarations).

  Definition agda_nat : kername := (MPfile ["Agda"; "Builtin"; "Nat"], "Nat").

  Fixpoint as_nat (t : Term) : option nat :=
    match t with
    | TConstruct ind 0 tnil =>
        if ind.(inductive_mind) == agda_nat then Some 0 else None
    | TConstruct ind 1 (tcons t tnil) =>
        if ind.(inductive_mind) == agda_nat then option_map S (as_nat t) else None
    | _ => None
    end.

  Definition constructor_name (ind : inductive) (c : nat) : string :=
    match EGlobalEnv.lookup_constructor Σ ind c with
    | Some (_, _, cdecl) => cdecl.(cstr_name)
    | None               => "<unknown constructor>"
    end.

  Definition atomic (t : Term) : bool :=
    match t with
    | TConstruct _ _ (tcons _ _) => isSome (as_nat t)
    | _                          => true
    end.

  Fixpoint show_term (t : Term) : string :=
    match t with
    | TConstruct ind c es =>
        match as_nat t with
        | Some n => string_of_nat n
        | None   => constructor_name ind c ++ show_args es
        end
    | TProof      => "□"
    | TLambda _ _ => "<function>"
    | TFix _ _    => "<fixpoint>"
    | TPrim _     => "<primitive>"
    | TWrong msg  => "<wrong: " ++ msg ++ ">"
    | _           => "<not a value>"
    end
  with show_args (es : Terms) : string :=
    match es with
    | tnil       => ""
    | tcons e es =>
        let s := show_term e in
        " " ++ (if atomic e then s else "(" ++ s ++ ")") ++ show_args es
    end.
End Show.

Definition show_program (p : EAst.program) : string :=
  match eval_program p with
  | Ret t => show_term (fst p) t
  | Exc m => "<error: " ++ m ++ ">"
  end%bs.


(* Courtesy of Eske *)

From CertiRocq Require Import LambdaBoxLocal.toplevel.
From CertiRocq Require Import LambdaANF.toplevel.
From CertiRocq Require Import Compiler.pipeline.
Require Import ExtLib.Structures.Monad.
Import MonadNotation.
From CertiRocq Require Import Common.Common Common.compM Common.Pipeline_utils.

Definition box_to_wasm (p : EAst.program) :=
  let p := blocks p in
  (* For simplicity we assume that the program contains no primitives *)
  let prims := [] in
  let next_id := 100%positive in
  let opts := default_opts in
  (* Translate lambda_box -> lambda_boxmut *)
  let p_mut := {| CertiRocq.Common.AstCommon.main := LambdaBoxMut.compile.compile (snd p) ; CertiRocq.Common.AstCommon.env := LambdaBoxMut.compile.compile_ctx (fst p) |} in
  check_axioms prims p_mut;;
  (* Translate lambda_boxmut -> lambda_boxlocal *)
  p_local <- compile_LambdaBoxLocal prims p_mut;;
  (* Translate lambda_boxlocal -> lambda_anf *)
  p_anf <- compile_LambdaANF_ANF next_id prims p_local;;
  (* Translate lambda_anf -> lambda_anf *)
  p_anf <- compile_LambdaANF next_id p_anf;;
  (* Compile lambda_anf -> WASM *)
  compile_LambdaANF_to_Wasm prims p_anf.

Definition test (p : EAst.program) :=
  run_pipeline _ _ default_opts p box_to_wasm.
