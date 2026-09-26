(* This file provides utilises to evaluate lambda box programs *)

From Stdlib                Require Import Nat.
From MetaRocq.Utils        Require Import utils.
From MetaRocq.Utils        Require Import ReflectEq.
From MetaRocq.Erasure      Require Import EAst EWellformed EEnvMap EProgram EConstructorsAsBlocks.
From CertiCoq.Common       Require Import Common.
From CertiCoq.LambdaBoxMut Require Import compile term program wcbvEval.
From Agda2Lambox           Require Import CheckWF.


(* TODO: eta-expand here *)

(* CertiRocq's LambdaBoxMut expects constructors as blocks, which Peregrine produces
   from our spines with `constructors_as_blocks_transformation`; we do the same here. *)
Definition from_reflect {P b} (r : reflectProp P b) : b = true -> P :=
  match r in reflectProp _ b' return b' = true -> P with
  | reflectP p  => fun _ => p
  | reflectF np => fun e => False_rect _ (diff_false_true e)
  end.

Definition blocks (p : EAst.program) : EAst.program :=
  match inspect (@check_wf_glob eflags p.1) with
  | exist true H =>
      let Σ := GlobalContextMap.make p.1 (wf_glob_fresh _ (from_reflect (check_wf_globP p.1) H)) in
      (transform_blocks_env Σ, transform_blocks Σ p.2)
  | exist false _ => p
  end.

(* convert a lambda box program to certicoq lambda box mut, and run it *)
Definition eval_program (p : EAst.program) : exception Term :=
  let p := blocks p in
  let prog := {| env  := LambdaBoxMut.compile.compile_ctx (fst p);
      main := compile (snd p)
  |}
  in wcbvEval (env prog) (2 ^ 14) (main prog).


(* Courtesy of Eske *)

From CertiCoq Require Import LambdaBoxLocal.toplevel.
From CertiCoq Require Import LambdaANF.toplevel.
From CertiCoq Require Import Compiler.pipeline.
Require Import ExtLib.Structures.Monad.
Import MonadNotation.
From CertiCoq Require Import Common.Common Common.compM Common.Pipeline_utils.

Definition box_to_wasm (p : EAst.program) :=
  let p := blocks p in
  (* For simplicity we assume that the program contains no primitives *)
  let prims := [] in
  let next_id := 100%positive in
  let opts := default_opts in
  (* Translate lambda_box -> lambda_boxmut *)
  let p_mut := {| CertiCoq.Common.AstCommon.main := LambdaBoxMut.compile.compile (snd p) ; CertiCoq.Common.AstCommon.env := LambdaBoxMut.compile.compile_ctx (fst p) |} in
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
