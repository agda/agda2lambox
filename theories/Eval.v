(* This file provides utilises to evaluate lambda box programs *)

From Stdlib                Require Import Nat.
From MetaRocq.Utils        Require Import utils.
From MetaRocq.Erasure      Require Import EAst.
From CertiCoq.Common       Require Import Common.
From CertiCoq.LambdaBoxMut Require Import compile term program wcbvEval.


(* TODO: eta-expand here *)

(* convert a lambda box program to certicoq lambda box mut, and run it *)
Definition eval_program (p : EAst.program) : exception Term :=
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
