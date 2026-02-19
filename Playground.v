From MetaRocq.Template      Require Import Loader Ast.
From MetaRocq.ErasurePlugin Require Import Erasure Loader.
From MetaRocq.Utils         Require Import utils.
From MetaRocq.Erasure.Typed Require Import ResultMonad.
From MetaRocq.Erasure.Typed Require Import Optimize.
From MetaRocq.Erasure.Typed Require Import Extraction.
From Stdlib Require Import ZArith List String Nat.

Import MCMonadNotation.
Import ListNotations.

Check run_erase_program.

Locate run_erase_program.

Program Definition cic_to_box (p : program) :=
  run_erase_program default_erasure_config ([], p) _.
Next Obligation.
  split. easy.
  split.
  now eapply assume_that_we_only_erase_on_welltyped_programs.
  cbv [PCUICWeakeningEnvSN.normalizationInAdjustUniversesIn].
  pose proof @PCUICSN.normalization.
  split; typeclasses eauto.
Qed.

Definition no_check_args :=
  {| check_wf_env_func Σ := Ok (assume_env_wellformed Σ);
     template_transforms := [];
     pcuic_args :=
       {| optimize_prop_discr := true;
          extract_transforms := [dearg_transform (fun _ => None) true true false false false] |} |}.

Definition cic_to_box_typed p :=
  entry <- match p.2 with
           | tConst kn _ => Ok kn
           | tInd ind _ => Ok (inductive_mind ind)
           | _ => Err ("Expected program to be a tConst or tInd")
           end;;
  Σ <- extract_template_env
         no_check_args
         p.1
         (KernameSet.singleton entry)
         (fun k => false);;
  Ok Σ.

Definition not (x : bool) := 
  match x with 
    | true => true
    | false => false
  end.

Fixpoint double (n : nat) :=
  match n with 
    | 0   => 0
    | S n => S (S (double n))
  end.

Fixpoint even (n : nat) :=
  match n with 
    | 0 => true
    | S n => odd n
  end
with odd (n : nat)  :=
  match n with 
    | 0 => false
    | S n => even n
  end.

MetaCoq Quote Recursively Definition ex1 := odd.
Eval vm_compute in cic_to_box_typed ex1.
