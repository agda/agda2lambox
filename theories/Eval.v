(* Evaluate λ-box programs.

Programs are evaluated call by value once constructors are blocks without parameters,
which MetaRocq's transformations `remove_params` and `constructors_as_blocks` make of
our spines, as Peregrine does.

*)

From Stdlib           Require Import Nat.
From MetaRocq.Utils   Require Import utils.
From MetaRocq.Utils   Require Import ReflectEq.
From MetaRocq.Common  Require Import Kernames.
From MetaRocq.Erasure Require Import EAst ECSubst EGlobalEnv EWellformed EEnvMap EProgram EConstructorsAsBlocks.
From MetaRocq.Erasure Require ERemoveParams.
From Agda2Lambox      Require Import CheckWF.

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

Inductive outcome (A : Type) := Value (a : A) | Error (msg : string).
Arguments Value {A}.
Arguments Error {A}.

Definition bind {A B} (o : outcome A) (k : A -> outcome B) : outcome B :=
  match o with
  | Value a => k a
  | Error m => Error m
  end.

Section Eval.
  Local Open Scope bs_scope.
  Context (Σ : global_declarations).

  Fixpoint eval (fuel : nat) (t : term) {struct fuel} : outcome term :=
    match fuel with
    | 0   => Error "out of fuel"
    | S n =>
      let fix eval_all (ts : list term) : outcome (list term) :=
        match ts with
        | []      => Value []
        | t :: ts => bind (eval n t) (fun v => bind (eval_all ts) (fun vs => Value (v :: vs)))
        end
      in
      match t with
      | tBox | tLambda _ _ | tFix _ _ | tCoFix _ _ | tPrim _ | tLazy _ => Value t
      | tLetIn _ b u => bind (eval n b) (fun v => eval n (csubst v 0 u))
      | tApp f a =>
          bind (eval n f) (fun f => bind (eval n a) (fun a =>
            match f with
            | tBox        => Value tBox
            | tLambda _ b => eval n (csubst a 0 b)
            | tFix mfix i =>
                match cunfold_fix mfix i with
                | Some (_, f) => eval n (tApp f a)
                | None        => Error "ill-formed fixpoint"
                end
            | _ => Error "application of a non-function"
            end))
      | tConst kn =>
          match lookup_constant Σ kn with
          | Some {| cst_body := Some b |} => eval n b
          | Some _ => Error ("axiom " ++ string_of_kername kn)
          | None   => Error ("unknown constant " ++ string_of_kername kn)
          end
      | tConstruct ind c args => bind (eval_all args) (fun vs => Value (tConstruct ind c vs))
      | tCase (ind, npar) d brs =>
          bind (eval n d) (fun d =>
            match d with
            | tConstruct _ c args =>
                match nth_error brs c with
                | Some br => eval n (iota_red npar args br)
                | None    => Error "missing branch"
                end
            | tBox =>
                match brs with
                | [br] => eval n (substl (repeat tBox #|br.1|) br.2)
                | _    => Error "case on □"
                end
            | tCoFix mfix i =>
                match cunfold_cofix mfix i with
                | Some (_, f) => eval n (tCase (ind, npar) f brs)
                | None        => Error "ill-formed cofixpoint"
                end
            | _ => Error "case on a non-constructor"
            end)
      | tForce u =>
          bind (eval n u) (fun v =>
            match v with
            | tLazy w => eval n w
            | _       => Error "force of a non-lazy value"
            end)
      | tProj _ _ => Error "projection"
      | tRel _ | tVar _ | tEvar _ _ => Error "open term"
      end
    end.
End Eval.

Definition eval_program (p : EAst.program) : outcome term :=
  let p := blocks p in eval p.1 (2 ^ 14) p.2.

(* Render an evaluated program as an Agda-like value: constructors are named as in
   the environment, and `Agda.Builtin.Nat` is printed in decimal. *)
Section Show.
  Local Open Scope bs_scope.
  Context (Σ : global_declarations).

  Definition agda_nat : kername := (MPfile ["Nat"; "Builtin"; "Agda"], "Nat").

  Fixpoint as_nat (t : term) : option nat :=
    match t with
    | tConstruct ind 0 [] =>
        if ind.(inductive_mind) == agda_nat then Some 0 else None
    | tConstruct ind 1 [t] =>
        if ind.(inductive_mind) == agda_nat then option_map S (as_nat t) else None
    | _ => None
    end.

  Definition constructor_name (ind : inductive) (c : nat) : string :=
    match lookup_constructor Σ ind c with
    | Some (_, _, cdecl) => cdecl.(cstr_name)
    | None               => "<unknown constructor>"
    end.

  Definition atomic (t : term) : bool :=
    match t with
    | tConstruct _ _ (_ :: _) => isSome (as_nat t)
    | _                       => true
    end.

  Fixpoint show_term (t : term) : string :=
    match t with
    | tConstruct ind c es =>
        match as_nat t with
        | Some n => string_of_nat n
        | None   =>
            let fix show_args es :=
              match es with
              | []      => ""
              | e :: es =>
                  let s := show_term e in
                  " " ++ (if atomic e then s else "(" ++ s ++ ")") ++ show_args es
              end
            in constructor_name ind c ++ show_args es
        end
    | tBox        => "□"
    | tLambda _ _ => "<function>"
    | tFix _ _    => "<fixpoint>"
    | tCoFix _ _  => "<cofixpoint>"
    | tLazy _     => "<lazy>"
    | tPrim _     => "<primitive>"
    | _           => "<not a value>"
    end.
End Show.

Definition show_program (p : EAst.program) : string :=
  match eval_program p with
  | Value t => show_term (fst p) t
  | Error m => "<error: " ++ m ++ ">"
  end%bs.
