From Stdlib           Require Import List Logic.Decidable ssreflect.
From MetaRocq.Common  Require Import BasicAst Kernames Universes EnvMap.
From MetaRocq.Erasure Require Import EAst EWellformed.
From MetaRocq.Utils   Require Import bytestring ReflectEq.
From Equations        Require Import Equations.

Import ListNotations.
Import EnvMap.
Import Kernames.

#[global] Obligation Tactic := idtac.
#[global] Set Equations Transparent.
#[global] Set Equations With UIP.

Definition inspect {A} (a : A) : {b | a = b} := exist _ a eq_refl.
Notation "x 'eqn:' p" := (exist _ x p) (only parsing, at level 20).

(*
#[local] Obligation Tactic :=
          try simpl;
          try intro wf_decls;
          try dependent elimination wf_decls;
          try auto.
*)

Definition eflags : EEnvFlags :=
  {| has_axioms      := true;
     term_switches   :=
       {| has_tBox        := true
        ; has_tRel        := true
        ; has_tVar        := true
        ; has_tEvar       := true
        ; has_tLambda     := true
        ; has_tLetIn      := true
        ; has_tApp        := true
        ; has_tConst      := true
        ; has_tConstruct  := true
        ; has_tCase       := true
        ; has_tProj       := false (* Our backends shouldn't produce projections *)
        ; has_tFix        := true
        ; has_tCoFix      := true
        ; has_tPrim       := all_primitive_flags
        ; has_tLazy_Force := true
        |};
     has_cstr_params := false;  (* Agda already drops constructor params *)
     cstr_as_blocks  := true;   (* The backend fully applies ctors       *)
  |}.

Fixpoint check_fresh_global (k : kername) (decls : global_declarations) : bool :=
  match decls with
  | []    => true
  | p::ds => negb (eq_kername (fst p) k) && check_fresh_global k ds
  end.

Fixpoint check_wf_glob {efl : EEnvFlags} (decls : global_declarations) : bool :=
  match decls with
  | []    => true
  | p::ds => check_wf_glob ds && check_fresh_global (fst p) ds && wf_global_decl ds (snd p)
  end.

Fixpoint check_wf_glob' {efl : EEnvFlags} (decls : global_declarations) : option kername :=
  match decls with
  | []    => None
  | p::ds =>
    match check_wf_glob' ds with
      | Some x => Some x
      | None   => if check_fresh_global (fst p) ds && wf_global_decl ds (snd p) then
                    None
                  else Some (fst p)
    end
  end.

Definition check_wf_program {efl : EEnvFlags} (p : program) : bool :=
  check_wf_glob (fst p) && wellformed (fst p) 0 (snd p).

(* freshness boolean check coincides with the freshness property *)
Fixpoint check_fresh_globalP (k : kername) (decls : global_declarations)
  : reflectProp (fresh_global k decls) (check_fresh_global k decls).
dependent elimination decls; simpl.
Proof.
- apply reflectP.
  apply Forall_nil.
- destruct (inspect (fst p == k)).
  destruct x.
  + rewrite e.
    simpl.
    apply reflectF.
    intro global_ds.
    dependent elimination global_ds.
    apply n.
    apply eqb_eq.
    auto.
  + rewrite e.
    simpl.
    destruct (check_fresh_globalP k l).
    * apply reflectP.
      apply Forall_cons.
      destruct (neqb (fst p) k).
      apply H0.
      rewrite e.
      simpl.
      auto.
      auto.
    * apply reflectF.
      intro gds.
      dependent elimination gds.
      auto.
Defined.

(* well-formedness boolean check coincides with the wf property *)
Fixpoint check_wf_globP {efl : EEnvFlags} (decls : global_declarations)
  : reflectProp (wf_glob decls) (check_wf_glob decls).
Proof.
dependent elimination decls.
- apply reflectP.
  apply wf_glob_nil.
- remember (check_wf_glob l).
  pose x := (check_wf_globP efl l).
  rewrite <-Heqb in x.
  destruct x.
  + simpl.
    rewrite <-Heqb.
    simpl.
    remember (check_fresh_global (fst p) l).
    pose x := (check_fresh_globalP (fst p) l).
    rewrite <-Heqb0 in x.
    destruct x.
    simpl.
    remember (wf_global_decl l (snd p)).
    destruct b.
    apply reflectP.
    destruct p.
    apply wf_glob_cons; auto.
    apply reflectF.
    intro gds.
    dependent elimination gds.
    simpl in Heqb1.
    rewrite <-Heqb1 in i.
    discriminate i.
    simpl.
    apply reflectF.
    intro gds.
    dependent elimination gds.
    auto.
  + simpl.
    rewrite <- Heqb.
    simpl.
    apply reflectF.
    intro gds.
    dependent elimination gds.
    auto.
Defined.

(*

check_fresh_globalP k [] := _;
check_fresh_globalP k ((k', _)::ds) with inspect (eq_kername k' k), inspect (check_fresh_global k ds) := {
 | false eqn: p | true eqn: q := _;
 | _     eqn: p | _    eqn: q := _
}.
- apply ReflectT.
  apply Forall_nil.
- rewrite q.
  rewrite p.
  simpl.
  apply ReflectT.
  apply Forall_cons.
  + .
  

Equations? dec_fresh_global (k : kername) (decls : global_declarations) : decidable (fresh_global k decls) :=
dec_fresh_global k [] := or_introl (Forall_nil _);
dec_fresh_global k ((k', _)::ds) with dec_fresh_global k ds := {
  | or_intror rotten_k_ds := or_intror _;
  | or_introl fresh_k_ds  with eq_dec k' k := {
     | left  k'eqk  := or_intror _;
     | right k'neqk := or_introl _
  };
}.
- apply Forall_cons; auto.
Defined.

Equations? dec_wf_glob {efl : EEnvFlags} (decls : global_declarations) : decidable (wf_glob decls) :=
dec_wf_glob [] := or_introl wf_glob_nil;
dec_wf_glob ((k, d)::ds) with dec_wf_glob ds := {
  | or_introl wf_ds with inspect (wf_global_decl ds d), dec_fresh_global k ds := {
      | true  eqn:     wf_d | or_introl fresh_k  := or_introl _;
      | true  eqn:     wf_d | or_intror rotten_k := or_intror _;
      | false eqn: not_wf_d | _                  := or_intror _;
    };
  | or_intror not_wf_ds := or_intror _
}.
- apply wf_glob_cons; auto.
- simpl in not_wf_d.
  rewrite not_wf_d in i.
  auto.
Defined.

Equations wf_program {efl : EEnvFlags} (p : program) : Prop :=
wf_program (decls , t) := wf_glob decls /\ is_true (wellformed decls 0 t).

Equations? dec_wf_program {efl : EEnvFlags} (p : program) : decidable (wf_program p) :=
dec_wf_program (decls, t) with dec_wf_glob decls := {
  | or_introl wf_glob_decls with inspect (wellformed decls 0 t) := {
      | true  eqn: wf_t := or_introl _;
      | false eqn: wf_t := or_intror _;
    };
  | or_intror _ := or_intror _;
}.
- rewrite wf_t in i.
  discriminate i.
Defined.


*)
