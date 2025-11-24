Notes and questions on the ongoing implementation of `agda2lambox`.

- MetaCoq's λ□ programs can be well-formed in different ways 
  --- [parametrized by `EEnvFlags`][envflags].

[envflags]: https://github.com/MetaCoq/metacoq/blob/ea3ed3c4b0d05508ce744f17a56c880c5f47c816/erasure/theories/EWellformed.v#L55

  However, CertiCoq seems to expect a specific kind of λ□ programs, in
  particular as input to the translation to λ□-Mut. I've inferred that:

  - Constructors should be fully-applied (and thus eta-expanded).
  - There should be *no projections* remaining.
  - Datatypes and record types should not be parametrized.

  Is this correct? Where exactly is this requirement stated?

- As `agda2lambox` is *not* going to be verified, it should strive to be
  as straightforward as possible.
  Currently I had to implement eta-expansion of constructors to comply to what
  *I think* CertiCoq expects (See `Agda.Utils (etaExpandCtor)`).
  The code isn't very complex and I'm almost convinced it's correct,
  but to me it falls in the category of transformations that should happen
  on the Coq side.

  Conveniently, MetaCoq already provides a verified *transformation* on λ□ 
  to perform this eta-expansion of constructors.
  See [`EConstructorsAsBlocks`][ctorblocks].

[ctorblocks]: https://github.com/MetaCoq/metacoq/blob/v1.3.1-8.19/erasure/theories/EConstructorsAsBlocks.v

  *However*, this transformation is implemented on `eprogram`,
  which is a slightly more efficient way to encode programs (where the
  environment is not a flat association list anymore).
  *But* CertiCoq's pipeline ony wants regular `program`s.

  - Is there a way to convert a `program` to an `eprogram`?
  - Is there a way to get CertiCoq to work on `eprograms`s?

- How would we go about making the backend incremental?

- Is it reasonnable to pack all the Agda codebase into a single output `.v`
    file?

- Can CertiCoq generate multiple files of Rust? Mutiple blobs of WASM?

- How does CertiCoq handles primitives/axioms? How does it work for types?

- How would we make it easier to do the plumbing from inside Agda?

- How to generate an actual file from CertiCoq-WASM or CertiCoq-Rust?

- How do we typecheck a typed λ□ environment from inside Coq?

- I was hoping that the `funMutuals`, `dataMutuals` and `recMutuals`
  would be enough to detect and prevent indution recursion. 

  But it looks like the function defined over the datatype doesn't appear
  in `dataMutuals` (even if used inside the types of the datatype constructors).
  Likewise, the the datatype doesn't appear in `funMutuals`.
  This means we currently accept code that produces "wrong-looking" λ□.

  Several ways to handle this:

  - Detect when we *require* a definition that has already been compiled.
    This means that it's impossible to reach a valid global environment.
    This can happen when Agda tries to compile the function before the datatype.
    But that's not very nice, because we don't have a lot of information on what went wrong.

  - Figure out how to use `MutualId`.

----

[Verified Extraction from Coq to OCaml][verified-extraction] goes 
in detail about the pipeline to get from Coq programs to OCaml programs.
The erasure pipeline is before reaching the final λ□ target is described in Fig 3.
Here are the successive transforms:

[verified-extraction]: https://dl.acm.org/doi/pdf/10.1145/3656379

*Initially we have a Template Coq Ast encoding of programs*

1. `build_template_program_env`:
    Build an efficient lookup table.
2. `eta_expand`:
    Eta-expand constructors and fixpoints.
3. `template_to_pcuic_transform`:
    Remove casts, application is binary, case annotations inferred.

*At this point, we have a PCUIC program*.

4. `erase_transform`:
    Erasure of proofs terms in Prop, and types.

*After erasure, we get a λ□ program.
All the remaining transformations optimize this program, but remain in λ□*.

5. `guarded_to_unguarded_fix`:
    Simulation of the guarded fixpoint rules with a single unguarded one.

    I don't know what this does, let's keep it.
6. ~~`remove_params_optimization`:~~
    Remove all constructor parameters.

    Treeless (and Agda's internal syntax?) already has dropped constructor parameters.
    We don't need this transformation.
7. `rebuild_wf_env_transform`:
    Rebuild the efficient lookup table.
8. `optimize_prop_discr_optimization`:
    Remove cases/projections on propositions.

    This should already be handled by the treeless translation?
9. `rebuild_wf_env_transform`:
    Rebuild the efficient lookup table.
10. ~~`inline_projections_optimization`:~~
    Inline projections to cases.

    This is already handled in the backend, so this transformation is obsolete.
    In particular, treeless terms don't have projections.
11. `rebuild_wf_env_transform`:
    Rebuild the efficient lookup table.
12. `constructors_as_blocks_transformation`:
    Eta-expand and fully apply constructors.

    This is currently implemented on our side in the backend,
    but it makes way more sense to do it on the Coq side.
    Let's use this transformation!

---

- Actually, now that I get a better understanding of typed λ□ environments, it looks like
  generation of the type information never fails.
  Worst case, it's always `TBox`, but we try to generate as much valid types as we can.

---

- Type erasure is defined in this paper, from ConCert: https://arxiv.org/pdf/2108.02995
  I should just look at what they describe.
  I think I need something like:

  ```hs
  compileTopLevelType :: Type -> C ([TypeVarInfo], LamBox.Type)
  
  compileType :: Type -> C (LamBox.Type)
  ```
