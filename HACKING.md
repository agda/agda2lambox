The `agda2lambox` backend is a work in progress. Currently it already shows
promising results, but should only be considered a proof of concept until
further work is put into making it easier to use.

This goal of this file is to document the project, in the hopes of making it
easier for other people to pick it up and finish the work. It describes the goal
of the backend, and details the implementation strategy.
Some notes discuss yet unimplemented features, along with some suggestions on
how to add those.

## The `agda2lambox` project

### Coq/Rocq extraction

The Rocq proof assistant is well known for its extraction mechanism, which can
turn any formalized development into a self-standing OCaml program. In the past
10 years or so, several projects have raised the bar and formally verified part
of the pipeline.

In particular:

- As part of the [MetaCoq] project,

  - The pipeline erasure transformation from PCUIC (Rocq's kernel theory) to λ□
    has been mechanized.
    See: [Certified Erasure for Coq, in Coq](https://inria.hal.science/hal-04077552).

  - The translation from λ□ to OCaml (Malfunction, really)
    has been mechanized.
    See: [Verified Extraction from Coq to OCaml](https://dl.acm.org/doi/10.1145/3656379).

- As part of the [CertiCoq] project,

  - A full extraction pipeline from Gallina (Rocq's surface language) to Clight (a subset
      of C) has been mechanized and verified.

  - CertiCoq has recently been extended with a verified pipeline to [WASM].
    See: [certicoq-wasm](https://github.com/womeier/certicoqwasm).

- As part of the [ConCert] project,

  - MetaCoq was extended with additional transformations.
  - A variation of λ□, annotated with type information, has been introduced.
  - There is support for even more targets using this intermediate language,
    such as Rust and Elm, even though correctness of the generated code is to
    our knowledge not verified.

Common to all those contributions is the intermediate language λ□ (Lambda Box), 
first introduced in Pierre Letouzey's PhD thesis on the (at the time new) Rocq
extraction mechanism. λ□ is now defined in Rocq inside the MetaCoq project, and
both the CertiCoq and ConCert projects rely on MetaCoq to use λ□ as an input
language.

### The plan

The plan, therefore, is to develop a new backend for Agda, that targets λ□ (and
its type-annotated variant). The hope is to be able to latch onto the rest of
the verified pipeline from the Rocq ecosystem, and down the line be able to
compile Agda code to new targets such as WASM, Rust, Elm and even machine code.

λ□ is on paper a very suitable target coming from Agda.
It's also much easier to maintain this one backend than to implement one for
each of those desired target languages.
Although there is no plan to verify the backend, we directy benefit from the
fact that the rest of the pipeline is (at least partially) verified.

[MetaCoq]: https://metacoq.github.io/
[CertiCoq]: https://certicoq.org/
[WASM]: https://webassembly.org/

## The `agda2lambox` backend

Now that the general context is out of the way, let's discuss the current
implementation.

### λ□ syntax

In folder `LambdaBox`, we define the syntax of λ□.
It's identical to the formalization that can be found in the MetaCoq project,
so that it's trivial for the backend to generate Rocq files or forward λ□ programs
to the rest of the pipeline.

Of note: Since the typed variant of λ□ was added after the fact by the ConCert
project, it introduces a completely separate notion of global environment in the
MetaCoq formalization. The *only* significant difference with the global
environement for λ□ is the inclusion of type annotations.

For the backend, in order to avoid unnecessary duplication, we have a single
notion of global environment, that *may* contain type information. 
It is defined in `LambdaBox.Env`.
We do a tiny bit of type-trickery to ensure that type information is always
there when we are targeting typed λ□, and never there when we target untyped λ□.
So far this has proven quite useful to ensure by construction that we never do
more work than necessary.

### Backend pipeline

Now let's dive into how the backend runs.
Agda provides multiple hooks for backends to trigger and do some work on
different modules.

In our case, it's quite simple: `agda2lambox` kicks in on the *main* module the
backend is running on.

- We retrieve all definitions in this main module. *All* of those are compiled
  and will be available in the generated λ□ environment. Unless, of course, they
  are logical and erased during compilation.

- When compiling these definitions, they may themselves require definitions
  coming from other modules. The backend transitively compiles all the required
  definitions (and only those), across modules.

  This is what the compilation monad `CompileM` (defined in
  `Agda2Lambox.Compile.Monad`) is for. It maintains a stack of definitions to
  compile, and a way to require new definitions to be compiled (`requireDef`).
  When we compile a function body, for example, we can then call `requireDef` on
  any name that occurs in the body, making sure that those definitions will in
  turn be compiled.

- Once we have compiled all definitions and their transitive dependencies,
  we *topologically* sort them so that they are properly ordered in the
  generated environment.
  The compilation loop and topological sort routine is implemented in
  `Agda2Lambox.Compile.Monad.compileLoop`.

- Once we have the environment, we convert it to either Rocq or an S-expression
  for later consumption by the rest of the pipeline.
