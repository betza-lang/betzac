# Separate compilation: the `.bi` interface artifact

Hand-off document. The goal is that compiling a file depends on its dependencies'
*artifacts*, never on their source or their ASTs — so an unchanged dependency is read,
not recompiled, and files can eventually be compiled independently of one another.

Design rationale is in the conversation. This is the how, plus what reading the code
turned up.

---

## 0. The one decision that shapes everything: interface ≠ IR

Two artifacts, two jobs, two lifetimes:

- **The interface** (`.bi`) — the minimum a *dependent* file needs in order to compile.
- **The IR** — the compiled meaning of a piece, for whatever consumes compiled pieces.

Trying to specify one object that does both is what has made the IR feel unspecifiable:
every imaginable consumer adds a requirement, and none of them are dependents. The
interface can be specified today, because its requirements are finite and already
observable in the code. The IR can stay undesigned.

Concretely: `.bi` is the interface. It is *not* the compiler's output for a piece, and
naming it that later will hurt.

---

## 1. What imported definitions are actually consulted for today

The proposal was to serialise `LabelTable ExportedDef`. That is heavier than it needs to
be — `ExportedDef` holds a whole `QualifiedStmt Ps`, which drags the entire AST and
megaparsec's `SourcePos` through whatever serialisation format gets chosen.

I traced every consumer of a def that came from another file. Grouped by whether the
def's *body* is reached:

| Consumer | Imported def? | What it uses |
| --- | --- | --- |
| `Scope.effectiveScope` → `importedCandidates` | yes | `edLabel`, `edOrder`. The diagnostic span comes from `getSpan via`, the `using` — not the def. |
| `Scope.effectiveScope` → `preludeCandidates` | yes | `edLabel`, `edOrder`, `getSpan d` — but every prelude-loser diagnostic is discarded by `loserWarning`, and `sealPrelude` silences the rest. |
| `Liveness.liveLabels` | stops at the boundary | `Just _ -> Set.insert name seen` for `from /= file`. Opaque. |
| `Liveness.checkDeadLabels` | no | guarded `from == file` |
| `Label.checkLabels` bodyProbs | no | guarded `from == file` |
| `Resolution.resolveLabelBody` | no | `from /= file -> (seen, [])` — *"imported: not our problem"* |
| `OnDefinition.labelTarget` | **yes** | `toRange (getSpan def)` — go-to-definition on an imported label |

`edIsOverride` is consulted only for *local* candidates (`localPrec d`); imported
precedence comes from `usingIsOverride via`. So it is not interface data.

**The resolution pass already treats an imported definition as opaque.** The boundary
you want is most of the way built. The one thing that still reaches through it is
go-to-definition, and all it needs is a span.

### The v1 interface entry, therefore

```
label      :: Labelling   -- the exported name
order      :: Int         -- lexical position in its file, for tie-breaks
origin     :: FilePath    -- the file that really defines it (see §2)
span       :: Span        -- where in `origin`, for go-to-definition
```

Four fields. No AST, no megaparsec types, no new dependency. If a later pass genuinely
needs more, add a field — that is the point of having drawn the boundary.

> If you would rather ship the fat version first (serialise `ExportedDef` whole) and
> narrow later, the rest of this plan is unchanged; only §4 changes. But the fat version
> costs a `Binary`/`Read` instance over the whole AST *and* commits the format to the
> `Ps` phase, which the desugarer is about to move.

---

## 2. Closedness — the property that says the interface is good enough

`exportedScope` republishes whatever won in the file's *effective* scope, so `export A;`
in a file that does `using c` re-exports **c's** definition. An interface entry must
therefore be self-contained: reading `a.bi` must never require reading `c.bi`.

> **Property to test:** compiling a file with only its *direct* dependencies' interfaces
> present in the environment must produce exactly the diagnostics it produces today.

That is checkable now and does not need the IR to exist.

### A bug this surfaces (predicted from the code, not yet reproduced — verify first)

`importedCandidates` sets `candFrom = usingPath via`, the file the `using` names — but
`getSpan def` is the span in the file that *originally* defined the label. For a
re-export chain `c → a → main`, `OnDefinition.labelTarget` builds
`Location (filePathToUri from) (toRange (getSpan def))`, i.e. **a's URI with c's line
numbers**. Right line, wrong file.

Reproduce with three files (`dep2` exports `P`; `dep1` does `using dep2; export P;`;
`main` does `using dep1; Q = P;`) and go-to-definition on `P` in `main`. Carrying
`origin` in the interface entry is the fix, and it is the same field closedness demands
anyway.

---

## 3. `.bi` file format

Line-oriented text. Not a serious wire format — a serious one is warranted once the IR
exists and the payload is big. This one has to survive being read by a human during
debugging, and cost no dependency.

```
betzac-bi 1                      # format version
source <64-bit hex>              # hash of the source text this was produced from
dep <64-bit hex> <path>          # one per direct dependency, interface hash, repeated
file <n> <path>                  # one per distinct defining file, interned
export <order> <line> <col> <endLine> <endCol> <file-n> <label>
```

Notes:

- **One free-form field per line, and it goes last.** A path may contain spaces, and so
  may a label -- the descriptor alphabet is alphanumerics, comma and space, so
  `:earth general:` is an ordinary label. Two such fields cannot share a line, which is
  why origins are interned into a `file` table and named by index. It also stops a file
  re-exporting 300 labels from repeating a 140-character path 300 times.
- **Format version first.** A compiler upgrade that changes the payload must invalidate
  every `.bi` on disk. Bump the version; a mismatched version is a miss, never an error.
- **The interface hash covers the exports alone** -- not the source hash, not the
  dependency stamps, and not the `file` table's numbering. A change in layout is not a
  change in meaning.
- **`Span`, not `SourcePos`.** Store four `Int`s and rebuild via `mkPos` on load.
  Serialising megaparsec's types couples the format to a dependency for no gain.
  `Generated` spans render as four zeroes, which no real position can be.
- **Paths are absolute and canonical**, as `ccFiles` keys already are.
- **Dependency hashes are *interface* hashes, not source hashes** -- see §5.

### Hashing

FNV-1a over the source's UTF-8 bytes; about five lines, no dependency. A collision means
a stale build rather than a crash, which is acceptable at this stage and is a swap to
`cryptohash-sha256` later if it stops being. Add the dependency deliberately if so — the
Hackage bounds are pinned to what LTS-24.38 resolved (see `TODO`), so a new dependency is
not free.

### Where the files live

**Not beside the source.** The prelude ships in the installed data directory
(`.stack-work/install/.../share/.../prelude/std.betza`), which is read-only; writing
`std.bi` next to it fails. A workspace under version control also should not sprout
artifacts.

One cache root, `$XDG_CACHE_HOME/betzac/bi/` (falling back to `~/.cache`), with each
entry named by the hash of the source's canonical absolute path. Uniform for workspace
files, the prelude and anything reached by an absolute `using`. A `--bi-dir` flag can
override it, and the tests will need one.

---

## 4. Staleness rules

A `.bi` is usable for file *F* when **all** of:

1. its format version matches the compiler's;
2. `source` equals the hash of the text that would be compiled *right now*;
3. every `dep` line names a file whose own interface is usable, with a matching hash.

Rule 2 does the work that matters for `bls`: an editor's dirty buffer hashes differently
from the file behind it, so an unsaved edit can never be served from a `.bi`. No
special-casing of the VFS is needed — the same content-keying `LangServer.Cache` already
relies on ("keyed by content rather than by mtime or LSP version") extends unchanged.

Rule 3 as written is *conservative*: a dependency whose own artifact is stale makes
every dependent stale too, because its current interface hash is unknown until something
recomputes it. Correct, and it already buys the main win -- nothing unchanged is ever
recompiled.

The stronger claim, **a dependency whose source changed but whose interface did not does
not invalidate its dependents**, needs one more move: on a stale dependency, recompile
it, then compare its *fresh* interface hash against the stamp before condemning the
dependent. That is exactly GHC's recompilation check, and it is a follow-up, not part of
the rules above. Until it lands, a comment edit deep in a chain still rebuilds the chain.

---

## 5. What already exists, and what this actually replaces

`Driver/Resolve.hs`'s `carriedEntry` is this design, in memory, for one process:

```haskell
if sourceText (fePipeline old) == sourceText (fePipeline entry)
    && feUsingTargets old == feUsingTargets entry
    && all (`Set.member` carried) deps
```

— same text, same imports, every dependency also carried. That is rules 2 and 3 with
identity in place of hashing and the previous `CompilationContext` in place of the disk.
The invalidation logic is written and works; this plan gives it a disk backing and a
narrower payload.

So the shape to aim for is *one* validity notion with two backings, not two mechanisms.
`resolveScopesFrom`'s `prev` argument and the `.bi` store answer the same question.

---

## 6. Steps

Each step builds and tests clean on its own. Steps 1-6 are done; what §4 calls the
stronger claim is not.

1. **`Betzac/Compilation/Interface.hs`** — the `Interface` and `InterfaceEntry` types,
   `interfaceHash`, `renderInterface`, `parseInterface`. Pure, no IO. Property test:
   `parseInterface . renderInterface === Right`.

2. **`interfaceOf :: FilePath -> Map Labelling ExportedDef -> Interface`** — derive an
   interface from a resolved file. Also `origin`: `feExported` currently stores
   `ExportedDef` with no record of which file defined it, so this has to read
   `feEffective`'s `ResolvedDef` (which has `rdFrom`) rather than `feExported`. Fixing
   §2's bug happens here.

3. **Consume interfaces instead of `ExportedDef` in scope resolution.** Change
   `ImportedScope` and `PreludeScope` to hold `LabelTable InterfaceEntry`. `localDefs`
   keeps producing `ExportedDef` — local definitions still need their bodies. This is the
   real refactor and the one to do before any IO exists: **at this point nothing is
   written to disk, and everything must still pass.** If it does, the boundary is proven.

4. **`Betzac/Compilation/Driver/Store.hs`** — read and write `.bi` in the cache root,
   with the §4 validity check. IO isolated here.

5. **Wire the store into discovery.** A `using` target with a usable `.bi` is loaded, not
   visited: no read, no lex, no parse, no semantic passes. This is where the win lands,
   and where `SourceAccess` grows a third field, or gains a variant that answers
   "interface only".

6. **Write `.bi` after a successful resolve**, for every file that resolved without
   errors. Never for a file with errors — a broken file has no interface, and caching one
   would serve a lie. Warnings do not block writing.

7. **`bls`**: nothing should change, and that is the assertion to check. It gets `.bi`
   reads for its dependencies for free through discovery, and rule 2 keeps dirty buffers
   out. Confirm `bls-test` still passes *with a warm cache*, which means the test
   harness needs its own `--bi-dir` under `temporary`, or the suites will interfere.

---

## 7. Tests

- **Round-trip**: `parseInterface . renderInterface === Right` over a generated
  `Interface` (Hedgehog).
- **Closedness** (§2): a file compiles to identical diagnostics with only its direct
  dependencies' interfaces in scope.
- **Equivalence**: for `pieces/chess.betza` and `pieces/taikyoku.betza`, the diagnostics
  from a cold cache and a warm cache are identical. This is the regression test that
  matters most and the one to write first.
- **Interface stability**: editing a comment or a private definition in a dependency
  leaves its interface hash unchanged — assert the *hash*, not the dependents' output,
  or the test passes for the wrong reason.
- **Staleness**: a changed dependency invalidates a dependent; a version bump invalidates
  everything; a corrupt `.bi` is a miss and not a crash.
- **Re-export go-to-definition** (§2's bug): `dep2 → dep1 → main`, definition of `P`
  resolves to `dep2`.

---

## 8. Verification

1. `stack build betzac:lib betzac:exe:betzac betzac:exe:bls betzac:test:betzac-test betzac:test:bls-test --fast`
2. `fourmolu --mode inplace` on every touched `.hs`.
3. `stack exec -- betzac pieces/taikyoku.betza --workspace pieces -Wall -vv`, twice —
   cold then warm. Byte-identical diagnostics, and the second run visibly does less.
4. Delete the cache root mid-session and re-run: must rebuild, not fail.
5. Make the cache root read-only and re-run: must compile, not fail. An unwritable cache
   is a missed optimisation, never an error.
6. `-O1` allocation A/B on the warm path, per CLAUDE.md §11 — never from a `--fast`
   build. The claim being made is "a warm dependency costs a file read instead of a
   compile"; `bytes allocated in the heap` is what proves it.

### Measured, 2026-08-28, `-O1`, `+RTS -s`

| Target | Cold | Warm | |
| --- | --- | --- | --- |
| a 2-line file whose `using` pulls in the 623-line `taikyoku.betza` | 40,889,408 B | 12,608,256 B | **-69%**, 24ms → 8ms |
| `taikyoku.betza` itself, whose dependencies are 46 lines between them | 38,852,784 B | 38,225,520 B | -1.6% |

Warm readings repeat to within 30 bytes across runs. The second row is the honest shape
of the thing: the target is always compiled, so the win is exactly the size of what you
depend on, and nothing else.

---

## 9. How the interface projects forward

The interface being thin today is worth little if the IR is going to fatten it. It is
not, and the reason is a property worth stating before either exists.

### The IR is closed under the language's combinators

If every combinator compiles to a **wrapper around an opaque operand**, a dependent never
needs to see inside a dependency:

| Combinator | As a wrapper over opaque `A` | What it requires of the bytecode |
| --- | --- | --- |
| `A + B` | run both, union the results | nothing |
| `A - B` | run `A`, then run `B` from each landing square | `B` re-runnable from an arbitrary origin and incoming direction -- no absolute origin or board orientation baked in |
| `fA`, `<ff>A` | filter the emitted first-leg displacements | the first leg's displacement is observable where a modifier could apply |
| `mA`, `cA` | filter by per-leg capture behaviour | per-leg behaviour is observable |
| `A2`, `A0` | repeat | `A` is a callable subroutine, not inlined text |
| `!A` | control flow around `A` | nothing |

Every one is a wrapper, so the three constraints in the right column are the whole
requirement. They are cheap to design in and expensive to retrofit; write them down
before the first opcode.

The case that looks like it breaks closure and does not: the implicit `f` on a step-chain
continuation wraps the operand at the continuation position rather than reaching into it,
and it is inserted at `Ds`, long before codegen meets a dependency. Same for the `a` that
cancels it.

### So the interface grows by two fields, and neither is the bytecode

```
label, order, origin, span       -- today: scope resolution and go-to-definition
classes :: Set DisplacementClass -- the geometry phase: diagnostics
symbol  :: <ref into the dependency's compiled unit>  -- codegen
```

Codegen needs a *reference* to the dependency's compiled subroutine, the way a C header
declares what an object file defines. Six fields, still no AST.

### Keep the code in a sibling artifact, `.bo`

Bundling bytecode into the `.bi` destroys the one property this whole document exists to
buy: a dependency whose source changed but whose interface did not must not invalidate
its dependents. If codegen output feeds the interface hash, every backend change and
every body edit invalidates the world.

Two artifacts, two hashes, two read frequencies -- every dependent reads the `.bi`, only
the linker reads the `.bo`. Re-exports reference the original symbol rather than copying
it, which is §2's closedness rule applied to codegen.

### Displacement classes are stored, not recovered

Exponent expansion belongs to the IR because `0` and `0*` are unbounded, so the bytecode
has loops and is board-dependent by construction. The class set therefore cannot be
recovered by *running* it -- that would mean running it on every board -- and recovering
it statically means re-deriving in the backend what the geometry phase computed exactly
and threw away. The interface carries the summary; the object carries the code.

### If closure ever fails

Cross-file *optimization* -- constant-folding `W-F` when both are imported rather than
emitting two calls -- would need unfoldings in the interface, which is GHC's design
exactly. It is purely additive, and it trades interface-hash stability for speed. Not a
reason to design for it now.

---

## 10. Out of scope

- **The IR itself.** Nothing here requires knowing what a betza object is; §9 argues
  the interface does not have to change when it arrives.
- **Distributing compilation.** Per-file independence is a consequence of this work, not
  a goal of it. Do not build a scheduler.
- **`sealPrelude`.** Interfaces carry no diagnostics, so a prelude loaded from `.bi`
  arrives already silent. Whether `sealPrelude` still has a job once step 5 lands is a
  question to answer *after* it does, not a refactor to bundle in.
