# Plan: delayed-perturbation ("post-establishment shock") sensitivity analysis

**Status:** Steps 1-3 (C++/Rcpp checkpoint + reinit; exposing the missing parameter fields;
repeated-climate CSV) are implemented/built and smoke-tested — see the "Step N status"
sections below. **Step 4 was redesigned after a major course-correction — see "Step 4
redesign" section near the end of this doc before reading the original "Concrete
implementation plan" below, which describes the SUPERSEDED v1 design.**

## Motivation

The existing OAT sensitivity analysis (`vignettes/sensitivity_analysis.Rmd`) perturbs each
parameter for the tree's *entire simulated life*, from seedling (`run_lho()` sets the field
before `lho$init()`). That confounds the parameter's steady-state physiological effect with
a life-history-stage effect: the perturbed run grows up differently from year 1, so by the
time you look at "last 10 years" it's not a mature tree responding to a changed parameter,
it's a tree that had a different childhood.

Goal: build a **second, separate** analysis where the tree grows to near-steady-state under
baseline parameters first, and the OAT perturbation is applied only after that — a genuine
"shock response" test — with climate held identical across the spin-up so all branches reach
the same starting point regardless of which parameter will later be perturbed. This is a
reliability/robustness check on the existing results, not a replacement for them.

## Decisions already made (confirmed with Joanna)

1. **Scope**: do NOT touch the main/normal simulation runs (calibration, Publication Plots,
   etc.) — this is purely a new, additional analysis, sensitivity_analysis-only.
2. **Keep the existing OAT analysis untouched.** Add the delayed-perturbation version as a
   **new section or new script/vignette alongside it**, so both are available for comparison
   (do not replace `sensitivity_analysis.Rmd`'s existing content in place).
3. **Climate after the perturbation point**: keep repeating the same single fixed year
   indefinitely (not a switch back to real sequential 1960–2022 history). This gives the
   cleanest, most textbook-like steady-state shock-response test, fully independent of any
   particular historical weather sequence.
4. Spin-up length: **50 years** before perturbing (revised from an initial 25y guess).
   Rationale: the background canopy in `ErgodicEnvironment::updateBackgroundCanopy()`
   (`src/life_history.cpp:21-60`) relaxes exponentially toward equilibrium stem density with
   `bg_canopy_tau = 25.0` (boreal params), i.e. an *e-folding* time, not a closure time — at
   elapsed=25y the background canopy is only ~63% of the way to equilibrium density, still
   visibly opening. 50y (2τ) reaches ~86%, a much better approximation of "canopy closed"
   while still being affordable. Since climate is already held fixed/repeated from the end of
   establishment onward (see #5), the extra simulated years cost nothing conceptually.
5. **Repeated-climate year: year 15** of the real climate record. Per decision #3/motivation
   above, this single year is repeated for the **entire** run — spin-up (from seedling) through
   the response window — not just after some real-climate establishment phase; that's what
   makes the spin-up identical across all branches regardless of which parameter is later
   perturbed.

6. **Response-window length: 20 years** after the 50y spin-up/perturbation point (not 15).
7. **Perturbation magnitude: ±25%, reported alongside the existing ±10%**, applied to every
   one of the 32 parameters individually (start with `root_no0`), in *both* this new script and
   the existing `sensitivity_analysis.Rmd` — see "Added requirement" section below for detail.
8. **File: separate `.Rmd`**, not a new section in `sensitivity_analysis.Rmd` —
   `vignettes/sensitivity_analysis_delayed_perturbation.Rmd`.

All 32 parameters are kept (no subset), reusing the existing OAT table corrected per finding #6.

## What's technically required, and why (investigation findings)

Two sub-agent investigations established the following facts about the model
(`LifeHistoryOptimizer`, in `inst/include/life_history.h` / `src/life_history.cpp`,
Rcpp-bound in `src/r_interface.cpp`):

1. **Parameters cannot be swapped mid-run today.** `par0`/`traits0`/`uptake0` are Rcpp-exposed
   RefClass fields, but they are only ever consumed **once**, inside `LifeHistoryOptimizer::init()`,
   via `P.init(par0, traits0, uptake0)` (`life_history.cpp:135`). `grow_for_dt()` only ever reads
   the live `P.par`/`P.traits`/`P.uptake`, never `par0`/`traits0`/`uptake0` again. So setting
   `lho$par0$x <- val` after `init()` has already run is currently a no-op (soil nitrogen is the
   one exception — `set_soil_nitrogen()` writes directly into the live `Climate` object and is
   re-read every step).

2. **No checkpoint/restart is exposed to R.** `Patch` (the multi-cohort engine) has
   `save()`/`restore()` in C++ but they're not Rcpp-bound, and are irrelevant anyway because...

3. **`LifeHistoryOptimizer` wraps a single `plant::Plant P` object directly** — no `Patch`/
   `Solver`/cohort machinery (`life_history.h:42`). `Plant` has no pointers/references (confirmed:
   the existing yearly-reopt trajectory code already relies on this, doing plain
   `plant::Plant P_backup = P;` value-copy backups, `life_history.cpp:470-471`). This means a
   **full, legitimate checkpoint is just a value copy** of `P` (and the environment `C`) — no new
   serialization machinery needed, just two new small methods.

4. **The inner `Plant::init(par, traits, uptake)`** (`src/plant.cpp:9-14`) does exactly:
   ```cpp
   par = _par; traits = _traits; uptake = _uptake;
   coordinateTraits();  // recomputes derived geometric constants from traits only
   ```
   `coordinateTraits()` → `geometry.init(par, traits)` (`plant_architecture.cpp:11-30`) only
   recomputes derived shape constants (`m,n,a,c,zm_H,qm,eta_c,dmat,pic_4a`) from the traits —
   it does **not** touch current size/mass/root-count/mycorrhizal-mass/N-pool state (those live
   in separate `geom` state fields untouched by `init`). So calling this inner `P.init(...)`
   again, **after** restoring a checkpoint, is exactly "re-parameterize one field, keep all
   accumulated state" — which is exactly what's needed.

5. **Climate matching is by decimal-year lookup** (`ClimateStream`/`flare::Stream`, binary search
   on sorted `Decimal_year`), not row index, and the stream is periodic over its whole span. So a
   "repeat the same year" climate file is just a CSV-construction task (duplicate one year's
   weather rows under ascending fake decimal-years) — **no code changes needed** for this part.

6. **Struct-membership check while building the field list** (see below) found the existing OAT
   parameter table in `sensitivity_analysis.Rmd` mislabels `nc_leaf`/`nc_root` as `par0`
   (`PlantParameters`) when they actually live in `PlantTraits` (`traits_params.h:52-53`). This is
   currently harmless there (neither has a live accessor yet, so both fall through to the
   temp-ini-file workaround regardless of the label) — but it needs to be labelled correctly in
   the **new** script once these fields get live accessors. Not fixing it in the original
   vignette (out of scope, per decision #2) — just flagging it here so it isn't silently
   re-introduced.

## Step 1 status: DONE, with one finding that affects Step 4

Implemented and smoke-tested (2026-10-02): `save_checkpoint()`/`restore_checkpoint()`/
`reinit_params()` added to `LifeHistoryOptimizer` (header + `.cpp` + Rcpp bindings), rebuilt
cleanly, `devtools::install()`'d. Checkpoint captures `P`, `C`, and the four scalar
accumulators (`rep`, `litter_pool`, `seeds`, `prod`) — mirroring the full backup set already
used by `run_with_relaxed_local_reopt_trajectory*`.

**Verified by direct smoke test** (not just inspection):
- A no-op `reinit_params()` (par0/traits0/uptake0 unchanged) reproduces the baseline forward
  trajectory bit-for-bit, confirmed over several months of `grow_for_dt()` past the checkpoint.
  (A frozen `get_state()` taken *immediately* after `reinit_params()`, before any further
  `grow_for_dt()`, does show some zeroed fields — `N_bar_roots`, `N_static`, `r_zone`, `alpha`,
  `SA_active` — but these are per-step diagnostic outputs cached as public members of `Uptake`,
  wiped because `P.uptake = uptake0` replaces the whole struct; they're stale, not wrong, and
  get recomputed correctly on the next `grow_for_dt()`. Not a bug.)
- Perturbing `par0$u_max`/traits-level fields that are read live each step and then calling
  `reinit_params()` does change the forward trajectory, as expected.

**Important finding — `root_no0`/`root_length0` need an extra call, not just `reinit_params()`:**
perturbing `par0$root_no0` and calling `reinit_params()` alone has **zero effect** on the
trajectory. Root cause: `root_no0`/`root_length0` are *initialization-only* — they're read
exactly once, at the original `init()`, to seed the geometry's live root state via
`P.geometry.set_root(P.par.root_no0, P.par.root_length0, P.traits)` (`life_history.cpp:141`).
After that, every step reads the live `P.geometry.root_no`/`root_length`, never `P.par.root_no0`
again — so `reinit_params()` (which only does `par=par0; traits=traits0; uptake=uptake0;
coordinateTraits();`) correctly leaves it untouched, exactly as designed, but that means it's
*not sufficient* to actually apply a root-count/length perturbation.

Fix: the already-existing (already Rcpp-bound) `root_override(rn, rl)` method
(`life_history.cpp:119-121`: `P.geometry.set_root(_rn, _rl, P.traits)`) does the job — confirmed
by smoke test, perturbing `root_no0` by +25% via `root_override()` after `reinit_params()`
propagates through root_mass, N uptake, ectomycorrhiza_mass, height, etc. within months.

**Consequence for Step 4**: the per-parameter perturbation dispatch in the new script can't be
a single uniform `lho$restore_checkpoint(); lho$par0[[field]] <- value; lho$reinit_params()` for
all 32 parameters — `root_no0`/`root_length0` need an added `lho$root_override(new_rn, new_rl)`
call (using the *other* of the pair's current/baseline value when only one is being perturbed).
Worth checking whether any other of the 32 parameters have the same "consumed once at init,
not reinit_params()-reachable" problem (prime suspects: `nitrogen_start0`, `nitrogen_uptake0` —
set via `P.geometry.init_nitrogen()` at construction only, same pattern) before Step 4 is
written — not yet checked.

## Step 2 status: DONE, with two findings that affect Step 4

Implemented (2026-10-02): added `.field(...)` bindings in `src/r_interface.cpp` for all 11
previously-missing fields — `nc_leaf`/`nc_root`/`mycorrhizal_biomass_conversion` on
`PlantTraits`, `les_u` on `PlantParameters`, `myco_diameter`/`rho_myco`/`D_static`/`f_static`/
`depth`/`k_13`/`investment_from_mycorrhiza` on `Uptake`. Rebuilt, `devtools::install()`'d.
All 11 read back correctly from `p_test_boreal.ini` via a fresh `lho`.

**Verified propagation by smoke test** (one field per struct, perturbed post-checkpoint via
`reinit_params()`, forward trajectory compared to baseline): `nc_leaf` (PlantTraits) and
`D_static` (Uptake) both correctly change the trajectory. The other 9 are read directly inside
`uptake_core()`/per-step code the same way `D_static`/`nc_leaf` are, so presumed live too, but
not each individually re-verified.

**Finding 1 — `les_u` is dead in the live model, independent of any of this work.** It only
feeds into `fac` in `Assimilator::les_update_lifespans()` (`src/assimilation.cpp:8-19`), but the
very next line hardcodes `kappa_l = 1.0/3.0`, discarding `fac` entirely — so varying `les_u`
(via `reinit_params()`, the temp-ini-file fallback, or even just editing the ini file by hand)
has **zero effect on any trajectory**, confirmed by smoke test. Pre-existing model code, nothing
to do with Steps 1-2. Flagging because both the existing and new sensitivity analyses include
`les_u` in their 32-parameter table — its sensitivity index will trivially be ~0 in both, which
could be misread as "confirmed insensitive" rather than "the parameter is currently inert."
Not fixed (out of scope here) — just don't be surprised by a flat-zero `les_u` row.

**Finding 2 — `nitrogen_start0` has the same init-only problem as `root_no0`/`root_length0`**
(flagged as unchecked after Step 1, now confirmed by smoke test): perturbing `par0$nitrogen_start0`
and calling `reinit_params()` alone has zero effect, because it's consumed once via
`P.geometry.init_nitrogen(P.par.nitrogen_uptake0, P.par.nitrogen_start0, P.traits)` at the
original `init()` only. Unlike root count/length, there is **no existing override method** for
the live N-pool state to mirror `root_override()`.

**Confirmed (2026-10-02): exclude `nitrogen_start0` from the delayed-perturbation table.**
By the time the 50y checkpoint is reached, the tree's actual N state is whatever 50 years of
dynamics produced from that starting condition — `nitrogen_start0`'s effect has already fully
played out and is baked into the checkpointed `P.geometry` state. There's no live "current N
pool" lever that corresponds to "what if it had started with different N" post-hoc, so it's left
at its baseline (unperturbed) value in the new script. Its perturbation stays in the existing
perturb-from-seedling `sensitivity_analysis.Rmd`, where "initial N" is a meaningful, well-defined
thing to vary. No new C++ code needed for this one.

So the init-only group is now fully resolved: **`root_no0`/`root_length0` get the extra
`root_override()` call in Step 4's per-parameter dispatch; `nitrogen_start0` is simply excluded
from the delayed-perturbation parameter table** (kept baseline there, perturbed only in the
original script).

## Step 3 status: DONE

Built via new generator script `vignettes/make_repeated_climate_csv.R`, which reads
`data/ERAS_Monthly.csv`, extracts one calendar year's 12 rows, and tiles them across 75 fake
years (50 spin-up + 20 response + 5 buffer) starting at 1960 (matching `bg_canopy_t0`/the
existing scripts' `start_year` convention), each block's `Decimal_year` strictly ascending
(`Year + (Month-1)/12`) so `ErgodicEnvironment::updateBackgroundCanopy()`'s elapsed-time
calculation still advances even though the weather itself never changes. Output:
`data/ERAS_Monthly_constant1974.csv` (900 rows). Column format (`Year,Month,Decimal_year,
Temp,VPD,PPFD,PPFD_max,SWP,GPP`) matches the real file and the existing
`data/ERAS_Monthly_constant1960.csv` (built earlier for an unrelated trajectory-based
constclimate analysis, same tiling idea, repeats 1960 instead) — **note: this is Year/Month/
Decimal_year/Temp/VPD/PPFD/PPFD_max/SWP/GPP, not the lowercase year/month/decimal_year/temp/
vpd/par/par_max/swp this doc's Step 3 section originally said** — corrected here.

**Which year is "year 15":** the record spans 1960-2022. Treating 1960 as year 1 of the
record, year 15 = **1974**. Not re-confirmed with Joanna after building — flagged clearly when
reported; easy to regenerate for a different year/index convention if that's wrong.

**Verified by direct smoke test** (not just inspection):
- Model loads the file and grows on it without error for a representative 2-year window.
- Ran the **full intended 70-year window** (1960-2030) with `grow_for_dt()` wrapped in
  `tryCatch` per step (matching the existing `run_lho()` error-handling convention in
  `sensitivity_analysis.Rmd` — occasional Phydro line-search non-convergence is pre-existing,
  see Step 1 status, and tolerated by skipping just that one step): **zero failed steps**
  across all 840 monthly steps.
- **Canopy closure is real and matches the design rationale**: `canopy_openness` is `{1.0}`
  (fully open, zero background layers registered yet) at elapsed=25y, but by elapsed=50y a
  second layer has formed (`{1.0, 0.27}`) — confirms the background stand genuinely continues
  closing between year 25 and year 50, which is exactly why spin-up was extended from 25y to
  50y (decision #4 above). By elapsed=70y it's unchanged from 50y (`{1.0, 0.27}`) — plausible
  (another full layer threshold not yet crossed), not re-investigated further.

**Consequence for Step 4**: every `grow_for_dt()` call in the new script must be wrapped in
`tryCatch` per step (one failed step skipped, not fatal to the whole run), exactly like
`run_lho()` already does in `sensitivity_analysis.Rmd` — confirmed necessary by this test
(a plain uncaught loop over the same file did hit a non-convergence error around elapsed=50y
in an earlier, non-tryCatch'd test run; wrapped, it didn't recur at all — consistent with this
being a transient/skippable solver hiccup rather than a persistent divergence).

## Concrete implementation plan

### Step 1 — C++/Rcpp: checkpoint + live re-parameterize (small, additive, mirrors existing patterns)

Files: `inst/include/life_history.h`, `src/life_history.cpp`, `src/r_interface.cpp`.

- Add to `LifeHistoryOptimizer`: `plant::Plant P_checkpoint; ErgodicEnvironment C_checkpoint;`
- Add methods:
  ```cpp
  void save_checkpoint();     // P_checkpoint = P; C_checkpoint = C;
  void restore_checkpoint();  // P = P_checkpoint; C = C_checkpoint;
  void reinit_params();       // P.init(par0, traits0, uptake0);  -- re-copies par/traits/uptake
                               // and recomputes geometry constants, WITHOUT resetting size/
                               // mass/root count/mycorrhizal mass/N pools/mortality.
  ```
- Register all three in the Rcpp module (`class_<pfate::LifeHistoryOptimizer>` block,
  `r_interface.cpp:180-206`), alongside the existing `.method(...)` calls.
- Rebuild required: `devtools::document()` then `devtools::install()`.

### Step 2 — Rcpp: expose the missing parameter fields

So every one of the 32 OAT parameters can be perturbed live via `reinit_params()` instead of
the temp-ini-file fallback. Add to the relevant `class_<...>` blocks in `r_interface.cpp`:

- `PlantTraits` (`r_interface.cpp:54-85`): add `nc_leaf`, `nc_root`, `mycorrhizal_biomass_conversion`
  (all confirmed to live in `traits_params.h`'s `PlantTraits` class).
- `PlantParameters` (`r_interface.cpp:87-116`): add `les_u` (confirmed in `PlantParameters`).
- `Uptake` (`r_interface.cpp:41-52`): add `myco_diameter`, `rho_myco`, `D_static`, `f_static`,
  `depth`, `k_13`, `investment_from_mycorrhiza` (all confirmed in `uptake.h`).

All are plain `double` members — straightforward `.field(...)` additions, same pattern as
existing entries.

### Step 3 — Build the repeated-climate CSV — DONE, see "Step 3 status" above

`data/ERAS_Monthly_constant1974.csv`, built by `vignettes/make_repeated_climate_csv.R`:
**year 1974** ("year 15" of the 1960-2022 record, 1960 = year 1) repeated across the *entire*
spin-up + response period (from seedling, not just post-establishment — see decision #5), 75
fake years total, strictly ascending `Decimal_year`. Point both `set_i_metFile()` and
`set_a_metFile()` at this file in Step 4's script.

### Step 4 — New script: `vignettes/sensitivity_analysis_delayed_perturbation.Rmd`

1. Construct `lho`, `init()`, grow 50 years under the repeated-climate CSV with baseline
   (unperturbed) parameters.
2. `lho$save_checkpoint()`.
3. For each of the 32 params × {±10%, ±25%} × {high, low} (reusing the existing `params` table
   from `sensitivity_analysis.Rmd`, corrected per finding #6 above): `lho$restore_checkpoint()`,
   set the one field on `par0`/`traits0`/`uptake0`, `lho$reinit_params()`, continue
   `grow_for_dt()` for 20 more years; last 10 of those 20 summarised, matching the existing
   "last 10 years, summer mean" convention.
4. Reuse the existing sensitivity-index math, tornado chart, and heatmap code, extended to show
   ±10% and ±25% results side-by-side per parameter (so magnitude-stability is visible directly).

### Step 5a — Add ±25% alongside ±10% in the existing `sensitivity_analysis.Rmd` too

Per the "Added requirement" section below, the magnitude-stability check also applies to the
*existing* perturb-from-seedling script. This is an **addition** (new column/comparison), not
a replacement of its current ±10% results — consistent with decision #2 ("keep the existing
OAT analysis untouched" means don't remove/alter its existing content, not "never touch the
file again"). Concretely: generalize `perturb_frac <- 0.10` to a vector `c(0.10, 0.25)`, run
both for every parameter, and extend the existing sensitivity-index/tornado/heatmap code to
plot both side-by-side.

### Step 5 — Sanity checks before trusting results

- Confirm `reinit_params()` really leaves size/mass/root/N-pool state untouched (quick smoke
  test: call it with no actual value change and confirm trajectory is bit-identical to not
  calling it).
- Confirm the repeated-climate spin-up actually reaches a visibly flatter/steadier trajectory
  by year 50 than the real-climate baseline does (plot height/GPP over the spin-up window).
- Spot-check one or two parameters against the existing (perturb-from-seedling) OAT results to
  see whether/how the sensitivity indices differ, since that's the whole point of the exercise.

## Open questions for tomorrow (pick up here)

1. ~~Response-window length~~ **Confirmed: 20 years** (grow to 50y under baseline, perturb,
   continue 20 more years; last-10-years summary convention presumably still applies within
   that 20y window — confirm when implementing).
2. ~~New section vs. separate file~~ **Confirmed: separate file**,
   `vignettes/sensitivity_analysis_delayed_perturbation.Rmd`.
3. ~~Widened perturbation range magnitude~~ **Confirmed: see new section below.**

All open questions are now resolved; ready to implement (Steps 1-5 above), pending go-ahead.

## Added requirement: widen the OAT perturbation range, check stability per parameter

Current `sensitivity_analysis.Rmd` uses one global `perturb_frac <- 0.10` (±10% of baseline)
for all 32 parameters (`vignettes/sensitivity_analysis.Rmd:40,81-125` — there is no
per-parameter high/low table, just a flat fraction applied uniformly at perturbation time).

Joanna wants this widened — **at minimum for root tip density (`root_no0`)** — and then wants
the *same widened-range treatment applied to every parameter separately* (not just
`root_no0`), to check whether each parameter's sensitivity conclusion (sign/magnitude/ranking)
is still **stable** when the perturbation is bigger, vs. an artifact of the small ±10% default.
This is a third, independent robustness axis alongside the delayed-perturbation timing check
above — it answers "does the sensitivity analysis's conclusion depend on how hard we perturb?"
rather than "does it depend on when (life-stage) we perturb?".

**Confirmed (2026-10-02):**
- **Magnitude: ±25%** (vs. the existing ±10% default), applied per parameter in turn — start
  with `root_no0`, then every one of the 32 parameters individually.
- **Report both ±10% and ±25% side-by-side** per parameter, so the sensitivity-index comparison
  itself shows whether the conclusion is stable to perturbation magnitude.
- **Applies to both scripts**: the existing perturb-from-seedling `sensitivity_analysis.Rmd`
  (add a ±25% column alongside its current ±10%) *and* the new delayed-perturbation
  (50y spin-up + 20y response) script (same ±10% vs ±25% comparison, once that script exists).

## Step 4 redesign (2026-10-02): the above "Concrete implementation plan" targeted the wrong figure

Built Step 4 v1 exactly as planned above (plain `grow_for_dt()` spin-up, OAT perturb via
`reinit_params()`/`root_override()`, ±10%/±25%, tornado + heatmap on last-10-years summary
stats) — but this reproduces the mechanism of **manuscript Figure S8 (page 39)**: a one-at-a-time
sensitivity index on raw physiological outputs (GPP, height, N uptake...) for a *fixed*
parameter value. Joanna's actual target was **Figure S10/S11 (page 41)**: whether the **yearly
local-pattern-search re-optimisation's converged trait choice** (root tip density, root tip
length, ECM allocation `investment_from_tree`, ECM colonisation `mycorrhized`) is **stable**,
currently only tested by varying the tree's *starting* `root_no0` across 3 values under
constant (Fig S10) and real (Fig S11) climate. "Stability of the optimum", not an OAT index on
physiology. v1's script and its output files were superseded, not deleted (still on disk as
`sensitivity_analysis_delayed_perturbation.Rmd`'s first version's cache, if ever generated).

**Confirmed design for v2** (via direct questions, all answered "Recommended"):
1. For the 4 parameters the search itself controls (`root_no0`, `root_length0`,
   `investment_from_tree`, `mycorrhized`): the "shock" is a **mid-course kick to the search's
   own current converged value** at year 50 (not a restart-from-seedling with a different
   starting value, which is what Fig S10/11 already do) — then watch whether
   `run_with_relaxed_local_reopt_trajectory()` returns to the baseline continuation's
   trajectory or settles on something else.
2. **Both the 50y spin-up and the 20y post-shock response window use the real re-optimisation
   search** (`run_with_relaxed_local_reopt_trajectory()`), not plain `grow_for_dt()` — this is
   "the normal model" as used throughout the project's other `run_phase2_*.R` scripts, and is
   what actually moves root_no/root_length/ecto_allo/mycorrhized.
3. **Output is trajectory plots** (root tip density, root tip length, ECM allocation, ECM
   colonisation, height, GPP/crown area, ECM biomass, total N uptake — 8 panels per parameter,
   echoing Fig S10/11's layout), shocked vs. unperturbed baseline continuation, not a collapsed
   scalar sensitivity index — the whole point is seeing *whether and how fast* it reconverges.

**Important correctness subtlety found while rebuilding**: since the spin-up now actively
re-optimises `root_no`/`root_length`/`ecto_allo`/`mycorrhized`, by year 50 these have moved away
from their ini seed values. So "reset to baseline" for these 4 (when testing a *different*
parameter, or for the unperturbed-pair dimension when testing one of a pair) must mean the
**converged value at the checkpoint**, not the ini value — otherwise every non-decision-variable
shock would silently also wipe out 50 years of the search's own progress on these four. The
other 27 "fixed" parameters don't have this problem (they never drift from ini during an
unperturbed spin-up), so they still reset to their ini baseline. Implemented as
`reset_to_checkpoint()` in the new script, verified by direct test: shocking `wood_density`
leaves the post-reset `root_no`/`ecto_allo`/`mycorrhized` starting point identical to the
unperturbed baseline's, while shocking `root_no0` directly changes it as expected and the
effect visibly persists/decays over the following year (full 20y reconvergence picture is what
the real run will show).

**Perturbation magnitude**: widened to **{±10%, ±25%, ±50%}**, applied uniformly to all 31
parameters (not just `root_no0`) — satisfies "at least root tip density" trivially by widening
everything consistently, rather than special-casing root_no0 alone. Flagged as a judgment call
in case Joanna wanted root_no0-only widening or an absolute-multiple range instead (closer to
Fig S10/11's own `{1e5, 3e5, 1e6}` style).

**Timing**: ~0.74s per simulated year of the reopt search (measured directly) → 50y spin-up
≈ 37s, one 20y response run ≈ 15s. Full run: 31 params × 3 magnitudes × 2 directions = 186
shocked runs + 1 baseline ≈ 47 minutes total. Each shocked run is cached to its own `.rds` file
in `vignettes/plots/delayed_perturbation_stability/` (survives interruption/resume).

**Status**: v2 was built and smoke-tested, but scope was wrong (see "Step 4 v3" below) —
superseded before its full run finished.

## Step 4 v3 (2026-10-02, same day): scope corrected to the 4 optimised parameters only, with a before/after comparison

v2 still over-reached: it ran all 31 "fixed" parameters through the delayed-shock mechanism.
Joanna's actual ask is narrower and adds a dimension v2 didn't have:

- **Only the 4 parameters the search itself optimises**: `root_no0`, `root_length0`,
  `investment_from_tree` (ECM allocation), `mycorrhized` (ECM colonisation). The other 27
  "fixed" parameters are out of scope for this analysis entirely (that style of question —
  OAT on a fixed external parameter — is Figure S8's job, explicitly not wanted here).
- **Two timing conditions, compared directly, for each of the 4**:
  1. **"Before growth"**: the parameter is perturbed at construction (before any growth), then
     the search runs continuously for the full horizon (70y: `start_year` to `end_year`). This
     generalises Fig S10/S11's existing method (which only varies `root_no0`'s starting value)
     to all 4 decision variables.
  2. **"After growth"**: baseline search for 50y (reopt search, not plain growth — same as v2),
     checkpoint, replace that one decision variable's current *converged* value, continue the
     search 20y more. This is what v2 already built, now restricted to just these 4 parameters.
- **Goal**: if the optimisation is genuinely stable, perturbing a parameter by the same amount
  before vs. after growth should converge to the same place. Divergence between the two timing
  conditions means the optimum is history-dependent, not purely a function of the parameter
  value — this is the actual "is the OPTIMISATION stable" question, not a sensitivity index.
- **Magnitude**: kept at ±10%/±25%/±50% (the earlier-agreed widened range), computed relative
  to the ini baseline for "before" runs and relative to the checkpoint-converged value for
  "after" runs (same rationale as v2 — the search has already moved these 4 away from their ini
  seed values by year 50, so "no perturbation" must mean "stay at the converged state").

**Verified by direct smoke test** before the full run: both "before" (start `mycorrhized` at
0.5 instead of the 0.9 baseline, confirm the search's first logged value sits near 0.5, not
0.9) and "after" (checkpoint at ~0.925 after 2y, kick to 0.5, confirm the next logged value
sits near 0.5) mechanics work correctly.

**Run scope**: 4 params × 2 timings × 3 magnitudes × 2 directions = 48 perturbed runs + 2
baselines (one continuous 70y "before"-style baseline, one spin-up+no-kick-continuation
"after"-style baseline). "Before" runs are a full 70y search each (~52s); "after" runs are a
20y continuation each (~15s) off one shared checkpoint. Estimated total ~25-30 min. Each run
cached to its own `.rds` in `vignettes/plots/delayed_perturbation_stability/` as
`run_<field>_<before|after>_<magnitude>_<high|low>.rds`.

**Output**: one figure per parameter (`stability_<field>.png`), two stacked panel rows (before
/ after), 8 trace variables each (root tip density, root tip length, ECM allocation, ECM
colonisation, height, GPP/crown area, ECM biomass, total N uptake), coloured by magnitude,
overlaid on the relevant baseline — lets you read off directly whether "before" and "after"
settle at the same level.

**Speed sanity-check (2026-10-02)**: Joanna flagged the run completing suspiciously fast.
Investigated directly rather than assumed away: the completed `baselines` chunk (70y "before" +
50y spin-up + 20y "after"-baseline = 140 search-years) finished in ~2 minutes on this machine,
vs. ~21.8-84s/search-year implied by existing `vignettes/sens_*_constclimate.log` files (dated
Sep 25). Confirmed this is **not** a bug by inspecting the cached chunk's actual data directly
(`lazyLoad()` on the knitr cache `.rdb`): `baseline_before` has exactly 840 rows (70y × 12,
the full expected count, no truncation), `baseline_after` exactly 240 rows (20y × 12), and zero
`[trial-fail]`/`[commit-fail]` diagnostic lines in the render log (these print to stderr
whenever a Phydro line-search fails inside the search, per the `390afa7` commit earlier today —
none fired, so nothing was silently skipped). The Sep 25 logs most likely came from a slower
machine or a more heavily loaded one; this session's machine (13th-gen i5, 12 threads) may
simply be faster. Not fully explained, but the completed run's own data is internally
consistent and complete, which is the stronger evidence.

**Response window widened to 30y** (from 20y), per Joanna's request, for more room to observe
reconvergence. Required regenerating `data/ERAS_Monthly_constant1974.csv` with more buffer
years (`vignettes/make_repeated_climate_csv.R`: `N_YEARS` 75 → 90, now 50 spin-up + 30
response + 10 buffer) since the old 75-year file was too short for the new 80-year total
horizon. Old cache (`vignettes/sensitivity_analysis_delayed_perturbation_cache/` and the
per-run `.rds` files) cleared and the run restarted from scratch under the new window.

**Status**: v3 script (with `response_years = 30`) completed its first full run. Reviewing the
output plots directly (not just trusting a clean exit code) surfaced two real bugs, both fixed:

**Bug 1 — plot filter bug (cosmetic, not a data bug)**: `plot_timing()`'s filter was
`field == pfield | (is.na(field) & timing == tm)`, which doesn't check `timing` on the
perturbed rows at all — so the "before" panel also plotted the "after" runs' data and vice
versa (visible as a dense overlaid scribble after year 2010 in the "before" panel). Fixed to
`(field == pfield & timing == tm) | (is.na(field) & timing == tm)`. The underlying simulation
data was never wrong, only the plot — fixing it required no recompute, just a re-knit (fast,
since both the knitr `cache=TRUE` baselines chunk and the manual per-run `.rds` cache already
held everything).

**Bug 2 — unclamped kicks exceeding physical bounds (real data bug, 2 of 4 parameters'
"after" condition)**: `run_table`'s `value = ref ± delta` was never clamped to the decision
variable's valid range. The search's own candidate generation
(`make_levels_mult`/`make_levels_add`) always clamps to `STEP_ARGS`' min/max, but a value
assigned directly via `root_override()` / `reinit_params()` has no such protection. This bit
two parameters whose **checkpoint** value happens to sit close to its upper bound:
`mycorrhized` (checkpoint ≈0.994, max 1.0 — a "+50%" kick computed to 1.49, confirmed by
direct inspection of the cached `.rds`: first logged value 1.44, physically impossible >100%
colonisation) and `root_length0` (checkpoint ≈5.66mm, max 6.0mm — "+25%"/"+50%" kicks computed
to 7.07/8.49mm). Both produced visibly pathological trajectories (e.g. mycorrhized crashing to
exactly 0 by year 30 for the two more-clamped-should-have-been cases). `root_no0` and
`investment_from_tree`'s checkpoints are far enough from their bounds that this didn't bite
them. Fixed: `value = pmin(pmax(ref ± delta, bound_lo), bound_hi)`, bounds taken from the same
`STEP_ARGS` the search itself uses. All 48 `.rds` caches cleared and the run restarted (the
cached `baselines` chunk, unaffected by this change, was kept).

**Also noted, not fixed (methodological caveat, flagged to Joanna)**: the "before" condition's
kicks are sized relative to the **ini baseline** value (e.g. `investment_from_tree` = 0.2),
while the "after" condition's kicks are sized relative to the **converged checkpoint** value
(e.g. 0.028 for the same parameter at N_s=0.32, since the search drives ECM allocation down
over the spin-up). A "±50%" label therefore means a very different absolute kick size in the
two conditions for parameters whose checkpoint value has drifted far from its ini seed — this
is an inherent consequence of each condition using its own natural reference point, not a bug,
but it means "before" vs "after" at the "same" magnitude label aren't strictly a controlled
like-for-like comparison for such parameters. Not resolved; flagged for Joanna's judgement.

**Bug 3 — `cache=TRUE` on the `baselines` chunk silently broke every "after" run, and the
failure was itself invisible**: after fixing Bug 2, the re-run produced all 24 "before" `.rds`
files but **zero** "after" ones, with no visible error. Root cause, confirmed by direct
reproduction (`lazyLoad()`-ing the cached chunk in a fresh session and calling a method on
`lho`): `lho` is an `Rcpp_LifeHistoryOptimizer` — an external-pointer-backed Rcpp Module
object. knitr's `cache=TRUE` serializes chunk results to disk and reloads them in later
*separate* R sessions (every `rmarkdown::render()` call spawns a fresh one) — an external
pointer deserialized that way is dead, and any method call on it throws `"NULL value passed as
symbol address"`. This error *was* being caught by the per-run `tryCatch` and reported via
`message("FAILED: ...")` — but the global `knitr::opts_chunk$set(message = FALSE)` in this
script's own `setup` chunk silently swallowed every one of those 24 messages, so the failure
produced no visible trace at all (exit code 0, "Output created", clean-looking log). Only
noticed by checking the actual file count in the output directory against the expected 48,
not by reading the render log. Two fixes applied: (1) removed `cache=TRUE` from `baselines` —
the spin-up is cheap (~1-2 min) so just always recompute it fresh within the same session as
the runs that consume `lho`, avoiding the serialization problem entirely (the 48 perturbation
runs keep their own manual per-run `.rds` cache, which is safe since those are plain data
frames); (2) changed the failure handler from `message()` to `cat()`, since `cat()` isn't
subject to the `message = FALSE` suppression.

**General lesson for any future script in this repo that checkpoints an `lho`/`Rcpp_*` object
across knitr chunks**: never put `cache=TRUE` on a chunk whose result includes a live Rcpp
Module object (anything holding a `plant::Plant`/`LifeHistoryOptimizer`/etc.) — only cache
plain R data (data frames, vectors, lists of numbers). This is a general risk for any script in
this repo using the Step 1 checkpoint/restore mechanism inside an Rmd with chunk caching.

**Status**: all three bugs fixed, fresh run launched; not yet complete/reviewed as of this
writing.
