# Root economy vs. soil N: why root_no doesn't decline with N, and the u_max/root_no0 tension

**Context:** Started from Figure 4 (root_no, root_length vs. soil_nitrogen) not matching the
expected decreasing-with-N trend. Investigation ended up touching uptake kinetics (N̄),
transfer capacity, mycorrhizal kinetics, and the fitness function itself. This note is the
handoff point — see "State of the repo" below for exactly what's committed vs. still open.

## 1. u_transfer was wrong, and is now fixed (committed)

`u_transfer` (Hartig-net fungus→tree transfer capacity) was `6e-3`, sourced from a per-tip
literature conversion (Plassard & Dell 2010) that turned out to measure **soil-uptake flux at
the EM mantle**, not **fungus-to-host transfer** — the wrong physiological step entirely.

Re-derived top-down from Korhonen (2013) boreal net mineralisation (~20 kg N ha⁻¹ yr⁻¹ = 0.002
kg N m⁻² ground yr⁻¹) as a ceiling on total N transfer to trees, divided by colonised root
surface area per m² ground (two independent geometry estimates converged on ~3–10×10⁻⁴).
**New value: `u_max=5e-4`. Committed** (`23c1c3c`).

Effect: made the transfer cap genuinely binding (was slack by 5-10x before) — but this alone
did **not** fix root_no's trend; it made root_no pin harder against the grid's upper bound,
because the transfer ceiling is linear in root_no with no self-limiting feedback (see §3).

## 2. N̄ is genuinely, badly low — not just vs. k_15, vs. real field data

`N_bar_roots` at the calibrated boreal `N_s=1.03` sits at ≈4.6×10⁻⁷ kg N/m³. Your own Fig. S7
reference band (Korhonen 2013 lysimeter soil-solution mineral N) is `nbar_lower=4e-6` to
`nbar_upper=8e-5`. The model's N̄ is **~9× below the lower bound**, ~170× below the upper —
not a theoretical nicety, a real mismatch against observed soil solution concentrations.

Cause: `alpha = u_max·SA_active·R/(D·A_zone)` stays ≫ `N_diffused` across the whole tested
soil-N range (alpha ~2,000–8,700 vs. N_diffused maxing at ~0.3), so the system never leaves
the demand-crowded regime. Checked every lever:
- `D`: already at 0.01 (mid-range of 0.003–0.03 lit. range) — only ~3× headroom.
- `f_static`/`N_diffused`: modest room, maybe 1.5–2×.
- Zone geometry (`k_13`, `A_zone`): would need ~10,000× the radius — not physical.
- `SA_active` reduction: not a real fix (crippling uptake capacity isn't defensible).
- **`u_max`: the only lever with real headroom** — see §4.

## 3. Why root_no doesn't decline with N (the Fig. 4 question)

Ran wide/fine root_no sweeps (well beyond the original `{3e3...3e6}` grid) at fixed
`ecto_allo`/`root_length`, across 5 soil-N levels:

- The "pinned at grid max" behaviour in the original coarse grid was **not** a resolution
  artifact — at fine resolution the curve is a genuine, smooth, unimodal peak. It's just that
  **the peak's location barely moves across N=0.58–2.0** (stays ~3.0–3.4×10⁶ the whole way),
  and only shifts for very low N (0.01, peak <10⁶).
- Mechanism: `root_mass ∝ SA_fr` exactly (same functional form, differing only by a
  root_no-independent geometric constant `density_root·diameter/4`). Root **cost** (rr+tr,
  tied to root_mass) and the **transfer-ceiling benefit** (tied to SA_fr) therefore scale in
  lockstep with root_no — profitability-per-root-tip is constant, not diminishing, until you
  hit the point where the ceiling stops binding, which produces a sharp corner, not a gradual
  economic curve.
- Confirmed the peak coincides almost exactly with where GPP-per-crown-area **saturates**
  (leaf-N/Vcmax ceiling, ~9.58 flat from root_no≈3.36e6 onward) — so root_no genuinely is
  "foraging until GPP stops responding," the classical logic **is** operating correctly.
- But the root investment needed to *reach* that GPP ceiling doesn't shrink as soil N rises,
  because N̄ (§2) doesn't improve proportionally with nominal soil N — the crowding effect
  means more soil N doesn't translate into cheaper N per unit root investment.
- **Ruled out, cleanly, with real tests (not just reasoning):**
  - Raising `investment_from_mycorrhiza` (0.2→0.8): no effect — confirms the bottleneck is
    uptake capacity, not how the acquired N pool gets redistributed.
  - Fixing `ecto_allo` (removing its own adaptive response): no effect — root_no's flatness
    isn't an artifact of interaction with ecto_allo's co-optimization.
  - Objective function choice: re-ran the optimum selection using height, raw
    `assim_gross - rr - tr`, and the manuscript's actual `F_net = (A_gross - C_a - R_r - R_t)/(A_c·L)`
    (Eq. in §Tree Fitness, MAIN.tex) — **root_no is pinned at 3e6 under all three**. Not a
    metric artifact.
  - Mycorrhizal-specific kinetics (see §5) — structurally blocked, not just untested.

## 4. u_max correction (NOT committed yet — still in working tree)

Old `u_max=0.88 kg N m⁻² yr⁻¹` converts to ≈199 pmol NH4⁺ cm⁻² s⁻¹ — compared against jack pine
(*Pinus banksiana*) hydroponic Imax = 10.9 pmol cm⁻² s⁻¹ (Hangs, Knight & Van Rees), that's
**~18× too high**. Hydroponic Imax is the right quantity (diffusion limitation is already
handled separately via D/alpha, so this isn't double-counting). No area-based Scots-pine value
found despite a real search effort; Iivonen & Vapaavuori (2002) is species-exact but reports
uptake per unit root *fresh mass*, and converting to area basis needs an unverified specific-
root-area assumption that gave an implausible, contradictory answer — not trustworthy.

**New value set in the ini: `u_max=0.048`.** This is a real, defensible correction — but it
**breaks the default calibration run**: with the (unchanged) default `root_no0=3e5`, the tree
fails to establish at all (N̄ flatly 0, final height stuck at ~4.17m after 62 years, vs. the
expected ~18-22m). Traced the failure precisely:
- `ectomycorrhiza_mass` starts correctly (`set_root()` sets it equal to initial `root_mass`)
  but then collapses from parity down to ~0 within ~10 years, while root_mass keeps growing
  normally — it's a genuine dynamic death-spiral in the fungal symbiont, not a bad starting
  condition.
- The fungus is carbon-*rich* (its C reserve pool grows steadily) but nitrogen-*starved*
  (both uptake channels — near-field, gated by N̄=0; and the static/organic-mining channel —
  produce essentially zero N), so growth is N-limited even though carbon is abundant.
- Confirmed via a properly-isolated comparison that this collapse does **not** happen under
  the old `u_max=0.88` at `root_no=3e6` (healthy, steady establishment) — so it's specifically
  the corrected-`u_max` + default-`root_no0` combination that fails, not a pre-existing bug.

## 5. Why the fix isn't "give mycorrhizae their own (higher) uptake kinetics"

Tempting next step, but it's **structurally blocked by the manuscript's own cited assumption**:
MAIN.tex line 1260 states "Ectomycorrhiza and root uptake is equal per unit biomass for the
diffusive soil pool (standard, citing Baskaran 2017, Franklin 2014)". Giving fungi a separate,
much higher `u_max_myco` (a Paxillus involutus Vmax-derived value came out ~96× the corrected
root u_max, via Javelle et al. 1999 — see below) would contradict that deliberate, already-
cited design choice, not just tweak an unsourced default.

The real mycorrhizal advantage in this model (and per Baskaran/Franklin) is meant to come from
**surface area** (fungal biomass/SA_m dwarfing root SA_fr — already true, SA_m routinely 5–10×
SA_fr), not from kinetic superiority. That's already structurally present. The Javelle et al.
Paxillus Vmax value, while a real, well-matched literature find, measures fungal *uptake into
mycelium*, not *export from fungus to host* (a different transporter family, "Ato" efflux
proteins) — so it isn't the right number for `u_transfer` either, despite the surface
similarity. Filed but not used anywhere.

## 6. The actual unresolved fork

`root_no` is a single trait fixed for a plant's **entire simulated lifetime** (`set_root()` is
called once, at init — grepped the whole codebase, confirmed no per-timestep update exists
anywhere). Real trees show root:shoot ratio declining with age/size (well-documented
allometric pattern) — this model structurally cannot represent that. Two live options,
neither implemented yet:

1. **Push `root_no0` well above Helmisaari's observed range** (0.7–2.9M tips/m² crown area,
   field data, established Scots pine stands) to survive establishment under the corrected
   `u_max`. Defensible only by arguing the single fixed value should be biased toward
   establishment-phase need over mature-stand field match — a real trade-off, not free.
2. **Soften the `u_max` correction** — e.g. white spruce's milder ~9.6× factor (→`u_max≈0.092`)
   instead of jack pine's ~18×, enough to move N̄ without breaking establishment.
3. (Raised, not yet pursued) **Post-hoc staged optimization in the R layer** — `root_override()`
   is callable mid-simulation (not just at init), so a two-phase run (high root_no during
   establishment, lower once mature) is achievable without touching C++. Caveat: `set_root()`
   also resets `ectomycorrhiza_mass = root_mass(traits)` every time it's called, so switching
   mid-run isn't a clean continuous transition — it partially resets the fungal biomass state.
   Not implemented; parked in favour of first checking whether the fitness-function fix (§3)
   already resolves enough of the picture without needing this.

## 7. Widened-grid rerun results (2026-09-17): trend still wrong, not just narrow

Reran `vignettes/joint_grid_new_umax.R` to completion (all 6 `ecto_allo` blocks, 301,320 rows,
`vignettes/plots/pub_plots_all_joint_annual_new_umax.csv`). Computed steady-state (last 10 sim
years) `F_net` per run and took the true joint optimum (best root_no/root_length/ecto_allo) per
soil-N level, established runs only (`height > 5m` threshold):

| soil_nitrogen | optimal root_no |
|---|---|
| 0.15 – 0.29 | 1.7×10⁶ |
| 0.44 – 2.0  | 8.2×10⁶ (flat) |

Spearman ρ(N, root_no_opt) = **+0.61** — the optimum *increases* with N then plateaus. Widening
the grid 2.5 orders of magnitude past the old 3e6 ceiling did not reveal a hidden declining
branch; it just moved the pinned optimum to a higher plateau. §3's diagnosis (flat, not
diminishing, profitability-per-root-tip) holds at this wider range too — it wasn't a grid-
resolution artifact after all, contrary to the hope in §6.

Establishment failure (68% of 4,860 combos) is **U-shaped in root_no**, not concentrated at the
low end as §4 hypothesized:

| root_no | frac failed |
|---|---|
| 3×10³ | 100% |
| 1.5×10⁴ | 88% |
| 7×10⁴ – 1.7×10⁶ | 22–24% (viable band) |
| 8.2×10⁶ | 69% |
| 4.0×10⁷ | 88% |
| ≥1.9×10⁸ | 98–100% |

Too few roots starves the tree under the weaker uptake kinetics; too many roots is too
carbon-costly to establish. Only a mid-band survives at all. Failure also falls steadily with
soil N (100% at N=0.01 → 61% at N=2.0) but that's a secondary effect next to the root_no
U-shape. At N=0.01 nothing established, at any root_no.

**Implication:** neither §6 option (push root_no0 up, or soften the u_max correction) looks
sufficient on its own — the real blocker is the flat (non-diminishing) profitability curve from
§3, which the u_max/root_no changes don't touch. Fixing Fig. 4 properly likely needs a change to
the fitness geometry itself (something that makes root cost superlinear or transfer-benefit
sublinear in root_no), not just recalibrating the uptake/transfer constants. Not yet decided
which — this is the next real fork.

## 8. Diagnostic (2026-09-17): mycorrhized=0 rules out the transfer cap as *the* cause

Hypothesis tested: since `max_N_transfer = u_transfer * mycorrhized * SA_fr` (`src/uptake.tpp:41-42`)
is linear in root_no with **zero dependence on soil N**, and §7 showed it's the binding
constraint across most of the N range, setting `mycorrhized=0` should kill that cap entirely
(`root_interface` -> 0 -> `max_N_transfer` -> 0 -> `N_export` hard-zeroed, confirmed via smoke
test: `mycorrhizal_export_to_tree` = 0 with `mycorrhized=0` vs. 0.00064 at the default 0.9).
That forces all N through the near-field root channel (`U_root`, gated by `compute_N_bar`'s
crowding), which genuinely depends on soil N. Prediction: root_no's optimum should now decline
(or at least change sign) with N. Reran the same widened grid with `mycorrhized=0`,
`ecto_allo=0` fixed (fungal C investment is pure waste once `mycorrhized=0` — `N_export` is
zeroed regardless of surplus, confirmed in code), `root_length`×`root_no`×`soil_nitrogen`
(810 combos, `vignettes/joint_grid_myco0.R` → `vignettes/plots/pub_plots_myco0_annual.csv`).

**Hypothesis refuted.** Joint-optimal root_no (established runs, `height>5m`):

| soil_nitrogen | optimal root_no |
|---|---|
| 0.15 | 3,000 |
| 0.29 – 2.0 | 14,609 (flat) |

Spearman ρ(N, root_no_opt) = **+0.45** — still positive, still plateaus almost immediately, just
at a lower level (14.6k vs. §7's 8.2M with mycorrhizae). Removing the linear N-independent cap
didn't restore a declining trend — it just re-pinned the plateau somewhere else.

**Root cause is one level deeper than the transfer cap — it's in `compute_N_bar` itself.**
Worked the asymptotics: for large root_no (`x`), `alpha ≈ k·x` (for some geometry-dependent
constant `k`), and the quadratic in `compute_N_bar` (`src/uptake.cpp:36-42`) gives
`N̄ ≈ k_15·N_s/(k·x)` in that limit. Substituting into `U_root(x) = SA_fr·u_max·mm_near`
(`SA_fr ∝ x`) gives `U_root(x) ∝ x·N_s/(N_s + k·x)`. Solving `d(U_root)/dx = marginal_cost` for
the optimal `x*` gives **`x* ∝ N_s`** — linear in soil N, same direction, not opposite. This
specific diffusion/crowding formula structurally requires *more* root investment to reach
diminishing returns as soil gets richer, which is the reverse of the classic root-economics
expectation (root:shoot declining with fertility, Chapin 1980 et al.). It's an algebraic
property of the quadratic competition equation (`compute_N_bar`), not an artifact of the
mycorrhizal pathway — so fixing `max_N_transfer`'s N-independence (the fix floated in the
uptake.tpp:42 comment thread) would **not** have been sufficient on its own.

Establishment failure stayed U-shaped but the viable band shifted down (no mycorrhizal SA to
lean on): 53% failed at root_no=3e3, 16–23% in the 1.5e4–3.5e5 viable band, back up to 44% at
1.7M, 87–100% at ≥8.2M. N=0.01 still fails 100% regardless of root_no, same as §7.

**Implication:** the fitness-geometry fix (raised as the open question at the end of §7) needs
to target `compute_N_bar`'s own functional form, not just the mycorrhizal transfer term — the
uptake law itself lacks whatever mechanism would make richer soil require *less* root
investment to saturate, not more. Candidates not yet evaluated: a genuinely different
competition/crowding functional form (not just this quadratic), or recognizing that real
root:shoot decline with fertility may come from the *demand* side (GPP/leaf-N ceiling reached
at a lower absolute N requirement when soil is richer) rather than the *supply* side economics
modelled here — worth checking whether `optimal_leaf_nitrogen`'s N-sensitivity plays that role
and is currently swamped by the same crowding problem (ties back to `notes_Ib_leaf_N_optimization.md`).

## 9. The real answer: it's not `compute_N_bar` — it's simulation duration vs. `hmat` (2026-09-17)

Follow-up session, same day. Two false leads first, then the actual resolution.

**False lead 1 — "internal tree N" as the Fig. 4 x-axis.** Asked whether plotting root_no
against internal tree N (instead of soil N) would dodge the problem. Initial answer (circularity
— internal N is downstream of root_no) was too glib and got corrected in §9's later finding
below (internal N genuinely is the right currency, just not exogenous like soil N — swapping the
axis would still lose the link to the field literature, Helmisaari/Chapin, which is framed
around measurable soil N, so it remains the wrong axis for Fig. 4 specifically, but the
*reasoning* about internal N turned out to matter a lot).

**False lead 2 — leaf-N has no ceiling, so add one (`leaf_nitrogen_concentration` in
`plant_architecture.cpp:336` is uncapped, `= k_10 * nitrogen_tree / lm`).** Correctly killed by
Joanna: phydro's own Jmax/Vcmax optimization already has genuine diminishing returns to N built
in (`A_j(jmax)` saturates against light/Rubisco limits) — a new artificial cap would have solved
a problem that doesn't exist. Confirmed empirically: at the economic optimum found in the §7/§8
grids, `leaf_nitrogen_concentration / optimal_leaf_nitrogen` (realized vs. what phydro's own
solve would want) sits at 1.0–1.4, not the wild 10–100× the raw grid stats suggested (those
reflected clearly-suboptimal, far-too-high-root_no combos, not the actual optimum). So the
demand-side saturation mechanism was already real and already operating — the optimizer wasn't
ignoring it.

**Rigorous math on `compute_N_bar`: `x* ∝ N_s` is exact, not asymptotic.** Non-dimensionalized
the quadratic properly: in the regime that matters (large `alpha`, i.e. the root_no range where
the observed optima actually sit), the system depends on root_no (`x`) and soil N (`N_s`) *only*
through the ratio `x/N_s` — `k_15` drops out entirely in this regime. That means any cost-benefit
analysis built on top is structurally forced to put the optimum at `x* ∝ N_s`. This is why
neither the NPP-vs-GPP fix (§ below) nor anything else downstream could ever have flipped the
sign on its own — it's baked into the uptake law's functional form, confirmed rigorously, not
just observed empirically.

**Why the demand-side cap (§8 above, real and operating) still didn't produce a decline: the
target itself moves.** Checked what `optimal_leaf_nitrogen` (phydro's target) actually
correlates with: crown_area (ρ=0.90), not soil_nitrogen directly (ρ=0.24) — soil N only reaches
it indirectly, through growing a bigger tree. Since crown_area is itself an outcome of how much N
was available over the tree's life, richer soil → faster-growing tree → bigger crown_area →
bigger absolute N target → needs *more* root investment to satisfy its own now-larger appetite.
The target runs away in step with supply, so "reach a fixed target with less effort in richer
soil" never gets to bind.

**Also checked, and ruled out as a fix: NPP vs. GPP fitness accounting.** Realized `"rr"` in the
model header is root respiration and there is a *separate* `"rl"` (leaf respiration) column that
none of the F_net calculations in §7/§8 included — leaf respiration scales with Vcmax (hence
leaf N), so this looked like it could be silently hiding the "excess N wastes carbon" cost.
Reran a 45-combo quick grid with `F_net_npp = (assim_net - C_export_to_myco)/(crown_area·lai)`
(assim_net = whole-plant NPP, already nets rleaf+rroot+rstem) instead of the GPP-based proxy.
**Identical optimal root_no values came out** (rho=+0.71, same sign, same numbers) — ruled out.
Consistent with the rigorous math above: the whole system is scale-invariant in `x/N_s`, so no
downstream cost-accounting fix could break that.

**The actual resolution — Joanna's point: crown closes (hmat is a hard ceiling), so the "moving
target" story should stop once the tree gets close to it.** Checked the growth trajectory
directly in the existing §7 data: at root_no=8.2M, crown_area is *still climbing steeply* at
year 2015 (doubling every decade) and height is only ~15m against `hmat=35m` (ini:
`tests/params/p_test_boreal.ini:68`). **The 62-year simulation window never gets anywhere near
canopy closure, at any N level tested.** That's the actual reason nothing downstream (NPP
accounting, wider grids, removing mycorrhizae) ever revealed a decline — the regime where it
should appear was never reached.

Confirmed with a real test: reran 45 combos (5 N × 9 root_no, `ecto_allo=0.3` fixed to match the
productive-growth combo, `root_length=1.24`) for **300 simulated years** instead of 62
(`/tmp/.../scratchpad/longrun_test.R` → `longrun_ecto03.csv`, not yet copied into the repo).
Results:
- Height reached 66–74% of `hmat` (23–26m of 35m) at 300 years, vs. ~43% (15m) at 62 years —
  real, substantial progress toward closure.
- **First negative correlation all session: Spearman ρ(N, optimal root_no) = −0.29.**
- The mechanism shows up directly: at `root_no=1,687,024` (the height-maximizing choice),
  leaf-N overshoot (`leaf_nitrogen_concentration/optimal_leaf_nitrogen`) reaches 1.05×–4.14× as N
  rises — real waste, and that root_no is *not* the economic winner (loses to 14,609–71,141,
  which stay near overshoot≈1.0–1.07).
- `root_no=8,215,259` — the consistent winner in every §7/§8 short-run grid — is now clearly
  *worse* (`F_net_npp=-0.64`, overshoot dropping *below* 1, now under-provisioned relative to its
  own cost) than the smaller options. Direct reversal from the short-run result.
- Not yet clean/monotonic: optimal root_no bounces between 71,141 and 14,609 across N rather than
  declining smoothly (coarse 9-point log grid, and the 5 N levels reached slightly different
  fractions of hmat in the same fixed 300 years — not quite apples-to-apples across N).

**Conclusion: the mechanism is real and confirmed operating in the correct direction once
canopy closure is actually approached. `compute_N_bar` does not need to be redesigned** — its
`x*∝N_s` property is real and unavoidable for the *supply* side, but it's the demand side
(phydro's leaf-N economics, via crown_area's approach to `hmat`) that was supposed to override
it, and does, once given enough simulated time to matter. The problem all along was
under-running the simulations relative to how slowly boreal conifers approach maturity, not a
flaw in the uptake physics.

### Figures (§9 visual summary)

![Optimal root_no vs soil N, 62-year vs 700-year runs](vignettes/plots/notes_figures/fig1_optimal_root_no_vs_N.png)

*The core result.* Orange (62-year, original grid) rises with N — backwards. Green (700-year,
canopy-closure test) is pinned flat at the grid floor for every N — the mechanism now dominates
so completely it stopped discriminating between N levels. Neither line is the answer yet; the
true curve almost certainly lies at an intermediate duration/grid range not yet tested.

![F_net vs root_no at four N levels, 700-year run](vignettes/plots/notes_figures/fig2_Fnet_vs_root_no_700yr.png)

*Why the 700-year optimum sits at the grid floor.* Every curve (all four N levels) is still
climbing toward `root_no=10,000`, the smallest value tested — the true peak is below the tested
range. Note the curves separate a little (N=0.15 sits above N=2 throughout most of the range)
before all converging at the top end, where excess root_no is uniformly disastrous regardless
of N.

![Height trajectories: 62-year vs 700-year](vignettes/plots/notes_figures/fig3_height_trajectories.png)

*Why the 62-year runs never see the mechanism at all.* Left: at 62 years, height is still on its
early, near-linear climb, nowhere near `hmat=35m` — the tree hasn't finished growing, so more N
never stops paying off. Right: at 700 years, growth has visibly slowed (concave, decelerating)
and reached ~60-80% of hmat, roughly evenly across N — this is the regime where the demand-side
saturation mechanism can finally operate.

![Leaf-N overshoot vs root_no, 700-year run](vignettes/plots/notes_figures/fig4_leaf_N_overshoot_700yr.png)

*The wasted-nitrogen mechanism, directly visible.* Overshoot ratio (realized leaf N / phydro's
own `optimal_leaf_nitrogen`) sits at ~1.0 (right at target) for root_no up to ~3e5, then spikes
dramatically — up to 21x overshoot at N=2, root_no≈2M — before crashing back down at the very
highest root_no (established runs collapse there entirely). This is exactly the "extra roots
buy nothing once you've overshot the target" pattern Joanna predicted from the canopy-closure
argument, now visible directly in the data rather than just inferred from F_net rankings.

*(Figures generated by `vignettes/make_notes_figures.R`; source images in
`vignettes/plots/notes_figures/`.)*

## State of the repo (as of this session)

- **Committed** (`23c1c3c`): `u_transfer` 6e-3 → 5e-4, with derivation comment.
- **Uncommitted, in working tree**: `u_max` 0.88 → 0.048 (§4) — known to break default
  establishment at `root_no0=3e5`; the `N_bar_static` cap fix in `src/uptake.cpp` (§ near line
  118–126, capped at `N_static`) — independently defensible, unrelated to the root_no question.
  Neither blocks the §9 resolution and both can likely be committed once the longer-run grid
  (below) confirms the declining trend cleanly.
- **Reverted, no net change**: `investment_from_mycorrhiza` (tested 0.8, confirmed no effect,
  put back to 0.2).
- **Pre-existing, not mine**: `D` 0.1→0.01 and `D_static` reformatting — already in the working
  tree before this session started, untouched by me, still uncommitted.
- Archived comparison data: `vignettes/plots/prev_u_transfer_6e-3/` (old joint-grid run, for
  before/after comparison).
- Scratch scripts from today's follow-up session (not yet copied into `vignettes/`, currently
  only in `/tmp/.../scratchpad/`): `longrun_test.R` (the 300-year, 45-combo confirmation test)
  and its output `longrun_ecto03.csv`.

## Where to start tomorrow

**§9's follow-up (the 700-year, finer-grid run) over-corrected** — see the figures above.
`root_no` collapsed to the grid floor (10,000) at every N, F_net still climbing toward that
floor in every curve (Figure 2), meaning the true optimum sits below the tested range entirely.
At 700 years, root cost dominates so completely it stops discriminating between N levels at all
— not useful for Fig. 4 as is.

**Decision made at the end of this session: go back to a realistic ~60-year timescale** (matching
the Göttlicher et al. 2008 field comparison, 48–59-year-old Scots pine stands, and close to the
original default) rather than chasing the 700-year grid-floor result further. The question for
tomorrow: does a **finer root_no grid at ~60 years** (not a longer duration) reveal a real but
subtle decline that the original coarse 9-point grid (spanning 3e3–3e8, log-spaced) was simply
too sparse to resolve? This is a genuinely different test from anything run today — every grid
so far (§7, §8, §9) used coarse, wide-ranging root_no grids; none tested a *fine* grid within a
*realistic* timescale.

Concretely: rerun close to `end_year=2022` (62 years, as in the original `joint_grid_new_umax.R`)
but with root_no concentrated and finely resolved somewhere in the 1e5–1e7 range (where §7/§8's
optima actually landed), rather than the coarse log-spaced sweep from 3e3 to 3e8. Reuse
`ecto_allo=0.3`, `root_length=1.24` (the productive combo established in §9) as fixed traits to
start, to keep the comparison clean against today's figures.

The `u_max=0.048` correction (§4) and the `N_bar_static` cap fix (uncommitted in `src/uptake.cpp`)
remain independently defensible and still uncommitted — still hold them until the root_no
question is actually resolved at a realistic timescale, not just understood mechanistically.

## 10. Session 2026-09-21/22: making the model internally dynamic, and a still-open height regression

Long follow-up session. Built, tested, and mostly reverted a real feature (yearly re-optimization);
found and fixed one real bug (`ectomycorrhiza_mass` reset); diagnosed but did not fix a second,
deeper one (`root_mass`/`root_surface_area` have no construction lag); and ended by discovering
the model no longer reproduces the manuscript's own published height trajectories even in the
original static-trait mode, for reasons not fully resolved. Read the "Where to start next" section
at the bottom before doing anything else.

### 10.1 Yearly re-optimization: three implementations, in `src/life_history.cpp`/`inst/include/life_history.h`

Motivation: `root_no`/`root_length`/`ecto_allo`/`mycorrhized` are fixed for the tree's entire
simulated life (`set_root()` called once, at init) — see §6. Built a genuine per-year
re-optimization loop instead, as three successive `LifeHistoryOptimizer` methods (each kept as a
separate method, none delete the others, so old cached comparison runs stay valid):

- **`run_with_yearly_reopt` / `run_with_yearly_reopt_trajectory`**: the first version. Each year,
  trial a small fixed candidate grid (root_no x root_length x ecto_allo x mycorrhized) for one
  simulated year each, using cheap value-copy backup/restore of `P`/`C`/`rep`/`litter_pool`/
  `seeds`/`prod` (safe — confirmed no pointer/reference members anywhere in `Plant`/
  `PlantArchitecture`), score by an objective, commit to the best, log every step. Objective
  went through two iterations: raw `npp` (absolute flow — biases toward whichever candidate has
  the smallest scale) → `npp_per_ca = npp/(crown_area*lai)` (`life_history.cpp:~408`, current).
  Note: the manuscript's own static-grid objective (`net_gpp_ca`, GPP minus belowground costs
  only) has a real gap of its own — it never charges the extra leaf respiration
  (`rleaf = vcmax*par.rd`) that comes with the higher Vcmax more root-delivered N buys, which
  `npp` (nets out `rleaf+rstem+rroot` and `tleaf+troot` correctly, `assimilation.tpp:246-250`)
  does not have. Not fixed in the manuscript's own pipeline, just avoided in the new objective.
- **`run_with_local_reopt_trajectory`** ("Phase 1"): replaced the fixed global grid with a local
  pattern search — build a 3-level `{down, current, up}` set per dimension around the *current*
  committed point and trial all `3^4=81` joint combinations (not independent per-axis steps, so
  no order-dependence and cross-trait interactions are captured). root_no/root_length steps are
  multiplicative (log-space), ecto_allo/mycorrhized additive. Still applies candidates
  **instantly** (no construction lag) — this was a deliberate test to isolate whether search
  *locality* alone (vs. the wide fixed global grid) would fix the collapse-to-floor behaviour.
  It didn't: `root_no` still walked straight to `root_no_min` every single year (verified: halved
  every year for 5/5 years at N=0.01).
- **`run_with_relaxed_local_reopt_trajectory`** ("Phase 2", current best): adds root-system
  relaxation. Two method-local state variables `n_eff`/`l_eff` (NOT new `PlantArchitecture`/
  `Plant` members — `root_mass()`/`root_surface_area()` etc. stay untouched) exponentially relax
  toward whichever `(root_no, root_length)` target is committed each year:
  `tau = root_lifespan(traits)` (recomputed each substep from the *current* `l_eff`, ~1.9 years
  at `root_length≈1.24mm` with the boreal ini's `k_0/k_1/k_6/k_7`), `decay = exp(-reopt_dt/tau)`,
  `n_eff = n_target + (n_eff-n_target)*decay`, then `set_root(n_eff, l_eff, traits,
  reset_ecto_mass=false)`. Candidate **scoring** stays instantaneous ("steady-state destination"
  scoring — judges "is this a good place to be", not "how far did we get toward it this year";
  otherwise a good-but-distant destination would be unfairly penalised for not having arrived
  yet) — only the **commit** step applies real relaxation, so what's actually simulated/logged is
  physically gradual while the ranking stays fair regardless of distance. `ecto_allo`/
  `mycorrhized` are NOT relaxed (they gate a carbon flow / an uptake fraction, not a standing
  stock — confirmed no separate timing bug there). Logs `get_state()` columns + trailing
  `ecto_allo, mycorrhized, root_no_target, root_length_target` (the last two let you see the
  relaxation lag directly against the `root_no`/`root_length` columns, which reflect `n_eff`/
  `l_eff`).

Result: Phase 2 measurably helps (real reversals/plateaus appear on ~15yr windows, e.g. N=1.1
holding `root_no≈171588` flat for 4 straight years — a genuine local optimum found and held) but
does **not** fully fix the collapse over a full 62yr run: `root_no` still ends up pinned at
`root_no_min` at all three tested N levels (0.15/0.20/2.00) by ~1985-2000, just via a smoother
path than the unrelaxed version. See §10.3 for why the relaxation doesn't fully solve it.

### 10.2 The `ectomycorrhiza_mass` reset bug — found and FIXED

`PlantArchitecture::set_root()` (`plant_architecture.cpp`) used to unconditionally reset
`ectomycorrhiza_mass = root_mass(traits)` on every call. Fine for genuine initialisation (no
history to discard) but wrong for a later trait update once real growth has accumulated actual
mycorrhizal biomass — every yearly re-optimization commit was discarding 62-78% of standing ECM
biomass, confirmed by direct measurement (a small illustrative run showed `ecto_before`/
`ecto_after` differing by that much at every year boundary). Fix: `set_root()` now takes
`bool reset_ecto_mass = true` (default preserves existing behaviour at the one legitimate call
site, `LifeHistoryOptimizer::init()`); the yearly-reopt commit steps pass `false`. Verified fixed:
`ecto_before == ecto_after` exactly, trajectory now smooth/monotonic instead of sawtoothed.

### 10.3 The `root_mass`/`root_surface_area` instantaneous-function problem — diagnosed, NOT fixed

Unlike `ectomycorrhiza_mass`, plain `root_mass()`/`root_surface_area()` (`plant_architecture.cpp`)
have **no state variable at all** — always recomputed instantaneously from current
`root_no`/`root_length`/`crown_area`/`lai`. `rroot`/`troot` (root respiration/turnover costs,
`assimilation.cpp`) are driven directly by `root_mass()`, so they're always instantaneous; the
*benefit* of more root_no flows through `tree_nitrogen` (`plant.tpp`'s RK4-integrated state,
confirmed genuinely slow-accumulating), so even Phase 2's "steady-state" scoring (which makes
`root_mass`/`root_surface_area` instant for trial purposes) doesn't capture the true multi-year
benefit of a candidate — a single 1-year trial only sees one year of `tree_nitrogen` accumulation
toward whatever equilibrium that root system would eventually support, while the cost is fully
realised immediately. This is the leading candidate for why `root_no` still collapses under
Phase 2. Not implemented: either a longer trial horizon (several years per candidate, ~4-5x
runtime) or promoting `root_mass` to a real state variable the way `ectomycorrhiza_mass` already
is (bigger change, mirrors §10.2's fix).

**Important counter-finding, though (2026-09-22 diagnostic, N=0.2, year 1968, 1-year sweep):**
directly swept `root_no` from 1e4 to 1e7 at a fixed representative state and found
`leaf_nitrogen_concentration` exactly equals `optimal_leaf_nitrogen` (phydro's own target) for
EVERY root_no from 1e4 through 3e5 — N demand is already fully met regardless of root
investment in that whole range; only above ~1e6 does the tree start over-accumulating N it can't
use. So **shedding "unnecessary" root investment down to whatever the current N demand needs is
economically correct, not a bug**, at least at this stage of stand development. The real open
question (not yet tested) is whether the model correctly *rebuilds* root investment later, once
absolute N demand grows enough (bigger tree) that the same root_no stops being sufficient — if
the search is reluctant to climb back up for the same myopic reason it was reluctant before
(mirrored in reverse), that would directly explain the observed late-stage height stagnation.
Next diagnostic to run: repeat the same 1-year sweep at a much later state (e.g. year ~2000-2010
in a run, once the tree is bigger) and check whether a *higher* root_no would actually pay off
there, and whether the dynamic run is failing to find that.

### 10.4 Self-thinning canopy formula: built, tested twice, reverted both times — NOT active

Early in this session, replaced the original time-based background-canopy closure formula
(`ErgodicEnvironment`, `bg_density_ini/eq/tau/t0`, pure function of `t - bg_t0`) with a
height-based self-thinning formula fit to real SMEAR II Hyytiala pine census data (`density =
bg_selfthin_A * height^bg_selfthin_b`, capped at `bg_canopy_density_cap` for young stands; also
fixed two real bugs found along the way — a NaN-unsafe height guard, and `updateBackgroundCanopy`
being called before `P` was reset/sized in `init()`, both now only relevant if this is
re-attempted). Confirmed mechanically working (diagnostic plot: actual root_no smoothly lagging
target) but broke the boreal calibration badly when combined with the *old* objective/grid search
(height stalled ~4.9m at N=0.2/0.15, `ecto_allo`/`mycorrhized` collapsed to zero ECM biomass by
1980). User asked to revert to the original time-based formula and focus on the root_no dynamics
instead — **fully reverted**, confirmed via `grep` for `bg_selfthin`/`self-thinning` across all
three files (life_history.h, life_history.cpp, p_test_boreal.ini) returning zero matches. The
self-thinning idea itself was never disproven under the *current* (Phase 2 + npp_per_ca) setup —
only under the old one — so it's a legitimate thing to re-try later once the more fundamental
root_mass/height issues are sorted out, not a dead end.

### 10.5 Unexplained: `ppfd` doesn't match the raw weather file, worse under yearly-reopt

While investigating a GPP crash (`assim_gross` falling ~24x from a 1963 peak while crown_area and
height kept growing, at N=0.2), found the model's own logged `ppfd` column reading ~14-90 for
1966-69 while the raw `data/ERAS_Monthly.csv` JJA-mean for those exact months is a healthy
~276-541 throughout. Confirmed NOT a climate-stream statefulness bug — `flare::Stream::
advance_to_time()` (`external/flare/include/stream.h:201`) is a pure, stateless lookup via binary
search on a static sorted array; rewinding time and re-querying is provably safe. But: a plain
sequential run (no reopt, fixed root_no=3e5) still shows a real discrepancy (~174-220 vs the raw
~276-541), and the yearly-reopt mechanism makes it *worse* (~14-90) for the same years, by a
mechanism not yet identified — candidate-trial repeated calls to `update_climate`/`grow_for_dt`
for the same in-year `t` range shouldn's cause this given the stream is stateless, but something
does. **Not resolved.** Next step: direct instrumentation of exactly what julian-day/`t` is being
requested at each step (not indirect JJA-averaging comparisons, which could hide a labelling
off-by-one) to pin down whether this is a genuine model bug or a diagnostic-script artifact in
how `t`/month get mapped in the ad hoc R scripts used this session.

### 10.6 Audit: what's changed since `MAIN.pdf` was last generated (2026-09-16 ~11:53, commit `23c1c3c`)

No commits have landed since `23c1c3c` (still HEAD). Systematically compared everything
uncommitted against that point:

- **All of `inst/include/life_history.h`, `src/life_history.cpp`, `inst/include/
  plant_architecture.h`, `src/plant_architecture.cpp`, `src/r_interface.cpp`**: new this session,
  purely additive (new methods, `reset_ecto_mass` parameter with a backward-compatible default).
  Confirmed these don't affect the original static-trait code path at all.
- **`u_max`**: was `0.048` at session start, reverted to `0.88`. Tested both in fixed-trait
  replication (see below) — **made almost no difference** (LowN 10.92 vs 11.25m, HighN identical
  19.27m both times) — ruled out as the discriminating factor. Left at `0.88` (better documented).
- **`src/uptake.cpp`** (`N_bar_static` cap, `min(k_15*k_SA/SA_m, N_static)`) and **`D`**
  (0.1→0.01 at session start): both reverted to match `HEAD`. This genuinely helped: fixed-trait
  LowN height went 8.35m → 11.25m, HighN 18.02m → 19.27m. **Currently reverted (matches HEAD).**
- **`D_static`**: diff was pure whitespace, no functional change — ignore.

**Decisive test performed**: fixed-trait replication (no yearly-reopt at all — exactly the
static-grid manuscript methodology) using the manuscript's own reported lifetime-optimal
combo (`\OptRootNoLowNMath=3e6`, `\OptRootLengthLowN=2.43`/`\OptRootLengthHighN=3.62`,
`\OptEctoLowN=\OptEctoHighN=24%`, from `dynamic_values.tex`), for the full 62 years:

| config | LowN (0.15) final height | HighN (2.00) final height |
|---|---|---|
| Manuscript (reported) | ~16m | ~23m |
| u_max=0.048 (session start state) | 8.35m | 18.02m |
| + revert `N_bar_static` cap, `D`→0.1 | 11.25m | 19.27m |
| u_max=0.88 vs 0.048 (with above reverts) | 10.92 vs 11.25m | 19.27m both |

**Still unresolved**: even with everything uncommitted reverted to match `HEAD` exactly, fixed-trait
replication still falls ~5m short on LowN and ~4m short on HighN. Since there are no commits since
`23c1c3c` and no more known uncommitted differences, either (a) the manuscript figures were
actually generated from an even earlier state than `23c1c3c` (the grid CSVs'
`pub_plots_all_joint_annual_new_umax.csv` partial files date to 2026-09-16 11:15-11:19, *before*
`23c1c3c`'s 11:24:27 commit — so the u_transfer change might postdate the manuscript's own grid
run too, not just u_max/N_bar_static/D), or (b) there's a methodology difference between the
plain `grow_for_dt` loop used in this session's replication test and whatever the original
`joint_grid_new_umax.R`/`Publication Plots.Rmd` pipeline actually did (dt, warm-up, or some other
setup difference not yet identified).

## State of the repo (as of 2026-09-22, this session)

**Uncommitted, currently active, matches HEAD (`23c1c3c`) exactly except for comments/whitespace:**
- `tests/params/p_test_boreal.ini`: `u_max=0.88`, `D=0.1`, `D_static` (whitespace only) — all
  reverted to match `HEAD`.
- `src/uptake.cpp`: `N_bar_static` cap reverted (back to uncapped `k_15*k_SA/SA_m`).

**Uncommitted, new this session, purely additive (safe, don't touch the original static-trait
path), NOT yet committed:**
- `inst/include/life_history.h` / `src/life_history.cpp`: `run_with_yearly_reopt[_trajectory]`,
  `run_with_local_reopt_trajectory` (Phase 1), `run_with_relaxed_local_reopt_trajectory`
  (Phase 2, current best), `npp_per_ca` helper.
- `inst/include/plant_architecture.h` / `src/plant_architecture.cpp`: `set_root(...,
  reset_ecto_mass=true)` — the §10.2 fix, backward-compatible default.
- `src/r_interface.cpp`: bindings for the three new methods.
- `manuscript/oup-authoring-template/MAIN.tex`: new `\subsection{Making the model internally
  dynamic}`, last subsection of Results, documenting §10.1-10.3 as a manuscript-facing criticism
  (written before the §10.6 height regression was discovered — may need a follow-up sentence once
  that's resolved, since it currently doesn't mention the fixed-trait replication also falling
  short of the paper's own reported numbers).
- **Canopy formula**: self-thinning work fully reverted (§10.4) — currently the *original*
  time-based `bg_canopy_density_ini/eq/tau/t0` formula, unchanged from `HEAD`.
- Various new `vignettes/*.R` scripts and cached CSVs/PNGs under `vignettes/plots/dynamic/` —
  scratch/diagnostic, not manuscript-facing. `plots/dynamic/phase2_relaxed/` has the most complete
  current diagnostic set (fig3/4/5-style plots across N=0.15/0.20/2.00, a root_no actual-vs-target
  diagnostic, a boreal-calibration comparison, and a starting-root_no0 sensitivity check).
- **Caution**: running `vignettes/Publication Plots.Rmd`'s full pipeline (or the purled
  `pub_plots_boreal_calib.R`-style scripts used this session) will silently overwrite the
  manuscript's actual `Figures/*.png` files (confirmed this already happened once, 2026-09-21,
  when checking the self-thinning canopy fix) — the originals are still embedded inside the
  existing `MAIN.pdf` (PDFs embed images at compile time) but no longer recoverable from the PNG
  files on disk. Worth extracting/backing up the original PNGs from `MAIN.pdf` before running that
  pipeline again, if the pre-session originals are needed for comparison.

## 11. Session 2026-09-23: `u_transfer` re-derivation (§1, `23c1c3c`) is the cause of the §10.6 calibration regression — reverted

Resolved §10.6's open question by testing the actual named "Boreal Calibration" setup (`run_lh_sim(0.2)`
in `Publication Plots.Rmd`/`lh_09_main_panel` — default traits, `root_no0=3e5`, N=0.2, 1960-2022,
compared against SMEAR II height/diameter and eddy-covariance GPP), not the N-gradient joint-optima
figure §10.6 had been chasing. Ran it at the current committed state (`u_transfer=5e-4`, everything else
matching `HEAD`):

| | Height 2022 | Diameter 2022 | Mean GPP (kg C m⁻² yr⁻¹) |
|---|---|---|---|
| `u_transfer=5e-4` (current committed) | 11.2m | 0.11m | 0.70 |
| `u_transfer=6e-3` (pre-`23c1c3c`) | 18.25m | 0.21m | 1.14 |
| Observed (SMEAR II / eddy covariance) | 17.5–20.8m | 0.20–0.23m | 1.14 |

Reverting `u_transfer` alone (nothing else changed) takes height/diameter from badly-undershooting to
squarely inside the observed range, and GPP from 39% low to an almost exact match (1.142 vs 1.144).
This is a clean, decisive result — `u_transfer` was the single cause of the §10.6 replication shortfall,
not a methodology mismatch in a lost scratch script as §10.6 speculated. (NPP now *overshoots* -NEE,
0.466 vs 0.204, but that's expected: -NEE ≈ NPP − heterotrophic respiration, so -NEE running below NPP
isn't itself evidence of a problem.)

**Caveat, not resolved:** `u_transfer=6e-3` restores calibration but is sourced from a Plassard & Dell
(2010) per-tip conversion that measures soil-uptake flux at the EM mantle, not Hartig-net fungus-to-host
transfer (§1's own objection to it) — so this is empirically right, mechanistically suspect. No better
literature value has been found. **Decision (Joanna, 2026-09-23): revert to 6e-3 and flag the sourcing
gap in the ini comment rather than keep the mechanistically-cleaner-but-uncalibrated 5e-4.** Committed
to `tests/params/p_test_boreal.ini` only (not v2/v3 inis, per usual practice). Finding a real Hartig-net
transfer-rate citation for boreal/Scots pine ECM remains open.

This also means §10.6's "fixed-trait replication still falls ~5m/~4m short even reverted to HEAD" result
needs re-reading: that test used `root_no=3e6` (the N-gradient joint-optimum trait), not the calibration's
`root_no0=3e5`, so it's a different comparison and wasn't retested here — worth rechecking once picking
the §7/§8/§9 root_no-vs-N thread back up, since the u_transfer fix likely changes those results too.

## 12. Session 2026-09-23 (cont.): re-ran Phase 2 dynamic reopt post-`u_transfer` fix — no more collapse-to-floor

Reran `run_with_relaxed_local_reopt_trajectory` (Phase 2, §10.1) at N=0.15/0.20/2.00, 1960-2022,
same candidate-grid shape as before, now with `u_transfer=6e-3` (§11's fix) instead of the broken
5e-4. Script: `vignettes/run_phase2_relaxed_reopt.R`; plots: `vignettes/plot_phase2_refixed.R` ->
`vignettes/plots/dynamic/relaxed_local_key_trajectories_refixed.png` /
`relaxed_local_root_no_diagnostic_refixed.png`; raw: `vignettes/plots/dynamic/
relaxed_local_N_{0.15,0.20,2.00}_refixed.csv`.

**Result: the §10.1/10.3 collapse-to-`root_no_min` failure is gone.** Previously all three N levels
walked `root_no` straight to the grid floor by ~1985-2000. Now:
- N=0.15, N=0.20: `root_no` fluctuates in a healthy 60k-380k range for the full 62 years, never
  approaching the 1e3 floor. Height trajectories (14.0m / 15.6m by 2018) track close to the
  static-trait calibration run's own trajectory (§11: 17.8m by 2022 at N=0.2) — same order, no
  longer pathological.
- N=2.00: `root_no` does decline substantially (down to ~6,000-50,000 by the 2000s) but settles at
  real interior values, not pinned at the floor. Accompanied by `ecto_allo`->0 and `mycorrhized`->
  ~0-3% by the end of the run (Fig. panels c/d) — the tree essentially abandons the mycorrhizal
  partnership once direct root uptake alone is sufficient at high soil N. This is the first time in
  the whole investigation that `root_no` has shown a genuine decline at the high-N end rather than
  pinning at a grid extreme (contrast §7's ρ=+0.61 "backwards" result). Not independently verified
  yet whether the ~0% mycorrhization is a real economic optimum or an artifact of the 1-year trial
  horizon favouring instant carbon savings over a longer-horizon benefit (flagged, not chased
  further this session -- next thing to check if this thread is picked back up).

**Caveat, not investigated further this session:** panels (c)/(e)/(f)/(g) show a synchronized
crash/whipsaw across *all three* N levels around 1966-1985 (GPP per crown area drops to near-zero
twice, ECM allocation and accessible N swing erratically) before all three settle into the smoother
declining trends described above. This lines up suspiciously with §10.5's still-unresolved finding
that logged `ppfd` doesn't match the raw weather file and gets *worse* specifically under the
yearly-reopt mechanism -- worth checking whether this is the same artifact resurfacing before
trusting the early-record (pre-1985) portion of these trajectories.

## 13. Session 2026-09-23 (cont.): redid the full `phase2_relaxed/` plot set — found and fixed a real `N_bar_static` divergence bug

Regenerated every plot that used to live in `vignettes/plots/dynamic/phase2_relaxed/` (fig3
annual trajectories, fig4 cross-section vs. accessible N, fig5 C:N exchange cost, the actual-
vs-target root_no diagnostic, the key-trajectories figure, and the starting-root_no0
sensitivity), using the post-§11 `u_transfer=6e-3` Phase 2 data. New scripts:
`vignettes/run_phase2_relaxed_reopt.R`, `vignettes/plot_phase2_relaxed_full_set.R`,
`vignettes/run_phase2_sensitivity_root_no0.R`, `vignettes/plot_phase2_sensitivity_root_no0.R`.
Output: `vignettes/plots/dynamic/phase2_relaxed_refixed/` (old `phase2_relaxed/` originals kept
untouched for before/after comparison).

**Found a real, pre-existing bug while building fig4/fig5** (unrelated to the u_transfer fix,
just newly exposed by this session's mycorrhized->0 result at high N, §12): in
`src/uptake.cpp`, `N_bar_static`'s guard against SA_m=0 (`if (SA_m >= 1e-9)`) only blocks
literal 0/0 division. As `ectomycorrhiza_mass` (and therefore SA_m) decays toward but not
exactly to zero -- e.g. ~1e-12 kg under the dynamic reopt's mycorrhized->0 trajectory at
N=2.00 -- `N_bar_static = k_15*k_SA/SA_m` still diverges to physically absurd values (up to
1.6e7 kg N/m^3 seen in year 2018 of the N=2.00 run, vs. a true ceiling of `N_static`,
~0.85-1.03 kg N/m^3). Confirmed this is a pure diagnostic/logging artifact: `N_bar_static`
(the guarded value) is never used in the actual `U_myco_static` uptake calculation, which
uses a separately well-behaved `denom = SA_m_eff + k_SA` formula -- so height/GPP/root_no/
mycorrhized trajectories from §12 are NOT affected, only the "accessible N" diagnostic
column/plots were corrupted.

**Fix (committed-pending):** capped `N_bar_static` at its physical ceiling, `N_static`:
```
N_bar_static = std::min(k_15 * k_SA / SA_m, N_static);
```
After the fix + rebuild + rerun, max accessible N in the N=2.00 run dropped from 1.6e7 to 1.7
kg N/m^3 -- physically sane. fig4/fig5 are now readable (previously all variation was
compressed into a corner by a handful of extreme outliers). Note: there's earlier history here
-- a "cap N_bar_static at N_static" fix was tried and explicitly reverted during §10.6's audit
(to isolate variables, not because it was wrong) -- this session's finding suggests that cap
was the right call all along, just for a different reason (extreme-low-SA_m divergence) than
originally motivated.

## 14. Session 2026-09-23 (cont.): constant-climate sandbox confirms root_no dynamics aren't a climate artifact

Per Joanna's request, reran the starting-root_no0 sensitivity (§13) with a synthetic weather
file (`data/ERAS_Monthly_constant1960.csv`, 1960's 12 months tiled across 1960-1980, so every
year sees *identical* climate) instead of the real 1960-2022 ERA5 series, specifically to check
whether the root_no trajectories seen under real climate are climate-driven or intrinsic to the
reopt/growth dynamics. Script: `vignettes/run_phase2_sensitivity_root_no0_constclimate.R` ->
`vignettes/plots/dynamic/phase2_relaxed_refixed/sensitivity_starting_root_no0_constant_climate.png`
(kept alongside, not instead of, the real-climate version per Joanna's request -- both are useful).

**Result, clean and informative:**
- At N=0.15 and N=0.20, `root_no` (all three starting points) converges to an *exactly flat*
  plateau by ~1965 and sits there dead flat for the remaining 15 years -- no wiggle at all. This
  confirms the real-climate run's year-to-year fluctuation in this range (§12/§13) is a genuine
  climate-driven effect (different years' weather shifting the committed optimum), not an
  independent internal oscillation in the search/relaxation machinery -- under truly constant
  forcing, the system finds a fixed point and stays there.
- At N=2.00, `root_no` keeps declining even under constant climate (not flat) -- confirming the
  high-N decline trend identified in §12 is driven by the tree's own growth/maturation (crown
  area, size, N demand all still changing year-on-year even with identical weather), not by
  climate noise. This reinforces that the "tree abandons mycorrhization once big enough to meet
  N demand from direct uptake alone" mechanism (§12) is a real structural/maturation effect.
- Height trajectories remain essentially insensitive to starting root_no0 under constant climate
  too, matching the real-climate result.

## 15. Open item flagged 2026-09-23: dynamic (Phase 2) boreal-calibration N=0.20 undershoots height, unlike the static calibration

Joanna's observation, not yet investigated: in the Phase 2 (`run_with_relaxed_local_reopt_trajectory`)
runs (§12/§13), the boreal-calibration soil N level (N=0.20) should reach a height around ~20m by
the end of the 62-year run (matching SMEAR II, ~17.5-20.8m — the same target the *static*-trait
calibration run hits almost exactly post-§11's `u_transfer` fix, 17.8m). But under the dynamic
reopt, N=0.20 only reaches ~15.6m by 2018 (`phase2_timebased_key_trajectories.png` panel a) — that
~20m height is instead being reached by the N=2.00 (High) dynamic run, not N=0.20. So the dynamic
model's calibrated-N trajectory and the static model's calibrated-N trajectory disagree with each
other, even though both are nominally run at the same N=0.20. **Not yet investigated why** — next
session should check this before trusting the Phase 2 dynamic trajectories as calibration-consistent
with the static model. Candidate causes to check: the dynamic reopt's starting point/traits differ
from the static calibration's fixed `root_no0=3e5, root_length0=2.0` (Phase 2 runs so far started
from `root_override(3e5, 1.24)` — root_length0 differs, 1.24 vs the ini's 2.0); the reopt's
`npp_per_ca` objective may systematically under-invest relative to what the static fixed-trait
combo achieves; or the yearly search/relaxation dynamics themselves suppress growth relative to a
fixed, already-good static trait combination.

## 16. Session 2026-09-23 (cont.): dead-code cleanup, mycorrhized_max cap noted (not yet rerun)

Removed the two superseded yearly-reopt methods (`run_with_yearly_reopt[_trajectory]`, global
grid, no relaxation) and Phase 1 (`run_with_local_reopt_trajectory`, local search, no relaxation)
from `inst/include/life_history.h`, `src/life_history.cpp`, and their R bindings in
`src/r_interface.cpp` -- per Joanna, only Phase 2 (`run_with_relaxed_local_reopt_trajectory`,
local search + root-system relaxation) is actually wanted going forward; the other two were dead
code once Phase 2 superseded them. `npp_per_ca` (shared helper) and `set_root(...,
reset_ecto_mass=...)` (plant_architecture) are still needed by Phase 2 and were kept as-is.
Rebuilt clean, no errors.

Also noticed (while fixing the ECM-colonisation plot axis to a true 0-100% range, see below): the
Low/boreal-N Phase 2 runs (§12/§13) sit pinned flat at ~95% ECM colonisation -- that's the
`mycorrhized_max=0.95` search-grid ceiling used in `run_phase2_relaxed_reopt.R` and the two
sensitivity-sweep scripts, an arbitrary choice (not a physical or ini constraint -- the model has
no hard cap on `mycorrhized`), not an economic optimum the search actually found. Raised
`mycorrhized_max` to `1.0` in all three run scripts so future reruns aren't artificially capped.
**Not yet rerun** -- the existing cached CSVs/figures (§12/§13/§14/§15) still reflect the old 0.95
cap; whether Low/boreal-N colonisation actually wants to go all the way to 100% once uncapped is
still an open question for next session.

## 17. Session 2026-09-23 (cont.): resolved §15 — dynamic N=0.20 undershoots height because `npp_per_ca` has no interior optimum for ecto_allo, not a bug in roots/canopy/climate

Chased the §15 question (why does dynamic N=0.20 reach only ~15.6m when the static calibration
reaches ~17.8-18.25m, when both are nominally the same N?) all the way to ground. Ruled out, with
direct evidence, in order:

1. **Root-system relaxation / search-locality itself**: not implicated -- not retested directly
   this session, but nothing below points at it.
2. **Canopy self-shading (`ErgodicEnvironment::updateBackgroundCanopy`)**: checked directly via
   `lho$env$canopy_openness`/`z_star` at several years -- `canopy_openness` is `1.0` (fully open,
   zero shading) for essentially the entire 62-year static run (`n_layers=0` until 2020). Not the
   cause of anything here; the earlier "self-referential crown_area shading" hypothesis (bigger
   `crown_area` -> more computed background layers -> more self-shading, since
   `PlantArchitecture::crown_area_extent_projected(0, traits)` literally returns the focal tree's
   own `crown_area`, confirmed in `plant_architecture.cpp:61-66`) is real and worth remembering as
   a structural property of this "ergodic" single-tree approximation, but it isn't what's driving
   this particular discrepancy, since `n_layers=0` (no shading at all) almost throughout.
3. **Weather file `Decimal_year` drift (major finding, separate bug)**: `data/ERAS_Monthly.csv`'s
   `Decimal_year` column drifts from true calendar time by up to **-0.92 years by 2022** (linear,
   ~-0.0146/yr, consistent with the file having been generated assuming 360-day years instead of
   365.25 -- 360/365.25=0.9856, matching the drift rate almost exactly). Since `ClimateStream`
   looks up weather by `Decimal_year`, this causes simulated "July" queries to increasingly
   retrieve wrong-season (often winter) records as a run progresses -- directly reproduced: a
   fixed-trait run's logged `ppfd` in July swings 298->159->86->26->**5.5**->19->168->281->495
   across 1965-2020 despite `canopy_openness=1` throughout (i.e., no shading involved -- this is
   pure wrong-row retrieval). **This is a real, serious, separate bug affecting every simulation in
   the repo that uses this weather file**, static or dynamic, and is very likely the actual
   explanation for the synchronized 1966-1985 GPP crash noted in §12 and the original §10.5
   mystery ("ppfd doesn't match the raw weather file"). **Not yet fixed, per Joanna's instruction
   this session** ("just note it, keep digging on height") -- flagged here as a priority independent
   fix. **Crucially, ruled out as the static-vs-dynamic differentiator**: directly tested whether
   `ClimateStream`/`flare::Stream`'s lookup is order-independent (queried the same t-range 5x
   non-monotonically, as the reopt's candidate trials do, before continuing) -- identical result to
   a single monotonic pass. So both runs see *exactly* the same (buggy) climate at matching `t`;
   this drift cannot explain why they diverge from *each other*, only why both diverge from reality.
4. **Starting root_length0 mismatch** (Phase 2 scripts use `root_override(3e5, 1.24)`, vs. the
   ini's `root_length0=2.0`): tested in isolation (static run, ini defaults except
   `root_length0=1.24`) -- costs only ~0.75m (18.25m -> 17.50m). Real, but far too small to explain
   the ~2.5-2.8m gap on its own.
5. **`ecto_allo` (carbon fraction to mycorrhizae) -- the actual answer.** The dynamic Phase 2 run's
   `ecto_allo` trajectory sits pinned at ~0.25-0.30 most years (vs. the calibration's fixed 0.2).
   Tested in isolation: static run, ini defaults except `ecto_allo=0.3` -> **15.79m** (a 2.46m
   drop, almost the *entire* observed gap on its own). Combined with the root_length0 starting
   point (`ecto_allo=0.3` + `root_length0=1.24`) -> **15.61m**, matching the real dynamic run's
   trajectory (~15.0-15.6m) almost exactly. This is a clean, complete, reproduced explanation.

**Why `ecto_allo` sits at 0.3: it's pinned at the search's arbitrary `ecto_max=0.3` ceiling, and
raising the ceiling makes it worse, not better.** Tested directly (Joanna's request): reran N=0.20
with `ecto_max` raised to `1.0` (`vignettes/scratchpad` diagnostic, not saved in repo -- rerun from
this description if needed). Result: `ecto_allo` climbs **monotonically, one search-step (+0.05)
per year, with no interior optimum**, from 0.25 in 1960 all the way to **1.00 by 1978** and stays
pinned at the true mathematical ceiling from then on -- while height *flatlines* at 5.96m from
1978 onward (the tree retains zero carbon for its own structure once `ecto_allo=1.0`, so it simply
stops growing). This is not a mild bias toward over-investment; **the `npp_per_ca` objective
(NPP / (crown_area*lai)) has no interior optimum for `ecto_allo` at all** -- it's a per-unit-leaf-
area efficiency metric, and the search can always inflate that ratio by diverting more carbon to
fungi rather than growing crown area, since a stalled/shrinking denominator combined with any
residual carbon still scores as "efficient," all the way to total carbon starvation. The
`ecto_max=0.3` cap wasn't holding back a legitimate optimum -- it was accidentally masking a
genuinely degenerate objective function.

**Implication, not yet acted on:** this calls into question the `ecto_allo` (and, since ecto_allo
directly competes with root investment for the same NPP pool, quite possibly also the `root_no`)
trajectories from every Phase 2 run generated this session (§12-§16), not just the N=0.20
calibration comparison -- since they all use the same `npp_per_ca` trial-scoring objective. The
`root_no` "no longer collapses to the floor" result (§12) may still be directionally real (it was
also confirmed under constant climate, §14, ruling out pure climate-noise as the cause), but the
*absolute* trait values chosen, especially `ecto_allo`, should not be trusted until the objective
is fixed. Two live options for next session: (a) revert `ecto_max` toward something closer to the
ini's own 0.2 (the ini comment describes 0.2 as itself sourced as a literature "maximum," within
CASSIA bounds, Ryhti 2022 -- so 0.3 may already have been pushing past a literature-defensible
ceiling even before this test), purely as a stopgap; (b) the real fix -- switch the trial-scoring
objective from `npp_per_ca` to something that tracks the manuscript's actual stated fitness
criterion (tree height, or `F_net` as defined in MAIN.tex's Tree Fitness section) instead of a
per-leaf-area efficiency ratio that has no reason to prefer bigger trees over "thinner but more
efficient" ones.

## 18. Session 2026-09-23 (cont.): fixed the ecto_allo accounting bug (committed-pending) -- but it exposed a deeper, still-unresolved problem

Per Joanna's choice ("corrected NPP-per-area"), fixed `npp_per_ca` (`src/life_history.cpp`) to
subtract `P.geometry.C_export_to_myco` from `P.assimilator.plant_assim.npp` before dividing by
crown_area*lai -- confirmed via `plant.tpp:169-186` that `plant_assim.npp` is computed *before* the
mycorrhizal export is deducted (the tree's own growth actually uses `npp - npp_exudates`), so the
old scoring genuinely never charged ecto_allo's carbon cost at all. Rebuilt; re-ran the same
ecto_max=1.0 diagnostic from §17: **confirmed fixed** -- `ecto_allo` now drops to exactly 0 by 1967
and stays there (opposite extreme from the old runaway-to-1.0), `mycorrhized` declines in step
(0.90->0.25 by 20 years), and height at 20 years is 7.71m vs. the old broken-objective/uncapped
result's 5.96m. The "free lunch" is genuinely gone.

**But the full 62-year N=0.20 run with the fixed objective is WORSE overall, not better:**

| | Height (end of run) | root_no (end) |
|---|---|---|
| Static calibration | 18.25m | 300,000 (fixed) |
| Dynamic, old broken objective (§12, ecto_max=0.3) | ~15.6m | 130k-200k |
| **Dynamic, fixed objective (this section)** | **11.32m** | **2,679** (collapsed near the 1e3 floor) |

With ecto_allo correctly costed, it (and `mycorrhized`) correctly collapse toward 0 -- but `root_no`
*also* now collapses hard, all the way from 247,501 (1960) down to 2,679 by 1990 and stays pinned
there. **This is the §10.1/§10.3 root_no-collapse-to-floor failure mode, back in force.** It had
looked resolved once the `u_transfer` calibration bug was fixed (§12) -- but that was a false
resolution: the OLD broken `npp_per_ca` was over-rewarding `ecto_allo`, and the resulting
(illegitimate, cost-free) extra nitrogen supply was propping up growth enough that the search
didn't need real root investment either. Two errors were cancelling. Fixing one exposed the other.

**Leading hypothesis, consistent with §10.3's original suspicion:** the search scores each
candidate on a *single* simulated trial year, judged "as if already at steady state" (see
`run_with_relaxed_local_reopt_trajectory`'s trial-scoring comment in `inst/include/life_history.h`).
Any investment that only pays off gradually -- `root_no`/`root_length` (via `root_surface_area()`
-> N uptake -> the genuinely slow-accumulating `tree_nitrogen` state -> leaf N -> Vcmax/GPP,
already flagged in §10.3) and now clearly also `ecto_allo`/`ectomycorrhiza_mass` (a real,
gradually-accumulating state variable, `dmyco_dt`) -- pays its full carbon cost *immediately* in
the trial year, but the trial can't see the benefit maturing beyond that single year. A correctly-
costed but still single-year-horizon search should be expected to systematically undervalue *any*
slow-building belowground investment, which is exactly what's observed now that nothing is left
artificially propping root_no up.

**Not yet fixed.** Two live options, neither attempted yet: (a) lengthen the trial horizon (score
each candidate over several simulated years, not one -- directly costed in runtime, ~n_years-fold
more expensive per year of search); (b) promote root_mass/root_surface_area to genuine state
variables with their own construction lag (mirroring the §10.2 fix already applied to
ectomycorrhiza_mass), so a trial's *cost* is also amortised gradually rather than paid in full
immediately -- a bigger structural change. Paused here, per Joanna, to decide direction before
proceeding further.

**Status of the fix itself:** the `npp_per_ca` C_export_to_myco fix (`src/life_history.cpp`) is
correct and should be kept/committed regardless of how the deeper trial-horizon issue is resolved
-- it fixes a real, unambiguous accounting bug. It did not cause the root_no collapse; it removed
a masking cancellation and made a pre-existing, deeper problem visible.

**Visual confirmation, `vignettes/plot_fixed_objective_comparison_N020.R` ->
`vignettes/plots/dynamic/phase2_relaxed_refixed/fixed_objective_comparison_N020.png`** (static vs.
old-objective dynamic vs. fixed-objective dynamic, N=0.20, 8 panels). Caught something the summary
numbers above missed: **`root_length` also gets driven to its own search ceiling** (6mm, hit by
1975 and pinned there) under the fixed objective, on top of `root_no` collapsing and
`ecto_allo`/`mycorrhized` correctly going to 0. With the ecto_allo/mycorrhized escape valves
closed off, the search found a third lever (root length) and rode that to its ceiling instead of
settling at a real interior optimum -- further evidence this is a structural single-year-horizon
problem, not something specific to `ecto_allo` alone: whatever trait dimension remains gameable
gets pinned to whatever cap is set on it.

## 19. Session 2026-09-23 (cont.): implemented and validated the receding-horizon trial fix

Per Joanna's direction (and her sharp point that scoring by height wouldn't have fixed anything,
since height in a single trial year is just a delayed proxy for that year's NPP -- the real issue
is the trial *horizon*, not which flow gets scored), implemented a receding/rolling horizon for
`run_with_relaxed_local_reopt_trajectory` (Model Predictive Control-style): added a
`trial_horizon_years` parameter: each of the 81 joint candidates is now trialled for
`trial_horizon_years` simulated years (not just one) before scoring, but still only ONE real year
is ever committed per outer loop iteration -- re-evaluated with a fresh lookahead next year. This
gives slow-building investments (root_no/root_length via the genuinely-integrated `tree_nitrogen`
state; `ectomycorrhiza_mass` via `dmyco_dt()`) a chance to show their return within the trial
before being judged, rather than only ever seeing their immediate cost.

Code: `inst/include/life_history.h` / `src/life_history.cpp` (new parameter inserted after
`reopt_dt`, before the step-size args); call sites updated in `run_phase2_relaxed_reopt.R`,
`run_phase2_sensitivity_root_no0.R`, `run_phase2_sensitivity_root_no0_constclimate.R` (all set
`trial_horizon_years <- 3`). `vignettes/sensitivity_spinup.R` (older, unmaintained script) NOT
updated -- will break if run, matching how other superseded scripts have been left alone this
session.

**Validated with a short test (N=0.20, 1960-1976, trial_horizon_years=3) -- clean, complete
success.** All three "escape valve" pathologies found at horizon=1 (post the §18 C_export_to_myco
fix) are gone simultaneously:

| | horizon=1 (post-§18 fix) | **horizon=3** | Static calibration |
|---|---|---|---|
| ecto_allo | 0 (collapsed) | **0.05-0.10** (genuine interior value) | 0.2 (fixed) |
| mycorrhized | 0 (collapsed) | **0.85-1.00** | 0.9 (fixed) |
| root_no | collapsed toward floor | **250k-740k** (healthy range) | 300,000 (fixed) |
| root_length | pinned at the 6mm ceiling | **1.4-2.2mm** (interior) | 2.0mm (fixed) |
| height @ 1976 | ~7.2m | **8.80m** | ~8.1-8.9m (interpolated) |

Height now tracks right on top of the static calibration at this checkpoint. Confirms the §18
diagnosis was correct: it really was the single-year trial horizon, not the cost accounting
itself (which was already correct after §18's fix).

**Cost:** ~3x slower than horizon=1 (7.9 min for 16 years vs. ~2-4 min at horizon=1 for a
similar window) -- expected, since each candidate's lookahead now simulates 3x as many years. A
full 62-year run per N level takes ~25-30 min; both 18-run sensitivity sweeps would add several
more hours. **Decision (Joanna):** regenerate just the 3 main full-length N-level runs and the 5
main figures for now (~75 min); hold off on the sensitivity sweeps.

## 20. Session 2026-09-23 (cont.): full 62-year confirmation -- receding horizon resolves the original §15 question

Reran the 3 main N-level Phase 2 runs at full length (1960-2022, `trial_horizon_years=3`), per
Joanna's scoped choice (main runs only, sensitivity sweeps deferred). ~18-19 min per N level
(3x the horizon=1 cost, as expected). Old horizon=1 results archived to
`vignettes/plots/dynamic/phase2_relaxed_refixed/horizon1_archive/`. Figures regenerated:
`fig3_annual_trajectories.png`, `fig4_cross_section_accessibleN.png`, `fig5_CN_exchange_cost.png`,
`diagnostic_root_no_actual_vs_target.png`, `phase2_timebased_key_trajectories.png`.

**Result: this resolves §15, the original question that started this whole thread.** Final
heights (2018/2022): N=0.15 -> ~17m, N=0.20 (boreal calib.) -> ~20.3m, N=2.00 -> ~23.4m. The
static calibration (§11) reaches 18.25m at N=0.20 by 2022 -- the dynamic run is now directly
comparable, not several metres short. No more collapse-to-floor, no more ceiling-pinning on any
trait, at any N level, for the full 62 years:

- **root_no**: stays healthy (300k-1.5M) for N=0.15/0.20 the entire run -- no collapse. For
  N=2.00 it does decline substantially (down to ~4-5k by 2000+), but settles at a genuine
  interior value well above the 1e3 floor, not pinned there.
- **ecto_allo**: fluctuates in a sensible 0-10% range for N=0.15/0.20 (never permanently 0, never
  at the ceiling) -- ends at 0% for N=2.00 by ~1970 and stays there.
- **mycorrhized**: stays high (90-100%) for N=0.15/0.20 throughout, matching the ini's own
  literature default (0.9, Taylor et al. 2000) and general boreal ECM prevalence -- declines
  toward 0 for N=2.00 by ~2000.
- **root_length**: mostly interior (1-5mm) for N=0.15/0.20; N=2.00 still touches the 6mm ceiling
  during 1985-2010 before retreating back to ~2mm by 2018 -- the one trait that still shows some
  ceiling-seeking behaviour, worth another look if this thread is revisited, but no longer
  breaking the overall result.

This is a coherent, ecologically sensible story matching real boreal ECM biology: sustained heavy
mycorrhizal investment under N limitation, tapering off once soil N is abundant enough that direct
root uptake suffices -- the pattern the whole multi-session investigation (starting from the
original Fig. 4 question, sections 1-9) was originally looking for, now finally visible without
being confounded by the `u_transfer` calibration bug (§11), the `N_bar_static` divergence (§13),
the ecto_allo accounting bug (§18), or the single-year trial myopia (§19) that were each masking
or distorting it in turn.

**Follow-up, same session: built a direct delayed-vs-not-delayed comparison figure** across all 3
N levels (`vignettes/plot_horizon_comparison.R` ->
`vignettes/plots/dynamic/phase2_relaxed_refixed/horizon_comparison_delayed_vs_not.png`, 8 panels,
colour=N level, linetype=horizon). Data: `horizon1_archive/main_N_*.csv` (fixed objective, not
delayed) vs. the new `main_N_*.csv` (fixed objective, delayed). This makes the fix's value visible
directly: panel (e) ECM colonisation is the starkest -- under "not delayed" it collapses toward 0
for *every* N level including Low/boreal, while "delayed" correctly sustains ~90-100% for Low/
boreal and only drops it at High N (the ecologically sensible pattern). Panel (c) root_length shows
both horizons touch the 6mm ceiling early on, but "delayed" recovers to interior values afterward
while "not delayed" (N=0.20, green dashed) stays pinned at the ceiling for the entire 62 years --
so the ceiling-touching issue is itself partly a manifestation of insufficient horizon, not fully
independent of it.

**Sensitivity sweeps completed** (18 runs total, `trial_horizon_years=3`; old horizon=1 CSVs
archived to `horizon1_archive/sens/`): `sensitivity_starting_root_no0.png` (real climate,
~8.5 min/run) and `sensitivity_starting_root_no0_constant_climate.png` (constant climate,
~12 min/run, slower -- more solver iterations under the repeated seasonal cycle). Both confirm the
main-run result robustly, regardless of starting `root_no0` and regardless of climate: `mycorrhized`
stays ~90-100% for N=0.15/0.20, root tip density stays healthy (10^5-10^6), height is essentially
insensitive to the starting point, and at N=2.00 colonisation/root density genuinely decline (not
a climate-noise artifact -- confirmed again under fully constant forcing).

## 21. Session 2026-09-23/24 (cont.): "constant climate" doesn't hold light constant -- checked, decline survives anyway

Joanna's sharp catch: the §14/§20 "constant climate" tests only repeat the raw weather (temp/VPD/
PPFD/SWP) -- they do NOT hold the *light environment* constant, because `ErgodicEnvironment::
updateBackgroundCanopy` (`src/life_history.cpp:19-23`) computes the background stand's closure
density as a function of *elapsed calendar time* (`density(t) = bg_density_eq + (bg_density_ini -
bg_density_eq)*exp(-(t-bg_t0)/bg_tau)`, ini: `bg_canopy_density_ini=0.0, bg_canopy_density_eq=0.1,
bg_canopy_tau=25, bg_canopy_t0=1960`), independent of weather. By 20 simulated years in,
`density(1980) ~= 0.055` -- already more than halfway to equilibrium -- so the self-shading
environment genuinely changes over exactly the window tested, regardless of weather. (The tree's
own growing `crown_area` also feeds into the same self-shading calculation, but that's a real,
intended consequence of the tree's own growth, not an external confound to remove.)

**Test: froze the succession dynamic too** (`bg_canopy_density_eq = bg_canopy_density_ini = 0.0`,
i.e. density(t)=0 for the whole run, no background canopy ever -- first tried freezing at the
*equilibrium* value 0.1 instead, which crashed with `vector::_M_default_append`, a numerical edge
case in `updateBackgroundCanopy` from starting a young, tiny-crowned tree under full mature-stand
density from year 1; freezing at 0 avoids this by permanently taking the safe `density<=0` guard
branch). Reran the N=2.00 sensitivity case (3 starting `root_no0`, constant-climate weather,
`trial_horizon_years=3`) with this frozen-canopy ini and compared directly against the existing
(non-frozen) constant-climate N=2.00 result.
Script: `/tmp/.../scratchpad/run_frozen_canopy_N200.R` (not saved in repo -- scratch only);
figure: `vignettes/plots/dynamic/phase2_relaxed_refixed/frozen_canopy_N2.00_comparison.png`.

**Result: the two conditions are visually identical** across root_no, root_length, ecto_allo,
mycorrhized, and height, for all three starting points. Removing the background-canopy succession
dynamic entirely makes no detectable difference. This confirms the N=2.00 decline is genuinely
driven by the tree's own soil-N economics (and its own self-shading as it grows, which is real,
not confounded), not by the closing background canopy coinciding with elapsed time. The "not a
climate artifact" conclusion from §14/§20 now rests on solid ground rather than an incomplete
control.

## 22. Session 2026-09-24: Test A (mature-tree start) revealed fixed-step oscillation; implemented adaptive step-size shrinking -- clean fix

Joanna's follow-up to section 21: designed two convergence tests to separate "the model/tree takes
time to grow" from "the solver takes time to find its answer" -- Test A (start the search from an
already-mature tree grown under fixed traits via a plain `grow_for_dt` loop, no search involved, so
Phase 1 is cheap; then switch on the search and watch how fast it settles) and Test B (refine the
search's own grid resolution/step sizes and check the answer stops changing).

**Test A, run first** (N=2.00, frozen-canopy + constant-climate setup from section 21; Phase 1:
40 years under fixed static-default traits reaching 22.67m/64.8% of hmat=35m; Phase 2: switch on
`run_with_relaxed_local_reopt_trajectory`, `trial_horizon_years=3`, for 15 more years). Result:
even from this already-mostly-mature tree, `root_length` kept climbing for the entire 15-year
window (2.0mm -> 5.7mm, never settling) and `ecto_allo`/`mycorrhized` never settled at all --
they oscillated indefinitely between adjacent fixed grid levels (ecto_allo bouncing through
0/0.05/0.10/0.15; mycorrhized through 0.85/0.90/0.95). Since the tree's size was only changing
slowly by this point, ontogeny alone could not explain this -- pointed at the search algorithm
itself: `root_no_step_factor`/`root_length_step_factor`/`ecto_step`/`mycorrhized_step` are FIXED
for the entire run (passed in once, never adapted), so if the true continuous optimum for a trait
falls between two grid levels, the search can only ever bounce between its two nearest neighbours
-- it structurally cannot converge to a point between grid points. (Also caught and fixed a bug in
this session's own diagnostic script: comparing two `get_state()` calls after a loop had already
finished doesn't retrieve historical state -- `get_state()` just reads current fields regardless of
the `t` argument passed, so a "height increment = 0" line in the raw log output is a script
artifact, not a real result.)

**Fix: adaptive step-size shrinking**, standard pattern-search design (Hooke-Jeeves / generalised
pattern search step-halving-on-failure). Implemented in `run_with_relaxed_local_reopt_trajectory`
(`src/life_history.cpp`, `inst/include/life_history.h`): the four step-size parameters are now only
INITIAL values, not fixed for the whole run. Each trait's step independently halves (floored at
1/16 of its initial value) whenever that year's winning choice either stalls at the centre (no
improvement found at the current resolution) or reverses direction relative to the previous year's
move (the direct signature of oscillating between two grid points straddling the true optimum).
Continuing in the same direction as before leaves the step untouched, so genuine sustained progress
isn't slowed. No new R-facing parameters or logged columns -- fully internal to the method, existing
call sites and CSV schemas unchanged.

**Validated by rerunning the identical Test A** with the fix. Clean, complete success -- no trait
oscillates or runs away anymore:

| Trait | Fixed step (before) | Adaptive step (after) |
|---|---|---|
| ecto_allo | oscillates 0/0.05/0.10/0.15 forever | converges to ~6.9% by year 7.5, stable 7+ years |
| mycorrhized | oscillates 0.85-0.95 forever | smoothly settles to ~86.5% by year ~11, stable after |
| root_length | climbs unboundedly toward the 6mm ceiling (2.0->5.7mm in 14.5yr) | stays in a modest 1.97-2.26mm band, no runaway |
| root_no | wild non-monotonic swings (300k->75k->146k->112k) | settles to a flat plateau at ~258,840 by year 13.4 |

**Implication for earlier sections:** the "root_length pinned at the 6mm ceiling" artifact seen in
the full 62-year N=2.00 main run (section 20) was very likely itself a symptom of the fixed-step
search's inability to converge, not a genuine economic optimum at the ceiling -- worth rerunning
the full main runs (and sensitivity sweeps) with the adaptive-step fix to check whether that
episode disappears. **Not yet done this session** -- next step.

## 23. Session 2026-09-24 (cont.): full 62-year confirmation of the adaptive step-size fix -- corrects an earlier over-strong claim

Reran the 3 main N-level Phase 2 runs at full length with the adaptive step-size fix (section 22).
Runtime was uneven: N=0.15 and N=0.20 took ~30 min each (vs. ~18-19 min at fixed step, section 20 --
some slowdown expected, the search now does extra work shrinking/retrying), but **N=2.00 took
168 minutes** -- a large, unexplained outlier worth watching if this method is rerun again; not
investigated further this session. Figures regenerated in `phase2_relaxed_refixed/` (old
fixed-step results archived to `fixedstep_archive/`).

**Result: clean across the board, and one real correction to an earlier conclusion.**

- **Root_length ceiling artifact is gone entirely.** All three N levels now stay in a modest,
  smoothly-varying 1.0-2.6mm range for the full 62 years -- no more pinning at the 6mm ceiling at
  any point for any N level (contrast section 20's N=2.00 run, which sat at the ceiling for
  1985-2010 before retreating). Directly confirms the section 22 prediction that this was a
  fixed-step search artifact, not genuine economics.
- **root_no and ecto_allo trajectories are markedly cleaner** -- smooth, close-to-monotonic trends
  (root_no climbing toward ~1e6 for N=0.15/0.20, declining smoothly for N=2.00) replacing the
  noisy non-monotonic zigzagging seen under the fixed-step search.
- **Correction to an earlier claim (sections 12/20): mycorrhized at N=2.00 does NOT collapse to
  0%.** It plateaus at ~70-75% instead. `ecto_allo` (new carbon investment) does still trend
  toward ~0% at high N -- that part of the story holds -- but the *existing* colonisation fraction
  settles at a substantial, non-trivial level rather than vanishing. The earlier "tree essentially
  abandons the mycorrhizal partnership entirely at high N" framing (used in sections 12, 17, 20)
  was too strong and was itself partly a fixed-step-search artifact. The corrected, more moderate
  result (reduced new investment, but sustained partial colonisation) is also more biologically
  plausible.

**Not yet done:** rerunning the two sensitivity-to-starting-root_no0 sweeps with the adaptive-step
fix (would take several more hours, especially given the N=2.00 runtime outlier above) -- deferred.
`vignettes/plot_horizon_comparison.R`-style before/after comparison figures for the step-size fix
specifically (analogous to the delayed-vs-not-delayed figure from section 20) also not yet built.

## Where to start next

1. **§10.6 is the most important open thread**: the fixed-trait replication (exact manuscript
   methodology + exact manuscript trait values) still doesn't reproduce the manuscript's own
   reported heights, even with every known uncommitted change reverted to match `HEAD`. This
   needs to be resolved (or at least understood) before trusting *any* further comparison against
   the manuscript figures, dynamic or static. Suggested next step: diff `joint_grid_new_umax.R`'s
   actual simulation setup line-by-line against the fixed-trait replication script used this
   session (`/tmp/.../scratchpad/test_manuscript_replication.R` — not saved in the repo, would
   need reconstructing from this write-up) — dt, warm-up, weather file, everything.
2. If §10.6 turns out to be a red herring (e.g. a genuine pre-`23c1c3c` change), the natural next
   step for §10.3 is the later-state root_no sweep described there — does the model actually want
   to rebuild root investment once N demand grows past what a shed-down root system can supply?
3. Phase 2 (`run_with_relaxed_local_reopt_trajectory`) is the current best yearly-reopt
   implementation and is a reasonable base to keep building on once §10.3/§10.6 are better
   understood — the mechanism itself (relaxation, steady-state scoring, local search) all work
   as designed; the remaining problems are in the underlying cost/benefit economics and the
   still-unexplained `MAIN.pdf` regression, not in the search machinery itself.
