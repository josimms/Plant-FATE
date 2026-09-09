# N̄ crowding → I_b → leaf N optimization: why "optimal leaf N below max" isn't reassuring

**Context:** N̄ (accessible peri-rhizosphere N) sits at ~10⁻⁵–10⁻⁷ kg N/m³ throughout the
boreal calibration run — several orders of magnitude below both `k_15` (0.001) and the
observed range (0.0012–0.003). Question raised: since this compresses the fitness
landscape for the belowground allocation optimization (φ_m, n, l), could it also be
distorting the P-hydro leaf-nitrogen optimization? Counter-argument raised: optimal leaf
N comes out *below* some max, at a sensible interior value — doesn't that mean the
leaf-N optimization is insulated from the N̄ problem?

**Answer: no — it's the same problem, working through a different channel.**

Leaf N isn't optimized against an abstract ceiling. It's optimized against:

```
F = A_j - [β / (1 + I_b/β̃)] · a_jmax · N_l - γΔψ²
```

The nitrogen cost coefficient `β / (1 + I_b/β̃)`:
- starts at its **maximum**, `β` (0.1008), when `I_b = 0`
- only drops toward zero (nitrogen becomes "cheap") as `I_b` grows large relative to
  `β̃` (0.05, the I_b value at which the cost is halved)

And `I_b` itself is built directly from the same crowded-out quantities:

```
I_b = (a_u(l)/A_c) · [(1-m)·U_fr(N̄) + N_e]
```

Since `U_fr` depends on N̄ (essentially zero) and `N_e` (N transferred) is also
constrained, `I_b` is almost certainly sitting far below `β̃ = 0.05` throughout the run —
meaning the nitrogen cost coefficient is pinned near its maximum (`β`) essentially the
whole time.

The optimizer finds a genuine, smooth interior optimum for `N_l` — it's just optimizing
against a price of nitrogen that's stuck at "as expensive as it ever gets," because the
thing that's supposed to make nitrogen cheaper (`I_b`, driven by realized uptake) never
gets the chance to rise. If N̄ were realistic, `I_b` would be higher, nitrogen would
register as cheaper, and the same optimization would land on a higher `N_l` — not
because it's now unconstrained, but because the price it's solving against would have
genuinely changed.

**So:** "optimal leaf N is below max, at a sensible interior point" is true, but it is
not evidence against the N̄ problem. It's the same upstream issue (N̄ crowding),
propagating through the cost side of the leaf-N trade-off (via I_b) rather than through
a hard availability ceiling.

## Next step (not yet done)

Pull the actual `belowground_infrastructure` (I_b) column from a simulation run and
check how close to zero it sits relative to `β̃ = 0.05`. That would quantify concretely
how far into the "nitrogen always expensive" corner of the cost formula the model is
stuck, and whether fixing the N̄ crowding (e.g. the `k_13` table/ini mismatch — ini has
2.5, MAIN.tex parameter table cites 5 from the same Göttlicher et al. 2008 source) would
plausibly move `I_b` enough to matter.

## Related threads from the same session (for context)

- N̄ collapses via the demand/supply ratio `α = u_max·SA_active·R / (D·A_zone)`
  (`eq:alpha`) — both `SA_active` and `A_zone` scale with tree/crown size as the tree
  grows, so the system likely never escapes the demand-limited regime once reached
  (~1965 onward in the calibration run).
- `D` (diffusion coefficient) is already set 3x above its cited literature ceiling
  (0.1 vs. 0.003–0.03 m² yr⁻¹) — not much room to raise it further without new
  justification.
- This may also explain the Figure 2 (`traj_key`) degenerate-selection bug found
  earlier this session: `best_combo_lifetime` picked `ecto_allo=0` (zero fungal
  investment) for every soil-N level, because its one valid data point (before the run
  crashed) beat the mean of runs that survived — consistent with a genuinely flat/near-
  zero marginal payoff to belowground investment when N̄ is pinned this low.
- Also consistent with the "flatness" diagnostics (Fig. `16e`/`16f`) and the existing
  Discussion note ("a lot of the optimisation values are in the interior of the space
  rather than being optimised to the edge, which is a good sign") — flatness that may
  be an artifact of crushed N̄ rather than a genuine biological result.
