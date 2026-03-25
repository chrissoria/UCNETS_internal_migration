# CLAUDE.md — paper/

Instructions for building the results section of the academic paper in this folder.

## Current State

- `methods.qmd` — Partially written. Analytical strategy section is complete (hybrid BW model, LPM, simultaneity, sample). Data, Measures sections are stubs.
- `all_results.qmd` — Empty skeleton. **This is the file to build out.**
- `appendix.qmd` — Has variable definitions for moving outcomes only.
- `Fixed and Hybrid Effects Models.bib` — Bibliography file for citations.

## Paper Section IV: Analysis/Results — Full Outline

The results section (`all_results.qmd`) follows this structure. The focus is on building Section IV only.

### IV.a — Who Moves?

Present whole sample results; parallel results for young and old where possible.

#### IV.a.i — Who Expects to Move?
- **Table:** Plan_to_Move regressed on all IVs using the hybrid BW model; whole sample.
- **Source scripts:** `code/10_mixed_models.Rmd` (Plan to Move models), `code/07_plan_action_gap.Rmd` (plan rates descriptives).

#### IV.a.ii — Who Actually Moves Where?

**IV.a.ii.1 — Who Moves Out of the Region?**
- Outcome: `Moved_out_of_Bay_w23` vs all others.
- **Table:** Out-of-Bay regressed on predictors (whole sample).
- **Design decision:** Models with and without `within_Plan_to_Move` / `between_Plan_to_Move` are very similar — Plan just adds explained variance without changing other coefficients. Note this finding; may go in text or appendix.
- **Source scripts:** `code/10_mixed_models.Rmd`, `code/10_fe_models.Rmd`, `code/13_mediation_analysis.Rmd` (confirms planning mediates <5% of network effects).

**IV.a.ii.2 — Who Moves Locally?**
- Outcome: `Moved_in_Bay_w23` vs didn't move.
- **Sample:** Excludes out-of-area movers.
- **Table:** Local move regressed on predictors.
- Same Plan_to_Move inclusion note as above.
- **Source scripts:** `code/10_mixed_models.Rmd`, `code/10_fe_models.Rmd`.

**IV.a.ii.3 — Plan-Action Discrepancies (Extra Credit)**
- Four-fold outcome: Expected/Did (n≈189 wtd), Expected/Didn't (n≈118), Not Expected/Did (n≈112), Neither (n≈1216). Plus DK-if-will-move category (92 more moved).
- Worth a paragraph in main text, detailed analysis in appendix.
- **Source scripts:** `code/07_plan_action_gap.Rmd`.

**IV.a.ii.4 — Preliminary Conclusions**
- What have we learned about predictors of planning and moving?
- Focus on social ties as predictive of moving.

### IV.b — Why People Move, According to Them

Analysis of young movers in main text; parallel runs with older adults in appendix.

#### IV.b.i — Reasons Analysis
- Present coding/LLM categorization approach.
- Sort by local vs distant move.
- **Table:** Combined reason categories by distance of move, percentage distribution — young movers only (n≈103 wtd who moved pre-w2 or pre-w3). Old movers in appendix.
- **Appendix table:** Original (uncombined) reason categories.
- **Source scripts:** `code/06_move_reason_descriptives.Rmd`.

#### IV.b.ii — Preliminary Conclusions
- Why people move based on their own reports.
- Focus discussion on social network reasons.

### IV.c — Network Consequences of Moving

How does moving (and move distance) affect networks, especially local ties?

#### IV.c.i — First-Order Effects (Right After Move)
- **Table:** Network outcome variables regressed on: just moved out-of-area, just moved in-area (before this wave) + controls.
- Possibly include major move reasons as predictors in a second table.
- **Source scripts:** `code/10.5_network_change_visualizations.Rmd` (within-person network changes by mover status), `code/12_causal_robustness.Rmd`.

#### IV.c.ii — Second-Order Effects (Longer-Term Adjustment)
- Do people rebuild local networks after a move? Drop in local ties followed by rebound a wave later?
- Prior evidence (Table 7 from Marseilles book) shows rebuilding for college graduates.
- **Table:** More sophisticated version of that analysis.
- May bring in findings from the friendship-making strategies analysis.
- **Source scripts:** `code/10.5_network_change_visualizations.Rmd`, `code/05_friendship_strategy_descriptives.Rmd`, `code/09_friendship_strategy_predicts_outcomes.Rmd`.

### IV.d — Other Outcomes (If Space/Sense Permits)
- Psychological distress, other wellbeing measures.
- **Source scripts:** `code/08_psych_distress_scale.Rmd`.

## Mapping: Analysis Scripts → Results Sections

| Results Section | Primary Code Scripts |
|---|---|
| IV.a.i (Plans) | `07_plan_action_gap`, `10_mixed_models` |
| IV.a.ii.1 (Out-of-Bay) | `10_mixed_models`, `10_fe_models`, `13_mediation_analysis` |
| IV.a.ii.2 (Local moves) | `10_mixed_models`, `10_fe_models` |
| IV.a.ii.3 (Plan-action gap) | `07_plan_action_gap` |
| IV.b (Reasons) | `06_move_reason_descriptives` |
| IV.c.i (First-order network) | `10.5_network_change_visualizations`, `12_causal_robustness` |
| IV.c.ii (Second-order network) | `10.5_network_change_visualizations`, `05_friendship_strategy_descriptives`, `09_friendship_strategy_predicts_outcomes` |
| IV.d (Other outcomes) | `08_psych_distress_scale` |

## Writing Conventions

- **Language:** Associational only — "associated with", "shows higher rates", "predicts" (in the statistical sense). No causal claims ("causes", "leads to", "drives").
- **Model reporting:** Report both within-person and between-person coefficients from the hybrid BW model. Flag within-person as the key causal-direction estimate; between-person as cross-sectional association.
- **Statistical detail:** Report coefficient, robust SE, p-value. Use stars sparingly; prefer confidence intervals or exact p-values.
- **Age stratification:** Young adults (agegroup==0) are the primary sample for most analyses. Older adults (agegroup==1) are secondary — report in appendix when underpowered.
- **Power caveats:** Always note when EPV < 10 (especially out-of-Bay models for older adults).
- **Sample sizes:** Report weighted and unweighted N where relevant.
- **Tables:** Refer to tables by number. Tables should include model specification notes (outcome, sample restriction, SE type).

## Quarto Conventions

- Use `format: pdf` with the existing bibliography (`Fixed and Hybrid Effects Models.bib`).
- Citations use `[@key]` syntax.
- Cross-reference tables and figures with Quarto labels.
- The results file currently uses `format: html` — this may need updating to `pdf` to match the methods file when assembling the final paper.

## Key Findings to Highlight (from analysis scripts)

These are the substantive findings already established in the code. Use them to guide narrative:

1. **Planning mediates <5% of network effects on mobility** — networks operate through non-deliberative mechanisms (code/13).
2. **Local kin reduce out-migration** — 97% direct effect, not through reducing plans (code/13).
3. **Distant non-kin increase out-migration** — 99% direct effect (code/13).
4. **Career dominates young adult mobility reasons**; financial reasons more prominent for older adults (code/06).
5. **Plan-to-move conversion is imperfect** — substantial numbers in both "expected but didn't" and "didn't expect but did" categories (code/07).
6. **College graduates show network rebuilding after moves** (prior Marseilles analysis).
