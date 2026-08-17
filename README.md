# FixedAndRandomEffectsInLogLinearModels

<br>

This repository contains the code for the analyses reported in:

> Milin, P. & Rudas, T. (2026). The Analysis of Fixed and Random Effects in a Log-linear Modeling Framework. To appear in *Journal of Applied Statistics*.	

### Contents

- `auxiliary_functions.R` — Auxiliary functions required by the analysis scripts

Parts of the analysis corresponding to particular sections of the paper:

- `Section5.1.1.R` - Two-way contingency tables
- `Section5.1.2.R` - Three-way contingency tables (the simplest multi-way table)
- `Section5.2.R` - Intentionally and ad hoc observed categories
- `Section6.R` - Approximate conditions 
- `Section7.1.R` - Prior studies revisited
- `Section7.2.R` - Practical considerations

### Summary

All data were either simulated, obtained from published sources, or retrieved from data repositories. References to all original sources are consistently provided. The data were used to explore various analytical scenarios in which all variables are categorical and, for at least some of them, only a subset of categories is observed. When all variables are categorical, one of the most common analytical approaches is log-linear modeling. The main theoretical result demonstrates that if a log-linear model holds when considering all categories of the involved variables (fixed effects), it also holds when some variables are restricted to a subset of their categories (random effects).

The data are available from the University of Birmingham Institutional Research Archive (UBIRA): [https://edata.bham.ac.uk/1740/](https://edata.bham.ac.uk/1740/)

The data are provided as comma-separated values files (`*.csv`). For large-scale, computationally demanding parts of the analysis, results are precomputed and stored as R Data Archives (`*.rda`).

---

For further enquiries, please contact: `p.milin@bham.ac.uk`

