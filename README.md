# A One-Sixty-Fourth Fraction of a $2^{10}$ Factorial Design

An AMS 582 project from Fall 2021 designing and analyzing a 16-run, 10-factor, two-level fractional factorial screening experiment.

## Design

The study uses a minimum-aberration $2^{10-6}_{III}$ design with factors A–J and generators:

$$
E=ABC,\quad F=BCD,\quad G=ACD,\quad H=ABD,\quad I=ABCD,\quad J=AB.
$$

The design trades a substantial reduction in experimental runs for Resolution III aliasing: main effects can be confounded with two-factor interactions. It is therefore appropriate as an initial screening design, with follow-up experiments needed to separate important effects.

## Analysis

The R workflow:

- generates the design with `FrF2`;
- examines main-effect plots;
- applies BIC-based stepwise selection;
- inspects candidate two-factor interactions;
- fits a reduced model with factors C, F, and I; and
- performs residual diagnostics, confidence intervals, and predictions.

The reported reduced-model F test has *p* = 0.00887. Because the experiment is unreplicated and highly fractionated, effect interpretation depends on sparsity assumptions and the alias structure.

## Repository contents

- `analysis.R` — design generation and statistical analysis
- `project.pdf` — final report
- `total22.csv` — response data joined to the design

## Author

Kai Li.
