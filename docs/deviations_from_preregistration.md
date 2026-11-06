# Deviations from the religion preregistration

Companion to the preregistration (https://doi.org/10.5281/zenodo.20234640),
implemented in `src/religion_analysis.R`.

Every departure from the registered plan is listed here. Analyses that the
preregistration does not cover at all (sample descriptives, the credible-interval
table, the suppression check) are not deviations;
they are reported as exploratory and are identified as such where they appear.

---

## 1. Model 2 is not reported in the main text

**Registered.** Parameters 3 to 7 were to be read from Model 2:

```
cpeb ~ gender + education + religiosity + ideology + religiosity : ideology +
       ( 1 + religiosity + ideology || case_study )
```

Model 3 adds a random slope for the interaction and was registered only for
Parameter 8. The preregistration gives the reason for splitting them:

> This term is omitted from Model 2 because given anticipated sample (see below),
> the model may struggle to define a three-way interaction involving a random
> variable, which could interfere with estimation of Parameters 3 to 7.

**Done instead.** Parameters 3 to 7 are read from Model 3 in the main text.
Model 2 is still fitted, and is reported in full in the supplementary materials.

**Reasoning.** Model 2 existed as insurance against a risk that did not
materialise. Model 3 converged without difficulty on every operationalisation
and both prior specifications: all R-hat values are below 1.02 and all bulk and
tail effective sample sizes exceed 400. With that concern removed, Model 3 is
the better of the two for reporting the fixed effects, because it does not force
between-case-study variation in the interaction into the fixed interaction term
or into the other random slopes. It is therefore the more conservative
specification, and reporting the more conservative of two converged models is
the direction in which a deviation of this kind should run.

The estimates barely differ, so nothing in the conclusions turns on the choice.
Readers can verify this directly: supplementary Figure S1
(`output/supplementary/cri_interval_plot.png`) shows
Models 2 and 3 side by side, and the full
supplementary tables give both in their entirety.

---

## 2. The random-intercept SD keeps the brms default prior

**Registered.** Table 1 assigns Student's *t* (M = 0.1221, scale = 0.0852,
df = 4.1561) to the religiosity, ideology, and interaction σ, deriving it from
van Erp et al. (2017).

**Done instead.** That prior is applied to the random *slopes* only. The
random-intercept SD keeps brms's default, Student's *t*(3, 0, 2.5).

**Reasoning.** The van Erp et al. distribution describes between-study variation
in regression slopes. It carries no information about how much case studies
differ in their mean level of CPEB, and it is far too tight to be a reasonable
prior for that quantity. In brms a `class = sd` prior applies to every SD in the
group, intercept included, so the intercept is given its own more specific prior
to restore the default. This is a correction to an unintended consequence of the
registered wording rather than a change of position, and it makes the analysis
less informative-prior-driven, not more.

---

## 3. Conditional branches of the preregistration that were not triggered

Recorded here for completeness; none of these is a deviation, but a reader
checking the preregistration against the script may want to know why the code
for them is absent.

| Registered condition | Observed | Consequence |
|---|---|---|
| Drop a CPEB item if α < 0.5 | α = 0.768 | Three-item composite retained |
| Use `family = cumulative()` if CPEB reduces to one item | did not reduce | Gaussian likelihood retained |
| Use the F9/F16 composite only if *r* ≥ 0.3 | *r* = 0.258 | Fallback applied: F16 reverse-coded (anti-diversity) is the primary ideology measure |
| Drop a control covariate if it costs more than 5% of complete cases | education 2.5%, gender 2.5% | Both covariates retained |
| Adjust the model if the diagnostic plots show relevant heteroscedasticity | none apparent | No adjustment made |
