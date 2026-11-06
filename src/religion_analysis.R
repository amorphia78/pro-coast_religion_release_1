# ══════════════════════════════════════════════════════════════════════════════
# Religiosity, political ideology, and civic pro-environmental behaviour (CPEB)
#
# Pre-registered analysis: https://doi.org/10.5281/zenodo.20234640
# Deviations from it:      docs/deviations_from_preregistration.md
#
# This script implements the pre-registration and produces the manuscript tables
# and figures that report it. Companion scripts:
#   src/religion_descriptives.R    — sample descriptives (not pre-registered)
#   src/religion_supplementary.py  — assembles the supplementary .docx
#
# Run from the root of this package. See README.md.
# ══════════════════════════════════════════════════════════════════════════════


# ── Run control ───────────────────────────────────────────────────────────────
# Fitting all ten models takes a while. A fit is computed if its file is not yet
# in output/models/, so the first run fits everything; after that it is read back
# from that file. To recompute a fit that already exists, name it in REFIT.
# REFIT <- MODEL_NAMES does a full refit.
#
# Each name below is one .rds file. The four ideology names each hold a list of
# two fits, Model 2 and Model 3, because those two always change together.

MODEL_NAMES <- c(
  "m1_spec",   # Model 1, specified (informative) priors
  "m1_def",    # Model 1, brms default priors
  "soc_spec",  # Models 2 and 3, anti-diversity ideology, specified priors
  "soc_def",   # Models 2 and 3, anti-diversity ideology, default priors
  "lr_spec",   # Models 2 and 3, left-right ideology,     specified priors
  "lr_def"     # Models 2 and 3, left-right ideology,     default priors
)

REFIT <- character(0)


library(posterior)
library(brms)
library(ggplot2)
library(patchwork)

source("src/data_prep.R")
source("src/religion_table_output.R")

model_dir <- "output/models"
out_dir   <- "output"
supp_dir  <- file.path(out_dir, "supplementary")

dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(supp_dir,  recursive = TRUE, showWarnings = FALSE)

# Full country names, for the figure legend and the diagnostic plots. Only case
# studies that contribute complete cases ever reach the output.
case_labels <- c(ee = "Estonia", ie = "Ireland",  it = "Italy",
                 me = "Montenegro", mt = "Malta", no = "Norway",
                 ro = "Romania", si = "Slovenia", uk = "UK")

# The threshold that defines "meaningful variation" in the pre-registration: the
# 20th percentile of the van Erp et al. (2017) inter-study variation prior.
MEANINGFUL_SIGMA <- 0.0708

# The two σ priors, as (df, location, scale). Both are truncated at zero, since
# a standard deviation cannot be negative.
PRIOR_SIGMA_SPECIFIED <- list(df = 4.1561, mu = 0.1221, scale = 0.0852)
PRIOR_SIGMA_DEFAULT   <- list(df = 3,      mu = 0,      scale = 2.5)

# The specified β prior. The brms default for a population-level coefficient is
# flat and improper, so it has no quantiles to report.
PRIOR_BETA_SD <- 0.2581


# ── Model helpers ─────────────────────────────────────────────────────────────

# Report R-hat and ESS for a fitted model, or for each element of a list of
# fitted models. The full table is printed only when a threshold is breached.
print_diagnostics <- function(model, label) {
  if (is.list(model) && !inherits(model, "brmsfit")) {
    Map(function(m, nm) print_diagnostics(m, paste0(label, " / ", nm)), model, names(model))
    return(invisible(NULL))
  }
  diag <- as_draws(model) |> summarise_draws(rhat, ess_bulk, ess_tail)
  bad_rhat <- any(diag$rhat >= 1.02, na.rm = TRUE)
  bad_ess  <- any(diag$ess_bulk <= 400 | diag$ess_tail <= 400, na.rm = TRUE)
  cat("Diagnostics for", label, "— ")
  if (bad_rhat || bad_ess) {
    cat("WARNING: convergence issues detected.\n")
    print(diag, n = Inf)
  } else {
    cat("R_hat and ESS OK.\n")
  }
}

# Fit and save, or reload, one entry of MODEL_NAMES.
fit_or_load <- function(name, filename, fit_fn) {
  path <- file.path(model_dir, filename)
  model <- if (name %in% REFIT || !file.exists(path)) {
    m <- fit_fn()
    saveRDS(m, path)
    m
  } else {
    readRDS(path)
  }
  print_diagnostics(model, name)
  model
}


# ══ 1. Pre-registration decision checks ═══════════════════════════════════════
#
# 1. CPEB composite reliability (A26/A27/A28)
#    The pre-registration drops the weakest item if alpha < 0.5, or defaults to
#    A27 if no item improves alpha. Observed alpha = 0.768, well above the 0.5
#    threshold, so the full three-item composite is retained (and so the
#    "family = cumulative()" branch of the pre-registration never applies).
#
# 2. Social conservatism composite reliability (F9/F16)
#    The pre-registration uses the two-item composite only if r >= 0.3; if not,
#    the single item F16 (reverse-coded, "anti-diversity") is used instead.
#    Observed r = 0.258, below the threshold, so the fallback applies: anti_div
#    is the ideology predictor for the primary operationalisation throughout.
#
# 3. Left-right ideology (F17)
#    A single item, so no composite reliability check applies. F17 is recoded in
#    data_prep.R: "DK" -> 4 (neutral midpoint, per pre-registration), "Prefer
#    not to say" -> NA.
#
# 4. Covariate-dropping rule (5% loss threshold)
#    A covariate is dropped if adding it to the core set (cpeb, relig, anti_div)
#    reduces complete cases by more than 5% relative to the core count. Both
#    covariates cost about 2.5%, so both are retained.

core_n     <- sum(complete.cases(df[, c("cpeb", "relig", "anti_div")]))
with_edu_n <- sum(complete.cases(df[, c("cpeb", "relig", "anti_div", "edu")]))
with_sex_n <- sum(complete.cases(df[, c("cpeb", "relig", "anti_div", "non_man_std")]))

message(sprintf("Complete cases on core (cpeb, relig, anti_div): %d", core_n))
message(sprintf("  + edu:         %d (%.1f%% of core retained)", with_edu_n, 100 * with_edu_n / core_n))
message(sprintf("  + non_man_std: %d (%.1f%% of core retained)", with_sex_n, 100 * with_sex_n / core_n))


# ══ 2. Priors ═════════════════════════════════════════════════════════════════
# Table 1 of the pre-registration. The van Erp et al. (2017) Student's t prior
# describes between-study variation in regression *slopes*, so it should not
# apply to the random intercept; the intercept SD is reset to the brms default
# via a more specific prior that overrides the broader class = sd entry.

priors_m1 <- c(
  prior(normal(0.0321, 0.2581),            class = b,  coef = non_man_std),
  prior(normal(0.13,   0.2581),            class = b,  coef = edu),
  prior(normal(0,      0.2581),            class = b,  coef = relig),
  prior(student_t(4.1561, 0.1221, 0.0852), class = sd),
  prior(student_t(3, 0, 2.5),              class = sd, group = case, coef = Intercept)
)

# Models 2 and 3, anti-diversity (F16 reverse-coded) as the ideology measure.
priors_soc <- c(
  prior(normal(0.0321, 0.2581),            class = b,  coef = non_man_std),
  prior(normal(0.13,   0.2581),            class = b,  coef = edu),
  prior(normal(0,      0.2581),            class = b,  coef = relig),
  prior(normal(0,      0.2581),            class = b,  coef = anti_div),
  prior(normal(0,      0.2581),            class = b,  coef = relig:anti_div),
  prior(student_t(4.1561, 0.1221, 0.0852), class = sd),
  prior(student_t(3, 0, 2.5),              class = sd, group = case, coef = Intercept)
)

# Models 2 and 3, left-right (F17) as the ideology measure.
priors_lr <- c(
  prior(normal(0.0321, 0.2581),            class = b,  coef = non_man_std),
  prior(normal(0.13,   0.2581),            class = b,  coef = edu),
  prior(normal(0,      0.2581),            class = b,  coef = relig),
  prior(normal(0,      0.2581),            class = b,  coef = left_right),
  prior(normal(0,      0.2581),            class = b,  coef = relig:left_right),
  prior(student_t(4.1561, 0.1221, 0.0852), class = sd),
  prior(student_t(3, 0, 2.5),              class = sd, group = case, coef = Intercept)
)


# ══ 3. Model fitting ══════════════════════════════════════════════════════════
#
# Model 1: cpeb ~ gender + education + religiosity + (1 + religiosity || case)
#          Parameters 1 and 2.
# Model 2: adds ideology and religiosity:ideology as fixed effects, and ideology
#          as a random slope. Parameters 3 to 7 as registered.
# Model 3: Model 2 plus a random slope for the interaction. Parameter 8.
#
# Every model is fitted twice: once with the specified (informative) priors of
# Table 1, and once with brm() defaults, which is the pre-registered robustness
# check.

# Models 2 and 3 differ only in whether the interaction has a random slope, so
# they are always fitted as a pair. priors = NULL gives the brm() defaults.
fit_ideology_models <- function(iv, priors = NULL) {
  f2 <- as.formula(sprintf(
    "cpeb ~ non_man_std + edu + relig + %s + relig:%s + (1 + relig + %s || case)",
    iv, iv, iv
  ))
  f3 <- as.formula(sprintf(
    "cpeb ~ non_man_std + edu + relig + %s + relig:%s + (1 + relig + %s + relig:%s || case)",
    iv, iv, iv, iv
  ))
  args2 <- list(formula = f2, data = df, cores = 4)
  args3 <- list(formula = f3, data = df, cores = 4)
  if (!is.null(priors)) {
    args2$prior <- priors
    args3$prior <- priors
  }
  list(m2 = do.call(brm, args2), m3 = do.call(brm, args3))
}

m1_spec <- fit_or_load("m1_spec", "model1_bayes.rds", function() brm(
  cpeb ~ non_man_std + edu + relig + (1 + relig || case),
  data = df, prior = priors_m1, cores = 4
))

m1_def <- fit_or_load("m1_def", "model1_bayes_default.rds", function() brm(
  cpeb ~ non_man_std + edu + relig + (1 + relig || case),
  data = df, cores = 4
))

soc_spec <- fit_or_load("soc_spec", "fit_soc_spec.rds",
                        function() fit_ideology_models("anti_div", priors_soc))
soc_def  <- fit_or_load("soc_def",  "fit_soc_def.rds",
                        function() fit_ideology_models("anti_div"))
lr_spec  <- fit_or_load("lr_spec",  "fit_lr_spec.rds",
                        function() fit_ideology_models("left_right", priors_lr))
lr_def   <- fit_or_load("lr_def",   "fit_lr_def.rds",
                        function() fit_ideology_models("left_right"))

m2_soc_spec <- soc_spec$m2;  m3_soc_spec <- soc_spec$m3
m2_soc_def  <- soc_def$m2;   m3_soc_def  <- soc_def$m3
m2_lr_spec  <- lr_spec$m2;   m3_lr_spec  <- lr_spec$m3
m2_lr_def   <- lr_def$m2;    m3_lr_def   <- lr_def$m3

# One registry of every fit, used by the supplementary model tables and by the
# credible-interval table. `iv` is the ideology coefficient name, NA for Model 1.
model_registry <- list(
  list(key = "model1_specified",              model = m1_spec,     number = 1, iv = NA,
       ideology = "—",              prior = "Specified"),
  list(key = "model1_default",                model = m1_def,      number = 1, iv = NA,
       ideology = "—",              prior = "Default"),
  list(key = "model2_antidiversity_specified", model = m2_soc_spec, number = 2, iv = "anti_div",
       ideology = "Ideology as anti-diversity", prior = "Specified"),
  list(key = "model3_antidiversity_specified", model = m3_soc_spec, number = 3, iv = "anti_div",
       ideology = "Ideology as anti-diversity", prior = "Specified"),
  list(key = "model2_antidiversity_default",   model = m2_soc_def,  number = 2, iv = "anti_div",
       ideology = "Ideology as anti-diversity", prior = "Default"),
  list(key = "model3_antidiversity_default",   model = m3_soc_def,  number = 3, iv = "anti_div",
       ideology = "Ideology as anti-diversity", prior = "Default"),
  list(key = "model2_leftright_specified",     model = m2_lr_spec,  number = 2, iv = "left_right",
       ideology = "Ideology as left-right", prior = "Specified"),
  list(key = "model3_leftright_specified",     model = m3_lr_spec,  number = 3, iv = "left_right",
       ideology = "Ideology as left-right", prior = "Specified"),
  list(key = "model2_leftright_default",       model = m2_lr_def,   number = 2, iv = "left_right",
       ideology = "Ideology as left-right", prior = "Default"),
  list(key = "model3_leftright_default",       model = m3_lr_def,   number = 3, iv = "left_right",
       ideology = "Ideology as left-right", prior = "Default")
)


# ══ 4. Model-fit diagnostic plots ═════════════════════════════════════════════
# Pre-registration: "residuals vs fitted values (scatterplot), and residuals vs
# group (boxplot) will be examined. Models will be adjusted as necessary (for
# example by modelling residual standard deviation as a function of case study
# if relevant heteroscedasticity is observed)."
#
# Produced for all five specified-prior models. The default-prior fits are a
# robustness check on the priors, not a different likelihood, so they are not
# separately diagnosed.

diagnostic_plots <- function(model, label) {
  # fitted() and residuals() return rows in the order of model$data, which is
  # already the complete-case subset the model was fitted on.
  diag_df <- data.frame(
    fitted = fitted(model)[, "Estimate"],
    resid  = residuals(model)[, "Estimate"],
    case   = unname(case_labels[model$data$case])
  )

  p_fitted <- ggplot(diag_df, aes(x = fitted, y = resid)) +
    geom_point(alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Fitted values", y = "Residuals",
         title = paste0("Residuals vs fitted — ", label)) +
    theme_minimal()

  p_group <- ggplot(diag_df, aes(x = case, y = resid)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Case study", y = "Residuals",
         title = paste0("Residuals vs case study — ", label)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p_fitted / p_group
}

diagnostic_targets <- list(
  list(file = "diagnostics_model1.png",
       label = "Model 1 (specified priors)",                     model = m1_spec),
  list(file = "diagnostics_model2_antidiversity.png",
       label = "Model 2, ideology as anti-diversity (specified priors)",     model = m2_soc_spec),
  list(file = "diagnostics_model3_antidiversity.png",
       label = "Model 3, ideology as anti-diversity (specified priors)",     model = m3_soc_spec),
  list(file = "diagnostics_model2_leftright.png",
       label = "Model 2, ideology as left-right (specified priors)",         model = m2_lr_spec),
  list(file = "diagnostics_model3_leftright.png",
       label = "Model 3, ideology as left-right (specified priors)",         model = m3_lr_spec)
)

for (target in diagnostic_targets) {
  ggsave(file.path(supp_dir, target$file),
         diagnostic_plots(target$model, target$label),
         width = 8, height = 8, dpi = 200)
  message("Wrote ", file.path(supp_dir, target$file))
}


# ══ 5. Pre-registered reporting quantities ════════════════════════════════════
#
# For fixed effects (β) the pre-registration asks for the median, the 95%
# credible interval, and the posterior probability that the parameter is in a
# given direction:
#   "Explore" (Parameters 1 and 3, religiosity): the observed direction,
#     determined post hoc, because no direction was predicted.
#   "Predict" (Parameters 5 and 7, ideology and the interaction): the predicted
#     direction, which is negative in both cases.
#
# For random-slope SDs (σ; Parameters 2, 4, 6 and 8) it asks for the median, the
# 95% credible interval, and the probability of "meaningful variation", i.e.
# P(σ > 0.0708) — the 20th percentile of the van Erp et al. (2017) inter-study
# variation prior.

extract_param <- function(model, coef, type = "fixed", direction = NULL) {
  # type:      "fixed" for a regression β, "random" for a between-case-study σ
  # direction: "negative" / "positive" for a predicted parameter, NULL to use
  #            the observed direction
  draws <- if (type == "fixed") {
    as_draws_df(model)[[paste0("b_", coef)]]
  } else {
    as_draws_df(model)[[paste0("sd_case__", coef)]]
  }

  prob <- if (type == "random") {
    mean(draws > MEANINGFUL_SIGMA)
  } else if (is.null(direction)) {
    max(mean(draws > 0), mean(draws < 0))
  } else if (direction == "negative") {
    mean(draws < 0)
  } else {
    mean(draws > 0)
  }

  ci <- quantile(draws, c(0.025, 0.975))
  c(prob = prob, med = median(draws), lower = ci[[1]], upper = ci[[2]])
}

fmt_interval <- function(x) sprintf("%.2f [%.2f, %.2f]", x["med"], x["lower"], x["upper"])
fmt_prob     <- function(x) sprintf("%.3f", x["prob"])

# Fixed decimal places, so that a column of estimates lines up in the output
# tables instead of losing trailing zeros the way round() does.
fmt_num <- function(x, digits = 3) sprintf(paste0("%.", digits, "f"), x)

# Quantiles of a Student's t truncated below at zero — the form both σ priors
# take, since a standard deviation cannot be negative.
trunc_t_quantile <- function(p, spec) {
  below_zero <- pt((0 - spec$mu) / spec$scale, spec$df)
  spec$mu + spec$scale * qt(below_zero + p * (1 - below_zero), spec$df)
}

# Median and 95% interval of a prior. The brms default prior for a
# population-level coefficient is flat and improper, so it has neither; NA marks
# that case.
prior_interval <- function(type, prior) {
  if (type == "random") {
    spec <- if (prior == "Specified") PRIOR_SIGMA_SPECIFIED else PRIOR_SIGMA_DEFAULT
    q    <- trunc_t_quantile(c(0.5, 0.025, 0.975), spec)
    c(med = q[1], lower = q[2], upper = q[3])
  } else if (prior == "Specified") {
    c(med = 0,
      lower = qnorm(0.025, 0, PRIOR_BETA_SD), upper = qnorm(0.975, 0, PRIOR_BETA_SD))
  } else {
    c(med = NA_real_, lower = NA_real_, upper = NA_real_)
  }
}


# ══ 6. Main-text model tables ═════════════════════════════════════════════════
#
# One table per ideology operationalisation, specified priors only. Model 2 is
# not reported in the main text (see docs/deviations_from_preregistration.md):
# it was pre-registered only as insurance against Model 3 failing to converge,
# which it did not, and Model 3 is the more conservative specification. Model 2
# still appears in full in the supplementary tables.
#
# Row 1 comes from Model 1, which is the only model without an ideology term.
# Rows 2 to 4 come from Model 3.

main_table_rows <- function(m1, m3, iv, ideology) {
  # `ideology` is the measure's name as it reads mid-sentence, e.g.
  # "anti-diversity"; rows 3 and 4 open with it, so it is capitalised there.
  Ideology <- paste0(toupper(substring(ideology, 1, 1)), substring(ideology, 2))
  list(
    list(parameter = paste("Religiosity, not controlling for", ideology),
         model = 1, coef = "relig",           fit = m1, direction = NULL),
    list(parameter = paste("Religiosity, controlling for", ideology),
         model = 3, coef = "relig",           fit = m3, direction = NULL),
    list(parameter = paste0(Ideology, ", controlling for religiosity"),
         model = 3, coef = iv,                fit = m3, direction = "negative"),
    list(parameter = paste("Religiosity ×", Ideology),
         model = 3, coef = paste0("relig:", iv), fit = m3, direction = "negative")
  )
}

# One main-text table as a data frame whose column names are the displayed
# ones, repeats included: two "Model" columns and two "Mdn, 95% CrI" columns.
main_table <- function(rows) {
  body <- do.call(rbind, lapply(rows, function(row) {
    beta  <- extract_param(row$fit, row$coef, "fixed", row$direction)
    sigma <- extract_param(row$fit, row$coef, "random")
    c(row$parameter,
      as.character(row$model), fmt_interval(beta),  fmt_prob(beta),
      as.character(row$model), fmt_interval(sigma), fmt_prob(sigma))
  }))
  body <- as.data.frame(body, stringsAsFactors = FALSE)
  names(body) <- c("Parameter", "Model", "Mdn, 95% CrI", "p(direction)",
                   "Model", "Mdn, 95% CrI", "p(meaningful variation)")
  body
}

# The two headers merged across the β and σ blocks of columns.
MAIN_TABLE_CLUSTERS <- list(
  list(label = "Standardised regression coefficient (β)", from = 2, to = 4),
  list(label = "Inter-case-study variation (σ)",          from = 5, to = 7)
)

for (spec in list(
  list(iv = "anti_div",   m3 = m3_soc_spec, file = "model_table_antidiversity.xlsx",
       label = "ideology as anti-diversity", ideology = "anti-diversity"),
  list(iv = "left_right", m3 = m3_lr_spec,  file = "model_table_leftright.xlsx",
       label = "ideology as left-right", ideology = "left-right ideology")
)) {
  body <- main_table(main_table_rows(m1_spec, spec$m3, spec$iv, spec$ideology))
  cat(sprintf("\n\nMain-text model table — %s\n\n", spec$label))
  print(body, row.names = FALSE)
  write_table_xlsx(body, file.path(out_dir, spec$file), clusters = MAIN_TABLE_CLUSTERS)
}


# ══ 7. Priors and posteriors: interval plot ═══════════════════════════════════
#
# The reader-friendly form of the pre-registered robustness check. For the
# religiosity and ideology β and σ, in Models 2 and 3 under both ideology
# operationalisations, each panel shows the median and 95% interval of every
# posterior beside the prior it started from, for the specified priors and the
# brm() defaults alike.
#
# Two of the default priors cannot be drawn to scale. The default σ prior, a
# zero-truncated Student's t(3, 0, 2.5), has a 95% interval running far past any
# posterior; the default β prior is flat over the whole real line and has no
# interval at all. Each is drawn as a bar running to the edge of its panel and
# ending in an arrow: one arrow for the σ prior, whose lower bound is on the
# panel, and one at each edge for the flat β prior.
#
# Each row of panels (one parameter) has its own x-axis, shared across the two
# ideology columns so they can be compared directly. A single axis for all four
# parameters would leave the σ panels mostly empty and stop the arrows well
# short of the panel edge.

PARAMETER_ROWS <- c("Religiosity β", "Ideology β", "Religiosity σ", "Ideology σ")
PRIOR_LEVELS   <- c("Specified", "Default")
PRIOR_COLOURS  <- c(Specified = "steelblue", Default = "grey45")

IDEOLOGY_LABELS <- unique(vapply(Filter(function(e) e$number > 1, model_registry),
                                 function(e) e$ideology, character(1)))

# The rows within a panel, top to bottom, as y positions; and the vertical
# offset that separates a row's specified-prior bar from its default-prior bar.
# Positions are set by hand rather than dodged, because the arrowed bars are
# drawn by separate layers and position_dodge() only separates bars that share
# a layer.
ROW_POSITIONS <- c(Prior = 3, `Model 2 posterior` = 2, `Model 3 posterior` = 1)
PRIOR_OFFSET  <- c(Specified = 0.17, Default = -0.17)

interval_rows <- list()

for (entry in model_registry) {
  if (entry$number == 1) next   # Model 1 has no ideology term

  for (s in list(
    list(label = "Religiosity β", coef = "relig",  type = "fixed"),
    list(label = "Religiosity σ", coef = "relig",  type = "random"),
    list(label = "Ideology β",    coef = entry$iv, type = "fixed"),
    list(label = "Ideology σ",    coef = entry$iv, type = "random")
  )) {
    x <- extract_param(entry$model, s$coef, s$type)
    interval_rows[[length(interval_rows) + 1]] <- data.frame(
      Parameter = s$label, Ideology = entry$ideology,
      Distribution = sprintf("Model %d posterior", entry$number),
      Priors = entry$prior,
      Median = x[["med"]], lower = x[["lower"]], upper = x[["upper"]],
      flat = FALSE, stringsAsFactors = FALSE
    )
  }
}

# A prior depends on neither the model nor the ideology measure, but it is
# repeated in both ideology columns so that each panel carries the prior its
# posteriors came from.
for (ideology in IDEOLOGY_LABELS) {
  for (label in PARAMETER_ROWS) {
    type <- if (endsWith(label, "σ")) "random" else "fixed"
    for (prior in PRIOR_LEVELS) {
      q <- prior_interval(type, prior)
      interval_rows[[length(interval_rows) + 1]] <- data.frame(
        Parameter = label, Ideology = ideology, Distribution = "Prior",
        Priors = prior,
        Median = q[["med"]], lower = q[["lower"]], upper = q[["upper"]],
        flat = is.na(q[["med"]]), stringsAsFactors = FALSE
      )
    }
  }
}

intervals <- do.call(rbind, interval_rows)

# Each parameter's x-axis is set by its posteriors and its specified prior, with
# a little room either side; the default priors are too wide to be allowed to
# set it.
panel_limits <- lapply(setNames(PARAMETER_ROWS, PARAMETER_ROWS), function(p) {
  d <- intervals[intervals$Parameter == p &
                   (intervals$Distribution != "Prior" | intervals$Priors == "Specified"), ]
  span <- diff(range(c(0, d$lower, d$upper)))
  c(lo = min(0, d$lower) - 0.05 * span, hi = max(0, d$upper) + 0.05 * span)
})

intervals$lo <- vapply(intervals$Parameter, function(p) panel_limits[[p]][["lo"]], numeric(1))
intervals$hi <- vapply(intervals$Parameter, function(p) panel_limits[[p]][["hi"]], numeric(1))

# A flat prior runs off both edges. Otherwise a bar runs off whichever edge its
# interval passes, and is cut exactly at that edge so its arrowhead meets it.
intervals$runs_left  <- intervals$flat | intervals$lower < intervals$lo
intervals$runs_right <- intervals$flat | intervals$upper > intervals$hi
intervals$draw_lower <- ifelse(intervals$runs_left,  intervals$lo, intervals$lower)
intervals$draw_upper <- ifelse(intervals$runs_right, intervals$hi, intervals$upper)
intervals$arrow_ends <- ifelse(intervals$runs_left & intervals$runs_right, "both",
                        ifelse(intervals$runs_right, "last",
                        ifelse(intervals$runs_left,  "first", "none")))

# No point where there is no median (the flat prior) or where it is off the panel
# (the default σ prior's).
intervals$median_shown <- !intervals$flat &
  intervals$Median >= intervals$lo & intervals$Median <= intervals$hi

intervals$y        <- unname(ROW_POSITIONS[intervals$Distribution] +
                               PRIOR_OFFSET[intervals$Priors])
intervals$Priors   <- factor(intervals$Priors,   levels = PRIOR_LEVELS)
intervals$Ideology <- factor(intervals$Ideology, levels = IDEOLOGY_LABELS)

# The default σ prior's median and upper bound are quoted in the figure notes,
# since neither is visible on the panel.
sigma_default_q <- prior_interval("random", "Default")

interval_caption <- paste(strwrap(sprintf(paste(
  "Points are medians and bars 95%% intervals. The specified β prior is Normal(0, 0.2581),",
  "its standard deviation derived from the distribution of effect sizes across 322",
  "social-psychology meta-analyses (Richard et al., 2003). The specified σ prior is a",
  "zero-truncated Student's t (M = 0.1221, scale = 0.0852, df = 4.1561), the best-fitting",
  "distribution for estimates of inter-study variation from 360 meta-analyses across",
  "psychology (van Erp et al., 2017). The default σ prior is a zero-truncated Student's t",
  "(M = 0, scale = 2.5, df = 3); its median (%.1f) and the upper bound of its 95%% interval",
  "(%.1f) lie beyond the panel, so its bar runs to the panel edge, ends in an arrow, and has",
  "no point. The default β prior is flat over the whole real line and so has no median or",
  "interval; its bar spans the panel with an arrow at each edge."),
  sigma_default_q[["med"]], sigma_default_q[["upper"]]), width = 110), collapse = "\n")

arrow_head <- function(ends) arrow(length = unit(1.8, "mm"), type = "closed", ends = ends)

# One row of panels: a single parameter, both ideology columns.
interval_panel_row <- function(parameter, first, last) {
  d   <- intervals[intervals$Parameter == parameter, ]
  lim <- panel_limits[[parameter]]

  p <- ggplot(d, aes(y = y, colour = Priors)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60")

  # One layer per arrow style, since arrow() is set per layer. Only the plain
  # bars feed the legend, so its keys are not drawn with arrowheads.
  for (ends in c("none", "first", "last", "both")) {
    bars <- d[d$arrow_ends == ends, ]
    if (nrow(bars) == 0) next
    p <- p + geom_segment(data = bars,
                          aes(x = draw_lower, xend = draw_upper, yend = y),
                          linewidth = 0.6, show.legend = ends == "none",
                          arrow = if (ends == "none") NULL else arrow_head(ends))
  }

  p <- p +
    geom_point(data = d[d$median_shown, ], aes(x = Median, shape = Priors), size = 2) +
    facet_grid(Parameter ~ Ideology) +
    scale_x_continuous(limits = c(lim[["lo"]], lim[["hi"]]), expand = c(0, 0)) +
    scale_y_continuous(breaks = ROW_POSITIONS, labels = names(ROW_POSITIONS),
                       limits = c(0.55, 3.45)) +
    scale_colour_manual(values = PRIOR_COLOURS, limits = PRIOR_LEVELS) +
    scale_shape_manual(values = c(Specified = 17, Default = 16), limits = PRIOR_LEVELS) +
    # Off, so an arrowhead sitting exactly on the panel edge is not clipped.
    coord_cartesian(clip = "off") +
    labs(x = if (last) "Median and 95% interval" else NULL, y = NULL,
         colour = "Priors", shape = "Priors",
         caption = if (last) interval_caption else NULL) +
    theme_bw(base_size = 11.5) +
    theme(panel.grid.minor    = element_blank(),
          panel.grid.major.y  = element_blank(),
          strip.background    = element_rect(fill = "grey92", colour = NA),
          strip.text          = element_text(face = "bold", size = 10.5),
          plot.caption        = element_text(size = 10.5, colour = "grey20", hjust = 0,
                                             lineheight = 1.15),
          plot.caption.position = "plot")

  # The ideology labels are needed once, above the top row.
  if (!first) {
    p <- p + theme(strip.text.x = element_blank(), strip.background.x = element_blank())
  }
  p
}

# One legend, above the top row: every row uses the same colour and shape
# scales, so a legend on the others would only repeat it. (Patchwork's `&`
# operator, the usual way to restyle a collected legend, fails to dispatch here.)
interval_plots <- lapply(seq_along(PARAMETER_ROWS), function(k) {
  interval_panel_row(PARAMETER_ROWS[k], first = k == 1, last = k == length(PARAMETER_ROWS)) +
    theme(legend.position = if (k == 1) "top" else "none")
})

p_cri <- patchwork::wrap_plots(interval_plots, ncol = 1)

ggsave(file.path(supp_dir, "cri_interval_plot.png"), p_cri,
       width = 8.5, height = 11.5, dpi = 300)
message("Wrote ", file.path(supp_dir, "cri_interval_plot.png"))


# ══ 8. Religiosity σ: prior against posterior ═════════════════════════════════
#
# The prior comparison as densities, with the medians and 95% intervals of those
# same four distributions beneath them on a shared x-axis. It has to be a σ
# rather than a β: population-level coefficients have a flat, improper default
# prior in brms, so no comparable prior density exists for b_relig under default
# priors. The random-slope SD has a proper zero-truncated Student's t prior
# under both specifications.
#
# Drawn for Model 3 with ideology as anti-diversity, the model the main text
# reports. The Model 2 version is near-identical, so it is not produced.

dprior_trunc <- function(x, spec) {
  density    <- dt((x - spec$mu) / spec$scale, spec$df) / spec$scale
  norm_const <- 1 - pt((0 - spec$mu) / spec$scale, spec$df)
  ifelse(x < 0, 0, density / norm_const)
}

LABEL_SPEC <- "Informative prior (van Erp et al.)"
LABEL_DEF  <- "Vague prior"

draws_spec <- as_draws_df(m3_soc_spec)$sd_case__relig
draws_def  <- as_draws_df(m3_soc_def)$sd_case__relig

# Wide enough for the longer posterior tail of the two prior specifications.
x_max <- 1.2 * unname(quantile(c(draws_spec, draws_def), 0.995))
x_grid <- seq(0, x_max, length.out = 512)

# The figure has no caption: its note is in the main text, which takes the
# vague prior's off-axis median and upper bound from main_text_analysis.md
# (section 12).

sigma_prior_posterior_plot <- function() {
  dens_post_spec <- density(draws_spec, from = 0, to = x_max, n = 512)
  dens_post_def  <- density(draws_def,  from = 0, to = x_max, n = 512)

  sigma_dist <- rbind(
    data.frame(x = x_grid, y = dprior_trunc(x_grid, PRIOR_SIGMA_SPECIFIED),
               distribution = "Prior",     prior = LABEL_SPEC),
    data.frame(x = x_grid, y = dprior_trunc(x_grid, PRIOR_SIGMA_DEFAULT),
               distribution = "Prior",     prior = LABEL_DEF),
    data.frame(x = dens_post_spec$x, y = dens_post_spec$y,
               distribution = "Posterior", prior = LABEL_SPEC),
    data.frame(x = dens_post_def$x,  y = dens_post_def$y,
               distribution = "Posterior", prior = LABEL_DEF)
  )

  sigma_intervals <- data.frame(
    prior        = c(LABEL_SPEC, LABEL_DEF, LABEL_SPEC, LABEL_DEF),
    distribution = c("Prior", "Prior", "Posterior", "Posterior"),
    median = c(trunc_t_quantile(0.5,   PRIOR_SIGMA_SPECIFIED),
               trunc_t_quantile(0.5,   PRIOR_SIGMA_DEFAULT),
               median(draws_spec), median(draws_def)),
    lower  = c(trunc_t_quantile(0.025, PRIOR_SIGMA_SPECIFIED),
               trunc_t_quantile(0.025, PRIOR_SIGMA_DEFAULT),
               quantile(draws_spec, 0.025), quantile(draws_def, 0.025)),
    upper  = c(trunc_t_quantile(0.975, PRIOR_SIGMA_SPECIFIED),
               trunc_t_quantile(0.975, PRIOR_SIGMA_DEFAULT),
               quantile(draws_spec, 0.975), quantile(draws_def, 0.975)),
    stringsAsFactors = FALSE
  )

  # Short row names: colour and line type already say which distribution a bar
  # belongs to, so the axis only has to name it. ggplot draws the first factor
  # level at the bottom, so these levels run bottom-to-top and each posterior
  # ends up directly above the prior it came from.
  sigma_intervals$row_label <- factor(
    paste(ifelse(sigma_intervals$prior == LABEL_SPEC, "Informative", "Vague"),
          tolower(sigma_intervals$distribution)),
    levels = c("Vague prior", "Vague posterior",
               "Informative prior", "Informative posterior")
  )

  # Bars that pass the right of the axis are cut exactly at its edge, where
  # their arrowhead sits; a median beyond the edge (the vague prior's) gets no
  # point.
  sigma_intervals$draw_upper   <- pmin(sigma_intervals$upper, x_max)
  sigma_intervals$clipped      <- sigma_intervals$upper > x_max
  sigma_intervals$median_shown <- sigma_intervals$median <= x_max

  x_scale <- scale_x_continuous(limits = c(0, x_max), expand = expansion(mult = c(0, 0)))

  p_density <- ggplot(sigma_dist,
                      aes(x = x, y = y, colour = prior, linetype = distribution)) +
    geom_line(linewidth = 1) +
    x_scale +
    labs(x = NULL, y = "Density", colour = "Model prior type",
         linetype = "Distribution") +
    guides(colour = guide_legend(override.aes = list(linewidth = 4))) +
    theme_minimal(base_size = 13.5) +
    # The two legends are stacked; side by side they overrun the figure width
    # at this text size.
    theme(axis.text.x = element_blank(), legend.position = "top",
          legend.box = "vertical", legend.box.just = "left")

  p_intervals <- ggplot(sigma_intervals,
                        aes(x = median, y = row_label, colour = prior,
                            linetype = distribution)) +
    geom_linerange(data = subset(sigma_intervals, !clipped),
                   aes(xmin = lower, xmax = draw_upper), linewidth = 0.9) +
    geom_segment(data = subset(sigma_intervals, clipped),
                 aes(x = lower, xend = draw_upper, y = row_label, yend = row_label),
                 linewidth = 0.9, arrow = arrow(length = unit(2, "mm"), type = "closed")) +
    geom_point(data = subset(sigma_intervals, median_shown), size = 2.4, shape = 16) +
    # Every layer here draws from a subset, and a discrete scale trained only on
    # subsets loses the factor's level order, so the row order is pinned.
    scale_y_discrete(limits = levels(sigma_intervals$row_label)) +
    x_scale +
    # Off, so the arrowhead sitting on the right edge is not clipped.
    coord_cartesian(clip = "off") +
    labs(x = bquote(sigma[religiosity] ~ "(Model 3, ideology as anti-diversity)"),
         y = NULL) +
    guides(colour = "none", linetype = "none") +
    theme_minimal(base_size = 13.5) +
    theme(panel.grid.major.y    = element_blank(),
          axis.text.y           = element_text(size = 11, lineheight = 1.1))

  # Only the density panel has legends, so they sit above it directly rather
  # than being collected (patchwork's `&` fails to dispatch here, as above).
  p_density / p_intervals + patchwork::plot_layout(heights = c(2, 1.1))
}

sigma_path <- file.path(out_dir, "sigma_relig_prior_posterior.png")
ggsave(sigma_path, sigma_prior_posterior_plot(), width = 9, height = 6.5, dpi = 300)
message("Wrote ", sigma_path)


# ══ 9. Main figure: predicted slopes by case study ════════════════════════════
#
# Two panels — CPEB against anti-diversity, and CPEB against religiosity — each
# showing the population-level fitted line with its 95% credible ribbon plus one
# line per case study. From Model 3 with specified priors, matching the
# main-text tables. Each case-study line is drawn with a width proportional to
# that case study's contribution to the model, so the eye is not drawn to a
# slope resting on forty respondents as readily as to one resting on a hundred.
# The legend repeats those widths, so a line can be matched to its case study by
# thickness as well as by colour.

main_figure_model <- m3_soc_spec
main_figure_re    <- ~ (1 + relig + anti_div + relig:anti_div || case)

ALL_CASES_LABEL  <- "All case studies"
LINE_WIDTH_RANGE <- c(0.4, 1.4)
ALL_CASES_WIDTH  <- 1.8

case_n <- as.data.frame(table(main_figure_model$data$case), stringsAsFactors = FALSE)
names(case_n) <- c("case", "n")
case_n <- case_n[case_n$n > 0, ]
case_n$case_label <- sprintf("%s (n = %d)", case_labels[case_n$case], case_n$n)
case_n <- case_n[order(case_n$case_label), ]

legend_levels  <- c(ALL_CASES_LABEL, case_n$case_label)
legend_colours <- c(setNames("black", ALL_CASES_LABEL),
                    setNames(scales::hue_pal()(nrow(case_n)), case_n$case_label))
# The legend keys have to be handed the widths the scale will render, since a
# legend cannot read them off a continuous scale by itself.
legend_widths <- c(ALL_CASES_WIDTH, scales::rescale(case_n$n, to = LINE_WIDTH_RANGE))

panel_preds <- function(iv) {
  other_iv <- setdiff(c("anti_div", "relig"), iv)

  pop_grid <- data.frame(
    x           = seq(min(df[[iv]], na.rm = TRUE), max(df[[iv]], na.rm = TRUE),
                      length.out = 100),
    other       = mean(df[[other_iv]],   na.rm = TRUE),
    edu         = mean(df$edu,           na.rm = TRUE),
    non_man_std = mean(df$non_man_std,   na.rm = TRUE)
  )
  names(pop_grid)[1:2] <- c(iv, other_iv)

  pop_pred <- cbind(fitted(main_figure_model, newdata = pop_grid, re_formula = NA),
                    pop_grid)
  # Carried as a column so the population line joins the colour legend.
  pop_pred$series <- factor(ALL_CASES_LABEL, levels = legend_levels)

  case_grid <- pop_grid[rep(seq_len(nrow(pop_grid)), times = nrow(case_n)), ]
  case_grid$case <- rep(case_n$case, each = nrow(pop_grid))

  case_pred <- cbind(fitted(main_figure_model, newdata = case_grid,
                            re_formula = main_figure_re),
                     case_grid)
  case_pred <- merge(case_pred, case_n, by = "case")
  case_pred$series <- factor(case_pred$case_label, levels = legend_levels)

  list(pop = pop_pred, case = case_pred)
}

preds <- list(anti_div = panel_preds("anti_div"),
              relig    = panel_preds("relig"))

y_lim <- range(unlist(lapply(preds, function(p)
  c(p$pop$Q2.5, p$pop$Q97.5, p$case$Estimate))))

make_panel <- function(iv, x_label, suppress_y = FALSE) {
  p <- ggplot() +
    geom_ribbon(data = preds[[iv]]$pop,
                aes(x = .data[[iv]], ymin = Q2.5, ymax = Q97.5), alpha = 0.3) +
    geom_line(data = preds[[iv]]$case,
              aes(x = .data[[iv]], y = Estimate, colour = series, linewidth = n)) +
    geom_line(data = preds[[iv]]$pop,
              aes(x = .data[[iv]], y = Estimate, colour = series),
              linewidth = ALL_CASES_WIDTH) +
    scale_colour_manual(values = legend_colours, limits = legend_levels) +
    scale_linewidth_continuous(range = LINE_WIDTH_RANGE, guide = "none") +
    guides(colour = guide_legend(override.aes = list(linewidth = legend_widths))) +
    coord_cartesian(ylim = y_lim) +
    labs(x = x_label, y = "Civic pro-environmental behaviour",
         colour = "Case study\n(line thickness\nis proportional to n)") +
    theme_minimal(base_size = 11)

  if (suppress_y) {
    p <- p + theme(axis.title.y = element_blank(),
                   axis.text.y  = element_blank(),
                   axis.ticks.y = element_blank())
  }
  p
}

main_figure <- (make_panel("anti_div", "Anti-diversity attitudes") |
                make_panel("relig",    "Religiosity", suppress_y = TRUE)) +
  patchwork::plot_layout(guides = "collect")

ggsave(file.path(out_dir, "religion_analysis.png"), main_figure,
       width = 9, height = 3.5, dpi = 300)
message("Wrote ", file.path(out_dir, "religion_analysis.png"))


# ══ 10. Supplementary: full model tables ══════════════════════════════════════
# Every fixed effect, every random-slope SD, and the residual SD, for all ten
# fits. Sigmas get their own rows rather than being paired with the matching
# beta, and the columns include the Monte Carlo diagnostics.

# "b_relig:anti_div" -> "β: Religiosity × Anti-diversity". The class prefix is
# split off first so that the ":" inside it is not caught by the interaction
# substitution.
pretty_parameter <- function(v) {
  prefix <- ifelse(startsWith(v, "b_"), "β: ",
                   ifelse(startsWith(v, "sd_case__"), "σ: ", ""))
  term <- sub("^sd_case__", "", sub("^b_", "", v))

  term <- gsub("non_man_std", "Gender (non-man)", term)
  term <- gsub("relig",       "Religiosity",      term)
  term <- gsub("anti_div",    "Anti-diversity",   term)
  term <- gsub("left_right",  "Left-right",       term)
  term <- gsub("\\bedu\\b",   "Education",        term)
  term <- gsub(":",           " × ",              term)
  term[term == "sigma"] <- "Residual SD"

  paste0(prefix, term)
}

full_model_table <- function(model) {
  keep  <- grep("^(b_|sd_case__|sigma$)", variables(model), value = TRUE)
  draws <- as_draws_df(model)

  summ <- summarise_draws(
    subset_draws(draws, variable = keep),
    mean, sd, median,
    ~quantile(.x, probs = c(0.025, 0.975)),
    rhat, ess_bulk, ess_tail
  )

  # p(direction) for fixed effects (observed direction), p(meaningful variation)
  # for random-slope SDs. Not defined for the fixed intercept, the residual SD,
  # or the random-intercept SD — the 0.0708 threshold is derived from between-
  # study variation in slopes, so it says nothing about intercept variation.
  prob <- vapply(summ$variable, function(v) {
    d <- draws[[v]]
    if (v %in% c("b_Intercept", "sd_case__Intercept", "sigma")) {
      NA_real_
    } else if (startsWith(v, "sd_case__")) {
      mean(d > MEANINGFUL_SIGMA)
    } else {
      max(mean(d > 0), mean(d < 0))
    }
  }, numeric(1))

  data.frame(
    Parameter    = pretty_parameter(summ$variable),
    Estimate     = fmt_num(summ$mean),
    SE           = fmt_num(summ$sd),
    Median       = fmt_num(summ$median),
    `CrI lower`  = fmt_num(summ$`2.5%`),
    `CrI upper`  = fmt_num(summ$`97.5%`),
    p            = ifelse(is.na(prob), "", sprintf("%.3f", prob)),
    Rhat         = fmt_num(summ$rhat),
    `Bulk ESS`   = round(summ$ess_bulk),
    `Tail ESS`   = round(summ$ess_tail),
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
  )
}

for (entry in model_registry) {
  write_table_xlsx(full_model_table(entry$model),
                   file.path(supp_dir, sprintf("model_full_%s.xlsx", entry$key)))
}


# ══ 11. Supplementary: suppression check ══════════════════════════════════════
#
# Standard effect-size yardsticks (Cohen's conventions, Funder & Ozer's
# benchmarks) were developed for zero-order Pearson correlations, not for
# partial regression coefficients. Reading a β against them is conservative
# *provided* there is no suppression: partialling out a correlated covariate
# normally shrinks a coefficient relative to the zero-order association, so the
# β understates the raw association the benchmarks describe. Suppression is the
# exception — it inflates |β| beyond |r|, or flips its sign — and would break
# the claim, so it is worth checking rather than assuming.
#
# For each predictor in each specified-prior model, this compares the zero-order
# correlation with CPEB (computed in that model's own complete-case sample) with
# the model's posterior median β, and reports the variance inflation factor of
# the fixed-effects design.
#
# A predictor is flagged when the signs of r and β disagree, or when |β| exceeds
# |r| by more than Monte Carlo noise (0.02). Predictors where both |r| and |β|
# fall below 0.10 are reported as negligible rather than flagged: 0.10 is the
# lower bound of the effect-size benchmarks at issue, so a sign flip or a
# widening between two coefficients that are both smaller than that cannot
# affect how either is read against those benchmarks.

NEGLIGIBLE_EFFECT <- 0.10
MC_TOLERANCE      <- 0.02

classify_suppression <- function(r, beta) {
  if (abs(r) < NEGLIGIBLE_EFFECT && abs(beta) < NEGLIGIBLE_EFFECT) {
    "negligible"
  } else if (sign(r) != sign(beta) || abs(beta) > abs(r) + MC_TOLERANCE) {
    "flagged"
  } else {
    "no"
  }
}

variance_inflation <- function(design) {
  # VIF_j = 1 / (1 - R²_j) from regressing predictor j on all the others.
  vapply(colnames(design), function(j) {
    others <- setdiff(colnames(design), j)
    r2 <- summary(lm(design[[j]] ~ ., data = design[, others, drop = FALSE]))$r.squared
    1 / (1 - r2)
  }, numeric(1))
}

suppression_rows <- do.call(rbind, lapply(
  Filter(function(e) e$prior == "Specified", model_registry),
  function(entry) {
    d <- entry$model$data
    # model$data also carries the grouping factor, which is not a predictor.
    predictors <- setdiff(names(d), c("cpeb", "case"))
    design     <- d[, predictors, drop = FALSE]

    # brms stores the interaction as a product of its components rather than as
    # a column, so it is added explicitly before computing the VIFs.
    if (!is.na(entry$iv)) {
      design[[paste0("relig:", entry$iv)]] <- d$relig * d[[entry$iv]]
    }
    vifs <- variance_inflation(design)

    do.call(rbind, lapply(names(vifs), function(term) {
      r    <- cor(design[[term]], d$cpeb)
      beta <- median(as_draws_df(entry$model)[[paste0("b_", term)]])
      data.frame(
        Model      = sprintf("Model %d", entry$number),
        Ideology   = entry$ideology,
        Predictor  = sub("^β: ", "", pretty_parameter(paste0("b_", term))),
        n          = nrow(d),
        `Zero-order r` = fmt_num(r),
        `β (median)`   = fmt_num(beta),
        VIF            = fmt_num(vifs[[term]], 2),
        Suppression    = classify_suppression(r, beta),
        check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
      )
    }))
  }
))

write_table_xlsx(
  suppression_rows, file.path(supp_dir, "suppression_check.xlsx"),
  note = paste("Note. A predictor is flagged when r and β have opposite signs, or",
               "when |β| exceeds |r| by more than 0.02 (Monte Carlo noise). Where",
               "both |r| and |β| are below 0.10, the lower bound of the effect-size",
               "benchmarks at issue, the predictor is marked negligible instead,",
               "since no benchmark reading can turn on the difference.",
               "Correlations are computed in each model's own complete-case",
               "sample.")
)

cat("\n\nSuppression check (specified-prior models)\n\n")
print(suppression_rows, row.names = FALSE)

flagged   <- suppression_rows[suppression_rows$Suppression == "flagged", ]
n_flagged <- nrow(flagged)
max_vif   <- max(as.numeric(suppression_rows$VIF))

assessed      <- suppression_rows[suppression_rows$Suppression == "no", ]
beta_within_r <- all(abs(as.numeric(assessed[["β (median)"]])) <=
                     abs(as.numeric(assessed[["Zero-order r"]])))

message(sprintf("Suppression check: %d of %d predictor-model combinations flagged; max VIF = %.2f.",
                n_flagged, nrow(suppression_rows), max_vif))

# A prose version of the same conclusion, for the supplementary document.
suppression_note <- c(
  "## Suppression check",
  "",
  paste0(
    "Conventional benchmarks for interpreting effect sizes were developed for ",
    "zero-order Pearson correlations rather than for partial regression ",
    "coefficients. Reading a standardised β against them is conservative so ",
    "long as there is no suppression: partialling out a correlated covariate ",
    "ordinarily shrinks a coefficient relative to the zero-order association, ",
    "so the β understates the raw association that the benchmarks describe. ",
    "Suppression is the exception, inflating |β| beyond |r| or reversing its ",
    "sign, and would invalidate the claim."
  ),
  "",
  paste0(
    "We therefore compared, for every predictor in every specified-prior model, ",
    "the zero-order correlation with CPEB in that model's own complete-case ",
    "sample against the model's posterior median β, and computed variance ",
    "inflation factors for the fixed-effects design (see the table below). ",
    if (n_flagged == 0) {
      paste0(
        "No predictor showed suppression. Where either coefficient reached 0.10 ",
        "— the lower bound of the benchmarks at issue — the β was of the same ",
        "sign as, and ",
        if (beta_within_r) "no larger in magnitude than" else
          "no more than 0.02 larger in magnitude than",
        ", the corresponding zero-order correlation. The remaining predictors ",
        "had both |r| and |β| below 0.10, so no benchmark reading turns on them. "
      )
    } else {
      sprintf(paste0(
        "%d of the %d predictor-model combinations were flagged for possible ",
        "suppression (%s); see the Suppression column of the table below. "),
        n_flagged, nrow(suppression_rows),
        paste(unique(flagged$Predictor), collapse = ", "))
    },
    sprintf("The largest variance inflation factor across all models was %.2f, ",
            max_vif),
    "far below any conventional cause for concern."
  ),
  "",
  paste0(
    "We accordingly interpret the standardised βs against correlation-based ",
    "benchmarks, treating that reading as conservative."
  )
)

write_utf8(suppression_note, file.path(supp_dir, "suppression_note.md"))


# ══ 12. Main-text numbers ══════════════════════════════════════════════════════
#
# Every number this script generates for the main text that appears in no output
# table, written to main_text_analysis.md. Numbers already in a table are not
# repeated. Specified priors unless stated.

# Effect-size benchmarks for Pearson's r (Lovakov & Agadullina, 2021): the 25th
# and 50th percentiles of effect sizes across social-psychology meta-analyses,
# labelled small and medium. Inputs to the probabilities below, not results.
SMALL_EFFECT  <- 0.12
MEDIUM_EFFECT <- 0.24

p_below <- function(model, coef, threshold) {
  mean(abs(as_draws_df(model)[[paste0("b_", coef)]]) < threshold)
}

religiosity_lines <- vapply(list(
  list(label = "Model 1",                             model = m1_spec),
  list(label = "Model 3, ideology as anti-diversity", model = m3_soc_spec),
  list(label = "Model 3, ideology as left-right",     model = m3_lr_spec)
), function(m) {
  sprintf("- %s: P(|β| < %.2f) = %.3f; P(|β| < %.2f) = %.3f",
          m$label, SMALL_EFFECT, p_below(m$model, "relig", SMALL_EFFECT),
          MEDIUM_EFFECT, p_below(m$model, "relig", MEDIUM_EFFECT))
}, character(1))

# The religiosity slope 1 SD below and 1 SD above the mean of anti-diversity
# (every predictor is z-scored, so ±1 is ±1 SD), because the interaction β alone
# is hard to read.
draws_m3   <- as_draws_df(m3_soc_spec)
slope_low  <- draws_m3$b_relig - draws_m3[["b_relig:anti_div"]]
slope_high <- draws_m3$b_relig + draws_m3[["b_relig:anti_div"]]

fmt_draws <- function(x) {
  sprintf("β = %.2f, 95%% CrI [%.2f, %.2f]", median(x), quantile(x, 0.025),
          quantile(x, 0.975))
}

analysis_lines <- c(
  "# Main-text numbers: analysis",
  "",
  paste("Generated by src/religion_analysis.R: every number it produces for the",
        "main text that is in no output table. Specified priors unless stated."),
  "",
  "## Religiosity: probability that |β| is below an effect-size benchmark",
  "",
  religiosity_lines,
  "",
  "## Simple slopes of religiosity (Model 3, ideology as anti-diversity)",
  "",
  sprintf("- 1 SD below the mean of anti-diversity: %s", fmt_draws(slope_low)),
  sprintf("- 1 SD above the mean of anti-diversity: %s", fmt_draws(slope_high)),
  "",
  "## Default (vague) σ prior, off the axis of sigma_relig_prior_posterior.png",
  "",
  sprintf("- Median: %.1f; upper bound of the 95%% interval: %.1f",
          sigma_default_q[["med"]], sigma_default_q[["upper"]]),
  ""
)

write_utf8(analysis_lines, file.path(out_dir, "main_text_analysis.md"))

cat("\n\n", paste(analysis_lines, collapse = "\n"), "\n", sep = "")

message("\nDone. Manuscript output in ", out_dir)
