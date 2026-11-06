# ══════════════════════════════════════════════════════════════════════════════
# Descriptives for the religiosity / ideology / CPEB analysis
#
# Companion to src/religion_analysis.R. Nothing here is pre-registered: the
# pre-registration specifies models, not sample description. Everything is
# computed on the individuals actually entering the models, on the original
# response scales rather than the z-scores the models use.
#
# Three analysis samples exist, because missingness differs by predictor:
#   Model 1                   n = 590   no ideology term, so nothing constrains
#                                       the sample on ideology missingness
#   Models 2 and 3, anti-div  n = 569   a strict subset of the Model 1 sample
#   Models 2 and 3, left-right n = 501  a different, heavily overlapping subset
#                                       of the Model 1 sample
#
# The main-text table and every chart describe the anti-diversity sample, which
# is the primary operationalisation. The other two samples are covered by the
# comparison tables in the supplementary rather than by a duplicate set of
# charts.
#
# Education is described as categories rather than as a mean. It is a seven-point
# ordinal scale whose points are qualitatively different kinds of qualification,
# so its central tendency is not a meaningful quantity; it therefore appears in
# the categorical tables with gender and denomination, not in the numeric ones.
# ══════════════════════════════════════════════════════════════════════════════

library(ggplot2)

source("src/data_prep.R")
source("src/religion_table_output.R")

out_dir   <- "output"
supp_dir  <- file.path(out_dir, "supplementary")
chart_dir <- file.path(out_dir, "descriptive_charts")

dir.create(supp_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)

case_labels <- c(ee = "Estonia", ie = "Ireland",  it = "Italy",
                 me = "Montenegro", mt = "Malta", no = "Norway",
                 ro = "Romania", si = "Slovenia", uk = "UK")

# ── Analysis samples ──────────────────────────────────────────────────────────
# Defined by the same complete-case rule brms applies, so these indices select
# exactly the rows each model was fitted on (verified against model$data).

covariates <- c("cpeb", "relig", "edu", "non_man_std")

samples <- list(
  model1     = list(label = "Model 1",
                    rows  = complete.cases(df[, covariates])),
  anti_div   = list(label = "Models 2 and 3, ideology as anti-diversity",
                    rows  = complete.cases(df[, c(covariates, "anti_div")])),
  left_right = list(label = "Models 2 and 3, ideology as left-right",
                    rows  = complete.cases(df[, c(covariates, "left_right")]))
)

for (s in samples) message(sprintf("%-28s n = %d", s$label, sum(s$rows)))

# The sample everything in the main text is computed on.
main_rows <- samples$anti_div$rows


# ── Variables on their original response scales ───────────────────────────────
#
# Each entry says how to recover the variable as respondents saw it, plus the
# labelling a table row and a chart panel need.
#
# `values`  is a full-length vector over df_in, subset per sample below.
# `kind`    is "ordinal" (integer scale points), "nominal", "age" or "cpeb".
# `special` names a response that is on the questionnaire but off the numeric
#           scale (only F17's "Don't know").
# `display` relabels a chart axis, `cat_labels` a category table row, and
#           `prose_labels` the same category inside a sentence — a table wants
#           "Other" where a sentence wants "another gender".

# F17 as the models see it: "DK" is the pre-registered neutral midpoint, not a
# missing value. Kept separate from the raw column so the charts can still show
# DK as its own bar.
f17_raw      <- trimws(as.character(df_in$F17))
f17_modelled <- suppressWarnings(as.numeric(f17_raw))
f17_modelled[f17_raw %in% "DK"] <- 4

# CPEB on its original scale: the mean of the three raw items, using the same
# na.rm rule as the modelled composite.
cpeb_raw <- rowMeans(df_in[, c("A26", "A27", "A28")], na.rm = TRUE)
cpeb_raw[is.nan(cpeb_raw)] <- NA

# The education response options, as respondents saw them.
edu_labels <- c(
  `1` = "1. No formal education",
  `2` = "2. Primary school",
  `3` = "3. Secondary school technical/vocational type",
  `4` = "4. Secondary school: university-preparatory type",
  `5` = "5. University-level education, Bachelor or equivalent",
  `6` = "6. University-level education, Master or equivalent",
  `7` = "7. Phd or equivalent"
)

desc_variables <- list(
  religiosity = list(
    label = "Religiosity", item = "F7", kind = "ordinal",
    values = as.numeric(df_in$F7), min = 1, max = 5,
    axis = "1 = Not at all religious  ·  5 = Very religious",
    question = "How religious would you consider yourself?"),

  anti_diversity = list(
    label = "Anti-diversity", item = "F16", kind = "ordinal",
    values = 6 - as.numeric(df_in$F16), min = 1, max = 5,
    axis = "Reverse-coded: 1 = agrees diversity enriches us  ·  5 = disagrees",
    question = paste("The different ways of life and increasing diversity in",
                     "[OWN COUNTRY] enrich us. (reverse-coded)")),

  left_right = list(
    label = "Left-right ideology", item = "F17", kind = "ordinal",
    values = f17_modelled, raw = f17_raw, special = "DK", min = 1, max = 7,
    axis = "1 = Left  ·  7 = Right",
    question = paste("In political matters, people talk of \"the left\" and",
                     "\"the right\". How would you place your views on this scale?",
                     "\"Don't know\" is shown as its own bar but, as pre-registered,",
                     "is recoded to the scale midpoint (4) in the models and is",
                     "counted as 4 in the mean reported here.")),

  age = list(
    label = "Age", item = "F2", kind = "age",
    values = as.numeric(df_in$F2),
    axis = "Age in years",
    question = "Age in years (banded; band midpoints shown)"),

  education = list(
    label = "Education", item = "F4", kind = "ordinal",
    values = as.numeric(df_in$F4), min = 1, max = 7,
    axis = "1 = No formal education  ·  7 = PhD or equivalent",
    question = "What is the highest educational level that you have attained?",
    cat_levels = names(edu_labels), cat_labels = edu_labels),

  gender = list(
    label = "Gender", item = "F1", kind = "nominal",
    values = trimws(as.character(df_in$F1)),
    levels = c("Man", "Woman", "Non-binary", "Other"),
    question = "Gender (shown before collapsing to the binary used in the models)",
    cat_levels = c("Man", "Woman", "Non-binary", "Other"),
    prose_labels = c(Man = "men", Woman = "women", `Non-binary` = "non-binary",
                     Other = "another gender")),

  denomination = list(
    label = "Religious denomination", item = "F6", kind = "nominal",
    values = trimws(as.character(df_in$F6)),
    levels = c("None", "Catholic", "Protestant", "Free", "Orthodox",
               "Islam", "Judaism", "Hinduism", "Buddhism", "Other"),
    display = c(None = "no denomination", Free = "Free Church",
                Judaism = "Jewish", Hinduism = "Hindu", Buddhism = "Buddhist"),
    question = "Which religious community, if any, do you belong to?",
    cat_levels = c("None", "Catholic", "Protestant", "Free", "Orthodox",
                   "Islam", "Judaism", "Hinduism", "Buddhism", "Other"),
    cat_labels = c(None = "No denomination", Free = "Free Church",
                   Judaism = "Jewish", Hinduism = "Hindu", Buddhism = "Buddhist"),
    prose_labels = c(None = "no denomination", Free = "Free Church",
                     Judaism = "Jewish", Hinduism = "Hindu", Buddhism = "Buddhist",
                     Other = "another denomination")),

  # Stakeholder category is not an item respondents answered directly. H1 asks
  # "Which of the following best describes your role in [case study area]?",
  # with options specific to each case study; data/groups.tsv groups those
  # options into categories that mean the same thing across case studies.
  stakeholder_category = list(
    label = "Stakeholder category", item = "H1", kind = "nominal",
    values = trimws(as.character(df_in$stakeholder_category)),
    levels = c("Local community", "Tourists", "Commercial", "Government",
               "Professionals and protectors", "Wild resource users"),
    question = paste("Grouped from H1, \"Which of the following best describes your",
                     "role in [case study area]?\", whose options differ between case",
                     "studies, into categories comparable across them."),
    prose_labels = c(`Local community` = "local community members",
                     Tourists = "tourists",
                     Commercial = "commercial stakeholders",
                     Government = "government stakeholders",
                     `Professionals and protectors` = "professionals and protectors",
                     `Wild resource users` = "wild resource users")),

  cpeb = list(
    label = "CPEB composite", item = "A26/A27/A28", kind = "cpeb",
    values = cpeb_raw, min = 1, max = 5,
    axis = "1 = Disagree  ·  5 = Agree",
    question = paste("Mean of the three CPEB items on their original 1-5 scale",
                     "(α = 0.77)")),

  cpeb_protest = list(
    label = "CPEB: protest", item = "A26", kind = "ordinal",
    values = as.numeric(df_in$A26), min = 1, max = 5,
    axis = "1 = Disagree  ·  5 = Agree",
    question = "I participate in protests against current environmental conditions."),

  cpeb_events = list(
    label = "CPEB: environmental events", item = "A27", kind = "ordinal",
    values = as.numeric(df_in$A27), min = 1, max = 5,
    axis = "1 = Disagree  ·  5 = Agree",
    question = "I participate in events organised by environmental groups."),

  cpeb_voting = list(
    label = "CPEB: voting", item = "A28", kind = "ordinal",
    values = as.numeric(df_in$A28), min = 1, max = 5,
    axis = "1 = Disagree  ·  5 = Agree",
    question = "I vote for a government proposing environmentally conscious policies.")
)

# Variables whose means are meaningful, and so appear in the numeric tables.
main_text_vars  <- c("cpeb", "religiosity", "anti_diversity", "left_right", "age")

# Added in the supplementary only: the three items the CPEB composite is built
# from.
supplement_vars <- c("cpeb_protest", "cpeb_events", "cpeb_voting")

# Described by category rather than by mean.
categorical_vars <- c("education", "gender", "denomination")

# Everything that gets a chart.
chart_vars <- c(main_text_vars, categorical_vars, "stakeholder_category",
                supplement_vars)


# ══ 1. Main-text descriptive table ════════════════════════════════════════════
# Continuous and ordinal variables whose means are meaningful. Education, gender
# and denomination are described by category instead (sections 2 and 4).

scale_range <- function(v) {
  if (v$kind == "age") "years (band midpoints)" else sprintf("%d-%d", v$min, v$max)
}

summary_row <- function(v, rows) {
  x  <- v$values[rows]
  ok <- !is.na(x)
  data.frame(
    Variable = v$label,
    Item     = v$item,
    n        = sum(ok),
    M        = sprintf("%.2f", mean(x[ok])),
    SD       = sprintf("%.2f", sd(x[ok])),
    Min      = sprintf("%g",   min(x[ok])),
    Max      = sprintf("%g",   max(x[ok])),
    `Scale range` = scale_range(v),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

main_summary <- do.call(rbind, lapply(desc_variables[main_text_vars],
                                      summary_row, rows = main_rows))

n_f17_missing <- sum(is.na(f17_modelled[main_rows]))
n_f17_dk      <- sum(f17_raw[main_rows] %in% "DK")
n_age_missing <- sum(is.na(desc_variables$age$values[main_rows]))

# The footnote travels with the table, so the numbers cannot drift apart from it.
main_footnote <- c(
  sprintf(paste("Note. Descriptives are for the %d respondents entering Models 2",
                "and 3 with ideology operationalised as anti-diversity, the primary",
                "operationalisation. The other two analysis samples differ:",
                "Model 1 n = %d, and Models 2 and 3 with ideology operationalised",
                "as left-right n = %d. Descriptives for all three samples are",
                "given in the supplementary materials."),
          sum(main_rows), sum(samples$model1$rows), sum(samples$left_right$rows)),
  sprintf(paste("Left-right ideology is missing for %d respondents in this sample,",
                "and a further %d answered \"Don't know\", which is recoded to the",
                "scale midpoint (4) as pre-registered and is included in the values",
                "summarised here. Age is missing for %d respondents. Anti-diversity",
                "is F16 reverse-coded, so a high score indicates disagreeing that",
                "diversity enriches one's country. CPEB is the mean of its three",
                "items on their original 1-5 scale. Education, gender and religious",
                "denomination are described by category rather than by mean."),
          n_f17_missing, n_f17_dk, n_age_missing)
)

write_table_xlsx(main_summary, file.path(out_dir, "descriptives_summary.xlsx"),
                 note = main_footnote)

cat("\n\nMain-text descriptives (anti-diversity sample)\n\n")
print(main_summary, row.names = FALSE)


# ══ 2. Main text: sample description ══════════════════════════════════════════
# Sample sizes, then one passage per categorical variable, for the main text.
# They are written to main_text_descriptives.md, with the Measures figures, in
# section 7.
# Gender is described before the collapse to the binary the models use.
# Education is summarised as the share holding a university degree: listing all
# seven of its qualitatively distinct levels makes for an unreadable sentence,
# and the full breakdown is in the category table in section 4.

# The lowest education level that is a university degree: "University-level
# education, Bachelor or equivalent". Levels 5 to 7 therefore count.
UNIVERSITY_LEVEL <- 5

# Categories are listed most common first, which is what a Participants section
# wants; the tables and charts keep questionnaire order instead. Ties keep
# questionnaire order too.
proportion_sentence <- function(v, rows) {
  x <- v$values[rows]
  x[is.na(x) | !nzchar(x)] <- NA

  observed <- unique(x[!is.na(x)])
  observed <- c(v$levels[v$levels %in% observed], sort(setdiff(observed, v$levels)))
  counts   <- vapply(observed, function(l) sum(x %in% l), integer(1))
  by_count <- order(counts, decreasing = TRUE)
  observed <- observed[by_count]
  counts   <- counts[by_count]
  total    <- sum(counts)

  wording <- observed
  named   <- observed %in% names(v$prose_labels)
  wording[named] <- unname(v$prose_labels[observed[named]])

  parts <- sprintf("%d %s (%.1f%%)", counts, wording, 100 * counts / total)
  list(parts = parts, total = total, missing = sum(is.na(x)))
}

as_list <- function(parts) {
  paste0(paste(head(parts, -1), collapse = ", "), ", and ", tail(parts, 1))
}

gender      <- proportion_sentence(desc_variables$gender,               main_rows)
denom       <- proportion_sentence(desc_variables$denomination,         main_rows)
stakeholder <- proportion_sentence(desc_variables$stakeholder_category, main_rows)

education   <- desc_variables$education$values[main_rows]
n_education <- sum(!is.na(education))
n_degree    <- sum(education >= UNIVERSITY_LEVEL, na.rm = TRUE)

# The full sample is every respondent, whether or not they can enter a model.
# Respondents from a case study that contributes to no model at all are
# described separately, together with the reason, where the reason is that the
# religiosity item was never asked there.
modelled_cases <- unique(df$case[samples$model1$rows])
in_modelled    <- df$case %in% modelled_cases
other_cases    <- unique(df$case[!in_modelled])
n_other        <- sum(!in_modelled)
other_lack_religiosity <- n_other > 0 && all(is.na(df$relig[!in_modelled]))

other_sentence <- if (n_other == 0) {
  "All of them are from case studies that contribute to the models."
} else {
  sprintf(paste("Of these, %d are from the %d case studies that contribute to the",
                "models; the remaining %d are from %s%s, so none of them can enter",
                "any model."),
          sum(in_modelled), length(modelled_cases), n_other,
          if (length(other_cases) == 1) "a further case study" else
            sprintf("%d further case studies", length(other_cases)),
          if (other_lack_religiosity)
            " whose questionnaire did not include the religiosity item (F7)" else "")
}

sample_lines <- c(
  "## Sample sizes",
  "",
  sprintf("- Full sample: %d respondents. %s", nrow(df), other_sentence),
  sprintf("- Primary sample (Model 3, ideology as anti-diversity): %d", sum(main_rows)),
  sprintf("- Model 1: %d", sum(samples$model1$rows)),
  sprintf("- Model 3, ideology as left-right: %d", sum(samples$left_right$rows)),
  "",
  sprintf("## Participants (primary sample, n = %d)", sum(main_rows)),
  "",
  "### Gender",
  "",
  sprintf(paste("Gender was reported by all %d respondents in this sample, because",
                "it is a model covariate and the models are complete-case. Before",
                "the pre-registered collapse to non-man versus man, the sample",
                "comprised %s."),
          gender$total, as_list(gender$parts)),
  "",
  "### Education",
  "",
  sprintf(paste("Education was likewise reported by all %d respondents, as it is",
                "also a model covariate. Of these, %d (%.1f%%) held a university",
                "degree, that is, a bachelor's degree or equivalent or higher."),
          n_education, n_degree, 100 * n_degree / n_education),
  "",
  "### Religious denomination",
  "",
  sprintf(paste("Religious denomination was reported by %d respondents (%d did not",
                "answer): %s."),
          denom$total, denom$missing, as_list(denom$parts)),
  "",
  "### Stakeholder category",
  "",
  sprintf(paste("Stakeholder category, grouped from each respondent's role in the",
                "case study area, was known for %d respondents (%d could not be",
                "categorised): %s."),
          stakeholder$total, stakeholder$missing, as_list(stakeholder$parts)),
  ""
)

cat("\n\n", paste(sample_lines, collapse = "\n"), "\n", sep = "")


# ══ 3. Supplementary: numeric descriptives, full sample and analysis samples ══
# n, M and SD side by side, one block of columns per sample under a merged header
# naming it, so the reader can see how little the samples differ on the
# variables they share. This replaces a per-sample repeat of the charts. The
# first block is the full sample: every respondent, including those who enter
# no model.
#
# Columns are built under unique names, because rbind() matches data frames by
# column name, and are given their repeated display names ("n", "M", "SD" once
# per sample) only after the table is assembled.

table_samples <- c(
  list(full = list(label = "Full sample", rows = rep(TRUE, nrow(df)))),
  samples
)

# The merged headers above each sample's block of columns.
sample_clusters <- function(sample_list, first_col, per_sample) {
  lapply(seq_along(sample_list), function(k) {
    from <- first_col + (k - 1) * per_sample
    list(label = sample_list[[k]]$label, from = from, to = from + per_sample - 1)
  })
}

by_sample <- do.call(rbind, lapply(
  desc_variables[c(main_text_vars, supplement_vars)],
  function(v) {
    row <- data.frame(Variable = v$label, Item = v$item,
                      check.names = FALSE, stringsAsFactors = FALSE)
    for (key in names(table_samples)) {
      x  <- v$values[table_samples[[key]]$rows]
      ok <- !is.na(x)
      row[[paste(key, "n")]]  <- sum(ok)
      row[[paste(key, "M")]]  <- sprintf("%.2f", mean(x[ok]))
      row[[paste(key, "SD")]] <- sprintf("%.2f", sd(x[ok]))
    }
    row
  }
))
names(by_sample) <- c("Variable", "Item",
                      rep(c("n", "M", "SD"), length(table_samples)))

write_table_xlsx(
  by_sample, file.path(supp_dir, "descriptives_by_sample.xlsx"),
  clusters = sample_clusters(table_samples, first_col = 3, per_sample = 3),
  note = paste("Note. The full sample is every respondent, including those who",
               "enter no model. Education, gender and religious denomination are",
               "described by category in the accompanying table rather than by",
               "mean.")
)

cat("\n\nNumeric descriptives, full sample and analysis samples\n\n")
print(by_sample, row.names = FALSE)


# ══ 4. Supplementary: categorical descriptives, full sample and analysis samples
# Education, gender and denomination as counts and percentages, one block of
# columns per sample, the full sample first. Percentages are of those who
# answered, matching
# the Participants prose; anyone who did not answer is its own row.

category_block <- function(v, rows, key) {
  x <- v$values[rows]
  x[is.na(x) | !nzchar(x)] <- NA

  present  <- unique(x[!is.na(x)])
  ordered  <- c(v$cat_levels[v$cat_levels %in% present],
                sort(setdiff(present, v$cat_levels)))
  counts   <- c(vapply(ordered, function(l) sum(x %in% l), integer(1)),
                `Not reported` = sum(is.na(x)))
  answered <- sum(counts) - counts[["Not reported"]]

  out <- data.frame(
    n       = as.integer(counts),
    percent = c(sprintf("%.1f", 100 * head(counts, -1) / answered), ""),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(out) <- paste(key, c("n", "%"))
  list(levels = names(counts), block = out)
}

by_category <- do.call(rbind, lapply(desc_variables[categorical_vars], function(v) {
  blocks <- lapply(names(table_samples),
                   function(key) category_block(v, table_samples[[key]]$rows, key))

  level_codes <- blocks[[1]]$levels
  named       <- level_codes %in% names(v$cat_labels)
  level_names <- level_codes
  level_names[named] <- unname(v$cat_labels[level_codes[named]])

  counts <- do.call(cbind, lapply(blocks, function(b) b$block))

  # Gender and education are model covariates, so the complete-case models leave
  # neither unreported anywhere; a row of zeros only adds noise.
  keep <- !(level_codes == "Not reported" &
              rowSums(counts[, grepl(" n$", names(counts)), drop = FALSE]) == 0)

  # Only the first kept row of a variable carries its name, so the block reads as
  # one unit once it is in a document.
  cbind(
    data.frame(Variable = c(v$label, rep("", sum(keep) - 1)),
               Category = level_names[keep],
               check.names = FALSE, stringsAsFactors = FALSE),
    counts[keep, , drop = FALSE]
  )
}))
names(by_category) <- c("Variable", "Category",
                        rep(c("n", "%"), length(table_samples)))

write_table_xlsx(
  by_category, file.path(supp_dir, "descriptives_categories_by_sample.xlsx"),
  clusters = sample_clusters(table_samples, first_col = 3, per_sample = 2),
  note = paste("Note. Percentages are of those who answered the item, so",
               "the \"Not reported\" row carries a count but no percentage.",
               "Gender is shown before the pre-registered collapse to",
               "non-man versus man. Gender and education are model",
               "covariates, so the complete-case models leave neither",
               "unreported in any analysis sample; their \"Not reported\"",
               "counts come from the full sample only.")
)

cat("\n\nCategorical descriptives, full sample and analysis samples\n\n")
print(by_category, row.names = FALSE)


# ══ 5. Main text: zero-order correlations ═════════════════════════════════════
# Among the variables entering the registered models, in the main analysis
# sample. Reported as r below the diagonal and n above it, which is the compact
# form that survives being pasted into a document.

corr_vars <- c(cpeb = "CPEB", relig = "Religiosity", anti_div = "Anti-diversity",
               left_right = "Left-right", edu = "Education", non_man = "Gender (non-man)")

corr_result <- psych::corr.test(df[main_rows, names(corr_vars)])

corr_table <- data.frame(Variable = unname(corr_vars),
                         stringsAsFactors = FALSE, check.names = FALSE)
for (j in seq_along(corr_vars)) {
  corr_table[[unname(corr_vars[j])]] <- vapply(seq_along(corr_vars), function(i) {
    if (i == j) "—"
    else if (i > j) sprintf("%.3f", corr_result$r[i, j])   # r below the diagonal
    else            sprintf("%d",  corr_result$n[i, j])    # n above it
  }, character(1))
}

write_table_xlsx(
  corr_table, file.path(out_dir, "correlations.xlsx"),
  note = paste("Note. Pearson correlations below the diagonal, pairwise n",
               "above it. Gender is coded 0 = man, 1 = non-man. Education",
               "is included here on its seven-point scale because a",
               "correlation needs a numeric coding, although it is",
               "described by category elsewhere. Left-right is missing",
               "for part of this sample, hence the smaller pairwise n.")
)

cat("\n\nZero-order correlations (anti-diversity sample)\n\n")
print(corr_table, row.names = FALSE)


# ══ 6. Descriptive charts ═════════════════════════════════════════════════════
#
# Charts of the analysis sample: only the case studies that contribute complete
# cases appear, and a pooled "All case studies" panel fills the ninth cell of the
# 3x3 grid.
#
# Bars are proportions within a panel. Substantive responses are blue; "Don't
# know" and missing are grey.

COL_VALUE    <- "steelblue"
COL_OFFSCALE <- "grey65"

# Wilson score interval on the mean rescaled to [min, max], so the bounds stay
# inside the response scale.
mean_ci_text <- function(x, lo, hi) {
  vals <- x[!is.na(x)]
  n    <- length(vals)
  if (n == 0L) return(NA_character_)
  m <- mean(vals)
  if (length(unique(vals)) <= 1L)
    return(sprintf("M = %.1f, 95%% CI unavailable", m))

  span   <- hi - lo
  p      <- (m - lo) / span
  z2     <- 1.96^2
  centre <- p + z2 / (2 * n)
  margin <- 1.96 * sqrt(p * (1 - p) / n + z2 / (4 * n^2))
  denom  <- 1 + z2 / n
  sprintf("M = %.1f, 95%% CI [%.1f, %.1f]", m,
          (centre - margin) / denom * span + lo,
          (centre + margin) / denom * span + lo)
}

# Sized for the charts being shrunk to page width in the supplementary .docx:
# they are drawn 210 mm wide and placed at about 160 mm.
chart_theme <- function(rotate_x = FALSE) {
  theme_bw(base_size = 11) %+replace%
    theme(
      strip.background   = element_rect(fill = "grey92", colour = NA),
      strip.text         = element_text(face = "bold", size = 9.5, lineheight = 1.2,
                                        margin = margin(3, 3, 3, 3, "mm")),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = if (rotate_x) element_text(angle = 45, hjust = 1, size = 8.5)
                           else element_text(size = 9),
      axis.text.y        = element_text(size = 9),
      axis.title         = element_text(size = 10.5),
      # A bottom margin on the title, because %+replace% drops the default one and
      # the subtitle's first line otherwise crowds it.
      plot.title         = element_text(face = "bold", size = 13.5, hjust = 0,
                                        margin = margin(b = 2, unit = "mm")),
      plot.subtitle      = element_text(size = 10.5, hjust = 0, colour = "grey25",
                                        lineheight = 1.3,
                                        margin = margin(b = 4, unit = "mm")),
      plot.margin        = margin(8, 8, 8, 8, "mm")
    )
}

# The eight contributing case studies plus a pooled panel, in that order.
panel_groups <- function(rows) {
  case <- unname(case_labels[df$case[rows]])
  c(split(seq_along(case), case), list(`All case studies` = seq_along(case)))
}

# Panels ordered with the pooled one last, whatever the case-study names sort to.
ordered_panels <- function(d) {
  pooled <- unique(d$panel[d$pooled])
  factor(d$panel, levels = c(sort(setdiff(unique(d$panel), pooled)), pooled))
}

panel_frame <- function(v, rows) {
  x <- v$values[rows]

  # Displayed value: the special level (F17's "DK") is shown as its own bar, so
  # it is taken from the raw column rather than the modelled one.
  shown <- if (!is.null(v$special)) {
    ifelse(v$raw[rows] %in% v$special, v$special, as.character(x))
  } else {
    as.character(x)
  }
  shown[is.na(shown) | !nzchar(shown)] <- "missing"

  levels_in_order <- if (v$kind == "nominal") {
    present <- unique(shown[shown != "missing"])
    c(v$levels[v$levels %in% present], sort(setdiff(present, v$levels)))
  } else {
    as.character(v$min:v$max)
  }
  bar_levels <- c(levels_in_order, v$special, "missing")

  groups <- panel_groups(rows)

  do.call(rbind, lapply(names(groups), function(g) {
    idx <- groups[[g]]
    strip <- if (v$kind == "nominal") {
      sprintf("%s\nn = %d", g, length(idx))
    } else {
      # M and the CI use the variable exactly as the models see it, so that a
      # strip label and the corresponding table row cannot disagree. For F17
      # that means "Don't know" counts as 4 even though it gets its own bar.
      sprintf("%s\nn = %d\n%s", g, length(idx), mean_ci_text(x[idx], v$min, v$max))
    }
    counts <- table(factor(shown[idx], levels = bar_levels))
    data.frame(
      panel    = strip,
      level    = factor(bar_levels, levels = bar_levels),
      n        = as.integer(counts),
      prop     = as.integer(counts) / length(idx),
      on_scale = !bar_levels %in% c(v$special, "missing"),
      pooled   = g == "All case studies",
      stringsAsFactors = FALSE
    )
  }))
}

bar_chart <- function(v, rows) {
  d <- panel_frame(v, rows)

  # Keep every scale point even where nobody chose it, so panels stay comparable;
  # drop only unused grey bars and unused nominal categories.
  used <- tapply(d$n, d$level, sum)
  keep <- names(used)[used > 0 | (d$on_scale[match(names(used), d$level)] &
                                    v$kind != "nominal")]
  d <- d[d$level %in% keep, ]
  d$level <- droplevels(d$level)
  d$panel <- ordered_panels(d)

  display_levels <- levels(d$level)
  if (!is.null(v$display)) {
    named <- display_levels %in% names(v$display)
    display_levels[named] <- unname(v$display[display_levels[named]])
  }

  ggplot(d, aes(x = level, y = prop, fill = on_scale)) +
    geom_col() +
    facet_wrap(~panel, ncol = 3) +
    # Long category names (e.g. "Professionals and protectors") are wrapped, or
    # at a 45-degree angle they run off the bottom-left of the chart.
    scale_x_discrete(labels = vapply(display_levels, function(l) {
      paste(strwrap(l, width = 16), collapse = "\n")
    }, character(1))) +
    scale_fill_manual(values = c(`TRUE` = COL_VALUE, `FALSE` = COL_OFFSCALE),
                      guide = "none") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.06))) +
    labs(title = sprintf("%s (%s)", v$label, v$item),
         subtitle = paste(strwrap(v$question, width = 95), collapse = "\n"),
         x = v$axis, y = NULL) +
    # Nominal labels are words; beyond eight bars (F17's seven points, "DK" and
    # "missing") even short labels collide unless angled.
    chart_theme(rotate_x = v$kind == "nominal" || nlevels(d$level) > 8)
}

# Age is continuous-ish (band midpoints) and CPEB is a mean of three 1-5 items,
# so both need a histogram rather than one bar per scale point. Counts differ by
# an order of magnitude between a single case study and the pooled panel, so
# these get a free y scale where the proportion-based bar charts do not.
histogram_chart <- function(v, rows, binwidth = NULL, bins = NULL, breaks = NULL) {
  x      <- v$values[rows]
  groups <- panel_groups(rows)

  d <- do.call(rbind, lapply(names(groups), function(g) {
    vals <- x[groups[[g]]]
    vals <- vals[!is.na(vals)]
    summary_line <- if (is.null(v$min)) {
      sprintf("M = %.1f, SD = %.1f", mean(vals), sd(vals))
    } else {
      mean_ci_text(vals, v$min, v$max)
    }
    data.frame(panel  = sprintf("%s\nn = %d\n%s", g, length(vals), summary_line),
               value  = vals,
               pooled = g == "All case studies",
               stringsAsFactors = FALSE)
  }))
  d$panel <- ordered_panels(d)

  p <- ggplot(d, aes(x = value)) +
    facet_wrap(~panel, ncol = 3, scales = "free_y") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
    labs(title = sprintf("%s (%s)", v$label, v$item),
         subtitle = paste(strwrap(v$question, width = 95), collapse = "\n"),
         x = v$axis, y = "Respondents") +
    chart_theme()

  p <- p + if (is.null(binwidth)) {
    geom_histogram(bins = bins, fill = COL_VALUE, colour = "white", linewidth = 0.3)
  } else {
    geom_histogram(binwidth = binwidth, center = v$min, fill = COL_VALUE,
                   colour = "white", linewidth = 0.2)
  }

  if (!is.null(breaks)) p <- p + scale_x_continuous(breaks = breaks)
  p
}

for (name in chart_vars) {
  v <- desc_variables[[name]]
  p <- switch(v$kind,
              age  = histogram_chart(v, main_rows, bins = 10),
              # CPEB's attainable values are multiples of 1/6, so one bin per
              # attainable value rather than an arbitrary bin count.
              cpeb = histogram_chart(v, main_rows, binwidth = 1/6,
                                     breaks = v$min:v$max),
              bar_chart(v, main_rows))

  path <- file.path(chart_dir, paste0(name, ".png"))
  ggsave(path, p, width = 210, height = 250, units = "mm", dpi = 200)
  message("Wrote ", path)
}

# ══ 7. Main text: measures, and the main-text numbers file ════════════════════
# The reliability figures the Measures section quotes.
#
# CPEB's Cronbach's alpha is given for the primary sample and for all
# respondents; the latter is what the pre-registered decision rule (drop an item
# if alpha < 0.5) was applied to in src/data_prep.R. The F9/F16 correlation is
# likewise over all respondents, since it is what decided (r < 0.3) that
# anti-diversity alone would stand for social conservatism.
#
# main_text_descriptives.md then collects every number this script generates for
# the main text that is in no output table, plus the Participants passages,
# which restate some Table S3 counts as prose ready for the manuscript.

cpeb_items <- c("A26", "A27", "A28")
cronbach_alpha <- function(rows) {
  psych::alpha(df_in[rows, cpeb_items], warnings = FALSE)$total$raw_alpha
}

alpha_main <- cronbach_alpha(main_rows)
alpha_all  <- cronbach_alpha(rep(TRUE, nrow(df_in)))

# Reverse-coding both items leaves their correlation unchanged, so the raw items
# serve.
soc_con_pairs <- complete.cases(df_in[, c("F9", "F16")])
r_soc_con     <- cor(as.numeric(df_in$F9[soc_con_pairs]),
                     as.numeric(df_in$F16[soc_con_pairs]))

measures_lines <- c(
  "## Measures",
  "",
  sprintf(paste("- CPEB (A26, A27, A28), Cronbach's α: %.2f in the primary sample",
                "(n = %d); %.2f across all respondents"),
          alpha_main, sum(main_rows), alpha_all),
  sprintf(paste("- Social conservatism items F9 and F16 (both reverse-coded):",
                "r = %.3f across the %d respondents who answered both"),
          r_soc_con, sum(soc_con_pairs)),
  ""
)

cat("\n\n", paste(measures_lines, collapse = "\n"), "\n", sep = "")

write_utf8(
  c("# Main-text numbers: descriptives",
    "",
    paste("Generated by src/religion_descriptives.R: every number it produces for",
          "the main text that is in no output table, plus the Participants",
          "passages, which restate some Table S3 counts as prose."),
    "",
    sample_lines,
    measures_lines),
  file.path(out_dir, "main_text_descriptives.md")
)


# ══ 8. Supplementary: partners and recruitment methods ════════════════════════
# The recruitment description for each case study, from
# data/recruitment_methods.csv, with the number of respondents each contributed
# to the full sample added.

recruitment <- read.csv("data/recruitment_methods.csv",
                        check.names = FALSE, stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8-BOM")

# Rows are matched to case studies by the country named at the start of the
# first column ("Estonia, Kloogaranna-Laulasmaa area" -> ee).
case_codes  <- setNames(names(case_labels), case_labels)
row_country <- sub(",.*$", "", recruitment[[1]])
n_by_case   <- table(df$case)

recruitment[["N (full sample)"]] <- as.integer(n_by_case[case_codes[row_country]])
stopifnot(!anyNA(recruitment[["N (full sample)"]]))

write_table_xlsx(recruitment, file.path(supp_dir, "recruitment.xlsx"))

cat("\n\nPartners and recruitment methods\n\n")
print(recruitment[, c(1, 2, 4)], row.names = FALSE)


message("\nDone. Descriptives in ", out_dir)
