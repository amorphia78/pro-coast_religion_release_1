# ══════════════════════════════════════════════════════════════════════════════
# Load the released survey data and build the model variables
#
# Sourced by src/religion_analysis.R and src/religion_descriptives.R. Provides:
#   df_in  the data file as released, on the original response scales
#   df     the variables as the models use them, z-scored
#
# Everything is standardised across all 821 respondents in the file, including
# the Montenegro respondents, who enter no model because their questionnaire did
# not include the religiosity item (F7). See data/codebook.md.
# ══════════════════════════════════════════════════════════════════════════════

library(psych)

scale_items <- function(df, vars, reversed = NULL) {
  # One item: its z-score.
  # Several items: z-score each, take the row mean, then z-score that composite.
  # Cronbach's alpha is reported for a composite.
  n <- length(vars)

  if (is.null(reversed)) reversed <- rep(FALSE, n)

  items <- df[, vars, drop = FALSE]

  for (i in seq_len(n)) {
    if (reversed[i]) items[[i]] <- -items[[i]]
  }

  if (n == 1) return(as.vector(scale(items[[1]])))

  alpha_result <- psych::alpha(items, warnings = FALSE)
  message(paste(vars, collapse = "/"), " alpha = ", round(alpha_result$total$raw_alpha, 3))

  combined <- rowMeans(scale(items), na.rm = TRUE)
  combined[is.nan(combined)] <- NA

  # Averaging z-scored items yields SD < 1 (the shortfall grows as the items
  # correlate less), so the composite is z-scored in turn. The pre-registration
  # requires every variable entering a model to be standardised, and the priors
  # are specified for standardised regression coefficients.
  as.vector(scale(combined))
}

df_in <- read.delim("data/procoast_wave2_religion_cpeb.tsv", stringsAsFactors = FALSE)

# F17 (left-right ideology). "DK" (don't know) is recoded to the neutral midpoint
# (4) per the pre-registration. Blank (no response or "Prefer not to say") becomes
# NA via numeric coercion, with the warning suppressed. The result is z-scored for
# use in the models.
f17_numeric <- suppressWarnings(as.numeric(df_in$F17))
f17_numeric[!is.na(df_in$F17) & df_in$F17 == "DK"] <- 4

# F1 (gender). Per the pre-registration, gender is collapsed to non-man (woman,
# non-binary, other) vs man, with "Prefer not to say" and non-response set to
# missing. F1 is a character column and read.delim() reads its blank cells as ""
# rather than NA, so blanks must be made missing explicitly: otherwise
# `"" == "Man"` is FALSE and every respondent who gave no gender is silently
# counted as non-man.
f1_clean <- df_in$F1
f1_clean[!is.na(f1_clean) & !nzchar(trimws(f1_clean))] <- NA
non_man_vec <- ifelse(is.na(f1_clean), NA, ifelse(f1_clean == "Man", 0, 1))

message(sprintf("Gender: %d man, %d non-man, %d missing",
                sum(non_man_vec == 0, na.rm = TRUE),
                sum(non_man_vec == 1, na.rm = TRUE),
                sum(is.na(non_man_vec))))

df <- data.frame(
  cpeb        = scale_items(df_in, c("A26", "A27", "A28")),
  relig       = scale_items(df_in, c("F7")),
  anti_div    = scale_items(df_in, c("F16"), reversed = c(TRUE)),
  left_right  = as.vector(scale(f17_numeric)),
  case        = df_in$case,
  edu         = scale_items(df_in, c("F4")),
  non_man     = non_man_vec,
  non_man_std = as.vector(scale(non_man_vec))
)
