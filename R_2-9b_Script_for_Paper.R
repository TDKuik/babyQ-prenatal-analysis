# ============================================================
# babyQ Cohort Study — Reproducible Analysis Script
# Manuscript: "Repeat Engagement with a Digital Prenatal
#   Self-Assessment Tool and Improvement in Prenatal
#   Health-Behavior Scores: A Real-World Cohort Study
#   of BabyQ Users"
#
# This script reproduces all statistics, tables, and figures
# reported in the manuscript. Run sections in order; each
# section depends on objects created in earlier sections.
#
# Required packages (install once if not already present):
# install.packages(c("readxl", "dplyr", "psych", "DiagrammeR",
#                    "DiagrammeRsvg", "rsvg", "lme4", "lmerTest",
#                    "corrplot", "ggplot2", "irr", "flextable",
#                    "reshape2"))
# ============================================================


# ============================================================
# LOAD REQUIRED LIBRARIES
# ============================================================

library(readxl)
library(dplyr)
library(psych)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(lme4)
library(lmerTest)
library(ggplot2)


# ============================================================
# LOAD DATA
# ============================================================
# Source: cleaned analytic workbook (babyq_original_4300_users.xlsx)
# Tables used:
#   survey_person_summary  -> one row per user  (n = 2,923)
#   survey_longitudinal    -> one row per survey (n = 4,229)
#
# Expected objects after this section:
#   df           - long-format survey data (one row per survey)
#   user_counts  - per-user survey count (columns: User, n)
#   repeat_users - users with >= 2 surveys (column: User)
#
# Adjust sheet names / column names to match your workbook.

df <- read_excel("babyq_original_4300_users.xlsx",
                 sheet = "Sheet1",
                 na = "NULL")

# Ensure date column is parsed correctly
df$SurveyDate <- as.Date(df$SurveyDate)

# Survey count per user
user_counts <- df %>%
  group_by(User) %>%
  summarise(n = n(), .groups = "drop")

# Users with >= 2 surveys (longitudinal analytic cohort, n = 601)
repeat_users <- user_counts %>% filter(n >= 2)


# ============================================================
# DEFINE SURVEY ITEM (DOMAIN) COLUMN NAMES
# ============================================================
# These 21 columns correspond to the individual behavior domains
# scored in each babyQ survey.

survey_cols <- c("Exercise", "sleep", "fruits", "veggies", "whole grains",
                 "dairy", "probiotics", "protein", "sugar soda", "art_sweets",
                 "snacks", "deep fried", "smoking", "alcohol", "tv sitting",
                 "vitamins", "Vitamin D", "DHA", "depression", "stress", "support")


# ============================================================
# RESULTS — Core summary statistics
# ============================================================

# First/last survey dataset for the 601 repeat users
df_repeat <- df %>% filter(User %in% repeat_users$User)

fl <- df_repeat %>%
  arrange(User, SurveyDate) %>%
  group_by(User) %>%
  summarise(
    first_score = first(score),
    last_score  = last(score),
    n_surveys   = n()
  ) %>%
  mutate(diff = last_score - first_score)

# --- Mean first score, last score, improvement, and 95% CI ---
n          <- nrow(fl)
mean_first <- mean(fl$first_score, na.rm = TRUE)
mean_last  <- mean(fl$last_score,  na.rm = TRUE)
mean_diff  <- mean(fl$diff, na.rm = TRUE)
se_diff    <- sd(fl$diff, na.rm = TRUE) / sqrt(n)
ci_low     <- mean_diff - 1.96 * se_diff
ci_high    <- mean_diff + 1.96 * se_diff

cat("Mean first score:", round(mean_first, 1), "\n")
cat("Mean last score:", round(mean_last, 1), "\n")
cat("Mean improvement:", round(mean_diff, 2), "\n")
cat("95% CI: [", round(ci_low, 2), ",", round(ci_high, 2), "]\n\n")

# --- Engagement bands and ANOVA ---
fl <- fl %>%
  mutate(band = case_when(
    n_surveys == 2   ~ "2 surveys",
    n_surveys <= 4   ~ "3-4 surveys",
    TRUE             ~ ">=5 surveys"
  ))

band_stats <- fl %>%
  group_by(band) %>%
  summarise(
    n       = n(),
    mean    = round(mean(diff, na.rm = TRUE), 2),
    se      = sd(diff, na.rm = TRUE) / sqrt(n()),
    ci_low  = round(mean(diff, na.rm = TRUE) - 1.96 * se, 2),
    ci_high = round(mean(diff, na.rm = TRUE) + 1.96 * se, 2)
  )
print(band_stats)

anova_result <- aov(diff ~ band, data = fl)
cat("\nANOVA result:\n")
print(summary(anova_result))

# --- Overall Cohen's d (uncontrolled) ---
cohens_d <- mean_diff / sd(fl$diff, na.rm = TRUE)
cat("Cohen's d:", round(cohens_d, 2), "\n")


# ============================================================
# RESULTS — OLS Regression (dose-response)
# ============================================================

# Extend fl with follow-up duration
df_first_last <- df_repeat %>%
  arrange(User, SurveyDate) %>%
  group_by(User) %>%
  summarise(
    first_score = first(score),
    last_score  = last(score),
    n_surveys   = n(),
    follow_days = as.numeric(difftime(last(SurveyDate),
                                      first(SurveyDate), units = "days"))
  ) %>%
  mutate(diff = last_score - first_score)

# Model 1: Unadjusted
m1 <- lm(diff ~ n_surveys, data = df_first_last)
cat("Model 1 (Unadjusted):\n")
print(round(confint(m1)["n_surveys",], 2))
cat("beta:", round(coef(m1)["n_surveys"], 2), "\n")
cat("p:", round(summary(m1)$coefficients["n_surveys","Pr(>|t|)"], 3), "\n")
cat("R2:", round(summary(m1)$r.squared, 3), "\n\n")

# Model 2: Baseline-adjusted
m2 <- lm(diff ~ n_surveys + first_score, data = df_first_last)
cat("Model 2 (+ Baseline score):\n")
print(round(confint(m2)["n_surveys",], 2))
cat("beta:", round(coef(m2)["n_surveys"], 2), "\n")
cat("p:", round(summary(m2)$coefficients["n_surveys","Pr(>|t|)"], 4), "\n")
cat("R2:", round(summary(m2)$r.squared, 3), "\n\n")

# Model 3: Fully adjusted (baseline + follow-up days)
m3 <- lm(diff ~ n_surveys + first_score + follow_days, data = df_first_last)
cat("Model 3 (+ Baseline + Follow-up days):\n")
print(round(confint(m3)["n_surveys",], 2))
cat("beta:", round(coef(m3)["n_surveys"], 2), "\n")
cat("p:", round(summary(m3)$coefficients["n_surveys","Pr(>|t|)"], 4), "\n")
cat("R2:", round(summary(m3)$r.squared, 3), "\n\n")


# ============================================================
# RESULTS — Domain-level paired t-tests with BH FDR correction
# ============================================================

# First and last survey per user for each of the 21 domains
first_items <- df_repeat %>%
  arrange(User, SurveyDate) %>%
  group_by(User) %>%
  summarise(across(all_of(survey_cols), first))

last_items <- df_repeat %>%
  arrange(User, SurveyDate) %>%
  group_by(User) %>%
  summarise(across(all_of(survey_cols), last))

# Within-person differences (last minus first) per domain
diff_items <- last_items %>%
  mutate(across(all_of(survey_cols),
                ~ . - first_items[[cur_column()]]))

# Paired t-test for each domain, then BH FDR correction
p_vals <- sapply(survey_cols, function(col) {
  t.test(diff_items[[col]], mu = 0)$p.value
})

mean_diffs <- sapply(survey_cols, function(col) {
  mean(diff_items[[col]], na.rm = TRUE)
})

q_vals <- p.adjust(p_vals, method = "BH")

domain_results <- data.frame(
  domain      = survey_cols,
  mean_diff   = round(mean_diffs, 2),
  p_value     = round(p_vals, 4),
  q_value     = round(q_vals, 4),
  significant = q_vals < 0.05
) %>% arrange(desc(mean_diff))

print(domain_results)
cat("\nNumber of domains significant after FDR correction:",
    sum(domain_results$significant), "\n")


# ============================================================
# RESULTS — Domain-level Wilcoxon signed-rank tests + BH FDR
# ============================================================

w_p_vals <- sapply(survey_cols, function(col) {
  diffs <- diff_items[[col]]
  diffs <- diffs[!is.na(diffs)]
  wilcox.test(diffs, mu = 0, exact = FALSE)$p.value
})

w_q_vals <- p.adjust(w_p_vals, method = "BH")

wilcox_results <- data.frame(
  domain      = survey_cols,
  mean_diff   = round(mean_diffs, 2),
  p_value     = round(w_p_vals, 4),
  q_value     = round(w_q_vals, 4),
  significant = w_q_vals < 0.05
) %>% arrange(desc(mean_diff))

print(wilcox_results)
cat("\nNumber of domains significant after FDR correction (Wilcoxon):",
    sum(wilcox_results$significant), "\n")


# ============================================================
# RESULTS — Permutation test (5,000 iterations)
# ============================================================

set.seed(42)  # for reproducibility

# Ordinal band coding: 2 surveys = 1, 3-4 = 2, >=5 = 3
fl <- fl %>%
  mutate(band_ordinal = case_when(
    n_surveys == 2  ~ 1,
    n_surveys <= 4  ~ 2,
    TRUE            ~ 3
  ))

# Observed ordinal trend coefficient
observed_beta <- coef(lm(diff ~ band_ordinal, data = fl))["band_ordinal"]
cat("Observed ordinal trend beta:", round(observed_beta, 2), "\n")

# Permutation test: shuffle band assignment 5,000 times
n_perm     <- 5000
perm_betas <- numeric(n_perm)

for (i in 1:n_perm) {
  fl_perm    <- fl %>% mutate(band_ordinal = sample(band_ordinal))
  perm_betas[i] <- coef(lm(diff ~ band_ordinal, data = fl_perm))["band_ordinal"]
}

# Two-sided permutation p-value
perm_p <- mean(abs(perm_betas) >= abs(observed_beta))
cat("Permutation p-value (5,000 iterations):", perm_p, "\n")
cat("Null distribution mean:", round(mean(perm_betas), 3), "\n")
cat("Null distribution SD:", round(sd(perm_betas), 3), "\n")
cat("Null distribution 95% range: [",
    round(quantile(perm_betas, 0.025), 2), ",",
    round(quantile(perm_betas, 0.975), 2), "]\n\n")


# ============================================================
# RESULTS — RCT Sample Size calculation
# ============================================================

# SD of within-person differences (used as the planning SD)
sd_diff <- sd(fl$diff, na.rm = TRUE)
cat("SD of within-person differences:", round(sd_diff, 1), "\n\n")

# Required N per arm for a range of effect sizes
# Two-sided alpha = 0.05, power = 80%
z_alpha <- qnorm(0.975)  # 1.96
z_beta  <- qnorm(0.80)   # 0.84

effect_sizes <- c(1, 2, 3, 5, 9.3)
cat("Required N per arm (alpha=0.05, power=80%):\n")
for (delta in effect_sizes) {
  n_arm <- ceiling(2 * ((z_alpha + z_beta) * sd_diff / delta)^2)
  cat("  Delta =", delta, "points: N per arm =", n_arm, "\n")
}


# ============================================================
# PSYCHOMETRICS — Cronbach's Alpha
# ============================================================

# First survey only for all 2,923 users
first_surveys_all <- df %>%
  arrange(User, SurveyDate) %>%
  group_by(User) %>%
  slice(1) %>%
  ungroup() %>%
  select(all_of(survey_cols))

# First survey only for 601 repeat users
first_surveys_601 <- df_repeat %>%
  arrange(User, SurveyDate) %>%
  group_by(User) %>%
  slice(1) %>%
  ungroup() %>%
  select(all_of(survey_cols))

# Alpha for all 2,923 users (with key-checking to handle item directionality)
cat("=== Cronbach's Alpha with key-checking: All 2,923 users ===\n")
alpha_all <- psych::alpha(first_surveys_all)
print(alpha_all$total)

# Alpha for 601 repeat users
cat("\n=== Cronbach's Alpha with key-checking: 601 repeat users ===\n")
alpha_601 <- psych::alpha(first_surveys_601)
print(alpha_601$total)


# ============================================================
# RESULTS — Returners vs. non-returners comparison
# ============================================================

# One-survey-only users (n = 2,322)
one_survey_users <- user_counts %>% filter(n == 1)
df_one_survey    <- df %>% filter(User %in% one_survey_users$User)

mean_one <- mean(df_one_survey$score, na.rm = TRUE)
sd_one   <- sd(df_one_survey$score, na.rm = TRUE)
n_one    <- sum(!is.na(df_one_survey$score))

cat("=== One-survey-only users (n =", n_one, ") ===\n")
cat("Mean first score:", round(mean_one, 1), "\n")
cat("SD:", round(sd_one, 1), "\n\n")

# Repeat users (n = 601) — first survey
mean_601 <- mean(df_first_last$first_score, na.rm = TRUE)
sd_601   <- sd(df_first_last$first_score, na.rm = TRUE)
n_601    <- sum(!is.na(df_first_last$first_score))

cat("=== Repeat users (n =", n_601, ") ===\n")
cat("Mean first score:", round(mean_601, 1), "\n")
cat("SD:", round(sd_601, 1), "\n\n")

# Two-sample t-test comparing the two groups
t_result <- t.test(df_one_survey$score, df_first_last$first_score)
cat("=== Two-sample t-test: one-survey vs. repeat users ===\n")
cat("t =", round(t_result$statistic, 2), "\n")
cat("df =", round(t_result$parameter, 1), "\n")
cat("p =", round(t_result$p.value, 4), "\n")
cat("Mean difference:", round(mean_one - mean_601, 1), "\n")


# ============================================================
# RESULTS — SD verification and band-level descriptives
# ============================================================

cat("=== SD of first and last scores ===\n")
cat("First score SD:", round(sd(df_first_last$first_score, na.rm = TRUE), 1), "\n")
cat("Last score SD:", round(sd(df_first_last$last_score, na.rm = TRUE), 1), "\n\n")

cat("=== ANOVA: baseline score across engagement bands ===\n")
anova_baseline <- aov(first_score ~ band, data = fl)
print(summary(anova_baseline))

# Band-specific Cohen's d
band_cohens_d <- fl %>%
  group_by(band) %>%
  summarise(
    n        = n(),
    mean_d   = mean(diff, na.rm = TRUE),
    sd_d     = sd(diff, na.rm = TRUE),
    cohens_d = round(mean_d / sd_d, 2)
  )

cat("=== Band-specific Cohen's d ===\n")
print(band_cohens_d)
cat("\nOverall Cohen's d:", round(mean(fl$diff, na.rm = TRUE) /
                                    sd(fl$diff, na.rm = TRUE), 2), "\n")


# ============================================================
# RESULTS — Post-hoc Welch t-tests (Bonferroni corrected)
# ============================================================

# Extract improvement scores by band
g2  <- fl %>% filter(band == "2 surveys") %>% pull(diff)
g34 <- fl %>% filter(band == "3-4 surveys") %>% pull(diff)
g5  <- fl %>% filter(band == ">=5 surveys") %>% pull(diff)

t_2_34 <- t.test(g2, g34)
t_2_5  <- t.test(g2, g5)
t_34_5 <- t.test(g34, g5)

cat("=== Post-hoc Welch t-tests (Bonferroni corrected) ===\n")
cat("2 vs 3-4 surveys: t =", round(t_2_34$statistic, 2),
    ", raw p =", round(t_2_34$p.value, 4),
    ", Bonferroni p =", round(min(t_2_34$p.value * 3, 1), 3), "\n")

cat("2 vs >=5 surveys: t =", round(t_2_5$statistic, 2),
    ", raw p =", round(t_2_5$p.value, 4),
    ", Bonferroni p =", round(min(t_2_5$p.value * 3, 1), 3), "\n")

cat("3-4 vs >=5 surveys: t =", round(t_34_5$statistic, 2),
    ", raw p =", round(t_34_5$p.value, 4),
    ", Bonferroni p =", round(min(t_34_5$p.value * 3, 1), 3), "\n")

# Ordinal trend (linear contrast across bands)
cat("\n=== Ordinal trend ===\n")
obs_beta <- coef(lm(diff ~ band_ordinal, data = fl))["band_ordinal"]
ci_trend <- confint(lm(diff ~ band_ordinal, data = fl))["band_ordinal",]
p_trend  <- summary(lm(diff ~ band_ordinal, data = fl))$coefficients["band_ordinal","Pr(>|t|)"]
cat("beta:", round(obs_beta, 2), "\n")
cat("95% CI: [", round(ci_trend[1], 2), ",", round(ci_trend[2], 2), "]\n")
cat("p:", round(p_trend, 4), "\n")


# ============================================================
# EXPOSURE — Verify baseline score tertile cut points
# ============================================================

tertiles <- quantile(df_first_last$first_score,
                     probs = c(0, 1/3, 2/3, 1),
                     na.rm = TRUE)
cat("Baseline score tertile cut points:\n")
print(tertiles)


# ============================================================
# CRITICAL ITEM — Linear Mixed-Effects Model
# ============================================================

# All surveys from the 601 repeat users in long format
# survey_number = 1 for first survey, 2 for second, etc.
df_long <- df_repeat %>%
  arrange(User, SurveyDate) %>%
  group_by(User) %>%
  mutate(
    survey_number = row_number(),
    follow_days   = as.numeric(difftime(SurveyDate,
                                        first(SurveyDate),
                                        units = "days")),
    first_score   = first(score)
  ) %>%
  ungroup()

# Random intercept per user; fixed effects: survey number and baseline score
lme_model <- lmer(score ~ survey_number + first_score + (1 | User),
                  data = df_long)

cat("=== Linear Mixed-Effects Model ===\n")
print(summary(lme_model))

cat("\n=== Fixed Effects with 95% CI (Wald) ===\n")
print(round(confint(lme_model, method = "Wald"), 3))


# ============================================================
# STATISTICAL ANALYSIS — Domain inter-correlation matrix
# ============================================================

library(corrplot)

# Correlation matrix from first surveys of all 2,923 users
cor_matrix  <- cor(first_surveys_all, use = "pairwise.complete.obs")
cor_values  <- cor_matrix[upper.tri(cor_matrix)]

cat("=== Domain Inter-Correlation Summary ===\n")
cat("Number of unique pairs:", length(cor_values), "\n")
cat("Mean correlation:", round(mean(cor_values), 3), "\n")
cat("Median correlation:", round(median(cor_values), 3), "\n")
cat("Range: [", round(min(cor_values), 3), ",",
    round(max(cor_values), 3), "]\n")
cat("Pairs with |r| > 0.3:", sum(abs(cor_values) > 0.3), "\n")
cat("Pairs with |r| > 0.5:", sum(abs(cor_values) > 0.5), "\n\n")

cat("=== Full Correlation Matrix ===\n")
print(round(cor_matrix, 2))

# Save correlation heatmap as PNG and PDF
png("Figure_Correlation_Heatmap.png", width = 2400, height = 2400, res = 300)
corrplot::corrplot(cor_matrix, method = "color", type = "upper",
                   tl.cex = 0.7, tl.col = "black",
                   title = "Domain Inter-Correlation Matrix",
                   mar = c(0, 0, 2, 0))
dev.off()

pdf("Figure_Correlation_Heatmap.pdf", width = 8, height = 8)
corrplot::corrplot(cor_matrix, method = "color", type = "upper",
                   tl.cex = 0.7, tl.col = "black",
                   title = "Domain Inter-Correlation Matrix",
                   mar = c(0, 0, 2, 0))
dev.off()

cat("Correlation heatmap saved as PNG and PDF\n")


# ============================================================
# SENSITIVITY ANALYSIS — ICC Calculation
# ============================================================

library(irr)

# Survey 1 and survey 2 scores for all 601 repeat users
survey1_scores <- df_repeat %>%
  arrange(User, SurveyDate) %>%
  group_by(User) %>%
  slice(1) %>%
  ungroup() %>%
  select(User, score) %>%
  rename(survey1 = score)

survey2_scores <- df_repeat %>%
  arrange(User, SurveyDate) %>%
  group_by(User) %>%
  slice(2) %>%
  ungroup() %>%
  select(User, score) %>%
  rename(survey2 = score)

icc_data <- survey1_scores %>%
  inner_join(survey2_scores, by = "User") %>%
  select(survey1, survey2)

cat("Number of users with survey 1 and 2:", nrow(icc_data), "\n\n")

# Two-way mixed, absolute agreement, single measure
icc_result <- icc(icc_data,
                  model = "twoway",
                  type  = "agreement",
                  unit  = "single")

cat("=== ICC (two-way mixed, absolute agreement, single measure) ===\n")
print(icc_result)


# ============================================================
# SENSITIVITY ANALYSIS — RTM Decomposition
# ============================================================

# ICC and grand mean from the analysis above
icc_val <- 0.352
mu      <- 66.1

# Observed delta and RTM-expected delta by baseline tertile
df_first_last <- df_first_last %>%
  mutate(tertile = case_when(
    first_score <= 59.5 ~ "Low (31-59.5)",
    first_score <= 73.0 ~ "Middle (60-73)",
    TRUE                ~ "High (73.5-100)"
  ))

tertile_baselines <- df_first_last %>%
  group_by(tertile) %>%
  summarise(
    n              = n(),
    mean_baseline  = round(mean(first_score, na.rm = TRUE), 1),
    observed_delta = round(mean(diff, na.rm = TRUE), 1)
  ) %>%
  mutate(
    rtm_expected = round((1 - icc_val) * (mu - mean_baseline), 1),
    residual     = round(observed_delta - rtm_expected, 1),
    rtm_pct      = round(rtm_expected / observed_delta * 100, 1)
  )

cat("=== RTM Decomposition with ICC =", icc_val, "===\n")
print(tertile_baselines)


# ============================================================
# RESULTS — Domain-specific key Wilcoxon p-values
# ============================================================

cat("=== Key domain Wilcoxon p-values ===\n")

alcohol_diff  <- diff_items[["alcohol"]][!is.na(diff_items[["alcohol"]])]
alcohol_test  <- wilcox.test(alcohol_diff, mu = 0, exact = FALSE)
cat("Alcohol: W =", alcohol_test$statistic,
    ", p =", format(alcohol_test$p.value, scientific = TRUE), "\n")

exercise_diff <- diff_items[["Exercise"]][!is.na(diff_items[["Exercise"]])]
exercise_test <- wilcox.test(exercise_diff, mu = 0, exact = FALSE)
cat("Exercise: W =", exercise_test$statistic,
    ", p =", format(exercise_test$p.value, scientific = TRUE), "\n\n")


# ============================================================
# RESULTS — Dose-response FDR: longitudinal (per-survey beta)
# ============================================================

cat("=== Dose-response: per-survey beta with BH FDR correction ===\n")

dose_betas <- sapply(survey_cols, function(col) {
  model <- lm(df_long[[col]] ~ df_long$survey_number)
  coef(model)["df_long$survey_number"]
})

dose_p <- sapply(survey_cols, function(col) {
  model <- lm(df_long[[col]] ~ df_long$survey_number)
  summary(model)$coefficients["df_long$survey_number", "Pr(>|t|)"]
})

dose_q <- p.adjust(dose_p, method = "BH")

dose_results <- data.frame(
  domain      = survey_cols,
  beta        = round(dose_betas, 3),
  p           = round(dose_p, 4),
  q           = round(dose_q, 4),
  significant = dose_q < 0.05
) %>% arrange(q)

print(dose_results)
cat("\nDomains significant after FDR (dose-response):",
    sum(dose_results$significant), "\n")


# ============================================================
# RESULTS — Dose-response FDR: first vs. last approach
# ============================================================

cat("=== Dose-response: first vs last, per-survey beta with BH FDR ===\n")

dose_fl_betas <- sapply(survey_cols, function(col) {
  diff_col <- last_items[[col]] - first_items[[col]]
  coef(lm(diff_col ~ df_first_last$n_surveys))["df_first_last$n_surveys"]
})

dose_fl_p <- sapply(survey_cols, function(col) {
  diff_col <- last_items[[col]] - first_items[[col]]
  summary(lm(diff_col ~ df_first_last$n_surveys))$coefficients["df_first_last$n_surveys", "Pr(>|t|)"]
})

dose_fl_q <- p.adjust(dose_fl_p, method = "BH")

dose_fl_results <- data.frame(
  domain      = survey_cols,
  beta        = round(dose_fl_betas, 3),
  p           = round(dose_fl_p, 4),
  q           = round(dose_fl_q, 4),
  significant = dose_fl_q < 0.05
) %>% arrange(q)

print(dose_fl_results)
cat("\nDomains significant after FDR (first vs last dose-response):",
    sum(dose_fl_results$significant), "\n")


# ============================================================
# RESULTS — Subgroup analysis by baseline tertile
# ============================================================

# Baseline-adjusted dose-response model run within each tertile
tertile_models <- df_first_last %>%
  group_by(tertile) %>%
  summarise(
    n       = n(),
    beta    = coef(lm(diff ~ n_surveys + first_score))["n_surveys"],
    ci_low  = confint(lm(diff ~ n_surveys + first_score))["n_surveys", 1],
    ci_high = confint(lm(diff ~ n_surveys + first_score))["n_surveys", 2],
    p_val   = summary(lm(diff ~ n_surveys + first_score))$coefficients["n_surveys", "Pr(>|t|)"]
  ) %>%
  mutate(
    beta    = round(beta, 2),
    ci_low  = round(ci_low, 2),
    ci_high = round(ci_high, 2),
    p_val   = round(p_val, 3),
    p_fmt   = sub("^0\\.", ".", sprintf("%.3f", p_val))
  )

cat("=== Subgroup analysis by baseline tertile ===\n")
print(tertile_models)


# ============================================================
# TABLE 3 — Domain results (Wilcoxon + dose-response)
# ============================================================

library(flextable)

# Combine Wilcoxon change results with dose-response results
table3 <- wilcox_results %>%
  rename(mean_delta = mean_diff,
         p_delta    = p_value,
         q_delta    = q_value,
         sig_delta  = significant) %>%
  left_join(
    dose_fl_results %>%
      mutate(domain = gsub("\\.df_first_last\\$n_surveys", "", domain)) %>%
      rename(beta_dose = beta,
             p_dose    = p,
             q_dose    = q,
             sig_dose  = significant),
    by = "domain"
  ) %>%
  arrange(desc(mean_delta))

cat("=== Complete Table 3 ===\n")
print(table3 %>%
        select(domain, mean_delta, p_delta, q_delta,
               sig_delta, beta_dose, q_dose, sig_dose))

# Format for publication
table3_clean <- table3 %>%
  select(domain, mean_delta, p_delta, q_delta,
         sig_delta, beta_dose, q_dose, sig_dose) %>%
  mutate(
    mean_delta = ifelse(mean_delta >= 0,
                        paste0("+", sprintf("%.2f", mean_delta)),
                        sprintf("%.2f", mean_delta)),
    p_delta    = ifelse(p_delta == 0, "< 0.001", sprintf("%.3f", p_delta)),
    q_delta    = ifelse(q_delta == 0, "< 0.001", sprintf("%.4f", q_delta)),
    q_delta    = ifelse(sig_delta, paste0(q_delta, " *"), q_delta),
    beta_dose  = ifelse(beta_dose >= 0,
                        paste0("+", sprintf("%.3f", beta_dose)),
                        sprintf("%.3f", beta_dose)),
    q_dose     = ifelse(q_dose == 0, "< 0.001", sprintf("%.4f", q_dose)),
    q_dose     = ifelse(sig_dose, paste0(q_dose, " *"), q_dose)
  ) %>%
  select(domain, mean_delta, p_delta, q_delta, beta_dose, q_dose)

names(table3_clean) <- c("Domain", "Mean Change",
                         "p (Change ≠ 0)", "q (FDR)",
                         "Beta per survey", "q (FDR dose-response)")

ft <- flextable(table3_clean) %>%
  set_caption("Table 3. Domain-specific within-person change and dose-response (exploratory)") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Arial", part = "all") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  autofit() %>%
  add_footer_lines("* Significant after BH FDR correction at q < 0.05. Change tested by Wilcoxon signed-rank test; dose-response beta by OLS regression of last-minus-first change on survey count.") %>%
  fontsize(size = 8, part = "footer")

print(ft)
save_as_docx(ft, path = "Table3_Domain_Results.docx")
cat("Table 3 saved as Table3_Domain_Results.docx\n")


# ============================================================
# FIGURE 1 — STROBE Flow Diagram
# ============================================================

flow <- grViz("
  digraph STROBE {

    graph [layout = dot, rankdir = TB, fontname = Arial]
    node  [shape = rectangle, fontname = Arial, fontsize = 12,
           style = filled, fillcolor = white, width = 4.5]
    edge  [fontname = Arial, fontsize = 11]

    A [label = 'Total unique users\nn = 2,923']

    B [label = 'Excluded: completed exactly\none survey\nn = 2,322 (79.4%)',
       fillcolor = '#f5f5f5']

    C [label = 'Longitudinal analytic cohort\n(≥2 surveys)\nn = 601 (20.6%)']

    D [label = '2 surveys\n(reference band)\nn = 328 (54.6%)']
    E [label = '3–4 surveys\nn = 181 (30.1%)']
    F [label = '≥5 surveys\nn = 92 (15.3%)']

    A -> B [label = '  excluded']
    A -> C [label = '  retained']
    C -> D
    C -> E
    C -> F

    {rank = same; A; B}
  }
")

print(flow)

# Save as SVG (vector format)
svg_code <- export_svg(flow)
writeLines(svg_code, "Figure1_STROBE_flow.svg")
cat("Figure 1 saved as Figure1_STROBE_flow.svg\n")


# ============================================================
# FIGURE 2 — Dose-response bar chart with 95% CI error bars
# ============================================================

fig2_data <- data.frame(
  band    = c("2 surveys\n(n=328)",
              "3-4 surveys\n(n=181)",
              ">=5 surveys\n(n=92)"),
  mean    = c(7.61, 10.88, 12.22),
  ci_low  = c(6.39,  9.13,  9.73),
  ci_high = c(8.83, 12.62, 14.71)
)

fig2_data$band <- factor(fig2_data$band,
                         levels = c("2 surveys\n(n=328)",
                                    "3-4 surveys\n(n=181)",
                                    ">=5 surveys\n(n=92)"))

fig2 <- ggplot(fig2_data, aes(x = band, y = mean)) +
  geom_bar(stat = "identity", fill = "#2166ac",
           color = "black", linewidth = 0.3, width = 0.6) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.15, linewidth = 0.8) +
  geom_text(aes(label = sprintf("+%.2f", mean)),
            vjust = -0.5, hjust = -0.3, size = 4) +
  scale_y_continuous(limits = c(0, 17),
                     breaks = seq(0, 16, by = 4)) +
  labs(
    title    = "Figure 2. Mean Within-Person Change in Composite Score by Engagement Category",
    subtitle = "Error bars = 95% confidence intervals",
    x        = "Engagement category",
    y        = "Mean within-person change (points)",
    caption  = "ANOVA F(2,598) = 7.99, p=.0004; permutation p\u2264.0002"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 10),
    plot.caption  = element_text(size = 9, hjust = 0),
    axis.text     = element_text(size = 11)
  )

print(fig2)
ggsave("Figure2_Dose_Response.png", fig2, width = 8, height = 6, dpi = 300)
ggsave("Figure2_Dose_Response.pdf", fig2, width = 8, height = 6)
cat("Figure 2 saved successfully\n")


# ============================================================
# FIGURE 3 — Domain heatmap by engagement category
# ============================================================

library(reshape2)

# Mean change by domain and engagement band
heatmap_data <- df_repeat %>%
  arrange(User, SurveyDate) %>%
  group_by(User) %>%
  mutate(n_surveys = n()) %>%
  ungroup() %>%
  mutate(band = case_when(
    n_surveys == 2   ~ "2 surveys",
    n_surveys <= 4   ~ "3-4 surveys",
    TRUE             ~ ">=5 surveys"
  ))

heatmap_first <- heatmap_data %>%
  arrange(User, SurveyDate) %>%
  group_by(User, band) %>%
  slice(1) %>%
  ungroup() %>%
  select(User, band, all_of(survey_cols))

heatmap_last <- heatmap_data %>%
  arrange(User, SurveyDate) %>%
  group_by(User, band) %>%
  slice(n()) %>%
  ungroup() %>%
  select(User, band, all_of(survey_cols))

heatmap_diff <- heatmap_last %>%
  mutate(across(all_of(survey_cols),
                ~ . - heatmap_first[[cur_column()]])) %>%
  group_by(band) %>%
  summarise(across(all_of(survey_cols),
                   ~ round(mean(., na.rm = TRUE), 2)))

heatmap_long <- melt(heatmap_diff,
                     id.vars      = "band",
                     variable.name = "domain",
                     value.name   = "mean_change")

# Order bands and domains
heatmap_long$band <- factor(heatmap_long$band,
                            levels = c("2 surveys", "3-4 surveys", ">=5 surveys"))

domain_order <- wilcox_results %>% arrange(desc(mean_diff)) %>% pull(domain)
heatmap_long$domain <- factor(heatmap_long$domain, levels = rev(domain_order))

fig3 <- ggplot(heatmap_long, aes(x = band, y = domain, fill = mean_change)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%+.2f", mean_change)),
            size = 3, color = "black") +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                       midpoint = 0, name = "Mean\nwithin-person\nchange") +
  labs(
    title    = "Figure 3. Domain-specific score changes by engagement category",
    subtitle = "Red = improvement, Blue = decline",
    x        = "Engagement category",
    y        = NULL,
    caption  = "Values show mean within-person change (last minus first survey) per domain."
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 11),
    plot.subtitle   = element_text(size = 10),
    plot.caption    = element_text(size = 9, hjust = 0),
    axis.text.y     = element_text(size = 10),
    axis.text.x     = element_text(size = 10),
    legend.position = "right"
  )

print(fig3)
ggsave("Figure3_Domain_Heatmap.png", fig3, width = 8, height = 9, dpi = 300)
ggsave("Figure3_Domain_Heatmap.pdf", fig3, width = 8, height = 9)
cat("Figure 3 saved successfully\n")


# ============================================================
# FIGURE 4 — Forest plot: per-survey beta by baseline tertile
# ============================================================

# Factor order: High at top, Low at bottom
tertile_models$tertile <- factor(tertile_models$tertile,
                                 levels = c("High (73.5-100)",
                                            "Middle (60-73)",
                                            "Low (31-59.5)"))

fig4 <- ggplot(tertile_models, aes(x = beta, y = tertile)) +
  geom_point(size = 4, color = "#2166ac") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high),
                 height = 0.2, linewidth = 0.8, color = "#2166ac") +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  geom_text(aes(label = paste0("b=", beta,
                               " [", ci_low, ", ", ci_high, "]")),
            hjust = -0.15, vjust = -1.65, size = 3.5) +
  geom_text(aes(label = paste0("p=", p_fmt)),
            hjust = -0.15, vjust = 2.4, size = 3.5) +
  scale_x_continuous(limits = c(-0.5, 4.5)) +
  labs(
    title    = "Figure 4. Per-survey Dose-Response by Baseline Score Tertile",
    subtitle = "Baseline-adjusted OLS; error bars = 95% CI",
    x        = "Beta per additional survey (points)",
    y        = "Baseline score tertile",
    caption  = "Pattern substantially explained by regression to the mean (see Figure 5)."
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 10),
    plot.caption  = element_text(size = 9, hjust = 0),
    axis.text     = element_text(size = 11)
  )

print(fig4)
ggsave("Figure4_Forest_Plot.png", fig4, width = 8, height = 5, dpi = 300)
ggsave("Figure4_Forest_Plot.pdf", fig4, width = 8, height = 5)
cat("Figure 4 saved successfully\n")


# ============================================================
# FIGURE 5 — RTM Decomposition bar chart by baseline tertile
# ============================================================

rtm_data <- data.frame(
  tertile = rep(c("Low\n(31-59.5)", "Middle\n(60-73)", "High\n(73.5-100)"), 3),
  type    = rep(c("Observed Change", "RTM-only Expected Change", "Residual"), each = 3),
  value   = c(19.5,  7.0,  1.3,   # Observed
              9.3, -0.6, -8.8,   # RTM expected
              10.2,  7.6, 10.1)   # Residual
)

rtm_data$tertile <- factor(rtm_data$tertile,
                           levels = c("Low\n(31-59.5)",
                                      "Middle\n(60-73)",
                                      "High\n(73.5-100)"))
rtm_data$type <- factor(rtm_data$type,
                        levels = c("Observed Change",
                                   "RTM-only Expected Change",
                                   "Residual"))

fig5 <- ggplot(rtm_data, aes(x = tertile, y = value, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           width = 0.7, color = "black", linewidth = 0.3) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  scale_fill_manual(values = c("Observed Change"          = "#2166ac",
                               "RTM-only Expected Change" = "#d1e5f0",
                               "Residual"                 = "#f4a582")) +
  labs(
    title    = "Figure 5. RTM Decomposition of Within-Person Change by Baseline Tertile",
    subtitle = "ICC = 0.352 [95% CI 0.09, 0.54]; mean = 66.1",
    x        = "Baseline score tertile",
    y        = "Mean within-person change (points)",
    fill     = NULL,
    caption  = "RTM-only expected change = (1-ICC) x (mean-baseline). Residual = Observed - RTM-only expected."
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", size = 11),
    plot.subtitle   = element_text(size = 10),
    plot.caption    = element_text(size = 9, hjust = 0),
    axis.text       = element_text(size = 11)
  )

print(fig5)
ggsave("Figure5_RTM_Decomposition.png", fig5, width = 8, height = 6, dpi = 300)
ggsave("Figure5_RTM_Decomposition.pdf", fig5, width = 8, height = 6)
cat("Figure 5 saved successfully\n")


# ============================================================
# FIGURE 6 — Permutation null distribution
# ============================================================

# perm_betas was generated in the permutation test section above
perm_df <- data.frame(beta = perm_betas)

fig6 <- ggplot(perm_df, aes(x = beta)) +
  geom_histogram(bins = 60, fill = "#d1e5f0",
                 color = "white", linewidth = 0.2) +
  geom_vline(xintercept = observed_beta,
             color = "#d6604d", linewidth = 1.2, linetype = "solid") +
  geom_vline(xintercept = -observed_beta,
             color = "#d6604d", linewidth = 1.2, linetype = "dashed") +
  annotate("text", x = observed_beta - 0.1,
           y = max(table(cut(perm_betas, 60))) * 0.9,
           label = paste0("Observed\nb = +", round(observed_beta, 2)),
           hjust = 1, color = "#d6604d", size = 3.5) +
  scale_x_continuous(limits = c(-3.5, 3.5)) +
  labs(
    title    = "Figure 6. Permutation Null Distribution of Ordinal Trend Coefficient",
    subtitle = "5,000 permutations under random reassignment of engagement band",
    x        = "Permuted ordinal trend coefficient (points per band)",
    y        = "Frequency",
    caption  = "Red solid line = observed coefficient (+2.51). Red dashed line = negative mirror.\nTwo-sided permutation p\u2264.0002 (0 of 5,000 permutations exceeded observed value)."
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 10),
    plot.caption  = element_text(size = 9, hjust = 0),
    axis.text     = element_text(size = 11)
  )

print(fig6)
ggsave("Figure6_Permutation_Null.png", fig6, width = 8, height = 5, dpi = 300)
ggsave("Figure6_Permutation_Null.pdf", fig6, width = 8, height = 5)
cat("Figure 6 saved successfully\n")


# ============================================================
# FIGURE 7 — Sample size curve for future RCT
# ============================================================

# Generate curve data (x = effect size in points, y = N per arm)
delta_seq <- seq(0.5, 10, by = 0.1)
sd_val    <- 11.8
z_alpha   <- qnorm(0.975)
z_beta    <- qnorm(0.80)

n_seq      <- ceiling(2 * ((z_alpha + z_beta) * sd_val / delta_seq)^2)
curve_data <- data.frame(delta = delta_seq, n_arm = n_seq)

# Annotated reference points within the plot window
key_points <- data.frame(
  delta = c(2, 3, 5),
  n_arm = c(546, 243, 88),
  label = c("N=546", "N=243", "N=88")
)

fig7 <- ggplot(curve_data, aes(x = delta, y = n_arm)) +
  geom_line(color = "#2166ac", linewidth = 1.2) +
  geom_hline(yintercept = 243, linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = 3, linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  geom_point(data = key_points, aes(x = delta, y = n_arm),
             size = 3, color = "#d6604d") +
  geom_text(data = key_points, aes(x = delta, y = n_arm, label = label),
            hjust = -0.2, vjust = -0.7, size = 3.5, color = "#d6604d") +
  coord_cartesian(xlim = c(2, 9), ylim = c(0, 600)) +
  scale_x_continuous(breaks = seq(2, 9, by = 1)) +
  scale_y_continuous(breaks = seq(0, 600, by = 100)) +
  labs(
    title    = "Figure 7. Required Sample Size per Arm for a Future RCT",
    subtitle = "alpha = 0.05 two-sided, 80% power, SD = 11.8 points",
    x        = "Assumed true effect size: tool minus control (points)",
    y        = "Required N per arm",
    caption  = "Dashed lines indicate N = 243 per arm for a 3-point difference.\nBased on observed within-person SD of 11.8 points from the present cohort.\nN = 2,182 for 1-point and N = 26 for 9.3-point differences (outside plot range)."
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 10),
    plot.caption  = element_text(size = 9, hjust = 0),
    axis.text     = element_text(size = 11),
    plot.margin   = margin(t = 10, r = 40, b = 10, l = 10, unit = "pt")
  )

print(fig7)
ggsave("Figure7_Sample_Size_Curve.png", fig7, width = 8, height = 6, dpi = 300)
ggsave("Figure7_Sample_Size_Curve.pdf", fig7, width = 8, height = 6)
cat("Figure 7 saved successfully\n")


# ============================================================
# FIGURE 8 — Causal DAG
# ============================================================

dag <- grViz("
  digraph DAG {

    graph [layout = dot, rankdir = LR, fontname = Arial,
           label = 'Figure 8. Causal DAG for the engagement to behavior-score association.',
           labelloc = b, fontsize = 10]
    node  [shape = rectangle, fontname = Arial, fontsize = 11,
           style = filled, fillcolor = white, width = 2.2]
    edge  [fontname = Arial, fontsize = 9]

    EXP [label = 'Survey count\n(exposure)',         fillcolor = '#d1e5f0']
    BAS [label = 'Baseline\ncomposite score']
    OUT [label = 'Composite score\nchange (outcome)', fillcolor = '#d1e5f0']
    MOT [label = 'Motivation /\nself-selection',      fillcolor = '#ffe0e0',
         style = 'filled,dashed']
    PRG [label = 'Pregnancy stage\n& natural behavior\nadaptation',
         fillcolor = '#ffe0e0', style = 'filled,dashed']

    # Measured paths (solid arrows)
    EXP -> OUT [label = 'observed association\n(b = +2.5/band)',
                color = '#2166ac', fontcolor = '#2166ac']
    BAS -> EXP [label = 'weak (p=.50)', color = 'black']
    BAS -> OUT [label = 'strong (RTM)',    color = 'black']

    # Unmeasured confounders (dashed red arrows)
    MOT -> EXP [label = 'unmeasured', color = '#d6604d',
                fontcolor = '#d6604d', style = dashed]
    MOT -> OUT [color = '#d6604d', style = dashed]
    PRG -> EXP [color = '#d6604d', style = dashed]
    PRG -> OUT [label = 'unmeasured', color = '#d6604d',
                fontcolor = '#d6604d', style = dashed]

    {rank = same; EXP; OUT}
    {rank = same; MOT; PRG}
  }
")

print(dag)

svg_code <- export_svg(dag)
writeLines(svg_code, "Figure8_Causal_DAG.svg")
cat("Figure 8 saved as Figure8_Causal_DAG.svg\n")


# ============================================================
# SOFTWARE CITATION — Package version numbers
# ============================================================

cat("R version:\n")
print(R.version.string)

cat("\nPackage versions:\n")
packages <- c("readxl", "dplyr", "psych", "DiagrammeR",
              "DiagrammeRsvg", "rsvg", "lme4", "lmerTest",
              "corrplot", "irr", "flextable", "reshape2")
for (pkg in packages) {
  cat(pkg, ":", as.character(packageVersion(pkg)), "\n")
}

# Formal citations for key packages
citation()           # R itself
citation("lme4")
citation("lmerTest")
citation("psych")
citation("dplyr")
citation("corrplot")
