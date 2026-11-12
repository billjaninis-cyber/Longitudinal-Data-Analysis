
############################################################
# LONGITUDINAL HEALTH OUTCOMES ANALYSIS
#
# Mixed-Effects Models, Generalized Estimating Equations,
# and Regression Analysis in R
#
# Outcomes:
# 1. Pulmonary function (FEV)
# 2. Asthma symptoms
# 3. Changes in moderate physical activity
#
# Statistical Methods:
# - Descriptive statistics
# - Linear mixed-effects regression
# - Likelihood-ratio tests
# - Generalized estimating equations (GEE)
# - Logistic regression
# - Robust Wald tests
# - Change-score regression
# - Baseline-adjusted linear regression
############################################################


############################################################
# 0. PROJECT SETUP AND PACKAGES
############################################################

# Set working directory
setwd("C:/Users/billj/OneDrive/Desktop/BS 835")

# Verify working directory
getwd()

# View available files
list.files()

# Install packages if necessary
# install.packages(c(
#   "readxl", "dplyr", "lme4",
#   "lmerTest", "geepack"
# ))

# Load packages
library(readxl)
library(dplyr)
library(lme4)
library(lmerTest)
library(geepack)

# Create results folder
dir.create("results", showWarnings = FALSE)

# Function to extract regression coefficients
extract_coefficients <- function(model) {
  
  coef_table <- as.data.frame(
    summary(model)$coefficients
  )
  
  coef_table$Variable <- rownames(coef_table)
  
  rownames(coef_table) <- NULL
  
  coef_table |>
    select(Variable, everything())
}


############################################################
# PART 1: LONGITUDINAL PULMONARY HEALTH ANALYSIS
############################################################


############################################################
# 1. DATA IMPORT AND PREPARATION
############################################################

# Import pulmonary health dataset
fhssmog <- read_excel("fhssmog.xlsx")

# Inspect dataset
str(fhssmog)
head(fhssmog)
summary(fhssmog)

# Examine missing values
colSums(is.na(fhssmog))

# Convert categorical variables to factors
fhssmog <- fhssmog |>
  mutate(
    educ = relevel(
      factor(educ),
      ref = "1"
    ),
    studyexam = relevel(
      factor(studyexam),
      ref = "1"
    )
  )

# Sort observations by participant and examination
fhssmog <- fhssmog |>
  arrange(studyid, studyexam)

# Count unique participants
n_distinct(fhssmog$studyid)

# Examine repeated observations per participant
observations_per_id <- fhssmog |>
  count(studyid, name = "n_observations")

summary(observations_per_id$n_observations)


############################################################
# 2. DESCRIPTIVE STATISTICS
############################################################

# Objective:
# Summarize pulmonary function and asthma symptoms
# according to roadway exposure.


# 2A. Overall descriptive statistics

summary_total <- fhssmog |>
  group_by(nearroad) |>
  summarise(
    N_observations = n(),
    
    N_participants = n_distinct(studyid),
    
    N_fev = sum(!is.na(fev)),
    
    mean_fev = mean(fev, na.rm = TRUE),
    
    sd_fev = sd(fev, na.rm = TRUE),
    
    asthma_n = sum(asthma == 1, na.rm = TRUE),
    
    asthma_total = sum(!is.na(asthma)),
    
    asthma_pct = if_else(
      asthma_total > 0,
      100 * asthma_n / asthma_total,
      NA_real_
    ),
    
    .groups = "drop"
  )

print(summary_total)

# Export descriptive statistics
write.csv(
  summary_total,
  "results/descriptive_statistics_overall.csv",
  row.names = FALSE
)


# 2B. Descriptive statistics by study examination

summary_exam <- fhssmog |>
  group_by(studyexam, nearroad) |>
  summarise(
    N_observations = n(),
    
    N_participants = n_distinct(studyid),
    
    N_fev = sum(!is.na(fev)),
    
    mean_fev = mean(fev, na.rm = TRUE),
    
    sd_fev = sd(fev, na.rm = TRUE),
    
    asthma_n = sum(asthma == 1, na.rm = TRUE),
    
    asthma_total = sum(!is.na(asthma)),
    
    asthma_pct = if_else(
      asthma_total > 0,
      100 * asthma_n / asthma_total,
      NA_real_
    ),
    
    .groups = "drop"
  )

print(summary_exam)

# Export examination-specific statistics
write.csv(
  summary_exam,
  "results/descriptive_statistics_by_exam.csv",
  row.names = FALSE
)


############################################################
# 3. MIXED-EFFECTS LINEAR REGRESSION
############################################################

# Outcome: FEV
#
# Fixed effects:
# Age, sex, smoking history, education,
# roadway exposure, and study examination
#
# Random effects:
# Participant-specific random intercept
#
# Accounts for repeated measurements
# within the same participant.


# Prepare complete-case dataset for FEV analysis

fev_data <- fhssmog |>
  filter(
    complete.cases(
      age, sexM, FormerSmoke, educ,
      nearroad, studyexam, fev, studyid
    )
  ) |>
  arrange(studyid, studyexam)

# Examine analysis sample
cat(
  "Number of FEV observations:",
  nrow(fev_data), "\n"
)

cat(
  "Number of participants:",
  n_distinct(fev_data$studyid), "\n"
)


# Fit mixed-effects model

mix.out <- lmer(
  fev ~ age + sexM + FormerSmoke +
    educ + nearroad + studyexam +
    (1 | studyid),
  
  data = fev_data,
  REML = TRUE
)


# Regression coefficients and p-values
summary(mix.out)

# Type III tests of fixed effects
anova(mix.out, type = 3)

# Bayesian Information Criterion
BIC(mix.out)

# Random-effects variance estimates
VarCorr(mix.out)


############################################################
# 4. INTRACLASS CORRELATION COEFFICIENT
############################################################

# ICC measures the proportion of residual variation
# attributable to between-participant differences
# under the random-intercept model.

random_var <- as.numeric(
  VarCorr(mix.out)$studyid[1, 1]
)

residual_var <- sigma(mix.out)^2

icc <- random_var / (
  random_var + residual_var
)

cat(
  "Intraclass correlation coefficient:",
  icc, "\n"
)


# Export mixed-effects regression results
mixed_results <- extract_coefficients(mix.out)

write.csv(
  mixed_results,
  "results/mixed_effects_results.csv",
  row.names = FALSE
)


############################################################
# 5. MIXED-EFFECTS MODEL DIAGNOSTICS
############################################################

# Residuals versus fitted values

plot(
  fitted(mix.out),
  resid(mix.out),
  xlab = "Fitted FEV",
  ylab = "Residuals",
  main = "Mixed-Effects Model: Residuals vs Fitted"
)

abline(h = 0, col = "red", lty = 2)


# Normal Q-Q plot of residuals

qqnorm(
  resid(mix.out),
  main = "Normal Q-Q Plot: Residuals"
)

qqline(resid(mix.out), col = "red")


# Normal Q-Q plot of random intercepts

random_intercepts <- ranef(mix.out)$studyid[, 1]

qqnorm(
  random_intercepts,
  main = "Normal Q-Q Plot: Random Intercepts"
)

qqline(random_intercepts, col = "red")


############################################################
# 6. ROADWAY EXPOSURE BY STUDY EXAM INTERACTION
############################################################

# Objective:
# Determine whether the association between
# roadway exposure and FEV varies across
# study examinations.
#
# Models are estimated using maximum likelihood
# for valid fixed-effect model comparison.


# Main-effects model

mix.main <- lmer(
  fev ~ age + sexM + FormerSmoke +
    educ + nearroad + studyexam +
    (1 | studyid),
  
  data = fev_data,
  REML = FALSE
)


# Interaction model

mix.interaction <- lmer(
  fev ~ age + sexM + FormerSmoke +
    educ + nearroad * studyexam +
    (1 | studyid),
  
  data = fev_data,
  REML = FALSE
)


# Examine interaction coefficients
summary(mix.interaction)


# Likelihood-ratio test

interaction_test <- anova(
  mix.main,
  mix.interaction
)

print(interaction_test)


# Compare BIC values

interaction_bic <- BIC(
  mix.main,
  mix.interaction
)

print(interaction_bic)


# Export model comparison

write.csv(
  as.data.frame(interaction_test),
  "results/interaction_model_comparison.csv"
)


############################################################
# 7. GEE LOGISTIC REGRESSION: ASTHMA SYMPTOMS
############################################################

# Outcome: Asthma symptoms (0 = No, 1 = Yes)
#
# Family: Binomial
# Link: Logit
# Working correlation: Exchangeable
#
# Estimates population-averaged associations
# while accounting for repeated measurements.


# Prepare complete-case analysis dataset

asthma_data <- fhssmog |>
  filter(
    complete.cases(
      asthma, age, sexM, FormerSmoke,
      educ, nearroad, studyexam, studyid
    )
  ) |>
  arrange(studyid, studyexam)


# Verify outcome coding
table(asthma_data$asthma)

stopifnot(
  all(asthma_data$asthma %in% c(0, 1))
)


# Fit GEE logistic regression

gee.asthma <- geeglm(
  asthma ~ age + sexM + FormerSmoke +
    educ + nearroad + studyexam,
  
  id = studyid,
  data = asthma_data,
  
  family = binomial(link = "logit"),
  corstr = "exchangeable",
  std.err = "san.se"
)


# Model results
summary(gee.asthma)


# Odds ratios
exp(coef(gee.asthma))


# Extract coefficient estimates and standard errors

asthma.coef <- summary(gee.asthma)$coefficients

asthma.beta <- coef(gee.asthma)

asthma.se <- asthma.coef[
  names(asthma.beta), "Std.err"
]


# Create odds ratio results table

asthma.results <- data.frame(
  Variable = names(asthma.beta),
  
  OR = exp(asthma.beta),
  
  Lower_95_CI = exp(
    asthma.beta - 1.96 * asthma.se
  ),
  
  Upper_95_CI = exp(
    asthma.beta + 1.96 * asthma.se
  ),
  
  P_value = asthma.coef[
    names(asthma.beta), "Pr(>|W|)"
  ]
)

print(asthma.results)


# Export odds ratios

write.csv(
  asthma.results,
  "results/gee_asthma_odds_ratios.csv",
  row.names = FALSE
)


############################################################
# 8. GEE LINEAR REGRESSION: PULMONARY FUNCTION
############################################################

# Outcome: FEV (continuous)
#
# Family: Gaussian
# Link: Identity
# Working correlation: Exchangeable
#
# Uses the same complete-case dataset
# as the mixed-effects FEV model.


gee.fev <- geeglm(
  fev ~ age + sexM + FormerSmoke +
    educ + nearroad + studyexam,
  
  id = studyid,
  data = fev_data,
  
  family = gaussian(link = "identity"),
  corstr = "exchangeable",
  std.err = "san.se"
)


# Regression results
summary(gee.fev)


# Extract model coefficients

fev.coef <- summary(gee.fev)$coefficients

fev.beta <- coef(gee.fev)


# Construct results table

fev.results <- data.frame(
  Variable = names(fev.beta),
  
  Slope = as.numeric(fev.beta),
  
  SE = fev.coef[
    names(fev.beta), "Std.err"
  ],
  
  P_value = fev.coef[
    names(fev.beta), "Pr(>|W|)"
  ]
)


# Calculate 95% confidence intervals

fev.results <- fev.results |>
  mutate(
    Lower_95_CI = Slope - 1.96 * SE,
    Upper_95_CI = Slope + 1.96 * SE
  )

print(fev.results)


# Export GEE results

write.csv(
  fev.results,
  "results/gee_fev_results.csv",
  row.names = FALSE
)


############################################################
# 9. JOINT WALD TEST FOR EDUCATION
############################################################

# Objective:
# Test the overall association between
# education and FEV in the GEE model.
#
# Null hypothesis:
# All education coefficients equal zero.
#
# Uses the robust covariance matrix.


# Identify education coefficients

educ.rows <- grep(
  "^educ",
  names(coef(gee.fev))
)


# Extract education coefficients

educ.beta <- coef(gee.fev)[educ.rows]


# Extract covariance matrix

educ.V <- vcov(gee.fev)[
  educ.rows,
  educ.rows,
  drop = FALSE
]


# Calculate robust Wald statistic

educ.Wald <- as.numeric(
  t(educ.beta) %*%
    solve(educ.V) %*%
    educ.beta
)


# Degrees of freedom

educ.df <- length(educ.beta)


# Chi-square p-value

educ.p <- pchisq(
  educ.Wald,
  df = educ.df,
  lower.tail = FALSE
)


# Display results

cat(
  "Education overall Wald statistic:",
  educ.Wald, "\n"
)

cat(
  "Education degrees of freedom:",
  educ.df, "\n"
)

cat(
  "Education joint Wald p-value:",
  educ.p, "\n"
)


# Create results table

education_wald_results <- data.frame(
  Variable = "Education",
  Wald_Statistic = educ.Wald,
  DF = educ.df,
  P_value = educ.p
)


# Export results

write.csv(
  education_wald_results,
  "results/education_joint_wald_test.csv",
  row.names = FALSE
)


############################################################
# 10. COMPARE MIXED-EFFECTS AND GEE MODELS
############################################################

# Objective:
# Compare regression coefficients obtained
# from mixed-effects and GEE models.
#
# Both models use identical observations.
#
# The linear mixed-effects model includes
# participant-specific random intercepts.
#
# GEE estimates marginal associations
# using an exchangeable working correlation.


# Mixed-effects fixed coefficients

mixed.coef <- data.frame(
  Variable = names(fixef(mix.out)),
  
  Mixed_Effects_Slope = as.numeric(
    fixef(mix.out)
  )
)


# GEE coefficients

gee.coef <- data.frame(
  Variable = names(coef(gee.fev)),
  
  GEE_Slope = as.numeric(
    coef(gee.fev)
  )
)


# Match coefficients by variable name

comparison <- mixed.coef |>
  full_join(
    gee.coef,
    by = "Variable"
  )


# Calculate coefficient differences

comparison <- comparison |>
  mutate(
    Difference = Mixed_Effects_Slope - GEE_Slope
  )


# Display comparison
print(comparison)


# Full model summaries
summary(mix.out)
summary(gee.fev)


# Export comparison table

write.csv(
  comparison,
  "results/mixed_effects_vs_gee.csv",
  row.names = FALSE
)


############################################################
# PART 2: PHYSICAL ACTIVITY ANALYSIS
############################################################


############################################################
# 11. IMPORT PHYSICAL ACTIVITY DATASET
############################################################

# Import second dataset from working directory

ExerciseStudy <- read_excel("ExerciseStudy.xlsx")


# Inspect dataset
str(ExerciseStudy)
head(ExerciseStudy)
summary(ExerciseStudy)


# Check missing values
colSums(is.na(ExerciseStudy))


############################################################
# 12. CREATE CHANGE SCORE
############################################################

# Outcome:
# Change in minutes of moderate physical
# activity between baseline and 6 months.
#
# Positive values indicate increased activity.
# Negative values indicate reduced activity.


ExerciseStudy <- ExerciseStudy |>
  mutate(
    minmodchng = minmod6 - minmod0
  )


# Examine change-score distribution
summary(ExerciseStudy$minmodchng)

sd(
  ExerciseStudy$minmodchng,
  na.rm = TRUE
)


############################################################
# 13. LINEAR REGRESSION ON CHANGE SCORE
############################################################

# Outcome:
# Change in moderate physical activity minutes
#
# Predictors:
# Age, sex, education, intervention group


reg.out <- lm(
  minmodchng ~ age + sexF + collgrad + PAprog,
  
  data = ExerciseStudy
)


# Regression results
summary(reg.out)


# 95% confidence intervals
confint(reg.out)


# Extract coefficient estimates

change_results <- extract_coefficients(reg.out)


# Export results

write.csv(
  change_results,
  "results/physical_activity_change_model.csv",
  row.names = FALSE
)


############################################################
# 14. BASELINE-ADJUSTED LINEAR REGRESSION
############################################################

# Outcome:
# Moderate physical activity minutes at 6 months
#
# Adjusts for:
# Age, sex, education, baseline activity,
# intervention group


reg.out2 <- lm(
  minmod6 ~ age + sexF + collgrad +
    minmod0 + PAprog,
  
  data = ExerciseStudy
)


# Regression results
summary(reg.out2)


# 95% confidence intervals
confint(reg.out2)


# Extract model coefficients

baseline_results <- extract_coefficients(reg.out2)


# Export results

write.csv(
  baseline_results,
  "results/physical_activity_baseline_adjusted.csv",
  row.names = FALSE
)


############################################################
# 15. LINEAR REGRESSION DIAGNOSTICS
############################################################

# Diagnostic plots for change-score model

par(mfrow = c(2, 2))

plot(reg.out)

par(mfrow = c(1, 1))


# Diagnostic plots for baseline-adjusted model

par(mfrow = c(2, 2))

plot(reg.out2)

par(mfrow = c(1, 1))


############################################################
# 16. FINAL MODEL SUMMARIES
############################################################

cat("\n====================================\n")
cat("FINAL MODEL SUMMARY\n")
cat("====================================\n")


cat("\nMixed-Effects FEV Model\n")
print(summary(mix.out))


cat("\nRoadway Exposure Interaction Test\n")
print(interaction_test)


cat("\nGEE Logistic Model: Asthma\n")
print(summary(gee.asthma))


cat("\nGEE Linear Model: FEV\n")
print(summary(gee.fev))


cat("\nMixed-Effects vs GEE Coefficients\n")
print(comparison)


cat("\nPhysical Activity Change Model\n")
print(summary(reg.out))


cat("\nBaseline-Adjusted Physical Activity Model\n")
print(summary(reg.out2))


############################################################
# 17. REPRODUCIBILITY INFORMATION
############################################################

# Save R version and package information

writeLines(
  capture.output(sessionInfo()),
  "results/session_info.txt"
)


############################################################
# END OF ANALYSIS
############################################################
