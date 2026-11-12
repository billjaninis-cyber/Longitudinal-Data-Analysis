Longitudinal Health Outcomes Analysis in R

Project Overview

This project examines longitudinal health outcomes using linear mixed-effects models, generalized estimating equations (GEE), and multivariable regression in R.

The analysis focuses on associations between roadway exposure, pulmonary function, and asthma symptoms, alongside a separate analysis of changes in moderate physical activity following an intervention.

The project demonstrates statistical methods for analyzing repeated measurements, accounting for within-participant correlation, evaluating interactions, and comparing modeling approaches.

Research Objectives

Describe pulmonary function and asthma symptoms across roadway exposure groups and study examinations.

Evaluate associations between roadway exposure and forced expiratory volume (FEV) using linear mixed-effects regression.

Assess whether associations between roadway exposure and pulmonary function differ across study examinations.

Estimate population-averaged associations with asthma symptoms and pulmonary function using GEE.

Compare results from mixed-effects and GEE models.

Evaluate changes in moderate physical activity using change-score and baseline-adjusted linear regression.

Statistical Methods

1. Descriptive Statistics

Calculated means, standard deviations, frequencies, and percentages.

Summarized pulmonary function and asthma outcomes by roadway exposure and study examination.

Examined missing values and participant-level observation counts.

2. Linear Mixed-Effects Regression

Modeled FEV as a continuous outcome.

Included participant-specific random intercepts to account for repeated observations.

Adjusted for age, sex, smoking history, education, roadway exposure, and study examination.

Examined variance components, intraclass correlation, and model diagnostics.

3. Interaction Analysis

Evaluated the interaction between roadway exposure and study examination.

Compared nested models using likelihood-ratio tests.

Assessed relative model fit using the Bayesian Information Criterion (BIC).

4. Generalized Estimating Equations

Applied logistic GEE to model binary asthma symptoms.

Applied linear GEE to model continuous pulmonary function.

Used exchangeable working correlation structures.

Calculated odds ratios, 95% confidence intervals, and robust Wald tests.

5. Mixed-Effects vs. GEE Comparison

Compared coefficient estimates between random-intercept mixed-effects and population-averaged GEE models.

Evaluated similarities and differences in estimated associations.

6. Physical Activity Analysis

Calculated changes in moderate physical activity from baseline to six months.

Estimated multivariable linear regression models predicting change in activity.

Fitted baseline-adjusted regression models predicting six-month physical activity.

Examined regression diagnostics.

R Packages

readxl: Excel data import

dplyr: Data manipulation and descriptive statistics

lme4: Linear mixed-effects regression

lmerTest: Statistical inference for mixed-effects models

geepack: Generalized estimating equations

Repository Structure

longitudinal-health-outcomes/
├── README.md
├── longitudinal_analysis.R
└── results/

Reproducibility

The analysis requires the original study datasets, fhssmog.xlsx and ExerciseStudy.xlsx.

The R script was developed using R and the packages listed above. The current script uses a local Windows working directory, which should be updated for use on another computer.

Dataset availability and sharing permissions should be verified before public distribution.

Limitations

Complete-case analysis may introduce bias depending on the missing-data mechanism.

GEE inference relies on appropriate assumptions about independent participant clusters and adequate sample size.

Mixed-effects regression and GEE employ different approaches to modeling correlated observations.

Findings should be interpreted as statistical associations rather than causal effects unless additional causal assumptions are justified.

Project Background

This project was developed from graduate-level coursework in biostatistics and longitudinal data analysis at Boston University School of Public Health. The original analysis was extended with additional data-quality checks, model diagnostics, comparisons, and reproducible reporting.

Author

Bill Janinis
Master of Public Health (MPH), Epidemiology & Biostatistics
Boston University School of Public Health

Areas of interest: Biostatistics, epidemiology, health economics and outcomes research (HEOR), and real-world evidence (RWE).
