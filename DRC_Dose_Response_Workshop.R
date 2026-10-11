# =============================================================================
# DRC WORKSHOP PRACTICAL: DOSE–RESPONSE ANALYSIS AND DRUG POTENCY USING R
# Audience: Beginner to intermediate R users in pharmacology/toxicology
# Duration: Approximately 90–120 minutes
#
# IMPORTANT:
# 1. This script creates SIMULATED teaching data; it is not real experimental data.
# 2. Run the script from top to bottom, one section at a time.
# 3. Read the comments before running each block. Try to predict the output.
# 4. Never report simulated estimates as experimental findings.
# =============================================================================


# ---------------------------------------------------------------------------
# SECTION 0. SET UP R AND RSTUDIO
# ---------------------------------------------------------------------------
# R is the language that performs the calculations.
# RStudio is an interface that makes it easier to write and run R code.
#
# In RStudio, create a new R Script, paste/open this file, and save it.
# Run a line or selected block with Ctrl+Enter (Windows/Linux) or Cmd+Enter
# (macOS). The Console displays results; the Environment pane lists objects.
#
# Choose a working folder for this practical. Replace the path below if needed.
# getwd() shows the current folder; setwd() changes it.
# Avoid using a folder path that does not exist.
# setwd("C:/DRC_Workshop")
getwd()


# ---------------------------------------------------------------------------
# SECTION 1. INSTALL AND LOAD REQUIRED PACKAGES
# ---------------------------------------------------------------------------
# install.packages() downloads a package and normally needs to be run only once
# on a computer. library() loads a package into the current R session.
# drc fits dose-response models; ggplot2 makes a clear graph.
#
# Uncomment the next line if these packages are not installed:
# install.packages(c("drc", "ggplot2"))
#
# If installation fails, check internet access, R version, and permissions.
library(drc)
library(ggplot2)


# ---------------------------------------------------------------------------
# SECTION 2. CREATE A REPRODUCIBLE SIMULATED DATASET
# ---------------------------------------------------------------------------
# Research question:
# Does Drug A reach the midpoint of its response range at a lower concentration
# than Drug B?
#
# A lower EC50 generally indicates greater potency when the endpoint, response
# scale, assay conditions, and interpretation are comparable.
#
# set.seed() makes the random noise repeatable, so participants should obtain
# the same simulated values when they run the script with the same R setup.
set.seed(123)

# Seven positive concentrations spaced approximately evenly on a log scale.
# The log scale is useful when responses change across several-fold differences.
concentrations <- c(0.1, 0.3, 1, 3, 10, 30, 100)

# expand.grid() creates all combinations of concentration, replicate, and drug.
# Three replicates per concentration are used for demonstration only.
dat <- expand.grid(
  concentration = concentrations,
  replicate = 1:3,
  drug = c("Drug A", "Drug B")
)

# These are the underlying EC50 values used to construct the teaching example.
# They are not estimates from a real experiment.
dat$target_EC50 <- ifelse(dat$drug == "Drug A", 1, 10)

# Generate an increasing sigmoidal response and add random measurement noise.
# The exponent controls steepness. rnorm() adds normally distributed noise.
dat$response <- 100 /
  (1 + (dat$target_EC50 / dat$concentration)^1.2) +
  rnorm(nrow(dat), mean = 0, sd = 3)

# Keep the simulated response within the illustrative assay range 0–100%.
# This clipping is solely for this teaching example. Do not automatically clip
# real data; investigate values outside an expected range and follow assay rules.
dat$response <- pmax(0, pmin(100, dat$response))

# Inspect the data. Each row is one observation.
head(dat)
str(dat)
summary(dat)

# Count observations per drug and concentration to check the design.
with(dat, table(drug, concentration))


# ---------------------------------------------------------------------------
# SECTION 3. CHECK DATA BEFORE MODELLING
# ---------------------------------------------------------------------------
# A model cannot use zero concentration on a logarithmic dose scale because
# log(0) is undefined. Vehicle/untreated controls can be displayed separately
# or handled using a model appropriate to the design, but should not simply be
# inserted into this positive-concentration LL.4 fit.
#
# Check missing values and the range of the response.
sum(is.na(dat$concentration))
sum(is.na(dat$response))
range(dat$concentration)
range(dat$response)

# Ensure concentrations used in this model are positive.
stopifnot(all(dat$concentration > 0))

# Confirm the drug labels and sample counts.
levels(factor(dat$drug))
table(dat$drug)


# ---------------------------------------------------------------------------
# SECTION 4. VISUALISE THE OBSERVED DATA
# ---------------------------------------------------------------------------
# Plot individual observations rather than only means so participants can see
# the variation between replicates. scale_x_log10() changes the display scale
# of the x-axis; it does not change the recorded concentration values.
p_observed <- ggplot(
  dat,
  aes(x = concentration, y = response, colour = drug)
) +
  geom_point(size = 2.5, alpha = 0.85) +
  scale_x_log10() +
  labs(
    title = "Observed concentration–response data (simulated)",
    x = "Drug concentration (log10 scale)",
    y = "Biological response (%)",
    colour = "Drug"
  ) +
  theme_classic()

print(p_observed)

# Save a high-resolution figure for reports or presentations.
ggsave(
  filename = "DRC_observed_data.png",
  plot = p_observed,
  width = 7,
  height = 5,
  dpi = 300
)


# ---------------------------------------------------------------------------
# SECTION 5. FIT A FOUR-PARAMETER LOG-LOGISTIC MODEL
# ---------------------------------------------------------------------------
# drm() fits a dose-response model. response ~ concentration states which
# variable is measured and which is the predictor. curveid = drug fits a
# separate curve for each drug. LL.4() specifies the four-parameter
# log-logistic model.
#
# The four parameters are commonly labelled:
# b = slope around the midpoint (sign depends on curve direction/parameterisation)
# c = lower asymptote
# d = upper asymptote
# e = midpoint concentration (relative EC50 in this model)
#
# The model estimates the parameters from the observed values; it does not
# simply copy target_EC50 from the simulated dataset.
model <- drm(
  response ~ concentration,
  curveid = drug,
  data = dat,
  fct = LL.4()
)

# Inspect coefficient estimates, standard errors, and model information.
summary(model)

# Teaching question: Is the estimated midpoint lower for Drug A than Drug B?
# Do not infer significance solely by looking at the point estimates.


# ---------------------------------------------------------------------------
# SECTION 6. PLOT THE FITTED CURVES
# ---------------------------------------------------------------------------
# Observed points show the measurements; fitted curves show the model's
# estimated concentration-response relationship. Look for systematic patterns
# where the model repeatedly over- or under-predicts the observations.
png("DRC_fitted_curves.png", width = 1800, height = 1300, res = 250)
plot(
  model,
  type = "all",
  log = "x",
  xlab = "Concentration",
  ylab = "Biological response (%)",
  main = "Four-parameter log-logistic fits (simulated data)"
)
dev.off()

# Open the saved plot in RStudio's Files pane, or use:
# browseURL("DRC_fitted_curves.png")


# ---------------------------------------------------------------------------
# SECTION 7. ESTIMATE EC10, EC50, AND EC90 WITH 95% CONFIDENCE INTERVALS
# ---------------------------------------------------------------------------
# ED() estimates effective concentrations at specified response levels.
# In this use, 10, 50, and 90 refer to percentages of each fitted curve's
# response range (relative effective concentrations), not necessarily absolute
# responses of 10, 50, and 90 assay units.
#
# Confidence intervals communicate uncertainty around estimates. Wide intervals
# can indicate imprecise estimation, for example if plateaus were not sampled.
ec_values <- ED(
  model,
  respLev = c(10, 50, 90),
  interval = "delta",
  level = 0.95
)

print(ec_values)

# Important interpretation:
# If the fitted lower/upper asymptotes differ between drugs, each relative EC50
# is the midpoint of its own fitted response range. It may not be the
# concentration producing an absolute response of 50 percentage points.


# ---------------------------------------------------------------------------
# SECTION 8. COMPARE RELATIVE POTENCY
# ---------------------------------------------------------------------------
# EDcomp() compares effective concentrations between curves at specified
# response levels. Here we request a comparison at the 50% relative response
# level. Inspect the printed comparison label carefully to establish the ratio's
# numerator and denominator before interpreting its direction.
potency_comparison <- EDcomp(
  model,
  percVec = c(50, 50),
  interval = "delta",
  level = 0.95
)

print(potency_comparison)

# If the reported ratio is EC50(Drug B) / EC50(Drug A), a ratio near 10 means
# Drug B requires about ten times the concentration of Drug A to reach the
# corresponding relative response. Confirm the actual ratio order in the
# output; do not assume it without checking the label.
#
# A confidence interval that includes 1 indicates that the data may be
# compatible with equal relative potency at the conventional 95% interval
# level. Formal interpretation depends on the model and analysis plan.


# ---------------------------------------------------------------------------
# SECTION 9. SIMPLE RESIDUAL CHECK
# ---------------------------------------------------------------------------
# Residuals are observed minus model-predicted values. They help reveal patterns
# the model has not captured. A residual plot is a diagnostic, not a proof that
# all model assumptions are satisfied.
res <- residuals(model)
fit <- fitted(model)

png("DRC_residual_plot.png", width = 1600, height = 1200, res = 220)
plot(
  fit, res,
  xlab = "Fitted response",
  ylab = "Residual (observed - fitted)",
  main = "Residual check"
)
abline(h = 0, lty = 2)
dev.off()

# Look for random scatter around zero rather than a clear curve, funnel shape,
# or group-specific pattern. Also examine the raw data and experimental design.


# ---------------------------------------------------------------------------
# SECTION 10. EXPORT DATA AND RESULTS
# ---------------------------------------------------------------------------
# write.csv() creates files that can be opened in spreadsheet software.
# These files are saved in the working directory shown by getwd().
write.csv(dat, "DRC_simulated_dataset.csv", row.names = FALSE)
write.csv(
  data.frame(
    drug = dat$drug,
    concentration = dat$concentration,
    response = dat$response,
    replicate = dat$replicate
  ),
  "DRC_analysis_data.csv",
  row.names = FALSE
)

# Save a plain-text record of the fitted model summary.
capture.output(summary(model), file = "DRC_model_summary.txt")
capture.output(ec_values, file = "DRC_effective_concentrations.txt")
capture.output(potency_comparison, file = "DRC_potency_comparison.txt")

# List the generated files so participants can verify where they were saved.
list.files(pattern = "^DRC_")


# ---------------------------------------------------------------------------
# SECTION 11. OPTIONAL EXERCISES — CHANGE ONE THING AT A TIME
# ---------------------------------------------------------------------------
# Exercise A: Change Drug B's target EC50 from 10 to 5 in Section 2, regenerate
# the data and repeat the fit. Predict how the relative potency should change.
#
# Exercise B: Give Drug B a lower maximum response. Discuss why potency (EC50)
# and efficacy (maximum response) can lead to different conclusions.
#
# Exercise C: Increase the noise standard deviation from 3 to 10. Observe how
# the points, fitted curves, and confidence intervals change.
#
# Exercise D: Remove the highest concentration and refit. Discuss whether the
# upper plateau and EC50 are still well supported by the data.
#
# Exercise E: Explain why three technical wells from one biological sample do
# not necessarily count as three independent biological replicates.
#
# To repeat the simulation after changing a parameter, rerun Sections 2–10
# from the top so the dataset, model, estimates, and plots remain consistent.


# ---------------------------------------------------------------------------
# SECTION 12. SCIENTIFIC REPORT TEMPLATE
# ---------------------------------------------------------------------------
# Replace all bracketed fields with actual output from your analysis.
#
# "Simulated concentration-response data for Drug A and Drug B were analysed
# using a four-parameter log-logistic model in the R package drc. The estimated
# relative EC50 was [value and units; 95% CI] for Drug A and [value and units;
# 95% CI] for Drug B. Relative potency was evaluated using EDcomp(), with the
# ratio interpreted according to the comparison label. Model fit was assessed
# by visual inspection of observed data, fitted curves, and residuals.
# Because these data are simulated for teaching, the results do not constitute
# evidence about real drugs."
#
# In a real study, report assay conditions, units, biological replication,
# model choice, estimates, confidence intervals, and any deviations from the
# planned analysis. Potency does not by itself establish efficacy, safety,
# mechanism, or clinical superiority.
# =============================================================================
