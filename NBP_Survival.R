###-----------------------------------------------------------------------------
### Survival Analysis Sandbox
### 
### Jan
###
### 02/2025
###-----------------------------------------------------------------------------

##---- PACKAGES ----------------------------------------------------------------
#library(ctv) # incl. packages relevant to survival analysis, cf here: https://github.com/cran-task-views/Survival/
library(tidyverse)
library(survival)
library(ggplot2)
library(dplyr)
library(ggfortify)

library(ranger)

##---- DATA --------------------------------------------------------------------
df_analysis_conflicts <- read.csv("NBP_2025_conflict_paper_analysis_c_250214.csv") %>%
  select(-X)

df_conflicts_duration <- df_analysis_conflicts %>%
  group_by(dyad_id) %>%
  summarise(
    start_year = min(Year),
    end_year = max(Year),
    conflict_length = max(Year) - min(Year) + 1,
    .groups = "drop"
  )

df_start_values <- df_analysis_conflicts %>%
  group_by(dyad_id) %>%
  filter(Year == min(Year))

df_analysis_survival <- df_conflicts_duration %>%
  left_join(df_start_values, by = "dyad_id")


##---- ANALYSIS ----------------------------------------------------------------

## Survival analysis sandbox

csurv <- with(df_analysis_survival, Surv(conflict_length))

head(csurv, 50)

csurv_fit <- survfit(Surv(conflict_length) ~ 1, data = df_analysis_survival)

autoplot(csurv_fit)

csurv_fit_edex <- survfit(Surv(conflict_length) ~ nbp_educational_exclusion, data = df_analysis_survival)

autoplot(csurv_fit_edex)

csurv_fit_pubex <- survfit(Surv(conflict_length) ~ nbp_public_exclusion, data = df_analysis_survival)

autoplot(csurv_fit_pubex)

csurv_cox <- coxph(Surv(conflict_length) ~ nbp_educational_exclusion + 
                     groupsize +
                     warhist +
                     Polity2 +
                     tek_egip,
                   data = df_analysis_survival)
summary(csurv_cox)

csurv_fit_cox <- survfit(csurv_cox)

autoplot(csurv_fit_cox)


## Random Forest model

df_analysis_rf <- df_analysis_survival %>%
  select(c(conflict_length, nbp_educational_exclusion, groupsize, warhist, Polity2, tek_egip)) %>%
  na.omit()

csurv_fit_r <- ranger(Surv(conflict_length) ~ nbp_educational_exclusion + 
                        groupsize +
                        warhist +
                        Polity2 +
                        tek_egip,
                      data = df_analysis_rf,
                      mtry = 3,
                      importance = "permutation",
                      splitrule = "extratrees",
                      verbose = TRUE)
death_times <- csurv_fit_r$unique.death.times
surv_prob <- data.frame(csurv_fit_r$survival)
avg_prob <- sapply(surv_prob,mean)

plot(csurv_fit_r$unique.death.times, csurv_fit_r$survival[1,],
     type = "l",
     ylim = c(0,1),
     col = "red")


cols <- colors()
for (n in sample(c(2:dim(df_analysis_rf)[1]), 20)){
  lines(csurv_fit_r$unique.death.times, csurv_fit_r$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)


vars <- data.frame(sort(round(csurv_fit_r$variable.importance, 4), decreasing = TRUE))
names(vars) <- "importance"
head(vars)

cat("Prediction Error = 1 - Harrell's c-index = ", csurv_fit_r$prediction.error)
