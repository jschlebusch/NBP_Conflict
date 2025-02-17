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
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

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

csurv <- with(df_analysis_survival, Surv(conflict_length))

head(csurv, 50)

csurv_fit <- survfit(Surv(conflict_length) ~ 1, data = df_analysis_survival)

autoplot(csurv_fit)

csurv_fit_edex <- survfit(Surv(conflict_length) ~ nbp_educational_exclusion, data = df_analysis_survival)

autoplot(csurv_fit_edex)

csurv_fit_pubex <- survfit(Surv(conflict_length) ~ nbp_public_exclusion, data = df_analysis_survival)

autoplot(csurv_fit_pubex)


