###-----------------------------------------------------------------------------
### Conflict Paper Analysis
### 
### Emre & Jan
###
### 02/2025
###-----------------------------------------------------------------------------


# Load libraries 

library(tidyverse)
library(haven)
library(readxl)
library(ggplot2)
library(ggthemes)
library(ggmosaic)
library(reshape2)
library(psych)
library(vcd)
library(naniar)
library(sandwich)
library(lmtest)
library(plm)
library(fixest)
library(stargazer)
library(splines)
library(car)
library(margins)
library(ggeffects)
library(MatchIt)
library(pROC)

# Load data

df_analysis_group <- read.csv("NBP_2025_conflict_paper_analysis_250214.csv")%>% # this is the group level data
  select(-X) # we got the X from the csv file. thats just the case no.
df_analysis_conflicts <- read.csv("NBP_2025_conflict_paper_analysis_c_250214.csv") %>% # this is the conflict level data
  select(-X)

df_analysis_group <- df_analysis_group %>%
  distinct()

summary(as.factor(df_analysis_group$nbp_anydown_1))
summary(as.factor(df_analysis_group$nbp_anydown_2))

# downgrade NAs as 0

df_analysis_group <- df_analysis_group %>%
  mutate(
    nbp_anydown_1 = replace_na(nbp_anydown_1, 0),
    nbp_anydown_2 = replace_na(nbp_anydown_2, 0),
    epr_downgraded1 = replace_na(epr_downgraded1, 0),
    epr_downgraded2 = replace_na(epr_downgraded2, 0)
  )

df_analysis_conflicts <- df_analysis_conflicts %>%
  mutate(
    nbp_anydown_1 = replace_na(nbp_anydown_1, 0),
    nbp_anydown_2 = replace_na(nbp_anydown_2, 0),
    epr_downgraded1 = replace_na(epr_downgraded1, 0),
    epr_downgraded2 = replace_na(epr_downgraded2, 0)
  )

summary(df_analysis_group)
summary(df_analysis_conflicts)

# ANYDOWN and LAG: Issue Check

check_lag <- df_analysis_group %>%
  arrange(iso3c, Group, Year) %>%
  group_by(iso3c, Group) %>%
  mutate(expected_lag = lag(nbp_anydown_1)) %>%
  summarise(correct = sum(lag_nbp_anydown_1 == expected_lag, na.rm = TRUE),
            incorrect = sum(lag_nbp_anydown_1 != expected_lag, na.rm = TRUE))

print(check_lag)

df_check_mismatches <- check_lag %>%
  filter(incorrect > 0)

mismatches <- df_analysis_group %>%
  arrange(iso3c, Group, Year) %>%
  group_by(iso3c, Group) %>%
  mutate(expected_lag = lag(nbp_anydown_1)) %>%
  filter(lag_nbp_anydown_1 != expected_lag & !is.na(lag_nbp_anydown_1) & !is.na(expected_lag))

View(mismatches)

df_mismatches_adc <- mismatches %>%
  filter(expected_lag != lag_nbp_anydown_1)

# Additional lagged variables (revised `NBP_Conflict`).

## Lagged Variables

# Lags now functional; regional markers (`un.region.name` and `un.regionsub.name`) 
# are taken from `NBP_Conflict`, with manual additions where `{countrycode}` is NA. 
# These are now included in the .csv file.

# `un.intermediate.region` contains too many missing values and is excluded from `NBP_Conflict`.
# As a result, the lag and region marker code has been removed here.

# Checking co-occurrence of `nbp_anydown_1` and `epr_downgrade1` to assess 
# potential interaction terms.

df_downgrades <- df_analysis_group %>%
  filter(nbp_anydown_1 == 1 & epr_downgraded1 == 1) 

view(df_downgrades)

## PAPER MODELS

## is there a reason we have multiple models with the same name? I suggest to use consecutive numbers (m1, m2, m3 etc.)

#ONSET: Educational Exclusion and Downgrades

m1_logit <- glm(onset_do_flag ~ lag_nbp_anydown_1 + 
                  nbp_educational_exclusion +
                  groupsize +
                  SpatialConc +
                  warhist +
                  tek_egip +
                  lag_Polity2 +
                  excl_groups_count +
                  log(lag_pop) +
                  log(lag_rgdpe) +
                  ns(peaceyears, df = 3),
                data = df_analysis_group,
                family = binomial())

summary(m1_logit)

m1_vcov_cluster <- vcovCL(m1_logit, cluster = df_analysis_group$iso3c)

m1_summary_clustered <- coeftest(m1_logit, vcov = m1_vcov_cluster)

print(m1_summary_clustered)


vif(m1_logit) # might have multicollinearity issues with country population size and GDP, but key IVs should be fine
influencePlot(m1_logit, id.method="identify", main="Influence Plot", sub="Circle size ~ Cook's Distance") 

cooksD_m1 <- cooks.distance(m1_logit)
influential_m1 <- which(cooksD > (4/nrow(df_analysis_group))) # we might consider other thresholds
print(influential_m1) 

# influential cases might also be an issue. We can excluse, specify robust models, or specify penalized logistic regressions - lets discuss


# WITH POLITICAL

m1_logit <- glm(onset_ko_flag ~ lag_nbp_anydown_1 + 
                  nbp_educational_exclusion +
                  epr_downgraded1 +
                  status_excl +
                  groupsize +
                  SpatialConc +
                  warhist +
                  tek_egip +
                  lag_Polity2 +
                  excl_groups_count +
                  log(lag_pop) +
                  log(lag_rgdpe) +
                  ns(peaceyears, df = 3),
                data = df_analysis_group,
                family = binomial())

summary(m1_logit)

m1_vcov_cluster <- vcovCL(m1_logit, cluster = df_analysis_group$iso3c)

m1_summary_clustered <- coeftest(m1_logit, vcov = m1_vcov_cluster)

print(m1_summary_clustered)


vif(m1_logit)

influencePlot(m1_logit, id.method="identify", main="Influence Plot", sub="Circle size ~ Cook's Distance") 


# PUBLIC EXCLUSION

m1_logit <- glm(onset_ko_flag ~ nbp_public_exclusion +
                  epr_downgraded1 +
                  status_excl +
                  groupsize +
                  SpatialConc +
                  warhist +
                  tek_egip +
                  lag_Polity2 +
                  excl_groups_count +
                  log(lag_pop) +
                  log(lag_rgdpe) +
                  ns(peaceyears, df = 3),
                data = df_analysis_group,
                family = binomial())

summary(m1_logit)

m1_vcov_cluster <- vcovCL(m1_logit, cluster = df_analysis_group$iso3c)

m1_summary_clustered <- coeftest(m1_logit, vcov = m1_vcov_cluster)

print(m1_summary_clustered)


vif(m1_logit)

influencePlot(m1_logit, id.method="identify", main="Influence Plot", sub="Circle size ~ Cook's Distance") 


# INCIDENCE

m1_logit <- glm(incidence_terr_flag ~ lag_nbp_anydown_1 + 
                  nbp_educational_exclusion +
                  epr_downgraded1 +
                  status_excl +
                  groupsize +
                  SpatialConc +
                  warhist +
                  tek_egip +
                  lag_Polity2 +
                  excl_groups_count +
                  log(lag_pop) +
                  log(lag_rgdpe) +
                  ns(peaceyears, df = 3),
                data = df_analysis_group,
                family = binomial())

summary(m1_logit)

m1_vcov_cluster <- vcovCL(m1_logit, cluster = df_analysis_group$iso3c)

m1_summary_clustered <- coeftest(m1_logit, vcov = m1_vcov_cluster)

print(m1_summary_clustered)


vif(m1_logit)

influencePlot(m1_logit, id.method="identify", main="Influence Plot", sub="Circle size ~ Cook's Distance") 


###---- ANALYSIS ---------------------------------------------------------------

# COMPARING POLITICAL AND EDUCATIONAL DOWNGRADES

# ONSET

m1_logit <- glm(onset_ko_flag ~ lag_nbp_anydown_1 + 
                  groupsize +
                  SpatialConc +
                  warhist +
                  tek_egip +
                  lag_Polity2 +
                  excl_groups_count +
                  log(lag_pop) +
                  log(lag_rgdpe) +
                ns(peaceyears, df = 3),
                data = df_analysis_group,
                family = binomial())

summary(m1_logit)

m1_vcov_cluster <- vcovCL(m1_logit, cluster = df_analysis_group$iso3c)

m1_summary_clustered <- coeftest(m1_logit, vcov = m1_vcov_cluster)

print(m1_summary_clustered)


vif(m1_logit)

influencePlot(m1_logit, id.method="identify", main="Influence Plot", sub="Circle size ~ Cook's Distance") 



m2_logit <- glm(onset_ko_flag ~ epr_downgraded1 + 
                  SpatialConc +
                  warhist +
                  tek_egip +
                  Polity2 +
                  excl_groups_count +
                  groupsize +
                  log(lag_pop) +
                  log(lag_rgdpe) +
                  ns(peaceyears, df = 3),
                data = df_analysis_group,
                family = binomial())

summary(m2_logit)

m2_vcov_cluster <- vcovCL(m2_logit, cluster = df_analysis_group$iso3c)

m2_summary_clustered <- coeftest(m2_logit, vcov = m2_vcov_cluster)

print(m2_summary_clustered)


vif(m2_logit)

influencePlot(m2_logit, id.method="identify", main="Influence Plot", sub="Circle size ~ Cook's Distance") 



m3_logit <- glm(onset_ko_flag ~ lag_nbp_anydown_1 +
                  epr_downgraded1 + 
                  SpatialConc +
                  warhist +
                  tek_egip +
                  Polity2 +
                  excl_groups_count +
                  groupsize +
                  log(lag_pop) +
                  log(lag_rgdpe) +
                  ns(peaceyears, df = 3),
                data = df_analysis_group,
                family = binomial())

summary(m3_logit)

m3_vcov_cluster <- vcovCL(m3_logit, cluster = df_analysis_group$iso3c)

m3_summary_clustered <- coeftest(m3_logit, vcov = m3_vcov_cluster)

print(m3_summary_clustered)


vif(m3_logit)

influencePlot(m3_logit, id.method="identify", main="Influence Plot", sub="Circle size ~ Cook's Distance") 



# Interaction Terms: Investigate(!)
### I think we can delete this, cf. above
m3i_logit <- glm(onset_ko_flag ~ lag_nbp_anydown_1 * epr_downgraded1 + 
                  SpatialConc +
                  warhist +
                  tek_egip +
                  Polity2 +
                  excl_groups_count +
                  groupsize +
                  log(lag_pop) +
                  log(lag_rgdpe) +
                  ns(peaceyears, df = 3),
                data = df_analysis_group,
                family = binomial())

summary(m3i_logit)

# Clustered Standard Errors

m3i_vcov_cluster <- vcovCL(m3i_logit, cluster = df_analysis_group$iso3c)
m3i_summary_clustered <- coeftest(m3i_logit, vcov = m3i_vcov_cluster)

print(m3i_summary_clustered)

interaction_plot <- ggpredict(m3i_logit, terms = c("nbp_anydown_1", "epr_downgraded1"))
plot(interaction_plot)

df_analysis_group %>%
  group_by(nbp_anydown_1, epr_downgraded1) %>%
  summarise(prob_onset = mean(onset_ko_flag, na.rm = TRUE),
            count = n())

# incidence

m4_logit <- glm(incidence_flag ~ nbp_anydown_1 + 
                  SpatialConc +
                  warhist +
                  tek_egip +
                  Polity2 +
                  excl_groups_count +
                  groupsize +
                  log(lag_pop) +
                  log(lag_rgdpe) +
                  ns(peaceyears, df = 3),
                data = df_analysis_group,
                family = binomial())

summary(m4_logit)

m4_vcov_cluster <- vcovCL(m4_logit, cluster = df_analysis_group$iso3c)

m4_summary_clustered <- coeftest(m4_logit, vcov = m4_vcov_cluster)

print(m4_summary_clustered)


vif(m4_logit)

influencePlot(m4_logit, id.method="identify", main="Influence Plot", sub="Circle size ~ Cook's Distance") 




m5_logit <- glm(incidence_flag ~ epr_downgraded1 + 
                  SpatialConc +
                  warhist +
                  tek_egip +
                  Polity2 +
                  excl_groups_count +
                  groupsize +
                  log(lag_pop) +
                  log(lag_rgdpe) +
                  ns(peaceyears, df = 3),
                data = df_analysis_group,
                family = binomial())

summary(m5_logit)

m5_vcov_cluster <- vcovCL(m5_logit, cluster = df_analysis_group$iso3c)

m5_summary_clustered <- coeftest(m5_logit, vcov = m5_vcov_cluster)

print(m5_summary_clustered)


vif(m5_logit)

influencePlot(m5_logit, id.method="identify", main="Influence Plot", sub="Circle size ~ Cook's Distance") 


m6_logit <- glm(incidence_flag ~ nbp_anydown_1 +
                  epr_downgraded1 + 
                  SpatialConc +
                  warhist +
                  tek_egip +
                  Polity2 +
                  excl_groups_count +
                  groupsize +
                  log(lag_pop) +
                  log(lag_rgdpe) +
                  ns(peaceyears, df = 3),
                data = df_analysis_group,
                family = binomial())

summary(m6_logit)

m6_vcov_cluster <- vcovCL(m6_logit, cluster = df_analysis_group$iso3c)

m6_summary_clustered <- coeftest(m6_logit, vcov = m6_vcov_cluster)

print(m6_summary_clustered)


vif(m6_logit)

influencePlot(m6_logit, id.method="identify", main="Influence Plot", sub="Circle size ~ Cook's Distance") 


# territorial onset

m7_logit <- glm(onset_ko_terr_flag ~ nbp_anydown_1 + 
                  SpatialConc +
                  warhist +
                  peaceyears +
                  tek_egip +
                  Polity2 +
                  excl_groups_count +
                  groupsize +
                  log(lag_pop) +
                  log(lag_rgdpe) +
                ns(peaceyears, df = 3),
                data = df_analysis_group,
                family = binomial())

summary(m7_logit)

m7_vcov_cluster <- vcovCL(m7_logit, cluster = df_analysis_group$iso3c)

m7_summary_clustered <- coeftest(m7_logit, vcov = m7_vcov_cluster)

print(m7_summary_clustered)


m8_logit <- glm(onset_ko_terr_flag ~ epr_downgraded1 + 
                  SpatialConc +
                  warhist +
                  tek_egip +
                  Polity2 +
                  excl_groups_count +
                  groupsize +
                  log(lag_pop) +
                  log(lag_rgdpe) +
                  ns(peaceyears, df = 3),
                data = df_analysis_group,
                family = binomial())

summary(m8_logit)

m8_vcov_cluster <- vcovCL(m8_logit, cluster = df_analysis_group$iso3c)

m8_summary_clustered <- coeftest(m8_logit, vcov = m8_vcov_cluster)

print(m8_summary_clustered)


m9_logit <- glm(onset_ko_terr_flag ~ nbp_anydown_1 +
                  epr_downgraded1 + 
                  SpatialConc +
                  warhist +
                  tek_egip +
                  Polity2 +
                  excl_groups_count +
                  groupsize + 
                  log(lag_pop) +
                  log(lag_rgdpe) +
                  ns(peaceyears, df = 3),
                data = df_analysis_group,
                family = binomial())

summary(m9_logit)

m9_vcov_cluster <- vcovCL(m9_logit, cluster = df_analysis_group$iso3c)

m9_summary_clustered <- coeftest(m9_logit, vcov = m9_vcov_cluster)

print(m9_summary_clustered)

# territorial incidence

m10_logit <- glm(incidence_terr_flag ~ nbp_anydown_1 + 
                   SpatialConc +
                   warhist +
                   peaceyears +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe) +
                   ns(peaceyears, df = 3) +
                   factor(un.region.name),
                 data = df_analysis_group,
                 family = binomial())

summary(m10_logit)

m10_vcov_cluster <- vcovCL(m10_logit, cluster = df_analysis_group$iso3c)

m10_summary_clustered <- coeftest(m10_logit, vcov = m10_vcov_cluster)

print(m10_summary_clustered)


m11_logit <- glm(incidence_terr_flag ~ epr_downgraded1 + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m11_logit)

m11_vcov_cluster <- vcovCL(m11_logit, cluster = df_analysis_group$iso3c)

m11_summary_clustered <- coeftest(m11_logit, vcov = m11_vcov_cluster)

print(m11_summary_clustered)


m12_logit <- glm(incidence_terr_flag ~ nbp_anydown_1 +
                   epr_downgraded1 + 
                   SpatialConc +
                   warhist +
                   peaceyears +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m12_logit)

m12_vcov_cluster <- vcovCL(m12_logit, cluster = df_analysis_group$iso3c)

m12_summary_clustered <- coeftest(m12_logit, vcov = m12_vcov_cluster)

print(m12_summary_clustered)


stargazer(
  m10_logit, m11_logit, m12_logit, type = "text")

stargazer(
  m10_summary_clustered, m11_summary_clustered, m12_summary_clustered, type = "text")

stargazer(
  m10_logit, m11_logit, m12_logit, type = "html", out = "downgrade_terr_inc_regr.html")

stargazer(
  m10_summary_clustered, m11_summary_clustered, m12_summary_clustered, type = "html", out = "downgrade_terr_inc_reg_clustered.html")


# HORIZONTAL INEQUALITY

m13_logit <- glm(onset_ko_flag ~ lag_HI + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m13_logit)

m13_vcov_cluster <- vcovCL(m13_logit, cluster = df_analysis_group$iso3c)

m13_summary_clustered <- coeftest(m13_logit, vcov = m13_vcov_cluster)

print(m13_summary_clustered)


m14_logit <- glm(onset_ko_terr_flag ~ lag_HI + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize + 
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m14_logit)

m14_vcov_cluster <- vcovCL(m14_logit, cluster = df_analysis_group$iso3c)

m14_summary_clustered <- coeftest(m14_logit, vcov = m14_vcov_cluster)

print(m14_summary_clustered)


m15_logit <- glm(incidence_flag ~ lag_HI + 
                   SpatialConc +
                   warhist +
                   peaceyears +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize + 
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m15_logit)

m15_vcov_cluster <- vcovCL(m15_logit, cluster = df_analysis_group$iso3c)

m15_summary_clustered <- coeftest(m15_logit, vcov = m15_vcov_cluster)

print(m15_summary_clustered)


m16_logit <- glm(incidence_terr_flag ~ lag_HI + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m16_logit)

m16_vcov_cluster <- vcovCL(m16_logit, cluster = df_analysis_group$iso3c)

m16_summary_clustered <- coeftest(m16_logit, vcov = m16_vcov_cluster)

print(m16_summary_clustered)

# SDM

m17_logit <- glm(SDM ~ nbp_anydown_1 + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m17_logit)

m17_vcov_cluster <- vcovCL(m17_logit, cluster = df_analysis_group$iso3c)

m17_summary_clustered <- coeftest(m17_logit, vcov = m17_vcov_cluster)

print(m17_summary_clustered)


m18_logit <- glm(SDM ~ epr_downgraded1 + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m18_logit)

m18_vcov_cluster <- vcovCL(m18_logit, cluster = df_analysis_group$iso3c)

m18_summary_clustered <- coeftest(m18_logit, vcov = m18_vcov_cluster)

print(m18_summary_clustered)


m19_logit <- glm(SDM ~ nbp_anydown_1 +
                   epr_downgraded1 + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m19_logit)

m19_vcov_cluster <- vcovCL(m19_logit, cluster = df_analysis_group$iso3c)

m19_summary_clustered <- coeftest(m19_logit, vcov = m19_vcov_cluster)

print(m19_summary_clustered)


# SDM without and migrant groups and core groups

df_analysis_sdm <- df_analysis %>%
  filter(ArrivedPoliticalMigrantsRefugees != 1, ArrivedLabourMigrants != 1)


m17_logit_alt <- glm(SDM ~ nbp_anydown_1 + 
                       SpatialConc +
                       warhist +
                       tek_egip +
                       Polity2 +
                       excl_groups_count +
                       groupsize + 
                       log(lag_pop) +
                       log(lag_rgdpe)+
                       ns(peaceyears, df = 3),
                     data = df_analysis_sdm,
                     family = binomial())

summary(m17_logit_alt)

m17_vcov_cluster_alt <- vcovCL(m17_logit_alt, cluster = df_analysis_sdm$iso3c)

m17_summary_clustered_alt <- coeftest(m17_logit_alt, vcov = m17_vcov_cluster_alt)

print(m17_summary_clustered_alt)


m18_logit_alt <- glm(SDM ~ epr_downgraded1 + 
                       SpatialConc +
                       warhist +
                       peaceyears +
                       tek_egip +
                       Polity2 +
                       excl_groups_count +
                       groupsize + 
                       log(lag_pop) +
                       log(lag_rgdpe)+
                       ns(peaceyears, df = 3),
                     data = df_analysis_sdm,
                     family = binomial())

summary(m18_logit_alt)

m18_vcov_cluster_alt <- vcovCL(m18_logit_alt, cluster = df_analysis_sdm$iso3c)

m18_summary_clustered_alt <- coeftest(m18_logit_alt, vcov = m18_vcov_cluster_alt)

print(m18_summary_clustered_alt)


m19_logit_alt <- glm(SDM ~ nbp_anydown_1 +
                       epr_downgraded1 + 
                       SpatialConc +
                       warhist +
                       tek_egip +
                       Polity2 +
                       excl_groups_count +
                       groupsize +
                       log(lag_pop) +
                       log(lag_rgdpe)+
                       ns(peaceyears, df = 3),
                     data = df_analysis_sdm,
                     family = binomial())

summary(m19_logit_alt)

m19_vcov_cluster_alt <- vcovCL(m19_logit_alt, cluster = df_analysis_sdm$iso3c)

m19_summary_clustered_alt <- coeftest(m19_logit_alt, vcov = m19_vcov_cluster_alt)

print(m19_summary_clustered_alt)


df_analysis_sdm2 <- df_analysis_sdm %>%
  filter(CoreGp != 1)

m17_logit_alt2 <- glm(SDM ~ nbp_anydown_1 + 
                        SpatialConc +
                        warhist +
                        tek_egip +
                        Polity2 +
                        excl_groups_count +
                        groupsize +
                        log(lag_pop) +
                        log(lag_rgdpe)+
                        ns(peaceyears, df = 3),
                      data = df_analysis_sdm2,
                      family = binomial())

summary(m17_logit_alt2)

m17_vcov_cluster_alt2 <- vcovCL(m17_logit_alt2, cluster = df_analysis_sdm2$iso3c)

m17_summary_clustered_alt2 <- coeftest(m17_logit_alt2, vcov = m17_vcov_cluster_alt2)

print(m17_summary_clustered_alt2)


m18_logit_alt2 <- glm(SDM ~ epr_downgraded1 + 
                        SpatialConc +
                        warhist +
                        tek_egip +
                        Polity2 +
                        excl_groups_count +
                        groupsize +
                        log(lag_pop) +
                        log(lag_rgdpe)+
                        ns(peaceyears, df = 3),
                      data = df_analysis_sdm2,
                      family = binomial())

summary(m18_logit_alt2)

m18_vcov_cluster_alt2 <- vcovCL(m18_logit_alt2, cluster = df_analysis_sdm2$iso3c)

m18_summary_clustered_alt2 <- coeftest(m18_logit_alt2, vcov = m18_vcov_cluster_alt2)

print(m18_summary_clustered_alt2)


m19_logit_alt2 <- glm(SDM ~ nbp_anydown_1 +
                        epr_downgraded1 + 
                        SpatialConc +
                        warhist +
                        tek_egip +
                        Polity2 +
                        excl_groups_count +
                        groupsize +
                        log(lag_pop) +
                        log(lag_rgdpe)+
                        ns(peaceyears, df = 3),
                      data = df_analysis_sdm2,
                      family = binomial())

summary(m19_logit_alt2)

m19_vcov_cluster_alt2 <- vcovCL(m19_logit_alt2, cluster = df_analysis_sdm2$iso3c)

m19_summary_clustered_alt2 <- coeftest(m19_logit_alt2, vcov = m19_vcov_cluster_alt2)

print(m19_summary_clustered_alt2)


# Status excluded as IV

m20_logit <- glm(onset_ko_flag ~ nbp_educational_exclusion + 
                   SpatialConc +
                   warhist +
                   peaceyears +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m20_logit)

m20_vcov_cluster <- vcovCL(m20_logit, cluster = df_analysis_group$iso3c)

m20_summary_clustered <- coeftest(m20_logit, vcov = m20_vcov_cluster)

print(m20_summary_clustered)



m21_logit <- glm(onset_ko_flag ~ nbp_public_exclusion + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m21_logit)

m21_vcov_cluster <- vcovCL(m21_logit, cluster = df_analysis_group$iso3c)

m21_summary_clustered <- coeftest(m21_logit, vcov = m21_vcov_cluster)

print(m21_summary_clustered)



m22_logit <- glm(onset_ko_flag ~ status_excl + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m22_logit)

m22_vcov_cluster <- vcovCL(m22_logit, cluster = df_analysis_group$iso3c)

m22_summary_clustered <- coeftest(m22_logit, vcov = m22_vcov_cluster)

print(m22_summary_clustered)


m23_logit <- glm(onset_ko_flag ~ nbp_educational_exclusion +
                   status_excl + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m23_logit)

m23_vcov_cluster <- vcovCL(m23_logit, cluster = df_analysis_group$iso3c)

m23_summary_clustered <- coeftest(m23_logit, vcov = m23_vcov_cluster)

print(m23_summary_clustered)


# Exclusion on incidence

m24_logit <- glm(incidence_flag ~ nbp_educational_exclusion + 
                   SpatialConc +
                   warhist +
                   peaceyears +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m24_logit)

m24_vcov_cluster <- vcovCL(m24_logit, cluster = df_analysis_group$iso3c)

m24_summary_clustered <- coeftest(m24_logit, vcov = m24_vcov_cluster)

print(m24_summary_clustered)


m25_logit <- glm(incidence_flag ~ nbp_public_exclusion + 
                   SpatialConc +
                   warhist +
                   peaceyears +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m25_logit)

m25_vcov_cluster <- vcovCL(m25_logit, cluster = df_analysis_group$iso3c)

m25_summary_clustered <- coeftest(m25_logit, vcov = m25_vcov_cluster)

print(m25_summary_clustered)


m26_logit <- glm(incidence_flag ~ status_excl + 
                   SpatialConc +
                   warhist +
                   peaceyears +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m26_logit)

m26_vcov_cluster <- vcovCL(m26_logit, cluster = df_analysis_group$iso3c)

m26_summary_clustered <- coeftest(m26_logit, vcov = m26_vcov_cluster)

print(m26_summary_clustered)


m27_logit <- glm(incidence_flag ~ nbp_educational_exclusion +
                   status_excl + 
                   SpatialConc +
                   warhist +
                   peaceyears +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m27_logit)

m27_vcov_cluster <- vcovCL(m27_logit, cluster = df_analysis_group$iso3c)

m27_summary_clustered <- coeftest(m27_logit, vcov = m27_vcov_cluster)

print(m27_summary_clustered)


# exclusion on territorial incidence

m28_logit <- glm(incidence_terr_flag ~ nbp_educational_exclusion + 
                   SpatialConc +
                   warhist +
                   peaceyears +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3) ,
                 data = df_analysis_group,
                 family = binomial())

summary(m28_logit)

m28_vcov_cluster <- vcovCL(m28_logit, cluster = df_analysis_group$iso3c)

m28_summary_clustered <- coeftest(m28_logit, vcov = m28_vcov_cluster)

print(m28_summary_clustered)


m29_logit <- glm(incidence_terr_flag ~ nbp_public_exclusion + 
                   SpatialConc +
                   warhist +
                   peaceyears +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3) ,
                 data = df_analysis_group,
                 family = binomial())

summary(m29_logit)

m29_vcov_cluster <- vcovCL(m29_logit, cluster = df_analysis_group$iso3c)

m29_summary_clustered <- coeftest(m29_logit, vcov = m29_vcov_cluster)

print(m29_summary_clustered)


m30_logit <- glm(incidence_terr_flag ~ status_excl + 
                   SpatialConc +
                   warhist +
                   peaceyears +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m30_logit)

m30_vcov_cluster <- vcovCL(m30_logit, cluster = df_analysis_group$iso3c)

m30_summary_clustered <- coeftest(m30_logit, vcov = m30_vcov_cluster)

print(m30_summary_clustered)


m31_logit <- glm(incidence_terr_flag ~ nbp_public_exclusion +
                   status_excl + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m31_logit)

m31_vcov_cluster <- vcovCL(m31_logit, cluster = df_analysis_group$iso3c)

m31_summary_clustered <- coeftest(m31_logit, vcov = m31_vcov_cluster)

print(m31_summary_clustered)


stargazer(m28_logit, m29_logit, m30_logit, m31_logit, type = "text")

stargazer(m29_summary_clustered, m30_summary_clustered, m31_summary_clustered, type = "text")

stargazer(m28_logit, m29_logit, m30_logit, m31_logit, type = "html", out = "excl_terr_inc_regr.html")

stargazer(m29_summary_clustered, m30_summary_clustered, m31_summary_clustered, type = "html", out = "excl_terr_inc_clustred.html")


# Any LOI on Onset, incidence, and territorial incidence


m32_logit <- glm(onset_ko_flag ~ nbp_any_loi + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m32_logit)

m32_vcov_cluster <- vcovCL(m32_logit, cluster = df_analysis_group$iso3c)

m32_summary_clustered <- coeftest(m32_logit, vcov = m32_vcov_cluster)

print(m32_summary_clustered)



m33_logit <- glm(incidence_flag ~ nbp_any_loi + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m33_logit)

m33_vcov_cluster <- vcovCL(m33_logit, cluster = df_analysis_group$iso3c)

m33_summary_clustered <- coeftest(m33_logit, vcov = m33_vcov_cluster)

print(m33_summary_clustered)



m34_logit <- glm(incidence_terr_flag ~ nbp_any_loi + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m34_logit)

m34_vcov_cluster <- vcovCL(m34_logit, cluster = df_analysis_group$iso3c)

m34_summary_clustered <- coeftest(m34_logit, vcov = m34_vcov_cluster)

print(m34_summary_clustered)


# Any LC

m35_logit <- glm(onset_ko_flag ~ nbp_any_lc + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m35_logit)

m35_vcov_cluster <- vcovCL(m35_logit, cluster = df_analysis_group$iso3c)

m35_summary_clustered <- coeftest(m35_logit, vcov = m35_vcov_cluster)

print(m35_summary_clustered)



m36_logit <- glm(incidence_flag ~ nbp_any_lc + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m36_logit)

m36_vcov_cluster <- vcovCL(m36_logit, cluster = df_analysis_group$iso3c)

m36_summary_clustered <- coeftest(m36_logit, vcov = m36_vcov_cluster)

print(m36_summary_clustered)



m37_logit <- glm(incidence_terr_flag ~ nbp_any_lc + 
                   SpatialConc +
                   warhist +
                   tek_egip +
                   Polity2 +
                   excl_groups_count +
                   groupsize +
                   log(lag_pop) +
                   log(lag_rgdpe)+
                   ns(peaceyears, df = 3),
                 data = df_analysis_group,
                 family = binomial())

summary(m37_logit)

m37_vcov_cluster <- vcovCL(m37_logit, cluster = df_analysis_group$iso3c)

m37_summary_clustered <- coeftest(m37_logit, vcov = m37_vcov_cluster)

print(m37_summary_clustered)



# Second: fixed effects models - I recall we talked about two-way fixed models so lets jump directly to fixed effects here. first country fixed effects and then country year two-way fixed effects 

# Country fixed effects

m1_fixed <- feglm(onset_ko_flag ~ nbp_anydown_1 + 
                    SpatialConc +
                    warhist +
                    tek_egip +
                    Polity2 +
                    excl_groups_count +
                    groupsize +
                    log(lag_pop) +
                    log(lag_rgdpe) +
                    ns(peaceyears, df = 3) | iso3c,
                  data = df_analysis_group,
                  family = binomial())

summary(m1_fixed)
summary(m1_fixed, cluster = "Group")


m2_fixed <- feglm(onset_ko_flag ~ epr_downgraded1 + 
                    SpatialConc +
                    warhist +
                    tek_egip +
                    Polity2 +
                    excl_groups_count +
                    groupsize +
                    log(lag_pop) +
                    log(lag_rgdpe) +
                    ns(peaceyears, df = 3)| iso3c,
                  data = df_analysis_group,
                  family = binomial())

summary(m2_fixed)
summary(m2_fixed, cluster = "Group")


m3_fixed <- feglm(onset_ko_flag ~ nbp_anydown_1 +
                    epr_downgraded1 +
                    SpatialConc +
                    warhist +
                    tek_egip +
                    Polity2 +
                    excl_groups_count + 
                    groupsize +
                    log(lag_pop) +
                    log(lag_rgdpe) +
                    ns(peaceyears, df = 3) | iso3c,
                  data = df_analysis_group,
                  family = binomial())

summary(m3_fixed)
summary(m3_fixed, cluster = "Group")


m4_fixed <- feglm(onset_ko_flag ~ HI + 
                    SpatialConc +
                    warhist +
                    tek_egip +
                    Polity2 +
                    excl_groups_count +
                    groupsize +
                    log(lag_pop) +
                    log(lag_rgdpe) +
                    ns(peaceyears, df = 3)| iso3c,
                  data = df_analysis_group,
                  family = binomial())

summary(m4_fixed)
summary(m4_fixed, cluster = "Group")


m5_fixed <- feglm(incidence_flag ~ nbp_anydown_1 + 
                    SpatialConc +
                    warhist +
                    tek_egip +
                    Polity2 +
                    excl_groups_count +
                    groupsize +
                    log(lag_pop) +
                    log(lag_rgdpe) +
                    ns(peaceyears, df = 3)| iso3c,
                  data = df_analysis_group,
                  family = binomial())

summary(m5_fixed)
summary(m5_fixed, cluster = "Group")


m6_fixed <- feglm(incidence_flag ~ epr_downgraded1 + 
                    SpatialConc +
                    warhist +
                    tek_egip +
                    Polity2 +
                    excl_groups_count +
                    groupsize +
                    log(lag_pop) +
                    log(lag_rgdpe) +
                    ns(peaceyears, df = 3) | iso3c,
                  data = df_analysis_group,
                  family = binomial())

summary(m6_fixed)
summary(m6_fixed, cluster = "Group")


m7_fixed <- feglm(incidence_flag ~ nbp_anydown_1 +
                    epr_downgraded1 +
                    SpatialConc +
                    warhist +
                    tek_egip +
                    Polity2 +
                    excl_groups_count +
                    groupsize +
                    log(lag_pop) +
                    log(lag_rgdpe) +
                    ns(peaceyears, df = 3) | iso3c,
                  data = df_analysis_group,
                  family = binomial())

summary(m7_fixed)
summary(m7_fixed, cluster = "Group")


m8_fixed <- feglm(incidence_flag ~ HI + 
                    SpatialConc +
                    warhist +
                    tek_egip +
                    Polity2 +
                    excl_groups_count +
                    groupsize +
                    log(lag_pop) +
                    log(lag_rgdpe) +
                    ns(peaceyears, df = 3)| iso3c,
                  data = df_analysis_group,
                  family = binomial())

summary(m8_fixed)
summary(m8_fixed, cluster = "Group")

# Two-ways fixed effects

m1_2w_fixed <- feglm(onset_ko_flag ~ nbp_anydown_1 + 
                       SpatialConc +
                       warhist +
                       tek_egip +
                       Polity2 +
                       excl_groups_count +
                       groupsize +
                       log(lag_pop) +
                       log(lag_rgdpe) +
                       ns(peaceyears, df = 3) | iso3c + Year,
                     data = df_analysis_group,
                     family = binomial())

summary(m1_2w_fixed)
summary(m1_2w_fixed, cluster = "Group")


m2_2w_fixed <- feglm(onset_ko_flag ~ epr_downgraded1 + 
                       SpatialConc +
                       warhist +
                       tek_egip +
                       Polity2 +
                       excl_groups_count +
                       groupsize +
                       log(lag_pop) +
                       log(lag_rgdpe) +
                       ns(peaceyears, df = 3)| iso3c + Year,
                     data = df_analysis_group,
                     family = binomial())

summary(m2_2w_fixed)
summary(m2_2w_fixed, cluster = "Group")


m3_2w_fixed <- feglm(onset_ko_flag ~ nbp_anydown_1 +
                       epr_downgraded1 +
                       SpatialConc +
                       warhist +
                       tek_egip +
                       Polity2 +
                       excl_groups_count +
                       groupsize +
                       log(lag_pop) +
                       log(lag_rgdpe) +
                       ns(peaceyears, df = 3)| iso3c + Year,
                     data = df_analysis_group,
                     family = binomial())

summary(m3_2w_fixed)
summary(m3_2w_fixed, cluster = "Group")


m4_2w_fixed <- feglm(onset_ko_flag ~ HI + 
                       SpatialConc +
                       warhist +
                       tek_egip +
                       Polity2 +
                       excl_groups_count +
                       groupsize +
                       log(lag_pop) +
                       log(lag_rgdpe) +
                       ns(peaceyears, df = 3)| iso3c + Year,
                     data = df_analysis_group,
                     family = binomial())

summary(m4_2w_fixed)
summary(m4_2w_fixed, cluster = "Group")


m5_2w_fixed <- feglm(incidence_flag ~ nbp_anydown_1 + 
                       SpatialConc +
                       warhist +
                       tek_egip +
                       Polity2 +
                       excl_groups_count +
                       groupsize +
                       log(lag_pop) +
                       log(lag_rgdpe) +
                       ns(peaceyears, df = 3)| iso3c + Year,
                     data = df_analysis_group,
                     family = binomial())

summary(m5_2w_fixed)
summary(m5_2w_fixed, cluster = "Group")


m6_2w_fixed <- feglm(incidence_flag ~ epr_downgraded1 + 
                       SpatialConc +
                       warhist +
                       tek_egip +
                       Polity2 +
                       excl_groups_count +
                       groupsize +
                       log(lag_pop) +
                       log(lag_rgdpe) +
                       ns(peaceyears, df = 3)| iso3c + Year,
                     data = df_analysis_group,
                     family = binomial())

summary(m6_2w_fixed)
summary(m6_2w_fixed, cluster = "Group")


m7_2w_fixed <- feglm(incidence_flag ~ nbp_anydown_1 +
                       epr_downgraded1 +
                       SpatialConc +
                       warhist +
                       tek_egip +
                       Polity2 +
                       excl_groups_count +
                       groupsize +
                       log(lag_pop) +
                       log(lag_rgdpe) +
                       ns(peaceyears, df = 3)| iso3c + Year,
                     data = df_analysis_group,
                     family = binomial())

summary(m7_2w_fixed)
summary(m7_2w_fixed, cluster = "Group")


m8_2w_fixed <- feglm(incidence_flag ~ HI + 
                       SpatialConc +
                       warhist +
                       tek_egip +
                       Polity2 +
                       excl_groups_count +
                       groupsize +
                       log(lag_pop) +
                       log(lag_rgdpe) +
                       ns(peaceyears, df = 3)| iso3c + Year,
                     data = df_analysis_group,
                     family = binomial())

summary(m8_2w_fixed)
summary(m8_2w_fixed, cluster = "Group")

## DOWNGRADE ANALYSES WITH LAGGED VARS

m1_logit_lag <- glm(onset_ko_flag ~ lag_nbp_anydown_1 + 
                      lag_groupsize +
                      lag_SpatialConc +
                      warhist +
                      tek_egip +
                      lag_Polity2 +
                      excl_groups_count +
                      log(lag_pop) +
                      log(lag_rgdpe)+
                      ns(peaceyears, df = 3),
                    data = df_analysis_group,
                    family = binomial())

summary(m1_logit_lag)

m1_lag_vcov_cluster <- vcovCL(m1_logit_lag, cluster = df_analysis_group$iso3c)

m1_lag_summary_clustered <- coeftest(m1_logit_lag, vcov = m1_lag_vcov_cluster)

print(m1_lag_summary_clustered)


m2_logit_lag <- glm(onset_ko_flag ~ lag_nbp_anydown_1 +
                      epr_downgraded1 + 
                      lag_SpatialConc +
                      warhist +
                      tek_egip +
                      lag_Polity2 +
                      excl_groups_count +
                      lag_groupsize +
                      log(lag_pop) +
                      log(lag_rgdpe)+
                      ns(peaceyears, df = 3),
                    data = df_analysis_group,
                    family = binomial())

summary(m2_logit_lag)

m2_lag_vcov_cluster <- vcovCL(m2_logit_lag, cluster = df_analysis_group$iso3c)

m2_lag_summary_clustered <- coeftest(m2_logit_lag, vcov = m2_lag_vcov_cluster)

print(m2_lag_summary_clustered)

# incidence

m3_logit_lag <- glm(incidence_flag ~ lag_nbp_anydown_1 + 
                      lag_SpatialConc +
                      warhist +
                      tek_egip +
                      lag_Polity2 +
                      excl_groups_count +
                      lag_groupsize +
                      log(lag_pop) +
                      log(lag_rgdpe)+
                      ns(peaceyears, df = 3),
                    data = df_analysis_group,
                    family = binomial())

summary(m3_logit_lag)

m3_lag_vcov_cluster <- vcovCL(m3_logit_lag, cluster = df_analysis_group$iso3c)

m3_lag_summary_clustered <- coeftest(m3_logit_lag, vcov = m3_lag_vcov_cluster)

print(m3_lag_summary_clustered)



m4_logit_lag <- glm(incidence_flag ~ lag_nbp_anydown_1 +
                      epr_downgraded1 + 
                      lag_SpatialConc +
                      warhist +
                      peaceyears +
                      tek_egip +
                      lag_Polity2 +
                      excl_groups_count +
                      lag_groupsize +
                      log(lag_pop) +
                      log(lag_rgdpe)+
                      ns(peaceyears, df = 3),
                    data = df_analysis_group,
                    family = binomial())

summary(m4_logit_lag)

m4_lag_vcov_cluster <- vcovCL(m4_logit_lag, cluster = df_analysis_group$iso3c)

m4_lag_summary_clustered <- coeftest(m4_logit_lag, vcov = m4_lag_vcov_cluster)

print(m4_lag_summary_clustered)

# territorial onset

m5_logit_lag <- glm(onset_ko_terr_flag ~ lag_nbp_anydown_1 + 
                      lag_SpatialConc +
                      warhist +
                      peaceyears +
                      tek_egip +
                      lag_Polity2 +
                      excl_groups_count +
                      lag_groupsize +
                      log(lag_pop) +
                      log(lag_rgdpe)+
                      ns(peaceyears, df = 3),
                    data = df_analysis_group,
                    family = binomial())

summary(m5_logit_lag)

m5_lag_vcov_cluster <- vcovCL(m5_logit_lag, cluster = df_analysis_group$iso3c)

m5_lag_summary_clustered <- coeftest(m5_logit_lag, vcov = m5_lag_vcov_cluster)

print(m5_lag_summary_clustered)


m6_logit_lag <- glm(onset_ko_terr_flag ~ lag_nbp_anydown_1 +
                      epr_downgraded1 + 
                      lag_SpatialConc +
                      warhist +
                      peaceyears +
                      tek_egip +
                      lag_Polity2 +
                      excl_groups_count +
                      lag_groupsize + 
                      log(lag_pop) +
                      log(lag_rgdpe)+
                      ns(peaceyears, df = 3),
                    data = df_analysis_group,
                    family = binomial())

summary(m6_logit_lag)

m6_lag_vcov_cluster <- vcovCL(m6_logit_lag, cluster = df_analysis_group$iso3c)

m6_lag_summary_clustered <- coeftest(m6_logit_lag, vcov = m6_lag_vcov_cluster)

print(m6_lag_summary_clustered)

# territorial incidence

m7_logit_lag <- glm(incidence_terr_flag ~ lag_nbp_anydown_1 + 
                      lag_SpatialConc +
                      warhist +
                      peaceyears +
                      tek_egip +
                      lag_Polity2 +
                      excl_groups_count +
                      groupsize +
                      log(lag_pop) +
                      log(lag_rgdpe)+
                      ns(peaceyears, df = 3),
                    data = df_analysis_group,
                    family = binomial())

summary(m7_logit_lag)

m7_lag_vcov_cluster <- vcovCL(m7_logit_lag, cluster = df_analysis_group$iso3c)

m7_lag_summary_clustered <- coeftest(m7_logit_lag, vcov = m7_lag_vcov_cluster)

print(m7_lag_summary_clustered)



m8_logit_lag <- glm(incidence_terr_flag ~ lag_nbp_anydown_1 +
                      epr_downgraded1 + 
                      lag_SpatialConc +
                      warhist +
                      peaceyears +
                      tek_egip +
                      lag_Polity2 +
                      excl_groups_count +
                      lag_groupsize +
                      log(lag_pop) +
                      log(lag_rgdpe)+
                      ns(peaceyears, df = 3),
                    data = df_analysis_group,
                    family = binomial())

summary(m8_logit_lag)

m8_lag_vcov_cluster <- vcovCL(m8_logit_lag, cluster = df_analysis_group$iso3c)

m8_lag_summary_clustered <- coeftest(m8_logit_lag, vcov = m8_lag_vcov_cluster)

print(m8_lag_summary_clustered)


# CONFLICT INTENSITY

df_analysis_conflicts$intensity_level <- factor(df_analysis_conflicts$intensity_level, levels = c(1, 2))


m1_intensity <- glm(intensity_level ~ nbp_anydown_1 + 
                      groupsize +
                      SpatialConc +
                      warhist +
                      peaceyears +
                      tek_egip +
                      Polity2 +
                      excl_groups_count +
                      log(lag_pop) +
                      log(lag_rgdpe),
                    data = df_analysis_conflicts,
                    family = binomial())

summary(m1_intensity)

m1_intensity_vcov_cluster <- vcovCL(m1_intensity, cluster = df_analysis_conflicts$iso3c)

m1_intensity_summary_clustered <- coeftest(m1_intensity, vcov = m1_intensity_vcov_cluster)

print(m1_intensity_summary_clustered)



m2_intensity <- glm(intensity_level ~ HI + 
                      groupsize +
                      SpatialConc +
                      warhist +
                      peaceyears +
                      tek_egip +
                      Polity2 +
                      excl_groups_count +
                      log(lag_pop) +
                      log(lag_rgdpe)+
                      ns(peaceyears, df = 3),
                    data = df_analysis_conflicts,
                    family = binomial())

summary(m2_intensity)

m2_intensity_vcov_cluster <- vcovCL(m2_intensity, cluster = df_analysis_conflicts$iso3c)

m2_intensity_summary_clustered <- coeftest(m2_intensity, vcov = m2_intensity_vcov_cluster)

print(m2_intensity_summary_clustered)



m3_intensity <- glm(intensity_level ~ lag_nbp_anydown_1 + 
                      lag_groupsize +
                      lag_SpatialConc +
                      warhist +
                      peaceyears +
                      tek_egip +
                      lag_Polity2 +
                      excl_groups_count +
                      log(lag_pop) +
                      log(lag_rgdpe)+
                      ns(peaceyears, df = 3),
                    data = df_analysis_conflicts,
                    family = binomial())

summary(m3_intensity)

m3_intensity_vcov_cluster <- vcovCL(m3_intensity, cluster = df_analysis_conflicts$iso3c)

m3_intensity_summary_clustered <- coeftest(m3_intensity, vcov = m3_intensity_vcov_cluster)

print(m3_intensity_summary_clustered)



m4_intensity <- glm(intensity_level ~ lag_HI + 
                      lag_groupsize +
                      lag_SpatialConc +
                      warhist +
                      peaceyears +
                      tek_egip +
                      lag_Polity2 +
                      excl_groups_count +
                      log(lag_pop) +
                      log(lag_rgdpe)+
                      ns(peaceyears, df = 3),
                    data = df_analysis_conflicts,
                    family = binomial())

summary(m4_intensity)

m4_intensity_vcov_cluster <- vcovCL(m4_intensity, cluster = df_analysis_conflicts$iso3c)

m4_intensity_summary_clustered <- coeftest(m4_intensity, vcov = m4_intensity_vcov_cluster)

print(m4_intensity_summary_clustered)




m5_intensity <- glm(intensity_level ~ Monolingual + 
                      groupsize +
                      SpatialConc +
                      warhist +
                      peaceyears +
                      tek_egip +
                      Polity2 +
                      excl_groups_count +
                      log(lag_pop) +
                      log(lag_rgdpe) +
                      ns(peaceyears, df = 3),
                    data = df_analysis_conflicts,
                    family = binomial())

summary(m5_intensity)

m5_intensity_vcov_cluster <- vcovCL(m5_intensity, cluster = df_analysis_conflicts$iso3c)

m5_intensity_summary_clustered <- coeftest(m5_intensity, vcov = m5_intensity_vcov_cluster)

print(m5_intensity_summary_clustered)



m6_intensity <- glm(intensity_level ~ lag_Monolingual + 
                      lag_groupsize +
                      lag_SpatialConc +
                      warhist +
                      peaceyears +
                      tek_egip +
                      lag_Polity2 +
                      excl_groups_count +
                      log(lag_pop) +
                      log(lag_rgdpe)+
                      ns(peaceyears, df = 3),
                    data = df_analysis_conflicts,
                    family = binomial())

summary(m6_intensity)

m6_intensity_vcov_cluster <- vcovCL(m6_intensity, cluster = df_analysis_conflicts$iso3c)

m6_intensity_summary_clustered <- coeftest(m6_intensity, vcov = m6_intensity_vcov_cluster)

print(m6_intensity_summary_clustered)





