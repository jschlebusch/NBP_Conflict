###-----------------------------------------------------------------------------
### MERGING NBP AND UCDP DYAD DATA
### 
### Jan
###
### 01 - 02/2025
###-----------------------------------------------------------------------------


##---- Packages ----------------------------------------------------------------
library(tidyverse)
library(haven)

##---- Data --------------------------------------------------------------------
#df_nbp <- read_dta("NBP_groups_final.dta")
#df_epr2nbp <- read_dta("EPR2NBP_Emre2.dta")
df_dyads <- openxlsx::read.xlsx("Dyadic_v24_1.xlsx")
df_acd2epr <- openxlsx::read.xlsx("ACD2EPR-2021.xlsx")

##---- Merging -----------------------------------------------------------------

df_dyads <- df_dyads %>%
  select(c(dyad_id, conflict_id, location, side_b_id, year, intensity_level)) %>%
  mutate(dyad_id = as.factor(dyad_id),
         side_b_id = as.factor(side_b_id))

summary(df_dyads)

df_ethnicc <- df_acd2epr %>%
  select(c(gwid, dyadid, sideb, sideb_id, group, gwgroupid)) %>%
  filter(!is.na(gwgroupid)) %>%
  rename(dyad_id = dyadid,
         side_b_id = sideb_id) %>%
  mutate(dyad_id = as.factor(dyad_id),
         side_b_id = as.factor(side_b_id))

summary(df_ethnicc)

df_ethnicdyads <- df_dyads %>%
  left_join(df_ethnicc, by = c("dyad_id", "side_b_id"))

summary(df_ethnicdyads)

openxlsx::write.xlsx(df_ethnicdyads, "dyads_intensity_groups.xlsx")
