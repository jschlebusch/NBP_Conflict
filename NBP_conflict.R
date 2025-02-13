###-----------------------------------------------------------------------------
### LINGUISTIC INEQUALITY AND ETHNIC CONFLICT
### 
### Jan 
###
### 12/2024 - 02/2025
###-----------------------------------------------------------------------------

###---- Packages ----------------------------------------------------------------

library(tidyverse)
library(haven)
library(readxl)
library(ggplot2)
library(ggthemes)
library(ggmosaic)
library(reshape2)
library(psych)
library(countrycode)
library(vcd)
library(naniar)
library(sandwich)
library(lmtest)
library(plm)
library(fixest)
library(stargazer)

###---- Data --------------------------------------------------------------------

df_complete <- read.csv("EPR2NBP_2025-csv.csv")%>%
  mutate(across(starts_with(c("anydown", "anyupgrade")), as.factor)) %>%
  rename_with(~ paste0("nbp_",.), starts_with(c("anydown", "anyupgrade"))) %>%
  mutate(across(starts_with(c("downgraded", "upgraded")), as.factor)) %>%
  rename_with(~ paste0("epr_",.), starts_with(c("downgraded", "upgraded"))) %>%
  rename(iso3c = iso3) %>%
  mutate(RacialGpPast = ifelse(RacialGpPast == 99, NA, RacialGpPast)) %>%
  distinct()

# Examine the upgrade / downgrade variable

summary(df_complete$nbp_anydown_1)
summary(df_complete$nbp_anydown_5)
summary(df_complete$nbp_anydown_10)

summary(df_complete$nbp_anyupgrade_1)
summary(df_complete$nbp_anyupgrade_5)
summary(df_complete$nbp_anyupgrade_10)

df_complete <- df_complete %>%
  mutate(across(starts_with("nbp_"), ~ as.numeric(as.character(.x))
  ))

# Manually check changes for one country to verify first year of country-group isn't counted as a downgrade (optional); I revised the first year rule in the original upgrade/downgrade code so we can skip this

df_cambodia <- df_complete %>%
  filter(Country == "Cambodia") %>%
  select(c(Country, Year, CountryDateStart, CountryDateEnd, Group, starts_with("nbp_")))
view(df_cambodia)

# Regional markers:
# Use the {countrycode} package to assign countries to regions. 
# Some cases (e.g., Yugoslavia, GDR) may require manual adjustment, but it should suffice for EDA.
# Can revisit and refine if analyses depend on the 'region' variable.

df_cc <- countrycode::codelist %>%
  select(c(country.name.en, iso3c, un.region.name, un.regionintermediate.name, un.regionsub.name))

summary(as.factor(df_cc$un.regionintermediate.name))
summary(as.factor(df_cc$un.regionsub.name))

df_cc_NA <- df_cc %>%
  filter(is.na(iso3c))

df_complete <- df_complete %>%
  left_join(df_cc, by = "iso3c")

df_complete_NA <- df_complete %>%
  filter(is.na(un.region.name)) %>%
  distinct(Country)

summary(as.factor(df_complete$un.region.name))
summary(as.factor(df_complete$un.regionsub.name))

# Manually add UN region and subregion for cases not covered by {countrycode}:
# Czechoslovakia, GDR, Kosovo, People's Democratic Republic of Yemen, South Vietnam, Taiwan, USSR, Yemen, and Yugoslavia.

df_complete <- df_complete %>%
  mutate(
    un.region.name = case_when(
      iso3c == "CSK" ~ "Europe",
      iso3c == "DDR"  ~ "Europe",
      iso3c == "XKX" ~ "Europe",
      iso3c == "YPR" ~ "Asia",
      iso3c == "YAR" ~ "Asia",
      iso3c == "VDR" ~ "Asia",
      iso3c == "TWN" ~ "Asia",
      iso3c == "SUN" ~ "Europe",
      iso3c == "YUG" ~ "Europe",
      TRUE ~ un.region.name
    ),
    un.regionsub.name = case_when(
      iso3c == "CSK" ~ "Eastern Europe",
      iso3c == "DDR"  ~ "Western Europe",
      iso3c == "XKX" ~ "Southern Europe",
      iso3c == "YPR" ~ "Western Asia",
      iso3c == "YAR" ~ "Western Asia",
      iso3c == "VDR" ~ "South-eastern Asia",
      iso3c == "TWN" ~ "Eastern Asia",
      iso3c == "SUN" ~ "Eastern Europe",
      iso3c == "YUG" ~ "Southern Europe",
      TRUE ~ un.regionsub.name  
    )
  )

summary(as.factor(df_complete$un.region.name))
summary(as.factor(df_complete$un.regionsub.name))

# Read additional data:
# BOP State History Data on Monolingual Education Variable.

df_bop_state <- read_excel("Bop_extended_state_history.xlsx", sheet = 2, skip = 1) %>%
  select(c(wbcode, wbname, statehiste00)) %>%
  rename(iso3c = wbcode)

df_bop_state <- df_bop_state %>%
  drop_na()

# Polity 5 for level of democracy

df_polity5 <- read_excel("POLITY5-PRC.xlsx", sheet = 1) %>%
  rename(iso3c = `Economy ISO3`,
         Country = `Economy Name`) %>%
  filter(`Indicator ID` == "POLITY5.PRC.polity2") %>%
  select(-Indicator)

df_polity5 <- df_polity5 %>%
  pivot_longer(
    cols = `1776`:`2020`,          
    names_to = "Year",            
    values_to = "Polity2"
  ) %>%
  select(-c(`Indicator ID`, starts_with("Attribute"), Partner))

df_polity5 <- df_polity5%>%
  mutate(Year = as.numeric(as.character(Year)))

df_polity5 <- df_polity5 %>%
  select(c(iso3c, Year, Polity2))

# EPR GrowUp Data

df_epr_gu <- read.csv("epr_growup_data.csv")

# Penn World Table: GDP and Population DATA

df_pwt <- read_excel("pwt1001.xlsx", sheet = 3) %>%
  select(c(countrycode, year, pop, rgdpe, rgdpo, rgdpna)) %>%
  rename(iso3c = countrycode,
         Year = year)

# Conflict Dyad Data

df_ethnicdyads <- openxlsx::read.xlsx("dyads_intensity_groups.xlsx") %>%
  filter(!is.na(group))

###---- DOWNGRADE AND CONFLICT -------------------------------------------------

##---- Inspect the data  -------------------------------------------------------

summary(df_complete)

#NBP Educational Status Changes: Upgrades and Downgrades

summary(df_complete$nbp_anydown_1)
str(df_complete$nbp_anydown_1)
class(df_complete$nbp_anydown_1)

summary(df_complete$nbp_anydown_5)
str(df_complete$nbp_anydown_5)
class(df_complete$nbp_anydown_5)

summary(df_complete$nbp_anydown_10)
str(df_complete$nbp_anydown_10)
class(df_complete$nbp_anydown_10)

summary(df_complete$nbp_anyupgrade_1)
str(df_complete$nbp_anyupgrade_1)
class(df_complete$nbp_anyupgrade_1)

summary(df_complete$nbp_anyupgrade_5)
str(df_complete$nbp_anyupgrade_5)
class(df_complete$nbp_anyupgrade_5)

summary(df_complete$nbp_anyupgrade_10)
str(df_complete$nbp_anyupgrade_10)
class(df_complete$nbp_anyupgrade_10)

#EPR Political Status Changes: Upgrades and Downgrades

summary(df_complete$epr_downgraded1)
str(df_complete$epr_downgraded1)
class(df_complete$epr_downgraded1)

summary(df_complete$epr_downgraded5)
str(df_complete$epr_downgraded5)
class(df_complete$epr_downgraded5)

summary(df_complete$epr_downgraded10)
str(df_complete$epr_downgraded10)
class(df_complete$epr_downgraded10)

summary(df_complete$epr_upgraded1)
str(df_complete$epr_upgraded1)
class(df_complete$epr_upgraded1)

summary(df_complete$epr_upgraded5)
str(df_complete$epr_upgraded5)
class(df_complete$epr_upgraded5)

summary(df_complete$epr_upgraded10)
str(df_complete$epr_upgraded10)
class(df_complete$epr_upgraded10)

#---- Compare NBP upgrade / downgrade with EPR upgrade / downgrade ---------------

# Contingency Tables

contingency_table_1y_down <- table(df_complete$nbp_anydown_1, df_complete$epr_downgraded1)
print(contingency_table_1y_down)
contingency_table_5y_down <- table(df_complete$nbp_anydown_5, df_complete$epr_downgraded5)
print(contingency_table_5y_down)
contingency_table_10y_down <- table(df_complete$nbp_anydown_10, df_complete$epr_downgraded10)
print(contingency_table_10y_down)

contingency_table_1y_up <- table(df_complete$nbp_anyupgrade_1, df_complete$epr_upgraded1)
print(contingency_table_1y_up)
contingency_table_5y_up <- table(df_complete$nbp_anyupgrade_5, df_complete$epr_upgraded5)
print(contingency_table_5y_up)
contingency_table_10y_up <- table(df_complete$nbp_anyupgrade_10, df_complete$epr_upgraded10)
print(contingency_table_10y_up)

# Phi coefficient

contingency_tables <- list(
  table_1y_down = contingency_table_1y_down,
  table_5y_down = contingency_table_5y_down,
  table_10y_down = contingency_table_10y_down,
  table_1y_up = contingency_table_1y_up,
  table_5y_up = contingency_table_5y_up,
  table_10y_up = contingency_table_10y_up
)

# loop through the tables 
phi_coeffs <- list()

for (table_name in names(contingency_tables)) {
  phi_coeffs[[table_name]] <- phi(contingency_tables[[table_name]])
}

for (table_name in names(phi_coeffs)) {
  print(paste("Phi coef. for", table_name, ":", round(phi_coeffs[[table_name]], 3)))
}

# No strong association observed, but proceeding with visualizations.

# ---- Visualizations -----------------------------------------------------------

# Visualizing the association (difference) between EPR and NBP upgrades/downgrades.

# The code is repetitive—many lines with copy-paste structure.
# Steps are kept transparent; consider skipping to outputs and checking code only if needed.

# DOWNGRADES

# Heatmap of counts.

heatmap_data_5y_down <- melt(contingency_table_5y_down)

ggplot(heatmap_data_5y_down, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "nbp_anydown_5", y = "epr_downgraded5", fill = "Count") +
  theme_clean()

ggsave("hm1_down_5y.png", width = 8, height = 6, dpi = 300)

# Bar Charts: NBP Downgrades vs. EPR Downgrades

df_complete$combination_5y_down <- with(df_complete, paste(nbp_anydown_5, epr_downgraded5, sep = "-"))
joint_freq <- as.data.frame(table(df_complete$combination_5y_down))

ggplot(joint_freq, aes(Var1, Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Combination (nbp_anydown_5 - epr_downgraded5)", y = "Frequency") +
  theme_clean()

ggsave("bpjf_down_5y.png", width = 8, height = 6, dpi = 300)

# UPGRADES 

# Heat map of counts: NBP Upgrades vs. EPR Upgrades

heatmap_data_5y_up <- melt(contingency_table_5y_up)

ggplot(heatmap_data_5y_up, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "nbp_anyupgrade_5", y = "epr_upgraded5", fill = "Count") +
  theme_clean()

ggsave("hm2_up_5y.png", width = 8, height = 6, dpi = 300)

# Bar charts: NBP Upgrades vs. EPR Upgrades

df_complete$combination_5y_up <- with(df_complete, paste(nbp_anyupgrade_5, epr_upgraded5, sep = "-"))
joint_freq <- as.data.frame(table(df_complete$combination_5y_up))

ggplot(joint_freq, aes(Var1, Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Combination (nbp_anyupgrade_5 - epr_upgraded5)", y = "Frequency") +
  theme_clean()

ggsave("bpjf_up_5y.png", width = 8, height = 6, dpi = 300)

# VISUALISATION OF TRENDS OVER TIME

# UPGRADES

# Number of upgrades compared to the following year over time - waves of language recognition?

df_hist_nbp <- df_complete %>%
  group_by(Year) %>%
  summarise(nbp_changes = sum(as.numeric(as.character(nbp_anyupgrade_1)), na.rm = TRUE)
            )

df_hist_epr <- df_complete %>%
  filter(!is.na(epr_upgraded1)) %>%
  group_by(Year) %>%
  summarise(epr_changes = sum(as.numeric(as.character(epr_upgraded1)), na.rm = TRUE)
            )

df_hist <- df_hist_nbp %>%
  left_join(df_hist_epr, by = "Year")

df_hist_long <- df_hist %>%
  pivot_longer(
    cols = c(epr_changes, nbp_changes),
    names_to = "type",
    values_to = "count"
  )

ggplot(df_hist_long, aes(x = Year, y = count, fill = type)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +  
  scale_fill_manual(
    values = c("epr_changes" = "red", "nbp_changes" = "blue"),
    labels = c("EPR Changes", "NBP Changes")
  ) +
  labs(
    title = "EPR and NBP upgrades compared to previous year",
    x = "Year",
    y = "Number of Changes",
    fill = "Upgrade Type"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "black"),
    legend.position = "bottom"
  )

ggsave("hist1_up_1y.png", width = 8, height = 6, dpi = 300)

# DOWNGRADES

# number downgrades compared to the previous year over time

df_hist_nbp_2 <- df_complete %>%
  group_by(Year) %>%
  summarise(nbp_changes = sum(as.numeric(as.character(nbp_anydown_1)), na.rm = TRUE)
  )

df_hist_epr_2 <- df_complete %>%
  filter(!is.na(epr_downgraded1)) %>%
  group_by(Year) %>%
  summarise(epr_changes = sum(as.numeric(as.character(epr_downgraded1)), na.rm = TRUE)
  )

df_hist_2 <- df_hist_nbp_2 %>%
  left_join(df_hist_epr_2, by = "Year")

df_hist_long_2 <- df_hist_2 %>%
  pivot_longer(
    cols = c(epr_changes, nbp_changes),
    names_to = "type",
    values_to = "count"
  )

ggplot(df_hist_long_2, aes(x = Year, y = count, fill = type)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +  
  scale_fill_manual(
    values = c("epr_changes" = "red", "nbp_changes" = "blue"),
    labels = c("EPR Changes", "NBP Changes")
  ) +
  labs(
    title = "EPR and NBP downgrades compared to previous year",
    x = "Year",
    y = "Number of Changes",
    fill = "Downgrade Type"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "black"),
    legend.position = "bottom"
  )

ggsave("hist2_down_1y.png", width = 8, height = 6, dpi = 300)

# ---- DISAGGREGATION BY REGION -------------------------------------------------

# Including plots showing the absolute number of group-level changes per year.
# However, for regional analysis, shifting to the country level might be preferable 
# (tracking where countries changed language policies) to avoid double counting.

# Awaiting response to 11 December 2024 email.
# Note: Graphs displaying absolute changes are sensitive to highly diverse countries, 
# especially those with many groups sharing few languages.

# Upgrades compared to the previous year, disaggregated by region (now using `un.region.name` only).


df_hist_nbp_3 <- df_complete %>%
  group_by(Year, un.region.name) %>%
  summarise(
    nbp_changes = sum(as.numeric(as.character(nbp_anyupgrade_1)), na.rm = TRUE),
    .groups = "drop"
  )

df_hist_epr_3 <- df_complete %>%
  filter(!is.na(epr_upgraded1)) %>%
  group_by(Year, un.region.name) %>%
  summarise(
    epr_changes = sum(as.numeric(as.character(epr_upgraded1)), na.rm = TRUE),
    .groups = "drop"
  )

df_hist_3 <- df_hist_nbp_3 %>%
  left_join(df_hist_epr_3, by = c("Year", "un.region.name")) %>%
  filter(!is.na(un.region.name))

ggplot(df_hist_3, aes(x = Year)) +
  geom_line(aes(y = nbp_changes, color = "NBP Changes"), size = 1) +
  geom_line(aes(y = epr_changes, color = "EPR Changes"), size = 0.5, linetype = "dashed") +
  scale_y_continuous(
    name = "NBP Changes",
    sec.axis = sec_axis(~ ., name = "EPR Changes")
  ) +
  scale_color_manual(
    values = c("NBP Changes" = "blue", "EPR Changes" = "red"),
    labels = c("EPR Changes", "NBP Changes")
  ) +
  facet_wrap(~ un.region.name) +  
  labs(
    title = "EPR and NBP Upgrades Compared to Previous Year by Region",
    x = "Year",
    color = "Change Type"
  ) +
  theme_clean() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    legend.position = "bottom"
  )

ggsave("l1_up_byregion_1y.png", width = 8, height = 6, dpi = 300)

# Downgrades compared to the previous year, disaggregated by region (`un.region.name`).

df_hist_nbp_4 <- df_complete %>%
  group_by(Year, un.region.name) %>%
  summarise(
    nbp_changes = sum(as.numeric(as.character(nbp_anydown_1)), na.rm = TRUE),
    .groups = "drop"
  )

df_hist_epr_4 <- df_complete %>%
  filter(!is.na(epr_downgraded1)) %>%
  group_by(Year, un.region.name) %>%
  summarise(
    epr_changes = sum(as.numeric(as.character(epr_downgraded1)), na.rm = TRUE),
    .groups = "drop"
  )

df_hist_4 <- df_hist_nbp_4 %>%
  left_join(df_hist_epr_4, by = c("Year", "un.region.name")) %>%
  filter(!is.na(un.region.name))

ggplot(df_hist_4, aes(x = Year)) +
  geom_line(aes(y = nbp_changes, color = "NBP Changes"), size = 1) +
  geom_line(aes(y = epr_changes, color = "EPR Changes"), size = 0.5, linetype = "dashed") +
  scale_y_continuous(
    name = "NBP Changes",
    sec.axis = sec_axis(~ ., name = "EPR Changes")
  ) +
  scale_color_manual(
    values = c("NBP Changes" = "blue", "EPR Changes" = "red"),
    labels = c("EPR Changes", "NBP Changes")
  ) +
  facet_wrap(~ un.region.name) +  # Facet by region
  labs(
    title = "EPR and NBP Downgrades Compared to Previous Year by Region",
    x = "Year",
    color = "Change Type"
  ) +
  theme_clean() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    legend.position = "bottom"
  )

ggsave("l2_down_byregion_1y.png", width = 8, height = 6, dpi = 300)

# Regarding regional overrepresentation:
# Upgrades and downgrades are coded at the group level, making counts sensitive to countries with many groups.
# Policies affecting multiple groups can inflate counts, likely driven by China and the USSR in Europe and Asia.

# First examine the USSR

df_ussr <- df_complete %>%
  filter(iso3c == "SUN")

df_ussr_hist_npb <- df_ussr %>%
  group_by(Year) %>%
  summarise(nbp_changes = sum(as.numeric(as.character(nbp_anydown_1)), na.rm = TRUE),
            .groups = "drop"
  )

ggplot(df_ussr_hist_npb, aes(x = Year)) +
  geom_line(aes(y = nbp_changes), size = 1) +
  theme_clean()

# Examine China

df_china <- df_complete %>%
  filter(iso3c == "CHN")

df_china_hist_nbp <- df_china %>%
  group_by(Year) %>%
  summarise(nbp_changes = sum(as.numeric(as.character(nbp_anydown_1)), na.rm = TRUE),
            .groups = "drop"
  )

ggplot(df_china_hist_nbp, aes(x = Year)) +
  geom_line(aes(y = nbp_changes), size = 1) +
  theme_clean()

# Excluding the USSR and China as outliers

df_hist_nbp_5_without_chinaussr <- df_complete %>%
  filter(!iso3c %in% c("SUN", "CHN")) %>%
  group_by(Year, un.region.name) %>%
  summarise(
    nbp_changes = sum(as.numeric(as.character(nbp_anydown_1)), na.rm = TRUE),
    .groups = "drop"
  )

df_hist_epr_5_without_chinaussr <- df_complete %>%
  filter(!iso3c %in% c("SUN", "CHN")) %>%
  filter(!is.na(epr_downgraded1)) %>%
  group_by(Year, un.region.name) %>%
  summarise(
    epr_changes = sum(as.numeric(as.character(epr_downgraded1)), na.rm = TRUE),
    .groups = "drop"
  )

df_hist_5_without_chinaussr <- df_hist_nbp_5_without_chinaussr %>%
  left_join(df_hist_epr_5_without_chinaussr, by = c("Year", "un.region.name")) %>%
  filter(!is.na(un.region.name))

ggplot(df_hist_5_without_chinaussr, aes(x = Year)) +
  geom_line(aes(y = nbp_changes, color = "NBP Changes"), size = 1) +
  geom_line(aes(y = epr_changes, color = "EPR Changes"), size = 0.5, linetype = "dashed") +
  scale_y_continuous(
    name = "NBP Changes",
    sec.axis = sec_axis(~ ., name = "EPR Changes")
  ) +
  scale_color_manual(
    values = c("NBP Changes" = "blue", "EPR Changes" = "red"),
    labels = c("EPR Changes", "NBP Changes")
  ) +
  facet_wrap(~ un.region.name) +  # Facet by region
  labs(
    title = "EPR and NBP Downgrades Compared to Previous Year by Region",
    x = "Year",
    color = "Change Type"
  ) +
  theme_clean() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    legend.position = "bottom"
  )

ggsave("l3_down_byregion_1y_without_china_ussr.png", width = 8, height = 6, dpi = 300)


## QUESTION WHETHER VARIABLES TO BE RECODED - CHANGES OF STATUS IN LANGAUGE AT COUNTRY LEVEL?

## DIFFERENT UPGRADE / DOWNGRADE VARIABLES BY REGION -- do the above-illustrated trends vary for different forms of upgrade/ downgrade?

# Upgrades 

df_region_summary <- df_complete %>%
  group_by(un.region.name) %>%
  summarise(
    total_LOI_upgrades = sum(as.numeric(as.character(upgradeLOI_1)), na.rm = TRUE),
    total_LC_upgrades = sum(as.numeric(as.character(upgradeLC_1)), na.rm = TRUE),
    .groups = "drop"
  )

df_region_long <- df_region_summary %>%
  pivot_longer(
    cols = c(total_LOI_upgrades, total_LC_upgrades),
    names_to = "upgrade_type",
    values_to = "count"
  )

ggplot(df_region_long, aes(x = un.region.name, y = count, fill = upgrade_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("total_LOI_upgrades" = "grey", "total_LC_upgrades" = "black"),
    labels = c("LOI Upgrades", "LC Upgrades", "NBP Upgrades")
  ) +
  labs(
    title = "Regional Differences in Upgrade Types",
    x = "Region",
    y = "Total Number of Upgrades",
    fill = "Upgrade Type"
  ) +
  theme_clean() +
  theme(
    axis.title.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave("b1_upgrLOILC_byregion.png", width = 8, height = 6, dpi = 300)

# Downgrades

df_region_summary_2 <- df_complete %>%
  group_by(un.region.name) %>%
  summarise(
    total_LOI_downgrades = sum(as.numeric(as.character(downLOI_1)), na.rm = TRUE),
    total_LC_downgrades = sum(as.numeric(as.character(downLC_1)), na.rm = TRUE),
    .groups = "drop"
  )

df_region_long_2 <- df_region_summary_2 %>%
  pivot_longer(
    cols = c(total_LOI_downgrades, total_LC_downgrades),
    names_to = "downgrade_type",
    values_to = "count"
  )

ggplot(df_region_long_2, aes(x = un.region.name, y = count, fill = downgrade_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("total_LOI_downgrades" = "grey", "total_LC_downgrades" = "black"),
    labels = c("LOI Downgrades", "LC Downgrades")
  ) +
  labs(
    title = "Regional Differences in Downgrade Types",
    x = "Region",
    y = "Total Number of Upgrades",
    fill = "Upgrade Type"
  ) +
  theme_clean() +
  theme(
    axis.title.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave("b2_downgrLOILC_byregion.png", width = 8, height = 6, dpi = 300)

## -- Number of languages used as LOI in a given year.
## -- Similar approach can help identify double counting at the country level.

## -- ON HOLD: Focus remains on the group level.

df_LOI <- df_complete %>%
  pivot_longer(
    cols = c(Lang1, Lang2, Lang3),
    names_to = "LangVariable",
    values_to = "Language"
  ) %>%
  pivot_longer(
    cols = c(LOIPrimary1, LOIPrimary2, LOIPrimary3, LOISecondary1, LOISecondary2, LOISecondary3),
    names_to = "LOIVariable",
    values_to = "LOIStatus"
  ) %>%
  filter(LOIStatus %in% c(1,2)) %>%
  group_by(Country, Year, un.region.name) %>%
  summarize(
    UniqueLOILanguages = n_distinct(Language, na.rm = TRUE),
    .groups = "drop"
  )

df_LOI_average <- df_LOI %>%
  group_by(un.region.name, Year) %>%
  summarize(
    AvgUniqueLOILanguages = mean(UniqueLOILanguages, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(un.region.name))

ggplot(df_LOI_average, aes(x = Year, y = AvgUniqueLOILanguages, color = un.region.name)) +
  geom_line(size = 1) +
  labs(
    title = "Average Number of Languages of Instruction Over Time",
    x = "Year",
    y = "Average Unique LOI Languages",
    color = "Region"
  ) +
  theme_clean() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# ---- COUNTRY-LEVEL CHANGES ---------------------------------------------------

## UPGRADE AT THE COUNTRY LEVEL
# Country-level LOI upgrades are listed below but require review.

## ON HOLD: Focus remains on the group level (see MONOLINGUAL variable below).

df_complete_LOIupgrade <- df_complete %>%
  left_join(df_LOI, by = c("Country", "Year"))

df_complete_LOIupgrade_flagged <- df_complete_LOIupgrade %>%
  group_by(Country) %>%
  arrange(Year) %>%
  mutate(LOI_anyupgrade_1 = as.factor(if_else(
    UniqueLOILanguages > lag(UniqueLOILanguages, default = NA), 1, 0))) %>%
  ungroup()

summary(df_complete_LOIupgrade_flagged$LOI_anyupgrade_1)

# where were the upgrades?
upgrades_per_region <- df_complete_LOIupgrade_flagged %>%
  filter(!is.na(LOI_anyupgrade_1)) %>%
  group_by(un.region.name.x) %>% 
  summarize(
    TotalUpgrades = sum(as.numeric(as.character(LOI_anyupgrade_1, na.rm = TRUE))), 
    .groups = "drop"
  ) 

print(upgrades_per_region)

ggplot(upgrades_per_region, aes(x = un.region.name.x, y = TotalUpgrades, fill = un.region.name.x)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Number of LOI Upgrades per Region",
    x = "Region",
    y = "Total Upgrades"
  ) +
  scale_fill_grey(name = "Region", start = 0.8, end = 0.2) +
  theme_clean() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

## ---- CORRELATION TESTS -------------------------------------------------------

# GROUP-LEVEL VARIABLES

# Group characteristics and language status upgrades/downgrades.
# Baseline language status is not accounted for here—should we test whether, for instance, indigenous group languages are generally more/less likely to be used as LOIs?

group_characteristics <- c("MigrantBackground", "RacialGpStudyPeriod", "RacialGpPast", "IndigGp", "SpatialConc")

#variable summaries
for (var in group_characteristics) {
  cat("\nSummary for:", var, "\n")
  
  print(summary(df_complete[[var]]))
}

#contingency tables
for (var in group_characteristics) {
  cat("\nContingency Table for:", var, "\n")
  
  valid_data <- df_complete %>%
    filter(!is.na(nbp_anyupgrade_1) & !is.na(.data[[var]]))
  
  table_result <- table(valid_data$nbp_anyupgrade_1, valid_data[[var]])
  
  print(table_result)
}

#Phi coefficients for 2x2 tables
for (var in group_characteristics) {
  cat("\nContingency Table for:", var, "\n")
  valid_data <- df_complete %>%
    filter(!is.na(nbp_anyupgrade_1) & !is.na(.data[[var]]))
  table_result <- table(valid_data$nbp_anyupgrade_1, valid_data[[var]])
  print(table_result)
  if (nrow(table_result) == 2 && ncol(table_result) == 2) {
    phi_value <- psych::phi(table_result)
    cat("\nPhi coefficient:", phi_value, "\n")
  } else {
    cat("\nPhi coefficient NA.\n")
  }
}

# Group-level characteristics and language recognition.

# Above: Group characteristics and language status changes.
# Now: Baseline association with recognized languages.

# Whether any group language was used in schools serves as a reasonable approximation.
# First, include core groups, then exclude them.

summary(as.factor(df_complete$AnyLangUsedInSchool))

df_complete <- df_complete %>%
  mutate(AnyLangUsedInSchool = ifelse(AnyLangUsedInSchool == 100, NA, AnyLangUsedInSchool))

#as above: contingency tables
for (var in group_characteristics) {
  cat("\nContingency Table for:", var, "\n")
  
  valid_data <- df_complete %>%
    filter(!is.na(AnyLangUsedInSchool) & !is.na(.data[[var]]))
  
  table_result <- table(valid_data$AnyLangUsedInSchool, valid_data[[var]])
  
  print(table_result)
}

#Phi coefficients
for (var in group_characteristics) {
  cat("\nContingency Table for:", var, "\n")
  valid_data <- df_complete %>%
    filter(!is.na(AnyLangUsedInSchool) & !is.na(.data[[var]]))
  table_result <- table(valid_data$AnyLangUsedInSchool, valid_data[[var]])
  print(table_result)
  if (nrow(table_result) == 2 && ncol(table_result) == 2) {
    phi_value <- psych::phi(table_result)
    cat("\nPhi coefficient:", phi_value, "\n")
  } else {
    cat("\nPhi coefficient NA.\n")
  }
}

# without core groups 
df_complete_noCGp <- df_complete %>%
  filter(CoreGp == 0)

# re-run tests
for (var in group_characteristics) {
  cat("\nContingency Table for:", var, "\n")
  
  valid_data <- df_complete_noCGp %>%
    filter(!is.na(AnyLangUsedInSchool) & !is.na(.data[[var]]))
  
  table_result <- table(valid_data$AnyLangUsedInSchool, valid_data[[var]])
  
  print(table_result)
}

#Phi coefficients
for (var in group_characteristics) {
  cat("\nContingency Table for:", var, "\n")
  valid_data <- df_complete_noCGp %>%
    filter(!is.na(AnyLangUsedInSchool) & !is.na(.data[[var]]))
  table_result <- table(valid_data$AnyLangUsedInSchool, valid_data[[var]])
  print(table_result)
  if (nrow(table_result) == 2 && ncol(table_result) == 2) {
    phi_value <- psych::phi(table_result)
    cat("\nPhi coefficient:", phi_value, "\n")
  } else {
    cat("\nPhi coefficient NA.\n")
  }
}

# Stricter measure: Whether L1–L3 are used as LOI among non-core groups.

df_complete_noCGp <- df_complete_noCGp %>%
  mutate(any_LangOI = ifelse(LOIPrimary1 %in% c(1,2) | LOIPrimary2 %in% c(1,2) | LOIPrimary3 %in% c(1,2), 1, 0))

summary(as.factor(df_complete_noCGp$any_LangOI))

for (var in group_characteristics) {
  cat("\nContingency Table for:", var, "\n")
  
  valid_data <- df_complete_noCGp %>%
    filter(!is.na(any_LangOI) & !is.na(.data[[var]]))
  
  table_result <- table(valid_data$any_LangOI, valid_data[[var]])
  
  print(table_result)
}

#Phi coefficients
for (var in group_characteristics) {
  cat("\nContingency Table for:", var, "\n")
  valid_data <- df_complete_noCGp %>%
    filter(!is.na(any_LangOI) & !is.na(.data[[var]]))
  table_result <- table(valid_data$any_LangOI, valid_data[[var]])
  print(table_result)
  if (nrow(table_result) == 2 && ncol(table_result) == 2) {
    phi_value <- psych::phi(table_result)
    cat("\nPhi coefficient:", phi_value, "\n")
  } else {
    cat("\nPhi coefficient NA.\n")
  }
}

# At an aggregate level, no clear association between these characteristics 
# and language status upgrades or recognition.

# Self-Determination

# Onset of SDM in one year and `nbp_anyupgrade_1` in the following year.

df_complete_sdmonset <- df_complete %>%
  mutate(group_country = paste0(Group, "_", Country))

df_complete_sdmonset <- df_complete_sdmonset %>%
  group_by(group_country) %>%
  arrange(iso3c, Group, Year) %>%
  mutate(
    SDM_lag = lag(SDM, n = 1),
    nbp_anydown_1_lag = lag(nbp_anydown_1, n = 1)
  ) %>%
  ungroup()

df_lagged <- df_complete_sdmonset %>%
  filter(!is.na(SDM_lag))

ct_sdmonset_upgrade <- table(df_lagged$nbp_anyupgrade_1, df_lagged$SDM_lag)
print(ct_sdmonset_upgrade)

chisq_test_up <- chisq.test(ct_sdmonset_upgrade)
print(chisq_test_up)

## There is an issue in this line of code starting at 951

mosaic(ct_sdmonset_upgrade,
       shade = TRUE,       
       legend = TRUE,      
       main = "Mosaic Plot: SDM Onset vs. NBP Upgrade",
       xlab = "SDM Lag (Onset)",
       ylab = "NBP Any Upgrade")

df_lagged$nbp_anyupgrade_1 <- as.numeric(as.character(df_lagged$nbp_anyupgrade_1))
df_lagged$SDM_lag <- as.numeric(as.character(df_lagged$SDM_lag))

df_lagged <- df_lagged %>%
  filter(!is.na(nbp_anyupgrade_1))

m1 <- glm(nbp_anyupgrade_1 ~ + SDM_lag, data = df_lagged)

summary(m1)

# Onset of SDM in one year and nbp_anydown_1 in the following (Emre: lags don't make sense here, you should be lagging the independent variable; we expect that a downgrade with trigger an SDM movement)

ct_sdmonset_downgrade <- table(df_lagged$nbp_anydown_1_lag, df_lagged$SDM)
print(ct_sdmonset_downgrade)

chisq_test <- chisq.test(ct_sdmonset_downgrade)
print(chisq_test)

# End of SDM --- there is an issue here (!)

df_complete_sdmend <- df_complete_sdmonset %>%
  group_by(group_country) %>%  
  arrange(Year) %>%  
  mutate(SDM_end = ifelse(SDM == 0 & lag(SDM, n = 1) == 1, 1, 0)) %>%
  ungroup()

df_lagged_2 <- df_complete_sdmend %>%
  filter(!is.na(SDM_end) & !is.na(nbp_anyupgrade_1))

ct_sdmend_upgrade <- table(df_lagged_2$nbp_anyupgrade_1, df_lagged_2$SDM_end)
print(ct_sdmend_upgrade)

# Issue here as well below starting in 992

fisher.test(ct_sdmend_upgrade) 

## ---- ASSOCIATION: LANGUAGE DOWNGRADE AND VIOLENCE -----------------------------

# Is language and political downgrade associated with higher ethnic/territorial conflict?

downgrade_vars <- c("nbp_anydown_1", "nbp_anydown_5", "nbp_anydown_10", "epr_downgraded1", "epr_downgraded5", "epr_downgraded10")

# ONSET

conflict_vars <- c("onset_ko_flag", "onset_ko_terr_flag", "onset_ko_gov_flag")

# Chi-Square tests - loop through variables defined above and run Chi-Square tests
for (dep_var in conflict_vars) {
  for (ind_var in downgrade_vars) {
    cat("\nTabulation for:", dep_var, "vs", ind_var, "\n")
    
    table_result <- table(df_complete[[dep_var]], df_complete[[ind_var]])
    
    print(table_result)
    
    chi2_test <- chisq.test(table_result)
    
    print(chi2_test)
  }
}

# Fisher's exact tests (two-sided), same as above, only different test
for (dep_var in conflict_vars) {
  for (ind_var in downgrade_vars) {
    cat("\nTabulation for:", dep_var, "vs", ind_var, "\n")
    
    table_result <- table(df_complete[[dep_var]], df_complete[[ind_var]])
    
    print(table_result)
    
    fisher_test <- fisher.test(table_result)
    
    print(fisher_test)
  }
}

# Fisher's exact tests (one-sided), same as above, but one-sided tests, expecting downgrade to lead to conlict

for (dep_var in conflict_vars) {
  for (ind_var in downgrade_vars) {
    cat("\nTabulation for:", dep_var, "vs", ind_var, "\n")
    
    table_result <- table(df_complete[[dep_var]], df_complete[[ind_var]])
    
    print(table_result)
    
    fisher_test <- fisher.test(table_result, alternative = "greater")
    
    print(fisher_test)
  }
}

# INCIDENCE

conflict_incidence_vars <- c("incidence_flag", "incidence_terr_flag", "incidence_gov_flag")

# Chi-Square tests - loop through incidence vars instead; does not seem necessary to also run Fisher's exact tests

for (dep_var in conflict_incidence_vars) {
  for (ind_var in downgrade_vars) {
    cat("\nTabulation for:", dep_var, "vs", ind_var, "\n")
    
    table_result <- table(df_complete[[dep_var]], df_complete[[ind_var]])
    
    print(table_result)
    
    chi2_test <- chisq.test(table_result)
    
    print(chi2_test)
  }
}

# ALTERNATIVE ONSET

conflict_do_vars <- c("onset_do_flag", "onset_do_terr_flag", "onset_do_gov_flag")

# Chi-Square tests
for (dep_var in conflict_do_vars) {
  for (ind_var in downgrade_vars) {
    cat("\nTabulation for:", dep_var, "vs", ind_var, "\n")
    
    table_result <- table(df_complete[[dep_var]], df_complete[[ind_var]])
    
    print(table_result)
    
    chi2_test <- chisq.test(table_result)
    
    print(chi2_test)
  }
}

# Fisher's exact tests
for (dep_var in conflict_do_vars) {
  for (ind_var in downgrade_vars) {
    cat("\nTabulation for:", dep_var, "vs", ind_var, "\n")
    
    table_result <- table(df_complete[[dep_var]], df_complete[[ind_var]])
    
    print(table_result)
    
    fisher_test <- fisher.test(table_result, alternative = "greater")
    
    print(fisher_test)
  }
}

# VISUALISATION OF ASSOCIATION

# Downgrade and Conflict Onset

ggplot(df_complete) +
  geom_mosaic(aes(x = product(nbp_anydown_5), fill = onset_ko_flag)) +
  labs(title = "Association nbp_anydown_5 and Onset") +
  theme_clean () +
  scale_fill_manual(values = c("0" = "lightgrey", "1" = "darkgrey"))

ggsave("mosaic1_anydown2onset.png", width = 8, height = 6, dpi = 300)

# Downgrade and Conflict Incidence

ggplot(df_complete) +
  geom_mosaic(aes(x = product(nbp_anydown_5), fill = incidence_flag)) +
  labs(title = "Association nbp_anydown_5 and Incidence") +
  theme_clean() +
  scale_fill_manual(values = c("0" = "lightgrey", "1" = "darkgrey"))

ggsave("mosaic2_anydown2incidence.png", width = 8, height = 6, dpi = 300)

### ---- MONOLINGUAL VARIABLE ---------------------------------------------------

## ---- VARIABLE CONSTRUCTION --------------------------------------------------

# Issue with mapping in the "Monolingual" script—using a new approach.
# Regarding dual counting of languages (per 16 Dec 2024 email): 
# When the law mandates one language but multiple are used in practice, report only one.

# Create subset with relevant variables.

df_languages <- df_complete %>%
  select(c(Year, Country, iso3c, Group, 
           starts_with("SubGroup"),
           Lang1, Lang2,Lang3, StandardArabicLang,
           OtherSpokenLang,
           starts_with("AddiLang"),
           starts_with("LOI"),
           starts_with("LC"),
           starts_with("AddiLoI")))%>%
  mutate(
    across(everything(), as.factor)
  )

# create a data frame with unique languages in a given country to identify issues with coding, cf. line 846

df_language_identification <- df_languages %>%
  pivot_longer(
    cols = c(Lang1, Lang2, Lang3, StandardArabicLang, OtherSpokenLang, starts_with("AddiLang")), 
    names_to = "LanguageC",
    values_to = "Language"
  ) %>%
  filter(!is.na(Language) & Language != "") %>%
  distinct(Country, iso3c, Language) %>%
  arrange(Country, Language)

view(df_language_identification)

# Create dummies indicating whether only one national language was used as LOI in a given country-year.

# SOFT monolingual variable: Does not account for LCs.
# Adjusted Monolingual Education script—added variables and revised mapping.

# Previous approach retained but commented out.
# Note: Now based on LOIPrimary; total number of LOIs per country is not available.

# New version of `df_languages_long` with updated pairing logic.

df_languages_long <- df_languages %>%
  pivot_longer(
    cols = c(Lang1, Lang2, Lang3, StandardArabicLang, OtherSpokenLang, AddiLang,
             AddiLangCountry1, AddiLangCountry2, AddiLangCountry3, AddiLangCountry4),
    names_to = "LangVariable",
    values_to = "Language"
  ) %>%
  pivot_longer(
    cols = c(LOIPrimary1, LOIPrimary2, LOIPrimary3, LOIPrimaryStArabic, LOIPrimaryOtherLang, LOIPrimaryAddiLang,
             AddiLOI1, AddiLOI2, AddiLOI3, AddiLOI4),
    names_to = "LOIVariable",
    values_to = "LOIScore"
  ) %>%
  mutate(Match = case_when(
    LangVariable == "Lang1" & LOIVariable == "LOIPrimary1" ~ TRUE,
    LangVariable == "Lang2" & LOIVariable == "LOIPrimary2" ~ TRUE,
    LangVariable == "Lang3" & LOIVariable == "LOIPrimary3" ~ TRUE,
    LangVariable == "StandardArabicLang" & LOIVariable == "LOIPrimaryStArabic" ~ TRUE,
    LangVariable == "OtherSpokenLang" & LOIVariable == "LOIPrimaryOtherLang" ~ TRUE,
    LangVariable == "AddiLang" & LOIVariable == "LOIPrimaryAddiLang" ~ TRUE,
    LangVariable == "AddiLangCountry1" & LOIVariable == "AddiLOI1" ~ TRUE,
    LangVariable == "AddiLangCountry2" & LOIVariable == "AddiLOI2" ~ TRUE,
    LangVariable == "AddiLangCountry3" & LOIVariable == "AddiLOI3" ~ TRUE,
    LangVariable == "AddiLangCountry4" & LOIVariable == "AddiLOI4" ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(Match & !is.na(Language) & !is.na(LOIScore) & Language != "") %>%
  select(-Match) %>%
  mutate(Language = case_when(
    Language == "Northern Pashto" ~ "Pashto",
    Language == "Southern Pashto" ~ "Pashto",
    Language == "Standard Arabic" ~ "Arabic",
    Language %in% c("German", "German, Standard", "German Standard", "Bavarian") ~ "German",
    Language %in% c("Kurdish, Northern", "Kurdish, Southern", "Kurdish, Central") ~ "Kurdish",
    str_detect(Language, "alagasy") ~ "Malagasy",
    TRUE ~ Language  
  ))

df_languages_summarised <- df_languages_long %>%
  group_by(Country, Year, Language) %>%
  summarise(
    TotalNational = sum(LOIScore == 1, na.rm = TRUE),
    TotalLocal = sum(LOIScore == 2, na.rm = TRUE),
    TotalNotTaught = sum(LOIScore == 0, na.rm = TRUE),
    TotalUnknown = sum(LOIScore == 99, na.rm = TRUE),
    TotalFailedState = sum(LOIScore == 100, na.rm = TRUE),
    .groups = "drop"
  )

# monolingual indicator
df_monolingual <- df_languages_summarised %>%
  group_by(Country, Year) %>%
  summarise(
    Monolingual = ifelse(
      sum(TotalNational >= 1) == 1 & 
        all((TotalNational)[TotalNational == 0] == 0),
      1,
      0
    ),
    .groups = "drop"
  )

summary(as.factor(df_monolingual$Monolingual))

openxlsx::write.xlsx(df_monolingual, "nbp_monolingualc_soft_rev_2025.xlsx")

# dummy to complete dataset

class(df_monolingual$Country)
class(df_monolingual$Year)

class(df_complete$Country)
class(df_complete$Year)

df_monolingual <- df_monolingual %>%
  mutate(Country = as.character(Country),
         Year = as.numeric(as.character(Year)))

df_complete <- df_complete %>%
  left_join(df_monolingual, by = c("Country", "Year"))

summary(as.factor(df_complete$Monolingual))

# The above focuses only on LOI—strict version: 
# - 1 national LOI 
# - No regional LOIs 
# - No LCs

# Logic: Among monolingual countries, strictly monolingual ones meet these conditions:
# - All LOI variables are either 1 or NA (excluding countries with regional LOIs).
# - All LC variables are 0 or NA (excluding countries with LCs).

df_monolingual_countries <- df_complete %>%
  filter(Monolingual == 1) %>%
  select(c(Country, Year, Group, 
           starts_with("SubGroup"),
           Lang1, Lang2,Lang3, StandardArabicLang,
           OtherSpokenLang,
           starts_with("AddiLang"),
           starts_with("LOI"),
           starts_with("LC"),
           starts_with("AddiLoI"),
           Monolingual))

summary(df_monolingual_countries)

df_monolingual_strict <- df_monolingual_countries %>%
  mutate(strictML_gr = ifelse(
    apply(select(., starts_with("LOI")), 1, function(x) all(is.na(x) | x %in% c(0, 1))) &
      apply(select(., starts_with("LC")), 1, function(x) all(is.na(x) | x == 0)) &
      apply(select(., starts_with("AddiLOI")), 1, function(x) all(is.na(x) | x %in% c(0, 1))),
    1, 0
  )) %>%
  group_by(Country, Year) %>% 
  summarise(MonolingualStrict = ifelse(all(strictML_gr == 1), 1, 0), .groups = "drop")

summary(as.factor(df_monolingual_strict$MonolingualStrict))

# indicator to main dataset

df_complete <- df_complete %>%
  left_join(df_monolingual_strict, by = c("Country", "Year"))

df_complete <- df_complete %>%
  mutate(MonolingualStrict = ifelse(is.na(MonolingualStrict), 0, MonolingualStrict))

summary(as.factor(df_complete$Monolingual))
summary(as.factor(df_complete$MonolingualStrict))


##---- CORRELATION TESTS -------------------------------------------------------


# MONOLINGUAL EDUCATION - CONFLICT ONSET

# create a variable that reports whether there was any onset on a given country-year

df_complete <- df_complete %>%
  group_by(Year, Country) %>%
  mutate(any_onset = ifelse(any(onset_ko_flag == 1, na.rm = TRUE), 1, 0)) %>%
  ungroup()

summary(as.factor(df_complete$any_onset))

# assoc. between monolingual education and any conflict onset
monoled_conflict_table <- table(df_complete$Monolingual, df_complete$any_onset)
print(monoled_conflict_table)

chisq.test(monoled_conflict_table)

fisher.test(monoled_conflict_table)

# lagg the monolingual education: conflict onset in the following year?
df_complete_ml_lagged <- df_complete %>%
  arrange(Country, Year) %>%  
  group_by(Country) %>%     
  mutate(monoled_lag = lag(Monolingual, n = 1)) %>%
  ungroup()

summary(df_complete_ml_lagged$monoled_lag)

monoled_conflict_table_lag <- table(df_complete_ml_lagged$monoled_lag, df_complete_ml_lagged$any_onset)
print(monoled_conflict_table_lag)

chisq.test(monoled_conflict_table_lag)

fisher.test(monoled_conflict_table_lag)

# ALTERNATIVE SPECIFICATION: STRICTER VERSION OF MONOLINGUAL VARIABLE
# MONOLINGUAL STRICT: 1 national LOI, NO regional LOIs, NO LCs

monoled_conflict_table_strict <- table(df_complete$MonolingualStrict, df_complete$any_onset)
print(monoled_conflict_table_strict)

chisq.test(monoled_conflict_table_strict)

fisher.test(monoled_conflict_table_strict)

# Barplot

df_bp_mlonset <- df_complete %>%
  mutate(Monolingual = as.factor(Monolingual),
         any_onset = as.factor(any_onset))

ggplot(df_bp_mlonset, aes(x = MonolingualStrict, fill = any_onset)) +
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Conflict Onset by MonolingualStrict Status",
    x = "MonolingualStrict (0 = No, 1 = Yes)",
    y = "Proportion of Conflict Onset",
    fill = "Conflict Onset"
  ) +
  theme_minimal()


# MONOLINGUAL EDUCATION, REGIME TYPE, AND STATE HISTORY

# level of democracy

df_complete <- df_complete %>%
  left_join(df_polity5, by = c("iso3c", "Year"))

summary(as.factor(df_complete$Polity2))

wilcox.test(Polity2 ~ Monolingual, data = df_complete)

cor.test(df_complete$Monolingual, df_complete$Polity2, method = "spearman")

ggplot(df_complete, aes(x = factor(Monolingual), y = Polity2)) +
  geom_boxplot() +
  labs(x = "Monolingual", y = "Polity2") +
  theme_clean()

ggplot(df_complete, aes(x = factor(Monolingual), y = Polity2)) +
  geom_violin() +
  labs(x = "Monolingual", y = "Polity2") +
  theme_clean()

# and the strict version

wilcox.test(Polity2 ~ MonolingualStrict, data = df_complete)

cor.test(df_complete$MonolingualStrict, df_complete$Polity2, method = "spearman")

ggplot(df_complete, aes(x = factor(MonolingualStrict), y = Polity2)) +
  geom_violin() +
  labs(x = "Monolingual Strict", y = "Polity2") +
  theme_clean()

# STATE HISTORY MERGE

df_complete <- df_complete %>%
  left_join(df_bop_state, by = "iso3c")

summary(df_complete$statehiste00)

ggplot(df_complete, aes(x = factor(Monolingual), y = statehiste00)) +
  geom_boxplot() +
  labs(x = "Monolingual", y = "State History") +
  theme_clean()

wilcox.test(statehiste00 ~ Monolingual, data = df_complete)

###---- EDUCATIONAL INEQUALITY -------------------------------------------------

##---- Horizontal Inequality Variable ------------------------------------------

df_HI_score <- df_complete %>%
  select(iso3c, Group, Year, Lang1, Lang2, Lang3, StandardArabicLang, AddiLang, 
         LOIPrimary1, LOIPrimary2, LOIPrimary3, LOIPrimaryStArabic, LOIPrimaryAddiLang,
         LOISecondary1, LOISecondary2, LOISecondary3, LOISecondaryStArabic, LOISecondaryAddiLang,
         LCPrimary1, LCPrimary2, LCPrimary3, LCPrimaryStArabic, LCPrimaryAddiLang,
         LCSecondary1, LCSecondary2, LCSecondary3, LCSecondaryStArabic, LCSecondaryAddiLang)

summary(df_HI_score)

loi_cols <- c("LOIPrimary1", "LOIPrimary2", "LOIPrimary3", 
              "LOIPrimaryStArabic", "LOIPrimaryAddiLang",
              "LOISecondary1", "LOISecondary2", "LOISecondary3", 
              "LOISecondaryStArabic", "LOISecondaryAddiLang")

lc_cols <- c("LCPrimary1", "LCPrimary2", "LCPrimary3", 
             "LCPrimaryStArabic", "LCPrimaryAddiLang",
             "LCSecondary1", "LCSecondary2", "LCSecondary3", 
             "LCSecondaryStArabic", "LCSecondaryAddiLang")

df_HI_score <- df_HI_score %>%
  mutate(across(all_of(c(loi_cols, lc_cols)), ~ if_else(.x %in% c(99, 100), 0, .x))) # Treating 99 and 100 as 0

summary(df_HI_score)

# Swap 1 and 2:
# - 2 as national, 1 as local
# - 2 as mandatory, 1 as optional

swap_values <- function(x) ifelse(x == 1, 2, ifelse(x == 2, 1, x))

df_HI_score <- df_HI_score %>%
  mutate(across(all_of(c(loi_cols, lc_cols)), swap_values))

# [copied code now under OLD CODE FRACMENTS below]
# unclarity in HI script: why *2? is that to attach weights? then, perhaps only for LOI?
# the values dont seem to add up, that's why commented out. Suggestion for an easier version: group-level Education score = 2(sum of LOI scores) + (sum of LC scores) / number of languages --- in the HI script, there seems to have been only the LOI sum used. We'd then not use the LC data - so let's work with weighting instead?

df_HI_score <- df_HI_score %>%
  mutate(across(c(Lang1, Lang2, Lang3, StandardArabicLang, AddiLang), ~ na_if(.x, "")))

# Creating a variable for the total number of languages associated with each group

df_HI_score <- df_HI_score %>%
  rowwise() %>%
  mutate(
    num_languages = length(na.omit(c_across(c(Lang1, Lang2, Lang3, StandardArabicLang, AddiLang))))) %>%
  ungroup()

summary(df_HI_score$num_languages)

# LOI educational score as sum

df_HI_score <- df_HI_score %>%
  rowwise() %>%
  mutate(LOIsum = sum(c_across(starts_with("LOI")), na.rm = TRUE) * 2) %>%
  ungroup()

# adjusted by group size

df_HI_score <- df_HI_score %>%
  mutate(LOIsum_adj = LOIsum / num_languages)

summary(df_HI_score$LOIsum)
summary(df_HI_score$LOIsum_adj)

# LC Educational Scores

df_HI_score <- df_HI_score %>%
  rowwise() %>%
  mutate(LCsum = sum(c_across(starts_with("LC")), na.rm = TRUE)) %>%
  ungroup()

df_HI_score <- df_HI_score %>%
  mutate(LCsum_adj = LCsum / num_languages)

summary(df_HI_score$LCsum)
summary(df_HI_score$LCsum_adj)

# Group-level education score: Sum of LOI and LC

df_HI_score <- df_HI_score %>%
  mutate(group_edu_score = LOIsum_adj + LCsum_adj)

summary(df_HI_score$group_edu_score)

# Country-Level Score

df_HI_score <- df_HI_score %>%
  group_by(iso3c, Year) %>%
  mutate(country_edu_score = mean(group_edu_score, na.rm = TRUE)) %>%
  ungroup()

summary(df_HI_score$country_edu_score)

# HI index calculated as |ḡ - G|

df_HI_score <- df_HI_score %>%
  mutate(HI = abs(group_edu_score - (country_edu_score - group_edu_score)))

summary(df_HI_score$HI)

hist(df_HI_score$HI)
hist(log(df_HI_score$HI))

# Join to main dataset

df_HI_clean <- df_HI_score %>%
  select(iso3c, Year, Group, group_edu_score, country_edu_score, HI) %>%
  distinct()

df_complete <- df_complete %>%
  left_join(df_HI_clean, by = c("iso3c", "Year", "Group"))

summary(df_complete$HI)


##---- Educational Exclusion Variable

# Educational Exclusion
# 1 if all LOI / LC == 0

df_complete <- df_complete %>%
  mutate(
    nbp_educational_exclusion = case_when(
      if_all(
        starts_with("LOI") | starts_with("LC"), ~ . == 0 | is.na(.)
      ) ~ 1,
      TRUE ~ 0
    )
  )

summary(as.factor(df_complete$nbp_educational_exclusion))

# Public Language Restrictions (Exclusion)
# 1 if any restriction on language use

df_complete <- df_complete %>%
  mutate(
    nbp_public_exclusion = case_when(
      if_any(
        c(AnyRestriLang1, AnyRestriLang2, AnyRestriLang3, 
          AnyRestriLangAddiLang, AnyRestriLangStArabic, 
          AnyRestriLangOtherLang), ~ . == 1
      ) ~ 1,
      TRUE ~ 0
    )
  )

summary(as.factor(df_complete$nbp_public_exclusion))

# Any LOI 

df_complete <- df_complete %>%
  mutate(
    nbp_any_loi = case_when(
      if_any(c(LOIPrimary1, LOIPrimary2, LOIPrimary3, LOIPrimaryStArabic, 
               LOIPrimaryAddiLang, LOISecondary1, LOISecondary2, LOISecondary3, 
               LOISecondaryStArabic, LOISecondaryAddiLang), ~ . == 1) ~ 1,
      TRUE ~ 0
    )
  )

summary(as.factor(df_complete$nbp_any_loi))

# just LC, without LOI

df_complete <- df_complete %>%
  mutate(
    nbp_any_lc = case_when(
       if_all(c(LOIPrimary1, LOIPrimary2, LOIPrimary3, LOISecondary1, LOISecondary2, LOISecondary3 # let's ease the condition here and only include the three main languages here - otherwise we dont get any cases fulfilling the conditions
                ), ~ . == 0) &
        if_any(c(LCPrimary1, LCPrimary2, LCPrimary3, LCPrimaryStArabic, 
                 LCPrimaryAddiLang, LCSecondary1, LCSecondary2, LCSecondary3, 
                 LCSecondaryStArabic, LCSecondaryAddiLang), ~ . == 1) ~ 1,
      TRUE ~ 0
    )
  )

summary(as.factor(df_complete$nbp_any_lc))

### ---- CONTROL VARIABLES ------------------------------------------------------

## ---- Number of NBP groups

df_complete <- df_complete %>%
  group_by(iso3c, Year) %>%
  mutate(
    nbp_groups_count = n_distinct(Group)
  ) %>%
  ungroup()

summary(df_complete$nbp_groups_count)

##---- Number of NBP groups educationally excluded (no LOI or LC of group language)

df_edu_ex <- df_complete %>%
  filter(nbp_educational_exclusion == 1) %>%
  group_by(iso3c, Year) %>%
  summarize(
    nbp_edu_exclusion_count = n_distinct(Group),
    .groups = "drop"
  )

summary(df_edu_ex$nbp_edu_exclusion_count)

df_complete <- df_complete %>%
  left_join(df_edu_ex, by = c("iso3c", "Year"))

df_complete <- df_complete %>%
  mutate(nbp_edu_exclusion_count = ifelse(is.na(nbp_edu_exclusion_count), 0, nbp_edu_exclusion_count))

summary(df_complete$nbp_edu_exclusion_count)

# Public Exclusion

df_pub_ex <- df_complete %>%
  filter(nbp_public_exclusion == 1) %>%
  group_by(iso3c, Year) %>%
  summarize(
    nbp_public_exclusion_count = n_distinct(Group),
    .groups = "drop"
  )

summary(df_pub_ex$nbp_public_exclusion_count)

df_complete <- df_complete %>%
  left_join(df_pub_ex, by = c("iso3c", "Year"))

df_complete <- df_complete %>%
  mutate(nbp_public_exclusion_count = ifelse(is.na(nbp_public_exclusion_count), 0, nbp_public_exclusion_count))

summary(df_complete$nbp_public_exclusion_count)

## ---- Transnational Ethnic Kin (TEK)

# Different ways to account for TEK.
# A suitable approach here: Include a dummy indicating whether a kin group holds power in another state.

df_tek <- df_epr_gu %>%
  select(c(gwgroupid, year, tek_egip))%>%
  rename(Year = year) %>%
  mutate(gwgroupid = as.factor(gwgroupid),
         Year = as.numeric(as.character(Year))) %>%
  filter(tek_egip == 1)

summary(df_tek)
str(df_tek$gwgroupid)

str(df_complete$EPRIDgroup)

# df_complete <- df_complete %>%
#   rowwise() %>%
#   mutate(tek_egip_dummy = ifelse(
#     any(c_across(contains("EPRID")) %in% 
#           df_tek[df_tek$Year == Year, ]$gwgroupid), 
#     1, 0
#   )) %>%
#   ungroup()
# 
# summary(df_complete$tek_egip_dummy)
# 
# matches <- df_complete %>%
#   rowwise() %>%
#   mutate(matches = list(intersect(
#     c_across(contains("EPRID")),
#     df_tek$gwgroupid[df_tek$Year == Year]
#   ))) %>%
#   ungroup()
# 
# matches %>%
#   filter(lengths(matches) > 0)

# df_complete <- merge(
#   df_complete,
#   df_tek,
#   by.x = c("EPRIDgroup", "Year"),
#   by.y = c("gwgroupid", "Year"),
#   all.x = TRUE
# )
# 
# summary(df_complete$tek_egip)

# any TEK

# df_tek_2 <- df_epr_gu %>%
#   select(c(gwgroupid, year, tek_count))%>%
#   rename(Year = year) %>%
#   mutate(gwgroupid = as.factor(gwgroupid),
#          Year = as.numeric(as.character(Year))) 
# 
# df_complete <- df_complete %>%
#     rowwise() %>%
#     mutate(tek_egip_dummy = ifelse(
#       any(c_across(contains("EPRID")) %in%
#             df_tek_2[df_tek_2$Year == Year, ]$gwgroupid),
#       1, 0
#     )) %>%
#     ungroup()
# 
# summary(df_complete$tek_egip_dummy)

## I think there is an issue with the EPR Group ID variable in the nbp dataset. The id should have 6 digits, indicating country and group; here starts with "1" - have there been any changes?

## we can try and merge by EPR group NAME, country and year because we already have these from EPR2NBP

df_tek_3 <- df_epr_gu %>%
  select(c(year, groupname, countryname, tek_egip, tek_count))%>%
  rename(Year = year) %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  distinct(Year, groupname, countryname, .keep_all = TRUE)

df_complete <- merge(df_complete, df_tek_3, 
                     by = c("Year", "groupname", "countryname"), 
                     all.x = TRUE)

# df_complete <- df_complete %>%
#   distinct(Year, Country, Group, EPRMergeLevel, .keep_all = TRUE)

summary(as.factor(df_complete$tek_egip))

##---- GDP

# Country-level GDP

# Group-level GDP

# Econ. HI

##---- Population


##### ISSUES WITH GDP AND POPULATION DATA - CF EMAIL


# Population and GDP data from PWT

df_complete <- df_complete %>%
  left_join(df_pwt, by = c("iso3c", "Year"))

##---- Conflict intensity

df_cc2 <- countrycode::codelist %>%
  select(c(iso3c, gwn)) %>%
  rename(gwid = gwn)

df_conflicts <- df_ethnicdyads %>%
  left_join(df_cc2, by = "gwid") %>%
  rename(groupname = group,
         Year = year)

summary(df_conflicts)
summary(as.factor(df_conflicts$iso3c))

df_cNA <- df_conflicts %>%
  filter(is.na(iso3c))

df_conflicts <- df_conflicts %>%
  mutate(iso3c = case_when(
    gwid == "678" ~ "YEM",
    gwid == "345" ~ "YUG",
    TRUE ~ iso3c
  ))

summary(as.factor(df_conflicts$iso3c))

class(df_conflicts$Year)

df_conflicts <- df_conflicts %>%
  mutate(Year = as.numeric(as.character(Year)))

###---- ANALYSIS ---------------------------------------------------------------

summary(as.factor(df_complete$onset_ko_flag))
summary(as.factor(df_complete$incidence_flag))

df_analysis <- df_complete %>%
  select(iso3c, Year, Group, groupname, starts_with("EPRID"), EPRMergeLevel, SizeApprox, groupsize, starts_with("warhist"), peaceyears, SpatialConc, starts_with("epr_"), starts_with("nbp_"), status_excl, excl_groups_count, tek_egip, tek_count, Polity2, HI, starts_with("onset_"), starts_with("incidence_"), SDM, pop, rgdpe, rgdpo, rgdpna, ArrivedPoliticalMigrantsRefugees, ArrivedLabourMigrants, MigrantBackground, CoreGp, Monolingual, MonolingualStrict)

df_analysis <- df_analysis %>%
  mutate(as.numeric(as.character(Year)))

summary(df_analysis)

# lagged variables
lag_vars <- c("nbp_anydown_1",
              "nbp_educational_exclusion",
              "nbp_public_exclusion",
              "nbp_any_loi",
              "nbp_any_lc",
              "HI",
              "SDM",
              "Polity2",
              "rgdpe",
              "rgdpo",
              "rgdpna",
              "groupsize",
              "SpatialConc",
              "status_excl",
              "epr_downgraded1", 
              "Monolingual",
              "MonolingualStrict",
              "pop")

df_analysis <- df_analysis %>%
  arrange(iso3c, Group, Year) %>%
  mutate(across(all_of(lag_vars), ~ lag(.), .names = "lag_{.col}"))

# Conflict Intensity

df_analysis_conflicts <- df_conflicts %>%
  left_join(df_analysis, by = c("iso3c", "Year", "groupname")) %>%
  distinct()

df_analysis_conflicts <- df_analysis_conflicts %>%
  mutate(intensity_level = as.factor(intensity_level))

summary(df_analysis_conflicts)

write.csv(df_analysis, "NBP_2025_conflict_paper_analysis.csv")
write.csv(df_analysis_conflicts, "NBP_2025_conflict_paper_analysis_c.csv")