###-----------------------------------------------------------------------------
### MERGING NBP AND EPR
### 
### Jan
###
### 12/2024
###-----------------------------------------------------------------------------

##---- Packages ----------------------------------------------------------------
library(tidyverse)
library(haven)
library(writexl)

##---- Data --------------------------------------------------------------------
#read the data

df_NBP <- read_dta("NBP_groups_final.dta")%>%
  mutate(across(contains("EPRID"), as.factor))
df_EPR <- read.csv("EPRConflict.csv") %>%
  mutate(gwgroupid = as.factor(gwgroupid)) %>%
  rename(Year = year)

#Bit of EDA - Just having a look at the NBP data and the EPR ID variable
#you can probably simple ignore everything up to line 91
#---- NBP ---------------------------------------------------------------------
 
str(df_NBP)
summary(df_NBP)

nrow(df_NBP)

nrow(distinct(df_NBP))

print(df_NBP$GpInEPR)
print(df_NBP$EPRIDgroup)

df_NBP %>%
  group_by(GpInEPR) %>%
  summarise(UniqueGroups = n_distinct(Group), .groups = "drop")

#check how many groups and unique EPR IDs we actually have
print(length(unique(df_NBP$Group)))
print(length(unique(df_NBP$EPRIDgroup)))

#groups in more than one country
list_tb_groups <- df_NBP %>%
  group_by(Group) %>%
  summarise(NumCountries = n_distinct(Country), .groups = "drop") %>%
  filter(NumCountries > 1) %>%
  pull(Group)

print(list_tb_groups)

#no. of countries and EPR IDs per group
df_numceprid <- df_NBP %>%
  group_by(Group) %>%
  summarise(NumCountries = n_distinct(Country), 
            UniqueEPRIDgroups = n_distinct(EPRIDgroup, na.rm = FALSE), 
            .groups = "drop") %>%
  select(Group, NumCountries, UniqueEPRIDgroups)

summary(df_numceprid)

#check for logical error: more EPRID that countries
df_check <- df_numceprid %>%
  filter(UniqueEPRIDgroups > NumCountries)

summary(df_check)

#and subgroups with unique EPR ID
df_subgroups <- df_NBP %>%
  select(starts_with("EPRIDsubgroup"))

unique_counts <- sapply(df_subgroups, function(x) n_distinct(x, na.rm = TRUE))

list_repgsg <- sum(as.character(df_NBP$EPRIDgroup) == as.character(df_NBP$EPRIDsubgroup1), na.rm = TRUE)

list_repgsg

print(unique_counts)

#---- EPR Conflicts ------------------------------------------------------------

summary(df_EPR)

print(df_EPR$gwgroupid)

print(length(unique(df_EPR$gwgroupid)))
print(length(unique(df_EPR$groupname)))

#---- MERGING ------------------------------------------------------------------

#DISCUSSED APPROACH:
#merge on group's EPR ID where available, 
#merge on subgroup ID where no group ID available
#step-wise for transparency
#and incl. variable indicating merge level so we can later filter them out or adjust e.g. groupsize accordingly

# create variable that records whether I merge on group or subgroup, start value NA
df_NBP$EPRMergeLevel <- NA

# simply merge on EPRID - works for groups that have one
df_groupmerge <- merge(
  df_NBP,
  df_EPR,
  by.x = c("EPRIDgroup", "Year"),
  by.y = c("gwgroupid", "Year"),
  all.x = TRUE
)

# indicate that where EPR ID is not NA, I merged on group level (EPRIDgroup)
df_groupmerge$EPRMergeLevel <- ifelse(!is.na(df_groupmerge$EPRIDgroup), "group", df_groupmerge$EPRMergeLevel)

summary(as.factor(df_groupmerge$EPRMergeLevel))

# take all groups that DO NOT have an EPR ID at the group level - either because there is none or because there is only one for a subgroup
df_unmatched <- subset(df_NBP, is.na(EPRIDgroup))

# have a look at the subgroups 
print(length(unique(df_unmatched$EPRIDsubgroup1)))
print(length(unique(df_unmatched$EPRIDsubgroup2)))
print(length(unique(df_unmatched$EPRIDsubgroup3)))

# merge on EPR ID of subgroup 1 - recall: only for groups that do not have an ID at the group level (There are some where e.g., EPRIDgroup == EPRIDsubgroup1 [maybe that's to be looked into] so that would otherwise be an issue)
df_merge_subgroup1 <- merge(
  df_unmatched,
  df_EPR,
  by.x = c("EPRIDsubgroup1", "Year"),
  by.y = c("gwgroupid", "Year"),
  all.x = TRUE
)

# report that ERPMergeLevel is subgroup1
df_merge_subgroup1$EPRMergeLevel <- ifelse(!is.na(df_merge_subgroup1$EPRIDsubgroup1), "subgroup1", df_merge_subgroup1$EPRMergeLevel)

summary(as.factor(df_merge_subgroup1$EPRMergeLevel))

# and the same for subgroup2 - where NO EPR ID is available at the group level, I merge on subgroup2
df_merge_subgroup2 <- merge(
  df_unmatched,
  df_EPR,
  by.x = c("EPRIDsubgroup2", "Year"),
  by.y = c("gwgroupid", "Year"),
  all.x = TRUE
)

# as above
df_merge_subgroup2$EPRMergeLevel <- ifelse(!is.na(df_merge_subgroup2$EPRIDsubgroup2), "subgroup2", df_merge_subgroup2$EPRMergeLevel)

summary(as.factor(df_merge_subgroup2$EPRMergeLevel))

# ... and for subgroup3
df_merge_subgroup3 <- merge(
  df_unmatched,
  df_EPR,
  by.x = c("EPRIDsubgroup3", "Year"),
  by.y = c("gwgroupid", "Year"),
  all.x = TRUE
)

df_merge_subgroup3$EPRMergeLevel <- ifelse(!is.na(df_merge_subgroup3$EPRIDsubgroup3), "subgroup3", df_merge_subgroup3$EPRMergeLevel)

summary(as.factor(df_merge_subgroup3$EPRMergeLevel))

# put it all together, always taking the merged groups from each step, and adding the groups that were not merged after merging on subgroup 3 in the end
df_merge_subgroup1 <- df_merge_subgroup1 %>%
  mutate(EPRMergeLevel = as.character(EPRMergeLevel))

df_merge_subgroup2 <- df_merge_subgroup2 %>%
  mutate(EPRMergeLevel = as.character(EPRMergeLevel))

df_merge_subgroup3 <- df_merge_subgroup3 %>%
  mutate(EPRMergeLevel = as.character(EPRMergeLevel))

print(df_merge_subgroup1$EPRIDsubgroup1)

df_EPR2NBP <- bind_rows(
  subset(df_groupmerge, !is.na(EPRMergeLevel)),        
  subset(df_merge_subgroup1, !is.na(EPRMergeLevel)),   
  subset(df_merge_subgroup2, !is.na(EPRMergeLevel)),   
  subset(df_merge_subgroup3, !is.na(EPRMergeLevel)),   
  subset(df_merge_subgroup3, is.na(EPRMergeLevel))     
)

# check final dataset - with subgroup as smallest u.o.a. now obviously now slightly more than in original NBP, but EPRMergeLevel variable allows to kick them out where we don't want them
nrow(df_EPR2NBP)
nrow(distinct(df_EPR2NBP))

df_EPR2NBP <- df_EPR2NBP %>%
  mutate(EPRMergeLevel = as.factor(EPRMergeLevel)) 

summary(df_EPR2NBP)

summary(df_EPR2NBP$EPRMergeLevel)

write.csv(df_EPR2NBP, "EPR2NBP-csv.csv")
#write_dta(df_EPR2NBP, "EPR2NBP.dta")
write_dta(df_EPR2NBP, "EPR2NBP-dta_new.dta")

#check against Andrei's results

df_Andrei <- read.csv ("UPD_Merged_Filtered_NBP_and_EPR_conflict.csv")

nrow(df_Andrei)

nrow(distinct(df_Andrei)) #check code again; lots of duplicates
