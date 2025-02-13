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

df_NBP <- read_dta("NBP_2025.dta")
df_EPR <- read.csv("EPRConflict.csv") %>%
  mutate(gwgroupid = as.factor(gwgroupid)) %>%
  rename(Year = year)

#---- Exploring NBP ------------------------------------------------------------
 
str(df_NBP)
summary(df_NBP)

nrow(df_NBP)

nrow(distinct(df_NBP))

print(df_NBP$GpInEPR)
print(df_NBP$EPRIDgroup)

df_NBP %>%
  group_by(GpInEPR) %>%
  summarise(UniqueGroups = n_distinct(Group), .groups = "drop")

# The number of unique groups and EPRIDs in NBP

print(length(unique(df_NBP$Group)))
print(length(unique(df_NBP$EPRIDgroup)))

# Groups that are in more than one country

list_tb_groups <- df_NBP %>%
  group_by(Group) %>%
  summarise(NumCountries = n_distinct(Country), .groups = "drop") %>%
  filter(NumCountries > 1) %>%
  pull(Group)

print(list_tb_groups)

# Number of countries and EPRIDs per group

df_numceprid <- df_NBP %>%
  group_by(Group) %>%
  summarise(NumCountries = n_distinct(Country), 
            UniqueEPRIDgroups = n_distinct(EPRIDgroup, na.rm = FALSE), 
            .groups = "drop") %>%
  select(Group, NumCountries, UniqueEPRIDgroups)

summary(df_numceprid)

# Check for logical errors: more EPRIDs than countries

df_check <- df_numceprid %>%
  filter(UniqueEPRIDgroups > NumCountries)

summary(df_check)

# Subgroups with unique EPR ID

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

# THE MERGING APPROACH ADOPTED:

# Step 1: Merge on the group's EPRID where available;
# Step 2: Merge on EPRIDsubgroup (if available) where no EPRID is available;
# Step-wise for transparency + clarity;
# Step 3: Add a variable for merge level to enable filtering or adjusting (e.g., group size) later.

# Create variable that records whether I merge on group or subgroup, start value NA

df_NBP$EPRMergeLevel <- NA

# Merge on EPRID - for groups that have this identifier

df_groupmerge <- merge(
  df_NBP,
  df_EPR,
  by.x = c("EPRIDgroup", "Year"),
  by.y = c("gwgroupid", "Year"),
  all.x = TRUE
)

# Indicate that when EPR ID is not NA, the merge was at the group level (EPRIDgroup)

df_groupmerge$EPRMergeLevel <- ifelse(!is.na(df_groupmerge$EPRIDgroup), "group", df_groupmerge$EPRMergeLevel)

summary(as.factor(df_groupmerge$EPRMergeLevel))

# Select groups without an EPR ID at the group level—either absent or limited to a subgroup

df_unmatched <- subset(df_NBP, is.na(EPRIDgroup))

# Check the SubGroups and Unique EPRIDsubgroup

print(length(unique(df_unmatched$EPRIDsubgroup1)))
print(length(unique(df_unmatched$EPRIDsubgroup2)))
print(length(unique(df_unmatched$EPRIDsubgroup3)))

# Merge on EPRID of subgroup 1, but only for groups without a group-level ID. Note: Some cases where EPRIDgroup == EPRIDsubgroup1 may require further review

df_merge_subgroup1 <- merge(
  df_unmatched,
  df_EPR,
  by.x = c("EPRIDsubgroup1", "Year"),
  by.y = c("gwgroupid", "Year"),
  all.x = TRUE
)

# Set ERPMergeLevel to subgroup1

df_merge_subgroup1$EPRMergeLevel <- ifelse(!is.na(df_merge_subgroup1$EPRIDsubgroup1), "subgroup1", df_merge_subgroup1$EPRMergeLevel)

summary(as.factor(df_merge_subgroup1$EPRMergeLevel))

# Merge on Subgroup2: For groups without a group-level EPR ID, merge on subgroup2.

df_merge_subgroup2 <- merge(
  df_unmatched,
  df_EPR,
  by.x = c("EPRIDsubgroup2", "Year"),
  by.y = c("gwgroupid", "Year"),
  all.x = TRUE
)

# Set ERPMergeLevel to subgroup2

df_merge_subgroup2$EPRMergeLevel <- ifelse(!is.na(df_merge_subgroup2$EPRIDsubgroup2), "subgroup2", df_merge_subgroup2$EPRMergeLevel)

summary(as.factor(df_merge_subgroup2$EPRMergeLevel))

# Merge on Subgroup3: For groups without a group-level EPRID, merge on subgroup3

df_merge_subgroup3 <- merge(
  df_unmatched,
  df_EPR,
  by.x = c("EPRIDsubgroup3", "Year"),
  by.y = c("gwgroupid", "Year"),
  all.x = TRUE
)

# Set EPRMergeLevel to subgroup3

df_merge_subgroup3$EPRMergeLevel <- ifelse(!is.na(df_merge_subgroup3$EPRIDsubgroup3), "subgroup3", df_merge_subgroup3$EPRMergeLevel)

summary(as.factor(df_merge_subgroup3$EPRMergeLevel))

# Combine all merged groups step by step, adding unmerged groups after the subgroup3 merge.

df_merge_subgroup1 <- df_merge_subgroup1 %>%
  mutate(EPRMergeLevel = as.character(EPRMergeLevel))

df_merge_subgroup2 <- df_merge_subgroup2 %>%
  mutate(EPRMergeLevel = as.character(EPRMergeLevel))

df_merge_subgroup3 <- df_merge_subgroup3 %>%
  mutate(EPRMergeLevel = as.character(EPRMergeLevel))


df_groupmerge <- df_groupmerge %>%
  mutate(EPRIDgroup = as.character(EPRIDgroup),
         EPRIDsubgroup1 = as.character(EPRIDsubgroup1),
         EPRIDsubgroup2 = as.character(EPRIDsubgroup2),
         EPRIDsubgroup3 = as.character(EPRIDsubgroup3))


df_merge_subgroup1 <- df_merge_subgroup1 %>%
  mutate(EPRIDgroup = as.character(EPRIDgroup),
         EPRIDsubgroup1 = as.character(EPRIDsubgroup1),
         EPRIDsubgroup2 = as.character(EPRIDsubgroup2),
         EPRIDsubgroup3 = as.character(EPRIDsubgroup3))


df_merge_subgroup2 <- df_merge_subgroup2 %>%
  mutate(EPRIDgroup = as.character(EPRIDgroup),
         EPRIDsubgroup1 = as.character(EPRIDsubgroup1),
         EPRIDsubgroup2 = as.character(EPRIDsubgroup2),
         EPRIDsubgroup3 = as.character(EPRIDsubgroup3))


df_merge_subgroup3 <- df_merge_subgroup3 %>%
  mutate(EPRIDgroup = as.character(EPRIDgroup),
         EPRIDsubgroup1 = as.character(EPRIDsubgroup1),
         EPRIDsubgroup2 = as.character(EPRIDsubgroup2),
         EPRIDsubgroup3 = as.character(EPRIDsubgroup3))

print(df_merge_subgroup1$EPRIDsubgroup1)
print(df_merge_subgroup1$EPRID)

df_EPR2NBP <- bind_rows(
  subset(df_groupmerge, !is.na(EPRMergeLevel)),        
  subset(df_merge_subgroup1, !is.na(EPRMergeLevel)),   
  subset(df_merge_subgroup2, !is.na(EPRMergeLevel)),   
  subset(df_merge_subgroup3, !is.na(EPRMergeLevel)),   
  subset(df_merge_subgroup3, is.na(EPRMergeLevel))     
)

# Check the final dataset—now with subgroups as the smallest unit. The count is slightly higher than in the original NBP, but EPRMergeLevel allows filtering as needed

nrow(df_EPR2NBP)
nrow(distinct(df_EPR2NBP))

df_EPR2NBP <- df_EPR2NBP %>%
  mutate(EPRMergeLevel = as.factor(EPRMergeLevel)) 

df_EPR2NBP <- df_EPR2NBP %>%
  distinct()

summary(df_EPR2NBP)

summary(df_EPR2NBP$EPRMergeLevel)

# Write the Matched Data in New Formats

write.csv(df_EPR2NBP, "EPR2NBP_2025-csv.csv")
#write_dta(df_EPR2NBP, "EPR2NBP.dta")
write_dta(df_EPR2NBP, "EPR2NBP-dta_new2.dta", label = NULL)

#check against Andrei's results

df_Andrei <- read.csv ("UPD_Merged_Filtered_NBP_and_EPR_conflict.csv")

nrow(df_Andrei)

nrow(distinct(df_Andrei)) #check code again; lots of duplicates
