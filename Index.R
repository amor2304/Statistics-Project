# ---------------------------------------------------------
# STEP 1: CLEAR MEMORY AND LOAD PACKAGES
# ---------------------------------------------------------
rm(list = ls())
gc()

library(readr)
library(dplyr)

# ---------------------------------------------------------
# STEP 2: LOAD THE PLFS CSV DATA
# ---------------------------------------------------------
setwd("C:/Users/amorp/Downloads")

# Load only the specific columns we need to save memory
# (If your CSV uses uppercase headers like SEX, change these to match)
plfs <- read_csv("cperv12025.csv", 
                 col_select = c(sex, age, gedu_lvl, pas, mult))

# ---------------------------------------------------------
# STEP 3: FILTER TO WORKING-AGE WOMEN
# ---------------------------------------------------------
# Keep only females (sex == 2) aged 15 to 59
plfs_women <- plfs %>%
  filter(sex == 2, age >= 15, age <= 59)

# ---------------------------------------------------------
# STEP 4: CREATE THE LABOUR FORCE VARIABLE
# ---------------------------------------------------------
# 1 = In labour force (Employed 11-51 OR Unemployed 81)
# 0 = Not in labour force (Students, Homemakers, Retired 91-97)
plfs_women <- plfs_women %>%
  mutate(
    in_labour_force = if_else(pas %in% c(11:51, 81), 1, 0)
  )

# ---------------------------------------------------------
# STEP 5: CALCULATE WEIGHTED LFPR BY EDUCATION
# ---------------------------------------------------------
results_plfs <- plfs_women %>%
  group_by(gedu_lvl) %>%
  summarise(
    total_women = n(),
    weighted_lfpr = weighted.mean(in_labour_force, w = mult)
  )

# ---------------------------------------------------------
# STEP 6: PRINT THE RESULTS
# ---------------------------------------------------------
print(results_plfs)
