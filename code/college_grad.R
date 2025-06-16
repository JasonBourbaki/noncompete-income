# Project: Analyzing the effect of income cutoffs in noncompete bans
# File Description: Data processing for R Shiny dashboard

# last update: 4/3/2025 by Jiaxin He

# remove dependencies
rm(list = ls())

###########################
###   Load Packages     ###
###########################
library(scales)
library(tidyr)
library(dplyr)
library(Hmisc)
library(grattan)
library(spatstat)
library(tidycensus)

#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "name" = "PATH TO GITHUB REPO",
  "jiaxinhe" = "/Users/jiaxinhe/Documents/projects/noncompete-income"
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]
path_data <- file.path(path_project, "data")
path_output <- file.path(path_project, "output")

##################
### Data build ###
##################

# Filter for private sector workers making non-zero wages
acs_23 <- read.csv(file.path(path_data, "acs_5y_2023.csv"))

# Query state fips-abbreviation crosswalk
state_codes <- fips_codes %>% select(state_code, state) %>%
  distinct(state_code, .keep_all = TRUE) %>%
  mutate(state_code = as.numeric((state_code))) %>%
  filter(state_code <= 56)

# Ordered list of occupations
occ_list <- read.csv(file.path(path_data, "IPUMS OCC2010 Codes.csv")) %>%
  rename(OCC2010 = Code)

acs_23_college_grads <- acs_23 %>%
  mutate(UNIQID = SAMPLE*(10^10) + SERIAL*(10^2) + PERNUM) %>%
  distinct(UNIQID, .keep_all = TRUE) %>% # the combination of SAMPLE, SERIAL, and PERNUM uniquely identifies every person in the database.
  filter(EMPSTAT == 1,
         INCWAGE > 0,
         STATEFIP <= 56,
         AGE >= 22 & AGE <= 30,
         EDUCD >= 101)

college_grad_occ_rank_median <- acs_23_college_grads %>% group_by(OCC2010) %>%
  summarise(median_wage = weighted.median(INCWAGE, PERWT),
            num_workers = sum(PERWT)) %>% ungroup() %>%
  arrange(desc(median_wage)) %>%
  left_join(occ_list, by = "OCC2010")

college_grad_occ_rank_mean <- acs_23_college_grads %>% group_by(OCC2010) %>%
  summarise(mean_wage = weighted.mean(x = INCWAGE, w = PERWT),
            num_workers = sum(PERWT)) %>% ungroup() %>%
  arrange(desc(mean_wage)) %>%
  left_join(occ_list, by = "OCC2010")

write.csv(college_grad_occ_rank_mean, file = file.path(path_output, "college grad 22-30 occ mean rank.csv"))
write.csv(college_grad_occ_rank_median, file = file.path(path_output, "college grad 22-30 occ median rank.csv"))
