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
path_app <- file.path(path_project, "income_dashboard")
path_appdata <- file.path(path_app, "data_cleaned")

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
occ_list <- c("Chief Executives",
              "Other Managers",
              "Business Operations Specialists",
              "Financial Specialists",
              "Computer and Mathematical",
              "Architecture and Engineering",
              "Life, Physical, and Social Science", 
              "Community and Social Services",
              "Legal", "Education, Training, and Library",
              "Arts, Design, Entertainment, Sports, and Media",
              "Healthcare Practitioners and Technicians",
              "Healthcare Support", "Protective Service",
              "Food Preparation and Serving",
              "Building and Grounds Cleaning and Maintenance",
              "Personal Care and Service",
              "Sales and Related",
              "Office and Administrative Support",
              "Farming, Fishing, and Forestry",
              "Construction",
              "Extraction",
              "Installation, Maintenance, and Repair",
              "Production",
              "Transportation and Material Moving",
              "Not Identified")

# Filter for waged currently employed private sector workers
acs_23_workers <- acs_23 %>% mutate(UNIQID = SAMPLE*(10^10) + SERIAL*(10^2) + PERNUM) %>%
  distinct(UNIQID, .keep_all = TRUE) %>% # the combination of SAMPLE, SERIAL, and PERNUM uniquely identifies every person in the database.
  filter(EMPSTAT == 1, INCWAGE > 0, # Employed and has non-zero wage income
         CLASSWKRD %in% c(20:23),   # Works for wages/salary, non-government
         STATEFIP <= 56) %>%        # Select 50 states and DC
  select(UNIQID, PERWT, STATEFIP, AGE, SEX, MARST, RACE, EDUC, INCTOT, INCWAGE,
         WKSWORK2, UHRSWORK, IND1990, OCC2010) %>%
  mutate(state = state_codes$state[match(.$STATEFIP, state_codes$state_code)],
         occ_category = case_when(OCC2010 == 10 ~ "Chief Executives",
                                  OCC2010 <= 20 & OCC2010 <= 430 ~ "Other Managers",
                                  OCC2010 <= 500 & OCC2010 <= 730 ~ "Business Operations Specialists",
                                  OCC2010 <= 800 & OCC2010 <= 950 ~ "Financial Specialists",
                                  OCC2010 >= 1000 & OCC2010 <= 1240 ~ "Computer and Mathematical",
                                  OCC2010 >= 1300 & OCC2010 <= 1540 ~ "Architecture and Engineering",
                                  OCC2010 >= 1600 & OCC2010 <= 1980 ~ "Life, Physical, and Social Science",
                                  OCC2010 <= 2000 & OCC2010 <= 2060 ~ "Community and Social Services",
                                  OCC2010 <= 2100 & OCC2010 <= 2150 ~ "Legal",
                                  OCC2010 <= 2200 & OCC2010 <= 2550 ~ "Education, Training, and Library",
                                  OCC2010 <= 2600 & OCC2010 <= 2920 ~ "Arts, Design, Entertainment, Sports, and Media",
                                  OCC2010 >= 3000 & OCC2010 <= 3540 ~ "Healthcare Practitioners and Technicians",
                                  OCC2010 >= 3600 & OCC2010 <= 3650 ~ "Healthcare Support",
                                  OCC2010 >= 3700 & OCC2010 <= 3950 ~ "Protective Service",
                                  OCC2010 >= 4000 & OCC2010 <= 4150 ~ "Food Preparation and Serving",
                                  OCC2010 >= 4200 & OCC2010 <= 4250 ~ "Building and Grounds Cleaning and Maintenance",
                                  OCC2010 >= 4300 & OCC2010 <= 4650 ~ "Personal Care and Service",
                                  OCC2010 >= 4700 & OCC2010 <= 4965 ~ "Sales and Related",
                                  OCC2010 >= 5000 & OCC2010 <= 5940 ~ "Office and Administrative Support",
                                  OCC2010 >= 6005 & OCC2010 <= 6130 ~ "Farming, Fishing, and Forestry",
                                  OCC2010 >= 6200 & OCC2010 <= 6765 ~ "Construction",
                                  OCC2010 >= 6800 & OCC2010 <= 6940 ~ "Extraction",
                                  OCC2010 >= 7000 & OCC2010 <= 7630 ~ "Installation, Maintenance, and Repair",
                                  OCC2010 >= 7700 & OCC2010 <= 8965 ~ "Production",
                                  OCC2010 >= 9000 & OCC2010 <= 9750 ~ "Transportation and Material Moving",
                                  TRUE ~ "Not Identified")) %>%
  mutate(occ_category = factor(occ_category, ordered = TRUE, levels = occ_list)) %>%
  group_by(state) %>% mutate(quantile = weighted_ntile(INCWAGE, PERWT, 40)) %>% ungroup()

# Data for graphing occupations over income distribution, weighted by PERWT
hist_occ_data <- acs_23_workers %>%
  group_by(state, quantile, occ_category) %>%
  summarise(occ_weight = sum(PERWT)) %>% ungroup() %>%
  mutate(share_quantile = occ_weight / sum(occ_weight), .by = c(state, quantile)) %>%
  mutate(percentile = quantile * 0.025) %>% select(-occ_weight)

# Helper function for summarizing sex, age, race, and education information by state
sum_demo <- function(data, column, weights){
  data %>% group_by(state) %>% count(.data[[column]], wt = .data[[weights]]) %>%
    pivot_wider(names_from = .data[[column]], values_from = n) %>%
    mutate(sum=rowSums(across(where(is.numeric))), across(where(is.numeric), ~.x/sum)) %>% select(-sum)
}

# Loop over 40 quantiles to calculate the occupation-based median incomes
# and demographic composition of workers above each quantile.
occ_income_summary <- vector(mode = "list", length = 40)
demo_summary <- vector(mode = "list", length = 40)
for(qtl in 1:40){
  occ_income_summary[[qtl]] <- acs_23_workers %>% filter(quantile >= qtl) %>%
    group_by(state, occ_category) %>%
    summarise(median_income = wtd.quantile(INCWAGE, PERWT, probs = c(0.5))[[1]],
              occ_weight = sum(PERWT)) %>% ungroup() %>%
    mutate(quantile = qtl) %>%
    mutate(share_quantile = occ_weight / sum(occ_weight), .by = state) %>%
    select(-occ_weight)
  
  demo_info <- acs_23_workers %>% filter(quantile >= qtl) %>%
    mutate(sex = case_when(SEX == 1 ~ "M", SEX == 2 ~ "F", TRUE ~ "Missing"),
           age = case_when(16 <= AGE & AGE <= 24 ~ "16-24",
                           25 <= AGE & AGE <= 34 ~ "25-34",
                           35 <= AGE & AGE <= 44 ~ "35-44",
                           45 <= AGE & AGE <= 64 ~ "45-64",
                           65 <= AGE ~ "65 and above"),
           race = case_when(RACE == 1 ~ "White",
                            RACE == 2 ~ "African American",
                            RACE == 3 ~ "Native American",
                            RACE %in% c(4,5,6) ~ "Asian and Pacific Islander",
                            RACE == 7 ~ "Other race",
                            RACE %in% c(8,9) ~ "Multi-racial"),
           education = case_when(EDUC %in% 0:6 ~ "High school or below",
                                 EDUC %in% 7:9 ~ "Some college",
                                 EDUC %in% 10:11 ~ "Bachelor's or above")) %>%
    select(state, sex, age, race, education, PERWT)
  
  demo_summary[[qtl]] <- acs_23_workers %>% group_by(state) %>%
    summarise(income_cutoff = as.integer(wtd.quantile(INCWAGE, PERWT, probs = c(qtl*0.025)))) %>%
    left_join(sum_demo(demo_info, "sex", "PERWT"), by = "state") %>%
    left_join(sum_demo(demo_info, "age", "PERWT"), by = "state") %>%
    left_join(sum_demo(demo_info, "race", "PERWT"), by = "state") %>%
    left_join(sum_demo(demo_info, "education", "PERWT"), by = "state") %>%
    ungroup() %>% mutate(quantile = qtl)
}
occ_income_summary <- bind_rows(occ_income_summary)
demo_summary <- bind_rows(demo_summary)

# Save the data for usage in R Shiny App
setwd(path_appdata)
write.csv(hist_occ_data, "hist_occ_data.csv")
write.csv(occ_income_summary, "occ_income_summary.csv")
write.csv(demo_summary, "demo_summary.csv")
