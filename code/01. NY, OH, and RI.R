# Project: Analyzing the effect of income cutoffs in noncompete bans
# File Description: 5 year New York and Ohio Labor Statistics

# last update: 3/31/2025 by Jiaxin He

# remove dependencies
rm(list = ls())

###########################
###   Load Packages     ###
###########################
library(readxl)
library(scales)
library(tidyr)
library(dplyr)
library(Hmisc)
library(grattan)
library(tidycensus)
library(ggplot2)

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
acs_23_workers <- acs_23 %>% mutate(UNIQID = SAMPLE*(10^10) + SERIAL*(10^2) + PERNUM)  %>%
  distinct(UNIQID, .keep_all = TRUE) %>% # the combination of SAMPLE, SERIAL, and PERNUM uniquely identifies every person in the database.
  filter(EMPSTAT == 1, INCWAGE > 0, # Employed and has non-zero wage income
         CLASSWKRD %in% c(20:23),   # Works for wages/salary, non-government
         STATEFIP <= 56) %>%        # Select 50 states and DC
  select(UNIQID, PERWT, STATEFIP, AGE, SEX, MARST, RACE, EDUC, INCTOT, INCWAGE, IND1990, OCC2010) %>%
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
  mutate(occ_category = factor(occ_category, ordered = TRUE, levels = occ_list))

# Generate income cutoffs for each state
percentile_bins <- c(0.8, 0.9, 0.95)
income_bins <- acs_23_workers %>% group_by(state) %>%
  reframe(bins = wtd.quantile(INCWAGE, weights = PERWT, probs = percentile_bins)) %>%
  mutate(row_id = row_number(), .by = state) %>%
  pivot_wider(names_from = row_id, values_from = bins) %>%
  rename_with(~paste0(percentile_bins*100, "p"), as.character(1:3))

setwd(path_output)
write.csv(income_bins %>% filter(state %in% c("NY", "OH")), "NY and OH Income Cutoffs.csv")

# Find the distribution of executives in each income bin
bin_names <- paste(paste0(c(0, percentile_bins)*100, "p"),
                   paste0(c(percentile_bins, 1)*100, "p"),
                   sep = "-")
exec_states <- acs_23_workers %>% left_join(income_bins, by = "state") %>%
  mutate(exec_weight = as.numeric(OCC2010 == 10) * PERWT,
         incbin = case_when(INCWAGE <= `80p` ~ 1,
                            `80p` < INCWAGE & INCWAGE <= `90p` ~ 2,
                            `90p` < INCWAGE & INCWAGE <= `95p` ~ 3,
                            `95p` < INCWAGE ~ 4, TRUE ~ NA)) %>% group_by(state, incbin) %>%
  summarise(exec_weight = sum(exec_weight)) %>% ungroup() %>%
  mutate(exec_dist = exec_weight / sum(exec_weight), .by = state) %>% select(-exec_weight) %>%
  pivot_wider(names_from = incbin, values_from = exec_dist) %>%
  rename_with(~bin_names, as.character(1:4)) %>%
  left_join(income_bins, by = "state")

# Generate histogram for share of executives
ny_hist_data <- acs_23_workers %>% filter(state == "NY") %>%
  mutate(exec_weight = as.numeric(OCC2010 == 10) * PERWT,
         quantile = weighted_ntile(INCWAGE, PERWT, 40)) %>%
  group_by(quantile) %>% summarise(exec_weight = sum(exec_weight), total_weight = sum(PERWT)) %>%
  mutate(percent_exec = exec_weight / total_weight, percentile = quantile * 0.025, is_exec = "Yes")
ny_hist_data <- rbind(ny_hist_data %>% mutate(percent_exec = 1-percent_exec, is_exec = "No"),
                      ny_hist_data) %>% arrange(percentile)

ny_hist <- ggplot(ny_hist_data, aes(fill = is_exec, x = percentile, y = percent_exec)) +
  geom_bar(position = "fill", stat = "identity", just = 1) + theme_bw() +
  
  geom_vline(xintercept = 0.8, linetype="solid", size=0.5) +
  geom_text(aes(x = 0.78, y = 1.05, label = "80%"), angle = 90, stat = "unique", size = 3) +
  geom_vline(xintercept = 0.9, linetype="solid", size=0.5) +
  geom_text(aes(x = 0.88, y = 1.05, label = "90%"), angle = 90, stat = "unique", size = 3) +
  geom_vline(xintercept = 0.95, linetype="solid", size=0.5) +
  geom_text(aes(x = 0.93, y = 1.05, label = "95%"), angle = 90, stat = "unique", size = 3) +
  
  ylab("Share of Workers in the Income Bin") +
  xlab("Weighted Income Percentile") +
  scale_fill_discrete(name = "Chief Exec")

# Save the histogram
setwd(path_output)
ggsave("NY State Share of Chief Executives by Income Percentile.jpg", ny_hist)

ny_hist_occ_data <- acs_23_workers %>% filter(state == "NY") %>%
  mutate(occ_category = case_when(OCC2010 == 10 ~ "0-Chief Executives",
                                  OCC2010 <= 20 & OCC2010 <= 430 ~ "1-Other Managers",
                                  OCC2010 <= 500 & OCC2010 <= 730 ~ "2-Business Operations Specialists",
                                  OCC2010 >= 4700 & OCC2010 <= 4965 ~ "3-Sales and Related",
                                  OCC2010 >= 1000 & OCC2010 <= 1240 ~ "Computer and Mathematical",
                                  OCC2010 >= 1300 & OCC2010 <= 1540 ~ "Architecture and Engineering",
                                  OCC2010 >= 1600 & OCC2010 <= 1980 ~ "Life, Physical, and Social Science",
                                  OCC2010 >= 3000 & OCC2010 <= 3540 ~ "Healthcare Practitioners and Technicians",
                                  TRUE ~ "Z-Other Occupations"),
         quantile = weighted_ntile(INCWAGE, PERWT, 40)) %>%
  group_by(quantile, occ_category) %>% summarise(occ_weight = sum(PERWT)) %>% ungroup() %>%
  mutate(share_quantile = occ_weight / sum(occ_weight), .by = quantile) %>%
  mutate(percentile = quantile * 0.025) %>% select(-occ_weight)

ny_hist_occ <- ggplot(ny_hist_occ_data, aes(fill = occ_category, x = percentile, y = share_quantile)) +
  geom_bar(position = "fill", stat = "identity", just = 1) + theme_bw() +
  
  geom_vline(xintercept = 0.8, linetype="solid", size=0.5) +
  geom_text(aes(x = 0.78, y = 1.05, label = "80%"), angle = 90, stat = "unique", size = 3) +
  geom_vline(xintercept = 0.9, linetype="solid", size=0.5) +
  geom_text(aes(x = 0.88, y = 1.05, label = "90%"), angle = 90, stat = "unique", size = 3) +
  geom_vline(xintercept = 0.95, linetype="solid", size=0.5) +
  geom_text(aes(x = 0.93, y = 1.05, label = "95%"), angle = 90, stat = "unique", size = 3) +
  
  ylab("Share of Workers in the Income Bin") +
  xlab("Weighted Income Percentile") +
  ggtitle("New York Occupation Breakdown by Income Percentile") +
  scale_fill_discrete(labels = c("Chief Executives", "Other Managers",
                                 "Business Operations Specialists", "Sales and Related",
                                 head(tail(unique(ny_hist_occ_data$occ_category), -4), -1),
                                 "Other Occupations"),
                      name = "Occupation")

ny_income_percents <- acs_23_workers %>% filter(state == "NY") %>% left_join(income_bins, by = "state") %>%
  mutate(occ_category = case_when(OCC2010 == 10 ~ "0-Chief Executives",
                                  OCC2010 <= 20 & OCC2010 <= 430 ~ "1-Other Managers",
                                  OCC2010 <= 500 & OCC2010 <= 730 ~ "2-Business Operations Specialists",
                                  OCC2010 >= 4700 & OCC2010 <= 4965 ~ "3-Sales and Related",
                                  OCC2010 >= 1000 & OCC2010 <= 1240 ~ "Computer and Mathematical",
                                  OCC2010 >= 1300 & OCC2010 <= 1540 ~ "Architecture and Engineering",
                                  OCC2010 >= 1600 & OCC2010 <= 1980 ~ "Life, Physical, and Social Science",
                                  OCC2010 >= 3000 & OCC2010 <= 3540 ~ "Healthcare Practitioners and Technicians",
                                  TRUE ~ "Z-Other Occupations"),
         incbin = case_when(INCWAGE <= `80p` ~ 1,
                            `80p` < INCWAGE & INCWAGE <= `90p` ~ 2,
                            `90p` < INCWAGE & INCWAGE <= `95p` ~ 3,
                            `95p` < INCWAGE ~ 4,
                            TRUE ~ NA)) %>%
  group_by(incbin, occ_category) %>% summarise(occ_weight = sum(PERWT)) %>% ungroup() %>%
  mutate(share_bin = occ_weight / sum(occ_weight), .by = incbin) %>% select(-occ_weight) %>%
  pivot_wider(names_from = incbin, values_from = share_bin) %>%
  mutate(Occupation = c("Chief Executives", "Other Managers",
                        "Business Operations Specialists", "Sales and Related",
                        head(tail(unique(occ_category), -4), -1),
                        "Other Occupations")) %>%
  select(-occ_category) %>% relocate(Occupation) %>%
  rename_with(~bin_names, as.character(1:4))

# Save the histogram
setwd(path_output)
write.csv(ny_income_percents, "New York Occupation Breakdown by Income Percentile.csv")
ggsave("New York Occupation Breakdown by Income Percentile.jpg", ny_hist_occ)

# Generate analysis for Rhode Island S0302
ri_income_percents <- acs_23_workers %>% filter(state == "RI") %>%
  mutate(above_threshold = as.numeric(INCTOT > 125000))

# Around 9.5% of Rhode Island's total workforce will be excluded
ri_tot_share <- sum(ri_income_percents %>% filter(above_threshold == 1) %>% .$PERWT) / sum(ri_income_percents$PERWT)

# Around 17.5% of Rhode Island's healthcare practitioners and technicians will be excluded
ri_health_share <- sum(ri_income_percents %>% filter(above_threshold == 1, occ_category == "Healthcare Practitioners and Technicians") %>% .$PERWT) /
  sum(ri_income_percents %>% filter(occ_category == "Healthcare Practitioners and Technicians") %>% .$PERWT)

# Around 20.4% of Rhode Island's STEM workforce will be excluded
ri_stem_share <- sum(ri_income_percents %>% filter(above_threshold == 1, occ_category %in% c("Computer and Mathematical",
                                                                            "Architecture and Engineering",
                                                                            "Life, Physical, and Social Science")) %>% .$PERWT) /
  sum(ri_income_percents %>% filter(occ_category %in% c("Computer and Mathematical",
                                                        "Architecture and Engineering",
                                                        "Life, Physical, and Social Science")) %>% .$PERWT)

# Read in 2023 RI OEWS employment numbers
ri_oews_emp <- read_xlsx(file.path(path_data, "state_M2023_dl.xlsx"), sheet = "state_M2023_dl") %>%
  filter(PRIM_STATE == "RI", substr(OCC_CODE, 4, 7) == "0000")
ri_noncompetes_summary <- ri_oews_emp %>% select(OCC_CODE, OCC_TITLE, TOT_EMP) %>%
  filter(substr(OCC_CODE, 1, 2) %in% c("00", "15", "17", "19", "29")) %>%
  mutate(`Total Employment` = as.numeric(TOT_EMP),
         Category = case_when(
           OCC_CODE == "00-0000" ~ "All Workers",
           OCC_CODE %in% c("15-0000", "17-0000", "19-0000") ~ "STEM Workers",
           OCC_CODE == "29-0000" ~ "Healthcare Practitioners and Technicians")) %>%
  group_by(Category) %>% summarise(`Total Employment` = sum(`Total Employment`)) %>% ungroup() %>%
  mutate(`Share Earning above $125,000` = c(ri_tot_share, ri_health_share, ri_stem_share),
         `Expected Number of Excluded Workers` = round(`Total Employment`*`Share Earning above $125,000`, 0))

write.csv(ri_noncompetes_summary, file.path(path_output, "RI S0302 Workers Earning above Income Threshold.csv"))
