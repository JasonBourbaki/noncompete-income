# Project: Analyzing the effect of income cutoffs in noncompete bans
# File Description: 5 year New York and Ohio Labor Statistics

# last update: 3/31/2025 by Jiaxin He

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

# Filter for waged currently employed private sector workers
acs_23_workers <- acs_23 %>% mutate(UNIQID = SAMPLE*(10^10) + SERIAL*(10^2) + PERNUM) %>%
  distinct(UNIQID, .keep_all = TRUE) %>%
  filter(EMPSTAT == 1, INCWAGE > 0, CLASSWKRD %in% c(20:23), STATEFIP <= 56) %>%
  select(UNIQID, PERWT, STATEFIP, AGE, SEX, MARST, RACE, EDUC, INCTOT, INCWAGE,
         WKSWORK2, UHRSWORK, IND1990, OCC2010) %>%
  mutate(state = state_codes$state[match(.$STATEFIP, state_codes$state_code)])

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
