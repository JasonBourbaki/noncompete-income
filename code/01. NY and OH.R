# Project: Analyzing the effect of income cutoffs in noncompete bans
# File Description: CPS ASEC New York and Ohio Labor Statistics

# last update: 03/26/2025 by Jiaxin He

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
  mutate(percent_exec = exec_weight / total_weight, percentile = quantile * 0.025)

ny_hist <- ggplot(ny_hist_data, aes(x = percentile, y = percent_exec)) +
  geom_bar(stat = "identity", just = 1) +
  geom_vline(xintercept = 0.8, linetype="dotted", color = "blue", size=0.5) +
  geom_vline(xintercept = 0.9, linetype="dotted", color = "blue", size=0.5) +
  geom_vline(xintercept = 0.95, linetype="dotted", color = "blue", size=0.5) +
  ylab("Share of Chief Executives in the Income Bin") +
  xlab("Weighted Income Percentile")

setwd(path_output)
ggsave("NY State Share of Chief Executives by Income Percentile.jpg", ny_hist)

