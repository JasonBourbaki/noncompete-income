# Project: Analyzing the effect of income cutoffs in noncompete bans
# File Description: R Shiny income breakdown dashboard

# last update: 03/28/2025 by Jiaxin He

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
library(shiny)
library(bslib)

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
path_app <- file.path(path_output, "app")

##################
### Data build ###
##################

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  # App title ----
  title = "Hello Shiny!",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: Slider for the number of bins ----
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 1,
      max = 50,
      value = 30
    )
  ),
  # Output: Histogram ----
  plotOutput(outputId = "distPlot")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
}

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

ny_hist_occ_data <- acs_23_workers %>% filter(state == "NY") %>%
  mutate(occ_category = case_when(OCC2010 == 10 ~ "0-Chief Executives",
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
                                  TRUE ~ "Not Identified"),
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
  scale_fill_discrete(labels = c("Chief Executives",
                                 tail(unique(ny_hist_occ_data$occ_category), -1),
                                 "Not Identified"),
                      name = "Occupation") +
  guides(shape = guide_legend(override.aes = list(size = 0.5)),
         color = guide_legend(override.aes = list(size = 0.5))) +
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))
