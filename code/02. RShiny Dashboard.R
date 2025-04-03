# Project: Analyzing the effect of income cutoffs in noncompete bans
# File Description: R Shiny income breakdown dashboard

# last update: 04/03/2025 by Jiaxin He

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
library(ggplot2)
library(dichromat)
library(cowplot)

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
acs_23_workers <- acs_23 %>% mutate(UNIQID = SAMPLE*(10^10) + SERIAL*(10^2) + PERNUM) %>%
  distinct(UNIQID, .keep_all = TRUE) %>%
  filter(EMPSTAT == 1, INCWAGE > 0, CLASSWKRD %in% c(20:23), STATEFIP <= 56) %>%
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

hist_occ_data <- acs_23_workers %>%
  group_by(state, quantile, occ_category) %>%
  summarise(occ_weight = sum(PERWT)) %>% ungroup() %>%
  mutate(share_quantile = occ_weight / sum(occ_weight), .by = c(state, quantile)) %>%
  mutate(percentile = quantile * 0.025) %>% select(-occ_weight)

sum_demo <- function(data, column, weights){
  data %>% group_by(state) %>% count(.data[[column]], wt = .data[[weights]]) %>%
    pivot_wider(names_from = .data[[column]], values_from = n) %>%
    mutate(sum=rowSums(across(where(is.numeric))), across(where(is.numeric), ~.x/sum)) %>% select(-sum)
}

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
  
  demo_summary[[qtl]] <- sum_demo(demo_info, "sex", "PERWT") %>%
    left_join(sum_demo(demo_info, "age", "PERWT"), by = "state") %>%
    left_join(sum_demo(demo_info, "race", "PERWT"), by = "state") %>%
    left_join(sum_demo(demo_info, "education", "PERWT"), by = "state") %>%
    ungroup() %>% mutate(quantile = qtl)
}
occ_income_summary <- bind_rows(occ_income_summary)
demo_summary <- bind_rows(demo_summary)

######################
### Build Shiny UI ###
######################

# Define EIG color palette
eig_colors <- c("#b3d6dd", "#79c5fd", "#176F96", "#234f8b", 	# EIG blue colors
                "#008080", "#5e9c86", "#1a654d", "#044140",	  # EIG green colors
                "#feecd6", "#f0b799", "#da9969", "#e1ad28")		# EIG beige, red, and yellow
eig_palette <- function(n_colors, input_palette){
  if(n_colors <= length(input_palette)){
    return(input_palette[floor(seq(from = 1, to = length(input_palette), length.out = n_colors))])
  }else{
    return(colorRampPalette(input_palette)(n_colors))
  }
}

ui <- page_sidebar(
  tags$style(type='text/css', "#description { font-size: 13px; }
             .selectize-input { font-size: 75%; }
             .selectize-dropdown { font-size: 75%; }
             .income_percent { display: flex; justify-content: center; margin: auto; width: 80%; }
             .irs-from, .irs-to, .irs-min, .irs-max { visibility: hidden !important"),
  
  title = "State Occupation Breakdown by Income Threshold",
  verbatimTextOutput("description"),
  sidebar = sidebar(
    style = "font-size:75%;",
    
    ### U.S. State Selector ###
    selectizeInput( 
      "state_selector", 
      "Select state to view", 
      choices = state_codes$state,
      selected = "NY"
    ),
    
    ### Occupation Category Selector ###
    selectizeInput(
      "occupation_selector", 
      "Select occupations", 
      choices = occ_list,
      selected = c(
        "Chief Executives",
        "Other Managers"
      ),
      multiple = TRUE
    ),
    
    sliderInput("income_percent", label = NULL, min = 0, max = 1, value = 0.95, step = 0.025),
    
    width = validateCssUnit(250)
  ),
  
  navset_card_tab(
    ### Plot with Income Percentile Slider ###
    nav_panel("Income Distribution", plotOutput("plot_dist")),
    
    ### Median Incomes per Category above Wage Cap ###
    nav_panel("Median Incomes", tableOutput("tbl_inc")),
    
    ### Demographic Characteristics of Those above Wage Cap ###
    nav_panel("Demographics", plotOutput("plots_dem"))
  )
)

##########################
### Build Shiny Server ###
##########################

server <- function(input, output) {
  selected_occ_data <- reactive({
    hist_occ_data %>% filter(state == input$state_selector) %>%
      mutate(Occupation = case_when(occ_category %in% input$occupation_selector ~ occ_category,
                                    TRUE ~ "Other Occupations")) %>%
      mutate(Occupation = factor(Occupation, ordered = TRUE,
                                 levels = c(input$occupation_selector, "Other Occupations")))
  })
  
  excluded_occ_income <- reactive({
    occ_income_summary %>% ungroup() %>%
      filter(state == input$state_selector,
             occ_category %in% input$occupation_selector,
             quantile == as.integer(as.numeric(input$income_percent)*40) + 1) %>%
      select(occ_category, median_income, share_quantile) %>%
      mutate(share_quantile = percent(share_quantile, accuracy = 0.1)) %>%
      rename(Occupations = occ_category, "Median Income" = median_income,
             "Share of Workers" = share_quantile)
  })
  
  selected_demo_data <- reactive({
    demo_summary %>% filter(state == input$state_selector,
                            quantile == as.integer(as.numeric(input$income_percent)*40) + 1)
  })
  selected_sex <- reactive({
    selected_demo_data() %>% select(M, `F`) %>%
      pivot_longer(cols = everything(), names_to = "Sex", values_to = "Share") %>%
      mutate(Sex = factor(Sex, ordered = TRUE, levels = c("F", "M")),
        dummy = as.character(row_number()))
  })
  selected_age <- reactive({
    selected_demo_data() %>% select(4:8) %>%
      pivot_longer(cols = everything(), names_to = "Age", values_to = "Share") %>%
      mutate(Age = factor(Age, ordered = TRUE, levels = c("65 and above", "45-64", "35-44", "25-34", "16-24")),
             dummy = as.character(row_number()))
  })
  selected_race <- reactive({
    selected_demo_data() %>% select(9:14) %>%
      pivot_longer(cols = everything(), names_to = "Race", values_to = "Share") %>%
      mutate(Race = factor(Race, ordered = TRUE, levels = c("Native American", "Other race", "Multi-racial",
                                                            "Asian and Pacific Islander", "African American", "White")),
             dummy = as.character(row_number()))
  })
  selected_edu <- reactive({
    selected_demo_data() %>% select(15:17) %>%
      pivot_longer(cols = everything(), names_to = "Education", values_to = "Share") %>%
      mutate(Education = factor(Education, ordered = TRUE, levels = c("Bachelor's or above", "Some college",
                                                                      "High school or below")),
             dummy = as.character(row_number()))
  })
  
  output$description <- renderText(
    "This dashboard tracks the occupational and demographic breakdown of each U.S. state above a given income threshold:
    1. The first tab displays the distribution of selected occupations across the income distribution.
    2. The second tab shows the median incomes and workforce share of selected occupations above the income threshold.
    3. The third tab illustrates the sex, age, race, and education composition of all workers above the income threshold."
  )
  output$plot_dist <- renderPlot(
    ggplot(selected_occ_data(), aes(fill = Occupation, x = percentile, y = share_quantile)) +
      geom_bar(position = "fill", stat = "identity", just = 1) +
      
      geom_vline(xintercept = as.numeric(input$income_percent), linetype="solid", size=0.8) +
      geom_text(aes(x = as.numeric(input$income_percent) - 0.02, y = 1.05,
                    label = paste0(as.character(as.numeric(input$income_percent) * 100), "%")),
                stat = "unique", size = 3) +
      geom_rect(xmin = 0, xmax = as.numeric(input$income_percent), ymin = 0, ymax = 1,
                fill = "grey", alpha = 0.01) +
      
      ylab("Share of Workers in the Income Bin") +
      xlab("Weighted Income Percentile") + theme_bw() +
      scale_fill_manual(values = eig_palette((length(input$occupation_selector) + 1), eig_colors))
  )
  
  output$tbl_inc <- renderTable(
    excluded_occ_income()
  )
  
  output$plots_dem <- renderPlot({
    plot_sex <- ggplot(selected_sex(), aes(x = Share, y = Sex, fill = dummy)) +
      geom_bar(stat = "identity") + geom_text(aes(label = percent(Share, accuracy = 0.1)), hjust = -0.02) +
      theme_bw() + scale_fill_manual(values = eig_palette(nrow(selected_sex()), eig_colors)) +
      theme(legend.position = "none") + xlim(0,1)
    
    plot_age <- ggplot(selected_age(), aes(x = Share, y = Age, fill = dummy)) +
      geom_bar(stat = "identity") + geom_text(aes(label = percent(Share, accuracy = 0.1)), hjust = -0.02) +
      theme_bw() + scale_fill_manual(values = eig_palette(nrow(selected_age()), eig_colors)) +
      theme(legend.position = "none") + xlim(0,1)
    
    plot_race <- ggplot(selected_race(), aes(x = Share, y = Race, fill = dummy)) +
      geom_bar(stat = "identity") + geom_text(aes(label = percent(Share, accuracy = 0.1)), hjust = -0.02) +
      theme_bw() + scale_fill_manual(values = eig_palette(nrow(selected_race()), eig_colors)) +
      theme(legend.position = "none") + xlim(0,1)
    
    plot_edu <- ggplot(selected_edu(), aes(x = Share, y = Education, fill = dummy)) +
      geom_bar(stat = "identity") + geom_text(aes(label = percent(Share, accuracy = 0.1)), hjust = -0.02) +
      theme_bw() + scale_fill_manual(values = eig_palette(nrow(selected_edu()), eig_colors)) +
      theme(legend.position = "none") + xlim(0,1)
    
    plot_grid(plot_sex, plot_age, plot_race, plot_edu, ncol = 1, align = "v", axis = "lr")
  })
}

shinyApp(ui, server)

