# Project: Analyzing the effect of income cutoffs in noncompete bans
# File Description: R Shiny income breakdown dashboard

# last update: 4/3/2025 by Jiaxin He

# remove dependencies
rm(list = ls())

# restart R session, run if shinyapps.io deployment gives an error
# .rs.restartR()

###########################
###   Load Packages     ###
###########################
library(scales)
library(tidyr)
library(dplyr)
library(Hmisc)
library(grattan)
library(tidycensus)
library(bslib)
library(ggplot2)
library(dichromat)
library(cowplot)

# R Shiny
library(shiny)
library(rsconnect)
rsconnect::setAccountInfo(name='economicinnovationgroup',
                          token='1E424A49864E72123BE5CAA19E6D2274',
                          secret='/OJ/Oy/GW2sk6ibHJt4JgoqzB80U03mcEyFJn0ev')
#################
### Load Data ###
#################
# Query state fips-abbreviation crosswalk
state_codes <- fips_codes %>% select(state_code, state) %>%
  distinct(state_code, .keep_all = TRUE) %>%
  mutate(state_code = as.numeric((state_code))) %>%
  filter(state_code <= 56)

# Load in cleaned data
# Change the path to "../data_cleaned" if running the app locally
# DO NOT USE SETWD IN CODE IF DEPLOYING TO SHINYAPPS.IO
hist_occ_data <- read.csv(file.path("data_cleaned", "hist_occ_data.csv")) %>% select(-X)
occ_income_summary <- read.csv(file.path("data_cleaned", "occ_income_summary.csv")) %>% select(-X)
demo_summary <- read.csv(file.path("data_cleaned", "demo_summary.csv")) %>% select(-X)

# Fix the column names
colnames(demo_summary) <- c("state", "income_cutoff", "F", "M", "16-24", "25-34", "35-44", "45-64", "65 and above",
                            "African American", "Asian and Pacific Islander", "Multi-racial", 
                            "Native American", "Other race", "White", "Bachelor's or above",
                            "High school or below", "Some college", "quantile")

# Make median incomes integers
occ_income_summary <- occ_income_summary %>% mutate(median_income = as.integer(median_income))

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

######################
### Build Shiny UI ###
######################

# Define EIG color palette
eig_colors <- c("#b3d6dd", "#79c5fd", "#176F96", "#234f8b", 	# EIG blue colors
                "#008080", "#5e9c86", "#1a654d", "#044140",	  # EIG green colors
                "#feecd6", "#f0b799", "#da9969", "#e1ad28")		# EIG beige, red, and yellow

# EIG palette selector
eig_palette <- function(n_colors, input_palette){
  if(n_colors <= length(input_palette)){
    return(input_palette[floor(seq(from = 1, to = length(input_palette), length.out = n_colors))])
  }else{
    return(colorRampPalette(input_palette)(n_colors))
  }
}

ui <- page_sidebar(
  tags$head(
  tags$link(
      href = "https://fonts.googleapis.com/css2?family=Source+Serif+Pro:wght@400;700&display=swap",
      rel = "stylesheet"
    ),
  tags$style(HTML("
      h1 {
        font-family: 'Source Serif Pro', serif;
        font-weight: bold;
        margin: 0;
        font-size: 2em;
      }
      .header-container {
        display: flex;
        align-items: center;
        background-color: #044140;
        padding: 20px;
        color: white;
        justify-content: center;
      }
      .header-logo {
        height: 60px;
        margin-right: 20px;
      }
      .card-header-tabs>li>a{
        color: #1a654d;
      }
      .card-header-tabs>li>a:hover{
        color: #5e9c86;
      }
      .nav-tabs>li>a{
        color: #1a654d;
      }
      .nav-tabs>li>a:hover{
        color: #5e9c86;
      }
      #description { font-size: 13px; }
      .selectize-input { font-size: 75%; }
      .selectize-dropdown { font-size: 75%; }
      .income_percent { display: flex; justify-content: center; margin: auto; width: 80%; }
      .irs-from, .irs-to, .irs-min, .irs-max { visibility: hidden !important;}
    "))
  ),
  
  ## Title ##
  div(
    class = "header-container",
    
    # Logo to left of the title
    img(src = "EIG_reverse.png", alt = "EIG Logo", class = "header-logo"),
    
    # Title with bold font
    h1("State Workforce Profile by Income Threshold")
  ),

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
    
    ### Income percentile selector ###
    sliderInput("income_percent", label = NULL, min = 0, max = 1, value = 0.95, step = 0.025),
    
    width = validateCssUnit(250)
  ),
  
  navset_card_tab(
    ### Plot with Income Percentile Slider ###
    nav_panel("Income Distribution", plotOutput("plot_dist")),
    
    ### Median Incomes per Category above Wage Cap ###
    nav_panel("Occupation Breakdown above Threshold", tableOutput("tbl_inc")),
    
    ### Demographic Characteristics of Those above Wage Cap ###
    nav_panel("Demographic Breakdown above Threshold", plotOutput("plots_dem"))
  )
)

##########################
### Build Shiny Server ###
##########################

server <- function(input, output) {
  # Filter by the state selected by the user and condense occupation categories
  # to only those selected by the user
  selected_occ_data <- reactive({
    hist_occ_data %>% filter(state == input$state_selector) %>%
      mutate(Occupation = case_when(occ_category %in% input$occupation_selector ~ occ_category,
                                    TRUE ~ "Other Occupations")) %>%
      mutate(Occupation = factor(Occupation, ordered = TRUE,
                                 levels = c(input$occupation_selector, "Other Occupations")))
  })
  
  # Filter by state, quantile, and occupations selected
  excluded_occ_income <- reactive({
    occ_income_summary %>% ungroup() %>%
      filter(state == input$state_selector,
             occ_category %in% input$occupation_selector,
             quantile == as.integer(as.numeric(input$income_percent)*40) + 1) %>%
      select(occ_category, median_income, share_quantile) %>%
      mutate(share_quantile = percent(share_quantile, accuracy = 0.1)) %>%
      rename(Occupations = occ_category,
             "Median Income above Income Threshold" = median_income,
             "Share of Workers above Income Threshold" = share_quantile)
  })
  
  selected_demo_data <- reactive({
    demo_summary %>% filter(state == input$state_selector,
                            quantile == as.integer(as.numeric(input$income_percent)*40) + 1)
  })
  selected_income_cutoff <- reactive({
    demo_summary %>% filter(state == input$state_selector,
                            quantile == as.integer(as.numeric(input$income_percent)*40)) %>%
      .$income_cutoff})
  selected_sex <- reactive({
    selected_demo_data() %>% select(M, `F`) %>%
      pivot_longer(cols = everything(), names_to = "Sex", values_to = "Share") %>%
      mutate(Sex = factor(Sex, ordered = TRUE, levels = c("F", "M")),
        dummy = as.character(row_number()))
  })
  selected_age <- reactive({
    selected_demo_data() %>% select(5:9) %>%
      pivot_longer(cols = everything(), names_to = "Age", values_to = "Share") %>%
      mutate(Age = factor(Age, ordered = TRUE, levels = c("65 and above", "45-64", "35-44", "25-34", "16-24")),
             dummy = as.character(row_number()))
  })
  selected_race <- reactive({
    selected_demo_data() %>% select(10:15) %>%
      pivot_longer(cols = everything(), names_to = "Race", values_to = "Share") %>%
      mutate(Race = factor(Race, ordered = TRUE, levels = c("Native American", "Other race", "Multi-racial",
                                                            "Asian and Pacific Islander", "African American", "White")),
             dummy = as.character(row_number()))
  })
  selected_edu <- reactive({
    selected_demo_data() %>% select(16:18) %>%
      pivot_longer(cols = everything(), names_to = "Education", values_to = "Share") %>%
      mutate(Education = factor(Education, ordered = TRUE, levels = c("Bachelor's or above", "Some college",
                                                                      "High school or below")),
             dummy = as.character(row_number()))
  })
  
  income_cutoff <- reactive({})
  
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
      geom_text(aes(x = as.numeric(input$income_percent) + 0.03, y = 1.05,
                    label = paste0("$", label_comma(accuracy = 1)(selected_income_cutoff()))),
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

shinyApp(ui = ui, server = server)
