# A study on the effects of income cutoffs in noncompete bans

Data: 2023 5-year American Community Survey from IPUMS ([link](https://usa.ipums.org/usa-action/variables/group))

Data codebook: Attached in the repository

Usage:
1. Download 2023 5-year ACS with the variables specified in the codebook
2. Run 02. Income Dashboard Data Cleaning.R to generate the cleaned and summarised data for the R Shiny application
3. Run app.R under the income_dashboard to check if the app builds locally without issues
4. Deploy the app to [shinyapps.io](shinyapps.io) by entering `rsconnect::deployApp(PATH_TO_INCOME_DASHBOARD)` in console
