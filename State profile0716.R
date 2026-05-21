################################################################################
# Project : State Profile
# Author  : Jaeyeon(Jenny) Park (jpark36@worldbank.org)
# Last mod: July 21, 2025
################################################################################

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)   
library(readr)
library(htmltools)
library(readxl)
library(purrr)
library(shinyjs)
library(zip)

# ––– Load data –––

# Tab 1: Intro – State Profile Overview
intro_section1 <- readxl::read_excel("C:/Users/jaeye/Downloads/State profile/Intro/section1-30july.xlsx")

# Tab 2a: Labour Market 
labour_df <- read.csv("C:/Users/jaeye/Downloads/State profile/Labour/merged_labour_jqi_all.csv")
wage_quintile <- read.csv("C:/Users/jaeye/Downloads/State profile/Labour/merged_wage_quintile.csv") 

#Welfare--monetary#
# Tab 3a: Monetary Welfare
hces1     <- read_csv("C:/Users/jaeye/Downloads/State profile/Welfare/Graph1_hces_pov_all.csv")    
hces2  <- read_csv("C:/Users/jaeye/Downloads/State profile/Welfare/Graph3_hces_income_all.csv")  
hces3 <- read_csv("C:/Users/jaeye/Downloads/State profile/Welfare/Graph2_hces_welfare_all_2.csv") 


#Tab 3b: Welfare--nonmonetary#
nonmon_1 <- read_csv("C:/Users/jaeye/Downloads/State profile/Welfare/Graph1_nfhs_pov_all.csv")
nonmon_2 <- read_csv("C:/Users/jaeye/Downloads/State profile/Welfare/Graph2_nfhs_wealth_all.csv")
nonmon_3 <- read_csv("C:/Users/jaeye/Downloads/State profile/Welfare/Graph3_nfhs_components_all.csv")


#Schemes#
#Tab 4a: 
monetary <- read.csv("C:/Users/jaeye/Downloads/State profile/Schemes/hces2022_2011.csv")

#Tab 4b:
nonmonetary <- read.csv(  "C:/Users/jaeye/Downloads/State profile/Schemes/nfhs2015_2019_mpi.csv")

##################################################––– UI –––#################################################
ui <- tagList(
  useShinyjs(),
  
  
  # 1) <head> styling
  tags$head(
    tags$style(HTML("
      /* General Styling & Colors */
      body { font-family: 'Arial', sans-serif; } 
      .box-header .box-title {
        display: block;
        text-align: center;
        font-size: 20px; 
        color: #333; 
      }
      .skin-red .main-header .navbar,
      .skin-red .main-header .logo {
        background-color: #800000  !important; 
      }
      header.main-header, .navbar-static-top {
        border-bottom: none !important;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .main-header .navbar { min-height: 80px !important; } 
      .main-header .logo, .main-header .sidebar-toggle { height: 80px !important; line-height: 80px !important; }
      .main-sidebar { top: 0px !important; padding-top: 80px; } 
      .content-wrapper, .main-footer { margin-top: 0px !important; }
      .skin-red .main-header .navbar .sidebar-toggle {
        display: block !important; 
        color: white !important;
        font-size: 24px !important; 
        margin-left: 15px;
      }

      /* BUTTONS */
      .btn-download {
        background-color: #6C757D !important; 
        color: white !important;
        font-weight: bold;
        border: none;
        padding: 10px 20px; 
        border-radius: 5px; 
        white-space: nowrap;
        transition: background-color 0.3s ease;
      }
      .btn-download:hover {
        background-color: #5A6268 !important; 
      }
      .form-group {
        min-width: 0;
        margin-bottom: 15px; 
      }
      .well-panel {
        background-color: #f8f9fa;
        border: 1px solid #e9ecef;
        border-radius: 5px;
        padding: 15px;
        margin-top: 20px;
      }
      .well-panel p, .well-panel ul {
        font-size: 13px; 
        line-height: 1.6;
        color: #555;
      }
      .well-panel strong {
        color: #333;
      }
      .plot-title {
        font-size: 22px;
        font-weight: bold;
        text-align: center;
        margin-bottom: 10px;
        color: #333;
      }
      .plot-subtitle {
        font-size: 16px;
        text-align: center;
        margin-bottom: 20px;
        color: #666;
        font-style: italic;
      }
      .fixed-footer {
  position: fixed;
  bottom: 0;
  left: 0;
  width: 100%;
  background-color: #9b0000;
  color: white;
  text-align: center;
  padding: 3px 0;
  font-size: 13px;
  z-index: 1000;
}

    "))
  ),
  
  # 2) dashboard CSS, Header, Sidebar
  dashboardPage(
    skin = "red", 
    
    
    dashboardHeader(
      titleWidth = "100%",
      title = tags$div(
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%; padding-left: 20px; padding-right: 20px;",
        
        # HAMBURGER ICON BUTTON
        actionButton("sidebarToggle", label = NULL, icon = icon("bars"),
                     style = "color:white; background:none; border:none; font-size:24px; margin-right:10px;"),
        
        # TITLE
        tags$span("India: State Profiles", style = "font-size:30px; font-weight:bold; color:white;"),
        
        # LOGO RIGHT
        tags$img(src="white.png", height="50px", style="margin-left: auto; display: block;")
      )
    )
    ,
    dashboardSidebar(
      sidebarMenu(
        menuItem("State Profile Overview", tabName="tab1", icon=icon("dashboard")),
        menuItem("Labour Market", tabName="tab2", icon=icon("users"),
                 menuSubItem("Key Labour Indicators", tabName="tab2a"),
                 menuSubItem("Quality of Employment", tabName="tab2b"),
                 menuSubItem("Distribution of Real Wage", tabName="tab2c")
        ),
        
        menuItem("Welfare Indicators", icon=icon("chart-line"),
                 menuSubItem("Monetary Welfare", tabName="tab3a"),
                 menuSubItem("Non‑monetary Welfare", tabName="tab3b")
        ),
        menuItem("Access to Schemes", icon=icon("hands-helping"),
                 menuSubItem("By Monetary Welfare", tabName="tab4a"),
                 menuSubItem("By Non‑monetary Poverty", tabName="tab4b")
        ),
        menuItem("State Comparison", tabName="tab5", icon=icon("chart-bar"))
      ),
      tags$div(
        style = "position: absolute; bottom: 20px; left: 10px; width: calc(100% - 20px); text-align: center;", 
        tags$img(src = "IMG1.png", style = "width: 90%; height: auto; max-width: 180px;")
      )
    ),
    
    #––– Body –––
    dashboardBody(
      
      tags$head(
        tags$style(HTML("
    .skin-red .main-header .logo {
      background-color: #bc0000 !important;
    }
    .skin-red .main-header .navbar {
      background-color: #bc0000 !important;
    }
    .skin-red .main-header .navbar .sidebar-toggle {
      background-color: #bc0000 !important;
    }
  "))
      )
      ,
      
      tabItems(
        
        
        ####### Tab 1: Intro- State Profile Overview #######
        tabItem(tabName = "tab1",
                tags$h2("State Profile Overview", style = "font-weight:bold; color:navy; margin-bottom:20px;"),
                
                # Dropdown for State Selection
                fluidRow(
                  column(3,
                         selectInput("overview_state", "Select State:", 
                                     choices = c("National", sort(unique(intro_section1$State))))
                  )
                ),
                
                # GVA by Industry box
                fluidRow(
                  column(12,
                         box(width = 12,
                             title = tags$div(style = "text-align:center; font-weight:bold;", 
                                              "GROSS VALUE ADDED (GVA) BY INDUSTRY"),
                             
                             # Tile row
                             fluidRow(
                               column(4,
                                      valueBoxOutput("tile_gva_share", width = 12)
                               ),
                               column(4),
                               column(4,
                                      valueBoxOutput("tile_fhi_rank", width = 12)
                               )
                             ),
                             
                             # Stacked bar chart
                             plotlyOutput("gva_bar_chart", height = "400px"),
                             
                             # Note
                             tags$div(
                               class = "well-panel",
                               tags$p(tags$strong("Note: "),
                                      "This chart displays the sectoral distribution of GVA (Gross Value Added) across key industries including agriculture, manufacturing, construction, and services.")
                             )
                         )
                  )
                ),
                
                # Summary Table and Indicator Chart side-by-side
                fluidRow(
                  column(6,
                         box(width = 12,
                             title = tags$div(style = "text-align:center; font-weight:bold;", 
                                              "ECONOMIC INDICATORS SUMMARY TABLE"),
                             tableOutput("overview_summary_table")
                         )
                  ),
                  column(6,
                         box(width = 12,
                             title = tags$div(style = "text-align:center; font-weight:bold;", 
                                              "SELECTED ECONOMIC INDICATOR"),
                             plotlyOutput("overview_indicator_chart", height = "400px")
                         )
                  )
                ),
                
                # Final Note Box
                fluidRow(
                  column(12,
                         tags$div(class = "well-panel",
                                  tags$p(tags$strong("Note: "), 
                                         "The summary table and chart highlight key macroeconomic trends including per capita income, inflation, fiscal deficit, and growth rates. Data may be updated periodically based on availability.")
                         )
                  )
                )
        ),
        
        
        
        # #######TAB 2B#########
        tabItem(tabName = "tab2b",
                tags$h2("Quality of Employment", style = "font-weight:bold; color:navy; margin-bottom:20px;"),
                # Dropdowns for State & Sector
                fluidRow(
                  column(3,
                         selectInput("lab2b_state", "Select State:",
                                     choices = c("National" = "National", sort(unique(labour_df$statename))))
                  ),
                  column(3,
                         selectInput("lab2b_sector", "Select Sector:",
                                     choices = c("All" = 99, "Rural" = 0, "Urban" = 1))
                  )
                ),
                
                fluidRow(
                  column(
                    width = 12,
                    box(
                      width = 12,
                      style = "max-height: 1300px",  
                      title = tags$div(
                        style = "text-align:center; font-weight:bold;", 
                        "QUALITY OF EMPLOYMENT"
                      ),
                      # Year and Demographic selectors for QUALITY OF EMPLOYMENT
                      fluidRow(
                        column(3,
                               selectInput("lab_quality_year", "Select Year:",
                                           choices = sort(unique(labour_df$year)))
                        ),
                        column(4,
                               selectInput("lab_quality_demo", "Select Demographic:",
                                           choices = list(
                                             "All" = "dem",
                                             
                                             "Education" = list(
                                               "Below Primary"        = "edu_cat::below primary",
                                               "Primary and Middle"   = "edu_cat::primary and middle",
                                               "Secondary"            = "edu_cat::secondary",
                                               "Tertiary"             = "edu_cat::tertiary"
                                             ),
                                             
                                             "Gender" = list(
                                               "Male"   = "sex_cat::1",
                                               "Female" = "sex_cat::2"
                                             ),
                                             
                                             "Youth" = list(
                                               "Youth"     = "youth_cat::1",
                                               "Non-Youth" = "youth_cat::0"
                                             )
                                           )
                               )
                        )
                      ),
                      
                      
                      fluidRow(
                        column(6, plotlyOutput("emp_sector_chart")),
                        column(6, plotlyOutput("emp_type_chart"))
                      ),
                      
                      # Note between rows
                      div(style = "padding:10px 20px;",
                          wellPanel(
                            tags$p(
                              tags$strong("Note: "), 
                              "The indicator shows the quality of job. The JQI is constructed around four key components:",
                              tags$ol(
                                tags$li("Income Adequacy — whether an individual earns enough to keep an average family above $3.65 per day (2017 PPP)"),
                                tags$li("Employment Benefits — whether the job provides at least one benefit such as health insurance, pension, social security, or paid leave"),
                                tags$li("Job Stability — whether the individual has a written contract for their current employment"),
                                tags$li("Job Satisfaction — whether the individual holds a regular full-time job (48 weekly work hours) or a second job/part-time work totaling at least 40 hours per week, with no desire to work additional hours")
                              ),
                              "Only calculated for salaried and casual workers."
                            )
                          )
                          
                      ),
                      
                      # Row 2: Graph 3 + Graph 4
                      fluidRow(
                        column(6, plotlyOutput("job_quality_chart")),
                        column(6, plotlyOutput("earning_poverty_chart"))
                      ),
                      
                      # Note between rows
                      div(style = "padding:10px 20px;",
                          wellPanel(
                            tags$p(
                              tags$strong("Note: "), 
                              "Earning poverty is the income sufficiency component of the job quality index, which captures whether job income is enough to maintain a minimum standard of living among workers and their families. It is calculated over all workers, above the age of 14, in paid employment."
                            )
                          )
                          
                      )
                    )
                  )
                )),
        
        
        ####TAB 2C#####
        tabItem(tabName = "tab2c",
                tags$h2("Distribution of Real Wage", style = "font-weight:bold; color:navy; margin-bottom:20px;"),
                
                # Dropdowns for State & Sector
                fluidRow(
                  column(3,
                         selectInput("lab2c_state", "Select State:",
                                     choices = c("National" = "National", sort(unique(wage_quintile$statename))))
                  ),
                  column(3,
                         selectInput("lab2c_sector", "Select Sector:",
                                     choices = c("All" = 99, "Rural" = 0, "Urban" = 1))
                  )
                ),
                
                fluidRow(
                  column(
                    width = 12,
                    box(
                      width = 12,
                      title = tags$div(
                        style = "text-align:center; font-weight:bold;",
                        "DISTRIBUTION OF REAL WAGE"
                      ),
                      
                      # Year & Demographic selectors
                      fluidRow(
                        column(3,
                               selectInput("realincome_year", "Select Year:", choices = c(2017:2022))
                        ),
                        column(4,
                               selectInput("realincome_demo", "HH Demographics:",
                                           choices = list(
                                             "All Households" = "dem",
                                             
                                             "CWS Type" = list(
                                               "Labour Force Participation Rate" = "cws_type::1",
                                               "Worker Population Ratio"         = "cws_type::2",
                                               "Unemployment Rate"               = "cws_type::3"
                                             ),
                                             
                                             "Employment Type" = list(
                                               "Salaried"        = "emp_type::1",
                                               "Casual"          = "emp_type::2",
                                               "Self Employment" = "emp_type::3"
                                             ),
                                             
                                             "Gender" = list(
                                               "Male"   = "sex_cat::1",
                                               "Female" = "sex_cat::2"
                                             ),
                                             
                                             "Youth" = list(
                                               "Youth"     = "youth_cat::1",
                                               "Non-Youth" = "youth_cat::0"
                                             )
                                           )
                               )
                               
                        )
                      ),
                      
                      # Plot Output
                      plotlyOutput("real_income_bar"),
                      
                      # Note
                      tags$div(style = "padding: 10px; font-size: 14px;",
                               tags$em("Note: Real wage refers to average daily earnings in INR (adjusted for inflation and household size) by economic quintile, including all employment types."))
                    )
                  )
                )
                
        ),
        
        
        
        
        
        ####### Tab 3a: Monetary Welfare #######
        tabItem(tabName = "tab3a",
                
                # — Shared State & Sector selector row —
                fluidRow(
                  column(3,
                         selectInput("tab3a_state", "State:",
                                     choices = c("National", sort(unique(hces1$state))))
                  ),
                  column(3,
                         selectInput("tab3a_sector", "Sector:",
                                     choices = c("All" = 99, "Rural" = 0, "Urban" = 1))
                  ),
                  column(3, offset = 3, align = "right",
                         downloadButton("download_tab3a", "Download Data", class = "btn-download")
                  )
                  
                  
                ),
                
                # — First row (Graph 1 + Graph 2 side-by-side with equal height) —
                fluidRow(
                  column(
                    width = 6,
                    box(
                      width = 12,
                      style = "height: 750px;", 
                      title = tags$div(
                        style = "text-align:center; font-weight:bold;", 
                        "POVERTY HEADCOUNT RATE, USING INTERNATIONAL POVERTY LINES"
                      ),
                      fluidRow(
                        column(6,
                               selectInput("mw_demo", "HH Demographics:",
                                           choices = list( "All Households" = "dem",
                                                           "Age classification of Household head" = list(
                                                             "15–29"  = "age_cat1",
                                                             "30–45"  = "age_cat2",
                                                             "46–64"  = "age_cat3",
                                                             "65+"    = "age_cat4"
                                                           ),
                                                           "Gender of Household head" = list(
                                                             "Male"   = "gender_cat1",
                                                             "Female" = "gender_cat2"
                                                           ),
                                                           "Education classification of Household head" = list(
                                                             "No education/Below primary"  = "edu_cat1",
                                                             "Primary completed"           = "edu_cat2",
                                                             "Middle completed"            = "edu_cat3",
                                                             "Secondary/Sr Secondary"      = "edu_cat4",
                                                             "Diploma / Graduate"          = "edu_cat5",
                                                             "Post Graduate and above"     = "edu_cat6"
                                                           ),
                                                           "Household Size classification" = list(
                                                             "One person"        = "hhsize_cat1",
                                                             "2–3 persons"       = "hhsize_cat2",
                                                             "4–6 persons"       = "hhsize_cat3",
                                                             "7+ persons"        = "hhsize_cat4"
                                                           ),
                                                           "Owns Dwelling Unit" = list(
                                                             "Yes" = "owns_dwelling_yes",
                                                             "No"  = "owns_dwelling_no"
                                                           ),
                                                           "Land Ownership" = list(
                                                             "Yes" = "owns_land_yes",
                                                             "No"  = "owns_land_no"
                                                           )
                                           )
                               )
                        ),
                        column(6,
                               selectInput("mw_ppp", "PPP-2017/2021:",
                                           choices = c("2017", "2021"),
                                           selected = "2021")
                        )
                      ),
                      
                      plotlyOutput("welfare_mon1"),
                      tags$br(),
                      div("Headcount – % of people classified as poor",
                          style = "text-align:center; font-weight:bold; margin-top:14px;"),
                      tags$br(),
                      wellPanel(
                        tags$p(tags$strong("Note:"), "Classification of poor and non-poor is based on International Poverty Lines."),
                        tags$ul(
                          tags$li(tags$strong("International Poverty Lines based on 2017 PPPs:"), "$2.65/day for Extreme poverty, $3.65/day for LMIC, $6.85/day for UMIC"),
                          tags$li(tags$strong("International Poverty Lines based on 2021 PPPs:"), "$3.00/day for Extreme poverty, $4.20/day for LMIC, $8.30/day for UMIC")
                        )
                      )
                    )
                  ),
                  
                  column(
                    width = 6,
                    box(
                      width = 12,
                      style = "height: 750px;", 
                      title = tags$div(
                        style = "text-align:center; font-weight:bold;", 
                        "MEDIAN REAL WELFARE AGGREGATE, by QUINTILE (INR, 2021 prices)"
                      ),
                      fluidRow(
                        column(12,
                               selectInput("mon2_demo", "HH Demographics:",
                                           choices = list( "All Households" = "dem",
                                                           "Age classification of Household head" = list(
                                                             "15–29" = "age_cat::15 to 29",
                                                             "30–45" = "age_cat::30 to 45",
                                                             "46–64" = "age_cat::46 to 64",
                                                             "65+"   = "age_cat::65+"
                                                           ),
                                                           "Gender of Household head" = list(
                                                             "Male"   = "gen_cat::1",
                                                             "Female" = "gen_cat::2"
                                                           ),
                                                           "Education classification of Household head" = list(
                                                             "No education" = "edu_cat::No education",
                                                             "Primary" = "edu_cat::Primary",
                                                             "Secondary" = "edu_cat::Secondary",
                                                             "Higher Secondary" = "edu_cat::Higher Secondary",
                                                             "Diploma/Graduate" = "edu_cat::Diploma/Graduate",
                                                             "Post-graduate" = "edu_cat::Post-graduate"
                                                           ),
                                                           "Household Size classification" = list(
                                                             "1–2 persons" = "hhsize_cat::1 to 2 members",
                                                             "3–4 persons" = "hhsize_cat::3 to 4 members",
                                                             "5–6 persons" = "hhsize_cat::5 to 6 members",
                                                             "7+ persons"  = "hhsize_cat::7+ members"
                                                           ),
                                                           "Owns Dwelling Unit" = list(
                                                             "Yes" = "owns_dwelling::1",
                                                             "No"  = "owns_dwelling::0"
                                                           )
                                                           ,
                                                           "Land Ownership" = list(
                                                             "Yes" = "owns_land::1",
                                                             "No"  = "owns_land::0"
                                                           )
                                           )
                               )
                        )
                      ),
                      
                      plotlyOutput("welfare_mon2"),
                      tags$br(),
                      div("Quintile Based on Real Welfare Aggregate",
                          style = "text-align:center; font-weight:bold; margin-top:14px;"),
                      tags$br(),
                      wellPanel(
                        tags$p(
                          tags$strong("Note: "),
                          "Real welfare is measured using a consumption aggregate that includes food and non-food non-durable monthly expenditures. This aggregate is adjusted for spatial and temporal price differences and expressed in 2021 prices (corresponding to the latest year for PPPs). ",
                          "Households are divided into 5 equal quintiles based on the welfare aggregate ranging from Bottom 20% (Q1) to Top 20% (Q5). ",
                          "In some cases, fewer than five quintiles may be displayed due to limited variation in the welfare distribution or low sample size within the subgroup. ",
                          "For further details on construction, please refer to ",
                          tags$a(
                            href   = "http://documents.worldbank.org/curated/en/099060325033540333",
                            target = "_blank",
                            "India – Trends in Poverty from 2011-2012 to 2022-2023: Methodology Note (English)"
                          ),
                          "."
                        )
                      )
                      
                    )
                  )
                  
                ),
                
                # --- Tab 3a: Monetary Welfare (Second Row – Graph 3 and Graph 4) ---
                # Graphs 3 and 4: Sources of Income (Poor and Non-Poor)
                fluidRow(
                  column(12,
                         box(width = 12,
                             title = tags$div(
                               style = "text-align:center; font-weight:bold;", "INCOME PROFILE OF POOR AND NON-POOR HOUSEHOLD"),
                             
                             # Dropdowns: Year, Demographic
                             fluidRow(
                               
                               column(4,
                                      selectInput("inc_year", "Year:",
                                                  choices = c("2011", "2022"))
                               ),
                               column(4,
                                      selectInput("inc_demo", "HH Demographics (up to 3):",
                                                  choices = list( "All Households" = "dem",
                                                                  "Age classification of Household head" = list(
                                                                    
                                                                    "15–29"  = "age_cat1",
                                                                    "30–45"  = "age_cat2",
                                                                    "46–64"  = "age_cat3",
                                                                    "65+"    = "age_cat4"
                                                                  ),
                                                                  "Gender of Household head" = list(
                                                                    
                                                                    "Male"   = "gender_cat1",
                                                                    "Female" = "gender_cat2"
                                                                  ),
                                                                  "Education classification of Household head" = list(
                                                                    
                                                                    "No education/Below primary"  = "edu_cat1",
                                                                    "Primary completed"           = "edu_cat2",
                                                                    "Middle completed"            = "edu_cat3",
                                                                    "Secondary/Sr Secondary"      = "edu_cat4",
                                                                    "Diploma / Graduate"          = "edu_cat5",
                                                                    "Post Graduate and above"     = "edu_cat6"
                                                                  ),
                                                                  "Household Size classification" = list(
                                                                    
                                                                    "One person"        = "hhsize_cat1",
                                                                    "2–3 persons"       = "hhsize_cat2",
                                                                    "4–6 persons"       = "hhsize_cat3",
                                                                    "7+ persons"        = "hhsize_cat4"
                                                                  ),
                                                                  "Owns Dwelling Unit" = list(
                                                                    "Yes" = "owns_dwelling_yes",
                                                                    "No"  = "owns_dwelling_no"
                                                                  ),
                                                                  "Land Ownership" = list(
                                                                    
                                                                    "Yes" = "owns_land_yes",
                                                                    "No"  = "owns_land_no"
                                                                  )
                                                                  
                                                  ),
                                                  multiple = TRUE,
                                                  selectize = TRUE
                                      )
                                      
                               )
                             ),
                             
                             # Graphs side-by-side
                             fluidRow(
                               column(6,
                                      box(width = 12,
                                          title = tags$div(
                                            style = "text-align:center; font-weight:bold;",
                                            "Sources of Income across poor households"
                                          ),
                                          plotlyOutput("welfare_mon3")
                                      )),
                               column(6,
                                      box(width = 12,
                                          title = tags$div(
                                            style = "text-align:center; font-weight:bold;",
                                            "Sources of Income across non-poor households"
                                          ),
                                          plotlyOutput("welfare_mon4"),
                                          
                                      ))
                             )
                             
                             ,
                             
                             tags$br(),
                             # Notes
                             wellPanel(
                               HTML(paste0(
                                 "<p><strong>• Note 1:</strong> Households are classified using the LMIC Poverty line of $4.20 per day (2021 PPP).</p>",
                                 "<p><strong>• Note 2:</strong> Sources of Income – Industry of activity which fetched the maximum earnings to the household during the last 365 days preceding the date of survey.</p>",
                                 "<p><strong>• Note 3:</strong></p>",
                                 "<p style='margin-left:20px'><strong>◦ a.</strong> Self-employed: Persons who operate their own farm or non-farm enterprises or are engaged independently in a profession or trade on own account or with one or a few partners.</p>",
                                 "<p style='margin-left:20px'><strong>◦ b.</strong> Salaried: Persons working in others’ farm or non-farm enterprises (both household and non-household) and getting in return salary or wages on a regular basis (and not based on daily or periodic renewal of work contract).</p>",
                                 "<p style='margin-left:20px'><strong>◦ c.</strong> Casual: A person casually engaged in other farm or non-farm enterprises (both household and non-household including in public works) and getting in return wage according to the terms of the daily or periodic work contract.</p>"
                               ))
                             ),
                             tags$br(),
                             wellPanel(
                               style = "background-color: #f0f4f8;",
                               tags$h5(tags$strong("Note on Demographics:")),
                               tags$ol(
                                 tags$li("Gender of Household head: Male includes transgenders"),
                                 tags$li("Education classification of Household head: Education divided into four groups – No education / Below primary, Primary, Middle, Secondary"),
                                 tags$li("Age classification of Household head: Age divided into four groups – Between age 15–29, Between age 30–45, Between age 46–64, 65 and above"),
                                 tags$li("Household Size classification: 4 groups – One person, Between 2–3 persons, Between 4–6 persons, more than 7 persons"),
                                 tags$li("Owns Agricultural Land: Household owns land usable for agriculture"),
                                 tags$li("Owns House: Household owns house")
                               )
                             )
                         )
                  )
                ),
                
                # Source Note
                fluidRow(
                  column(12, align = "right",
                         tags$em("Source: Data on household consumption expenditure across categories is sourced from NSS 2011 and HCES 2022."))
                )
                
        ),
        
        ####### Tab 3b: Non‑monetary Welfare #######
        tabItem(tabName = "tab3b",
                
                # — State selector row —
                fluidRow(
                  column(3,
                         selectInput("tab3b_state", "State:",
                                     choices = c("National", sort(unique(nonmon_3$StateName))))
                         
                  ),
                  column(3,
                         selectInput("tab3b_sector", "Sector:",
                                     choices = c("All" = 99, "Rural" = 0, "Urban" = 1)
                         )
                  ),
                  column(3, offset = 3, align = "right",
                         downloadButton("download_tab3b", "Download Data", class = "btn-download")
                  )
                ),
                
                # — First row: Graph 1 & Graph 2 —
                fluidRow(
                  ## Graph 1
                  column(6,
                         box(width = 12,height = 800,
                             title = tags$div(
                               style = "text-align:center; font-weight:bold;", 
                               "Incidence of Non-Monetary Poverty"
                             ),
                             
                             # Combined HH Demographics dropdown (like Tab 3a)
                             selectInput("nmw_demo_combined", "HH Demographics:",
                                         choices = list( "All Households" = "dem",
                                                         "Household Size" = list(
                                                           "1 person" = "hhsize_cat::1",
                                                           "2 to 3 persons" = "hhsize_cat::2 to 3",
                                                           "4 to 6 persons" = "hhsize_cat::4 to 6",
                                                           "7+ persons" = "hhsize_cat::7+"
                                                         ),
                                                         "Education Group" = list(
                                                           "No education / Preschool" = "edu_group::No education / Preschool",
                                                           "Primary" = "edu_group::Primary",
                                                           "Secondary" = "edu_group::Secondary",
                                                           "Higher" = "edu_group::Higher"
                                                         ),
                                                         "Gender of Head" = list(
                                                           "Male" = "head_gender::1",
                                                           "Female" = "head_gender::2"
                                                         ),
                                                         "Age of Head" = list(
                                                           "15 to 29" = "head_age_cat::15 to 29",
                                                           "30 to 45" = "head_age_cat::30 to 45",
                                                           "46 to 64" = "head_age_cat::46 to 64",
                                                           "65+" = "head_age_cat::65+"
                                                         ),
                                                         "Owns Agricultural Land" = list(
                                                           "Yes" = "agri_land::1",
                                                           "No" = "agri_land::0"
                                                         )
                                                         ,
                                                         
                                                         "Own House" = list(
                                                           "Yes" = "own_house::1",
                                                           "No" = "own_house::0"
                                                         )
                                         )
                             ),
                             
                             plotlyOutput("nonmon_graph1"),
                             
                             tags$div(
                               style = "text-align:center; margin-top:10px; font-style:italic;font-weight:bold; font-size:14px;",
                               "Headcount – % of people classified as multidimensionally poor"
                             ),
                             tags$br(),
                             wellPanel(
                               tags$p(
                                 tags$strong("Note: "),
                                 "Non-monetary poverty is measured using the Multidimensional Poverty Index (MPI), a composite index based on 10 indicators. 
                                 The indicators measure household deprivation across three areas: Health, Education, and Living Standards (see note below for details). 
                                 A household is classified as being in non-monetary poverty if it is deprived in at least one-third (33%) of the weighted indicators (MPI_33%). 
                                 It is classified as being in extreme non-monetary poverty if deprived in at least half (50%) of the weighted indicators (MPI_50%)"
                               )
                             )
                         )
                  ),
                  
                  ## Graph 2: Share of Households in Non-Monetary Poverty, By Wealth Quintiles
                  column(6,
                         box(width = 12,height = 800,
                             title = tags$div(
                               style = "text-align:center; font-weight:bold;", 
                               "Share of Households in Non-Monetary Poverty, By Wealth Quintiles"
                             ),
                             
                             # Combined HH Demographics dropdown (like Tab 3a)
                             selectInput("nmw_demo_combined2", "HH Demographics:",
                                         choices = list( "All Households" = "dem",
                                                         "Household Size" = list(
                                                           "1 person" = "hhsize_cat::1",
                                                           "2 to 3 persons" = "hhsize_cat::2 to 3",
                                                           "4 to 6 persons" = "hhsize_cat::4 to 6",
                                                           "7+ persons" = "hhsize_cat::7+"
                                                         ),
                                                         "Education Group" = list(
                                                           "No education / Preschool" = "edu_group::No education / Preschool",
                                                           "Primary" = "edu_group::Primary",
                                                           "Secondary" = "edu_group::Secondary",
                                                           "Higher" = "edu_group::Higher"
                                                         ),
                                                         "Gender of Head" = list(
                                                           "Male" = "head_gender::1",
                                                           "Female" = "head_gender::2"
                                                         ),
                                                         "Age of Head" = list(
                                                           "15 to 29" = "head_age_cat::15 to 29",
                                                           "30 to 45" = "head_age_cat::30 to 45",
                                                           "46 to 64" = "head_age_cat::46 to 64",
                                                           "65+" = "head_age_cat::65+"
                                                         ),
                                                         "Owns Agricultural Land" = list(
                                                           "Yes" = "agri_land::1",
                                                           "No" = "agri_land::0"
                                                         )
                                                         
                                                         ,
                                                         "Own House" = list(
                                                           "Yes" = "own_house::1",
                                                           "No" = "own_house::0"
                                                         )
                                         )
                             ),
                             
                             plotlyOutput("nonmon_graph2"),
                             
                             tags$div(
                               style = "text-align:center; margin-top:10px; font-style:italic;font-weight:bold; font-size:14px;",
                               "% HHs in non-monetary poverty across wealth quintiles"
                             ),
                             tags$br(),
                             wellPanel(
                               tags$p(
                                 tags$strong("Note: "),
                                 "A household is identified as multidimensionally (non-monetary) poor if it is deprived in at least one-third (33%) of the weighted indicators (MPI_33). ",
                                 "The households are divided into 5 equal quintiles based on wealth index ranging from Bottom 20% (Q1) to the Wealthiest 20% (Q5)."
                               )
                             )
                             
                         )
                  )
                  ,
                  
                  
                  
                  ## Graph 3 and 4
                  fluidRow(
                    column(12,
                           box(width = 12,
                               title = tags$div(
                                 style = "text-align:center; font-weight:bold;", 
                                 "SOURCES OF NON-MONETARY DEPRIVATION"
                               ),
                               
                               # Controls: Year + Combined Demographics
                               fluidRow(
                                 column(3,
                                        selectInput("nmwcomp_year", "Year:",
                                                    choices = c("Round 4 (2015–16)" = 4, "Round 5 (2019–21)" = 5))
                                 ),
                                 column(6,
                                        selectInput("nmwcomp_demo_combined", "HH Demographics:",
                                                    choices = list( "All Households" = "dem",
                                                                    "Household Size" = list(
                                                                      "1 person" = "hhsize_cat::1",
                                                                      "2 to 3 persons" = "hhsize_cat::2 to 3",
                                                                      "4 to 6 persons" = "hhsize_cat::4 to 6",
                                                                      "7+ persons" = "hhsize_cat::7+"
                                                                    ),
                                                                    "Education Group" = list(
                                                                      "No education / Preschool" = "edu_group::No education / Preschool",
                                                                      "Primary" = "edu_group::Primary",
                                                                      "Secondary" = "edu_group::Secondary",
                                                                      "Higher" = "edu_group::Higher"
                                                                    ),
                                                                    "Gender of Head" = list(
                                                                      "Male" = "head_gender::1",
                                                                      "Female" = "head_gender::2"
                                                                    ),
                                                                    "Age of Head" = list(
                                                                      "15 to 29" = "head_age_cat::15 to 29",
                                                                      "30 to 45" = "head_age_cat::30 to 45",
                                                                      "46 to 64" = "head_age_cat::46 to 64",
                                                                      "65+" = "head_age_cat::65+"
                                                                    ),
                                                                    "Owns Agricultural Land" = list(
                                                                      "Yes" = "agri_land::1",
                                                                      "No" = "agri_land::0"
                                                                    )
                                                                    
                                                                    ,
                                                                    "Own House" = list(
                                                                      "Yes" = "own_house::1",
                                                                      "No" = "own_house::0"
                                                                    )
                                                    )
                                        )
                                 )
                               ),
                               
                               # Plots side-by-side
                               fluidRow(
                                 column(6,
                                        box(width = NULL, title = tags$div(
                                          style = "text-align:center; font-weight:bold;",
                                          "Poor Households"
                                        ),
                                        plotlyOutput("welfare_nonmon3_poor"))
                                 ),
                                 column(6,
                                        box(width = NULL, title = tags$div(
                                          style = "text-align:center; font-weight:bold;",
                                          "Non-Poor Households"
                                        ),
                                        plotlyOutput("welfare_nonmon3_nonpoor"))
                                 )
                               ),
                               
                               tags$br(),
                               wellPanel(
                                 HTML("
    <p><strong>Multidimensional deprivations</strong> are measured using an <strong>uncensored score</strong> based on 10 indicators grouped into three dimensions: <strong>Health</strong>, <strong>Education</strong>, and <strong>Living Standards</strong>. A household is classified as <strong>multidimensionally poor</strong> if it has a weighted deprivation score greater than 0.33 (on a scale from 0 to 1) across the 10 indicators.</p>

    <ul>
      <li><strong>Health indicators</strong> (each weight = 1/6):
        <ul>
          <li><em>Nutrition deprivation</em> – If any household member under 70 years is undernourished.</li>
          <li><em>Child mortality deprivation</em> – If a child under 18 has died in the household in the five years preceding the survey.</li>
        </ul>
      </li>

      <li><strong>Education indicators</strong> (each weight = 1/6):
        <ul>
          <li><em>Schooling deprivation</em> – If no household member aged 12+ has completed six years of schooling.</li>
          <li><em>School attendance deprivation</em> – If any child aged 6–14 is not attending school.</li>
        </ul>
      </li>

      <li><strong>Living Standards indicators</strong> (each weight = 1/18):
        <ul>
          <li><em>Cooking fuel deprivation</em> – If the household uses solid fuels (e.g., dung, firewood, charcoal).</li>
          <li><em>Sanitation deprivation</em> – If no sanitation facility, uses an unimproved one, or shares it.</li>
          <li><em>Drinking water deprivation</em> – If water is unsafe or the source is ≥30 minutes roundtrip.</li>
          <li><em>Electricity deprivation</em> – If the household has no electricity.</li>
          <li><em>Housing deprivation</em> – If housing materials for floor, roof, or walls are inadequate.</li>
          <li><em>Asset deprivation</em> – If the household does not own more than one of the following: radio, TV, telephone, computer, animal cart, bicycle, motorbike, refrigerator, car/truck.</li>
        </ul>
      </li>
    </ul>
  ")
                               ),
                               
                               tags$br(),
                               wellPanel(
                                 style = "background-color: #f0f4f8;",
                                 tags$h5(tags$strong("Note on Demographics:")),
                                 tags$ol(
                                   tags$li("Gender of Household head: Male includes transgenders"),
                                   tags$li("Education classification of Household head: Divided into 4 groups – No education / Below primary, Primary, Middle, Secondary"),
                                   tags$li("Age classification of Household head: 4 groups – Between age 15–29, Between age 30–45, Between age 46–64, 65 and above"),
                                   tags$li("Household Size classification: 4 groups – One person, Between 2–3 persons, Between 4–6 persons, More than 7 persons"),
                                   tags$li("Household owns land usable for agriculture")
                                 )
                               )                           
                               
                               
                           )
                    )
                  )
                  ,
                  # Source Note
                  fluidRow(
                    column(12, align = "right",
                           tags$em("Source: Calculations from NFHS 2015-16 and 2019-21."))
                  )
                )
        ),
        
        
        ####### Tab 4a: Monetary Poverty #######
        tabItem(tabName = "tab4a",
                tags$h2("Monetary Welfare",
                        style = "font-weight:bold; color:navy; margin-bottom:20px;"),
                
                # State & Sector selectors
                fluidRow(
                  column(3,
                         selectInput("mp_state",  "Select State:",
                                     choices = c("National" = "India", sort(setdiff(unique(monetary$state), "India"))))
                  ),
                  column(3,
                         selectInput("mp_sector", "Select Sector:",
                                     choices = c("All" = "all", "Rural" = "rural", "Urban" = "urban"))
                         
                  ),
                  column(4,
                         selectInput("mp_indicators", "Select up to 3 Indicators:",
                                     choices = list(
                                       "Food" = list(
                                         "PDS - Food Subsidy" = "food_pds_subs",
                                         "PMGKAY - Free Food" = "food_pds_free"
                                       ),
                                       "Household Utilities" = list(
                                         "Kerosene Subsidy" = "kerosene_pds_subs",
                                         "LPG Subsidy" = "lpg_subs",
                                         "Free Electricity" = "elec_free"
                                       ),
                                       "Durables" = list(
                                         "Any Free Durables" = "durables_free",
                                         "Free Laptop" = "laptop2",
                                         "Free Tablet" = "tablet2",
                                         "Free Mobile" = "mobile2",
                                         "Free Bicycle" = "bicycle2"
                                       ),
                                       "Education" = list(
                                         "Any free school items (Government)" = "gov_school_free",
                                         "Any free school items (Private)" = "pvt_school_free",
                                         "Fees reimbursement" = "fees_waived",
                                         "Free Books" = "books",
                                         "Free Stationary" = "stationary",
                                         "Free Clothing" = "clothing",
                                         "Free Schoolbag" = "schoolbag",
                                         "Free Footwear" = "footwear"
                                       ),
                                       "Health" = list(
                                         "PMJAY Beneficiary Coverage" = "pmjay_ben",
                                         "PMJAY Benefits Availed" = "pmjay_ben_avail"
                                       )
                                     ),
                                     multiple = TRUE,
                                     selectize = TRUE
                         )
                  ),
                  column(2, align = "right", style = "padding-top: 25px;",
                         downloadButton("download_tab4a", "Download Data", class = "btn-download"))
                  
                ),
                
                # Chart
                fluidRow(
                  column(12,
                         box(width = 12, title = tags$strong("SHARE OF INDIVIDUAL WITH ACCESS"),
                             plotlyOutput("monetary_bar"))
                  )
                ),
                
                fluidRow(
                  column(12,
                         box(
                           width = NULL,
                           status = "primary", 
                           solidHeader = TRUE,
                           div(
                             style = "font-weight: bold; font-size: 18px; color: black; margin-bottom: 5px;",
                             "Note on Indicator"
                           ),
                           "Bars compare Poor vs Non‑poor for each chosen indicator, across the five quintiles.",
                           br(), br(),
                           tags$table(
                             style = "width: 100%; font-size: 14px;",
                             tags$tbody(
                               tags$tr(
                                 tags$td(tags$b("PDS - Food Subsidy")), 
                                 tags$td("% HHs who received food items from PDS shops")
                               ),
                               tags$tr(
                                 tags$td(tags$b("PMGKAY - Free Food")), 
                                 tags$td("% HHs who received free food items from PDS shops")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Kerosene Subsidy")), 
                                 tags$td("% HHs procuring kerosene using ration card")
                               ),
                               tags$tr(
                                 tags$td(tags$b("LPG Subsidy")), 
                                 tags$td("% HHs receiving subsidy for LPG cylinder")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Free Electricity")), 
                                 tags$td("% HHs receiving free electricity")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Any Free Durables")), 
                                 tags$td("% HHs receiving any free durable goods")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Free Laptop")), 
                                 tags$td("% HHs receiving free laptop")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Free Tablet")), 
                                 tags$td("% HHs receiving free tablet")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Free Mobile")), 
                                 tags$td("% HHs receiving free mobile phone")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Free Bicycle")), 
                                 tags$td("% HHs receiving free bicycle")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Any type of free school items (Government)")), 
                                 tags$td("% HHs with members in Government school who received any type of free school items")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Any type of free school items (Private)")), 
                                 tags$td("% HHs with members in Private school who received any type of free school items")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Fee reimbursement")), 
                                 tags$td("% HHs receiving reimbursement/waiver from educational institution")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Free Books")), 
                                 tags$td("% HHs receiving free books")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Free Stationery")), 
                                 tags$td("% HHs receiving free stationery")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Free School Uniform")), 
                                 tags$td("% HHs receiving free school uniform")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Free School Bag")), 
                                 tags$td("% HHs receiving free schoolbag")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Free School Footwear")), 
                                 tags$td("% HHs receiving free school footwear")
                               ),
                               tags$tr(
                                 tags$td(tags$b("PMJAY Beneficiary Coverage")), 
                                 tags$td("% HHs with beneficiaries of PMJAY")
                               ),
                               tags$tr(
                                 tags$td(tags$b("PMJAY Benefits Availed")), 
                                 tags$td("% HHs which received medical benefits from PMJAY")
                               )
                             )
                           )
                         )
                  )
                )
                ,
                
                fluidRow(
                  column(12, align = "right",
                         tags$em("Source: Data on household consumption expenditure across categories is sourced from NSS 2011 and HCES 2022."))
                )
        ),
        
        
        
        
        ################################################################################################################
        ####### Tab 4b: Non‑monetary Poverty #######
        tabItem(tabName = "tab4b",
                tags$h2("Non‑monetary Poverty",
                        style = "font-weight:bold; color:navy; margin-bottom:20px;"),
                
                fluidRow(
                  column(3,
                         selectInput("nm_state", "Select State:",
                                     choices = c("National" = "India", sort(setdiff(unique(nonmonetary$state_numeric), "India"))))
                  ),
                  column(3,
                         selectInput("nm_sector", "Select Sector:",
                                     choices = c("All" = 99, "Rural" = 0, "Urban" = 1))
                  ),
                  column(4,
                         selectInput("nm_indicators", "Select up to 3 Indicators:",
                                     choices = list(
                                       "Access" = list(
                                         "BPL" = "bpl",
                                         "Aadhar" = "aadhar",
                                         "State/Central Health Insurance" = "health_insu_govt"
                                       ),
                                       "Women Health" = list(
                                         "Met Healthcare Worker" = "household_health_met",
                                         "Delivery Assistance" = "household_has_preg_fin",
                                         "Pregnancy Benefits" = "household_has_preg_benefits"
                                       ),
                                       "Child Health" = list(
                                         "Anganwadi Benefits" = "household_angan_benefits",
                                         "Anganwadi Immunization" = "household_angan_immun",
                                         "Anganwadi Early Childhood Care" = "household_angan_ecc"
                                       )
                                     ),
                                     multiple = TRUE,
                                     selectize = TRUE
                         )
                  ),
                  column(2, align = "right", style = "padding-top: 25px;",
                         downloadButton("download_tab4b", "Download Data", class = "btn-download"))
                  
                ),
                
                fluidRow(
                  column(6,
                         box(width = NULL, title = tags$strong("Poor Households"),
                             plotlyOutput("non_monetary_bar_poor"))
                  ),
                  column(6,
                         box(width = NULL, title = tags$strong("Non-poor Households"),
                             plotlyOutput("non_monetary_bar_nonpoor"))
                  )
                ),
                
                
                fluidRow(
                  column(12,
                         box(
                           width = NULL, 
                           status = "primary", 
                           solidHeader = TRUE,
                           div(
                             style = "font-weight: bold; font-size: 16px; color: navy; margin-bottom: 5px;",
                             "Note on Indicator"
                           ),
                           "The graphs display the share across poor and non-poor households. 
           Households are classified based on their non-monetary poverty score. 
           MPI poor households have a deprivation score equal to or above the multidimensional poverty 
           threshold of 0.33 (MPI – 33%), while non-poor households fall below this threshold.",
                           br(), br(),
                           tags$table(
                             style = "width: 100%; font-size: 14px;",
                             tags$tbody(
                               tags$tr(
                                 tags$td(tags$b("BPL")), 
                                 tags$td("% HHs with BPL (ration) card ownerships")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Aadhar")), 
                                 tags$td("% HHs with any member having an Aadhar card")
                               ),
                               tags$tr(
                                 tags$td(tags$b("State/Central Health Insurance")), 
                                 tags$td("% HHs with access to any state or centrally sponsored health insurance")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Met Healthcare Worker")), 
                                 tags$td("% HHs accessing Anganwadi center, ASHA or community health worker")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Pregnancy Benefits")), 
                                 tags$td("% HHs that received any pregnancy benefits from Anganwadi/ICDS centre")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Delivery Assistance")), 
                                 tags$td("% HHs that received financial assistance for delivery care")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Anganwadi Benefits")), 
                                 tags$td("% HHs that received any benefits for children from Anganwadi/ICDS centre")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Anganwadi Immunization")), 
                                 tags$td("% HHs that accessed Anganwadi/ICDS center for child immunization")
                               ),
                               tags$tr(
                                 tags$td(tags$b("Anganwadi Early Childhood Care")), 
                                 tags$td("% HHs that received early childhood care at Anganwadi/ICDS centre")
                               )
                             )
                           )
                         )
                  )
                )
                ,
                fluidRow(
                  column(12, align = "right",
                         tags$em("Data is sourced from National Family Health Surveys. Round 4 (conducted 2015 - 16) and Round 5 (conducted 2019 - 21) have been included in this analysis."))
                )
        )
      )
    )
  ),
  div(class = "fixed-footer",
      HTML('If you encounter any issues or have any questions, please feel free to reach out to 
          <a href="mailto:nkochhar@worldbank.org" style="color:white; text-decoration: underline;">nkochhar@worldbank.org</a> 
          / 
          <a href="mailto:India_POV_Economists@worldbankgroup.org" style="color:white; text-decoration: underline;">India_POV_Economists@worldbankgroup.org</a>.')
  )
  
)

##################################################––– Server –––#################################################
server <- function(input, output, session) {
  updateTabItems(session, "tabs", selected = "tab3a")
  
  observeEvent(input$sidebarToggle, {
    shinyjs::runjs("
    $('body').toggleClass('sidebar-collapse');
  ")
  })
  
  ###########################################################################################
  ####### Tab 1: INTRO #######
  # Filter data based on state
  selected_intro_data <- reactive({
    req(input$overview_state)
    
    selected_state <- if (input$overview_state == "National") "India" else input$overview_state
    
    intro_section1 %>% filter(State == selected_state)
  })
  
  
  # Tile 1: Real GVA as % of India GVA
  output$tile_gva_share <- renderValueBox({
    df <- selected_intro_data()
    req(nrow(df) > 0)
    
    valueBox(
      paste0(round(df$state_share_india, 1), "%"),
      "Real GVA as % of India GVA",
      icon = icon("percent"),
      color = NULL,
      href = NULL
    ) %>% tagAppendAttributes(style = "background-color:#cc3366; color:white;")  # darker pink
  })
  
  output$tile_fhi_rank <- renderValueBox({
    df <- selected_intro_data()
    req(nrow(df) > 0)
    
    valueBox(
      df$rank202223,
      "FHI Rank (2022–23)",
      icon = icon("trophy"),
      color = NULL,
      href = NULL
    ) %>% tagAppendAttributes(style = "background-color:#003366; color:white;")  # darker blue
  })
  
  # Bar chart: GVA by Industry
  output$gva_bar_chart <- renderPlotly({
    df <- selected_intro_data()
    req(nrow(df) > 0)
    
    plot_data <- data.frame(
      Sector = c("Agriculture", "Mining", "Manufacturing", "Construction", 
                 "Electricity", "Trade", "Services"),
      Value = c(
        df$argiperc,
        df$miningperc,
        df$manugva,
        df$consgva,
        df$elecgva,
        df$tradegva,
        df$servicesgva
      )
    ) %>%
      mutate(
        Value = round(Value * 100, 2),
        Tooltip = paste0(Sector, ": ", Value, "%")
      )
    
    plot_ly(
      data = plot_data,
      x = ~Sector,
      y = ~Value,
      type = 'bar',
      text = ~paste0(Value, "%"),
      textposition = "auto",
      hoverinfo = "text",
      hovertext = ~Tooltip,
      marker = list(color = c("#d31f11", "#ff6347", "#e89f00", "#008080", "#007191", "#00BF7D", "#4a2377"))
    ) %>%
      layout(
        barmode = 'stack',
        yaxis = list(title = "Share of GVA (%)", tickformat = ".2f", range = c(0, 100)),
        xaxis = list(title = "Sector", tickfont = list(size = 14), titlefont = list(size = 16)),
        font = list(size = 14)
      )
    
  })
  
  
  ###########################################################################################
  ####### Tab 2a: Labour Market #######
  
  output$labour_line <- renderPlotly({
    req(input$lab_state, input$lab_sector, input$lab_indicator, input$lab_demo)
    
    df <- labour_df %>%
      filter(
        (input$lab_state == "National" & level == "allindia") |
          (input$lab_state != "National" & statename == input$lab_state),
        if (input$lab_sector == 99) TRUE else as.numeric(urban) == input$lab_sector
      )
    
    
    if (input$lab_demo != "dem") {
      parts <- strsplit(input$lab_demo, "::")[[1]]
      col <- parts[1]
      val <- parts[2]
      if (!is.na(suppressWarnings(as.numeric(val)))) val <- as.numeric(val)
      df <- df %>% filter(.data[[col]] == val)
    } else {
      df <- df %>% filter(dem == "All")
    }
    
    df_plot <- df %>%
      select(year, value = all_of(input$lab_indicator)) %>%
      group_by(year) %>%
      summarise(value = mean(value, na.rm = TRUE) * 100, .groups = "drop") %>%
      mutate(tooltip = paste0("Year: ", year, "<br>Value: ", sprintf("%.2f%%", value)))
    
    plot_ly(df_plot,
            x = ~year,
            y = ~value,
            type = "scatter",
            mode = "lines+markers",
            text = ~tooltip,
            hoverinfo = "text",
            line = list(width = 3)
    ) %>%
      layout(
        xaxis = list(title = "Year", type = "category"),
        yaxis = list(    title = "",
                         ticksuffix = "%")
      )
  })
  
  
  
  ##########TAB 2b###
  get_labour_filtered_data <- reactive({
    req(input$lab2b_state, input$lab2b_sector, input$lab_quality_demo, input$lab_quality_year)
    
    df <- labour_df %>%
      filter(
        (input$lab2b_state == "National" & level == "allindia") |
          (input$lab2b_state != "National" & statename == input$lab2b_state),
        if (input$lab2b_sector == 99) TRUE else as.numeric(urban) == input$lab2b_sector,
        year == input$lab_quality_year
      )
    
    if (input$lab_quality_demo != "dem") {
      parts <- strsplit(input$lab_quality_demo, "::")[[1]]
      col <- parts[1]
      val <- if (col == "edu_cat") parts[2] else as.numeric(parts[2])
      df <- df %>% filter(.data[[col]] == val)
    } else {
      df <- df %>% filter(trimws(dem) == "All")
    }
    
    print(paste("Matched rows:", nrow(df)))  # Debug
    return(df)
  })
  
  ##########TAB 2B-1
  
  colors_sector <- c("#E41A1C", "#E6C200", "#00BF7D", "#007191", "#4a2377")
  
  output$emp_sector_chart <- renderPlotly({
    df <- get_labour_filtered_data()
    
    sector_data <- df %>%
      slice(1) %>% 
      select(ind_cws_1, ind_cws_2, ind_cws_3, ind_cws_4, ind_cws_5) %>%
      pivot_longer(cols = everything(), names_to = "sector", values_to = "value") %>%
      mutate(
        sector = recode(sector,
                        ind_cws_1 = "Agriculture",
                        ind_cws_2 = "Industries",
                        ind_cws_3 = "Services",
                        ind_cws_4 = "Unemployed",
                        ind_cws_5 = "Not in LF"),
        value = round(value * 100, 2),
        color = case_when(
          sector == "Agriculture" ~ "#E41A1C",
          sector == "Industries" ~ "#FFDB58",
          sector == "Services" ~ "#00BF7D",
          sector == "Unemployed" ~ "#007191",
          sector == "Not in LF" ~ "#4a2377"
        )
      )
    
    plot_ly(
      data = sector_data,
      x = ~sector,
      y = ~value,
      type = 'bar',
      text = ~paste0(value, "%"),
      textposition = 'auto',
      marker = list(color = ~color)
    ) %>%
      layout(
        title = "Employment Sector",
        yaxis = list(title = "Share (%)", range = c(0, 100)),
        xaxis = list(title = ""),
        showlegend = FALSE
      )
    
  })
  
  
  
  ##########TAB 2B-2 
  
  colors_type <- c("#4a2377", "#0d7d87", "#f55f74")
  
  output$emp_type_chart <- renderPlotly({
    df <- get_labour_filtered_data()
    
    type_data <- df %>%
      slice(1) %>%  
      select(salaried, casual, self_emp) %>%
      pivot_longer(everything(), names_to = "type", values_to = "value") %>%
      mutate(
        type = recode(type,
                      salaried = "Salaried",
                      casual = "Casual",
                      self_emp = "Self-employed"),
        value = round(value * 100, 2)
      )
    
    plot_ly(type_data, x = ~type, y = ~value, type = 'bar',
            text = ~paste0(value, "%"), textposition = 'auto',
            marker = list(color = colors_type)) %>%
      layout(title = "Employment Type", yaxis = list(title = "Share (%)", range = c(0, 100)))
  })
  
  
  
  
  ##########TAB 2B-3 
  colors_jqi <- c("#d31f11", "#007191")
  
  output$job_quality_chart <- renderPlotly({
    df <- get_labour_filtered_data()
    
    jq_data <- df %>%
      slice(1) %>%
      select(JQdim215, JQdim365) %>%
      pivot_longer(everything(), names_to = "index", values_to = "value") %>%
      mutate(
        index = recode(index,
                       JQdim215 = "JQI (215 Days)",
                       JQdim365 = "JQI (365 Days)"),
        value = round(value, 2)
      )
    
    plot_ly(jq_data, x = ~index, y = ~value, type = 'bar',
            text = ~value, textposition = 'auto',
            marker = list(color = colors_jqi)) %>%
      layout(title = "Job Quality Index", yaxis = list(title = "Score", range = c(0, 1)))
  })
  
  
  
  
  ##########TAB 2B-4
  colors_pov <- c("#d31f11", "#007191")
  
  output$earning_poverty_chart <- renderPlotly({
    df <- get_labour_filtered_data()
    
    pov_data <- df %>%
      slice(1) %>%
      select(POV_215, POV_365) %>%
      pivot_longer(everything(), names_to = "pov", values_to = "value") %>%
      mutate(
        pov = recode(pov,
                     POV_215 = "Below ₹215/day",
                     POV_365 = "Below ₹365/day"),
        value = round(value * 100, 2)
      )
    
    plot_ly(pov_data, x = ~pov, y = ~value, type = 'bar',
            text = ~paste0(value, "%"), textposition = 'auto',
            marker = list(color = colors_pov)) %>%
      layout(title = "Earning Poverty", yaxis = list(title = "Share (%)", range = c(0, 100)))
  })
  
  
  
  
  
  ##########TAB 2c: Distribution of real income
  #Colors
  bar_colors <- c("1" = "#E41A1C",    # Q1
                  "2" = "#FFDB58",    # Q2
                  "3" = "#f55f74",    # Q3
                  "4" = "#00BF7D",    # Q4
                  "5" = "#007191",    # Q5
                  "All" = "#4a2377")  # All
  
  output$real_income_bar <- renderPlotly({
    req(input$lab2c_state, input$lab2c_sector, input$realincome_year, input$realincome_demo)
    
    df <- wage_quintile %>%
      filter(
        (input$lab2c_state == "National" & level == "National") |
          (input$lab2c_state != "National" & statename == input$lab2c_state),
        if (input$lab2c_sector != 99) urban == input$lab2c_sector else TRUE,
        Year == input$realincome_year
      )
    
    if (input$realincome_demo != "dem") {
      parts <- strsplit(input$realincome_demo, "::")[[1]]
      col <- parts[1]
      val <- as.numeric(parts[2])
      df <- df %>% filter(.data[[col]] == val)
    }
    
    if (nrow(df) == 0) return(NULL)
    
    df_plot <- df %>%
      filter(!is.na(real_wage_23)) %>%
      mutate(
        quint_label = ifelse(quint_label == "Total", "All", quint_label),
        quint_label = factor(quint_label, levels = c("1", "2", "3", "4", "5", "All"))
      ) %>%
      group_by(quint_label) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        label = sprintf("₹%.2f", real_wage_23),
        color = bar_colors[as.character(quint_label)]
      )
    
    
    plot_ly(
      data = df_plot,
      x = ~quint_label,
      y = ~real_wage_23,
      type = "bar",
      text = ~paste0("Quintile: ", quint_label, "<br>Income: ", label),
      hoverinfo = "text",
      marker = list(color = ~color)
    ) %>%
      layout(
        yaxis = list(title = "Real Wage (INR/day)", tickformat = ".2f"),
        xaxis = list(title = "Quintile"),
        title = ""
      )
  })
  
  
  
  
  ###########################################################################################
  ####### Tab 3a: Monetary Welfare #######
  
  #DOWNLOAD FILES#
  
  output$download_tab3a <- downloadHandler(
    filename = function() {
      paste0("Monetary_Welfare_Data_", Sys.Date(), ".zip")
    },
    content = function(file) {
      tmpdir <- tempdir()
      
      file1 <- file.path(tmpdir, "Graph1_hces_pov_all.csv")
      file2 <- file.path(tmpdir, "Graph3_hces_income_all.csv")
      file3 <- file.path(tmpdir, "Graph2_hces_welfare_all_2.csv")
      
      readr::write_csv(hces1, file1)
      readr::write_csv(hces2, file2)
      readr::write_csv(hces3, file3)
      
      zip::zipr(zipfile = file, files = c(file1, file2, file3))
    },
    contentType = "application/zip" 
  )
  
  
  
  
  
  
  # === Graph 1: Poverty Headcount ===
  output$welfare_mon1 <- renderPlotly({
    req(input$tab3a_state, input$tab3a_sector, input$mw_demo, input$mw_ppp)
    
    # Base filtered data
    df <- hces1 %>%
      filter(
        (input$tab3a_state == "National" & level == "National") |
          (input$tab3a_state != "National" & state == input$tab3a_state),
        if (input$tab3a_sector != 99) sector2 == input$tab3a_sector else TRUE
      )
    
    
    # poverty lines to show
    if (input$mw_ppp == "2017") {
      poverty_vars <- c("2017ppp_poor_215", "2017ppp_poor_365", "2017ppp_poor_685")
      labels <- c("2017ppp_poor_215" = "$2.15", "2017ppp_poor_365" = "$3.65", "2017ppp_poor_685" = "$6.85")
    } else {
      poverty_vars <- c("2021ppp_poor_300", "2021ppp_poor_420", "2021ppp_poor_830")
      labels <- c("2021ppp_poor_300" = "$3.00", "2021ppp_poor_420" = "$4.20", "2021ppp_poor_830" = "$8.30")
    }
    
    # === MODE 1: binary column selected
    if (input$mw_demo == "dem") {
      # All households — no demographic filter 
      df <- df
    } else if (input$mw_demo %in% c("Age", "Gender", "Education", "HH Size", "Owns Land")) {
      df <- df %>% filter(dem == input$mw_demo)
    } else if (input$mw_demo == "owns_land_yes") {
      df <- df %>% filter(owns_land == 1)
    } else if (input$mw_demo == "owns_land_no") {
      df <- df %>% filter(owns_land == 0)
    } else if (input$mw_demo == "owns_dwelling_yes") {
      df <- df %>% filter(owns_dwelling == 1)
    } else if (input$mw_demo == "owns_dwelling_no") {
      df <- df %>% filter(owns_dwelling == 0)
      
    } else {
      df <- df %>% filter(.data[[input$mw_demo]] == 1)
    }
    
    
    # === Reshape for Plotting ===
    df_long <- df %>%
      distinct(year, .keep_all = TRUE) %>%
      select(year, all_of(poverty_vars)) %>%
      pivot_longer(cols = -year, names_to = "poverty_line", values_to = "headcount") %>%
      mutate(
        threshold = labels[poverty_line],
        headcount = headcount * 100,
        headcount_label = sprintf("%.2f", headcount)
      )
    
    
    
    #years
    df_long <- df_long %>%
      filter(year %in% c(2011, 2022)) %>%
      mutate(year = factor(year, levels = c(2011, 2022), labels = c("2011-12", "2022-23")))
    
    poverty_colors <- c(
      "$2.15" = "#f47a00", 
      "$3.65" = "#E41A1C",  
      "$6.85" = "#00BF7D", 
      "$3.00" = "#f47a00",
      "$4.20" = "#E41A1C",
      "$8.30" = "#00BF7D"
    )
    
    
    plot_ly(
      df_long,
      x = ~year,  
      y = ~headcount,
      color = ~threshold,
      colors =poverty_colors,
      type = "scatter",
      mode = "lines+markers",
      line = list(width = 4),       
      marker = list(size = 10), 
      hovertext = ~paste0("Year: ", year, "<br>Poverty Line: ", threshold, "<br>Headcount: ", headcount_label, "%"),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Year", type = "category", categoryorder = "array", categoryarray = c("2011-12", "2022-23")),
        yaxis = list(title = "", range = c(0, 100), tickformat = ".2f"),
        legend = list(title = list(text = paste0("Poverty Line")),
                      orientation = "h",
                      x = 0.5,
                      y = 1.1,
                      xanchor = "center",
                      yanchor = "bottom")
      )
  })
  
  ###Graph 2 - mon welfare 
  output$welfare_mon2 <- renderPlotly({
    req(input$tab3a_state, input$tab3a_sector, input$mon2_demo)
    
    df <- hces3 %>%
      filter(
        state == ifelse(input$tab3a_state == "National", "India", input$tab3a_state),
        if (input$tab3a_sector != 99) sector2 == input$tab3a_sector else TRUE
      )
    
    # --- Demographic filtering ---
    if (input$mon2_demo == "dem") {
      # No additional filtering needed
    } else if (grepl("::", input$mon2_demo)) {
      demo_split <- strsplit(input$mon2_demo, "::")[[1]]
      if (length(demo_split) == 2) {
        col_name <- demo_split[1]
        filter_val <- demo_split[2]
        
        if (!is.null(filter_val) && filter_val != "ALL") {
          # Convert to numeric if needed
          if (col_name %in% c("gen_cat", "owns_land", "owns_dwelling")) {
            filter_val <- as.numeric(filter_val)
          }
          df <- df %>% filter(.data[[col_name]] == filter_val)
        }
      }
    }
    
    # --- Prepare data for plotting ---
    df_long <- df %>%
      filter(!is.na(welfare_agg21_sp)) %>%
      distinct(year, quintile_overall, .keep_all = TRUE) %>%
      mutate(
        quintile = case_when(
          quintile_overall == 1 ~ "Bottom 20%",
          quintile_overall == 2 ~ "Q2",
          quintile_overall == 3 ~ "Q3",
          quintile_overall == 4 ~ "Q4",
          quintile_overall == 5 ~ "Top 20%"
        ),
        tooltip_text = paste0("Year: ", year,
                              "<br>Quintile: ", quintile,
                              "<br>Welfare: ", sprintf("%.2f", welfare_agg21_sp))
      )
    
    
    
    year_colors <- c("2011" = "#d31f11", "2022" = "#007191")
    
    plot_ly(
      df_long,
      x = ~quintile,
      y = ~welfare_agg21_sp,
      color = ~factor(year),
      colors = year_colors,
      type = "bar",
      hovertext = ~tooltip_text,
      hoverinfo = "text",
      text = ~paste0(sprintf("%.2f", welfare_agg21_sp)),
      textposition = "auto",
      textfont = list(size = 14, color = "#ffffff", family = "Arial")
    ) %>%
      layout(
        barmode = "group",
        bargap = 0.1,
        xaxis = list(title = ""),
        yaxis = list(title = "", tickformat = ".2f"),
        legend = list(title = list(text = "Survey Year"),
                      orientation = "h",
                      x = 0.5,
                      y = 1.1,
                      xanchor = "center",
                      yanchor = "bottom"
        ))
  })
  
  # Graph 3 
  label_map <- list( "dem" = "All Households",
                     "Age" = "All Age groups",
                     "Gender" = "All Gender groups",
                     "Education" = "All Education groups",
                     "HH Size" = "All Size groups",
                     
                     "owns_dwelling_yes" = "Owns Dwelling Unit – Yes",
                     "owns_dwelling_no"  = "Owns Dwelling Unit – No",
                     
                     "Owns Land" = "All Owns Land groups",
                     "owns_land_yes" = "Owns Land – Yes",
                     "owns_land_no" = "Owns Land – No",
                     "age_cat1" = "15–29", "age_cat2" = "30–45", "age_cat3" = "46–64", "age_cat4" = "65+",
                     "gender_cat1" = "Male", "gender_cat2" = "Female",
                     "edu_cat1" = "No education/Below primary", "edu_cat2" = "Primary completed",
                     "edu_cat3" = "Middle completed", "edu_cat4" = "Secondary/Sr Secondary",
                     "edu_cat5" = "Diploma / Graduate", "edu_cat6" = "Post Graduate and above",
                     "hhsize_cat1" = "One person", "hhsize_cat2" = "2–3 persons",
                     "hhsize_cat3" = "4–6 persons", "hhsize_cat4" = "7+ persons"
  )
  
  # -----------------------------------------------------------
  # Graph 3 – Sources of Income • POOR (poor_420 == 1)
  # -----------------------------------------------------------
  output$welfare_mon3 <- renderPlotly({
    req(input$tab3a_state, input$tab3a_sector, input$inc_year, input$inc_demo)
    
    if (length(input$inc_demo) == 0 || length(input$inc_demo) > 3) return(NULL)
    
    df_long <- map_dfr(input$inc_demo, function(demo_var) {
      df_filtered <- hces2 %>%
        filter(
          state == ifelse(input$tab3a_state == "National", "India", input$tab3a_state),
          year == as.numeric(input$inc_year),
          poor_420 == 1,  # Or 0 for non-poor
          if (input$tab3a_sector != 99) sector2 == input$tab3a_sector else TRUE
        )
      
      # Demographic filtering
      if (demo_var != "dem") {
        if (demo_var %in% c("owns_dwelling_yes", "owns_land_yes")) {
          df_filtered <- df_filtered %>% filter(
            .data[[sub("_yes$", "", demo_var)]] == 1
          )
        } else if (demo_var %in% c("owns_dwelling_no", "owns_land_no")) {
          df_filtered <- df_filtered %>% filter(
            .data[[sub("_no$", "", demo_var)]] == 0
          )
        } else {
          df_filtered <- df_filtered %>% filter(.data[[demo_var]] == 1)
        }
      } else {
        df_filtered <- df_filtered %>% filter(dem == "All Households")
      }
      
      # If no data, return empty
      if (nrow(df_filtered) == 0)
        return(tibble(source = character(), value = numeric(), group = character()))
      
      df_filtered %>%
        slice(1) %>%
        select(inc_selfemp, inc_salaried, inc_casual) %>%
        pivot_longer(cols = everything(), names_to = "source", values_to = "value") %>%
        mutate(group = label_map[[demo_var]] %||% demo_var)
    })
    
    
    df_long <- df_long %>%
      mutate(
        source = recode(source,
                        inc_selfemp = "Self-employed",
                        inc_salaried = "Salaried",
                        inc_casual   = "Casual"),
        value = value * 100,
        value_label = sprintf("%.2f", value)
      )
    
    custom_palette <- c("#4a2377", "#0d7d87", "#f55f74")
    group_colors <- setNames(custom_palette[seq_along(unique(df_long$group))], unique(df_long$group))
    
    plot_ly(
      df_long,
      x = ~source,
      y = ~value,
      color = ~group,
      colors = group_colors,
      type = "bar",
      hovertext = ~paste0(source, "<br>", value_label, "%"),
      hoverinfo = "text",
      text = ~paste0(value_label, "%"),
      textposition = "auto",
      textfont = list(size = 14, color = "#ffffff", family = "Arial")
    ) %>%
      layout(
        barmode = "group",
        bargap = 0.1,
        xaxis = list(title = ""),
        yaxis = list(title = "", range = c(0, 100), tickformat = ".2f"),
        legend = list(
          orientation = "h",
          x = 0.5,
          y = 1.1,
          xanchor = "center",
          yanchor = "bottom"
        )
      )
  })
  
  
  
  # -----------------------------------------------------------
  # Graph 4 – Sources of Income • NON-POOR (poor_420 == 0)
  # -----------------------------------------------------------
  output$welfare_mon4 <- renderPlotly({
    req(input$tab3a_state, input$tab3a_sector, input$inc_year, input$inc_demo)
    
    if (length(input$inc_demo) == 0 || length(input$inc_demo) > 3) return(NULL)
    
    df_long <- map_dfr(input$inc_demo, function(demo_var) {
      df_filtered <- hces2 %>%
        filter(
          state == ifelse(input$tab3a_state == "National", "India", input$tab3a_state),
          year == as.numeric(input$inc_year),
          poor_420 == 0,  # Or 0 for non-poor
          if (input$tab3a_sector != 99) sector2 == input$tab3a_sector else TRUE
        )
      
      # Demographic filtering
      if (demo_var != "dem") {
        if (demo_var %in% c("owns_dwelling_yes", "owns_land_yes")) {
          df_filtered <- df_filtered %>% filter(
            .data[[sub("_yes$", "", demo_var)]] == 1
          )
        } else if (demo_var %in% c("owns_dwelling_no", "owns_land_no")) {
          df_filtered <- df_filtered %>% filter(
            .data[[sub("_no$", "", demo_var)]] == 0
          )
        } else {
          df_filtered <- df_filtered %>% filter(.data[[demo_var]] == 1)
        }
      } else {
        df_filtered <- df_filtered %>% filter(dem == "All Households")
      }
      
      # If no data, return empty
      if (nrow(df_filtered) == 0)
        return(tibble(source = character(), value = numeric(), group = character()))
      
      df_filtered %>%
        slice(1) %>%
        select(inc_selfemp, inc_salaried, inc_casual) %>%
        pivot_longer(cols = everything(), names_to = "source", values_to = "value") %>%
        mutate(group = label_map[[demo_var]] %||% demo_var)
    })
    
    
    df_long <- df_long %>%
      mutate(
        source = recode(source,
                        inc_selfemp = "Self-employed",
                        inc_salaried = "Salaried",
                        inc_casual   = "Casual"),
        value = value * 100,
        value_label = sprintf("%.2f", value)
      )
    
    custom_palette <- c("#4a2377", "#0d7d87", "#f55f74")
    group_colors <- setNames(custom_palette[seq_along(unique(df_long$group))], unique(df_long$group))
    
    plot_ly(
      df_long,
      x = ~source,
      y = ~value,
      color = ~group,
      colors = group_colors,
      type = "bar",
      hovertext = ~paste0(source, "<br>", value_label, "%"),
      hoverinfo = "text",
      text = ~paste0(value_label, "%"),
      textposition = "auto",
      textfont = list(size = 14, color = "#ffffff", family = "Arial") 
    ) %>%
      layout(
        barmode = "group",
        bargap = 0.1,
        xaxis = list(title = ""),
        yaxis = list(title = "", range = c(0, 100), tickformat = ".2f"),
        legend = list(
          orientation = "h",
          x = 0.5,
          y = 1.1,
          xanchor = "center",
          yanchor = "bottom"
        )
      )
  })
  
  
  
  
  ###########################################################################################
  ####### Tab 3b: Non‑monetary Welfare #######
  ###DOWNLOAD###
  output$download_tab3b <- downloadHandler(
    filename = function() {
      paste0("NonMonetary_Welfare_Data_", Sys.Date(), ".zip")
    },
    content = function(file) {
      tmpdir <- tempdir()
      
      # File paths
      file1 <- file.path(tmpdir, "Graph1_nfhs_pov_all.csv")
      file2 <- file.path(tmpdir, "Graph2_nfhs_wealth_all.csv")
      file3 <- file.path(tmpdir, "Graph3_nfhs_components_all.csv")
      
      # Save CSVs
      readr::write_csv(nonmon_1, file1)
      readr::write_csv(nonmon_2, file2)
      readr::write_csv(nonmon_3, file3)
      
      # Create zip file
      zip::zipr(zipfile = file, files = c(file1, file2, file3))
    },
    contentType = "application/zip"
  )
  
  
  
  
  # UI: dynamic second dropdown based on selected demographic variable
  output$nmw_demo_value_ui <- renderUI({
    req(input$nmw_demo_var)
    
    values <- sort(na.omit(unique(nonmon_1[[input$nmw_demo_var]])))
    
    # own_house
    if (input$nmw_demo_var %in% c("own_house", "agri_land")) {
      label_map <- setNames(c("No", "Yes"), c("0", "1"))
      values <- intersect(names(label_map), as.character(values)) 
      choices_named <- setNames(values, label_map[values])
    } else {
      choices_named <- values
    }
    
    selectInput("nmw_demo_value", "Select Value:", choices = choices_named)
  })
  
  
  # Graph 1: Incidence of Non-Monetary Poverty (Line Chart)
  # ---- Graph 1: Incidence of Non-Monetary Poverty (same look as Tab 3a) ----
  output$nonmon_graph1 <- renderPlotly({
    req(input$tab3b_state, input$tab3b_sector, input$nmw_demo_combined)
    
    # Filter based on state, sector, and round
    df <- nonmon_1 %>%
      filter(
        (input$tab3b_state == "National" & StateName == "India") |
          (input$tab3b_state != "National" & StateName == input$tab3b_state),
        if (input$tab3b_sector != 99) sector == input$tab3b_sector else TRUE,
        round %in% c(4, 5)
      )
    
    # Demographic filtering
    if (input$nmw_demo_combined == "dem") {
      df <- df %>% filter(dem == "All Households")
    } else {
      split <- strsplit(input$nmw_demo_combined, "::")[[1]]
      col <- split[1]
      val <- split[2]
      df <- df %>% filter(.data[[col]] == if (col %in% c("own_house", "head_gender", "agri_land")) as.numeric(val) else val)
    }
    
    df <- df %>% distinct(round, .keep_all = TRUE)
    
    
    # Don't group or average, just reshape
    df_long <- df %>%
      select(round, m_poor_1_33, m_poor_1_50) %>%
      pivot_longer(cols = starts_with("m_poor"),
                   names_to = "poverty_line",
                   values_to = "headcount") %>%
      mutate(
        year = ifelse(round == 4, "2015–16", "2019–21"),
        threshold = recode(poverty_line,
                           m_poor_1_33 = "MPI_33",
                           m_poor_1_50 = "MPI_50"),
        headcount = headcount * 100,
        headcount_label = sprintf("%.2f", headcount)
      )
    
    if (nrow(df_long) == 0) {
      showNotification("No data for this combination of filters.", type = "message")
      return(NULL)
    }
    
    # Plot
    plot_ly(
      df_long,
      x = ~year,
      y = ~headcount,
      color = ~threshold,
      colors = c("MPI_33" = "#d31f11", "MPI_50" = "#007191"), 
      type = "scatter",
      mode = "lines+markers",
      line = list(width = 4),       
      marker = list(size = 10),  
      hovertext = ~paste0(
        "Year: ", year,
        "<br>Poverty Line: ", threshold,
        "<br>Headcount: ", headcount_label, "%"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Headcount (%)", range = c(0, 100), tickformat = ".2f"),
        legend = list(title = list(text = "Poverty Line"),
                      orientation = "h",
                      x = 0.5,
                      y = 1.1,
                      xanchor = "center",
                      yanchor = "bottom")
      )
  })
  
  
  
  
  
  
  ## Graph2: Share by Welfare‑Aggregate Quintiles
  output$nonmon_graph2 <- renderPlotly({
    req(input$tab3b_state, input$tab3b_sector, input$nmw_demo_combined2)
    
    # Filter dataset
    df <- nonmon_2 %>%
      filter(
        (input$tab3b_state == "National" & StateName == "India") |
          (input$tab3b_state != "National" & StateName == input$tab3b_state),
        if (input$tab3b_sector != 99) sector == input$tab3b_sector else TRUE,
        round %in% c(4, 5)
      )
    
    # Demographic filter
    if (input$nmw_demo_combined2 == "dem") {
      df <- df %>% filter(dem == "All Households")
    } else {
      demo_split <- strsplit(input$nmw_demo_combined2, "::")[[1]]
      col_name <- demo_split[1]
      filter_val <- demo_split[2]
      
      filter_val_final <- if (col_name %in% c("own_house", "head_gender", "agri_land")) {
        as.numeric(filter_val)
      } else {
        filter_val
      }
      
      df <- df %>% filter(.data[[col_name]] == filter_val_final)
    }
    
    # Keep only one row per round (if duplicates exist)
    df <- df %>% distinct(round, .keep_all = TRUE)
    
    if (nrow(df) == 0) return(NULL)
    
    # Reshape for plotting
    df_long <- df %>%
      select(round, wi_cat1, wi_cat2, wi_cat3, wi_cat4, wi_cat5) %>%
      pivot_longer(cols = starts_with("wi_cat"),
                   names_to = "quintile",
                   values_to = "value") %>%
      filter(!is.na(value)) %>%
      mutate(
        quintile = recode(quintile,
                          "wi_cat1" = "Bottom 20%",
                          "wi_cat2" = "Q2",
                          "wi_cat3" = "Q3",
                          "wi_cat4" = "Q4",
                          "wi_cat5" = "Wealthiest 20%"),
        year = case_when(
          round == 4 ~ "2015–16",
          round == 5 ~ "2019–21"
        ),
        value = value * 100,
        value_label = sprintf("%.2f", value),
        tooltip_text = paste0(
          "Year: ", year, "<br>",
          "Quintile: ", quintile, "<br>",
          "Share: ", value_label, "%"
        )
      )
    
    plot_ly(
      data = df_long,
      x = ~quintile,
      y = ~value,
      color = ~year,                                 
      colors = c("2015–16" = "#d31f11", "2019–21" = "#007191"),
      type = "bar",
      text = ~paste0(value_label, "%"),
      textposition = "auto",                        
      insidetextfont = list(color = "#ffffff", size = 14),
      outsidetextfont = list(color = "#000000", size = 14),
      hovertext = ~tooltip_text,
      hoverinfo = "text"
    ) %>%
      layout(
        barmode = "group",
        bargap = 0.05,          
        bargroupgap = 0.0,
        xaxis = list(title = ""),
        yaxis = list(title = "Share of Households (%)", tickformat = ".2f", range = c(0, 100)),
        legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center", yanchor = "bottom")
      )
    
  })
  
  
  
  #––– Graph3: Deprivation Indicators by Area & Year –––
  
  #COLORS
  custom_colors <- c(
    "#f3c35d",  # Mustard Yellow - Asset
    "#f26b2b",  # Bright Orange - Attendance
    "#d03e27",  # Brick Red - Child Mortality
    "#4c4172",  # Deep Indigo - Education
    "#c37f9e",  # Dusty Pink - Electricity
    "#2ab3a7",  # Teal - Fuel
    "#edbb4c",  # Golden Yellow - Housing
    "#a96c9a",  # Mauve - Nutrition
    "#1a8893",  # Cyan Blue - Toilet
    "#302752"   # Dark Purple - Water
  )
  
  # Map indicator name -> color (names MUST match the recoded labels below)
  indicator_colors <- setNames(custom_colors, c(
    "Asset", "Attendance", "Child Mortality", "Education", "Electricity",
    "Fuel", "Housing", "Nutrition", "Toilet", "Water"
  ))
  
  
  # UI output for demographic value
  output$welfare_nonmon3_poor <- renderPlotly({
    req(input$tab3b_state, input$tab3b_sector, input$nmwcomp_year, input$nmwcomp_demo_combined)
    
    df <- nonmon_3 %>%
      filter(
        (input$tab3b_state == "National" & StateName == "India") |
          (input$tab3b_state != "National" & StateName == input$tab3b_state),
        if (input$tab3b_sector != 99) sector == input$tab3b_sector else TRUE,
        round == input$nmwcomp_year,
        m_poor_1_33 == 1
      )
    
    if (input$nmwcomp_demo_combined == "dem") {
      df <- df %>% filter(dem == "All Households")
    } else {
      demo_split <- strsplit(input$nmwcomp_demo_combined, "::")[[1]]
      col_name <- demo_split[1]
      filter_val <- demo_split[2]
      
      filter_val_final <- if (col_name %in% c("own_house", "head_gender", "agri_land")) {
        as.numeric(filter_val)
      } else {
        filter_val
      }
      
      df <- df %>% filter(.data[[col_name]] == filter_val_final)
    }
    
    if (nrow(df) == 0) return(NULL)
    
    df_long <- df %>%
      select(g01_edu_1, g01_atten_1, g01_cm_1, g01_nutri_1,
             g01_elec_1, g01_toilet_1, g01_water_1, g01_house_1,
             g01_fuel_1, g01_asset_1) %>%
      pivot_longer(everything(), names_to="indicator", values_to="value") %>%
      mutate(indicator = recode(indicator,
                                g01_edu_1="Education", g01_atten_1="Attendance", g01_cm_1="Child Mortality",
                                g01_nutri_1="Nutrition", g01_elec_1="Electricity", g01_toilet_1="Toilet",
                                g01_water_1="Water", g01_house_1="Housing", g01_fuel_1="Fuel", g01_asset_1="Asset"
      )) %>%
      distinct(indicator, .keep_all=TRUE) %>%
      mutate(
        value = value * 100,
        label = sprintf("%.2f%%", value),
        bar_color = indicator_colors[indicator],
        hover = paste0(indicator, "<br>Value: ", label)
      )
    
    plot_ly(
      df_long,
      x = ~indicator, y = ~value, type = "bar",
      marker = list(color = ~bar_color),
      text = ~label,
      textposition = "auto",
      insidetextfont  = list(color = "#ffffff", size = 13, family = "Arial Black"),
      outsidetextfont = list(color = "#000000", size = 13, family = "Arial Black"),
      hovertext = ~hover, hoverinfo = "text"
    ) %>%
      layout(
        showlegend = FALSE,
        xaxis = list(title = "", tickangle = 45),
        yaxis = list(title = "Share (%)", range = c(0,100), tickformat = ".2f"),
        bargap = 0.05, bargroupgap = 0
      )
  })
  
  # Graph 4
  output$welfare_nonmon3_nonpoor <- renderPlotly({
    req(input$tab3b_state, input$tab3b_sector, input$nmwcomp_year, input$nmwcomp_demo_combined)
    
    df <- nonmon_3 %>%
      filter(
        (input$tab3b_state == "National" & StateName == "India") |
          (input$tab3b_state != "National" & StateName == input$tab3b_state),
        if (input$tab3b_sector != 99) sector == input$tab3b_sector else TRUE,
        round == input$nmwcomp_year,
        m_poor_1_33 == 0
      )
    
    if (input$nmwcomp_demo_combined != "dem") {
      demo_split <- strsplit(input$nmwcomp_demo_combined, "::")[[1]]
      col_name <- demo_split[1]
      filter_val <- demo_split[2]
      
      filter_val_final <- if (col_name %in% c("own_house", "head_gender", "agri_land")) {
        as.numeric(filter_val)
      } else {
        filter_val
      }
      
      df <- df %>% filter(.data[[col_name]] == filter_val_final)
    }
    
    if (nrow(df) == 0) return(NULL)
    
    df_long <- df %>%
      select(g01_edu_1, g01_atten_1, g01_cm_1, g01_nutri_1,
             g01_elec_1, g01_toilet_1, g01_water_1, g01_house_1,
             g01_fuel_1, g01_asset_1) %>%
      pivot_longer(everything(), names_to="indicator", values_to="value") %>%
      mutate(indicator = recode(indicator,
                                g01_edu_1="Education", g01_atten_1="Attendance", g01_cm_1="Child Mortality",
                                g01_nutri_1="Nutrition", g01_elec_1="Electricity", g01_toilet_1="Toilet",
                                g01_water_1="Water", g01_house_1="Housing", g01_fuel_1="Fuel", g01_asset_1="Asset"
      )) %>%
      distinct(indicator, .keep_all=TRUE) %>%
      mutate(
        value = value * 100,
        label = sprintf("%.2f%%", value),
        bar_color = indicator_colors[indicator],
        hover = paste0(indicator, "<br>Value: ", label)
      )
    
    plot_ly(
      df_long,
      x = ~indicator, y = ~value, type = "bar",
      marker = list(color = ~bar_color),
      text = ~label,
      textposition = "auto",
      insidetextfont  = list(color = "#ffffff", size = 13, family = "Arial Black"),
      outsidetextfont = list(color = "#000000", size = 13, family = "Arial Black"),
      hovertext = ~hover, hoverinfo = "text"
    ) %>%
      layout(
        showlegend = FALSE,
        xaxis = list(title = "", tickangle = 45),
        yaxis = list(title = "Share (%)", range = c(0,100), tickformat = ".2f"),
        bargap = 0.05, bargroupgap = 0
      )
  })
  
  
  ####### Tab 4a: monetary Poverty ####### 
  ###DOWNLOAD###
  
  output$download_tab4a <- downloadHandler(
    filename = function() {
      paste0("Monetary_Schemes_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(monetary, file)
    },
    contentType = "text/csv"
  )
  ###GRAPH1
  mon_indicator_labels <- c(
    food_pds_subs     = "PDS - Food Subsidy",
    food_pds_free     = "PMGKAY - Free Food",
    kerosene_pds_subs = "Kerosene Subsidy",
    lpg_subs          = "LPG Subsidy",
    elec_free         = "Free Electricity",
    durables_free     = "Any Free Durables",
    laptop2           = "Free Laptop",
    tablet2           = "Free Tablet",
    mobile2           = "Free Mobile",
    bicycle2          = "Free Bicycle",
    gov_school_free   = "Any free school items (Government)",
    pvt_school_free   = "Any free school items (Private)",
    fees_waived       = "Fee reimbursement",
    books             = "Free Books",
    stationary        = "Free Stationery",
    schoolbag         = "Free School Bag",
    clothing          = "Free School Uniform",
    footwear          = "Free School Footwear",
    pmjay_ben         = "PMJAY Beneficiary Coverage",
    pmjay_ben_avail   = "PMJAY Benefits Availed"
  )
  
  # Reactive filter
  filtered_data_mp <- reactive({
    req(input$mp_state, input$mp_sector)
    
    data <- monetary %>%
      filter(
        state == input$mp_state,
        state_sec_quint != 99
      )
    
    if (input$mp_sector != "all") {
      data <- data %>% filter(sector == input$mp_sector)
    }
    
    data
  })
  
  # Output plot
  output$monetary_bar <- renderPlotly({
    req(input$mp_state, input$mp_sector, input$mp_indicators)
    
    if (length(input$mp_indicators) == 0 || length(input$mp_indicators) > 3) {
      return(NULL)
    }
    
    # Filter data
    df <- monetary %>%
      filter(
        state == input$mp_state,
        state_sec_quint %in% c(1:5, 99),   
        year %in% c(2011, 2022)
      ) %>%
      filter(
        (
          year == 2022 & sector == input$mp_sector
        ) |
          (
            year == 2011 &
              (
                (input$mp_sector == "urban" & sector == 1) |
                  (input$mp_sector == "rural" & sector == 0) |
                  (input$mp_sector == "all" & sector == 99)
              )
          )
      )
    
    # Pivot longer
    df_long <- df %>%
      select(year, state_sec_quint, all_of(input$mp_indicators)) %>%
      pivot_longer(
        cols = all_of(input$mp_indicators),
        names_to = "indicator_code",
        values_to = "value"
      ) %>%
      mutate(
        value = value * 100,
        indicator = purrr::map_chr(indicator_code, ~ mon_indicator_labels[[.x]]),
        quintile = factor( state_sec_quint,
                           levels = c(1, 2, 3, 4, 5, 99),
                           labels = c("Poorest", "Q2", "Q3", "Q4", "Richest", "Overall")
        ),
        value_label = sprintf("%.2f%%", value)
      )
    
    # Plot
    plot_ly(
      df_long,
      x = ~quintile,
      y = ~value,
      color = ~paste(indicator, year, sep = " – "),
      colors = colors_sector,
      type = "bar",
      text = ~value_label,
      textposition = "auto", 
      insidetextfont  = list(size = 14, color = "#ffffff"),
      outsidetextfont = list(size = 14, color = "#000000"),
      hovertext = ~paste0(
        "Indicator: ", indicator,
        "<br>Year: ", year,
        "<br>Quintile: ", quintile,
        "<br>Value: ", value_label
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        barmode = "group",
        bargap = 0.15,
        yaxis = list(title = "", range = c(0, 100)),
        xaxis = list(title = ""),
        font = list(size = 14),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.3
        )
      )
    
  })
  ###########################################################################################
  ####### Tab 4b: Non‑monetary Poverty #######
  # Legend names
  ###########################################################################################
  ####### Tab 4b: Non‑monetary Poverty #######
  output$download_tab4b <- downloadHandler(
    filename = function() {
      paste0("NonMonetary_Schemes_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(nonmonetary, file)
    },
    contentType = "text/csv"
  )
  
  # Legend names
  indicator_labels <- c(
    bpl                      = "BPL",
    aadhar                   = "Aadhar",
    health_insu_govt         = "State/Central Health Insurance",
    household_health_met     = "Met Healthcare Worker",
    household_has_preg_fin   = "Delivery Assistance",
    household_has_preg_benefits = "Pregnancy Benefits",
    household_angan_benefits = "Anganwadi Benefits",
    household_angan_immun    = "Anganwadi Immunization",
    household_angan_ecc      = "Anganwadi Early Childhood Care"
  )
  
  # Reactive data filter
  filtered_data_nm <- reactive({
    req(input$nm_state, input$nm_sector)
    
    df <- nonmonetary %>%
      filter(state_numeric == input$nm_state)
    
    if (input$nm_sector != 99) {
      df <- df %>% filter(sector == input$nm_sector)
    }
    
    df
  })
  
  
  ##COlors###
  palette_poor <- c("#4a2377", "#0d7d87", "#f55f74")
  palette_nonpoor <- c("#f47a00", "#E41A1C", "#00BF7D")
  
  
  # Reusable plot renderer for poor or non-poor groups
  render_nm_plot <- function(df, palette) {
    sel <- input$nm_indicators
    req(length(sel) > 0)
    
    df_long <- df %>%
      select(Year, all_of(sel)) %>%
      pivot_longer(
        cols = all_of(sel),
        names_to = "indicator_code",
        values_to = "value"
      ) %>%
      group_by(Year, indicator_code) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        indicator_label = purrr::map_chr(indicator_code, ~ indicator_labels[[.x]]),
        Year_label = case_when(
          Year == 2015 ~ "2015-16",
          Year == 2019 ~ "2019-21",
          TRUE ~ as.character(Year)
        ),
        Year_label = factor(Year_label, levels = c("2015-16", "2019-21")),
        value_percent = value * 100,
        value_label = sprintf("%.2f%%", value_percent)
      )
    
    plot_ly(
      df_long,
      x = ~indicator_label,
      y = ~value_percent,
      color = ~Year_label,
      colors = palette,
      type = 'bar',
      text = ~value_label,
      textposition = "auto",
      insidetextfont  = list(size = 14, color = "#ffffff"),
      outsidetextfont = list(size = 14, color = "#000000"),
      hovertext = ~paste0(
        "Indicator: ", indicator_label,
        "<br>Year: ", Year_label,
        "<br>Value: ", value_label
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        barmode = "group",
        bargap = 0.05,
        bargroupgap = 0,
        xaxis = list(title = "", tickangle = 45),
        yaxis = list(title = "Percentage (%)", range = c(0, 100)),
        legend = list(title = list(text = "Year"))
      )
  }
  
  # Output: Poor households plot
  output$non_monetary_bar_poor <- renderPlotly({
    df <- filtered_data_nm() %>% filter(m_poor_1_33 == 1)
    render_nm_plot(df, palette_poor)
  })
  
  # Output: Non-poor households plot
  output$non_monetary_bar_nonpoor <- renderPlotly({
    df <- filtered_data_nm() %>% filter(m_poor_1_33 == 0)
    render_nm_plot(df, palette_nonpoor)
  })
  
}

#––– Run App –––
shinyApp(ui = ui, server = server)