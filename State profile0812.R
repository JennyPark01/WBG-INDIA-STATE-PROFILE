
################################################################################
# Project : State Profile
# Author  : Jan 15, 2025
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
library(shinycssloaders)

# ––– Load data –––

# Tab 1: Intro – State Profile Overview
intro_section1 <- read.csv("C:/Users/jaeye/Downloads/State profile/Intro/section1-30july.csv")
intro_section2 <- read.csv("C:/Users/jaeye/Downloads/State profile/Intro/section2_merged_all.csv")
intro_section3 <- read.csv("C:/Users/jaeye/Downloads/State profile/Intro/section3_intro.csv")


# Tab 2a: Labour Market 
labour_profile_df <- read.csv("C:/Users/jaeye/Downloads/State profile/Labour/merged_labour_intro_1.csv")
labour_intro2 <- read.csv("C:/Users/jaeye/Downloads/State profile/Labour/merged_labour_intro_2.csv")

labour_df <- read.csv("C:/Users/jaeye/Downloads/State profile/Labour/Section1_2_merged_labour_jqi_all.csv")
wage_quintile <- read.csv("C:/Users/jaeye/Downloads/State profile/Labour/Section3_merged_wage_quintile.csv") 

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
# Tab 4a-2
graph2_mp <- read.csv("C:/Users/jaeye/Downloads/State profile/Schemes/hces_schemes_poor.csv")


#Tab 4b:
nonmonetary <- read.csv(  "C:/Users/jaeye/Downloads/State profile/Schemes/nfhs2015_2019_mpi.csv")
# Tab 4b-2:
graph2_nm <- read_csv("C:/Users/jaeye/Downloads/State profile/Schemes/graph2-nfhsschemes.csv")

#Comparison#
comp <- read.csv("C:/Users/jaeye/Downloads/State profile/comparison/statecomp_15sep.csv")


# ---- Download Button Data ----
# directory 
download_dir <- "C:/Users/jaeye/Downloads/State profile/data"

# ---- Tab 1: Intro (Latest Snapshot) ----
file_intro_section1 <- file.path(download_dir, "Section1.xlsx")
file_intro_section2 <- file.path(download_dir, "Section2.xlsx")
file_intro_section3 <- file.path(download_dir, "Section3.xlsx")

# ---- Tab 2: Labour Market ----
file_labour_1<- file.path(download_dir, "Section1_Part1.xlsx")
file_labour_2<- file.path(download_dir, "Section1_Part2.xlsx")

file_labour_indicators <- file.path(download_dir, "Section2_Labour_Indicators.xlsx")
file_JobQuality <- file.path(download_dir, "Section3_JobQuality.xlsx")
file_real_earnings     <- file.path(download_dir, "Section4_RealEarningsDistribution.xlsx")

# ---- Tab 3a: Monetary Welfare ----
file_monetary_section1 <- file.path(download_dir, "Section1_hces_pov_all.xlsx")
file_monetary_section2 <- file.path(download_dir, "Section2_hces_welfare_all.xlsx")
file_monetary_section3 <- file.path(download_dir, "Section3_hces_income_all.xlsx")

# ---- Tab 3b: Non-monetary Welfare ----
file_nonmon_section1 <- file.path(download_dir, "Section1_nfhs_pov_all.xlsx")
file_nonmon_section2 <- file.path(download_dir, "Section2_nfhs_wealth_all.xlsx")
file_nonmon_section3 <- file.path(download_dir, "Section3_nfhs_components_all.xlsx")

# ---- Tab 4a / 4b ----
file_monetary_schemes  <- file.path(download_dir, "Graph2_MonetarySchemes_ Quintiles.xlsx")
file_monetary_poor     <- file.path(download_dir, "Graph1_MonetarySchemes_Poor.xlsx")
file_nonmon_schemes    <- file.path(download_dir, "Graph2-NonMonetary_Schemes.xlsx")
file_nonmon_poor       <- file.path(download_dir, "Graph1_NonMonetary_Poor.xlsx")

# ---- Tab 5: Comparison ----
file_state_comparison  <- file.path(download_dir, "statecomp.xlsx")


##################################################––– UI –––#################################################
ui <- tagList(
  useShinyjs(),
  
  
  # 1) <head> styling
  tags$head(
    
    tags$title("India State Profiles"),
    
    
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


 /* Style for all Shiny tables */
  .shiny-table {
    font-size: 16px;
    width: 100%;
    border-collapse: collapse;
    margin-top: 10px;
  }

  /* Header row base */
  .shiny-table th {
    background-color:#E6F2FA;  
    font-weight: bold;
    text-align: center;
     padding: 12px 10px !important;
    font-size: 15px;
    line-height: 1.8 !important;
  }

  /* Header cell colors */
  .shiny-table th:nth-child(1) { color: black; }
  .shiny-table th:nth-child(2) { color: #003366; }  
  .shiny-table th:nth-child(3) { color: #800000; }  



  /* Table body cells */
  .shiny-table td {
    padding: 12px 10px; 
    text-align: center;
    border: 5px solid #595959;
    line-height: 3.8 !important;  
    height: 50px !important;   
    font-size:18px !important; 
  }

  /* Alternating row shading */
  .shiny-table tr:nth-child(even) {
    background-color: #f9f9f9;
  }
 
 /* Center everything in this table (belt & suspenders) */
#real_welfare_table th,
#real_welfare_table td {
  text-align: center !important;
}
 
/* First column body cells: All / Urban / Rural at 15px */
#real_welfare_table tbody td:nth-child(1) {
  font-size: 15px !important;
}
 
 /* Only affect the REAL WELFARE table */
#real_welfare_table table {
  table-layout: fixed;   /* make width rules stick */
  width: 100%;
}
#real_welfare_table th,
#real_welfare_table td {
  word-wrap: break-word;
  white-space: normal;
}

/* 1st column: 20% */
#real_welfare_table th:nth-child(1),
#real_welfare_table td:nth-child(1) {
  width: 20% !important;
}

/* 2nd & 3rd columns: 40% each */
#real_welfare_table th:nth-child(2),
#real_welfare_table td:nth-child(2),
#real_welfare_table th:nth-child(3),
#real_welfare_table td:nth-child(3) {
  width: 40% !important;
}

 
 
 
  /* Hover effect */
  .shiny-table tr:hover {
    background-color: #f1f1f1;
  }

  /* Column-specific colors */
  .shiny-table td:nth-child(1) {   /* Indicator column */
    background-color: white;       /* no color */
    color: black;
    font-weight: bold;
  }
  .shiny-table td:nth-child(2) {   /* Welfare Aggregate column */
    background-color: white;     /* light blue background #e6f0ff*/
    color: #003366;                /* dark blue text */
    font-weight: bold;
  }
  .shiny-table td:nth-child(3) {   /* Share of National Aggregate column */
    background-color: white;    /* light maroon/pink background #ffe6e6*/
    color: #800000;                /* maroon text */
    font-weight: bold;
  }
  
.box .well,
.content .well {
  font-size: 12px;         /* adjust to taste: 11–13px */
  line-height: 1.5;
}

.box .well p,
.box .well li,
.box .well em,
.box .well strong {
  font-size: inherit;      /* keep inline elements consistent */
}

.box .well h5,
.box .well h6 {
  font-size: 0.95em;       /* slightly smaller headings inside notes */
}

/* Tighter spacing inside notes (optional) */
.box .well p { margin-bottom: 6px; }
.box .well ul { margin-bottom: 8px; }

/* Your custom “well-panel” blocks (you already use this in a few places) */
.well-panel,
.well-panel p,
.well-panel li {
  font-size: 12px !important;
  line-height: 1.5;
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
        menuItem("Latest Snapshot", tabName="tab1", icon=icon("dashboard")),
        menuItem("Labour Market", tabName="tab2", icon=icon("users"),
                 menuSubItem("Labour Profile", tabName = "tab2z"),
                 menuSubItem("Key Labour Indicators", tabName="tab2a"),
                 menuSubItem("Quality of Employment", tabName="tab2b"),
                 menuSubItem("Distribution of Real Earnings", tabName="tab2c")
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
        tabItem(
          tabName = "tab1",
          tags$h2("Latest Snapshot",
                  style = "font-weight:bold; color:navy; margin-bottom:20px;"),
          
          # --- State selector ---
          fluidRow(
            column(3,
                   selectInput(
                     "overview_state", "Select State:",
                     choices  = c("National" = "India",
                                  sort(setdiff(unique(intro_section1$State), "India"))),
                     selected = "India"
                   )
                   
            ),
            column(9,
                   div(style = "text-align:right;",
                       downloadButton("download_tab1", "Download Data", class = "btn-download")
                   )
            )
          ),
          
          # === SECTION 1 : Donut (left) + Tiles stacked (right) 
          fluidRow(
            column(
              width = 12,
              box(
                width = 12,
                title = tags$div(style = "text-align:center; font-weight:bold;",
                                 "GROSS VALUE ADDED (GVA) BY INDUSTRY"),
                
                fluidRow(
                  # Donut chart
                  column(
                    width = 8,
                    plotlyOutput("gva_chart", height = "500px")
                  ),
                  # Tiles
                  column(
                    width = 4,
                    div(class = "tile-stack",
                        style = "padding-top:45px;",
                        valueBoxOutput("tile_gva_share", width = 12),
                        div(style = "height: 20px;"),
                        valueBoxOutput("tile_fhi_rank", width = 12)
                    )
                  )
                ),
                
                # --- Section 1 NOTE
                div(style = "padding:10px 20px;",
                    wellPanel(
                      tags$p(tags$strong("Note:")),
                      tags$ul(
                        tags$li(
                          "The Fiscal Health Indicator (FHI) score, developed by NITI Aayog, evaluates the financial performance of 18 Indian states. ",
                          "Using a composite index, it benchmarks states’ fiscal health across key dimensions—Quality of Expenditure, Revenue Mobilization, ",
                          "Fiscal Prudence, Debt Index, and Debt Sustainability. ",
                          "The indicator is based on data from the Comptroller and Auditor General of India (CAG) for the financial year 2022–23, ",
                          "providing a comprehensive framework to assess and compare states’ fiscal management practices. ",
                          "It ranges from 0 to 100."
                        ),
                        tags$li("Gross Value Added (GVA) values are for the year 2022."),
                        tags$li("GVA data for Ladakh and Jammu & Kashmir has been combined. Data not available for Daman & Diu and Lakshadweep.")
                      )
                    )
                    
                )
                
                
              )
            )
          ),
          
          # === SECTION 2 : Table (left) + Stacked Bar (right)
          fluidRow(
            # LEFT: REAL WELFARE AGGREGATE
            column(
              6,style="padding-left:8px; padding-right:8px;",
              box(
                width = 12,
                height = 740,  
                title = tags$div(style = "text-align:center; font-weight:bold;",
                                 "REAL WELFARE AGGREGATE"),
                tableOutput("real_welfare_table"),
                
                # --- Table Note  (Graph 2)
                div(style = "padding:10px 20px;",
                    wellPanel(
                      tags$p(tags$strong("Note:")),
                      tags$p(
                        "Real welfare aggregate (WA) includes food and non-food non-durable monthly expenditures, ",
                        "and is adjusted for spatial and temporal price differences. ",
                        "This aggregate is expressed in 2021 prices (corresponding to the latest year for PPPs). ",
                        "For further details on construction, please refer to ",
                        tags$a(
                          href = "http://documents.worldbank.org/curated/en/099060325033540333",
                          target = "_blank",
                          "India - Trends in Poverty from 2011-2012 to 2022-2023 : Methodology Note (English)"
                        ),
                        ". Washington, D.C. : World Bank Group."),
                      tags$p(
                        "For Poverty and Welfare data, data for Daman & Diu also includes Dadra & Nagar Haveli.")
                    )
                )
                
              )
            ),
            
            # RIGHT: POVERTY INCIDENCE
            column(
              6,style="padding-left:8px; padding-right:8px;",
              box(
                width = 12,  height = 740,  
                title = tags$div(style = "text-align:center; font-weight:bold;",
                                 "POVERTY INCIDENCE"),
                plotlyOutput("overview_indicator_chart", height = "400px"),
                
                # --- Bar Note Graph3
                div(style = "padding:10px 20px;",
                    wellPanel(
                      tags$p(tags$strong("Note:")),
                      tags$p(
                        "For monetary poverty, classification of poor and non-poor is based on International Poverty Lines. ",
                        "$3.00/day for Extreme poverty and $4.20/day for Lower-Middle Income (LMIC) poverty using 2021 PPP."
                      ),
                      tags$p(
                        "For non-monetary poverty, a household is identified as multidimensionally (non-monetary) poor ",
                        "if it is deprived in at least one-third (33%) of the weighted indicators. ",
                        "For more details, check the welfare page."),
                      tags$p(
                        "For Poverty and Welfare data, data for Daman & Diu also includes Dadra & Nagar Haveli.")
                    )
                )
                
              )
            )
          )
          ,
          
          # === SECTION 3 : Labour Overview (bar + two tiles + note)
          fluidRow(
            column(
              width = 12,
              box(
                width = 12,
                title = tags$div(style = "text-align:center; font-weight:bold;",
                                 "LABOUR OVERVIEW (Year: 2023)"),
                solidHeader = TRUE,
                
                #  Dropdowns for this section
                fluidRow(
                  column(
                    width = 4,
                    selectInput(
                      "intro3_sector", "Sector",
                      choices = c("All" = 99, "Rural" = 0, "Urban" = 1),
                      selected = 99
                    )
                  )
                ),
                
                # Layout: bar (left) + two tiles (right)
                fluidRow(
                  column(
                    width = 8,
                    plotlyOutput("intro3_bar", height = "420px")
                  ),
                  column(
                    width = 4,
                    div(class = "tile-stack",
                        style = "padding-top:20px;",
                        valueBoxOutput("intro3_tile_female_lfpr", width = 12),
                        div(style = "height: 20px;"),
                        valueBoxOutput("intro3_tile_female_unemp", width = 12)
                    )
                  )
                ),
                
                # --- Note below chart + tiles ---
                div(style = "padding:10px 20px;",
                    wellPanel(
                      tags$p(tags$strong("Note:")),
                      tags$ul(
                        tags$li(
                          tags$strong("Worker Population Ratio (WPR): "),
                          "Percentage of employed persons in the working age population (15+ years)."
                        ),
                        tags$li(
                          tags$strong("Labour Force Participation Rate (LFPR): "),
                          "Percentage of persons in labour force (currently working or seeking/available for work) in the working age population (15+ years)."
                        ),
                        tags$li(
                          tags$strong("Share of unpaid employment: "),
                          "The share of unpaid employment as percentage of total employment."
                        ),
                        tags$li(
                          tags$strong("Unemployment Rate: "),
                          "Percentage of persons unemployed among persons in the labour force (15+ years)."
                        ),
                        tags$li(
                          tags$strong("NEET (15–29 years): "),
                          "Percentage of youth (15–29 years) not in Employment, Education, or Training."
                        ),
                        tags$li(
                          tags$strong("Female Labour Force Participation Rate (Female LFPR): "),
                          "The percentage of women in the labour force (i.e. currently working or seeking or available for work) among the female working age population (15+ years)."
                        ),
                        tags$li(
                          tags$strong("Unemployment Rate (Female): "),
                          "The percentage of unemployed women among the women in the labour force (15+ years)."
                        )
                      ),
                      tags$p(
                        "For Labour data, Dadra & Nagar Haveli data has been combined under Daman & Diu. ",
                        "Jammu and Kashmir and Ladakh have been combined."
                      )
                    )
                )
                
                
                
              )
            )
          ),
          # Source Note
          fluidRow(
            column(
              12, align = "right",
              tags$em(
                style = "display:block; margin-bottom:40px;",
                HTML(paste0(
                  "Source: Periodic Labour Force Survey (PLFS) 2022-23;",
                  "Household Consumer Expenditure Survey (HCES), 2022-23;<br>",
                  "World Bank PIP Website (www.pip.worldbank.org)"
                  
                ))
              )
            )
          )
          
        ),
        
        
        ########LABOUR TAB#########
        # ######## TAB 2Z: Labour Profile ########
        tabItem(
          tabName = "tab2z",
          tags$h2("Labour Profile", style="font-weight:bold; color:navy; margin-bottom:20px;"),
          
          # Dropdown filters
          fluidRow(
            column(3,
                   selectInput("labprof_state", "Select State:",
                               choices = sort(unique(labour_profile_df$state)))
            ),
            column(3,
                   selectInput("labprof_sector", "Select Sector:",
                               choices = c("All" = 99, "Rural" = 0, "Urban" = 1),
                               selected = 99)
            ),
            column(6, 
                   div(style = "text-align:right;",
                       downloadButton("download_tab2z", "Download Data", class = "btn-download")))
          ),
          
          # Year Selector
          fluidRow(
            box(
              width = 12,
              title = tags$div(
                style = "text-align:center; font-weight:bold;",
                "LABOUR PROFILE"
              ),
              solidHeader = TRUE,
              
              # Year Selector 
              fluidRow(
                column(3,
                       selectInput("labprof_year", "Select Year:",
                                   choices = sort(unique(labour_profile_df$year)),
                                   selected = max(labour_profile_df$year, na.rm = TRUE))
                )
              ),
              div(style = "height: 10px;"),
              
              # Chart Layout
              fluidRow(
                column(
                  4,
                  plotlyOutput("gender_comp_plot", height = "350px"),
                  div(
                    style = "padding:10px 20px; margin-top:5px;",
                    wellPanel(
                      style = "background-color:#f8f9fa; border:1px solid #e9ecef; padding:8px; text-align:left;",
                      tags$p(
                        tags$strong("Note: "),
                        "Gender – 'Male' category includes transgender individuals."
                      )
                    )
                  )
                  
                ),
                column(
                  4,
                  plotlyOutput("working_age_plot", height = "350px"),
                  div(
                    style = "padding:10px 20px; margin-top:5px;",
                    wellPanel(
                      style = "background-color:#f8f9fa; border:1px solid #e9ecef; 
                 padding:8px; text-align:left;",
                      tags$p(
                        tags$strong("Note: "),
                        "Working age defined as age more than or equal to 15 years."
                      )
                    )
                  )
                ),
                column(4, plotlyOutput("age_cohort_plot", height="350px"))
              )
            )
          ) ,
          
          
          # --- Row 2: Education & Vocational Training ---
          fluidRow(
            box(
              width = 12,
              solidHeader = TRUE,
              
              # Dropdowns (same row layout)
              fluidRow(
                column(3,
                       selectInput("labprof_year2", "Select Year:",
                                   choices = sort(unique(labour_intro2$year)),
                                   selected = max(labour_intro2$year, na.rm = TRUE))
                ),
                column(3,
                       selectInput("labprof_demo", "Select Demographics:",
                                   choices = list(
                                     #"All households" = "All",
                                     "Working Age" = "Working Age",
                                     "Age Cohort" = c("15 to 29", "30 to 44", "45 to 64", "65 and above")
                                   ),
                                   selected = "All"
                       )
                       
                )
              ),
              
              div(style = "height: 10px;"),
              
              # Charts side by side
              fluidRow(
                column(6, plotlyOutput("edu_comp_plot", height = "380px")),
                column(6, plotlyOutput("voc_train_plot", height = "380px"))
              ),
              
              # Note below both charts
              fluidRow(
                column(
                  12,
                  div(
                    style = "padding:10px 20px; margin-top:5px;",
                    wellPanel(
                      style = "background-color:#f8f9fa; border:1px solid #e9ecef; 
                 padding:10px; text-align:left; font-size:13px; color:#333;",
                      tags$p(
                        tags$strong("Note: "),
                        "Vocational or technical training imparts job-specific skills through practical experience to enhance employability across industries. ",
                        "It can be acquired through three modes: ",
                        tags$strong("formal training "),
                        "(structured, certified programs by institutions or NSDC), ",
                        tags$strong("non-formal training "),
                        "(organized but flexible learning in community or workplace settings), and ",
                        tags$strong("informal training "),
                        "(unstructured, experience-based learning without certification)."
                      )
                    )
                  )
                )
              )
            )
          ),
          # Source Note
          fluidRow(
            column(
              12, align = "right",
              tags$em(
                style = "display:block; margin-bottom:40px;",
                HTML(paste0(
                  "Source: Periodic Labour Force Survey (PLFS), 2017-18 to 2023-24"
                  
                  
                ))
              )
            )
          ) 
        ),
        ########TAB 2A#########
        tabItem(
          tabName = "tab2a",
          tags$h2("Key Labour Indicators", style = "font-weight:bold; color:navy; margin-bottom:20px;"),
          
          # ---- Row 1: State & Sector ----
          fluidRow(
            column(
              width = 12,
              fluidRow(
                column(3,
                       selectInput("lab_state", "State",
                                   choices = c("National" = "India",
                                               sort(setdiff(unique(labour_df$statename), "India")))
                       )
                       
                ),
                column(3,
                       selectInput("lab_sector", "Sector",
                                   choices = c("All" = 99, "Rural" = 0, "Urban" = 1), selected = 99)
                ),
                column(6, div(style="text-align:right;",
                              downloadButton("download_tab2a", "Download Data", class="btn-download")))
                
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                width = 12,
                style = "max-height: 1300px;",
                title = tags$div(
                  style = "text-align:center; font-weight:bold;",
                  "KEY LABOUR INDICATOR"
                ),
                
                solidHeader = TRUE,
                
                # --- Dropdowns 
                fluidRow(
                  column(
                    width = 4,
                    selectInput(
                      "lab_indicator", "Indicator:",
                      choices = c(
                        "Unemployment Rate (CWS)" = "unemp_cws",
                        "LFPR (CWS)"              = "lf_cws",
                        "WPR (CWS)"               = "emp_cws",
                        "Share of Unpaid Employment"    = "unpaid_emp",
                        "NEET (15–29 years)"                    = "neet"
                      ),
                      selected = "unemp_cws"
                    )
                  ),
                  column(4,
                         selectInput("lab_demo", "Demographic:",
                                     choices = list(
                                       "All households" = "dem",
                                       "Education" = list(
                                         "Below Primary"        = "edu_cat::below primary",
                                         "Primary and Middle"   = "edu_cat::primary and middle",
                                         "Secondary"            = "edu_cat::secondary",
                                         "Tertiary"             = "edu_cat::tertiary"
                                       ),
                                       "Gender" = list(
                                         "Male"   = "sex1::Male",
                                         "Female" = "sex1::Female"
                                       ),
                                       "Youth" = list(
                                         "Youth"     = "youth::1",
                                         "Non-Youth" = "youth::0"
                                       )
                                     ),
                                     selected = "dem"
                         )
                         
                  )
                ),
                
                # --- Line chart below the dropdowns
                div(style = "height: 8px;"),
                plotlyOutput("labour_line", height = "420px"),
                # Note 
                div(style = "padding:10px 20px;",
                    wellPanel(
                      tags$p(
                        tags$strong("Note:")
                      ),
                      tags$ul(
                        tags$li(
                          tags$strong("Labour Force Participation Rate (LFPR): "),
                          "Percentage of persons in the labour force (currently working or seeking/available for work) in the working-age population (15+ years)."
                        ),
                        tags$li(
                          tags$strong("Worker Population Ratio (WPR): "),
                          "Percentage of employed persons in the working-age population (15+ years)."
                        ),
                        tags$li(
                          tags$strong("Share of unpaid employment: "),
                          "Share of unpaid employment as a percentage of total employment."
                        ),
                        tags$li(
                          tags$strong("NEET (15–29 years): "),
                          "Percentage of youth (15–29 years) Not in Employment, Education, or Training."
                        ),
                        tags$li(
                          tags$strong("Unemployment Rate: "),
                          "Percentage of persons unemployed among persons in the labour force (15+ years)."
                        )
                      )
                    )
                )
                
              )
            )
          ),
          # Source Note
          fluidRow(
            column(
              12, align = "right",
              tags$em(
                style = "display:block; margin-bottom:40px;",
                HTML(paste0(
                  "Source: Periodic Labour Force Survey (PLFS), 2017-18 to 2023-24"
                  
                ))
              )
            )
          ) ),
        
        
        # #######TAB 2B - Quality of Employment #########
        tabItem(tabName = "tab2b",
                tags$h2("Quality of Employment", style = "font-weight:bold; color:navy; margin-bottom:20px;"),
                # Dropdowns for State & Sector
                fluidRow(
                  column(3,
                         selectInput("lab2b_state", "Select State:",
                                     choices = c("National" = "India",
                                                 sort(setdiff(unique(labour_df$statename), "India")))
                         )
                         
                  ),
                  column(3,
                         selectInput("lab2b_sector", "Select Sector:",
                                     choices = c("All" = 99, "Rural" = 0, "Urban" = 1))
                  ),
                  column(6,
                         div(style = "text-align:right;",
                             downloadButton("download_tab2b", "Download Data", class = "btn-download")))
                  
                ),
                
                fluidRow(
                  column(
                    width = 12,
                    box(
                      width = 12,
                      
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
                                             "All households" = "dem",
                                             
                                             "Education" = list(
                                               "Below Primary"        = "edu_cat::below primary",
                                               "Primary and Middle"   = "edu_cat::primary and middle",
                                               "Secondary"            = "edu_cat::secondary",
                                               "Tertiary"             = "edu_cat::tertiary"
                                             ),
                                             
                                             "Gender" = list(
                                               "Male"   = "sex1::Male",
                                               "Female" = "sex1::Female"
                                             ),
                                             
                                             "Youth" = list(
                                               "Youth"     = "youth::1",
                                               "Non-Youth" = "youth::0"
                                             )
                                           ),
                                           selected = "dem"
                               )
                               
                        )
                      ),
                      
                      
                      fluidRow(
                        column(6, plotlyOutput("emp_sector_chart")),
                        column(6, plotlyOutput("emp_type_chart"))
                      ),
                      
                      # Note 
                      
                      div(style = "padding:10px 20px;",
                          wellPanel(
                            div(style="font-size: 90%;",
                                tags$p(tags$strong("Note:")),
                                tags$ul(
                                  tags$li(
                                    "Industry of activity based on NIC-2008."
                                  ),
                                  tags$li(
                                    tags$strong("Employment Type: "),
                                    tags$ul(
                                      tags$li(
                                        tags$strong("Self Employed: "),
                                        "Persons who operated their own farm or non-farm enterprises or were engaged independently in a profession ",
                                        "or trade on own-account or with one or a few partners were deemed to be self-employed in household enterprises."
                                      ),
                                      tags$li(
                                        tags$strong("Regular: "),
                                        "Persons who worked in others’ farm or non-farm enterprises (both household and non-household) ",
                                        "and, in return, received salary or wages on a regular basis (i.e. not on the basis of daily or periodic renewal of work contract)."
                                      ),
                                      tags$li(
                                        tags$strong("Casual: "),
                                        "A person who was casually engaged in others’ farm or non-farm enterprises (both household and non-household) ",
                                        "and, in return, received wages according to the terms of the daily or periodic work contract."
                                      )
                                    )
                                  )
                                )
                            )
                          )
                      )
                      ,
                      
                      # Row 2: Graph 3 + Graph 4
                      fluidRow(
                        column(6, plotlyOutput("job_quality_chart")),
                        column(6, plotlyOutput("earning_poverty_chart"))
                      ),
                      
                      # Note 
                      div(style = "padding:10px 20px;",
                          wellPanel(
                            div(style="font-size: 90%;",
                                tags$p(tags$strong("Note:")),
                                tags$ul(
                                  tags$li(
                                    tags$strong("Job Quality Index (JQI): "),
                                    "Constructed around four key components: ",
                                    "(i) Income Adequacy (whether an individual earns enough to keep an average family above $3.65 or $2.15 per day); ",
                                    "(ii) Employment Benefits (whether the job provides at least one benefit such as health insurance, pension, social security, or paid leave); ",
                                    "(iii) Job Stability (whether the individual has a written contract for their current employment); and ",
                                    "(iv) Job Satisfaction (whether the individual holds a regular full-time job of 48 hours per week, or a second job/part-time work totaling at least 40 hours per week, with no desire to work additional hours). ",
                                    "JQI ranges from 0 (lowest) to 4 (highest), and the mean JQI is presented here."
                                  )
                                  ,
                                  tags$li(
                                    tags$strong("Earning poverty: "),
                                    "The income sufficiency component of the JQI, which captures whether job income is enough to maintain a minimum standard of living among workers and their families. ",
                                    "It is calculated over all workers above the age of 14, in paid employment."
                                  )
                                )
                            )
                          )
                      )
                      
                    )
                  )
                ),
                # Source Note
                fluidRow(
                  column(
                    12, align = "right",
                    tags$em(
                      style = "display:block; margin-bottom:40px;",
                      HTML(paste0(
                        "Source: Periodic Labour Force Survey (PLFS), 2017-18 to 2023-24"
                        
                      ))
                    )
                  )
                ) ),
        
        
        ####TAB 2C#####
        tabItem(tabName = "tab2c",
                tags$h2("Distribution of Real Earnings", style = "font-weight:bold; color:navy; margin-bottom:20px;"),
                
                # Dropdowns for State & Sector
                fluidRow(
                  column(3,
                         selectInput("lab2c_state", "Select State:",
                                     choices = c("National" = "India",
                                                 sort(setdiff(unique(wage_quintile$statename), "India")))
                         )
                         
                  ),
                  column(3,
                         selectInput("lab2c_sector", "Select Sector:",
                                     choices = c("All" = "All", "Rural" = "Rural", "Urban" = "Urban"))
                  ),
                  column(6,
                         div(style = "text-align:right;",
                             downloadButton("download_tab2c", "Download Data", class = "btn-download")))
                  
                  
                ),
                
                fluidRow(
                  column(
                    width = 12,
                    box(
                      width = 12,
                      title = tags$div(
                        style = "text-align:center; font-weight:bold;",
                        "DISTRIBUTION OF REAL EARNINGS"
                      ),
                      
                      # Year & Demographic selectors
                      fluidRow(
                        column(3,
                               selectInput("realincome_year", "Select Year:", choices = sort(as.numeric(unique(wage_quintile$Year))))
                               
                        ),
                        column(4,
                               selectInput("realincome_demo", "HH Demographics:",
                                           choices = list(
                                             "All Households" = "All Households",
                                             
                                             "Employment Sector" = list(
                                               "Agriculture" = "cws_type::1",
                                               "Industries"  = "cws_type::2",
                                               "Services"    = "cws_type::3"
                                             ),
                                             
                                             "Employment Type" = list(
                                               "Regular"        = "emp_type::1",
                                               "Casual"          = "emp_type::2",
                                               "Self Employment" = "emp_type::3"
                                             ),
                                             
                                             "Gender" = list(
                                               "Male"   = "sex_cat::1",
                                               "Female" = "sex_cat::2"
                                             ),
                                             
                                             "Youth" = list(
                                               "Youth"     = "youth_cat::1",
                                               "Non-Youth" = "youth_cat::2"
                                             )
                                           )
                               )
                               
                        )
                      ),
                      
                      # Plot Output
                      plotlyOutput("real_income_bar"),
                      
                      # Note
                      div(style = "padding:10px 20px;",
                          wellPanel(
                            div(style="font-size: 90%;",
                                tags$p(tags$strong("Note:"),
                                       
                                       "Real Earnings refers to average monthly earnings in 2023 prices, including all employment types 
                                       (wage, regular, and self-employed workers). Groups are distributed into five equal parts, and each 
                                       quintile represents 20% of the group."
                                )
                            )
                          )
                      )
                    )
                  )
                ),
                # Source Note
                fluidRow(
                  column(
                    12, align = "right",
                    tags$em(
                      style = "display:block; margin-bottom:40px;",
                      HTML(paste0(
                        "Source: Periodic Labour Force Survey (PLFS) 2022-23;",
                        "Household Consumer Expenditure Survey (HCES), 2022-23;<br>",
                        "World Bank PIP Website (www.pip.worldbank.org)"
                        
                      ))
                    )
                  )
                )   
        ),
        
        
        
        
        
        ####### Tab 3a: Monetary Welfare #######
        tabItem(tabName = "tab3a",
                tags$h2("Monetary Welfare", style = "font-weight:bold; color:navy; margin-bottom:20px;"),
                
                # — Shared State & Sector selector row —
                fluidRow(
                  column(3,
                         selectInput("tab3a_state", "State:",
                                     choices = c("National" = "India",
                                                 sort(setdiff(unique(hces1$state), "India")))
                         )
                         
                  ),
                  column(3,
                         selectInput("tab3a_sector", "Sector:",
                                     choices = c("All" = 99, "Rural" = 0, "Urban" = 1))
                  ),
                  column(3, offset = 3, align = "right",
                         downloadButton("download_tab3a", "Download Data", class = "btn-download")
                  )
                  
                  
                ),
                
                # — First row (Graph 1 + Graph 2 side-by-side with equal height) 
                fluidRow(
                  column(
                    width = 6,
                    box(
                      width = 12,
                      height= 850, 
                      title = tags$div(
                        style = "text-align:center; font-weight:bold;", 
                        "POVERTY RATE"
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
                                                           "Employment Type" = list(
                                                             "Self-Employed" = "inc_selfemp",
                                                             "Casual"        = "inc_casual",
                                                             "Regular"      = "inc_salaried"
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
                      #uiOutput("mw_line_note"),
                      # div("Headcount – % of people classified as poor",
                      #    style = "text-align:center; font-weight:bold; margin-top:14px;"),
                      tags$br(),
                      wellPanel(
                        tags$p(tags$strong("Note:"), "Classification of poor and non-poor is based on International Poverty Lines."),
                        tags$ul(
                          tags$li(tags$strong("International Poverty Lines based on 2017 PPPs:"), "$2.15/day for Extreme poverty, $3.65/day for LMIC, $6.85/day for UMIC"),
                          tags$li(tags$strong("International Poverty Lines based on 2021 PPPs:"), "$3.00/day for Extreme poverty, $4.20/day for LMIC, $8.30/day for UMIC")
                        )
                      )
                    )
                  ),
                  
                  column(
                    width = 6,
                    box(
                      width = 12,
                      height= 850, 
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
                                                             "Yes" = "owns_dwelling::1", "No" = "owns_dwelling::0"
                                                           ),
                                                           "Employment Type" = list(
                                                             "Self-employed" = "inc_cat::Self-employed",
                                                             "Casual"        = "inc_cat::Casual",
                                                             "Regular"      = "inc_cat::Salaried"
                                                           )
                                                           
                                           )
                               )
                        )
                      ),
                      
                      plotlyOutput("welfare_mon2"),
                      tags$br(),
                      #div("Quintile Based on Real Welfare Aggregate",
                      #   style = "text-align:center; font-weight:bold; margin-top:14px;"),
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
                               style = "text-align:center; font-weight:bold;", "INCOME PROFILES OF POOR AND NON-POOR HOUSEHOLDS"),
                             
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
                                            "Major source of income for poor households"
                                          ),
                                          plotlyOutput("welfare_mon3")
                                      )),
                               column(6,
                                      box(width = 12,
                                          title = tags$div(
                                            style = "text-align:center; font-weight:bold;",
                                            "Major source of income for non-poor households"
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
                                 "<p style='margin-left:20px'><strong>◦ b.</strong> Regular: Persons working in others’ farm or non-farm enterprises (both household and non-household) and getting in return salary or wages on a regular basis (and not based on daily or periodic renewal of work contract).</p>",
                                 "<p style='margin-left:20px'><strong>◦ c.</strong> Casual: A person casually engaged in other farm or non-farm enterprises (both household and non-household including in public works) and getting in return wage according to the terms of the daily or periodic work contract.</p>"
                               ))
                             ),
                             
                             wellPanel(
                               
                               HTML(paste0(
                                 "<p><strong>• Note on Demographics:</strong></p>",
                                 "<p style='margin-left:20px'><strong>◦ Gender of Household head:</strong> Male includes transgenders</p>",
                                 "<p style='margin-left:20px'><strong>◦ Education classification of Household head:</strong> Education divided into six groups – No education / Below primary, Primary completed, Middle completed, Secondary/Sr Secondary, Diploma / Graduate, Post Graduate and above</p>",
                                 "<p style='margin-left:20px'><strong>◦ Age classification of Household head:</strong> Age divided into four groups – Between age 15–29, Between age 30–45, Between age 46–64, 65 and above</p>",
                                 "<p style='margin-left:20px'><strong>◦ Household Size classification:</strong> Household size divided into four groups – One person, Between 2–3 persons, Between 4–6 persons, More than 7 persons</p>",
                                 "<p style='margin-left:20px'><strong>◦ Owns Dwelling Unit:</strong> Dwelling unit refers to the unit of accommodation where the household is residing during the date of survey</p>",
                                 "<p style='margin-left:20px'><strong>◦ Land Ownership:</strong> Household owns (owns & possesses or leases out) any land (within the country) as on the date of survey</p>"
                               ))
                             )
                         )
                  )
                ),
                
                # Source Note
                fluidRow(
                  column(12, align = "right",
                         tags$em( style = "display:block; margin-bottom:40px;",
                                  "Source: Data on household consumption expenditure across categories is sourced from NSS 2011 and HCES 2022."))
                )
                
        ),
        
        ####### Tab 3b: Non‑monetary Welfare #######
        tabItem(tabName = "tab3b",
                tags$h2("Non‑monetary Welfare", style = "font-weight:bold; color:navy; margin-bottom:20px;"),
                
                # — State selector row —
                fluidRow(
                  column(3,
                         selectInput("tab3b_state", "State:",
                                     choices = c("National" = "India",
                                                 sort(setdiff(unique(nonmon_3$StateName), "India")))
                         )
                         
                         
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
                
                # — First row: Graph1 & Graph2 —
                fluidRow(
                  ## Graph 1
                  column(6,
                         box(width = 12,height = 800,
                             title = tags$div(
                               style = "text-align:center; font-weight:bold;", 
                               "INCIDENCE OF NON-MONETARY POVERTY"
                             ),
                             
                             # Combined HH Demographics dropdown 
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
                                                         )#,
                                                         #"Owns Agricultural Land" = list(
                                                         #  "Yes" = "agri_land::1",
                                                         #  "No" = "agri_land::0"
                                                         #)
                                                         #,
                                                         
                                                         #"Own House" = list(
                                                         #  "Yes" = "own_house::1",
                                                         #  "No" = "own_house::0"
                                                         #)
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
                                 "Non-monetary poverty is measured using the Multidimensional Poverty Index (MPI), a composite index
                                 of 10 indicators across three dimensions: Health, Education, and Living Standards. Each household receives 
                                 a deprivation score based on the sum of its indicator deprivations (see note for details). Households with 
                                 a score of one-third (33%) or more are classified as multidimensionally poor. Those with a score of at least 
                                 one-fifth (20%) but less than one-third (33%) are classified as vulnerable to multidimensional poverty."
                               )
                             )
                         )
                  ),
                  
                  ## Graph 2: Share of Households in Non-Monetary Poverty, By Wealth Quintiles
                  column(6,
                         box(width = 12,height = 800,
                             title = tags$div(
                               style = "text-align:center; font-weight:bold;", 
                               "SHARE OF HOUSEHOLDS IN NON-MONETARY POVERTY, BY WEALTH QUINTILES"
                             ),
                             
                             # Combined HH Demographics dropdown 
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
                                                         )#,
                                                         #"Owns Agricultural Land" = list(
                                                         #  "Yes" = "agri_land::1",
                                                         #  "No" = "agri_land::0"
                                                         #)
                                                         
                                                         #,
                                                         #"Own House" = list(
                                                         #  "Yes" = "own_house::1",
                                                         #  "No" = "own_house::0"
                                                         #)
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
                                 "A household is identified as multidimensionally (non-monetary) poor if it is deprived in at least one-third (33%) of the weighted indicators. The households are divided into 5 equal quintiles based on wealth index ranging from Bottom 20% (Q1) to the Wealthiest 20% (Q5).")
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
                                                                    )#,
                                                                    #"Owns Agricultural Land" = list(
                                                                    #  "Yes" = "agri_land::1",
                                                                    #  "No" = "agri_land::0"
                                                                    #)
                                                                    
                                                                    #,
                                                                    #"Own House" = list(
                                                                    #  "Yes" = "own_house::1",
                                                                    #  "No" = "own_house::0"
                                                                    #)
                                                    )
                                        )
                                 )
                               ),
                               
                               # Plots side-by-side
                               fluidRow(
                                 column(6,
                                        box(width = NULL, title = tags$div(
                                          style = "text-align:center; font-weight:bold;",
                                          "POOR HOUSEHOLDS"
                                        ),
                                        plotlyOutput("welfare_nonmon3_poor"))
                                 ),
                                 column(6,
                                        box(width = NULL, title = tags$div(
                                          style = "text-align:center; font-weight:bold;",
                                          "NON-POOR HOUSEHOLDS"
                                        ),
                                        plotlyOutput("welfare_nonmon3_nonpoor"))
                                 )
                               ),
                               
                               tags$br(),
                               wellPanel(
                                 HTML("
    <p>
      Multidimensional deprivations are measured using an <strong>uncensored score</strong> based on 10 indicators grouped into three dimensions: 
      <strong>Health</strong>, <strong>Education</strong>, and <strong>Living Standards</strong>. 
      A household is classified as <strong>multidimensionally poor</strong> if it has a weighted deprivation score greater than 0.33 
      (on a scale from 0 to 1) across the 10 indicators.
    </p>

    <p><strong>Health indicators</strong> (each weight = 1/6):</p>
    <ul>
      <li>Nutrition deprivation – If any household member under 70 years is undernourished.</li>
      <li>Child mortality deprivation – If a child under 18 has died in the household in the five years preceding the survey.</li>
    </ul>

    <p><strong>Education indicators</strong> (each weight = 1/6):</p>
    <ul>
      <li>Schooling deprivation – If no household member aged 12+ has completed six years of schooling.</li>
      <li>School attendance deprivation – If any child aged 6–14 is not attending school.</li>
    </ul>

    <p><strong>Living Standards indicators</strong> (each weight = 1/18):</p>
    <ul>
      <li>Cooking fuel deprivation – If the household uses solid fuels (e.g., dung, firewood, charcoal).</li>
      <li>Sanitation deprivation – If no sanitation facility, uses an unimproved one, or shares it.</li>
      <li>Drinking water deprivation – If water is unsafe or the source is ≥30 minutes roundtrip.</li>
      <li>Electricity deprivation – If the household has no electricity.</li>
      <li>Housing deprivation – If housing materials for floor, roof, or walls are inadequate.</li>
      <li>Asset deprivation – If the household does not own more than one of the following: radio, TV, telephone, computer, animal cart, bicycle, motorbike, refrigerator, car/truck.</li>
    </ul>

    <p style='margin-top:10px;'>
      <em>This graph shows % of households who are poor and % of households who are non-poor and deprived in each indicator.</em>
    </p>
  ")
                               )
                               ,
                               
                               
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
                
                #Poor / Non-poor side-by-side graphs
                fluidRow(
                  column(6,
                         box(width=NULL, title=tags$strong("Poor Households"),
                             plotlyOutput("monetary_bar_poor"))
                  ),
                  column(6,
                         box(width=NULL, title=tags$strong("Non-poor Households"),
                             plotlyOutput("monetary_bar_nonpoor"))
                  )
                ),
                
                
                # bar graph
                
                fluidRow(
                  column(12,
                         box(width = 12, 
                             title = tags$strong("SHARE OF INDIVIDUALS WITH ACCESS"),
                             
                             # Graph
                             plotlyOutput("monetary_bar"),
                             
                             # Note inside the box
                             tags$br(),
                             wellPanel(
                               style = "background-color:#f0f4f8; font-size:12px; line-height:1.5;",
                               
                               tags$h5(
                                 style = "font-size:15px; font-weight:bold; margin-top:0; color:#333;",
                                 "Note:"
                               ),
                               
                               tags$p("Bars compare Poor vs Non-poor for each chosen indicator, across the five quintiles."),
                               
                               # --- Added bullet points ---
                               tags$ul(
                                 tags$li("Households are divided into 5 equal quintiles based on the welfare aggregate ranging from Bottom 20% (Q1) to Top 20% (Q5). In some cases, fewer than five quintiles may be displayed due to limited variation in the welfare distribution or low sample size within the subgroup."),
                                 tags$li("Real welfare is measured using a consumption aggregate that includes food and non-food non-durable monthly expenditures. This aggregate is adjusted for spatial and temporal price differences and expressed in 2021 prices (corresponding to the latest year for PPPs).")
                               ),
                               
                               tags$br(),
                               
                               tags$table(
                                 style = "width:100%;",
                                 tags$tbody(
                                   tags$tr(tags$td(tags$b("PDS - Food Subsidy")),
                                           tags$td("% HHs who received food items from PDS shops")),
                                   tags$tr(tags$td(tags$b("PMGKAY - Free Food")),
                                           tags$td("% HHs who received free food items from PDS shops")),
                                   tags$tr(tags$td(tags$b("Kerosene Subsidy")),
                                           tags$td("% HHs procuring kerosene using ration card")),
                                   tags$tr(tags$td(tags$b("LPG Subsidy")),
                                           tags$td("% HHs receiving subsidy for LPG cylinder")),
                                   tags$tr(tags$td(tags$b("Free Electricity")),
                                           tags$td("% HHs receiving free electricity")),
                                   tags$tr(tags$td(tags$b("Any Free Durables")),
                                           tags$td("% HHs receiving any free durable goods")),
                                   tags$tr(tags$td(tags$b("Free Laptop")),
                                           tags$td("% HHs receiving free laptop")),
                                   tags$tr(tags$td(tags$b("Free Tablet")),
                                           tags$td("% HHs receiving free tablet")),
                                   tags$tr(tags$td(tags$b("Free Mobile")),
                                           tags$td("% HHs receiving free mobile phone")),
                                   tags$tr(tags$td(tags$b("Free Bicycle")),
                                           tags$td("% HHs receiving free bicycle")),
                                   tags$tr(tags$td(tags$b("Any type of free school items (Government)")),
                                           tags$td("% HHs with members in Government school who received any type of free school items")),
                                   tags$tr(tags$td(tags$b("Any type of free school items (Private)")),
                                           tags$td("% HHs with members in Private school who received any type of free school items")),
                                   tags$tr(tags$td(tags$b("Fee reimbursement")),
                                           tags$td("% HHs receiving reimbursement/waiver from educational institution")),
                                   tags$tr(tags$td(tags$b("Free Books")),
                                           tags$td("% HHs receiving free books")),
                                   tags$tr(tags$td(tags$b("Free Stationery")),
                                           tags$td("% HHs receiving free stationery")),
                                   tags$tr(tags$td(tags$b("Free School Uniform")),
                                           tags$td("% HHs receiving free school uniform")),
                                   tags$tr(tags$td(tags$b("Free School Bag")),
                                           tags$td("% HHs receiving free schoolbag")),
                                   tags$tr(tags$td(tags$b("Free School Footwear")),
                                           tags$td("% HHs receiving free school footwear")),
                                   tags$tr(tags$td(tags$b("PMJAY Beneficiary Coverage")),
                                           tags$td("% HHs with beneficiaries of PMJAY")),
                                   tags$tr(tags$td(tags$b("PMJAY Benefits Availed")),
                                           tags$td("% HHs which received medical benefits from PMJAY"))
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
                         box(width = NULL, title = tags$strong("POOR HOUSEHOLDS"),
                             plotlyOutput("non_monetary_bar_poor"))
                  ),
                  column(6,
                         box(width = NULL, title = tags$strong("NON-POOR HOUSEHOLDS"),
                             plotlyOutput("non_monetary_bar_nonpoor"))
                  )
                ),
                
                # ── Tab 4b: Wealth Quintile chart (chart + note inside one box) ───────────────
                fluidRow(
                  column(
                    12,
                    box(
                      width = 12,
                      title = tags$div(
                        style = "text-align:center; font-weight:bold;",
                        "USAGE OF SCHEMES ACROSS WEALTH INDEX QUINTILES"
                      ),
                      
                      # Chart
                      div(style="position:relative; z-index:1; margin-bottom:22px; clear:both;",
                          uiOutput("nm_quintile_plot_ui")
                      ),                   
                      # Note inside the same box
                      tags$br(),
                      wellPanel(
                        style = "background-color:#f0f4f8; font-size:12px; line-height:1.5;",
                        
                        tags$h5(
                          style = "font-size:15px; font-weight:bold; margin-top:0; color:#333;",
                          "Note:"
                        ),
                        
                        # --- Bullet points section ---
                        tags$ul(
                          tags$li("The graphs display the share across poor and non-poor households. Households are classified based on their non-monetary poverty score. MPI poor households have a deprivation score equal to or above the multidimensional poverty threshold of 0.33 (MPI – 33%), while non-poor households fall below this threshold."),
                          tags$li("The Wealth Index is a composite score calculated using household ownership of assets. The households are divided into 5 equal quintiles based on wealth index ranging from poorest (1) to the richest (5). A household is identified as poor if it is deprived in at least one-third (33%) of the weighted indicators.")
                        ),
                        
                        tags$br(),
                        
                        tags$table(
                          style = "width:100%;",
                          tags$tbody(
                            tags$tr(tags$td(tags$b("BPL")),
                                    tags$td("% HHs with BPL (ration) card ownerships")),
                            tags$tr(tags$td(tags$b("Aadhar")),
                                    tags$td("% HHs with any member having an Aadhar card")),
                            tags$tr(tags$td(tags$b("State/Central Health Insurance")),
                                    tags$td("% HHs with access to any state or centrally sponsored health insurance")),
                            tags$tr(tags$td(tags$b("Met Healthcare Worker")),
                                    tags$td("% HHs accessing Anganwadi center, ASHA or community health worker")),
                            tags$tr(tags$td(tags$b("Pregnancy Benefits")),
                                    tags$td("% HHs that received any pregnancy benefits from Anganwadi/ICDS centre")),
                            tags$tr(tags$td(tags$b("Delivery Assistance")),
                                    tags$td("% HHs that received financial assistance for delivery care")),
                            tags$tr(tags$td(tags$b("Anganwadi Benefits")),
                                    tags$td("% HHs that received any benefits for children from Anganwadi/ICDS centre")),
                            tags$tr(tags$td(tags$b("Anganwadi Immunization")),
                                    tags$td("% HHs that accessed Anganwadi/ICDS center for child immunization")),
                            tags$tr(tags$td(tags$b("Anganwadi Early Childhood Care")),
                                    tags$td("% HHs that received early childhood care at Anganwadi/ICDS centre"))
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
        ),
        
        
        
        
        
        
        ###### Tab 5: State Comparison ######
        tabItem(
          tabName = "tab5",
          tags$h2("State Comparison", style = "font-weight:bold; color:navy; margin-bottom:20px;"),
          
          # ── Dropdowns: Year / Sector / Indicator 
          fluidRow(
            column(3,
                   selectInput(
                     inputId = "comp_sector",
                     label   = "Select Sector:",
                     choices = c("All", "Rural", "Urban"),
                     selected = "All"
                   )
                   
            ),
            
            
            
            column(
              3,
              selectInput(
                inputId   = "comp_indicator",
                label     = "Select Indicator:",
                
                choices = list(
                  "Macroeconomic and Fiscal Indicators" = list(
                    "Real GVA as % of India GVA" = "state_share_india",
                    "Fiscal Health Indicator" = "fhiscore",
                    "Mean Real Welfare Aggregate (2021 prices)" = "welfare_agg21_sp",
                    "Mean State Aggregate as share of Mean National Aggregate" = "welfare_pct_india"
                  ),
                  "Poverty Measures" = list(
                    "Extreme Poverty ($3.00, 2011 PPP)" = "poor_300_2011",
                    "Extreme Poverty ($3.00, 2022 PPP)" = "poor_300_2022",
                    "LMIC Poverty ($4.20, 2011 PPP)"   = "poor_420_2011",
                    "LMIC Poverty ($4.20, 2022 PPP)"   = "poor_420_2022",
                    #"% Multidimensionally poor (2015, 20%)" = "m_poor_1_20_2015",
                    #"% Multidimensionally poor (2019, 20%)" = "m_poor_1_20_2019",
                    "% Multidimensionally poor (2015, 33%)" = "m_poor_1_33_2015",
                    "% Multidimensionally poor (2019, 33%)" = "m_poor_1_33_2019"
                    
                    
                  ),
                  "Components of Non-monetary Poverty" = list(
                    "MPI - Education"    = "mpi_edu",
                    "MPI - Attendance"   = "mpi_att",
                    "MPI - Child Mortality" = "mpi_cm",
                    "MPI - Nutrition"    = "mpi_nutri",
                    "MPI - Electricity"  = "mpi_elec",
                    "MPI - Toilet"       = "mpi_toilet",
                    "MPI - Water"        = "mpi_water",
                    "MPI - Fuel"         = "mpi_fuel",
                    "MPI - Asset"        = "mpi_asset"
                  ),
                  "Labour Market Indicators" = list(
                    "LFPR" = "lf_cws",
                    "WPR" = "emp_cws",
                    "Unemployment Rate" = "unemp_cws",
                    "Share Unpaid Workers" = "unpaid_emp",
                    "NEET (15–29 years)" = "neet",
                    "JQI — $3.65/day (2017 PPP)" = "JQdim365",
                    "JQI — $2.15/day (2017 PPP)" = "JQdim215",
                    "Earnings Poverty — <$3.65/day" = "POV_365",
                    "Earnings Poverty — <$2.15/day" = "POV_215",
                    "Median Real Earnings (₹, 2023)" = "real_wage_23"
                  ),
                  "Social Protection and Welfare Schemes" = list(
                    "% HHs who received food items from PDS shops" = "food_pds_subs",
                    "% HHs who received free food items from PDS shops" = "food_pds_free",
                    "% HHs receiving subsidy for LPG cylinder" = "lpg_subs",
                    "% HHs receiving free electricity" = "elec_free",
                    "% HHs receiving any free durable goods" = "durables_free",
                    "% HHs with beneficiaries of PMJAY" = "pmjay_ben",
                    "% HHs which received medical benefits from PMJAY" = "pmjay_ben_avail",
                    "% HHs with BPL (ration) card ownership" = "bpl",
                    "% HHs with any member having an Aadhaar card" = "aadhar",
                    "% HHs accessing Anganwadi/ASHA services" = "household_health_met",
                    "% HHs that received pregnancy benefits from Anganwadi/ICDS centre" = "household_has_preg_benefits",
                    "% HHs that received child benefits from Anganwadi/ICDS centre" = "household_angan_benefits"
                  )
                )
                ,
                
                selectize = TRUE
              )
            ),
            column(6,
                   div(style = "text-align:right;",
                       downloadButton("download_tab5", "Download Data", class = "btn-download"))
            )
            
          ),
          
          # ── Graph + Note
          fluidRow(
            column(
              width = 12,
              box(
                width = 12,
                title = tags$div(
                  style = "text-align:center;",
                  tags$h4(style = "font-weight:bold; margin-bottom:5px;", "State Comparison"),
                  uiOutput("selected_indicator_title")
                ),
                plotlyOutput("comp_plot", height = "700px"),
                
                
                
                #note
                div(
                  style = "padding:10px 20px;",
                  wellPanel(
                    div(
                      style = "font-size: 90%;",
                      tags$p(tags$strong("Notes")),
                      
                      # --- Macroeconomic and Fiscal Indicators ---
                      tags$p(
                        style = "display: inline-block;
                 font-weight: bold; 
                 font-size: 110%; 
                 background-color: #e6f0ff;
                 padding: 3px 8px; 
                 border-radius: 6px;
                 margin-top: 10px;",
                        "Macroeconomic and Fiscal Indicators"
                      ),
                      
                      tags$ul(
                        tags$li(
                          tags$strong("FHI Score: "),
                          "The Fiscal Health Indicator (FHI) score, developed by NITI Aayog, evaluates the financial performance of 18 Indian states. ",
                          "The indicator is based on data from the Comptroller and Auditor General of India (CAG) for the financial year 2022–23. ",
                          "It ranges from 0 to 100. For more information, check the introduction page."
                        ),
                        tags$li("Real Gross Value Added as % of Gross Value Added (GVA) of India"),
                        tags$li(
                          tags$strong("Real Welfare Aggregate (WA): "),
                          "The Real Welfare Aggregate (WA) consists of monthly food and non-food non-durable expenditures. ",
                          "It is adjusted for spatial and temporal price differences and expressed in 2021 prices, aligned with the latest PPP benchmark. ",
                          "The mean WA is calculated at the per-person level. For further details, refer to ",
                          tags$a(href = "http://documents.worldbank.org/curated/en/099060325033540333",
                                 target = "_blank",
                                 "India - Trends in Poverty from 2011-2012 to 2022-2023: Methodology Note (World Bank Group)"),
                          "."
                        )
                      ),
                      
                      # --- Labour Market Indicators ---
                      tags$p(
                        style = "display: inline-block;
                 font-weight: bold; 
                 font-size: 110%; 
                 background-color: #ffe6e6;
                 padding: 3px 8px; 
                 border-radius: 6px;
                 margin-top: 10px;",
                        "Labour Market Indicators (Source: PLFS 2023–24)"
                      ),
                      
                      tags$ul(
                        tags$li(tags$strong("Worker Population Ratio (WPR): "), "Percentage of employed persons in the working-age population (15+ years)."),
                        tags$li(tags$strong("Labour Force Participation Rate (LFPR): "), "Percentage of persons in the labour force (working or seeking/available for work) among the working-age population (15+ years)."),
                        tags$li(tags$strong("Share of unpaid employment: "), "Share of unpaid employment as a percentage of total employment."),
                        tags$li(tags$strong("Not in Employment, Education, or Training (NEET): "), "Percentage of youth (15–29 years) not in employment, education, or training."),
                        tags$li(tags$strong("Unemployment Rate: "), "Percentage of persons unemployed among persons in the labour force (15+ years)."),
                        tags$li(
                          tags$strong("Job Quality Index (JQI): "),
                          "Measures job quality across four components: (i) Income adequacy (above $3.65/day, 2017 PPP); (ii) Employment benefits (health insurance, pension, social security, or paid leave); (iii) Job stability (written contract); (iv) Job satisfaction (regular full-time or part-time ≥40 hrs/week, no desire for more work). Only calculated for regular and casual workers. JQI ranges from 0 (lowest) to 4 (highest), Mean JQI is presented here."
                        ),
                        tags$li(
                          tags$strong("Earnings Poverty: "),
                          "Captures whether job income is sufficient to maintain a minimum living standard among workers and families. ",
                          "Calculated over all workers aged 14+ in paid employment."
                        )
                      ),
                      
                      # --- Poverty Measures ---
                      tags$p(
                        style = "display: inline-block;
                 font-weight: bold; 
                 font-size: 110%; 
                 background-color: #e6ffe6;
                 padding: 3px 8px; 
                 border-radius: 6px;
                 margin-top: 10px;",
                        "Poverty Measures"
                      ),
                      tags$ul(
                        tags$li(
                          tags$strong("Monetary poverty (% HHs in poverty, 2011 and 2022): "),
                          "Classification of poor/non-poor is based on international poverty lines using 2021 PPPs: $3.00/day (Extreme Poverty) and $4.20/day (LMIC Poverty). ",
                          "Estimates are based on the Household Consumption Expenditure Survey (HCES) 2011 and 2022."
                        ),
                        tags$li(
                          tags$strong("Non-monetary poverty (% HHs multidimensionally poor, 2015 and 2019): "),
                          "Non-monetary poverty is measured using the Multidimensional Poverty Index (MPI), a composite index based on 10 indicators of household deprivation across Health, Education, and Living Standards. A household is classified as multidimensionally poor if deprived in at least one-third (33%) of the weighted indicators . Estimates are based on two rounds of the National Family Health Survey (NFHS), 2015 and 2019. 

MPI (Multidimensional Poverty Index) measures non-monetary poverty using 10 indicators across Health, Education, and Living Standards across all households. A household is poor if it is deprived in ≥33% of weighted indicators."
                        )
                      ),
                      
                      # --- Social Protection and Welfare Schemes ---
                      tags$p(
                        style = "display: inline-block;
                 font-weight: bold; 
                 font-size: 110%; 
                 background-color: #fff2cc;
                 padding: 3px 8px; 
                 border-radius: 6px;
                 margin-top: 10px;",
                        "Social Protection and Welfare Schemes"
                      ),
                      tags$p(tags$strong("Source: Household Consumption Expenditure Survey, 2022")),
                      tags$ul(
                        tags$li("% HHs who received food items from PDS shops: Any food items (rice, wheat, pulses, sugar, etc.) from PDS shops in last 30 days."),
                        tags$li("% HHs who received free food items from PDS shops: Free items under PMGKAY or other schemes in last 30 days."),
                        tags$li("% HHs receiving subsidy for LPG cylinder: Subsidy received in last three months."),
                        tags$li("% HHs procuring kerosene using ration card: Kerosene procured in last 3 months."),
                        tags$li("% HHs receiving free electricity: Free electricity in last 30 days."),
                        tags$li("% HHs who are beneficiaries of PMJAY: Any household member covered under PMJAY."),
                        tags$li("% HHs which received medical benefits from PMJAY: Any member received medical benefits under PMJAY."),
                        tags$li("% HHs receiving any free durable goods: % HHs receiving any free durable goods (Free durables include free laptop, tablet, mobile, bicycle, motorcycles and other free durables (excluding school shoes and uniforms).")
                        
                      ),
                      tags$p(tags$strong("Source: National Family Health Survey, 2019")),
                      tags$ul(
                        tags$li("% HHs with BPL (ration) card ownership."),
                        tags$li("% HHs with any member having an Aadhaar card."),
                        tags$li("% HHs accessing Anganwadi/ASHA services: At least one woman (15–49) visited Anganwadi or met ASHA/health worker in last 3 months."),
                        tags$li("% HHs that received pregnancy benefits from Anganwadi/ICDS centre: Any woman (15–49) received supplementary food, check-ups, or nutrition education."),
                        tags$li("% HHs that received child benefits from Anganwadi/ICDS centre: Any woman (15–49) received child-related benefits in past 12 months.")
                      ),
                      
                      # --- Components of Non-monetary Poverty ---
                      tags$p(
                        style = "display: inline-block;
                 font-weight: bold; 
                 font-size: 110%; 
                 background-color: #f3e6ff;
                 padding: 3px 8px; 
                 border-radius: 6px;
                 margin-top: 10px;",
                        "Components of Non-monetary Poverty"
                      ),
                      tags$ul(
                        tags$li("MPI (Multidimensional Poverty Index) measures non-monetary poverty using 10 indicators across Health, Education, and Living Standards across all households. A household is poor if it is deprived in ≥33% of weighted indicators."),
                        tags$li("Health indicators (weight 1/6): (i) Nutrition deprivation—if any household member under 70 is undernourished; (ii) Child mortality deprivation—if a child under 18 died in last 5 years."),
                        tags$li("Education indicators (weight 1/6): (i) Schooling deprivation—no household member 12+ completed six years of schooling; (ii) Attendance deprivation—any child aged 6–14 not attending school."),
                        tags$li("Living standards indicators (weight 1/18): (i) Cooking fuel (solid fuels), (ii) Sanitation (unimproved/shared), (iii) Drinking water (unsafe or >30 mins away), (iv) Electricity (none), (v) Housing (inadequate materials), (vi) Assets (owns ≤1 of: radio, TV, phone, computer, animal cart, bicycle, motorbike, refrigerator, or motor vehicle).")
                      )
                    )
                  )
                )
                
                
                
                # end wellPanel/div/box
              )     # end column
            )       # end fluidRow
          )
        )
      )
    )
  ),         # end tabItem
  
  
  # ---- Fixed footer
  div(class = "fixed-footer",
      HTML('If you encounter issues or have questions, please reach out to 
          <a href="mailto:nkochhar@worldbank.org" style="color:white; text-decoration: underline;">nkochhar@worldbank.org,</a> 
           copying 
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
  
  
  # Tab 1: INTRO – Download ZIP of 3 CSVs
  output$download_tab1 <- downloadHandler(
    filename = function() paste0("Intro_Data_", Sys.Date(), ".zip"),
    content = function(file) {
      tmpdir <- tempdir()
      if (!dir.exists(tmpdir)) dir.create(tmpdir, recursive = TRUE)
      
      tmpzip <- tempfile(fileext = ".zip")
      zip::zipr(
        zipfile = tmpzip,
        files = c(file_intro_section1, file_intro_section2, file_intro_section3)
      )
      file.copy(tmpzip, file)
    },
    contentType = "application/zip"
  )
  
  
  
  #  pick value by sector label
  pull_val_by_sector <- function(df, sector_label, col) {
    if (!nrow(df) || !(col %in% names(df))) return(NA_real_)
    # blanks -> NA (All)
    sec <- df$sector
    if (is.character(sec)) sec[sec == ""] <- NA
    sec_num <- suppressWarnings(as.numeric(sec))
    
    idx <- dplyr::case_when(
      sector_label == "All"   ~ is.na(sec_num),
      sector_label == "Urban" ~ !is.na(sec_num) & sec_num == 1,
      sector_label == "Rural" ~ !is.na(sec_num) & sec_num == 0,
      TRUE ~ FALSE
    )
    
    v <- suppressWarnings(as.numeric(df[[col]][which(idx)][1]))
    if (length(v) == 0) NA_real_ else v
  }
  
  safe_pull1_num <- function(df, col) {
    if (is.null(df) || nrow(df) == 0 || !col %in% names(df)) return(NA_real_)
    val <- suppressWarnings(as.numeric(df[[col]][1]))
    if (length(val) == 0) return(NA_real_)
    val
  }
  
  
  # Filter data based on state
  selected_intro_data <- reactive({
    req(input$overview_state)
    
    want <- if (identical(input$overview_state, "National")) "India" else input$overview_state
    df <- intro_section1 %>% dplyr::filter(trimws(State) == trimws(want))
    validate(need(nrow(df) > 0, paste0("No data for state: ", want)))
    dplyr::slice(df, 1)  
  })
  
  
  
  # Tile 1: Real GVA as % of India GVA  
  output$tile_gva_share <- renderValueBox({
    df <- selected_intro_data()
    val <- safe_pull1_num(df, "state_share_india")
    
    txt <- if (is.na(val) || length(val) == 0) {
      "N/A"
    } else {
      paste0(formatC(val, format = "f", digits = 2), "%")
    }
    
    
    valueBox(
      value = txt,
      subtitle = "Real GVA as % of India GVA",
      icon = icon("percent"),
      color = "navy"
    )
  })
  
  # Tile2: FHI Score (fhiscore)
  output$tile_fhi_rank <- renderValueBox({
    df <- selected_intro_data()
    val <- safe_pull1_num(df, "fhiscore")
    
    txt <- if (is.na(val) || length(val) == 0) {
      "N/A"
    } else {
      round(val, 1)
    }
    
    valueBox(
      value = txt,
      subtitle = "FHI Score",
      icon = icon("trophy"),
      color = "maroon"   
    )
  })
  
  # Donut Chart: GVA by Industry (robust)
  output$gva_chart <- renderPlotly({
    df <- selected_intro_data()
    plot_data <- data.frame(
      Sector = c("Agriculture","Mining","Manufacturing","Construction","Electricity","Trade","Services"),
      Value  = c(df$argiperc,df$miningperc,df$manugva,df$consgva,
                 df$elecgva,df$tradegva,df$servicesgva)
    )
    
    plot_ly(
      plot_data,
      labels = ~Sector,
      values = ~Value,
      type   = 'pie',
      hole   = 0.5,text = ~paste0(Sector, ": ","<br>", sprintf("%.1f%%", Value * 100)),
      textinfo = "text",
      
      textfont = list(size = 16, family = "Arial Black", color = "black"),
      marker   = list(colors = c("#d31f11","#ff6347","#e89f00",
                                 "#008080","#007191","#00BF7D","#4a2377")),
      sort = FALSE
    ) %>%
      layout(
        title = list(
          text = " ",
          font = list(size = 22, family = "Arial Black", color = "black") 
        ),
        showlegend = FALSE,  
        margin = list(t = 80, b = 80, l = 0, r = 0), 
        transition = list(duration = 500, easing = "cubic-in-out")
      )
  })
  
  
  ### Intro SECTION 2 (server) — Table + Bar ###
  
  # Ensure sector is numeric (0, 1, 99)
  intro_section2 <- intro_section2 %>%
    dplyr::mutate(sector = suppressWarnings(as.numeric(sector)))
  
  # Helper: pick a value by sector label for a given column
  pull_val_by_sector <- function(df, sector_label, colname) {
    stopifnot(colname %in% names(df))
    sel <- switch(
      sector_label,
      "All"   = df$sector == 99,
      "Urban" = df$sector == 1,
      "Rural" = df$sector == 0,
      rep(FALSE, nrow(df))
    )
    v <- suppressWarnings(as.numeric(df[[colname]][sel]))
    if (length(v) == 0 || all(is.na(v))) NA_real_ else v[which(!is.na(v))[1]]
  }
  
  # Reactive: subset by state (National -> India)
  selected_intro2 <- reactive({
    req(input$overview_state)
    want <- if (identical(input$overview_state, "National")) "India" else input$overview_state
    df2  <- intro_section2 %>% dplyr::filter(trimws(State) == trimws(want))
    validate(need(nrow(df2) > 0, paste("No section-2 data for", want)))
    df2
  })
  
  # ---------- TABLE: Real Welfare ----------
  output$real_welfare_table <- renderTable({
    df2  <- selected_intro2()
    rows <- c("All", "Urban", "Rural")
    
    out <- tibble::tibble(
      Indicator = rows,
      `Mean State WA` = sapply(rows, function(r)
        pull_val_by_sector(df2, r, "welfare_agg21_sp")),
      `Mean State WA as % of Mean National WA` = sapply(rows, function(r)
        pull_val_by_sector(df2, r, "welfare_pct_india"))
    )
    
    out %>%
      dplyr::mutate(
        `Mean State WA` = ifelse(
          is.na(`Mean State WA`), "N/A",
          paste0("₹ ", scales::comma(`Mean State WA`, accuracy = 1))
        ),
        `Mean State WA as % of Mean National WA` = ifelse(
          is.na(`Mean State WA as % of Mean National WA`), "N/A",
          paste0(round(`Mean State WA as % of Mean National WA`, 1), "%")
        )
      )
  },
  striped = TRUE, bordered = TRUE, spacing = "m", align = "lcc", width = "100%")
  
  # ---------- BAR: Poverty Incidence (grouped) ----------
  output$overview_indicator_chart <- renderPlotly({
    df2 <- selected_intro2()
    
    plot_df <- df2 %>%
      dplyr::mutate(
        Sector = dplyr::case_when(
          sector == 99 ~ "All",
          sector == 1  ~ "Urban",
          sector == 0  ~ "Rural",
          TRUE         ~ "Other"
        )
      ) %>%
      dplyr::transmute(
        Sector,
        `Extreme Poverty (%)`        = as.numeric(poor_300),
        `LMIC Poverty (%)`           = as.numeric(poor_420),
        `Non-monetary Poverty (%)`   = as.numeric(m_poor_1_33)
      ) %>%
      dplyr::group_by(Sector) %>%
      dplyr::summarise(dplyr::across(everything(), ~ .[which(!is.na(.))[1]]), .groups = "drop") %>%
      tidyr::pivot_longer(cols = -Sector, names_to = "Indicator", values_to = "Value") %>%
      dplyr::mutate(
        Sector = factor(Sector, levels = c("All", "Urban", "Rural")),
        Value  = 100 * Value,                            
        label  = paste0(round(Value, 1), "%")
      ) %>%
      dplyr::arrange(Sector)
    
    validate(need(nrow(plot_df) > 0, "No poverty incidence data available."))
    
    plot_ly(
      data  = plot_df,
      x     = ~Sector,
      y     = ~Value,
      type  = "bar",
      color = ~Indicator,
      colors = c("#f55f74", "#0d7d87", "#4a2377"),
      text  = ~label,
      textposition = "outside",
      texttemplate = "<b>%{text}</b>",
      textfont = list(size = 12, family = "Arial Black"),
      cliponaxis = FALSE,
      hovertemplate = "<b>%{x}</b><br>%{customdata}: %{y:.1f}%<extra></extra>",
      customdata = ~Indicator
    ) %>%
      layout(
        barmode = "group",
        bargap = 0.25,
        bargroupgap = 0.1,
        margin = list(t = 40, r = 10, b = 60, l = 50),
        xaxis = list(title = "", categoryorder = "array",
                     categoryarray = c("All", "Urban", "Rural")),
        yaxis = list(title = "", range = c(0, 100), ticksuffix = "%"),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
        title = list(text = " ", font = list(size = 20))
      )
  })
  
  # ---- SECTION 3: Labour Overview (server) ----
  
  
  # Helper: scale 0–1 to 0–100
  to_percent_0_100 <- function(v) {
    v <- suppressWarnings(as.numeric(v))
    if (all(is.na(v))) return(v)
    mx <- suppressWarnings(max(v, na.rm = TRUE))
    if (is.finite(mx) && mx <= 1) v * 100 else v
  }
  
  intro3_filtered <- reactive({
    req(input$overview_state, input$intro3_sector)
    
    want_state  <- tolower(stringr::str_squish(as.character(input$overview_state)))
    want_sector <- suppressWarnings(as.numeric(input$intro3_sector))  # 0=Rural, 1=Urban, 99=All
    want_year   <- 2023
    
    # 
    df0 <- intro_section3 %>%
      dplyr::rename_with(tolower)
    
    # Pick a state-name column 
    name_candidates <- c("state.1", "statename", "state_name", "state")
    name_col <- (names(df0)[names(df0) %in% name_candidates])[1]
    validate(need(length(name_col) == 1,
                  "Section 3: Could not find a state-name column (state/state.1/statename/state_name)."))
    
    # Pick the year column 
    year_col <- if ("year" %in% names(df0)) "year" else if ("yr" %in% names(df0)) "yr" else NA_character_
    validate(need(!is.na(year_col), "Section 3: Could not find a year column ('year' or 'yr')."))
    
    df0 <- df0 %>%
      dplyr::mutate(
        state_key  = tolower(stringr::str_squish(as.character(.data[[name_col]]))),
        year_num   = suppressWarnings(as.numeric(.data[[year_col]])),
        sector_num = suppressWarnings(as.numeric(.data[["sector"]]))
      )
    
    # --- STATE filter (strict National = exactly 'india') ---
    cand_state <- if (identical(input$overview_state, "National")) {
      df0 %>% dplyr::filter(state_key == "india")
    } else {
      df0 %>% dplyr::filter(state_key == want_state)
    }
    validate(need(nrow(cand_state) > 0,
                  paste0("Section 3: No rows for state=", input$overview_state, " (via '", name_col, "').")))
    
    # --- YEAR filter ---
    cand_year <- cand_state %>% dplyr::filter(year_num == want_year)
    validate(need(nrow(cand_year) > 0,
                  paste0("Section 3: No rows for state=", input$overview_state, ", year=", want_year, ".")))
    
    # --- SECTOR filter (no averaging; sector is authoritative) ---
    out <- cand_year %>% dplyr::filter(sector_num == want_sector)
    validate(need(nrow(out) > 0,
                  paste0("Section 3: No row for state=", input$overview_state,
                         ", year=", want_year, ", sector=", want_sector, ".")))
    
    dplyr::slice(out, 1)  
  })
  
  # --- Bar chart: lf_cws, emp_cws, unpaid_emp, unemp_cws, neet ---
  output$intro3_bar <- renderPlotly({
    df <- intro3_filtered()
    
    vals <- c(
      WPR                          = df$emp_cws,
      LFPR                         = df$lf_cws,
      `Share of unpaid employment` = df$unpaid_emp,
      `Unemployment Rate`          = df$unemp_cws,
      `NEET (15–29 years)`         = df$neet
    )
    
    plot_df <- tibble::tibble(
      Indicator = factor(names(vals),
                         levels = c("WPR", "LFPR", "Share of unpaid employment",
                                    "Unemployment Rate", "NEET (15–29 years)")),
      Value = to_percent_0_100(unlist(vals, use.names = FALSE))
    )
    
    plot_ly(
      data = plot_df,
      x = ~Indicator, y = ~Value, type = "bar",
      marker = list(color = c("#E41A1C", "#e89f00", "#00BF7D", "#007191", "#4a2377")),
      text = ~paste0(round(Value, 1), "%"),
      textposition = "inside",
      textfont = list(color = "white", size = 15, family = "Arial Black"),
      hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<extra></extra>"
    ) %>%
      layout(
        margin = list(t = 30, r = 10, b = 60, l = 50),
        xaxis = list(title = ""),
        yaxis = list(title = "As share of population (%)", range = c(0, 100), ticksuffix = "%"),
        showlegend = FALSE
      )
  })
  
  # --- Tiles: lfpr_female, unemp_female ---
  output$intro3_tile_female_lfpr <- renderValueBox({
    df  <- intro3_filtered()
    val <- to_percent_0_100(df$lfpr_female)
    valueBox(
      value    = ifelse(is.na(val), "N/A", paste0(round(val, 1), "%")),
      subtitle = "Female LFPR",
      icon     = icon("person"),
      color    = "navy"
    )
  })
  
  output$intro3_tile_female_unemp <- renderValueBox({
    df  <- intro3_filtered()
    val <- to_percent_0_100(df$unemp_female)
    valueBox(
      value    = ifelse(is.na(val), "N/A", paste0(round(val, 1), "%")),
      subtitle = "Female Unemployment Rate",
      icon     = icon("chart-line"),
      color    = "maroon"
    )
  })
  
  
  
  ###########################################################################################
  ####### Tab 2a: Labour Market #######
  ###########################################################################################
  
  ######## TAB 2Z: Labour Profile ########
  #––––– Filter reactive data for Labour Profile –––––#
  filtered_labprof <- reactive({
    req(input$labprof_state, input$labprof_sector, input$labprof_year)
    labour_profile_df %>%
      dplyr::filter(
        state == input$labprof_state,
        sector == input$labprof_sector,
        year == input$labprof_year
      )
  })
  
  
  #####download button#####
  ##### Download button #####
  output$download_tab2z <- downloadHandler(
    filename = function() {
      paste0("Labour_Profile_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create a temporary ZIP file
      tmpzip <- tempfile(fileext = ".zip")
      
      # Files to include
      files_to_zip <- c(file_labour_1, file_labour_2)
      existing <- files_to_zip[file.exists(files_to_zip)]
      
      if (length(existing) == 0) {
        stop("No source files found in ", download_dir)
      }
      
      # Create the ZIP archive
      zip::zipr(zipfile = tmpzip, files = existing)
      
      # Copy ZIP to Shiny download location
      file.copy(tmpzip, file, overwrite = TRUE)
      
      # Cleanup
      on.exit(unlink(tmpzip), add = TRUE)
    },
    contentType = "application/zip"
  )
  
  
  #––––– GENDER COMPOSITION PIE CHART –––––#
  output$gender_comp_plot <- renderPlotly({
    df <- filtered_labprof()
    validate(need(nrow(df) > 0, "No data available for selected filters."))
    
    pie_df <- data.frame(
      Gender = c("Male", "Female"),
      Value = c(df$sex1_male[1] * 100, df$sex1_female[1] * 100)
    )
    
    plot_ly(
      data = pie_df,
      labels = ~Gender,
      values = ~Value,
      type = "pie",
      text = ~paste0(Gender, ": ", sprintf("%.2f", Value), "%"), 
      textinfo = "text",
      insidetextfont = list(color = "white", size = 13),
      marker = list(colors = c("#007191", "#f55f74")),
      hoverinfo = "label+value+percent"
    ) %>%
      layout(
        title = list(text = "Gender Composition", y = 0.95, yanchor = "top"),
        margin = list(t = 80),  # gap above chart
        legend = list(orientation = "h", x = 0.25, y = 1.15)
      )
  })
  
  
  #––––– SHARE OF WORKING AGE POPULATION –––––#
  output$working_age_plot <- renderPlotly({
    df <- filtered_labprof()
    validate(need(nrow(df) > 0, "No data available for selected filters."))
    
    bar_df <- data.frame(
      Category = factor(c("Female", "Male", "All"), levels = c("Female", "Male", "All")),
      Value = c(df$working_age_female[1], df$working_age_male[1], df$working_age[1]) * 100
    )
    
    
    plot_ly(
      data = bar_df,
      x = ~Category,
      y = ~Value,
      type = "bar",
      text = ~paste0("<b>", round(Value, 1), "%</b>"),  
      textposition = "auto",
      textfont = list(family = "Arial", size = 14, weight = "bold"),
      marker = list(color = c("#4a2377", "#007191", "#f55f74"))
    ) %>%
      layout(
        title = list(text = "Share of Working Age population", y = 0.95, yanchor = "top"),
        margin = list(t = 80),  # spacing above chart
        yaxis = list(title = "Percentage (%)", range = c(0, 100)),
        xaxis = list(title = ""),
        legend = list(orientation = "h", x = 0.25, y = 1.1)
      )
  })
  
  #––––– AGE COHORT BAR CHART –––––#
  output$age_cohort_plot <- renderPlotly({
    df <- filtered_labprof()
    validate(need(nrow(df) > 0, "No data available for selected filters."))
    
    cohort_df <- data.frame(
      Age_Cohort = c("15–29", "30–44", "45–64", "65+"),
      All = c(df$age_cohort_1[1], df$age_cohort_2[1], df$age_cohort_3[1], df$age_cohort_4[1]) * 100,
      Male = c(df$age_cohort_male_1[1], df$age_cohort_male_2[1], df$age_cohort_male_3[1], df$age_cohort_male_4[1]) * 100,
      Female = c(df$age_cohort_female_1[1], df$age_cohort_female_2[1], df$age_cohort_female_3[1], df$age_cohort_female_4[1]) * 100
    ) %>%
      tidyr::pivot_longer(
        cols = c("All", "Male", "Female"),
        names_to = "Sex",
        values_to = "Value"
      ) %>%
      dplyr::mutate(
        Sex = factor(Sex, levels = c("Female", "Male", "All"))
      )
    
    
    plot_ly(
      data = cohort_df,
      x = ~Age_Cohort,
      y = ~Value,
      color = ~Sex,
      colors = c("Female" = "#f55f74", "Male" = "#007191", "All" = "#4a2377"),
      type = "bar",
      text = ~round(Value, 1),
      textposition = "auto"
    ) %>%
      layout(
        barmode = "group",
        title = list(text = "Age Cohort", y = 0.95, yanchor = "top"),
        margin = list(t = 80),  
        yaxis = list(title = "Percentage (%)", range = c(0, 100)),
        xaxis = list(title = "Age Group"),
        legend = list(orientation = "h", x = 0.25, y = 1.1)
      )
  })
  
  
  # --- Row 2: Age Cohort, Education & Vocational Training ---
  #––––– Filter reactive data for second-row charts –––––#
  filtered_intro2 <- reactive({
    req(input$labprof_state, input$labprof_sector, input$labprof_year2)
    labour_intro2 %>%
      dplyr::filter(
        state == input$labprof_state,
        sector == input$labprof_sector,
        year == input$labprof_year2
      )
  })
  
  
  filter_by_demographics <- function(df, demo_sel) {
    if (demo_sel == "All") {
      df <- dplyr::filter(df, dem == "All")
    } else if (demo_sel == "Working Age") {
      df <- dplyr::filter(df, dem == "Working_Age")
    } else if (demo_sel %in% c("15 to 29", "30 to 44", "45 to 64", "65 and above")) {
      df <- dplyr::filter(df, age_cohort == demo_sel)
    }
    df
  }
  
  
  # --- EDUCATION COMPOSITION (stacked, Y = year)
  output$edu_comp_plot <- renderPlotly({
    df <- filtered_intro2()
    validate(need(nrow(df) > 0, "No data available for selected filters."))
    
    df <- filter_by_demographics(df, input$labprof_demo)
    validate(need(nrow(df) > 0, "No data available for selected demographics."))
    
    edu_df <- data.frame(
      Education_Level = c("Below Primary", "Primary / Middle", "Secondary", "Tertiary"),
      Female = c(
        dplyr::first(df$edu_cat_female_1, default = NA_real_),
        dplyr::first(df$edu_cat_female_2, default = NA_real_),
        dplyr::first(df$edu_cat_female_3, default = NA_real_),
        dplyr::first(df$edu_cat_female_4, default = NA_real_)
      ) * 100,
      Male = c(
        dplyr::first(df$edu_cat_male_1, default = NA_real_),
        dplyr::first(df$edu_cat_male_2, default = NA_real_),
        dplyr::first(df$edu_cat_male_3, default = NA_real_),
        dplyr::first(df$edu_cat_male_4, default = NA_real_)
      ) * 100,
      All = c(
        dplyr::first(df$edu_cat_1, default = NA_real_),
        dplyr::first(df$edu_cat_2, default = NA_real_),
        dplyr::first(df$edu_cat_3, default = NA_real_),
        dplyr::first(df$edu_cat_4, default = NA_real_)
      ) * 100
    ) %>%
      tidyr::pivot_longer(
        cols = c("Female", "Male", "All"),
        names_to = "Sex",
        values_to = "Value"
      ) %>%
      dplyr::mutate(
        Sex = factor(Sex, levels = c("Female", "Male", "All"))
      )
    
    plot_ly(
      data = edu_df,
      x = ~Education_Level,
      y = ~Value,
      color = ~Sex,
      colors = c("Female" = "#f55f74", "Male" = "#007191", "All" = "#4a2377"),
      type = "bar",
      text = ~paste0(round(Value, 1), "%"),
      textposition = "auto"
    ) %>%
      layout(
        barmode = "group",
        title = list(text = "Education Composition", y = 0.95, yanchor = "top"),
        margin = list(t = 80),
        yaxis = list(title = "Percentage (%)", range = c(0, 100)),
        xaxis = list(title = ""),
        legend = list(orientation = "h", x = 0.3, y = 1.1)
      )
  })
  
  
  
  # --- % WITH VOCATIONAL TRAINING (stacked, Y = year)
  output$voc_train_plot <- renderPlotly({
    df <- filtered_intro2()
    validate(need(nrow(df) > 0, "No data available for selected filters."))
    
    df <- filter_by_demographics(df, input$labprof_demo)
    validate(need(nrow(df) > 0, "No data available for selected demographics."))
    
    voc_df <- data.frame(
      Sex = factor(c("Female", "Male", "All"), levels = c("Female", "Male", "All")),
      Value = c(
        dplyr::first(df$vocat1_female, default = NA_real_),
        dplyr::first(df$vocat1_male, default = NA_real_),
        dplyr::first(df$vocat1, default = NA_real_)
      ) * 100
    )
    
    plot_ly(
      data = voc_df,
      x = ~Sex,
      y = ~Value,
      type = "bar",
      text = ~paste0("<b>", round(Value, 1), "%</b>"),
      textposition = "auto",
      textfont = list(family = "Arial",  size = 13, weight = "bold"),
      marker = list(color = c("#f55f74", "#007191", "#4a2377"))
      
      
    ) %>%
      layout(
        title = list(text = "% Vocational Training", y = 0.95, yanchor = "top"),
        margin = list(t = 80),
        yaxis = list(title = "Percentage (%)", range = c(0, 100)),
        xaxis = list(title = ""),
        legend = list(orientation = "h", x = 0.3, y = 1.1)
      )
  })
  
  
  ######## Tab 2a — Labour Indicators (XLSX) ########
  output$download_tab2a <- downloadHandler(
    filename = function() paste0("Labour_Indicators_", Sys.Date(), ".xlsx"),
    content = function(file) {
      file.copy(file_labour_indicators, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  
  ######## Tab 2b — Quality of Employment (XLSX) ########
  output$download_tab2b <- downloadHandler(
    filename = function() paste0("Job_Quality_", Sys.Date(), ".xlsx"),
    content = function(file) {
      file.copy(file_JobQuality, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  
  ######## Tab 2c — Distribution of Real Earnings (XLSX) ########
  output$download_tab2c <- downloadHandler(
    filename = function() paste0("RealEarningsDistribution_", Sys.Date(), ".xlsx"),
    content = function(file) {
      file.copy(file_real_earnings, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  
  
  
  ###
  
  # Unified demographic filter
  apply_demo_filter <- function(df, sel) {
    df <- df %>% dplyr::mutate(dem = trimws(dem))
    if (identical(sel, "dem") || is.null(sel) || is.na(sel)) {
      return(df %>% dplyr::filter(dem == "All"))
    }
    parts <- strsplit(sel, "::", fixed = TRUE)[[1]]
    if (length(parts) != 2) return(df)
    
    col <- parts[1]; raw <- parts[2]
    if (!col %in% names(df)) return(df[0, , drop = FALSE])
    
    if (is.numeric(df[[col]])) {
      val <- suppressWarnings(as.numeric(raw))
      df %>% dplyr::filter(.data[[col]] == val)
    } else {
      norm <- function(x) tolower(trimws(x))
      df %>% dplyr::filter(norm(.data[[col]]) == norm(raw))
    }
  }
  
  # Helper to map "National" -> "India"
  normalize_state <- function(x) ifelse(identical(x, "National"), "India", x)
  
  # Toggle if your indicators are already percent (0–100)
  SCALE_TO_PERCENT <- TRUE
  
  ########## TAB 2a: Line chart (Key Labour Indicators)
  
  output$labour_line <- renderPlotly({
    req(input$lab_state, input$lab_sector, input$lab_indicator, input$lab_demo)
    state_val <- normalize_state(input$lab_state)
    
    df <- labour_df %>%
      dplyr::mutate(
        dem = trimws(dem),
        sector_num = suppressWarnings(as.integer(as.character(sector)))
      ) %>%
      dplyr::filter(
        statename == state_val,
        if (input$lab_sector == 99) TRUE else sector_num == input$lab_sector
      )
    
    df <- apply_demo_filter(df, input$lab_demo)
    
    validate(
      need(nrow(df) > 0, "No data for the selected filters."),
      need(input$lab_indicator %in% names(df), "Selected indicator not found.")
    )
    
    df_plot <- df %>%
      dplyr::select(year, value = dplyr::all_of(input$lab_indicator)) %>%
      dplyr::arrange(year) %>%
      dplyr::distinct(year, .keep_all = TRUE) %>%     
      dplyr::mutate(
        value   = if (SCALE_TO_PERCENT) value * 100 else value,
        tooltip = paste0("Year: ", year, "<br>Value: ",
                         if (SCALE_TO_PERCENT) sprintf("%.2f%%", value) else sprintf("%.2f", value))
      )
    
    
    plot_ly(df_plot, x = ~year, y = ~value,
            type = "scatter", mode = "lines+markers",
            text = ~tooltip, hoverinfo = "text",
            line = list(width = 4),
            marker = list(size = 10)) %>%
      layout(xaxis = list(title = "Year", type = "category"),
             yaxis = list(title = "", ticksuffix = if (SCALE_TO_PERCENT) "%" else ""))
  })
  
  ###########################################################################################
  ####### Tab 2b: Quality of Employment #######
  
  get_labour_filtered_data <- reactive({
    req(input$lab2b_state, input$lab2b_sector, input$lab_quality_demo, input$lab_quality_year)
    state_val <- normalize_state(input$lab2b_state)
    
    df <- labour_df %>%
      dplyr::mutate(
        dem = trimws(dem),
        sector_num = suppressWarnings(as.integer(as.character(sector)))
      ) %>%
      dplyr::filter(
        statename == state_val,
        if (input$lab2b_sector == 99) TRUE else sector_num == input$lab2b_sector,
        year == input$lab_quality_year
      )
    
    df <- apply_demo_filter(df, input$lab_quality_demo)
    validate(need(nrow(df) > 0, "No data for selected filters."))
    df
  })
  
  # TAB 2b-1 Employment by sector
  colors_sector <- c("Agriculture" = "#E41A1C",
                     "Industries"  = "#FFDB58",
                     "Services"    = "#4a2377")
  
  output$emp_sector_chart <- renderPlotly({
    df <- get_labour_filtered_data()
    sector_data <- df %>%
      dplyr::slice(1) %>%
      dplyr::select(agri_cws, ind_cws, services_cws) %>%
      tidyr::pivot_longer(everything(), names_to = "sector", values_to = "value") %>%
      dplyr::mutate(
        sector = dplyr::recode(sector,
                               agri_cws = "Agriculture",
                               ind_cws = "Industries",
                               services_cws = "Services"),
        value = round(value * 100, 2),
        color = colors_sector[sector]
      )
    plot_ly(sector_data, x = ~sector, y = ~value, type = 'bar',
            text = ~paste0(value, "%"), textposition = 'auto',
            textfont = list(size = 14, family = "Arial Black"),
            marker = list(color = ~color)) %>%
      layout(title = "Employment Sector", yaxis = list(title = "Share (%)", range = c(0, 100)),
             xaxis = list(title = ""), showlegend = FALSE)
  })
  
  # TAB 2b-2 Employment by type
  colors_type <- c("#4a2377", "#0d7d87", "#f55f74")
  
  output$emp_type_chart <- renderPlotly({
    df <- get_labour_filtered_data()
    type_data <- df %>%
      dplyr::slice(1) %>%
      dplyr::select(salaried, casual, self_emp) %>%
      tidyr::pivot_longer(everything(), names_to = "type", values_to = "value") %>%
      dplyr::mutate(
        type = dplyr::recode(type,
                             salaried = "Regular",
                             casual = "Casual",
                             self_emp = "Self-employed"),
        value = round(value * 100, 2)
      )
    plot_ly(type_data, x = ~type, y = ~value, type = 'bar',
            text = ~paste0(value, "%"), textposition = 'auto',
            textfont = list(size = 14, family = "Arial Black"),
            marker = list(color = colors_type)) %>%
      layout(title = "Employment Type", xaxis = list(title = ""),
             yaxis = list(title = "Share (%)", range = c(0, 100)))
  })
  
  # TAB 2b-3 Job Quality Index
  colors_jqi <- c("#d31f11", "#007191")
  
  output$job_quality_chart <- renderPlotly({
    
    
    df <- get_labour_filtered_data()
    jq_data <- df %>%
      dplyr::slice(1) %>%
      dplyr::select(JQdim215, JQdim365) %>%
      tidyr::pivot_longer(everything(), names_to = "index", values_to = "value") %>%
      dplyr::mutate(
        index = dplyr::recode(index,
                              JQdim215 = "JQI ($2.15/day)",
                              JQdim365 = "JQI ($3.65/day)"),
        value = round(value, 2)
      )
    
    plot_ly(jq_data, x = ~index, y = ~value, type = 'bar',
            text = ~value, textposition = 'auto',
            textfont = list(size = 14, family = "Arial Black"),
            marker = list(color = colors_jqi)) %>%
      layout(title = "Job Quality Index",
             xaxis = list(title = ""),
             yaxis = list(title = "Score"))
  })
  
  
  
  # TAB 2b-4 Earning Poverty
  colors_pov <- c("#d31f11", "#007191")
  
  output$earning_poverty_chart <- renderPlotly({
    
    
    df <- get_labour_filtered_data()
    pov_data <- df %>%
      dplyr::slice(1) %>%
      dplyr::select(POV_215, POV_365) %>%
      tidyr::pivot_longer(everything(), names_to = "pov", values_to = "value") %>%
      dplyr::mutate(
        pov = dplyr::recode(pov,
                            POV_215 = "Below $2.15/day",
                            POV_365 = "Below $3.65/day"),
        value = round(value * 100, 2)
      )
    
    plot_ly(pov_data, x = ~pov, y = ~value, type = 'bar',
            text = ~paste0(value, "%"), textposition = 'auto',
            textfont = list(size = 14, family = "Arial Black"),
            marker = list(color = colors_pov)) %>%
      layout(title = "Earning Poverty",
             xaxis = list(title = ""),
             yaxis = list(title = "Share (%)", range = c(0, 100)))
  })
  
  ###########################################################################################
  ####### Tab 2c: Distribution of Real Income #######
  
  bar_colors <- c("1" = "#E41A1C", "2" = "#FFDB58",
                  "3" = "#f55f74", "4" = "#00BF7D",
                  "5" = "#007191", "All" = "#4a2377")
  
  output$real_income_bar <- renderPlotly({
    req(input$lab2c_state, input$lab2c_sector, input$realincome_year, input$realincome_demo)
    state_val <- normalize_state(input$lab2c_state)
    
    df <- wage_quintile %>%
      dplyr::filter(
        statename == state_val,
        if (input$lab2c_sector == "All") TRUE else sector == input$lab2c_sector,
        Year == input$realincome_year
      )
    
    df <- apply_demo_filter(df, input$realincome_demo)
    validate(need(nrow(df) > 0, "No data for selected filters."))
    
    df_plot <- df %>%
      dplyr::filter(!is.na(real_wage_23)) %>%
      dplyr::mutate(
        quint_label = ifelse(quint_label == "Total", "All", quint_label),
        quint_label = factor(quint_label, levels = c("1", "2", "3", "4", "5", "All"))
      ) %>%
      dplyr::group_by(quint_label) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        label = sprintf("₹%.2f", real_wage_23),
        color = bar_colors[as.character(quint_label)]
      )
    
    plot_ly(df_plot, x = ~quint_label, y = ~real_wage_23,
            type = "bar",
            text = ~paste0("Quintile: ", quint_label, "<br>Income: ", label),
            
            hoverinfo = "text",
            marker = list(color = ~color)) %>%
      layout(yaxis = list(title = "Real Earnings (INR/day)", tickformat = ".2f"),
             xaxis = list(title = "Quintile"),
             title = "")
  })
  
  
  
  
  
  ###########################################################################################
  ####### Tab 3a: Monetary Welfare #######
  
  #DOWNLOAD FILES#
  
  output$download_tab3a <- downloadHandler(
    filename = function() paste0("Monetary_Welfare_Data_", Sys.Date(), ".zip"),
    content = function(file) {
      tmpzip <- tempfile(fileext = ".zip")
      zip::zipr(
        zipfile = tmpzip,
        files = c(file_monetary_section1, file_monetary_section2, file_monetary_section3)
      )
      file.copy(tmpzip, file)
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
      if (input$mw_demo %in% names(df)) {
        df <- df %>% filter(.data[[input$mw_demo]] == 1)
      } else {
        showNotification(
          paste("Column", input$mw_demo, "not found in data."),
          type = "error"
        )
        return(NULL)
      }
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
  
  
  output$mw_line_note <- renderUI({
    req(input$mw_ppp)
    msg <- if (input$mw_ppp == "2017") {
      "$2.15/day and PPP-2017"
    } else {
      "$3.00/day and PPP-2021"
    }
    tags$div(
      tags$strong(msg),
      style = "text-align:center; margin-top:14px;"
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
    } else if (grepl("::", input$mon2_demo)) {
      demo_split <- strsplit(input$mon2_demo, "::")[[1]]
      if (length(demo_split) == 2) {
        col_name <- demo_split[1]
        filter_val <- demo_split[2]
        
        if (!is.null(filter_val) && filter_val != "ALL") {
          if (col_name %in% c("gen_cat", "owns_land", "owns_dwelling")) {
            filter_val <- as.numeric(filter_val)
          }
          df <- df %>% filter(.data[[col_name]] == filter_val)
        }
      }else {
        df <- df %>% filter(inc_cat == input$mon2_demo)
      }
    }
    
    # --- Prep plotting
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
          poor_420 == 1,  
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
                        inc_salaried = "Regular",
                        inc_casual   = "Casual"),
        value = value * 100,
        value_label = sprintf("%.2f", value),
        plot_value = ifelse(value == 0, 0.0001, value),  
        orig_value = value                  
      )
    
    custom_palette <- c("#4a2377", "#0d7d87", "#f55f74")
    group_colors <- setNames(custom_palette[seq_along(unique(df_long$group))], unique(df_long$group))
    
    plot_ly(
      df_long,
      x = ~source,
      y = ~plot_value,
      color = ~group,
      colors = group_colors,
      type = "bar",
      hovertext = ~paste0(source, "<br>", value_label, "%"),
      hoverinfo = "text",
      text = ~paste0(sprintf("%.2f", orig_value), "%"), 
      textposition = "outside",
      textfont = list(size = 14, color = "#010101", family = "Arial"),
      cliponaxis = FALSE   
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
          poor_420 == 0,  
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
                        inc_salaried = "Regular",
                        inc_casual   = "Casual"),
        value = value * 100,
        value_label = sprintf("%.2f", value),
        plot_value = ifelse(value == 0, 0.0001, value),
        orig_value = value
      )
    
    custom_palette <- c("#4a2377", "#0d7d87", "#f55f74")
    group_colors <- setNames(custom_palette[seq_along(unique(df_long$group))], unique(df_long$group))
    
    plot_ly(
      df_long,
      x = ~source,
      y = ~plot_value,
      color = ~group,
      colors = group_colors,
      type = "bar",
      hovertext = ~paste0(source, "<br>", value_label, "%"),
      hoverinfo = "text",
      text = ~paste0(sprintf("%.2f", orig_value), "%"),
      textposition = "outside",
      textfont = list(size = 14, color = "#010101", family = "Arial") ,
      cliponaxis = FALSE
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
    filename = function() paste0("NonMonetary_Welfare_Data_", Sys.Date(), ".zip"),
    content = function(file) {
      tmpzip <- tempfile(fileext = ".zip")
      zip::zipr(
        zipfile = tmpzip,
        files = c(file_nonmon_section1, file_nonmon_section2, file_nonmon_section3)
      )
      file.copy(tmpzip, file)
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
  output$nonmon_graph1 <- renderPlotly({
    req(input$tab3b_state, input$tab3b_sector, input$nmw_demo_combined)
    
    # Filter based on state, sector, and round
    df <- nonmon_1 %>%
      filter(
        (input$tab3b_state == "National" & StateName == "India") |
          (input$tab3b_state != "National" & StateName == input$tab3b_state),
        if (input$tab3b_sector != 99) sector == input$tab3b_sector else TRUE,
        Year %in% c(2015, 2019)
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
    
    df <- df %>%
      filter(!(is.na(m_poor_1_33) & is.na(m_poor_1_20))) %>%
      arrange(Year) %>%
      distinct(Year, .keep_all = TRUE)
    
    df_long <- df %>%
      select(Year, m_poor_1_33, m_poor_1_20) %>%
      pivot_longer(cols = c(m_poor_1_33, m_poor_1_20),
                   names_to = "status_raw",
                   values_to = "headcount") %>%
      mutate(
        year = as.character(Year),
        status = recode(status_raw,
                        m_poor_1_33 = "MPI Poor",
                        m_poor_1_20 = "MPI Vulnerable"),
        status = factor(status, levels = c("MPI Poor", "MPI Vulnerable")),
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
      color = ~status,
      colors = c("MPI Poor" = "#007191", "MPI Vulnerable" = "#d31f11"), 
      type = "scatter",
      mode = "lines+markers",
      line = list(width = 4),       
      marker = list(size = 10),  
      hovertext = ~paste0(
        "Year: ", year,
        "<br>Poverty Line: ", status,
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
    
    df <- df %>% distinct(round, .keep_all = TRUE)
    
    if (nrow(df) == 0) return(NULL)
    
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
  ### DOWNLOAD ###
  output$download_tab4a <- downloadHandler(
    filename = function() "Monetary_Schemes.zip",
    content = function(file) {
      
      tmpdir <- tempdir()
      if (!dir.exists(tmpdir)) dir.create(tmpdir, recursive = TRUE)
      tmpzip <- tempfile(fileext = ".zip")
      
      
      files_to_zip <- c(file_monetary_schemes, file_monetary_poor)
      existing <- files_to_zip[file.exists(files_to_zip)]
      if (length(existing) == 0) {
        stop("No source files found in ", download_dir)
      }
      
      
      zip::zipr(zipfile = tmpzip, files = existing)
      
      
      file.copy(tmpzip, file, overwrite = TRUE)
      
      
      on.exit(unlink(tmpzip), add = TRUE)
    },
    contentType = "application/zip"
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
        value_label = ifelse(
          is.na(value), "",
          ifelse(value == 0, "0%", sprintf("%.2f%%", value))
        )
        
      )
    
    # Plot
    plot_ly(
      df_long,
      x = ~quintile,
      y = ~value,
      color = ~paste(indicator, year, sep = " – "),
      colors = c("#4a2377",
                 "#f47a00","#E41A1C","#00BF7D"), 
      type = "bar",
      text = ~value_label,
      textposition = "outside",
      cliponaxis = FALSE,
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
  
  
  ### poor / non-poor monetary charts 
  render_mp_poor_plot <- function(df, palette) {
    req(nrow(df) > 0)
    
    plot_ly(
      df,
      x = ~indicator_label,
      y = ~value_percent,
      color = ~year_label,          
      colors = palette,
      type = "bar",
      text = ~value_label,
      textposition = "outside",
      cliponaxis = FALSE,
      insidetextfont  = list(size = 14, color = "#ffffff"),
      outsidetextfont = list(size = 14, color = "#000000"),
      hovertext = ~paste0(
        "Indicator: ", indicator_label,
        "<br>Year: ", year_label,
        "<br>Value: ", value_label
      ),
      hoverinfo = "text",
      showlegend = TRUE             
    ) %>% layout(
      showlegend = TRUE,            
      legend = list(
        title = list(text = "Year"),
        orientation = "v",
        x = 1, y = 1           
      ),
      barmode = "group",
      bargap = 0.05,
      xaxis = list(title = "", tickangle = 45),
      yaxis = list(title = "Percentage (%)", range = c(0, 100))
    )
  }
  
  
  
  ### Reactive filter using graph2_mp
  filtered_data_mp_poor <- reactive({
    req(input$mp_state,input$mp_sector,input$mp_indicators)
    
    sec <- switch(input$mp_sector,
                  "rural" = 0,
                  "urban" = 1,
                  "all"   = 99)
    
    graph2_mp %>%
      filter(state == input$mp_state, sector == sec) %>%
      select(state, sector, year, poor_420, any_of(input$mp_indicators)) %>%
      pivot_longer(
        cols = any_of(input$mp_indicators),
        names_to="indicator_code", values_to="value"
      ) %>%
      mutate(
        indicator_label = purrr::map_chr(indicator_code,~mon_indicator_labels[[.x]]),
        year_label      = factor(year,levels=c(2011,2022),labels=c("2011-12","2022-23")),
        value_percent   = value*100,
        value_label = ifelse(is.na(value_percent), "",
                             ifelse(value_percent == 0, "0%", sprintf("%.2f%%", value_percent)))
        
      )
  })
  
  ### Poor households chart ###
  output$monetary_bar_poor <- renderPlotly({
    df <- filtered_data_mp_poor() %>% filter(poor_420 == 1)
    render_mp_poor_plot(df, c("#4a2377","#0d7d87","#f55f74"))
  })
  
  ### Non-poor households chart ###
  output$monetary_bar_nonpoor <- renderPlotly({
    df <- filtered_data_mp_poor() %>% filter(poor_420 == 0)
    render_mp_poor_plot(df, c("#4a2377","#0d7d87","#f55f74"))
  })
  
  
  ###########################################################################################
  ####### Tab 4b: Non‑monetary Poverty #######
  #######download button
  output$download_tab4b <- downloadHandler(
    filename = function() "NonMonetary_Schemes.zip",
    content = function(file) {
      tmpdir <- tempdir()
      if (!dir.exists(tmpdir)) dir.create(tmpdir, recursive = TRUE)
      tmpzip <- tempfile(fileext = ".zip")
      
      files_to_zip <- c(file_nonmon_schemes, file_nonmon_poor)
      existing <- files_to_zip[file.exists(files_to_zip)]
      if (length(existing) == 0) stop("No source files found in ", download_dir)
      
      zip::zipr(zipfile = tmpzip, files = existing)
      file.copy(tmpzip, file, overwrite = TRUE)
      on.exit(unlink(tmpzip), add = TRUE)
    },
    contentType = "application/zip"
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
  palette_poor <- c("#4a2377","#00BF7D","#0d7d87")
  palette_nonpoor <- c("#4a2377","#00BF7D","#0d7d87")
  
  
  # plot renderer for poor or non-poor groups
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
  
  
  
  
  
  #######Quintile Graph
  
  ##COLOR
  .quint_palettes <- list(
    c("#4a2377", "#f55f74" ),
    c("#0d7d87", "#E41A1C"),
    c("#4a2377", "#00BF7D")
  )
  
  
  filtered_data_nm_quint <- reactive({
    req(input$nm_state, input$nm_sector, input$nm_indicators)
    
    df <- graph2_nm %>%
      filter(
        state_numeric == input$nm_state,
        sector == input$nm_sector
      ) %>%
      pivot_longer(
        cols = tidyselect::all_of(input$nm_indicators),
        names_to = "indicator_code",
        values_to = "value"
      ) %>%
      distinct(Year, wealth_index, indicator_code, .keep_all = TRUE) %>%  
      mutate(
        indicator_label = purrr::map_chr(indicator_code, ~ indicator_labels[[.x]]),
        Year_label = case_when(
          Year == 2015 ~ "2015-16",
          Year == 2019 ~ "2019-21",
          TRUE ~ as.character(Year)
        ),
        wealth_label = factor(paste0("Q", wealth_index), levels = paste0("Q", 1:5)),
        value_percent = value * 100,
        value_label = sprintf("%.2f%%", value_percent)
      )
    
    df
  })
  
  
  
  
  # up to 3 selected indicators
  output$nm_quintile_plot_ui <- renderUI({
    req(input$nm_indicators)
    n <- min(length(input$nm_indicators), 3)
    
    tagList(
      lapply(seq_len(n), function(i) {
        box(
          width = 12,
          title = tags$div(
            style = "text-align:center; font-weight:bold;",
            paste0(
              
              indicator_labels[[ input$nm_indicators[i] ]]
            )          ),
          plotlyOutput(paste0("non_monetary_bar_quintile_", i), height = 450)
        )
      })
    )
  })
  
  # Render up to 3 individual wealth-quintile plots
  observe({
    req(input$nm_indicators)
    inds <- input$nm_indicators
    n     <- min(length(inds), 3)
    
    for (i in seq_len(n)) {
      local({
        ii <- i
        indicator_code <- inds[ii]
        
        output[[paste0("non_monetary_bar_quintile_", ii)]] <- plotly::renderPlotly({
          
          df <- filtered_data_nm_quint() %>%
            dplyr::filter(indicator_code == inds[ii])
          
          pal <- .quint_palettes[[ ((ii - 1) %% length(.quint_palettes)) + 1 ]]
          
          plotly::plot_ly() %>%
            plotly::add_bars(
              data = df %>% dplyr::filter(Year_label == "2015-16"),
              x = ~wealth_label, y = ~value_percent,
              name = "2015-16",
              legendgroup = ii,
              marker = list(color = pal[1]),
              showlegend = TRUE,
              text = ~value_label,
              textposition = "auto",
              insidetextfont  = list(size = 14, color = "#ffffff"),
              outsidetextfont = list(size = 14, color = "#000000")
            ) %>%
            plotly::add_bars(
              data = df %>% dplyr::filter(Year_label == "2019-21"),
              x = ~wealth_label, y = ~value_percent,
              name = "2019-21",
              legendgroup = ii,
              marker = list(color = pal[2]),
              showlegend = TRUE,
              text = ~value_label,
              textposition = "auto",
              insidetextfont  = list(size = 14, color = "#ffffff"),
              outsidetextfont = list(size = 14, color = "#000000")
            ) %>%
            plotly::layout(
              barmode = "group",
              bargap = 0.10,
              bargroupgap = 0.02,
              xaxis = list(title = ""),
              yaxis = list(title = "Percentage (%)", range = c(0, 100)),
              legend = list(
                orientation = "v", x = 1.02, xanchor = "left",
                y = 0.5,  yanchor = "middle"
              ),
              margin = list(r = 90, b = 70)
            ) %>%
            plotly::config(displayModeBar = FALSE, displaylogo = FALSE)  
        })
      })
    }
  })
  
  
  ###### Tab 5: State Comparison ######
  ###Indicator title UI###
  comp_choices <- list(
    "Macroeconomic and Fiscal Indicators" = list(
      "Real GVA as % of India GVA" = "state_share_india",
      "Fiscal Health Indicator" = "fhiscore",
      "Mean Real Welfare Aggregate (2021 prices)" = "welfare_agg21_sp",
      "Mean State Aggregate as share of Mean National Aggregate" = "welfare_pct_india"
    ),
    "Poverty Measures" = list(
      "Extreme Poverty ($3.00, 2021PPP, 2011)" = "poor_300_2011",
      "Extreme Poverty ($3.00, 2021PPP, 2022)" = "poor_300_2022",
      "LMIC Poverty ($4.20, 2011 PPP)"   = "poor_420_2011",
      "LMIC Poverty ($4.20, 2022 PPP)"   = "poor_420_2022",
      "% Multidimensionally poor (2019, 33%)" = "m_poor_1_33_2019",
      "% Multidimensionally poor (2015, 33%)" = "m_poor_1_33_2015",
      "% Multidimensionally poor (2019, 20%)" = "m_poor_1_20_2019",
      "% Multidimensionally poor (2015, 20%)" = "m_poor_1_20_2015"
    ),
    "Components of Non-monetary Poverty" = list(
      "MPI - Education"    = "mpi_edu",
      "MPI - Attendance"   = "mpi_att",
      "MPI - Child Mortality" = "mpi_cm",
      "MPI - Nutrition"    = "mpi_nutri",
      "MPI - Electricity"  = "mpi_elec",
      "MPI - Toilet"       = "mpi_toilet",
      "MPI - Water"        = "mpi_water",
      "MPI - Fuel"         = "mpi_fuel",
      "MPI - Asset"        = "mpi_asset"
    ),
    "Labour Market Indicators" = list(
      "LFPR" = "lf_cws",
      "WPR" = "emp_cws",
      "Unemployment Rate" = "unemp_cws",
      "Share Unpaid Workers" = "unpaid_emp",
      "NEET (15–29 years)" = "neet",
      "JQI — $3.65/day (2017 PPP)" = "JQdim365",
      "JQI — $2.15/day (2017 PPP)" = "JQdim215",
      "Earnings Poverty — <$3.65/day" = "POV_365",
      "Earnings Poverty — <$2.15/day" = "POV_215",
      "Median Real Earnings (₹, 2023)" = "real_wage_23"
    ),
    "Social Protection and Welfare Schemes" = list(
      "% HHs who received food items from PDS shops" = "food_pds_subs",
      "% HHs who received free food items from PDS shops" = "food_pds_free",
      "% HHs receiving subsidy for LPG cylinder" = "lpg_subs",
      "% HHs receiving free electricity" = "elec_free",
      "% HHs receiving any free durable goods" = "durables_free",
      "% HHs with beneficiaries of PMJAY" = "pmjay_ben",
      "% HHs which received medical benefits from PMJAY" = "pmjay_ben_avail",
      "% HHs with BPL (ration) card ownership" = "bpl",
      "% HHs with any member having an Aadhaar card" = "aadhar",
      "% HHs accessing Anganwadi/ASHA services" = "household_health_met",
      "% HHs that received pregnancy benefits from Anganwadi/ICDS centre" = "household_has_preg_benefits",
      "% HHs that received child benefits from Anganwadi/ICDS centre" = "household_angan_benefits"
    )
  )
  
  # --- Download handler ----
  output$download_tab5 <- downloadHandler(
    filename = function() "State_Comparison.xlsx",
    content = function(file) {
      file.copy(file_state_comparison, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  
  ###
  percent_vars <- c(
    "bpl", "aadhar", "household_has_preg_benefits", "household_health_met",
    "household_angan_benefits", "food_pds_subs", "durables_free",
    "food_pds_free", "lpg_subs", "elec_free", "pmjay_ben", "pmjay_ben_avail",
    "POV_365", "POV_215", "lf_cws", "emp_cws", "unpaid_emp", "unemp_cws", "neet",
    "household_has_preg_benefits", "household_health_met","household_angan_benefits",
    
    # year-specific poverty
    "poor_300_2011", "poor_300_2022", "poor_420_2011", "poor_420_2022",
    "m_poor_1_33_2019", "m_poor_1_33_2015"
    # MPI component deprivations
    #, "m_poor_1_20_2019", "m_poor_1_20_2015""kerosene_pds_subs",
  )
  
  
  
  
  comp_filtered <- reactive({
    req(input$comp_sector)
    req(exists("comp", inherits = TRUE))
    df <- comp
    
    # Expect 'State' and text 'sector' (values: "Rural","Urban","All")
    if (!all(c("State", "sector") %in% names(df))) {
      stop("Expected columns 'State' and 'sector' in `comp`.")
    }
    
    sel <- input$comp_sector
    
    if (sel == "All") {
      if ("All" %in% unique(df$sector)) {
        df <- df %>% dplyr::filter(sector == "All")
      } else {
        # Average Rural + Urban into an All row
        df <- df %>%
          dplyr::filter(sector %in% c("Rural", "Urban")) %>%
          dplyr::group_by(State) %>%
          dplyr::summarise(
            dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop"
          ) %>%
          dplyr::mutate(sector = "All")
      }
    } else {
      df <- df %>% dplyr::filter(sector == sel)
    }
    
    df
  })
  
  
  
  # Plot:
  output$comp_plot <- renderPlotly({
    req(input$comp_indicator)
    df <- comp_filtered()
    
    ind <- input$comp_indicator
    if (!ind %in% names(df)) {
      validate(need(FALSE, paste0("Indicator '", ind, "' not found in data.")))
    }
    
    plot_df <- df %>%
      select(State, !!ind) %>%
      rename(value = !!ind) %>%
      filter(!is.na(value))
    
    if (ind %in% c("state_share_india", "Real GVA as % of India GVA")) {
      plot_df <- plot_df %>% filter(State != "India")
    }
    
    if (ind %in% percent_vars) {
      plot_df <- plot_df %>% mutate(value = value * 100)
    }
    
    plot_df <- plot_df %>%
      arrange(desc(value)) %>%
      mutate(State = factor(State, levels = State))
    
    validate(need(nrow(plot_df) > 0, "No data available for this selection."))
    
    colors  <- rep("#4e79a7", nrow(plot_df))
    
    colors[plot_df$State == "India"] <- "#4a2377" 
    
    plot_ly(
      plot_df,
      x = ~State,
      y = ~round(value, 2),
      type = "bar",
      marker = list(
        color = colors,   
        showscale = FALSE
      ),
      text = ~sprintf("%.2f", value),
      textposition = "outside",
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        ind, ": %{y:.2f}<extra></extra>"
      )
    ) %>%
      layout(
        yaxis = list(
          title = "",
          tickformat = ".2f",
          ticksuffix = if (ind %in% percent_vars) "%" else NULL
        ),
        xaxis = list(title = "State", tickangle = -35),
        margin = list(b = 90)
      )
  })
  
  # --- Indicator subtitle ----
  lookup_label <- function(val, tree) {
    for (grp in tree) {
      v <- unlist(grp, use.names = TRUE)
      hit <- names(v)[v == val]
      if (length(hit)) return(hit[1])
    }
    val
  }
  
  output$selected_indicator_title <- renderUI({
    req(input$comp_indicator)
    label <- lookup_label(input$comp_indicator, comp_choices)
    
    tags$p(
      style = "display: inline-block;
             
             font-size: 80%; 
             background-color: #ffe6e6;   
             padding: 3px 8px; 
             border-radius: 6px;
             margin-top: 5px;",
      label
    )
  })
  
  
  
  
}

#––– Run App –––
shinyApp(ui = ui, server = server)