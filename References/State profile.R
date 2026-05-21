################################################################################
# Project : State Profile
# Author : Jaeyeon(Jenny) Park(jpark36@worldbank.org)
# Date created : December 15, 2024
# Date last modified : February 24, 2025
################################################################################

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readxl)
library(tidyr)
library(htmltools)

#files
nonmonetary <- read.csv("C:/Users/jaeye/Downloads/State profile/Nonmon/nfhs2015_2019_mpi.csv")

#year column
nfhs2015$year <- 2015
nfhs2019$year <- 2019

# Combine datasets
combined_data <- bind_rows(nfhs2015, nfhs2019)

# UI
ui <- tagList(
  
  # 1) <head>
  tags$head(
    tags$style(HTML("
    .skin-red .main-header .navbar,
      .skin-red .main-header .logo {
        background-color: #C11B17 !important;
    
      /* ---- HEADER ---- */
      header.main-header,
      .navbar-static-top {
        border-bottom: none !important;
        box-shadow: none    !important;
      }

      /* nav height */
      .main-header .navbar {
        min-height: 70px !important;
      }
      .main-header .logo,
      .main-header .sidebar-toggle {
        height:    70px !important;
        line-height: 70px !important;
      }

      /* ---- SIDEBAR & CONTENT ---- */
      /* drop the sidebar down below the new header */
      .main-sidebar {
        top: 15px !important;
      }
      /* push down  */
      .content-wrapper,
      .main-footer {
        margin-top: 0px !important;
      }
      
      /* --- hide the built‑in nav toggle --- */
      .skin-red .main-header .navbar .sidebar-toggle {
        display: none !important;
      }
    ")),
    tags$script(HTML("
      // When our custom toggle is clicked, collapse/expand the sidebar
      $(document).on('click', '.custom-toggle', function(e) {
        e.preventDefault();
        $('body').toggleClass('sidebar-collapse');
      });
    "))
  
  ),
  
  # 2) dashboard CSS
  dashboardPage(
    skin = "red",
    
    
    dashboardHeader(
      titleWidth = "100%",
      title = tags$div(
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        # 1)  title
        tags$span(
          "India State Profile",
          style = "font-size:28px; font-weight:bold; color:white;"
        ),
        
        # 2) Toggle button - right
        tags$span(
          class = "custom-toggle",
          style = "color:white; font-size:20px; margin-left:10px; cursor:pointer;",
          tags$i(class = "fa fa-bars")
        ),
        
        
        # 3) logo
        tags$div(style="flex:1;"),
        
                    tags$img(src = "white.png", height = "40px")
                    
                  )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("State Profile Overview", tabName = "tab1", icon = icon("dashboard")),
      menuItem("Labour Market", tabName = "tab2", icon = icon("users")),
      menuItem("Welfare Indicators", tabName = "tab3", icon = icon("chart-line")),
      menuItem("Access to Schemes", icon = icon("hands-helping"),
               menuSubItem("Monetary Poverty", tabName = "tab4a"),
               menuSubItem("Non-monetary Poverty", tabName = "tab4b")
      ),
      menuItem("State Comparison", tabName = "tab5", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    
    tabItems(
      
      ####### Tab 4b: Non-monetary Poverty
      tabItem(tabName = "tab4b",
              
              tags$h2(
                "Non‑monetary Poverty",
                style = "font-weight: bold; color: navy; margin-bottom: 20px;"
              ),
              
              
              fluidRow(
                column(6, selectInput("nm_state", "Select State:", choices = sort(unique(combined_data$state_numeric)))),
                column(6, selectInput("nm_year", "Select Year:", choices = c(2015, 2019)))
              ),
              fluidRow(
                column(4,
                       selectInput("access_var", "Access",
                                   choices = c("BPL" = "bpl",
                                               "Aadhar" = "aadhar",
                                               "State/Central Health Insurance" = "health_insu_govt"))
                ),
                column(4,
                       selectInput("women_var", "Women Health",
                                   choices = c("Met Healthcare Worker" = "household_health_met",
                                               "Delivery Assistance" = "household_has_preg_fin",
                                               "Pregnancy Benefits" = "household_has_preg_benefits"))
                ),
                column(4,
                       selectInput("child_var", "Child Health",
                                   choices = c("Anganwadi Benefits" = "household_angan_benefits",
                                               "Anganwadi Immunization" = "household_angan_immun",
                                               "Anganwadi Early Childhood Care" = "household_angan_ecc"))
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Indicator by Quintile",
                    plotlyOutput("non_monetary_bar"))
              ),
              fluidRow(
                column(12,
                       tags$b("Note on Indicator"),
                       tags$p("The graphs display the share of each quintile and the overall average. Each quintile represents 20% of the total population/sample, ranked by the non-monetary poverty score, with Quintile 1 being the poorest 20% and Quintile 5 representing the wealthiest 20%.")
                )
              ),
              fluidRow(
                column(12, align = "right",
                       tags$em("Source: Data is sourced from the National Family Health Surveys. This analysis includes Round 4 (2015-16) and Round 5 (2019-21).")
                )
              )
      )
    )
  )
))



############## Server ############## 
server <- function(input, output, session) {
  
  # Mapping - legend names
  indicator_labels <- c(
    bpl = "BPL",
    aadhar = "Aadhar",
    health_insu_govt = "State/Central Health Insurance",
    household_health_met = "Met Healthcare Worker",
    household_has_preg_fin = "Delivery Assistance",
    household_has_preg_benefits = "Pregnancy Benefits",
    household_angan_benefits = "Anganwadi Benefits",
    household_angan_immun = "Anganwadi Immunization",
    household_angan_ecc = "Anganwadi Early Childhood Care"
  )
  
  # Filtered data
  filtered_data_nm <- reactive({
    req(input$nm_state, input$nm_year)
    combined_data %>%
      filter(state_numeric == input$nm_state, year == input$nm_year)
  })
  
  # Bar Chart
  output$non_monetary_bar <- renderPlotly({
    df <- filtered_data_nm()
    
    selected_vars <- c(input$access_var, input$women_var, input$child_var)
    
    df_long <- df %>%
      select(quintile_state_sector, all_of(selected_vars)) %>%
      pivot_longer(cols = all_of(selected_vars), names_to = "indicator", values_to = "value") %>%
      mutate(
        quintile = paste0("Q", quintile_state_sector),
        indicator_label = indicator_labels[indicator]
      ) %>%
      filter(quintile != "Q99")  # Exclude Q99
    
    plot_ly(df_long,
            x = ~quintile,
            y = ~value,
            color = ~indicator_label,
            type = 'bar') %>%
      layout(barmode = "group",
             xaxis = list(title = "Quintile"),
             yaxis = list(title = "Value"),
             legend = list(title = list(text = "Indicator")))
  })
  
}





################# Run app ################# 
shinyApp(ui = ui, server = server)
