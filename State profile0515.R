################################################################################
# Project : State Profile
# Author  : Jaeyeon(Jenny) Park (jpark36@worldbank.org)
# Date    : December 15, 2024
# Last mod: May 15, 2025
################################################################################

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)   
library(readr)
library(htmltools)


#––– Load your new CSV –––
nonmonetary <- read.csv(
  "C:/Users/jaeye/Downloads/State profile/Nonmon/nfhs2015_2019_mpi.csv"
)

#––– UI –––
ui <- tagList(
  
  # 1) <head> styling (unchanged) …
  tags$head(
    tags$style(HTML("
      .skin-red .main-header .navbar,
      .skin-red .main-header .logo { background-color: #C11B17 !important; }
      header.main-header, .navbar-static-top { border-bottom: none !important; box-shadow: none !important; }
      .main-header .navbar { min-height: 70px !important; }
      .main-header .logo, .main-header .sidebar-toggle { height: 70px !important; line-height: 70px !important; }
      .main-sidebar { top: 15px !important; }
      .content-wrapper, .main-footer { margin-top: 0px !important; }
      .skin-red .main-header .navbar .sidebar-toggle { display: none !important; }
    ")),
    tags$script(HTML("
      $(document).on('click', '.custom-toggle', function(e) {
        e.preventDefault();
        $('body').toggleClass('sidebar-collapse');
      });
    "))
  ),
  
  # 2) dashboard CSS, Header, Sidebar (unchanged) …
  dashboardPage(
    skin = "red",
    dashboardHeader(
      titleWidth = "100%",
      title = tags$div(
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span("India State Profile", style = "font-size:28px; font-weight:bold; color:white;"),
        tags$span(class = "custom-toggle", style = "color:white; font-size:20px; cursor:pointer;", tags$i(class = "fa fa-bars")),
        tags$div(style="flex:1;"), tags$img(src="white.png", height="40px")
      )
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("State Profile Overview", tabName="tab1", icon=icon("dashboard")),
        menuItem("Labour Market",         tabName="tab2", icon=icon("users")),
        menuItem("Welfare Indicators",   tabName="tab3", icon=icon("chart-line")),
        menuItem("Access to Schemes", icon=icon("hands-helping"),
                 menuSubItem("Monetary Poverty",     tabName="tab4a"),
                 menuSubItem("Non-monetary Poverty", tabName="tab4b")),
        menuItem("State Comparison",     tabName="tab5", icon=icon("chart-bar"))
      )
    ),
    
    #––– Body –––
    dashboardBody(
      tabItems(
        ####### Tab 4b: Non‑monetary Poverty (UPDATED) #######
        tabItem(tabName = "tab4b",
                
                tags$h2("Non‑monetary Poverty",
                        style = "font-weight:bold; color:navy; margin-bottom:20px;"),
                
                # State selector only
                fluidRow(
                  column(6, selectInput("nm_state", "Select State:", choices = sort(unique(nonmonetary$state_numeric)))),
                  column(6, selectInput("nm_sector", "Select Sector:", choices = c("Poor" = 1, "Non-poor" = 0)))
                ),
                
                # Indicators on left, plot on right
                fluidRow(
                  column(3,
                         box(width = NULL, title = "Indicator Selection", status = "primary",
                             selectInput("access_var", "Access",
                                         choices = c("BPL" = "bpl",
                                                     "Aadhar" = "aadhar",
                                                     "State/Central Health Insurance" = "health_insu_govt")),
                             selectInput("women_var", "Women Health",
                                         choices = c("Met Healthcare Worker" = "household_health_met",
                                                     "Delivery Assistance"        = "household_has_preg_fin",
                                                     "Pregnancy Benefits"         = "household_has_preg_benefits")),
                             selectInput("child_var", "Child Health",
                                         choices = c("Anganwadi Benefits"           = "household_angan_benefits",
                                                     "Anganwadi Immunization"        = "household_angan_immun",
                                                     "Anganwadi Early Childhood Care"= "household_angan_ecc"))
                         )
                  ),
                  column(9,
                         box(width = 12, title = "Indicator by Sector & Year",
                             plotlyOutput("non_monetary_bar"))
                  )
                ),
                
                fluidRow(
                  column(12, tags$b("Note on Indicator"),
                         tags$p("Bars compare Poor vs Non‑poor for each chosen indicator, across both years."))
                ),
                fluidRow(
                  column(12, align = "right",
                         tags$em("Source: NFHS Rounds 4 (2015–16) & 5 (2019–21)"))
                )
        )
        
        # … other tabs untouched …
      )
    )
  )
)

#––– Server –––
server <- function(input, output, session) {
  
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
  
  # Filter by state only (we’ll show both Poor & Non‑poor)
  filtered_data_nm <- reactive({
    req(input$nm_state, input$nm_sector)
    nonmonetary %>%
      filter(state_numeric == input$nm_state, m_poor_1_33 == input$nm_sector)
  })
  
  
  output$non_monetary_bar <- renderPlotly({
    df <- filtered_data_nm()
    sel <- c(input$access_var, input$women_var, input$child_var)
    
    df_long <- df %>%
      select(Year, all_of(sel)) %>%
      pivot_longer(cols = all_of(sel),
                   names_to = "indicator",
                   values_to = "value") %>%
      mutate(indicator_label = indicator_labels[indicator])
    
    # Plot: X = indicator, Bars = Year
    plot_ly(df_long,
            x = ~indicator_label,
            y = ~value,
            color = ~factor(Year, levels = c(2015, 2019)),
            type = 'bar',
            colors = c('#1f77b4', '#ff7f0e')) %>%
      layout(barmode = "group",
             xaxis = list(title = "Indicator"),
             yaxis = list(title = "Value"),
             legend = list(title = list(text = "Year")))
    
  })
}

#––– Run App –––
shinyApp(ui = ui, server = server)
