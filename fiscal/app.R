# India Subnational PFR Data Visualization Tool - R Shiny
# Migrated from Excel dashboard v0.4

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(readr)

source("R/data_utils.R")
source("R/chart_utils.R")

# ── Load data on startup ─────────────────────────────────────────────────────
fiscal_data   <- load_fiscal_data()
states_config <- load_states_config()
var_map       <- load_codes_lists()
all_states    <- get_state_list(fiscal_data)

# ── Year range available ──────────────────────────────────────────────────────
year_cols <- setdiff(names(fiscal_data), c("State", "Variable"))
year_range <- range(as.integer(year_cols))

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = tags$span(
    tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/87/WBG_Horizontal-black-web.svg/200px-WBG_Horizontal-black-web.svg.png",
             height = "28px", style = "margin-right: 10px; vertical-align: middle;",
             onerror = "this.style.display='none'"),
    "India Subnational PFR Dashboard"
  ),
  id = "main_nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#002244",
    secondary = "#0071BC",
    success = "#009A44",
    warning = "#F4A100",
    danger = "#E4002B",
    "navbar-bg" = "#002244",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  header = tags$head(
    tags$style(HTML("
      .navbar { padding: 0.3rem 1rem; }
      .card { border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); margin-bottom: 16px; }
      .card-header { background: linear-gradient(135deg, #002244 0%, #0071BC 100%);
                     color: white; font-weight: 600; border-radius: 8px 8px 0 0 !important; }
      .value-box { border-radius: 8px; }
      .traffic-light { width: 28px; height: 28px; border-radius: 50%; display: inline-block;
                       vertical-align: middle; margin-right: 8px; }
      .sidebar-panel { background-color: #f8f9fa; padding: 15px; border-radius: 8px; }
      .nav-link.active { border-bottom: 3px solid #F4A100 !important; }
      .section-title { color: #002244; border-bottom: 2px solid #0071BC;
                       padding-bottom: 8px; margin-bottom: 16px; }
      .info-box { background: #f0f4f8; border-left: 4px solid #0071BC;
                  padding: 12px 16px; border-radius: 0 8px 8px 0; margin-bottom: 12px; }
    "))
  ),

  # ── Tab 1: State Selection & Overview ────────────────────────────────────

nav_panel(
    title = "Overview",
    icon = icon("home"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Configuration",
        width = 320,
        selectInput("state_select", "Select State",
                    choices = all_states, selected = "Gujarat"),
        hr(),
        h6("Peer States"),
        uiOutput("peers_display"),
        hr(),
        sliderInput("year_range", "Analysis Period",
                    min = 2000, max = 2024,
                    value = c(2015, 2023), sep = ""),
        hr(),
        actionButton("refresh_btn", "Refresh Analysis",
                     class = "btn-primary w-100", icon = icon("sync"))
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("About This Tool"),
          card_body(
            div(class = "info-box",
              p("This tool provides a comprehensive fiscal analysis of Indian states,
                 covering macroeconomic indicators, revenues, expenditures, fiscal health,
                 and fiscal rules compliance."),
              p("Select a state from the sidebar to begin analysis. The tool automatically
                 compares the selected state against large states and peer groups.")
            )
          )
        ),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = "Selected State",
            value = textOutput("vb_state"),
            theme = "primary",
            showcase = icon("map-marker-alt")
          ),
          value_box(
            title = "Peer Count",
            value = textOutput("vb_peers"),
            theme = "secondary",
            showcase = icon("users")
          ),
          value_box(
            title = "Latest Fiscal Revenue (% GDP)",
            value = textOutput("vb_revenue"),
            theme = "success",
            showcase = icon("chart-line")
          ),
          value_box(
            title = "Total Liabilities (% GDP)",
            value = textOutput("vb_debt"),
            theme = "warning",
            showcase = icon("exclamation-triangle")
          )
        ),
        card(
          card_header("Key Economic Indicators"),
          card_body(DTOutput("key_indicators_table"))
        )
      )
    )
  ),

  # ── Tab 2: Macro-Fiscal Analysis ─────────────────────────────────────────
  nav_panel(
    title = "Macro-Fiscal",
    icon = icon("chart-area"),
    navset_card_tab(
      title = "Macro-Fiscal Analysis",

      nav_panel("Key Indicators",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("GDP Growth"), card_body(plotlyOutput("macro_gdp_growth", height = "350px"))),
          card(card_header("GDP Growth Decomposition"), card_body(plotlyOutput("macro_gdp_decomp", height = "350px")))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Inflation (CPI)"), card_body(plotlyOutput("macro_inflation", height = "350px"))),
          card(card_header("GDP Per Capita (INR '000)"), card_body(plotlyOutput("macro_gdp_pc", height = "350px")))
        )
      ),

      nav_panel("Fiscal Balance",
        layout_columns(
          col_widths = c(12),
          card(
            card_header("Drivers of Fiscal Balance (Waterfall)"),
            card_body(
              layout_columns(
                col_widths = c(3, 3, 3, 3),
                selectInput("wf_year1", "Base Year", choices = 2010:2023, selected = 2018),
                selectInput("wf_year2", "End Year", choices = 2010:2023, selected = 2023)
              ),
              plotlyOutput("waterfall_chart", height = "450px")
            )
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Fiscal Balance (% GDP)"), card_body(plotlyOutput("fiscal_balance_ts", height = "350px"))),
          card(card_header("Primary Balance (% GDP)"), card_body(plotlyOutput("primary_balance_ts", height = "350px")))
        )
      ),

      nav_panel("Debt Dynamics",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Total Liabilities (% GDP)"), card_body(plotlyOutput("debt_total_ts", height = "350px"))),
          card(card_header("Interest Payments (% GDP)"), card_body(plotlyOutput("debt_interest_ts", height = "350px")))
        ),
        layout_columns(
          col_widths = c(12),
          card(card_header("Total Liabilities Over Time - All States"),
               card_body(plotlyOutput("debt_all_states", height = "400px")))
        )
      ),

      nav_panel("Elasticities & Cyclicality",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Revenue Elasticity to GDP"),
               card_body(plotlyOutput("rev_elasticity", height = "350px"))),
          card(card_header("Expenditure Elasticity to GDP"),
               card_body(plotlyOutput("exp_elasticity", height = "350px")))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Current Expenditure Cyclicality"),
               card_body(plotlyOutput("exp_cyclicality", height = "350px"))),
          card(card_header("Capital Expenditure Cyclicality"),
               card_body(plotlyOutput("cap_cyclicality", height = "350px")))
        )
      ),

      nav_panel("Climate & Risks",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Climate Vulnerability Index"),
               card_body(plotlyOutput("climate_cvi", height = "350px"))),
          card(card_header("State Energy & Climate Index (SECI)"),
               card_body(plotlyOutput("climate_seci", height = "350px")))
        )
      )
    )
  ),

  # ── Tab 3: Revenue Analysis ──────────────────────────────────────────────
  nav_panel(
    title = "Revenues",
    icon = icon("coins"),
    navset_card_tab(
      title = "Revenue Analysis",

      nav_panel("Total Revenues",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Total Revenue (% GDP)"), card_body(plotlyOutput("rev_total_ts", height = "350px"))),
          card(card_header("Revenue Composition (% GDP)"), card_body(plotlyOutput("rev_composition", height = "350px")))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header(textOutput("rev_avg_title")),
               card_body(plotlyOutput("rev_avg_bar", height = "350px"))),
          card(card_header("Revenue Composition (Latest Year)"),
               card_body(plotlyOutput("rev_pie", height = "350px")))
        )
      ),

      nav_panel("Tax Revenue",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Own Tax Revenue (% GDP)"), card_body(plotlyOutput("rev_own_tax_ts", height = "350px"))),
          card(card_header("Share in Central Taxes (% GDP)"), card_body(plotlyOutput("rev_central_share_ts", height = "350px")))
        ),
        layout_columns(
          col_widths = c(12),
          card(card_header("Tax Revenue Breakdown"),
               card_body(DTOutput("rev_tax_breakdown")))
        )
      ),

      nav_panel("Dependency",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Dependency on Centre (% GDP)"),
               card_body(plotlyOutput("rev_dep_gdp", height = "350px"))),
          card(card_header("Dependency on Centre (% Total Revenue)"),
               card_body(plotlyOutput("rev_dep_rev", height = "350px")))
        )
      )
    )
  ),

  # ── Tab 4: Expenditure Analysis ──────────────────────────────────────────
  nav_panel(
    title = "Expenditure",
    icon = icon("money-bill-wave"),
    navset_card_tab(
      title = "Expenditure Analysis",

      nav_panel("Total Expenditure",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Total Expenditure (% GDP)"), card_body(plotlyOutput("exp_total_ts", height = "350px"))),
          card(card_header("Expenditure Composition (% GDP)"), card_body(plotlyOutput("exp_composition", height = "350px")))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Current vs Capital Expenditure"), card_body(plotlyOutput("exp_cur_vs_cap", height = "350px"))),
          card(card_header("Expenditure Comparison"),
               card_body(plotlyOutput("exp_avg_bar", height = "350px")))
        )
      ),

      nav_panel("Current Expenditure",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Non-Development Expenditure (% GDP)"),
               card_body(plotlyOutput("exp_nondev_ts", height = "350px"))),
          card(card_header("Social Services (% GDP)"),
               card_body(plotlyOutput("exp_social_ts", height = "350px")))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Economic Services (% GDP)"),
               card_body(plotlyOutput("exp_economic_ts", height = "350px"))),
          card(card_header("Interest Payments (% GDP)"),
               card_body(plotlyOutput("exp_interest_ts", height = "350px")))
        )
      ),

      nav_panel("Capital Outlay",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Capital Outlay - Social Services"),
               card_body(plotlyOutput("cap_social_ts", height = "350px"))),
          card(card_header("Capital Outlay - Economic Services"),
               card_body(plotlyOutput("cap_economic_ts", height = "350px")))
        ),
        layout_columns(
          col_widths = c(12),
          card(card_header("Capital Outlay Comparison Across States"),
               card_body(plotlyOutput("cap_comparison_bar", height = "400px")))
        )
      ),

      nav_panel("Efficiency Analysis",
        layout_columns(
          col_widths = c(12),
          card(
            card_header("Education Efficiency"),
            card_body(
              layout_columns(
                col_widths = c(6, 6),
                plotlyOutput("edu_efficiency_scatter", height = "350px"),
                plotlyOutput("edu_efficiency_bar", height = "350px")
              )
            )
          )
        ),
        layout_columns(
          col_widths = c(12),
          card(
            card_header("Health Efficiency"),
            card_body(
              layout_columns(
                col_widths = c(6, 6),
                plotlyOutput("hlt_efficiency_scatter", height = "350px"),
                plotlyOutput("hlt_efficiency_bar", height = "350px")
              )
            )
          )
        )
      )
    )
  ),

  # ── Tab 5: Fiscal Health Index ───────────────────────────────────────────
  nav_panel(
    title = "Fiscal Health",
    icon = icon("heartbeat"),
    navset_card_tab(
      title = "Fiscal Health Dashboard",

      nav_panel("Overview",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Quality of Expenditure"),
               card_body(
                 layout_columns(
                   col_widths = c(6, 6),
                   plotlyOutput("fh_qoe_ts1", height = "300px"),
                   plotlyOutput("fh_qoe_bar", height = "300px")
                 )
               )),
          card(card_header("Revenue Mobilization"),
               card_body(
                 layout_columns(
                   col_widths = c(6, 6),
                   plotlyOutput("fh_rev_ts1", height = "300px"),
                   plotlyOutput("fh_rev_bar", height = "300px")
                 )
               ))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Debt Sustainability"),
               card_body(
                 layout_columns(
                   col_widths = c(6, 6),
                   plotlyOutput("fh_debt_ts1", height = "300px"),
                   plotlyOutput("fh_debt_bar", height = "300px")
                 )
               )),
          card(card_header("Fiscal Prudence"),
               card_body(
                 layout_columns(
                   col_widths = c(6, 6),
                   plotlyOutput("fh_prud_ts1", height = "300px"),
                   plotlyOutput("fh_prud_bar", height = "300px")
                 )
               ))
        )
      ),

      nav_panel("Debt & Interest",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Interest Payments (% Revenue Receipts)"),
               card_body(plotlyOutput("fh_int_rev_ts", height = "350px"))),
          card(card_header("Outstanding Liabilities (% GSDP)"),
               card_body(plotlyOutput("fh_liab_gdp_ts", height = "350px")))
        ),
        layout_columns(
          col_widths = c(12),
          card(card_header("Debt Index Comparison"),
               card_body(plotlyOutput("fh_debt_index_bar", height = "400px")))
        )
      ),

      nav_panel("Composite Index",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Fiscal Health Index"),
               card_body(plotlyOutput("fh_composite_ts", height = "350px"))),
          card(card_header("Component Breakdown"),
               card_body(plotlyOutput("fh_component_bar", height = "350px")))
        )
      )
    )
  ),

  # ── Tab 6: Fiscal Rules ─────────────────────────────────────────────────
  nav_panel(
    title = "Fiscal Rules",
    icon = icon("traffic-light"),
    navset_card_tab(
      title = "Fiscal Rules: Traffic Light System",

      nav_panel("Dashboard",
        layout_columns(
          col_widths = c(12),
          card(
            card_header("Fiscal Rules Compliance"),
            card_body(
              uiOutput("traffic_light_ui"),
              hr(),
              DTOutput("traffic_light_table")
            )
          )
        )
      ),

      nav_panel("Debt / Net Revenue",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Total Liabilities (% Net Revenue)"),
               card_body(plotlyOutput("fr_debt_rev_ts", height = "350px"))),
          card(card_header("Total Liabilities Comparison"),
               card_body(plotlyOutput("fr_debt_rev_bar", height = "350px")))
        )
      ),

      nav_panel("Interest / Net Revenue",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Interest Payments (% Net Revenue)"),
               card_body(plotlyOutput("fr_int_rev_ts", height = "350px"))),
          card(card_header("Interest Payments Comparison"),
               card_body(plotlyOutput("fr_int_rev_bar", height = "350px")))
        )
      ),

      nav_panel("Operating Balance",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Operating Balance (% Net Revenue)"),
               card_body(plotlyOutput("fr_opbal_rev_ts", height = "350px"))),
          card(card_header("Operating Balance Comparison"),
               card_body(plotlyOutput("fr_opbal_rev_bar", height = "350px")))
        )
      )
    )
  ),

  # ── Tab 7: Custom Analysis ──────────────────────────────────────────────
  nav_panel(
    title = "Explorer",
    icon = icon("search"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Variable Explorer",
        width = 350,
        selectInput("explorer_var", "Select Variable",
                    choices = sort(unique(fiscal_data$Variable)),
                    selected = "REV_TOTAL"),
        checkboxInput("explorer_pct_gdp", "Show as % of GDP", value = TRUE),
        selectizeInput("explorer_states", "Compare States",
                       choices = c("All States/UT", all_states),
                       selected = c("Gujarat"), multiple = TRUE),
        sliderInput("explorer_years", "Year Range",
                    min = 1990, max = 2024, value = c(2010, 2023), sep = ""),
        selectInput("explorer_chart", "Chart Type",
                    choices = c("Line" = "line", "Bar (latest year)" = "bar",
                                "Table" = "table")),
        hr(),
        h6("Scatter Plot"),
        selectInput("scatter_var_x", "X Variable",
                    choices = sort(unique(fiscal_data$Variable)),
                    selected = "EXP_CUR_I_A_ES"),
        selectInput("scatter_var_y", "Y Variable",
                    choices = sort(unique(fiscal_data$Variable)),
                    selected = "EDU.OUT.ASR"),
        selectInput("scatter_year", "Year", choices = 2014:2023, selected = 2022)
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header(textOutput("explorer_title")),
          card_body(
            conditionalPanel("input.explorer_chart != 'table'",
                             plotlyOutput("explorer_plot", height = "500px")),
            conditionalPanel("input.explorer_chart == 'table'",
                             DTOutput("explorer_table"))
          )
        ),
        card(
          card_header("Correlates Analysis (Scatter)"),
          card_body(plotlyOutput("explorer_scatter", height = "450px"))
        )
      )
    )
  )
)


# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Reactive values ──────────────────────────────────────────────────────
  rv <- reactiveValues(state = "Gujarat")

  observeEvent(input$state_select, { rv$state <- input$state_select })

  state_name <- reactive({ rv$state })
  year_start <- reactive({ input$year_range[1] })
  year_end   <- reactive({ input$year_range[2] })
  peers      <- reactive({ get_peers(states_config, state_name()) })
  comparison_states <- reactive({
    unique(c(state_name(), peers(), get_large_states(states_config)))
  })

  # ── Helper to get comparison data ────────────────────────────────────────
  get_comp_data <- function(var_code, pct_gdp = TRUE) {
    states <- unique(c(state_name(), peers()))
    if (pct_gdp) {
      get_multi_state_pct_gdp(fiscal_data, states, var_code) %>%
        filter(Year >= year_start(), Year <= year_end())
    } else {
      get_multi_state_variable(fiscal_data, states, var_code) %>%
        filter(Year >= year_start(), Year <= year_end())
    }
  }

  get_all_states_data <- function(var_code, pct_gdp = TRUE, yr = NULL) {
    if (pct_gdp) {
      d <- get_multi_state_pct_gdp(fiscal_data, all_states, var_code)
    } else {
      d <- get_multi_state_variable(fiscal_data, all_states, var_code)
    }
    if (!is.null(yr)) d <- d %>% filter(Year == yr)
    d
  }

  # ── Overview Tab ──────────────────────────────────────────────────────────
  output$vb_state <- renderText(state_name())
  output$vb_peers <- renderText(length(peers()))

  output$vb_revenue <- renderText({
    d <- get_pct_gdp(fiscal_data, state_name(), "REV_TOTAL")
    if (nrow(d) == 0) return("N/A")
    paste0(round(tail(d$Value, 1), 1), "%")
  })

  output$vb_debt <- renderText({
    d <- get_pct_gdp(fiscal_data, state_name(), "TOTLIAB")
    if (nrow(d) == 0) return("N/A")
    paste0(round(tail(d$Value, 1), 1), "%")
  })

  output$peers_display <- renderUI({
    p <- peers()
    if (length(p) == 0) return(p("No peers configured"))
    tags$ul(class = "list-unstyled", lapply(p, function(x) tags$li(icon("circle", class = "text-primary"), " ", x)))
  })

  output$key_indicators_table <- renderDT({
    tbl <- build_key_indicators(fiscal_data, states_config, state_name(),
                                 year_start(), year_end())
    datatable(tbl, options = list(dom = 't', pageLength = 20, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = 2:4, digits = 2)
  })

  # ── Macro-Fiscal Tab ─────────────────────────────────────────────────────
  output$macro_gdp_growth <- renderPlotly({
    d <- get_comp_data("realgsdp", pct_gdp = FALSE)
    plot_time_series(d, title = "Real GDP Growth (%)", ylab = "Percent")
  })

  output$macro_gdp_decomp <- renderPlotly({
    sectors <- c("agriculture", "industry", "services")
    d <- bind_rows(lapply(sectors, function(s) {
      get_state_variable(fiscal_data, state_name(), s) %>%
        filter(Year >= year_start(), Year <= year_end()) %>%
        mutate(Component = tools::toTitleCase(s))
    }))
    plot_stacked_area(d, title = "GDP Growth Decomposition (pp)", ylab = "Percentage points")
  })

  output$macro_inflation <- renderPlotly({
    d <- get_comp_data("CPI", pct_gdp = FALSE)
    plot_time_series(d, title = "Inflation (CPI, %)", ylab = "Percent")
  })

  output$macro_gdp_pc <- renderPlotly({
    d <- get_state_variable(fiscal_data, state_name(), "realgsdp") %>%
      filter(Year >= year_start(), Year <= year_end()) %>%
      mutate(State = state_name())
    # Get GDP in per capita terms from NGSDP and POP
    gdp <- get_state_variable(fiscal_data, state_name(), "NGSDP") %>%
      filter(Year >= year_start(), Year <= year_end())
    pop <- get_state_variable(fiscal_data, state_name(), "POP") %>%
      filter(Year >= year_start(), Year <= year_end())
    if (nrow(gdp) > 0 && nrow(pop) > 0) {
      merged <- inner_join(gdp, pop, by = "Year", suffix = c("_gdp", "_pop")) %>%
        mutate(Value = Value_gdp / Value_pop / 1000, State = state_name()) %>%
        select(Year, Value, State)
      plot_time_series(merged, title = "GDP Per Capita (INR '000)", ylab = "INR '000")
    } else {
      plotly_empty() %>% layout(title = "GDP Per Capita data not available")
    }
  })

  # Waterfall chart
  output$waterfall_chart <- renderPlotly({
    wd <- build_waterfall_data(fiscal_data, state_name(),
                                as.integer(input$wf_year1), as.integer(input$wf_year2))
    plot_waterfall(wd, title = paste("Change in Fiscal Balance,",
                                      input$wf_year1, "-", input$wf_year2))
  })

  output$fiscal_balance_ts <- renderPlotly({
    # Fiscal balance = Revenue - Expenditure as % GDP
    rev_d <- get_comp_data("REV_TOTAL", pct_gdp = TRUE)
    exp_total <- bind_rows(lapply(unique(rev_d$State), function(s) {
      r <- get_pct_gdp(fiscal_data, s, "REV_TOTAL") %>%
        filter(Year >= year_start(), Year <= year_end())
      e_cur <- get_pct_gdp(fiscal_data, s, "EXP_CUR_TOTAL") %>%
        filter(Year >= year_start(), Year <= year_end())
      e_cap <- get_pct_gdp(fiscal_data, s, "EXP_CAP_I") %>%
        filter(Year >= year_start(), Year <= year_end())
      e_la <- get_pct_gdp(fiscal_data, s, "EXP_CAP_IV") %>%
        filter(Year >= year_start(), Year <= year_end())

      if (nrow(r) == 0) return(NULL)
      merged <- r %>% rename(Rev = Value)
      if (nrow(e_cur) > 0) merged <- left_join(merged, e_cur %>% rename(Cur = Value), by = "Year")
      if (nrow(e_cap) > 0) merged <- left_join(merged, e_cap %>% rename(Cap = Value), by = "Year")
      if (nrow(e_la) > 0) merged <- left_join(merged, e_la %>% rename(LA = Value), by = "Year")

      merged %>%
        mutate(
          Cur = ifelse(is.na(Cur), 0, Cur),
          Cap = ifelse(is.na(Cap), 0, Cap),
          LA = ifelse(is.na(LA), 0, LA),
          Value = Rev - Cur - Cap - LA,
          State = s
        ) %>%
        select(Year, Value, State)
    }))
    plot_time_series(exp_total, title = "Fiscal Balance (% GDP)", ylab = "% GDP")
  })

  output$primary_balance_ts <- renderPlotly({
    # Primary balance = Fiscal balance + interest payments
    states <- unique(c(state_name(), peers()))
    d <- bind_rows(lapply(states, function(s) {
      r <- get_pct_gdp(fiscal_data, s, "REV_TOTAL") %>% filter(Year >= year_start(), Year <= year_end())
      e_cur <- get_pct_gdp(fiscal_data, s, "EXP_CUR_TOTAL") %>% filter(Year >= year_start(), Year <= year_end())
      e_cap <- get_pct_gdp(fiscal_data, s, "EXP_CAP_I") %>% filter(Year >= year_start(), Year <= year_end())
      e_la <- get_pct_gdp(fiscal_data, s, "EXP_CAP_IV") %>% filter(Year >= year_start(), Year <= year_end())
      ip <- get_pct_gdp(fiscal_data, s, "EXP_CUR_II_C_IP") %>% filter(Year >= year_start(), Year <= year_end())

      if (nrow(r) == 0) return(NULL)
      merged <- r %>% rename(Rev = Value) %>%
        left_join(e_cur %>% rename(Cur = Value), by = "Year") %>%
        left_join(e_cap %>% rename(Cap = Value), by = "Year") %>%
        left_join(e_la %>% rename(LA = Value), by = "Year") %>%
        left_join(ip %>% rename(IP = Value), by = "Year") %>%
        mutate(across(c(Cur, Cap, LA, IP), ~ifelse(is.na(.), 0, .))) %>%
        mutate(Value = Rev - Cur - Cap - LA + IP, State = s) %>%
        select(Year, Value, State)
    }))
    plot_time_series(d, title = "Primary Balance (% GDP)", ylab = "% GDP")
  })

  # Debt dynamics
  output$debt_total_ts <- renderPlotly({
    d <- get_comp_data("TOTLIAB", pct_gdp = TRUE)
    plot_time_series(d, title = "Total Liabilities (% GDP)", ylab = "% GDP")
  })

  output$debt_interest_ts <- renderPlotly({
    d <- get_comp_data("EXP_CUR_II_C_IP", pct_gdp = TRUE)
    plot_time_series(d, title = "Interest Payments (% GDP)", ylab = "% GDP")
  })

  output$debt_all_states <- renderPlotly({
    latest_yr <- year_end()
    d <- get_all_states_data("TOTLIAB", pct_gdp = TRUE, yr = latest_yr)
    if (nrow(d) == 0) {
      # Try the previous year
      d <- get_all_states_data("TOTLIAB", pct_gdp = TRUE, yr = latest_yr - 1)
    }
    if (nrow(d) > 0) {
      d_avg <- d %>% group_by(State) %>% summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      plot_bar_comparison(d_avg, title = paste("Total Liabilities (% GDP),", latest_yr),
                          ylab = "% GDP", highlight_state = state_name())
    } else {
      plotly_empty()
    }
  })

  # Elasticities
  output$rev_elasticity <- renderPlotly({
    rev <- get_state_variable(fiscal_data, state_name(), "REV_TOTAL") %>%
      filter(Year >= year_start(), Year <= year_end()) %>%
      calc_growth()
    gdp <- get_state_variable(fiscal_data, state_name(), "NGSDP") %>%
      filter(Year >= year_start(), Year <= year_end()) %>%
      calc_growth()
    if (nrow(rev) > 0 && nrow(gdp) > 0) {
      merged <- inner_join(rev %>% select(Year, Growth), gdp %>% select(Year, Growth),
                          by = "Year", suffix = c("_rev", "_gdp")) %>%
        filter(!is.na(Growth_rev), !is.na(Growth_gdp))
      p <- plot_ly(merged, x = ~Growth_gdp, y = ~Growth_rev, type = "scatter",
                   mode = "markers+text", text = ~Year, textposition = "top right",
                   marker = list(color = WB_COLORS$primary, size = 8)) %>%
        standard_layout(title = "Revenue Elasticity", xlab = "GDP Growth (%)", ylab = "Revenue Growth (%)")
      p
    } else plotly_empty()
  })

  output$exp_elasticity <- renderPlotly({
    exp <- get_state_variable(fiscal_data, state_name(), "EXP_CUR_TOTAL") %>%
      filter(Year >= year_start(), Year <= year_end()) %>%
      calc_growth()
    gdp <- get_state_variable(fiscal_data, state_name(), "NGSDP") %>%
      filter(Year >= year_start(), Year <= year_end()) %>%
      calc_growth()
    if (nrow(exp) > 0 && nrow(gdp) > 0) {
      merged <- inner_join(exp %>% select(Year, Growth), gdp %>% select(Year, Growth),
                          by = "Year", suffix = c("_exp", "_gdp")) %>%
        filter(!is.na(Growth_exp), !is.na(Growth_gdp))
      plot_ly(merged, x = ~Growth_gdp, y = ~Growth_exp, type = "scatter",
              mode = "markers+text", text = ~Year, textposition = "top right",
              marker = list(color = WB_COLORS$accent, size = 8)) %>%
        standard_layout(title = "Expenditure Elasticity", xlab = "GDP Growth (%)", ylab = "Expenditure Growth (%)")
    } else plotly_empty()
  })

  output$exp_cyclicality <- renderPlotly({
    exp_cyc <- get_state_variable(fiscal_data, state_name(), "REXP_CUR_TOTAL_cyc") %>%
      filter(Year >= year_start(), Year <= year_end()) %>%
      mutate(State = "Current Expenditure (cyclical)")
    gdp_cyc <- get_state_variable(fiscal_data, state_name(), "RGSDP_cyc") %>%
      filter(Year >= year_start(), Year <= year_end()) %>%
      mutate(State = "GDP (cyclical)")
    d <- bind_rows(exp_cyc, gdp_cyc)
    plot_time_series(d, title = "Cyclical Components: Current Exp. vs GDP")
  })

  output$cap_cyclicality <- renderPlotly({
    cap_cyc <- get_state_variable(fiscal_data, state_name(), "REXP_CAP_I_cyc") %>%
      filter(Year >= year_start(), Year <= year_end()) %>%
      mutate(State = "Capital Expenditure (cyclical)")
    gdp_cyc <- get_state_variable(fiscal_data, state_name(), "RGSDP_cyc") %>%
      filter(Year >= year_start(), Year <= year_end()) %>%
      mutate(State = "GDP (cyclical)")
    d <- bind_rows(cap_cyc, gdp_cyc)
    plot_time_series(d, title = "Cyclical Components: Capital Exp. vs GDP")
  })

  # Climate
  output$climate_cvi <- renderPlotly({
    d <- get_all_states_data("cvi", pct_gdp = FALSE)
    if (nrow(d) > 0) {
      d_latest <- d %>% group_by(State) %>% filter(Year == max(Year)) %>% ungroup()
      plot_bar_comparison(d_latest, title = "Climate Vulnerability Index",
                          ylab = "Index", highlight_state = state_name())
    } else plotly_empty()
  })

  output$climate_seci <- renderPlotly({
    d <- get_all_states_data("SECI_score", pct_gdp = FALSE)
    if (nrow(d) > 0) {
      d_latest <- d %>% group_by(State) %>% filter(Year == max(Year)) %>% ungroup()
      plot_bar_comparison(d_latest, title = "State Energy & Climate Index",
                          ylab = "Score", highlight_state = state_name())
    } else plotly_empty()
  })

  # ── Revenue Tab ──────────────────────────────────────────────────────────
  output$rev_total_ts <- renderPlotly({
    d <- get_comp_data("REV_TOTAL", pct_gdp = TRUE)
    plot_time_series(d, title = "Total Revenue (% GDP)", ylab = "% GDP")
  })

  output$rev_composition <- renderPlotly({
    d <- build_revenue_decomposition(fiscal_data, state_name(),
                                      years = year_start():year_end())
    plot_stacked_area(d, title = "Revenue Composition (% GDP)", ylab = "% GDP")
  })

  output$rev_avg_title <- renderText({
    paste("Revenue Period Average,", year_start(), "-", year_end())
  })

  output$rev_avg_bar <- renderPlotly({
    comp <- build_comparison_table(fiscal_data, states_config, state_name(),
                                    "REV_TOTAL", year_start(), year_end())
    if (!is.null(comp)) {
      plot_grouped_bar(comp$state_avg, comp$large_avg, comp$peers_avg,
                        state_name(), title = "Revenue (% GDP) - Period Average",
                        ylab = "% GDP")
    } else plotly_empty()
  })

  output$rev_pie <- renderPlotly({
    rev_vars <- list(
      list(code = "REV_TAX_I_A", label = "Own Tax Revenue"),
      list(code = "REV_TAX_I_B", label = "Share in Central Taxes"),
      list(code = "REV_NTAX_II_C", label = "Own Non-Tax Revenue"),
      list(code = "REV_NTAX_II_D", label = "Grants from Centre")
    )
    latest_yr <- year_end()
    labels <- c()
    values <- c()
    for (v in rev_vars) {
      d <- get_pct_gdp(fiscal_data, state_name(), v$code)
      val <- d %>% filter(Year == latest_yr) %>% pull(Value)
      if (length(val) == 0) val <- d %>% filter(Year == latest_yr - 1) %>% pull(Value)
      labels <- c(labels, v$label)
      values <- c(values, ifelse(length(val) > 0, val, 0))
    }
    plot_donut(labels, values, title = paste("Revenue Composition,", latest_yr))
  })

  output$rev_own_tax_ts <- renderPlotly({
    d <- get_comp_data("REV_TAX_I_A", pct_gdp = TRUE)
    plot_time_series(d, title = "Own Tax Revenue (% GDP)", ylab = "% GDP")
  })

  output$rev_central_share_ts <- renderPlotly({
    d <- get_comp_data("REV_TAX_I_B", pct_gdp = TRUE)
    plot_time_series(d, title = "Share in Central Taxes (% GDP)", ylab = "% GDP")
  })

  output$rev_tax_breakdown <- renderDT({
    tax_vars <- list(
      list(code = "REV_TAX_I_A_3_GS", label = "State GST"),
      list(code = "REV_TAX_I_A_3_SE", label = "State Excise"),
      list(code = "REV_TAX_I_A_3_TV", label = "Taxes on Vehicles"),
      list(code = "REV_TAX_I_A_2_SR", label = "Stamps & Registration"),
      list(code = "REV_TAX_I_A_3_TE", label = "Electricity Duties"),
      list(code = "REV_TAX_I_A_1_PT", label = "Professional Tax"),
      list(code = "REV_TAX_I_B_CG", label = "Central GST Share"),
      list(code = "REV_TAX_I_B_IT", label = "Income Tax Share"),
      list(code = "REV_TAX_I_B_CT", label = "Corporation Tax Share")
    )

    rows <- lapply(tax_vars, function(v) {
      d <- get_pct_gdp(fiscal_data, state_name(), v$code) %>%
        filter(Year >= year_start(), Year <= year_end())
      if (nrow(d) == 0) return(NULL)

      d_wide <- d %>% mutate(Year = as.character(Year)) %>%
        pivot_wider(names_from = Year, values_from = Value)
      d_wide <- d_wide %>% mutate(Component = v$label, .before = 1)
      d_wide
    })

    tbl <- bind_rows(rows)
    if (nrow(tbl) > 0) {
      num_cols <- setdiff(names(tbl), "Component")
      datatable(tbl, options = list(dom = 't', pageLength = 20, scrollX = TRUE),
                rownames = FALSE) %>%
        formatRound(columns = num_cols, digits = 2)
    }
  })

  # Dependency
  output$rev_dep_gdp <- renderPlotly({
    # Dependency = (Central Share + Grants) / GDP
    states <- unique(c(state_name(), peers()))
    d <- bind_rows(lapply(states, function(s) {
      share <- get_pct_gdp(fiscal_data, s, "REV_TAX_I_B") %>%
        filter(Year >= year_start(), Year <= year_end())
      grants <- get_pct_gdp(fiscal_data, s, "REV_NTAX_II_D") %>%
        filter(Year >= year_start(), Year <= year_end())
      if (nrow(share) == 0 || nrow(grants) == 0) return(NULL)
      inner_join(share, grants, by = "Year", suffix = c("_s", "_g")) %>%
        mutate(Value = Value_s + Value_g, State = s) %>%
        select(Year, Value, State)
    }))
    plot_time_series(d, title = "Dependency on Centre (% GDP)", ylab = "% GDP")
  })

  output$rev_dep_rev <- renderPlotly({
    states <- unique(c(state_name(), peers()))
    d <- bind_rows(lapply(states, function(s) {
      share <- get_state_variable(fiscal_data, s, "REV_TAX_I_B") %>%
        filter(Year >= year_start(), Year <= year_end())
      grants <- get_state_variable(fiscal_data, s, "REV_NTAX_II_D") %>%
        filter(Year >= year_start(), Year <= year_end())
      total <- get_state_variable(fiscal_data, s, "REV_TOTAL") %>%
        filter(Year >= year_start(), Year <= year_end())
      if (nrow(share) == 0 || nrow(total) == 0) return(NULL)

      dep <- share %>% rename(Share = Value) %>%
        left_join(grants %>% rename(Grant = Value), by = "Year") %>%
        left_join(total %>% rename(Total = Value), by = "Year") %>%
        mutate(Grant = ifelse(is.na(Grant), 0, Grant),
               Value = (Share + Grant) / Total * 100, State = s) %>%
        select(Year, Value, State)
    }))
    plot_time_series(d, title = "Dependency on Centre (% Total Revenue)", ylab = "Percent")
  })

  # ── Expenditure Tab ──────────────────────────────────────────────────────
  output$exp_total_ts <- renderPlotly({
    # Total expenditure = current + capital + loans
    states <- unique(c(state_name(), peers()))
    d <- bind_rows(lapply(states, function(s) {
      cur <- get_pct_gdp(fiscal_data, s, "EXP_CUR_TOTAL") %>% filter(Year >= year_start(), Year <= year_end())
      cap <- get_pct_gdp(fiscal_data, s, "EXP_CAP_I") %>% filter(Year >= year_start(), Year <= year_end())
      la <- get_pct_gdp(fiscal_data, s, "EXP_CAP_IV") %>% filter(Year >= year_start(), Year <= year_end())
      if (nrow(cur) == 0) return(NULL)
      merged <- cur %>% rename(Cur = Value) %>%
        left_join(cap %>% rename(Cap = Value), by = "Year") %>%
        left_join(la %>% rename(LA = Value), by = "Year") %>%
        mutate(across(c(Cap, LA), ~ifelse(is.na(.), 0, .))) %>%
        mutate(Value = Cur + Cap + LA, State = s) %>%
        select(Year, Value, State)
    }))
    plot_time_series(d, title = "Total Expenditure (% GDP)", ylab = "% GDP")
  })

  output$exp_composition <- renderPlotly({
    d <- build_expenditure_decomposition(fiscal_data, state_name(),
                                          years = year_start():year_end())
    plot_stacked_area(d, title = "Expenditure Composition (% GDP)", ylab = "% GDP")
  })

  output$exp_cur_vs_cap <- renderPlotly({
    cur <- get_pct_gdp(fiscal_data, state_name(), "EXP_CUR_TOTAL") %>%
      filter(Year >= year_start(), Year <= year_end()) %>%
      mutate(State = "Current Expenditure")
    cap <- get_pct_gdp(fiscal_data, state_name(), "EXP_CAP_I") %>%
      filter(Year >= year_start(), Year <= year_end()) %>%
      mutate(State = "Capital Outlay")
    d <- bind_rows(cur, cap)
    plot_time_series(d, title = "Current vs Capital (% GDP)", ylab = "% GDP")
  })

  output$exp_avg_bar <- renderPlotly({
    comp <- build_comparison_table(fiscal_data, states_config, state_name(),
                                    "EXP_CUR_TOTAL", year_start(), year_end())
    if (!is.null(comp)) {
      plot_grouped_bar(comp$state_avg, comp$large_avg, comp$peers_avg,
                        state_name(), title = "Current Expenditure (% GDP) - Period Average",
                        ylab = "% GDP")
    } else plotly_empty()
  })

  output$exp_nondev_ts <- renderPlotly({
    d <- get_comp_data("EXP_CUR_II", pct_gdp = TRUE)
    plot_time_series(d, title = "Non-Dev. Expenditure (% GDP)", ylab = "% GDP")
  })

  output$exp_social_ts <- renderPlotly({
    d <- get_comp_data("EXP_CUR_I_A", pct_gdp = TRUE)
    plot_time_series(d, title = "Social Services (% GDP)", ylab = "% GDP")
  })

  output$exp_economic_ts <- renderPlotly({
    d <- get_comp_data("EXP_CUR_I_B", pct_gdp = TRUE)
    plot_time_series(d, title = "Economic Services (% GDP)", ylab = "% GDP")
  })

  output$exp_interest_ts <- renderPlotly({
    d <- get_comp_data("EXP_CUR_II_C_IP", pct_gdp = TRUE)
    plot_time_series(d, title = "Interest Payments (% GDP)", ylab = "% GDP")
  })

  # Capital outlay
  output$cap_social_ts <- renderPlotly({
    d <- get_comp_data("EXP_CAP_I_1_A_SS", pct_gdp = TRUE)
    plot_time_series(d, title = "Capital Outlay: Social Services (% GDP)", ylab = "% GDP")
  })

  output$cap_economic_ts <- renderPlotly({
    d <- get_comp_data("EXP_CAP_I_1_B_ES", pct_gdp = TRUE)
    plot_time_series(d, title = "Capital Outlay: Economic Services (% GDP)", ylab = "% GDP")
  })

  output$cap_comparison_bar <- renderPlotly({
    d <- get_all_states_data("EXP_CAP_I", pct_gdp = TRUE, yr = year_end())
    if (nrow(d) == 0) d <- get_all_states_data("EXP_CAP_I", pct_gdp = TRUE, yr = year_end() - 1)
    if (nrow(d) > 0) {
      d_avg <- d %>% group_by(State) %>% summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      plot_bar_comparison(d_avg, title = paste("Capital Outlay (% GDP),", year_end()),
                          ylab = "% GDP", highlight_state = state_name())
    } else plotly_empty()
  })

  # Efficiency analysis
  output$edu_efficiency_scatter <- renderPlotly({
    exp_data <- get_all_states_data("EXP_CUR_I_A_ES", pct_gdp = TRUE, yr = 2022)
    out_data <- get_multi_state_variable(fiscal_data, all_states, "EDU.OUT.ASR") %>%
      filter(Year == 2022)
    if (nrow(exp_data) > 0 && nrow(out_data) > 0) {
      plot_scatter(exp_data, out_data, title = "Education: Spending vs Outcomes",
                   xlab = "Education Spending (% GDP)", ylab = "Adjusted Survival Rate",
                   highlight_state = state_name())
    } else plotly_empty()
  })

  output$edu_efficiency_bar <- renderPlotly({
    d <- get_multi_state_variable(fiscal_data, all_states, "EDU.OOSS.DEA.GEX.ASR") %>%
      filter(Year == max(Year))
    if (nrow(d) > 0) {
      d_avg <- d %>% group_by(State) %>% summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      plot_bar_comparison(d_avg, title = "Education DEA Efficiency Score",
                          ylab = "Score", highlight_state = state_name())
    } else plotly_empty()
  })

  output$hlt_efficiency_scatter <- renderPlotly({
    exp_data <- get_all_states_data("EXP_CUR_I_A_MH", pct_gdp = TRUE, yr = 2022)
    out_data <- get_multi_state_variable(fiscal_data, all_states, "HLT.OUT.NHI") %>%
      filter(Year == 2022)
    if (nrow(exp_data) > 0 && nrow(out_data) > 0) {
      plot_scatter(exp_data, out_data, title = "Health: Spending vs Outcomes",
                   xlab = "Health Spending (% GDP)", ylab = "National Health Index",
                   highlight_state = state_name())
    } else plotly_empty()
  })

  output$hlt_efficiency_bar <- renderPlotly({
    d <- get_multi_state_variable(fiscal_data, all_states, "HLT.OOSS.DEA.GEX.NHI") %>%
      filter(Year == max(Year))
    if (nrow(d) > 0) {
      d_avg <- d %>% group_by(State) %>% summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      plot_bar_comparison(d_avg, title = "Health DEA Efficiency Score",
                          ylab = "Score", highlight_state = state_name())
    } else plotly_empty()
  })

  # ── Fiscal Health Tab ────────────────────────────────────────────────────
  # Quality of Expenditure
  output$fh_qoe_ts1 <- renderPlotly({
    d <- get_comp_data("EXP_CAP_I", pct_gdp = TRUE)
    plot_time_series(d, title = "Capital Outlay (% GSDP)", ylab = "% GSDP")
  })

  output$fh_qoe_bar <- renderPlotly({
    d <- get_all_states_data("EXP_CAP_I", pct_gdp = TRUE, yr = year_end())
    if (nrow(d) == 0) d <- get_all_states_data("EXP_CAP_I", pct_gdp = TRUE, yr = year_end() - 1)
    if (nrow(d) > 0) {
      d_avg <- d %>% group_by(State) %>% summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      plot_bar_comparison(d_avg, ylab = "% GSDP", highlight_state = state_name())
    } else plotly_empty()
  })

  # Revenue Mobilization
  output$fh_rev_ts1 <- renderPlotly({
    d <- get_comp_data("REV_TAX_I_A", pct_gdp = TRUE)
    plot_time_series(d, title = "Own Tax Revenue (% GSDP)", ylab = "% GSDP")
  })

  output$fh_rev_bar <- renderPlotly({
    d <- get_all_states_data("REV_TAX_I_A", pct_gdp = TRUE, yr = year_end())
    if (nrow(d) == 0) d <- get_all_states_data("REV_TAX_I_A", pct_gdp = TRUE, yr = year_end() - 1)
    if (nrow(d) > 0) {
      d_avg <- d %>% group_by(State) %>% summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      plot_bar_comparison(d_avg, ylab = "% GSDP", highlight_state = state_name())
    } else plotly_empty()
  })

  # Debt Sustainability
  output$fh_debt_ts1 <- renderPlotly({
    d <- get_comp_data("realgsdp", pct_gdp = FALSE)
    plot_time_series(d, title = "GSDP Growth Rate (%)", ylab = "Percent")
  })

  output$fh_debt_bar <- renderPlotly({
    d <- get_all_states_data("realgsdp", pct_gdp = FALSE, yr = year_end())
    if (nrow(d) == 0) d <- get_all_states_data("realgsdp", pct_gdp = FALSE, yr = year_end() - 1)
    if (nrow(d) > 0) {
      d_avg <- d %>% group_by(State) %>% summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      plot_bar_comparison(d_avg, ylab = "Percent", highlight_state = state_name())
    } else plotly_empty()
  })

  # Fiscal Prudence
  output$fh_prud_ts1 <- renderPlotly({
    d <- get_comp_data("BAL_OVR", pct_gdp = TRUE)
    plot_time_series(d, title = "Gross Fiscal Deficit (% GSDP)", ylab = "% GSDP")
  })

  output$fh_prud_bar <- renderPlotly({
    d <- get_all_states_data("BAL_OVR", pct_gdp = TRUE, yr = year_end())
    if (nrow(d) == 0) d <- get_all_states_data("BAL_OVR", pct_gdp = TRUE, yr = year_end() - 1)
    if (nrow(d) > 0) {
      d_avg <- d %>% group_by(State) %>% summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      plot_bar_comparison(d_avg, ylab = "% GSDP", highlight_state = state_name())
    } else plotly_empty()
  })

  # Debt & Interest sub-tab
  output$fh_int_rev_ts <- renderPlotly({
    states <- unique(c(state_name(), peers()))
    d <- bind_rows(lapply(states, function(s) {
      ip <- get_state_variable(fiscal_data, s, "EXP_CUR_II_C_IP") %>%
        filter(Year >= year_start(), Year <= year_end())
      rev <- get_state_variable(fiscal_data, s, "REV_TOTAL") %>%
        filter(Year >= year_start(), Year <= year_end())
      if (nrow(ip) == 0 || nrow(rev) == 0) return(NULL)
      inner_join(ip, rev, by = "Year", suffix = c("_ip", "_rev")) %>%
        mutate(Value = Value_ip / Value_rev * 100, State = s) %>%
        select(Year, Value, State)
    }))
    plot_time_series(d, title = "Interest Payments (% Revenue Receipts)", ylab = "Percent")
  })

  output$fh_liab_gdp_ts <- renderPlotly({
    d <- get_comp_data("TOTLIAB", pct_gdp = TRUE)
    plot_time_series(d, title = "Outstanding Liabilities (% GSDP)", ylab = "% GSDP")
  })

  output$fh_debt_index_bar <- renderPlotly({
    d <- get_all_states_data("TOTLIAB", pct_gdp = TRUE, yr = year_end())
    if (nrow(d) == 0) d <- get_all_states_data("TOTLIAB", pct_gdp = TRUE, yr = year_end() - 1)
    if (nrow(d) > 0) {
      d_avg <- d %>% group_by(State) %>% summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      plot_bar_comparison(d_avg, title = "Outstanding Liabilities (% GSDP)",
                          ylab = "% GSDP", highlight_state = state_name())
    } else plotly_empty()
  })

  # Composite Index
  output$fh_composite_ts <- renderPlotly({
    # Simple composite: average of normalized scores for key metrics
    states <- unique(c(state_name(), peers()))
    metrics <- c("EXP_CAP_I", "REV_TAX_I_A", "TOTLIAB", "BAL_OVR")
    d_all <- bind_rows(lapply(metrics, function(m) {
      get_multi_state_pct_gdp(fiscal_data, states, m) %>%
        filter(Year >= year_start(), Year <= year_end()) %>%
        mutate(Metric = m)
    }))
    if (nrow(d_all) == 0) return(plotly_empty())

    # Normalize each metric 0-1
    d_norm <- d_all %>%
      group_by(Metric, Year) %>%
      mutate(NormVal = (Value - min(Value, na.rm = TRUE)) /
               (max(Value, na.rm = TRUE) - min(Value, na.rm = TRUE) + 0.001)) %>%
      ungroup() %>%
      # For deficit/debt, lower is better -> invert
      mutate(NormVal = ifelse(Metric %in% c("TOTLIAB", "BAL_OVR"), 1 - NormVal, NormVal)) %>%
      group_by(State, Year) %>%
      summarise(Value = mean(NormVal, na.rm = TRUE), .groups = "drop")

    plot_time_series(d_norm, title = "Composite Fiscal Health Index (0-1)", ylab = "Index")
  })

  output$fh_component_bar <- renderPlotly({
    metrics <- list(
      list(code = "EXP_CAP_I", label = "Quality of Exp.", invert = FALSE),
      list(code = "REV_TAX_I_A", label = "Revenue Mobilization", invert = FALSE),
      list(code = "TOTLIAB", label = "Debt Sustainability", invert = TRUE),
      list(code = "BAL_OVR", label = "Fiscal Prudence", invert = TRUE)
    )

    scores <- sapply(metrics, function(m) {
      d <- get_pct_gdp(fiscal_data, state_name(), m$code) %>%
        filter(Year >= year_start(), Year <= year_end())
      d_all <- get_all_states_data(m$code, pct_gdp = TRUE) %>%
        filter(Year >= year_start(), Year <= year_end())
      if (nrow(d) == 0 || nrow(d_all) == 0) return(0.5)

      s_avg <- mean(d$Value, na.rm = TRUE)
      all_avg <- d_all %>% group_by(State) %>%
        summarise(V = mean(Value, na.rm = TRUE), .groups = "drop")
      rng <- range(all_avg$V, na.rm = TRUE)
      norm <- (s_avg - rng[1]) / (rng[2] - rng[1] + 0.001)
      if (m$invert) norm <- 1 - norm
      norm
    })

    labels <- sapply(metrics, function(m) m$label)
    plot_ly(x = labels, y = scores, type = "bar",
            marker = list(color = c(WB_COLORS$primary, WB_COLORS$secondary,
                                    WB_COLORS$accent, WB_COLORS$positive)),
            text = round(scores, 2), textposition = "outside") %>%
      standard_layout(title = paste("Fiscal Health Components -", state_name()), ylab = "Score (0-1)")
  })

  # ── Fiscal Rules Tab ─────────────────────────────────────────────────────
  output$traffic_light_ui <- renderUI({
    tl <- build_traffic_light(fiscal_data, states_config, state_name())
    div(
      style = "display: flex; gap: 24px; flex-wrap: wrap; padding: 16px;",
      lapply(seq_len(nrow(tl)), function(i) {
        div(
          style = "flex: 1; min-width: 200px; text-align: center; padding: 20px;
                   background: #f8f9fa; border-radius: 12px;",
          div(class = "traffic-light",
              style = paste0("background-color: ", traffic_light_color(tl$Status[i]),
                            "; width: 48px; height: 48px; margin: 0 auto 12px auto;")),
          h5(tl$Metric[i], style = "color: #333;"),
          h3(paste0(ifelse(is.na(tl$Value[i]), "N/A", tl$Value[i]), "%"),
             style = paste0("color: ", traffic_light_color(tl$Status[i]), ";"))
        )
      })
    )
  })

  output$traffic_light_table <- renderDT({
    tl_all <- bind_rows(lapply(all_states, function(s) {
      tl <- build_traffic_light(fiscal_data, states_config, s)
      tl %>% mutate(State = s) %>% select(State, everything())
    }))
    datatable(tl_all, options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle("Status", backgroundColor = styleEqual(
        c("green", "yellow", "red", "grey"),
        c("#d4edda", "#fff3cd", "#f8d7da", "#e2e3e5")
      ))
  })

  # Fiscal rules sub-tabs
  output$fr_debt_rev_ts <- renderPlotly({
    states <- unique(c(state_name(), peers()))
    d <- bind_rows(lapply(states, function(s) {
      liab <- get_state_variable(fiscal_data, s, "TOTLIAB") %>%
        filter(Year >= year_start(), Year <= year_end())
      rev <- get_state_variable(fiscal_data, s, "REV_TOTAL") %>%
        filter(Year >= year_start(), Year <= year_end())
      if (nrow(liab) == 0 || nrow(rev) == 0) return(NULL)
      # Using pct of GDP approximation - actual net revenue ratio
      get_pct_gdp(fiscal_data, s, "TOTLIAB") %>%
        filter(Year >= year_start(), Year <= year_end()) %>%
        mutate(State = s)
    }))
    plot_time_series(d, title = "Total Liabilities (% GDP)", ylab = "% GDP")
  })

  output$fr_debt_rev_bar <- renderPlotly({
    d <- get_all_states_data("TOTLIAB", pct_gdp = TRUE, yr = year_end())
    if (nrow(d) == 0) d <- get_all_states_data("TOTLIAB", pct_gdp = TRUE, yr = year_end() - 1)
    if (nrow(d) > 0) {
      d_avg <- d %>% group_by(State) %>% summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      plot_bar_comparison(d_avg, title = "Total Liabilities", ylab = "% GDP",
                          highlight_state = state_name())
    } else plotly_empty()
  })

  output$fr_int_rev_ts <- renderPlotly({
    d <- get_comp_data("EXP_CUR_II_C_IP", pct_gdp = TRUE)
    plot_time_series(d, title = "Interest Payments (% GDP)", ylab = "% GDP")
  })

  output$fr_int_rev_bar <- renderPlotly({
    d <- get_all_states_data("EXP_CUR_II_C_IP", pct_gdp = TRUE, yr = year_end())
    if (nrow(d) == 0) d <- get_all_states_data("EXP_CUR_II_C_IP", pct_gdp = TRUE, yr = year_end() - 1)
    if (nrow(d) > 0) {
      d_avg <- d %>% group_by(State) %>% summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      plot_bar_comparison(d_avg, title = "Interest Payments", ylab = "% GDP",
                          highlight_state = state_name())
    } else plotly_empty()
  })

  output$fr_opbal_rev_ts <- renderPlotly({
    d <- get_comp_data("BAL_REV", pct_gdp = TRUE)
    plot_time_series(d, title = "Revenue Balance (% GDP)", ylab = "% GDP")
  })

  output$fr_opbal_rev_bar <- renderPlotly({
    d <- get_all_states_data("BAL_REV", pct_gdp = TRUE, yr = year_end())
    if (nrow(d) == 0) d <- get_all_states_data("BAL_REV", pct_gdp = TRUE, yr = year_end() - 1)
    if (nrow(d) > 0) {
      d_avg <- d %>% group_by(State) %>% summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      plot_bar_comparison(d_avg, title = "Revenue Balance", ylab = "% GDP",
                          highlight_state = state_name())
    } else plotly_empty()
  })

  # ── Explorer Tab ─────────────────────────────────────────────────────────
  output$explorer_title <- renderText({
    var_label <- var_map %>% filter(Code == input$explorer_var) %>% pull(Name)
    label <- if (length(var_label) > 0) var_label[1] else input$explorer_var
    pct <- if (input$explorer_pct_gdp) " (% GDP)" else ""
    paste(label, pct)
  })

  output$explorer_plot <- renderPlotly({
    states_sel <- input$explorer_states
    if (length(states_sel) == 0) return(plotly_empty())

    if (input$explorer_pct_gdp) {
      d <- get_multi_state_pct_gdp(fiscal_data, states_sel, input$explorer_var) %>%
        filter(Year >= input$explorer_years[1], Year <= input$explorer_years[2])
    } else {
      d <- get_multi_state_variable(fiscal_data, states_sel, input$explorer_var) %>%
        filter(Year >= input$explorer_years[1], Year <= input$explorer_years[2])
    }

    if (input$explorer_chart == "line") {
      plot_time_series(d, ylab = if (input$explorer_pct_gdp) "% GDP" else "Value")
    } else {
      # Bar for latest year
      d_latest <- d %>% group_by(State) %>% filter(Year == max(Year)) %>% ungroup()
      plot_bar_comparison(d_latest, ylab = if (input$explorer_pct_gdp) "% GDP" else "Value",
                          highlight_state = state_name())
    }
  })

  output$explorer_table <- renderDT({
    states_sel <- input$explorer_states
    if (length(states_sel) == 0) return(NULL)

    if (input$explorer_pct_gdp) {
      d <- get_multi_state_pct_gdp(fiscal_data, states_sel, input$explorer_var) %>%
        filter(Year >= input$explorer_years[1], Year <= input$explorer_years[2])
    } else {
      d <- get_multi_state_variable(fiscal_data, states_sel, input$explorer_var) %>%
        filter(Year >= input$explorer_years[1], Year <= input$explorer_years[2])
    }

    if (nrow(d) == 0) return(NULL)

    d_wide <- d %>%
      mutate(Year = as.character(Year)) %>%
      pivot_wider(names_from = Year, values_from = Value)

    num_cols <- setdiff(names(d_wide), "State")
    datatable(d_wide, options = list(dom = 'tp', pageLength = 35, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = num_cols, digits = 2)
  })

  output$explorer_scatter <- renderPlotly({
    yr <- as.integer(input$scatter_year)
    if (input$explorer_pct_gdp) {
      d_x <- get_all_states_data(input$scatter_var_x, pct_gdp = TRUE, yr = yr)
      d_y <- get_all_states_data(input$scatter_var_y, pct_gdp = TRUE, yr = yr)
    } else {
      d_x <- get_multi_state_variable(fiscal_data, all_states, input$scatter_var_x) %>% filter(Year == yr)
      d_y <- get_multi_state_variable(fiscal_data, all_states, input$scatter_var_y) %>% filter(Year == yr)
    }

    x_label <- var_map %>% filter(Code == input$scatter_var_x) %>% pull(Name)
    y_label <- var_map %>% filter(Code == input$scatter_var_y) %>% pull(Name)
    x_label <- if (length(x_label) > 0) x_label[1] else input$scatter_var_x
    y_label <- if (length(y_label) > 0) y_label[1] else input$scatter_var_y

    plot_scatter(d_x, d_y, title = paste(x_label, "vs", y_label, "-", yr),
                 xlab = x_label, ylab = y_label, highlight_state = state_name())
  })
}

shinyApp(ui = ui, server = server)
