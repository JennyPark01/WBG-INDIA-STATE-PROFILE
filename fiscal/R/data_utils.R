# data_utils.R - Data loading and processing utilities

library(dplyr)
library(tidyr)
library(readr)

# Load and prepare the fiscal dataset
load_fiscal_data <- function(data_dir = "data") {
  fiscal_raw <- read_csv(
    file.path(data_dir, "fiscal_data.csv"),
    col_types = cols(State = col_character(), Variable = col_character(), .default = col_double()),
    show_col_types = FALSE
  )
  fiscal_raw
}

# Load state configuration (peers, groups)
load_states_config <- function(data_dir = "data") {
  cfg <- read_csv(
    file.path(data_dir, "states_config.csv"),
    col_names = FALSE,
    show_col_types = FALSE
  )
  cfg
}

# Load variable codes and labels
load_codes_lists <- function(data_dir = "data") {
  codes <- read_csv(
    file.path(data_dir, "codes_lists.csv"),
    col_names = FALSE,
    show_col_types = FALSE
  )
  # Extract variable name -> code mapping
  var_map <- codes %>%
    select(Name = X4, Code = X5) %>%
    filter(!is.na(Code), !is.na(Name)) %>%
    distinct(Code, .keep_all = TRUE)
  var_map
}

# Get list of all states
get_state_list <- function(fiscal_data) {
  states <- sort(unique(fiscal_data$State))
  states[states != "All States/UT"]
}

# Get data for a specific state and variable, in long format
get_state_variable <- function(fiscal_data, state_name, variable_code) {
  row <- fiscal_data %>%
    filter(State == state_name, Variable == variable_code)
  if (nrow(row) == 0) return(tibble(Year = integer(), Value = double()))

  year_cols <- setdiff(names(row), c("State", "Variable"))
  row %>%
    select(all_of(year_cols)) %>%
    pivot_longer(everything(), names_to = "Year", values_to = "Value") %>%
    mutate(Year = as.integer(Year)) %>%
    filter(!is.na(Value)) %>%
    arrange(Year)
}

# Get data for multiple states for one variable
get_multi_state_variable <- function(fiscal_data, state_names, variable_code) {
  rows <- fiscal_data %>%
    filter(State %in% state_names, Variable == variable_code)
  if (nrow(rows) == 0) return(tibble(State = character(), Year = integer(), Value = double()))

  year_cols <- setdiff(names(rows), c("State", "Variable"))
  rows %>%
    select(State, all_of(year_cols)) %>%
    pivot_longer(-State, names_to = "Year", values_to = "Value") %>%
    mutate(Year = as.integer(Year)) %>%
    filter(!is.na(Value)) %>%
    arrange(State, Year)
}

# Get data as percent of GDP (NGSDP) for a state
get_pct_gdp <- function(fiscal_data, state_name, variable_code) {
  var_data <- get_state_variable(fiscal_data, state_name, variable_code)
  gdp_data <- get_state_variable(fiscal_data, state_name, "NGSDP")

  if (nrow(var_data) == 0 || nrow(gdp_data) == 0) {
    return(tibble(Year = integer(), Value = double()))
  }

  inner_join(var_data, gdp_data, by = "Year", suffix = c("_var", "_gdp")) %>%
    mutate(Value = Value_var / Value_gdp * 100) %>%
    select(Year, Value)
}

# Get data as pct of GDP for multiple states
get_multi_state_pct_gdp <- function(fiscal_data, state_names, variable_code) {
  result <- bind_rows(lapply(state_names, function(s) {
    d <- get_pct_gdp(fiscal_data, s, variable_code)
    if (nrow(d) > 0) d$State <- s
    d
  }))
  result
}

# Get peer states for a given state
get_peers <- function(states_config, state_name) {
  # Parse from config: columns 2-4 contain StateName, GroupName, PeerName
  peers_data <- states_config %>%
    filter(X2 == state_name, X3 == "Peers") %>%
    pull(X4)
  unique(peers_data[!is.na(peers_data)])
}

# Get large states group
get_large_states <- function(states_config) {
  # Column 8 onwards contains large states groupings
  large <- states_config %>%
    filter(X8 == "Large states" | X7 == "Large states") %>%
    select(X5, X6) %>%
    unlist() %>%
    unique()
  large <- large[!is.na(large) & large != "Large states"]
  sort(unique(large))
}

# Calculate period average
calc_period_avg <- function(data_long, year_start, year_end) {
  data_long %>%
    filter(Year >= year_start, Year <= year_end) %>%
    group_by(across(any_of("State"))) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
}

# Calculate growth rate
calc_growth <- function(data_long) {
  data_long %>%
    arrange(across(any_of("State")), Year) %>%
    group_by(across(any_of("State"))) %>%
    mutate(Growth = (Value / lag(Value) - 1) * 100) %>%
    ungroup()
}

# Build a comparison table: state vs large states avg vs peers avg
build_comparison_table <- function(fiscal_data, states_config, state_name, variable_code,
                                    year_start = 2015, year_end = 2023, as_pct_gdp = TRUE) {
  peers <- get_peers(states_config, state_name)
  large_states <- get_large_states(states_config)
  all_states <- unique(c(state_name, peers, large_states))

  if (as_pct_gdp) {
    data <- get_multi_state_pct_gdp(fiscal_data, all_states, variable_code)
  } else {
    data <- get_multi_state_variable(fiscal_data, all_states, variable_code)
  }

  if (nrow(data) == 0) return(NULL)

  # Calculate averages
  state_avg <- data %>% filter(State == state_name) %>%
    calc_period_avg(year_start, year_end) %>% pull(Value)
  large_avg <- data %>% filter(State %in% large_states) %>%
    calc_period_avg(year_start, year_end) %>% pull(Value)
  peers_avg <- data %>% filter(State %in% peers) %>%
    calc_period_avg(year_start, year_end) %>% pull(Value)

  list(
    data = data,
    state_avg = ifelse(length(state_avg) > 0, state_avg, NA),
    large_avg = ifelse(length(large_avg) > 0, large_avg, NA),
    peers_avg = ifelse(length(peers_avg) > 0, peers_avg, NA)
  )
}

# Prepare key indicators table (sheet 1_1 equivalent)
build_key_indicators <- function(fiscal_data, states_config, state_name,
                                  year_start = 2015, year_end = 2023) {
  indicators <- list(
    list(code = "realpcnsdp", label = "Growth of real per capita income", pct_gdp = FALSE),
    list(code = "realgsdp", label = "GDP growth, percent", pct_gdp = FALSE),
    list(code = "agriculture", label = "Agriculture", pct_gdp = FALSE),
    list(code = "industry", label = "Industry", pct_gdp = FALSE),
    list(code = "services", label = "Services", pct_gdp = FALSE),
    list(code = "CPI", label = "Inflation (CPI)", pct_gdp = FALSE),
    list(code = "REV_TOTAL", label = "Fiscal revenues, % of GDP", pct_gdp = TRUE),
    list(code = "EXP_CUR_TOTAL", label = "Total current expenditure, % of GDP", pct_gdp = TRUE),
    list(code = "EXP_CUR_II_C_IP", label = "Interest payments, % of GDP", pct_gdp = TRUE),
    list(code = "EXP_CAP_I", label = "Capital outlay, % of GDP", pct_gdp = TRUE),
    list(code = "TOTLIAB", label = "Total liabilities, % of GDP", pct_gdp = TRUE),
    list(code = "POV", label = "Multidimensional poverty index", pct_gdp = FALSE),
    list(code = "cvi", label = "Climate Vulnerability Index", pct_gdp = FALSE)
  )

  peers <- get_peers(states_config, state_name)
  large_states <- get_large_states(states_config)
  all_states <- unique(c(state_name, peers, large_states))

  results <- lapply(indicators, function(ind) {
    if (ind$pct_gdp) {
      data <- get_multi_state_pct_gdp(fiscal_data, all_states, ind$code)
    } else {
      data <- get_multi_state_variable(fiscal_data, all_states, ind$code)
    }

    if (nrow(data) == 0) {
      return(tibble(
        Indicator = ind$label,
        !!state_name := NA_real_,
        `Large States` = NA_real_,
        Peers = NA_real_
      ))
    }

    s_avg <- data %>% filter(State == state_name, Year >= year_start, Year <= year_end) %>%
      summarise(v = mean(Value, na.rm = TRUE)) %>% pull(v)
    l_avg <- data %>% filter(State %in% large_states, Year >= year_start, Year <= year_end) %>%
      summarise(v = mean(Value, na.rm = TRUE)) %>% pull(v)
    p_avg <- data %>% filter(State %in% peers, Year >= year_start, Year <= year_end) %>%
      summarise(v = mean(Value, na.rm = TRUE)) %>% pull(v)

    tibble(
      Indicator = ind$label,
      !!state_name := round(s_avg, 2),
      `Large States` = round(l_avg, 2),
      Peers = round(p_avg, 2)
    )
  })

  bind_rows(results)
}

# Build debt dynamics decomposition data
build_debt_dynamics <- function(fiscal_data, state_name, year_start = 2014, year_end = 2022) {
  totliab <- get_pct_gdp(fiscal_data, state_name, "TOTLIAB") %>%
    filter(Year >= year_start, Year <= year_end)
  gdp_growth <- get_state_variable(fiscal_data, state_name, "realgsdp") %>%
    filter(Year >= year_start, Year <= year_end)
  inflation <- get_state_variable(fiscal_data, state_name, "CPI") %>%
    filter(Year >= year_start, Year <= year_end)
  interest <- get_pct_gdp(fiscal_data, state_name, "EXP_CUR_II_C_IP") %>%
    filter(Year >= year_start, Year <= year_end)
  rev_total <- get_pct_gdp(fiscal_data, state_name, "REV_TOTAL") %>%
    filter(Year >= year_start, Year <= year_end)
  exp_cur <- get_pct_gdp(fiscal_data, state_name, "EXP_CUR_TOTAL") %>%
    filter(Year >= year_start, Year <= year_end)
  cap_outlay <- get_pct_gdp(fiscal_data, state_name, "EXP_CAP_I") %>%
    filter(Year >= year_start, Year <= year_end)

  list(
    total_liabilities = totliab,
    gdp_growth = gdp_growth,
    inflation = inflation,
    interest_payments = interest,
    revenues = rev_total,
    current_expenditure = exp_cur,
    capital_outlay = cap_outlay
  )
}

# Build fiscal balance waterfall data
build_waterfall_data <- function(fiscal_data, state_name, year1, year2) {
  vars <- list(
    list(code = "REV_TAX_I_A", label = "State's own tax revenues", side = "Revenue"),
    list(code = "REV_TAX_I_B", label = "Share in central tax", side = "Revenue"),
    list(code = "REV_NTAX_II_C", label = "State's own non-tax revenue", side = "Revenue"),
    list(code = "REV_NTAX_II_D", label = "Grants from the centre", side = "Revenue"),
    list(code = "EXP_CUR_I", label = "Current dev. expenditure", side = "Expenditure"),
    list(code = "EXP_CUR_II", label = "Current non-dev. expenditure", side = "Expenditure"),
    list(code = "EXP_CAP_I_1_DEV", label = "Dev. capital outlay", side = "Expenditure"),
    list(code = "EXP_CAP_I_2_ND", label = "Non-dev. capital outlay", side = "Expenditure"),
    list(code = "EXP_CAP_IV", label = "Loans and advances", side = "Expenditure")
  )

  results <- lapply(vars, function(v) {
    d <- get_pct_gdp(fiscal_data, state_name, v$code)
    val_y1 <- d %>% filter(Year == year1) %>% pull(Value)
    val_y2 <- d %>% filter(Year == year2) %>% pull(Value)
    change <- if (length(val_y2) > 0 && length(val_y1) > 0) val_y2 - val_y1 else NA_real_
    # Revenue increases improve balance, expenditure increases worsen it
    if (v$side == "Expenditure" && !is.na(change)) change <- -change
    tibble(Label = v$label, Side = v$side, Change = change)
  })

  bind_rows(results)
}

# Build revenue decomposition
build_revenue_decomposition <- function(fiscal_data, state_name, years = 2014:2023) {
  rev_vars <- list(
    list(code = "REV_TAX_I_A", label = "Own Tax Revenue"),
    list(code = "REV_TAX_I_B", label = "Share in Central Taxes"),
    list(code = "REV_NTAX_II_C", label = "Own Non-Tax Revenue"),
    list(code = "REV_NTAX_II_D", label = "Grants from Centre")
  )

  results <- lapply(rev_vars, function(v) {
    d <- get_pct_gdp(fiscal_data, state_name, v$code) %>%
      filter(Year %in% years) %>%
      mutate(Component = v$label)
  })

  bind_rows(results)
}

# Build expenditure decomposition
build_expenditure_decomposition <- function(fiscal_data, state_name, years = 2014:2023) {
  exp_vars <- list(
    list(code = "EXP_CUR_I", label = "Current Dev. Expenditure"),
    list(code = "EXP_CUR_II", label = "Current Non-Dev. Expenditure"),
    list(code = "EXP_CUR_III", label = "Grants-in-Aid"),
    list(code = "EXP_CAP_I", label = "Capital Outlay"),
    list(code = "EXP_CAP_IV", label = "Loans & Advances")
  )

  results <- lapply(exp_vars, function(v) {
    d <- get_pct_gdp(fiscal_data, state_name, v$code) %>%
      filter(Year %in% years) %>%
      mutate(Component = v$label)
  })

  bind_rows(results)
}

# Build fiscal health index components
build_fiscal_health <- function(fiscal_data, states_config, state_name, years = 2014:2023) {
  components <- list(
    list(
      label = "Quality of Expenditure",
      sub = list(
        list(code = "EXP_CAP_I", label = "Capital outlay % GSDP", pct_gdp = TRUE)
      )
    ),
    list(
      label = "Revenue Mobilization",
      sub = list(
        list(code = "REV_TAX_I_A", label = "Own revenue % GSDP", pct_gdp = TRUE)
      )
    ),
    list(
      label = "Debt Sustainability",
      sub = list(
        list(code = "realgsdp", label = "GSDP growth rate", pct_gdp = FALSE),
        list(code = "EXP_CUR_II_C_IP", label = "Interest payment growth", pct_gdp = TRUE)
      )
    ),
    list(
      label = "Fiscal Prudence",
      sub = list(
        list(code = "BAL_OVR", label = "Gross fiscal deficit % GSDP", pct_gdp = TRUE),
        list(code = "BAL_REV", label = "Revenue deficit % GSDP", pct_gdp = TRUE)
      )
    ),
    list(
      label = "Debt Index",
      sub = list(
        list(code = "TOTLIAB", label = "Outstanding liabilities % GSDP", pct_gdp = TRUE),
        list(code = "EXP_CUR_II_C_IP", label = "Interest payments % revenue", pct_gdp = FALSE)
      )
    )
  )
  components
}

# Build traffic light data for fiscal rules
build_traffic_light <- function(fiscal_data, states_config, state_name) {
  all_states <- get_state_list(fiscal_data)
  metrics <- list(
    list(code = "TOTLIAB", label = "Total liabilities (% GDP)", threshold_red = 35, threshold_yellow = 25),
    list(code = "EXP_CUR_II_C_IP", label = "Interest payments (% revenue)", threshold_red = 15, threshold_yellow = 10),
    list(code = "BAL_REV", label = "Revenue balance (% GDP)", threshold_red = -3, threshold_yellow = 0),
    list(code = "BAL_OVR", label = "Fiscal deficit (% GDP)", threshold_red = -4, threshold_yellow = -3)
  )

  results <- lapply(metrics, function(m) {
    val <- get_pct_gdp(fiscal_data, state_name, m$code)
    latest <- if (nrow(val) > 0) tail(val$Value, 1) else NA
    color <- if (is.na(latest)) "grey"
    else if (latest > m$threshold_red || (m$threshold_red < 0 && latest < m$threshold_red)) "red"
    else if (latest > m$threshold_yellow || (m$threshold_yellow < 0 && latest < m$threshold_yellow)) "yellow"
    else "green"
    tibble(Metric = m$label, Value = round(latest, 2), Status = color)
  })

  bind_rows(results)
}
