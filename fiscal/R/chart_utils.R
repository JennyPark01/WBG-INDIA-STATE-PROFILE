# chart_utils.R - Charting utilities using plotly

library(plotly)

# Color palette matching World Bank / professional fiscal analysis style
WB_COLORS <- list(
  primary = "#002244",
  secondary = "#0071BC",
  accent = "#F4A100",
  positive = "#009A44",
  negative = "#E4002B",
  neutral = "#6C757D",
  light_blue = "#B3D4FC",
  light_orange = "#FFD699",
  bg = "#FFFFFF",
  grid = "#E8E8E8",
  text = "#333333"
)

STATE_COLORS <- c(
  "#0071BC", "#E4002B", "#009A44", "#F4A100", "#8B5CF6",
  "#EC4899", "#14B8A6", "#F97316", "#6366F1", "#84CC16",
  "#06B6D4", "#EF4444", "#10B981", "#F59E0B", "#8B5CF6",
  "#DC2626", "#059669", "#D97706", "#7C3AED", "#DB2777"
)

# Standard plotly layout
standard_layout <- function(p, title = "", xlab = "", ylab = "", legend_pos = "bottom") {
  p %>%
    layout(
      title = list(text = title, font = list(size = 14, color = WB_COLORS$text, family = "Arial")),
      xaxis = list(title = xlab, gridcolor = WB_COLORS$grid, zeroline = FALSE),
      yaxis = list(title = ylab, gridcolor = WB_COLORS$grid, zeroline = TRUE,
                   zerolinecolor = WB_COLORS$neutral, zerolinewidth = 1),
      legend = list(orientation = if (legend_pos == "bottom") "h" else "v",
                    x = if (legend_pos == "bottom") 0.5 else 1.02,
                    y = if (legend_pos == "bottom") -0.15 else 1,
                    xanchor = if (legend_pos == "bottom") "center" else "left"),
      plot_bgcolor = WB_COLORS$bg,
      paper_bgcolor = WB_COLORS$bg,
      margin = list(t = 50, b = 80, l = 60, r = 20),
      font = list(family = "Arial", color = WB_COLORS$text)
    )
}

# Line chart for time series comparison
plot_time_series <- function(data, title = "", ylab = "", show_avg_lines = NULL) {
  if (is.null(data) || nrow(data) == 0) return(plotly_empty())

  states <- unique(data$State)
  p <- plot_ly()

  for (i in seq_along(states)) {
    s_data <- data %>% filter(State == states[i])
    p <- p %>%
      add_trace(
        data = s_data, x = ~Year, y = ~Value,
        type = "scatter", mode = "lines+markers",
        name = states[i],
        line = list(color = STATE_COLORS[i], width = if (i == 1) 3 else 1.5),
        marker = list(size = if (i == 1) 6 else 3)
      )
  }

  p %>% standard_layout(title = title, ylab = ylab)
}

# Bar chart for cross-state comparison
plot_bar_comparison <- function(data, title = "", ylab = "", highlight_state = NULL) {
  if (is.null(data) || nrow(data) == 0) return(plotly_empty())

  colors <- ifelse(data$State == highlight_state, WB_COLORS$primary, WB_COLORS$light_blue)

  plot_ly(data, x = ~reorder(State, Value), y = ~Value, type = "bar",
          marker = list(color = colors)) %>%
    standard_layout(title = title, ylab = ylab) %>%
    layout(xaxis = list(tickangle = -45))
}

# Stacked area chart
plot_stacked_area <- function(data, title = "", ylab = "") {
  if (is.null(data) || nrow(data) == 0) return(plotly_empty())

  components <- unique(data$Component)
  p <- plot_ly()

  for (i in seq_along(components)) {
    c_data <- data %>% filter(Component == components[i])
    p <- p %>%
      add_trace(
        data = c_data, x = ~Year, y = ~Value,
        type = "scatter", mode = "lines",
        name = components[i],
        stackgroup = "one",
        fillcolor = STATE_COLORS[i],
        line = list(color = STATE_COLORS[i], width = 0.5)
      )
  }

  p %>% standard_layout(title = title, ylab = ylab)
}

# Waterfall chart
plot_waterfall <- function(waterfall_data, title = "") {
  if (is.null(waterfall_data) || nrow(waterfall_data) == 0) return(plotly_empty())

  wd <- waterfall_data %>% filter(!is.na(Change))

  colors <- ifelse(wd$Change >= 0, WB_COLORS$positive, WB_COLORS$negative)

  plot_ly(wd, x = ~Label, y = ~Change, type = "bar",
          marker = list(color = colors),
          text = ~paste0(ifelse(Change >= 0, "+", ""), round(Change, 2), " pp"),
          textposition = "outside") %>%
    standard_layout(title = title, ylab = "Percentage points of GDP") %>%
    layout(xaxis = list(tickangle = -45, categoryorder = "array", categoryarray = wd$Label))
}

# Grouped bar chart for period averages
plot_grouped_bar <- function(state_val, large_val, peers_val, state_name,
                              title = "", ylab = "") {
  categories <- c(state_name, "Large States", "Peers")
  values <- c(state_val, large_val, peers_val)
  colors <- c(WB_COLORS$primary, WB_COLORS$secondary, WB_COLORS$accent)

  plot_ly(x = categories, y = values, type = "bar",
          marker = list(color = colors),
          text = round(values, 2), textposition = "outside") %>%
    standard_layout(title = title, ylab = ylab)
}

# Scatter plot
plot_scatter <- function(data_x, data_y, title = "", xlab = "", ylab = "",
                          highlight_state = NULL) {
  if (is.null(data_x) || is.null(data_y) || nrow(data_x) == 0) return(plotly_empty())

  merged <- inner_join(data_x, data_y, by = c("State", "Year"), suffix = c("_x", "_y"))
  if (nrow(merged) == 0) return(plotly_empty())

  colors <- ifelse(merged$State == highlight_state, WB_COLORS$primary, WB_COLORS$light_blue)
  sizes <- ifelse(merged$State == highlight_state, 12, 6)

  plot_ly(merged, x = ~Value_x, y = ~Value_y, type = "scatter", mode = "markers",
          text = ~State, marker = list(color = colors, size = sizes),
          hoverinfo = "text+x+y") %>%
    standard_layout(title = title, xlab = xlab, ylab = ylab)
}

# Heatmap for fiscal health index
plot_heatmap <- function(data, title = "") {
  if (is.null(data) || nrow(data) == 0) return(plotly_empty())

  plot_ly(data, x = ~Year, y = ~State, z = ~Value, type = "heatmap",
          colorscale = list(c(0, WB_COLORS$negative), c(0.5, WB_COLORS$accent),
                           c(1, WB_COLORS$positive)),
          hoverinfo = "x+y+z") %>%
    standard_layout(title = title) %>%
    layout(yaxis = list(autorange = "reversed"))
}

# Traffic light indicator
traffic_light_color <- function(status) {
  switch(status,
    "green" = "#009A44",
    "yellow" = "#F4A100",
    "red" = "#E4002B",
    "#6C757D"
  )
}

# Donut / pie chart for revenue composition
plot_donut <- function(labels, values, title = "") {
  plot_ly(labels = labels, values = values, type = "pie",
          hole = 0.5,
          marker = list(colors = STATE_COLORS[seq_along(labels)]),
          textinfo = "label+percent",
          hoverinfo = "label+value+percent") %>%
    standard_layout(title = title)
}

# Dual axis line chart
plot_dual_axis <- function(data1, data2, name1, name2, title = "", ylab1 = "", ylab2 = "") {
  p <- plot_ly() %>%
    add_trace(data = data1, x = ~Year, y = ~Value, type = "scatter", mode = "lines+markers",
              name = name1, line = list(color = WB_COLORS$primary, width = 2),
              marker = list(size = 4)) %>%
    add_trace(data = data2, x = ~Year, y = ~Value, type = "scatter", mode = "lines+markers",
              name = name2, yaxis = "y2",
              line = list(color = WB_COLORS$accent, width = 2, dash = "dash"),
              marker = list(size = 4))

  p %>%
    layout(
      title = list(text = title, font = list(size = 14)),
      yaxis = list(title = ylab1, side = "left"),
      yaxis2 = list(title = ylab2, overlaying = "y", side = "right"),
      legend = list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center"),
      plot_bgcolor = WB_COLORS$bg,
      paper_bgcolor = WB_COLORS$bg,
      font = list(family = "Arial", color = WB_COLORS$text)
    )
}
