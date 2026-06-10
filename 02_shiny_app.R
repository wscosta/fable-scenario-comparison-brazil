library(shiny)
library(dplyr)
library(plotly)

# Run 01_process_data.R first if any processed file is missing
if (!file.exists("data/processed/df_scenarios.rds") ||
    !file.exists("data/processed/fable_units.rds")) {
  source("01_process_data.R")
}

df_scenarios <- readRDS("data/processed/df_scenarios.rds")
df_hist      <- readRDS("data/processed/df_hist.rds")
fable_units  <- readRDS("data/processed/fable_units.rds")

# ── Unit conversion ───────────────────────────────────────────────────────────
to_mha <- function(values, col_name, unit_override = NULL) {
  unit <- if (!is.null(unit_override)) unit_override
          else trimws(tolower(fable_units[col_name]))
  if (is.na(unit))                             return(values)
  if (grepl("^1[,.]?000\\s*ha$|^kha$", unit)) return(values / 1000)
  if (grepl("^ha$", unit))                     return(values / 1e6)
  values
}

# ── Land-use class configuration ──────────────────────────────────────────────
# fable_col: column in SCENATHON_report
# hist_type / hist_source: matching fields in histdatabrazil.csv
landuse_map <- list(
  "Cropland"   = list(fable_col = "CalcCropland",  fable_unit = "1000 ha",
                      hist_type = "Cropland",                hist_source = "IBGE"),
  "Pasture"    = list(fable_col = "CalcPasture",   fable_unit = "1000 ha",
                      hist_type = "Pastures and Rangelands", hist_source = "LAPIG"),
  "Forest"     = list(fable_col = "CalcForest",    fable_unit = "1000 ha",
                      hist_type = "Forest",                  hist_source = "Mapbiomas"),
  "Other Land" = list(fable_col = "CalcOtherLand", fable_unit = "1000 ha",
                      hist_type = "Other Land",              hist_source = "Mapbiomas"),
  "Urban"      = list(fable_col = "CalcUrban",     fable_unit = "1000 ha",
                      hist_type = "Urban",                   hist_source = "Mapbiomas")
)

# Keep only classes whose FABLE column exists in the data
fable_cols  <- names(df_scenarios)
landuse_map <- Filter(function(cfg) cfg$fable_col %in% fable_cols, landuse_map)

# Print detected units for all classes (useful for debugging)
message("── Land-use class units ──────────────────────────────────────")
for (nm in names(landuse_map)) {
  col <- landuse_map[[nm]]$fable_col
  message(sprintf("  %-12s  col=%-15s  unit=%s", nm, col, fable_units[col]))
}

# Helper: historical data for a class (empty tibble if not found)
get_hist <- function(class_name, x_max) {
  cfg <- landuse_map[[class_name]]
  df_hist %>%
    filter(trimws(type)   == cfg$hist_type,
           trimws(source) == cfg$hist_source,
           year > 1995, year <= x_max) %>%
    select(year, value) %>%
    mutate(value = as.numeric(value))
}

# ── Colors ────────────────────────────────────────────────────────────────────
COL_HIST <- "#000000"
COL_CT   <- "#1565C0"
COL_NDC  <- "#009C3B"

# ── Shared layout helper ──────────────────────────────────────────────────────
base_layout <- function(p, title_text, title_color = "black", x_max, y_range) {
  shapes <- list(
    list(type = "rect",
         xref = "paper", yref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 1,
         line = list(color = "black", width = 1),
         fillcolor = "rgba(0,0,0,0)")
  )
  if (x_max > 2020) {
    shapes <- c(
      list(list(type = "line",
                x0 = 2020, x1 = 2020, yref = "paper", y0 = 0, y1 = 1,
                line = list(color = "grey", dash = "dash", width = 1.5))),
      shapes
    )
  }

  p %>% plotly::layout(
    title = list(text = paste0("<b>", title_text, "</b>"),
                 font = list(color = title_color, size = 15)),
    xaxis = list(
      title     = "",
      tickvals  = seq(2000, x_max, 5),
      tickangle = 0,
      range     = c(1999, x_max + 1),
      ticks     = "outside", ticklen = 6, tickcolor = "white",
      gridcolor = "#CCCCCC"
    ),
    yaxis = list(
      title     = "Area (Mha)",
      range     = y_range,
      ticks     = "outside", ticklen = 6, tickcolor = "white",
      gridcolor = "#CCCCCC"
    ),
    legend = list(x = 1.02, y = 1, xanchor = "left", yanchor = "top",
                  bgcolor = "white", bordercolor = "black", borderwidth = 1),
    margin        = list(l = 70, r = 160, t = 50, b = 50),
    shapes        = shapes,
    plot_bgcolor  = "white",
    paper_bgcolor = "white"
  )
}

# ── Compute shared y-range ────────────────────────────────────────────────────
calc_y_range <- function(class_name, x_max) {
  cfg  <- landuse_map[[class_name]]
  col  <- cfg$fable_col
  scen_vals <- to_mha(
    as.numeric(df_scenarios[[col]][df_scenarios$Year <= x_max]),
    col,
    landuse_map[[class_name]]$fable_unit
  )
  hist_vals <- get_hist(class_name, x_max)$value
  all_vals  <- c(scen_vals, hist_vals)
  pad <- diff(range(all_vals, na.rm = TRUE)) * 0.05
  c(0, max(all_vals, na.rm = TRUE) + pad)
}

# ── Plot builders ─────────────────────────────────────────────────────────────
add_hist_trace <- function(p, hist_data, source_label) {
  if (nrow(hist_data) == 0) return(p)
  trace_name <- paste0("Historical (", source_label, ")")
  add_trace(p, data = hist_data, x = ~year, y = ~value,
            type = "scatter", mode = "lines+markers",
            name = trace_name,
            line   = list(color = COL_HIST, width = 2),
            marker = list(color = COL_HIST, size = 7),
            hovertemplate = paste0("%{x}: <b>%{y:.2f} Mha</b><extra>",
                                   trace_name, "</extra>"))
}

make_combined_plot <- function(class_name, x_max, y_range) {
  col  <- landuse_map[[class_name]]$fable_col

  ct <- df_scenarios %>%
    filter(scenario == "Current Trends", Year <= x_max) %>%
    select(year = Year, value = all_of(col)) %>%
    mutate(value = to_mha(as.numeric(value), col, landuse_map[[class_name]]$fable_unit))

  ndc <- df_scenarios %>%
    filter(scenario == "NDC Commitments", Year <= x_max) %>%
    select(year = Year, value = all_of(col)) %>%
    mutate(value = to_mha(as.numeric(value), col, landuse_map[[class_name]]$fable_unit))

  hist_data    <- get_hist(class_name, x_max)
  source_label <- landuse_map[[class_name]]$hist_source

  p <- plot_ly() %>%
    add_trace(data = ct, x = ~year, y = ~value,
              type = "scatter", mode = "lines+markers",
              name = "Current Trends",
              line   = list(color = COL_CT, width = 2),
              marker = list(color = COL_CT, size = 7),
              hovertemplate = "%{x}: <b>%{y:.2f} Mha</b><extra>Current Trends</extra>") %>%
    add_trace(data = ndc, x = ~year, y = ~value,
              type = "scatter", mode = "lines+markers",
              name = "NDC Commitments",
              line   = list(color = COL_NDC, width = 2),
              marker = list(color = COL_NDC, size = 7),
              hovertemplate = "%{x}: <b>%{y:.2f} Mha</b><extra>NDC Commitments</extra>") %>%
    add_hist_trace(hist_data, source_label)

  base_layout(p, paste0(class_name, ": Current Trends vs NDC Commitments"),
              x_max = x_max, y_range = y_range)
}

make_single_plot <- function(class_name, scenario_name, line_color, x_max, y_range) {
  col  <- landuse_map[[class_name]]$fable_col

  scen <- df_scenarios %>%
    filter(scenario == scenario_name, Year <= x_max) %>%
    select(year = Year, value = all_of(col)) %>%
    mutate(value = to_mha(as.numeric(value), col, landuse_map[[class_name]]$fable_unit))

  hist_data    <- get_hist(class_name, x_max)
  source_label <- landuse_map[[class_name]]$hist_source

  p <- plot_ly() %>%
    add_trace(data = scen, x = ~year, y = ~value,
              type = "scatter", mode = "lines+markers",
              name = scenario_name,
              line   = list(color = line_color, width = 2),
              marker = list(color = line_color, size = 7),
              hovertemplate = paste0("%{x}: <b>%{y:.2f} Mha</b><extra>",
                                     scenario_name, "</extra>")) %>%
    add_hist_trace(hist_data, source_label)

  base_layout(p, paste0(class_name, ": ", scenario_name),
              line_color, x_max = x_max, y_range = y_range)
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "FABLE Calculator — Brazil",

  tabPanel("Land-use",
    br(),
    fluidRow(
      column(3,
        selectInput("scenario_sel", "Scenario:",
                    choices  = c("Both", "Current Trends", "NDC Commitments"),
                    selected = "Both")
      ),
      column(3,
        selectInput("years_sel", "Years:",
                    choices  = c("Calibration & Projections", "Calibration"),
                    selected = "Calibration & Projections")
      ),
      column(3,
        selectInput("class_sel", "Landuse Class:",
                    choices  = names(landuse_map),
                    selected = "Cropland")
      )
    ),
    uiOutput("charts_ui")
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  x_max <- reactive({
    if (input$years_sel == "Calibration") 2020L else 2050L
  })

  y_range <- reactive({
    calc_y_range(input$class_sel, x_max())
  })

  output$charts_ui <- renderUI({
    switch(input$scenario_sel,
      "Both"            = fluidRow(column(6, plotlyOutput("plot_both", height = "460px"))),
      "Current Trends"  = fluidRow(column(6, plotlyOutput("plot_ct",   height = "460px"))),
      "NDC Commitments" = fluidRow(column(6, plotlyOutput("plot_ndc",  height = "460px")))
    )
  })

  output$plot_both <- renderPlotly({
    make_combined_plot(input$class_sel, x_max(), y_range())
  })
  output$plot_ct <- renderPlotly({
    make_single_plot(input$class_sel, "Current Trends",  COL_CT,  x_max(), y_range())
  })
  output$plot_ndc <- renderPlotly({
    make_single_plot(input$class_sel, "NDC Commitments", COL_NDC, x_max(), y_range())
  })
}

shinyApp(ui, server)
