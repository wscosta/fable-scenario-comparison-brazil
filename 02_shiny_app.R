library(shiny)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(chorddiag))

# Run 01_process_data.R if any processed file is missing, if the scenario set
# on disk (data/xlsx/scenarios.csv) no longer matches what's cached, or if any
# source file was modified more recently than the cache. That last check is
# what catches an in-place edit to an existing xlsx (same filename/label) —
# the label-set comparison alone can't see that, since nothing about which
# scenarios exist actually changed.
PROCESSED_FILES <- c("data/processed/df_scenarios.rds", "data/processed/fable_units.rds",
                     "data/processed/df_crops.rds",     "data/processed/df_livestock.rds",
                     "data/processed/df_luc_matrix.rds", "data/processed/df_luc_stock.rds")

needs_reprocess <- function() {
  if (!all(file.exists(PROCESSED_FILES))) return(TRUE)

  scenario_meta <- read.csv("data/xlsx/scenarios.csv", stringsAsFactors = FALSE)
  cached  <- sort(unique(readRDS("data/processed/df_scenarios.rds")$scenario))
  current <- sort(scenario_meta$label)
  if (!identical(cached, current)) return(TRUE)

  source_files <- c("data/xlsx/scenarios.csv", "data/csv/histdatabrazil.csv",
                    file.path("data/xlsx", scenario_meta$file))
  source_files <- source_files[file.exists(source_files)]
  newest_source    <- max(file.info(source_files)$mtime)
  oldest_processed <- min(file.info(PROCESSED_FILES)$mtime)
  newest_source > oldest_processed
}
if (needs_reprocess()) source("01_process_data.R")

df_scenarios  <- readRDS("data/processed/df_scenarios.rds")
df_hist       <- readRDS("data/processed/df_hist.rds")
fable_units   <- readRDS("data/processed/fable_units.rds")
df_crops      <- readRDS("data/processed/df_crops.rds")
df_livestock  <- readRDS("data/processed/df_livestock.rds")
df_luc_matrix <- readRDS("data/processed/df_luc_matrix.rds")
df_luc_stock  <- readRDS("data/processed/df_luc_stock.rds")

# ── Scenario discovery, order, and colors ─────────────────────────────────────
# data/xlsx/scenarios.csv is the single source of truth for which scenarios
# exist and what order they display in (switches, legend, table rows, palette
# assignment) — adding a scenario is just a new xlsx + a new CSV row.
scenario_meta       <- read.csv("data/xlsx/scenarios.csv", stringsAsFactors = FALSE)
available_scenarios <- scenario_meta$label

SCENARIO_PALETTE <- c("#1565C0", "#009C3B", "#E65100", "#6A1B9A",
                      "#00838F", "#C62828", "#4527A0", "#AD1457")
scenario_colors <- setNames(
  SCENARIO_PALETTE[((seq_along(available_scenarios) - 1) %% length(SCENARIO_PALETTE)) + 1],
  available_scenarios
)

# Only the most recent UP calibration starts with its switches on — otherwise
# every new UP added to scenarios.csv piles onto an already-crowded default
# view. Driven by the optional `up` column (numeric calibration number); rows
# whose `up` equals the max `up` across the whole file default to checked.
# Falls back to "everything on" if `up` is absent/all-NA, so scenarios.csv
# without that column keeps working exactly as before.
default_scenarios <- if ("up" %in% names(scenario_meta) && any(!is.na(scenario_meta$up))) {
  scenario_meta$label[scenario_meta$up == max(scenario_meta$up, na.rm = TRUE)]
} else {
  available_scenarios
}

# The Maps tab only shows switches for scenarios with downscaled LUC data on
# disk (data/luc/downscaled_LUC_UP<up>_<ct|ndc>.rds, matched case-insensitive
# — the provided files have used both ct/ndc and CT/NDC casing) — a switch
# for a scenario with no data would just be a dead control that always shows
# "Map data not available". Recomputes automatically as downscaled data is
# added or removed, no code changes needed.
maps_scenario_pathway <- ifelse(grepl("NDC", scenario_meta$file, ignore.case = TRUE), "ndc", "ct")
maps_scenario_has_rds <- mapply(function(up, pw) {
  length(list.files("data/luc",
                    pattern = sprintf("^downscaled_LUC_UP%d_%s\\.rds$", up, pw),
                    ignore.case = TRUE)) > 0
}, scenario_meta$up, maps_scenario_pathway)
maps_available_scenarios <- scenario_meta$label[maps_scenario_has_rds]

# The Maps tab also needs its own default, separate from default_scenarios
# above: the highest `up` with downscaled data isn't necessarily the highest
# `up` overall (e.g. UP51 may exist in scenarios.csv with no downscaled data
# yet), and a diff map only makes sense when BOTH Current Trends and NDC are
# downscaled for the same UP. So default to the highest `up` that has both
# pathways' downscaled .rds present — today that's UP50 — falling back to
# whatever of default_scenarios is actually available if no UP has both.
maps_default_scenarios <- local({
  complete_ups <- intersect(scenario_meta$up[maps_scenario_pathway == "ct"  & maps_scenario_has_rds],
                            scenario_meta$up[maps_scenario_pathway == "ndc" & maps_scenario_has_rds])
  if (length(complete_ups) > 0)
    return(scenario_meta$label[scenario_meta$up == max(complete_ups, na.rm = TRUE)])
  utils::head(intersect(default_scenarios, maps_available_scenarios), 2)
})

# One switch per scenario instead of a fixed-choice checkboxGroupInput — any
# number of scenarios can be selected at once, same pattern as the sister
# MAgPIE app's scenario_switches_ui()/get_selected_scenarios_r().
scenario_switches_ui <- function(prefix, scenarios = available_scenarios, default_on = default_scenarios) {
  div(class = "scen-group",
      tags$label("Scenario", class = "control-label"),
      lapply(scenarios, function(s)
        input_switch(paste0(prefix, "_scen_", make.names(s)), s, value = s %in% default_on)))
}

get_selected_scenarios_r <- function(input, prefix, scenarios = available_scenarios) {
  scenarios[sapply(scenarios, function(s)
    isTRUE(input[[paste0(prefix, "_scen_", make.names(s))]]))]
}

# Icon-only "Chart type" picker (mirrors the sister MAgPIE app's UI) — a
# single button showing the currently-selected chart type's icon; clicking it
# opens a small dropdown menu (Line/Bar/Area, each icon + label) and picking
# one collapses back to just that icon. Everything downstream still reads
# input[[input_id]] and compares it against the literal strings "Line
# chart"/"Bar chart"/"Area chart" — this keeps those exact values/id by using
# a REAL (but visually hidden) radioButtons() under the hood; the dropdown is
# a pure CSS/JS skin driven by the shiny:connected handler in the page header,
# clicking a dropdown item just checks the matching hidden native radio and
# fires its `change` event, so nothing downstream needed to change.
CHART_TYPE_CHOICES <- c("Line chart", "Bar chart", "Area chart")
CHART_TYPE_ICONS   <- c("Line chart" = "chart-line", "Bar chart" = "chart-column", "Area chart" = "chart-area")

# choices/icons default to the Line/Bar/Area picker used by the 6 main tabs;
# passing a different pair (e.g. LUC_DIAGRAM_CHOICES/ICONS) reuses the exact
# same icon-dropdown widget for a different picker (Land Use Change's
# Chord/Sankey/Stacked Bar) — the shiny:connected JS handler queries
# `.chart-type-dropdown` generically by class/data-attributes, so it needs no
# changes to support a second instance with different choices.
chart_type_ui <- function(input_id, default = "Line chart", label = "Chart type",
                          choices = CHART_TYPE_CHOICES, icons = CHART_TYPE_ICONS) {
  div(class = "chart-type-dropdown dropdown", `data-chart-id` = input_id,
      tags$button(
        class = "btn btn-outline-secondary btn-sm dropdown-toggle chart-type-toggle",
        type = "button", `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
        title = label,
        tags$i(class = paste0("fas fa-", icons[[default]]))
      ),
      tags$ul(class = "dropdown-menu",
        lapply(choices, function(choice) {
          tags$li(tags$a(
            href = "#",
            class = paste("dropdown-item chart-type-item", if (choice == default) "active"),
            `data-value` = choice, `data-icon` = icons[[choice]],
            tags$i(class = paste0("fas fa-", icons[[choice]])), " ", choice
          ))
        })
      ),
      div(class = "chart-type-hidden-radio",
          radioButtons(input_id, label, choices = choices, selected = default))
  )
}

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
                      hist_type = "Cropland",                hist_source = "IBGE",
                      y_label   = "Area (Mha)"),
  "Pasture"    = list(fable_col = "CalcPasture",   fable_unit = "1000 ha",
                      hist_type = "Pastures and Rangelands", hist_source = "LAPIG",
                      y_label   = "Area (Mha)"),
  "Forest"     = list(fable_col = "CalcForest",    fable_unit = "1000 ha",
                      hist_type = "Forest",                  hist_source = "Mapbiomas",
                      y_label   = "Area (Mha)"),
  "Other Land" = list(fable_col = "CalcOtherLand", fable_unit = "1000 ha",
                      hist_type = "Other Land",              hist_source = "Mapbiomas",
                      y_label   = "Area (Mha)"),
  "Urban"      = list(fable_col = "CalcUrban",     fable_unit = "1000 ha",
                      hist_type = "Urban",                   hist_source = "Mapbiomas",
                      y_label   = "Area (Mha)")
)

# Keep only classes whose FABLE column exists in the data
fable_cols  <- names(df_scenarios)
landuse_map <- Filter(function(cfg) cfg$fable_col %in% fable_cols, landuse_map)

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
# Per-scenario colors come from scenario_colors (assigned above from
# SCENARIO_PALETTE by scenario order) instead of fixed CT/NDC constants.
COL_HIST <- "#000000"

hex_to_rgba <- function(hex, alpha = 0.25) {
  r <- strtoi(substr(hex, 2, 3), 16L)
  g <- strtoi(substr(hex, 4, 5), 16L)
  b <- strtoi(substr(hex, 6, 7), 16L)
  sprintf("rgba(%d,%d,%d,%.2f)", r, g, b, alpha)
}

# Shared scenario-trace builder — used by every chart in every tab so
# "Area chart" styling (black outline, colour fill at 25% alpha) etc. stays
# consistent instead of being reimplemented per tab.
add_scenario_trace <- function(p, dat, name, col, chart_type, hover) {
  if (chart_type == "Bar chart") {
    add_trace(p, data = dat, x = ~year, y = ~value, type = "bar", name = name,
              marker = list(color = col, line = list(color = "black", width = 1)),
              hovertemplate = hover)
  } else if (chart_type == "Area chart") {
    add_trace(p, data = dat, x = ~year, y = ~value, type = "scatter", mode = "lines",
              fill = "tozeroy", fillcolor = hex_to_rgba(col, 0.25), name = name,
              line = list(color = "black", width = 1.5), hovertemplate = hover)
  } else {
    add_trace(p, data = dat, x = ~year, y = ~value, type = "scatter", mode = "lines+markers",
              name = name, line = list(color = col, width = 2),
              marker = list(color = col, size = 7), hovertemplate = hover)
  }
}

# ── Shared layout helper ──────────────────────────────────────────────────────
base_layout <- function(p, title_text, title_color = "black", x_max, y_range,
                        y_label = "Area (Mha)", barmode = NULL, x_min = 2000L,
                        zero_line = FALSE) {
  is_bar  <- !is.null(barmode)
  x_range <- if (is_bar) c(x_min - 4, x_max + 4) else c(x_min - 1, x_max + 1)

  shapes <- list(
    list(type = "rect",
         xref = "paper", yref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 1,
         line = list(color = "black", width = 1),
         fillcolor = "rgba(0,0,0,0)")
  )
  if (x_max > 2020 && !is_bar) {
    shapes <- c(
      list(list(type = "line",
                x0 = 2020, x1 = 2020, yref = "paper", y0 = 0, y1 = 1,
                line = list(color = "grey", dash = "dash", width = 1.5))),
      shapes
    )
  }
  if (zero_line) {
    shapes <- c(
      list(list(type = "line",
                xref = "paper", yref = "y", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                line = list(color = "grey", dash = "dash", width = 1))),
      shapes
    )
  }

  p %>% plotly::layout(
    title = list(text = paste0("<b>", title_text, "</b>"),
                 font = list(color = title_color, size = 15)),
    xaxis = list(
      title     = "",
      tickvals  = seq(x_min, x_max, 5),
      tickangle = 0,
      range     = x_range,
      ticks     = "outside", ticklen = 6, tickcolor = "white",
      gridcolor = "#CCCCCC"
    ),
    yaxis = list(
      title     = y_label,
      range     = y_range,
      ticks     = "outside", ticklen = 6, tickcolor = "white",
      gridcolor = "#CCCCCC"
    ),
    legend = list(x = 1.02, y = 1, xanchor = "left", yanchor = "top",
                  bgcolor = "white", bordercolor = "black", borderwidth = 1),
    margin        = list(l = 70, r = 160, t = 50, b = 50),
    shapes        = shapes,
    barmode       = barmode,
    bargap        = if (is_bar) 0.5  else NULL,
    bargroupgap   = if (is_bar) 0.15 else NULL,
    plot_bgcolor  = "white",
    paper_bgcolor = "white"
  )
}

# ── Compute shared y-range ────────────────────────────────────────────────────
calc_y_range <- function(class_name, x_max, zero_base = TRUE) {
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
  y_min <- if (zero_base && min(all_vals, na.rm = TRUE) >= 0) 0
            else min(all_vals, na.rm = TRUE) - pad
  c(y_min, max(all_vals, na.rm = TRUE) + pad)
}

# ── Plot builders ─────────────────────────────────────────────────────────────
add_hist_trace <- function(p, hist_data, source_label, chart_type = "Line chart",
                           unit_label = "Mha") {
  if (nrow(hist_data) == 0) return(p)
  trace_name <- paste0("Historical (", source_label, ")")
  hover <- paste0("%{x}: <b>%{y:.2f} ", unit_label, "</b><extra>", trace_name, "</extra>")
  if (chart_type == "Bar chart") {
    add_trace(p, data = hist_data, x = ~year, y = ~value,
              type = "bar", name = trace_name,
              marker = list(color = "#1a1a1a",
                            line  = list(color = "black", width = 1)),
              hovertemplate = hover)
  } else if (chart_type == "Area chart") {
    add_trace(p, data = hist_data, x = ~year, y = ~value,
              type = "scatter", mode = "lines",
              name = trace_name,
              line = list(color = "black", width = 1.5, dash = "dash"),
              hovertemplate = hover)
  } else {
    add_trace(p, data = hist_data, x = ~year, y = ~value,
              type = "scatter", mode = "lines+markers",
              name = trace_name,
              line   = list(color = COL_HIST, width = 2),
              marker = list(color = COL_HIST, size = 7),
              hovertemplate = hover)
  }
}

make_plot <- function(class_name, scenario_sel, x_max, y_range, chart_type = "Line chart") {
  cfg <- landuse_map[[class_name]]
  p   <- plot_ly()

  for (s in scenario_sel) {
    dat <- df_scenarios %>%
      filter(scenario == s, Year <= x_max) %>%
      select(year = Year, value = all_of(cfg$fable_col)) %>%
      mutate(value = to_mha(as.numeric(value), cfg$fable_col, cfg$fable_unit))
    hover <- paste0("%{x}: <b>%{y:.2f} Mha</b><extra>", s, "</extra>")
    p <- add_scenario_trace(p, dat, s, scenario_colors[[s]], chart_type, hover)
  }

  hist_data <- get_hist(class_name, x_max)
  p <- add_hist_trace(p, hist_data, cfg$hist_source, chart_type)

  base_layout(p, class_name,
              x_max = x_max, y_range = y_range,
              y_label = cfg$y_label,
              barmode = if (chart_type == "Bar chart") "group" else NULL)
}

# ── Table builder ─────────────────────────────────────────────────────────────
make_table_data <- function(class_name, scenario_sel, x_max) {
  cfg   <- landuse_map[[class_name]]
  col   <- cfg$fable_col
  years <- seq(2000, x_max, 5)
  rows  <- list()

  pull_scenario <- function(scen_name) {
    raw <- df_scenarios %>%
      filter(scenario == scen_name, Year %in% years) %>%
      arrange(Year) %>%
      pull(all_of(col)) %>%
      as.numeric()
    to_mha(raw, col, cfg$fable_unit)
  }

  for (s in scenario_sel) rows[[s]] <- pull_scenario(s)

  hist_data <- get_hist(class_name, 2020)
  rows[["Historical"]] <- sapply(years, function(y) {
    if (y > 2020) return(NA_real_)
    v <- hist_data$value[hist_data$year == y]
    if (length(v) == 0) NA_real_ else v[1]
  })

  mat <- do.call(rbind, rows)
  df  <- as.data.frame(mat)
  colnames(df) <- as.character(years)
  cbind(` ` = rownames(df), df, stringsAsFactors = FALSE)
}

# ── Difference table builder ──────────────────────────────────────────────────
make_diff_data <- function(class_name, scenario_sel, type = "absolute") {
  cfg   <- landuse_map[[class_name]]
  col   <- cfg$fable_col
  years <- seq(2000, 2020, 5)

  hist_data <- get_hist(class_name, 2020)
  hist_vals <- sapply(years, function(y) {
    v <- hist_data$value[hist_data$year == y]
    if (length(v) == 0) NA_real_ else v[1]
  })

  pull_scenario <- function(scen_name) {
    raw <- df_scenarios %>%
      filter(scenario == scen_name, Year %in% years) %>%
      arrange(Year) %>%
      pull(all_of(col)) %>%
      as.numeric()
    to_mha(raw, col, cfg$fable_unit)
  }

  rows <- list()
  for (s in scenario_sel) {
    v <- pull_scenario(s)
    rows[[s]] <- if (type == "absolute") abs(v - hist_vals)
                 else (v - hist_vals) / hist_vals * 100
  }

  mat <- do.call(rbind, rows)
  df  <- as.data.frame(mat)
  colnames(df) <- as.character(years)
  cbind(` ` = rownames(df), df, stringsAsFactors = FALSE)
}

# ── Relative difference — colour text by threshold ───────────────────────────
color_rel_val <- function(v) {
  if (is.na(v)) return("")
  col <- if (abs(v) <= 10) "#009C3B"        # flag green
         else if (abs(v) <= 20) "#cc6600"   # orange
         else "#cc0000"                     # red
  sprintf('<span style="color:%s;font-weight:bold">%.2f</span>', col, v)
}

make_rel_diff_colored <- function(class_name, scenario_sel) {
  df    <- make_diff_data(class_name, scenario_sel, "relative")
  years <- names(df)[-1]
  for (y in years)
    df[[y]] <- vapply(as.numeric(df[[y]]), color_rel_val, character(1))
  df
}

# ── Unit conversion for production (to Mt) ───────────────────────────────────
to_mt <- function(values, col_name, unit_override = NULL) {
  unit <- if (!is.null(unit_override)) unit_override
          else trimws(tolower(fable_units[col_name]))
  if (is.na(unit))                              return(values)
  if (grepl("^mt$|^million.*t", unit))          return(values)
  if (grepl("^1[,.]?000\\s*t$|^kt$", unit))    return(values / 1000)
  if (grepl("^t$|^tonne", unit))                return(values / 1e6)
  values
}

# ── Crops configuration ───────────────────────────────────────────────────────
# Data comes from the second table in SCENATHON_report (header row 29).
# Columns used: Product, Year, ProdQ_feas (production), FeasHarvarea (area).
# NOTE: fable_product must match exact values in df_crops$Product.
# Run 01_process_data.R — it prints all unique product names found.
crops_map <- list(
  "Soybeans" = list(
    fable_product = "soyabean",
    area_unit     = "1000 ha",   # FeasHarvarea unit — verify from the Excel unit row
    prod_unit     = "1000 t",    # ProdQ_feas is in 1000 t → to_mt() divides by 1000
    hist_area_type = "Soybean Area",       hist_area_src = "IBGE",
    hist_prod_type = "Soybean Production", hist_prod_src = "IBGE"
  ),
  "Corn" = list(
    fable_product = "corn",
    area_unit     = "1000 ha",
    prod_unit     = "1000 t",    # ProdQ_feas in 1000 t → to_mt() divides by 1000
    hist_area_type = "Maize Area",       hist_area_src = "IBGE",
    hist_prod_type = "Maize Production", hist_prod_src = "IBGE"
  ),
  "Sugarcane" = list(
    fable_product = "sugarcane",
    area_unit     = "1000 ha",
    prod_unit     = "1000 t",    # ProdQ_feas in 1000 t → to_mt() divides by 1000
    hist_area_type = "Sugarcane Area",       hist_area_src = "IBGE",
    hist_prod_type = "Sugarcane Production", hist_prod_src = "IBGE"
  )
)
crops_map <- Filter(function(cfg) cfg$fable_product %in% df_crops$Product, crops_map)

crop_y_label    <- c("Area" = "Area (Mha)", "Production" = "Production (Mt)", "Yield" = "Yield (t/ha)")
crop_hover_unit <- c("Area" = "Mha",        "Production" = "Mt",              "Yield" = "t/ha")

# ── Crop data helpers ─────────────────────────────────────────────────────────
get_crop_hist_data <- function(crop_name, type_sel, x_max) {
  cfg  <- crops_map[[crop_name]]
  hmax <- min(x_max, 2020)

  get_metric <- function(hist_type, hist_src)
    df_hist %>%
      filter(trimws(type)   == hist_type,
             trimws(source) == hist_src,
             year > 1995, year <= hmax) %>%
      select(year, value) %>%
      mutate(value = as.numeric(value))

  if (type_sel == "Area")       return(get_metric(cfg$hist_area_type, cfg$hist_area_src))
  if (type_sel == "Production") return(get_metric(cfg$hist_prod_type, cfg$hist_prod_src))

  # Yield = Production / Area (Mt / Mha = t/ha)
  a <- get_metric(cfg$hist_area_type, cfg$hist_area_src)
  p <- get_metric(cfg$hist_prod_type, cfg$hist_prod_src)
  inner_join(a, p, by = "year", suffix = c("_a", "_p")) %>%
    mutate(value = value_p / value_a) %>%
    select(year, value)
}

get_crop_fable <- function(crop_name, type_sel, scenario_name, x_max) {
  cfg  <- crops_map[[crop_name]]
  rows <- df_crops %>%
    filter(scenario == scenario_name,
           trimws(Product) == cfg$fable_product,
           Year <= x_max) %>%
    arrange(Year)

  area <- to_mha(rows$FeasHarvarea, "FeasHarvarea", cfg$area_unit)
  prod <- to_mt(rows$ProdQ_feas,   "ProdQ_feas",   cfg$prod_unit)

  if (type_sel == "Area")       return(tibble(year = rows$Year, value = area))
  if (type_sel == "Production") return(tibble(year = rows$Year, value = prod))
  tibble(year = rows$Year, value = prod / area)  # Yield (t/ha)
}

calc_crop_y_range <- function(crop_name, type_sel, x_max, zero_base = TRUE) {
  scen_vals <- bind_rows(lapply(available_scenarios, get_crop_fable,
                                crop_name = crop_name, type_sel = type_sel, x_max = x_max))$value
  hist_vals <- get_crop_hist_data(crop_name, type_sel, x_max)$value
  all_vals  <- c(scen_vals, hist_vals)
  pad <- diff(range(all_vals, na.rm = TRUE)) * 0.05
  y_min <- if (zero_base && min(all_vals, na.rm = TRUE) >= 0) 0
            else min(all_vals, na.rm = TRUE) - pad
  c(y_min, max(all_vals, na.rm = TRUE) + pad)
}

# ── Crop plot builders ────────────────────────────────────────────────────────
make_crop_plot <- function(crop_name, type_sel, scenario_sel, x_max, y_range, chart_type) {
  y_label    <- crop_y_label[[type_sel]]
  unit_label <- crop_hover_unit[[type_sel]]
  p          <- plot_ly()

  for (s in scenario_sel) {
    dat   <- get_crop_fable(crop_name, type_sel, s, x_max)
    hover <- paste0("%{x}: <b>%{y:.2f} ", unit_label, "</b><extra>", s, "</extra>")
    p <- add_scenario_trace(p, dat, s, scenario_colors[[s]], chart_type, hover)
  }

  hist_data <- get_crop_hist_data(crop_name, type_sel, x_max)
  p <- add_hist_trace(p, hist_data, "IBGE", chart_type, unit_label)
  base_layout(p, crop_name,
              x_max = x_max, y_range = y_range, y_label = y_label,
              barmode = if (chart_type == "Bar chart") "group" else NULL)
}

# ── Crop table builders ───────────────────────────────────────────────────────
make_crop_table_data <- function(crop_name, type_sel, scenario_sel, x_max) {
  years <- seq(2000, x_max, 5)
  rows  <- list()

  pull_fable <- function(scen_name)
    get_crop_fable(crop_name, type_sel, scen_name, x_max) %>%
      filter(year %in% years) %>% arrange(year) %>% pull(value)

  for (s in scenario_sel) rows[[s]] <- pull_fable(s)

  hist_all <- get_crop_hist_data(crop_name, type_sel, 2020)
  rows[["Historical"]] <- sapply(years, function(y) {
    if (y > 2020) return(NA_real_)
    v <- hist_all$value[hist_all$year == y]
    if (length(v) == 0) NA_real_ else v[1]
  })

  mat <- do.call(rbind, rows)
  df  <- as.data.frame(mat)
  colnames(df) <- as.character(years)
  cbind(` ` = rownames(df), df, stringsAsFactors = FALSE)
}

make_crop_diff_data <- function(crop_name, type_sel, scenario_sel, type = "absolute") {
  years <- seq(2000, 2020, 5)

  hist_all  <- get_crop_hist_data(crop_name, type_sel, 2020)
  hist_vals <- sapply(years, function(y) {
    v <- hist_all$value[hist_all$year == y]
    if (length(v) == 0) NA_real_ else v[1]
  })

  pull_fable <- function(scen_name)
    get_crop_fable(crop_name, type_sel, scen_name, 2020) %>%
      filter(year %in% years) %>% arrange(year) %>% pull(value)

  rows <- list()
  for (s in scenario_sel) {
    v <- pull_fable(s)
    rows[[s]] <- if (type == "absolute") abs(v - hist_vals)
                 else (v - hist_vals) / hist_vals * 100
  }

  mat <- do.call(rbind, rows)
  df  <- as.data.frame(mat)
  colnames(df) <- as.character(years)
  cbind(` ` = rownames(df), df, stringsAsFactors = FALSE)
}

make_crop_rel_diff_colored <- function(crop_name, type_sel, scenario_sel) {
  df    <- make_crop_diff_data(crop_name, type_sel, scenario_sel, "relative")
  years <- names(df)[-1]
  for (y in years)
    df[[y]] <- vapply(as.numeric(df[[y]]), color_rel_val, character(1))
  df
}

# ── Livestock configuration ───────────────────────────────────────────────────
# Entries with fable_product come from df_crops (ProdQ_feas, 1000 t → Mt).
# Entries with fable_col come from df_livestock (5_feas_livestock sheet).
livestock_map <- list(
  "Beef Production"    = list(fable_product = "beef",    prod_unit = "1000 t",
                              hist_type = "Beef Production", hist_source = "IBGE", has_hist = TRUE,
                              y_label = "Production (Mt)", unit_label = "Mt"),
  "Milk Production"    = list(fable_product = "milk",    prod_unit = "1000 t",
                              hist_type = "Milk Production", hist_source = "IBGE", has_hist = TRUE,
                              y_label = "Production (Mt)", unit_label = "Mt"),
  "Chicken Production" = list(fable_product = "chicken", prod_unit = "1000 t",
                              hist_type = "Chicken Production", hist_source = "IBGE", has_hist = TRUE,
                              y_label = "Production (Mt)", unit_label = "Mt"),
  "Pork Production"    = list(fable_product = "pork",    prod_unit = "1000 t",
                              hist_type = "Pork Production", hist_source = "IBGE", has_hist = TRUE,
                              y_label = "Production (Mt)", unit_label = "Mt"),
  # FeasHerd/1000 lines up almost exactly with IBGE's head count (e.g. 2020:
  # 153.2 vs 152.5 million) — confirmed empirically this is head count, not
  # TLU, despite the unit/label previously assuming TLU. FAOSTAT's own
  # "Cattle Herd" series (~17-27 million throughout) is a different, much
  # smaller subset and is NOT used here.
  "Cattle Herd" = list(fable_col = "FeasHerd", unit_divisor = 1000,
                        hist_type = "Cattle Herd", hist_source = "IBGE", has_hist = TRUE,
                        y_label = "Cattle Herd (Million head)", unit_label = "Million head"),
  "Cattle Stocking Rate" = list(fable_col = "RumDensity", unit_divisor = 1, has_hist = FALSE,
                                 y_label = "Density (TLU/ha)", unit_label = "TLU/ha")
)
livestock_map <- Filter(function(cfg) {
  if (!is.null(cfg$fable_product)) cfg$fable_product %in% df_crops$Product
  else if (!is.null(cfg$fable_col)) cfg$fable_col %in% names(df_livestock)
  else FALSE
}, livestock_map)

get_live_fable <- function(product, scenario_name, x_max) {
  cfg <- livestock_map[[product]]
  if (!is.null(cfg$fable_col)) {
    divisor <- if (!is.null(cfg$unit_divisor)) cfg$unit_divisor else 1
    df_livestock %>%
      filter(scenario == scenario_name, Year <= x_max) %>%
      arrange(Year) %>%
      select(year = Year, value = all_of(cfg$fable_col)) %>%
      mutate(value = as.numeric(value) / divisor)
  } else {
    rows <- df_crops %>%
      filter(scenario == scenario_name,
             trimws(Product) == cfg$fable_product,
             Year <= x_max) %>%
      arrange(Year)
    tibble(year = rows$Year, value = to_mt(rows$ProdQ_feas, "ProdQ_feas", cfg$prod_unit))
  }
}

get_live_hist <- function(product, x_max) {
  cfg <- livestock_map[[product]]
  if (!cfg$has_hist) return(tibble(year = integer(), value = numeric()))
  hmax <- min(x_max, 2020)
  df_hist %>%
    filter(trimws(type)   == cfg$hist_type,
           trimws(source) == cfg$hist_source,
           year > 1995, year <= hmax) %>%
    select(year, value) %>%
    mutate(value = as.numeric(value))
}

calc_live_y_range <- function(product, x_max, zero_base = TRUE) {
  scen_vals <- bind_rows(lapply(available_scenarios, get_live_fable,
                                product = product, x_max = x_max))$value
  hist_vals <- get_live_hist(product, x_max)$value
  all_vals  <- c(scen_vals, hist_vals)
  pad <- diff(range(all_vals, na.rm = TRUE)) * 0.05
  y_min <- if (zero_base && min(all_vals, na.rm = TRUE) >= 0) 0
            else min(all_vals, na.rm = TRUE) - pad
  c(y_min, max(all_vals, na.rm = TRUE) + pad)
}

# ── Livestock plot builders ───────────────────────────────────────────────────
make_live_plot <- function(product, scenario_sel, x_max, y_range, chart_type) {
  cfg      <- livestock_map[[product]]
  unit_lbl <- cfg$unit_label
  y_lbl    <- cfg$y_label
  p        <- plot_ly()

  for (s in scenario_sel) {
    dat   <- get_live_fable(product, s, x_max)
    hover <- paste0("%{x}: <b>%{y:.2f} ", unit_lbl, "</b><extra>", s, "</extra>")
    p <- add_scenario_trace(p, dat, s, scenario_colors[[s]], chart_type, hover)
  }

  hist_data <- get_live_hist(product, x_max)
  src_label <- if (cfg$has_hist) cfg$hist_source else ""
  p <- add_hist_trace(p, hist_data, src_label, chart_type, unit_lbl)
  base_layout(p, product,
              x_max = x_max, y_range = y_range, y_label = y_lbl,
              barmode = if (chart_type == "Bar chart") "group" else NULL)
}

# ── Livestock table builders ──────────────────────────────────────────────────
make_live_table_data <- function(product, scenario_sel, x_max) {
  years <- seq(2000, x_max, 5)
  rows  <- list()

  pull_fable <- function(scen_name)
    get_live_fable(product, scen_name, x_max) %>%
      filter(year %in% years) %>% arrange(year) %>% pull(value)

  for (s in scenario_sel) rows[[s]] <- pull_fable(s)

  if (livestock_map[[product]]$has_hist) {
    hist_all <- get_live_hist(product, 2020)
    rows[["Historical"]] <- sapply(years, function(y) {
      if (y > 2020) return(NA_real_)
      v <- hist_all$value[hist_all$year == y]
      if (length(v) == 0) NA_real_ else v[1]
    })
  }

  mat <- do.call(rbind, rows)
  df  <- as.data.frame(mat)
  colnames(df) <- as.character(years)
  cbind(` ` = rownames(df), df, stringsAsFactors = FALSE)
}

make_live_diff_data <- function(product, scenario_sel, type = "absolute") {
  years <- seq(2000, 2020, 5)

  hist_all  <- get_live_hist(product, 2020)
  hist_vals <- sapply(years, function(y) {
    if (nrow(hist_all) == 0) return(NA_real_)
    v <- hist_all$value[hist_all$year == y]
    if (length(v) == 0) NA_real_ else v[1]
  })

  pull_fable <- function(scen_name)
    get_live_fable(product, scen_name, 2020) %>%
      filter(year %in% years) %>% arrange(year) %>% pull(value)

  rows <- list()
  for (s in scenario_sel) {
    v <- pull_fable(s)
    rows[[s]] <- if (type == "absolute") abs(v - hist_vals)
                 else (v - hist_vals) / hist_vals * 100
  }

  mat <- do.call(rbind, rows)
  df  <- as.data.frame(mat)
  colnames(df) <- as.character(years)
  cbind(` ` = rownames(df), df, stringsAsFactors = FALSE)
}

make_live_rel_diff_colored <- function(product, scenario_sel) {
  df    <- make_live_diff_data(product, scenario_sel, "relative")
  years <- names(df)[-1]
  for (y in years)
    df[[y]] <- vapply(as.numeric(df[[y]]), color_rel_val, character(1))
  df
}

# ── Trade configuration ───────────────────────────────────────────────────────
# Data from df_crops: Export_quantity / Import_quantity, both in 1000 t → Mt.
# hist_type maps each product to its matching histdatabrazil.csv "type" string
# (Comex = Brazil's own official foreign-trade statistics, used as the primary
# historical source here the same way IBGE is primary for domestic production
# elsewhere). MAPA rows (source == "MAPA", years 2025/2030/2035) are Brazil's
# Ministry of Agriculture's own forward projections, not historical
# observations — kept as a separate "Projections (MAPA)" trace/row rather than
# folded into "Historical" (see get_trade_projection/add_projection_trace).
# "Soybeans (all)" has no MAPA row of its own (no combined-product projection
# exists in the source data) — proj_hist_type gives it a vector of the 3
# sub-product MAPA types instead, so get_trade_projection sums grain+cake+oil
# per year rather than looking up a single (missing) type. hist_type (the
# real Comex/FAOSTAT historical series) is unaffected — "Soybean (all)
# Export" already exists directly in the source data for that.
TRADE_HIST_SOURCE <- "Comex"
TRADE_PROJ_SOURCE <- "MAPA"

trade_map <- list(
  "Exports" = list(
    fable_col = "Export_quantity",
    unit      = "1000 t",
    products  = c("Soybeans (all)", "Soybeans (grain)", "Soybeans (cake)", "Soybeans (oil)",
                  "Corn", "Beef"),
    fable_product = list(
      "Soybeans (all)"   = c("soyabean", "soycake", "soyoil"),
      "Soybeans (grain)" = "soyabean",
      "Soybeans (cake)"  = "soycake",
      "Soybeans (oil)"   = "soyoil",
      "Corn"             = "corn",
      "Beef"             = "beef"
    ),
    hist_type = list(
      "Soybeans (all)"   = "Soybean (all) Export",
      "Soybeans (grain)" = "Soybean (grain) Export",
      "Soybeans (cake)"  = "Soybean (cake) Export",
      "Soybeans (oil)"   = "Soybean (oil) Export",
      "Corn"             = "Corn Export",
      "Beef"             = "Beef Export"
    ),
    proj_hist_type = list(
      "Soybeans (all)" = c("Soybean (grain) Export", "Soybean (cake) Export", "Soybean (oil) Export")
    )
  ),
  "Imports" = list(
    fable_col = "Import_quantity",
    unit      = "1000 t",
    products  = c("Wheat"),
    fable_product = list("Wheat" = "wheat"),
    hist_type     = list("Wheat" = "Wheat Import")
  )
)

get_trade_fable <- function(trade_type, product, scenario_name, x_max) {
  cfg   <- trade_map[[trade_type]]
  col   <- cfg$fable_col
  prods <- cfg$fable_product[[product]]
  df_crops %>%
    filter(scenario == scenario_name,
           trimws(Product) %in% prods,
           Year <= x_max) %>%
    group_by(Year) %>%
    summarise(value = to_mt(sum(.data[[col]], na.rm = TRUE), col, cfg$unit),
              .groups = "drop") %>%
    arrange(Year) %>%
    rename(year = Year)
}

get_trade_hist <- function(trade_type, product, x_max) {
  ht <- trade_map[[trade_type]]$hist_type[[product]]
  if (is.null(ht)) return(tibble(year = integer(), value = numeric()))
  hmax <- min(x_max, 2020)
  df_hist %>%
    filter(trimws(type) == ht, trimws(source) == TRADE_HIST_SOURCE,
           year > 1995, year <= hmax) %>%
    select(year, value) %>%
    mutate(value = as.numeric(value))
}

get_trade_projection <- function(trade_type, product, x_max) {
  cfg <- trade_map[[trade_type]]
  ht  <- cfg$hist_type[[product]]
  if (is.null(ht)) return(tibble(year = integer(), value = numeric(), extrapolated = logical()))
  # proj_types is normally just ht (one type), but "Soybeans (all)" has no
  # MAPA row of its own — proj_hist_type gives it grain+cake+oil's types
  # instead, summed per year below, per explicit user request.
  proj_types <- if (!is.null(cfg$proj_hist_type[[product]])) cfg$proj_hist_type[[product]] else ht
  real <- df_hist %>%
    filter(trimws(type) %in% proj_types, trimws(source) == TRADE_PROJ_SOURCE, year <= x_max) %>%
    mutate(value = as.numeric(value)) %>%
    group_by(year) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(extrapolated = FALSE) %>%
    arrange(year)

  # Extend MAPA's own trend out to 2050 via linear regression on its three
  # real points (2025/2030/2035) — an OLS fit through all three captures the
  # growth rate across both the 2025->2030 and 2030->2035 intervals (not just
  # the last one), per explicit user request. Flagged `extrapolated = TRUE`
  # so the hover text can distinguish it from MAPA's own published figures.
  if (x_max >= 2050 && nrow(real) >= 2 && !(2050 %in% real$year)) {
    fit      <- lm(value ~ year, data = real)
    val_2050 <- unname(predict(fit, newdata = data.frame(year = 2050)))
    real <- bind_rows(real, tibble(year = 2050, value = val_2050, extrapolated = TRUE))
  }
  real
}

calc_trade_y_range <- function(trade_type, product, x_max, zero_base = TRUE) {
  scen_vals <- bind_rows(lapply(available_scenarios, get_trade_fable,
                               trade_type = trade_type, product = product, x_max = x_max))$value
  hist_vals <- get_trade_hist(trade_type, product, x_max)$value
  proj_vals <- get_trade_projection(trade_type, product, x_max)$value
  all_vals  <- c(scen_vals, hist_vals, proj_vals)
  pad <- diff(range(all_vals, na.rm = TRUE)) * 0.05
  y_min <- if (zero_base && min(all_vals, na.rm = TRUE) >= 0) 0
            else min(all_vals, na.rm = TRUE) - pad
  c(y_min, max(all_vals, na.rm = TRUE) + pad)
}

# Discrete future anchor points (MAPA's own 2025/2030/2035 projections, plus
# the linear-extrapolated 2050 point from get_trade_projection) — always
# rendered as markers regardless of chart_type, since these are isolated
# reference points, not a series meant to look like a line/bar/area. Salmon,
# translucent diamonds per explicit user request (previously solid grey).
add_projection_trace <- function(p, proj_data, unit_label = "Mt") {
  if (nrow(proj_data) == 0) return(p)
  trace_name  <- "Projections (MAPA)"
  proj_data$note <- ifelse(proj_data$extrapolated,
                            " (extrapolated from 2025-35 trend)", "")
  hover <- paste0("%{x}: <b>%{y:.2f} ", unit_label, "</b>%{customdata}<extra>", trace_name, "</extra>")
  add_trace(p, data = proj_data, x = ~year, y = ~value, customdata = ~note,
            type = "scatter", mode = "markers", name = trace_name,
            marker = list(color = hex_to_rgba("#FA8072", 0.55), size = 10, symbol = "diamond",
                          line = list(color = "#C0392B", width = 1)),
            hovertemplate = hover)
}

# ── Trade plot builders ───────────────────────────────────────────────────────
make_trade_plot <- function(trade_type, product, scenario_sel, x_max, y_range, chart_type) {
  p <- plot_ly()

  for (s in scenario_sel) {
    dat   <- get_trade_fable(trade_type, product, s, x_max)
    hover <- paste0("%{x}: <b>%{y:.2f} Mt</b><extra>", s, "</extra>")
    p <- add_scenario_trace(p, dat, s, scenario_colors[[s]], chart_type, hover)
  }

  hist_data <- get_trade_hist(trade_type, product, x_max)
  p <- add_hist_trace(p, hist_data, TRADE_HIST_SOURCE, chart_type, "Mt")

  proj_data <- get_trade_projection(trade_type, product, x_max)
  p <- add_projection_trace(p, proj_data, "Mt")

  base_layout(p, paste0(product, " ", trade_type),
              x_max = x_max, y_range = y_range, y_label = paste0(trade_type, " (Mt)"),
              barmode = if (chart_type == "Bar chart") "group" else NULL)
}

# ── Trade table builder ───────────────────────────────────────────────────────
make_trade_table_data <- function(trade_type, product, scenario_sel, x_max) {
  years <- seq(2000, x_max, 5)

  pull_fable <- function(scen_name)
    get_trade_fable(trade_type, product, scen_name, x_max) %>%
      filter(year %in% years) %>% arrange(year) %>% pull(value)

  rows <- list()
  for (s in scenario_sel) rows[[s]] <- pull_fable(s)

  hist_data <- get_trade_hist(trade_type, product, min(x_max, 2020))
  rows[["Historical"]] <- sapply(years, function(y) {
    if (y > 2020) return(NA_real_)
    v <- hist_data$value[hist_data$year == y]
    if (length(v) == 0) NA_real_ else v[1]
  })

  proj_data <- get_trade_projection(trade_type, product, x_max)
  if (nrow(proj_data) > 0) {
    rows[["Projections (MAPA)"]] <- sapply(years, function(y) {
      v <- proj_data$value[proj_data$year == y]
      if (length(v) == 0) NA_real_ else v[1]
    })
  }

  mat <- do.call(rbind, rows)
  df  <- as.data.frame(mat)
  colnames(df) <- as.character(years)
  cbind(` ` = rownames(df), df, stringsAsFactors = FALSE)
}

# ── Trade difference table builders ───────────────────────────────────────────
make_trade_diff_data <- function(trade_type, product, scenario_sel, type = "absolute") {
  years <- seq(2000, 2020, 5)

  hist_data <- get_trade_hist(trade_type, product, 2020)
  hist_vals <- sapply(years, function(y) {
    v <- hist_data$value[hist_data$year == y]
    if (length(v) == 0) NA_real_ else v[1]
  })

  pull_fable <- function(scen_name)
    get_trade_fable(trade_type, product, scen_name, 2020) %>%
      filter(year %in% years) %>% arrange(year) %>% pull(value)

  rows <- list()
  for (s in scenario_sel) {
    v <- pull_fable(s)
    rows[[s]] <- if (type == "absolute") abs(v - hist_vals)
                 else (v - hist_vals) / hist_vals * 100
  }

  mat <- do.call(rbind, rows)
  df  <- as.data.frame(mat)
  colnames(df) <- as.character(years)
  cbind(` ` = rownames(df), df, stringsAsFactors = FALSE)
}

make_trade_rel_diff_colored <- function(trade_type, product, scenario_sel) {
  df    <- make_trade_diff_data(trade_type, product, scenario_sel, "relative")
  years <- names(df)[-1]
  for (y in years)
    df[[y]] <- vapply(as.numeric(df[[y]]), color_rel_val, character(1))
  df
}

# ── Food configuration ────────────────────────────────────────────────────────
# Data from df_scenarios (aggregate table). No historical series.
food_map <- list(
  "Food Consumption" = list(
    fable_col = "kcal_feas",
    y_label   = "Intake (kcal/cap/day)"
  )
)

get_food_fable <- function(variable, scenario_name, x_max) {
  cfg <- food_map[[variable]]
  df_scenarios %>%
    filter(scenario == scenario_name, Year <= x_max) %>%
    select(year = Year, value = all_of(cfg$fable_col)) %>%
    mutate(value = as.numeric(value)) %>%
    arrange(year)
}

calc_food_y_range <- function(variable, x_max, zero_base = TRUE) {
  all_vals <- bind_rows(lapply(available_scenarios, get_food_fable,
                               variable = variable, x_max = x_max))$value
  pad <- diff(range(all_vals, na.rm = TRUE)) * 0.05
  y_min <- if (zero_base && min(all_vals, na.rm = TRUE) >= 0) 0
            else min(all_vals, na.rm = TRUE) - pad
  c(y_min, max(all_vals, na.rm = TRUE) + pad)
}

# ── Food plot builders ────────────────────────────────────────────────────────
make_food_plot <- function(variable, scenario_sel, x_max, y_range, chart_type) {
  cfg <- food_map[[variable]]
  p   <- plot_ly()

  for (s in scenario_sel) {
    dat   <- get_food_fable(variable, s, x_max)
    hover <- paste0("%{x}: <b>%{y:.0f} Intake (kcal/cap/day)</b><extra>", s, "</extra>")
    p <- add_scenario_trace(p, dat, s, scenario_colors[[s]], chart_type, hover)
  }

  base_layout(p, variable,
              x_max = x_max, y_range = y_range, y_label = cfg$y_label,
              barmode = if (chart_type == "Bar chart") "group" else NULL)
}

# ── Food table builder ────────────────────────────────────────────────────────
make_food_table_data <- function(variable, scenario_sel, x_max) {
  years <- seq(2000, x_max, 5)

  pull_fable <- function(scen_name)
    get_food_fable(variable, scen_name, x_max) %>%
      filter(year %in% years) %>% arrange(year) %>% pull(value)

  rows <- list()
  for (s in scenario_sel) rows[[s]] <- pull_fable(s)

  mat <- do.call(rbind, rows)
  df  <- as.data.frame(mat)
  colnames(df) <- as.character(years)
  cbind(` ` = rownames(df), df, stringsAsFactors = FALSE)
}

# ── Emissions configuration ───────────────────────────────────────────────────
# FABLE columns are already in Mt CO2e.
# Historical data (SEEG13) is in million tonnes of the gas → multiply by hist_gwp.
# GWP: CH4 = 27.2, N2O = 273, CO2 = 1.
emissions_map <- list(
  "CO₂ AFOLU" = list(
    fable_col   = "CalcAllLandCO2e",
    hist_type   = "CO2 AFOLU",
    hist_source = "SEEG13",
    hist_gwp    = 1,
    year_min    = 2005L,
    y_label     = "Emissions (MtCO₂e)"
  ),
  "CH₄ Enteric Fermentation" = list(
    fable_col   = "CalcLiveCH4",
    hist_type   = "CH4 Enteric Fermentation",
    hist_source = "SEEG13",
    hist_gwp    = 27.2,
    y_label     = "Emissions (MtCO₂e)"
  ),
  "CH₄ Rice" = list(
    fable_col   = "CalcCropCH4",
    hist_type   = "CH4 Rice",
    hist_source = "SEEG13",
    hist_gwp    = 27.2,
    y_label     = "Emissions (MtCO₂e)"
  ),
  "N₂O from Agriculture" = list(
    fable_cols  = c("CalcLiveN2O", "CalcCropN2O"),
    hist_types  = c("N2O Animal Waste Management", "N2O Burning of Crop Residues",
                    "N2O Decay of Crop Residues",  "N2O Inorganic Fertilizers",
                    "N2O Manure Applied to Croplands", "N2O Pasture",
                    "N2O Peatland", "N2O Soil Organic Matter Loss"),
    hist_source = "SEEG13",
    hist_gwp    = 273,
    y_label     = "Emissions (MtCO₂e)"
  )
)
emissions_map <- Filter(function(cfg) {
  cols <- if (!is.null(cfg$fable_cols)) cfg$fable_cols else cfg$fable_col
  all(cols %in% fable_cols)
}, emissions_map)

get_emiss_fable <- function(emiss_name, scenario_name, x_max) {
  cfg      <- emissions_map[[emiss_name]]
  year_min <- if (!is.null(cfg$year_min)) cfg$year_min else 2000L
  base <- df_scenarios %>% filter(scenario == scenario_name, Year >= year_min, Year <= x_max)
  if (!is.null(cfg$fable_cols)) {
    base %>%
      select(year = Year, all_of(cfg$fable_cols)) %>%
      mutate(value = rowSums(across(all_of(cfg$fable_cols), as.numeric), na.rm = TRUE)) %>%
      select(year, value)
  } else {
    base %>%
      select(year = Year, value = all_of(cfg$fable_col)) %>%
      mutate(value = as.numeric(value))
  }
}

get_emiss_hist <- function(emiss_name, x_max) {
  cfg        <- emissions_map[[emiss_name]]
  hmax       <- min(x_max, 2020)
  year_min   <- if (!is.null(cfg$year_min)) cfg$year_min else 2000L
  hist_types <- if (!is.null(cfg$hist_types)) cfg$hist_types else cfg$hist_type
  df_hist %>%
    filter(trimws(type) %in% hist_types,
           trimws(source) == cfg$hist_source,
           year >= year_min, year <= hmax) %>%
    group_by(year) %>%
    summarise(value = sum(as.numeric(value), na.rm = TRUE) * cfg$hist_gwp, .groups = "drop")
}

calc_emiss_y_range <- function(emiss_name, x_max, zero_base = TRUE) {
  scen_vals <- bind_rows(lapply(available_scenarios, get_emiss_fable,
                                emiss_name = emiss_name, x_max = x_max))$value
  hist_vals <- get_emiss_hist(emiss_name, x_max)$value
  all_vals  <- c(scen_vals, hist_vals)
  pad <- diff(range(all_vals, na.rm = TRUE)) * 0.05
  y_min <- min(all_vals, na.rm = TRUE)
  c(if (zero_base && y_min >= 0) 0 else y_min - pad, max(all_vals, na.rm = TRUE) + pad)
}

make_emiss_plot <- function(emiss_name, scenario_sel, x_max, y_range, chart_type) {
  cfg       <- emissions_map[[emiss_name]]
  p         <- plot_ly()

  for (s in scenario_sel) {
    dat   <- get_emiss_fable(emiss_name, s, x_max)
    hover <- paste0("%{x}: <b>%{y:.2f} MtCO₂e</b><extra>", s, "</extra>")
    p <- add_scenario_trace(p, dat, s, scenario_colors[[s]], chart_type, hover)
  }

  hist_data <- get_emiss_hist(emiss_name, x_max)
  p <- add_hist_trace(p, hist_data, "SEEG13", chart_type, "MtCO₂e")
  year_min <- if (!is.null(cfg$year_min)) cfg$year_min else 2000L
  base_layout(p, emiss_name,
              x_max = x_max, y_range = y_range, y_label = cfg$y_label,
              barmode = if (chart_type == "Bar chart") "group" else NULL,
              x_min = year_min,
              zero_line = emiss_name == "CO₂ AFOLU")
}

make_emiss_table_data <- function(emiss_name, scenario_sel, x_max) {
  year_min <- if (!is.null(emissions_map[[emiss_name]]$year_min)) emissions_map[[emiss_name]]$year_min else 2000L
  years <- seq(year_min, x_max, 5)
  rows  <- list()

  pull_fable <- function(scen_name)
    get_emiss_fable(emiss_name, scen_name, x_max) %>%
      filter(year %in% years) %>% arrange(year) %>% pull(value)

  for (s in scenario_sel) rows[[s]] <- pull_fable(s)

  hist_all <- get_emiss_hist(emiss_name, 2020)
  rows[["Historical"]] <- sapply(years, function(y) {
    if (y > 2020) return(NA_real_)
    v <- hist_all$value[hist_all$year == y]
    if (length(v) == 0) NA_real_ else v[1]
  })

  mat <- do.call(rbind, rows)
  df  <- as.data.frame(mat)
  colnames(df) <- as.character(years)
  cbind(` ` = rownames(df), df, stringsAsFactors = FALSE)
}

make_emiss_diff_data <- function(emiss_name, scenario_sel, type = "absolute") {
  year_min <- if (!is.null(emissions_map[[emiss_name]]$year_min)) emissions_map[[emiss_name]]$year_min else 2000L
  years <- seq(year_min, 2020, 5)

  hist_all  <- get_emiss_hist(emiss_name, 2020)
  hist_vals <- sapply(years, function(y) {
    v <- hist_all$value[hist_all$year == y]
    if (length(v) == 0) NA_real_ else v[1]
  })

  pull_fable <- function(scen_name)
    get_emiss_fable(emiss_name, scen_name, 2020) %>%
      filter(year %in% years) %>% arrange(year) %>% pull(value)

  rows <- list()
  for (s in scenario_sel) {
    v <- pull_fable(s)
    rows[[s]] <- if (type == "absolute") abs(v - hist_vals)
                 else (v - hist_vals) / hist_vals * 100
  }

  mat <- do.call(rbind, rows)
  df  <- as.data.frame(mat)
  colnames(df) <- as.character(years)
  cbind(` ` = rownames(df), df, stringsAsFactors = FALSE)
}

make_emiss_rel_diff_colored <- function(emiss_name, scenario_sel) {
  df    <- make_emiss_diff_data(emiss_name, scenario_sel, "relative")
  years <- names(df)[-1]
  for (y in years)
    df[[y]] <- vapply(as.numeric(df[[y]]), color_rel_val, character(1))
  df
}

# ── Land Use Change tab (Chord / Sankey / Stacked Bar) ────────────────────────
# Mirrors the sister MAgPIE app's Land Use Change tab (same diagram types,
# styling, JS/CSS fixes), but the underlying data is simpler here: both source
# tables (df_luc_matrix, df_luc_stock — see 01_process_data.R) already use
# column/value names matching the class names directly (ToForest, ToCropland,
# ...), so no "LUC|{from}|{to}" string-key matching is needed the way MAgPIE's
# long report.rds required.
#
# 6 classes (no Primary/Secondary Forest split here, unlike MAgPIE) —
# NewForest plays the role MAgPIE's Planted Forest did (a class that's 0 in
# some scenarios, active in others — confirmed empirically: 0 throughout in
# UP50 Current Trends, grows to 12,000 x 1000 ha by 2030 in UP50 NDC).
LUC_CLASSES       <- c("Forest", "Cropland", "Pasture", "OtherLand", "Urban", "NewForest")
LUC_CHORD_CLASSES <- c("Forest", "NewForest", "Cropland", "Pasture", "OtherLand", "Urban")
LUC_BAR_CLASSES   <- LUC_CLASSES

# One representative colour per class — reused directly from MAgPIE's palette
# for the equivalent role (forest = darkest green, NewForest/Planted Forest =
# light green, Cropland = egg-yolk yellow, Pasture = blue-leaning purple,
# Other Land = brownish, Urban = grey).
LUC_CLASS_COLORS <- c(
  "Forest"    = "#1B5E20",
  "NewForest" = "#A5D6A7",
  "Cropland"  = "#F4B400",
  "Pasture"   = "#5E35B1",
  "OtherLand" = "#8D6E63",
  "Urban"     = "#757575"
)

# Display labels for the Chord diagram's group names — internal class names
# (`OtherLand`, `NewForest`) stay unspaced everywhere else (column lookups,
# match()/dimnames keys), only the on-screen Chord labels get a space.
LUC_CLASS_LABELS <- c(
  "Forest"    = "Forest",
  "NewForest" = "New Forest",
  "Cropland"  = "Cropland",
  "Pasture"   = "Pasture",
  "OtherLand" = "Other Land",
  "Urban"     = "Urban"
)

LUC_DIAGRAM_CHOICES <- c("Chord", "Sankey", "Stacked Bar")
LUC_DIAGRAM_ICONS   <- c("Sankey" = "water", "Chord" = "circle-nodes", "Stacked Bar" = "chart-column")

# Sums the from->to flows over the periods between start_year and end_year
# (periods fully inside the window: YearStart >= start_year & YearEnd <=
# end_year) and converts 1000 ha -> Mha. Also adds one "stayed as X" row per
# class (from == to) so node/arc widths reflect each class's full start_year
# stock, not just the area that actually converted — persistence =
# AreaStart(X, start_year) - total outflow from X over the window, floored at
# 0. AreaStart is already keyed by (class, YearStart) in df_luc_matrix, so no
# separate stock lookup is needed here (simpler than MAgPIE, which had to
# cross-reference a different variable family for stock).
get_luc_flows <- function(scen, classes = LUC_CLASSES, start_year = 2020, end_year = 2050,
                          include_persistence = TRUE) {
  if (end_year <= start_year)
    return(data.frame(from = character(0), to = character(0), value = numeric(0)))

  sub <- df_luc_matrix %>%
    filter(scenario == scen, LandCoverInit %in% classes,
           YearStart >= start_year, YearEnd <= end_year)

  to_cols <- paste0("To", classes)
  long <- sub %>%
    select(from = LandCoverInit, all_of(to_cols)) %>%
    pivot_longer(all_of(to_cols), names_to = "to", values_to = "value") %>%
    mutate(to = sub("^To", "", to), value = value / 1000) %>%
    group_by(from, to) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    filter(from != to, value > 1e-4)

  if (include_persistence) {
    stock <- df_luc_matrix %>%
      filter(scenario == scen, LandCoverInit %in% classes, YearStart == start_year) %>%
      transmute(class = LandCoverInit, area_start = AreaStart / 1000)
    outflow <- long %>% group_by(from) %>% summarise(total = sum(value), .groups = "drop")
    pers <- stock %>%
      left_join(outflow, by = c("class" = "from")) %>%
      mutate(total = coalesce(total, 0), value = pmax(area_start - total, 0)) %>%
      filter(value > 1e-4) %>%
      transmute(from = class, to = class, value)
    long <- bind_rows(long, pers)
  }

  as.data.frame(long)
}

# Plain-text title shared by Sankey and Chord (chorddiag has no title concept
# of its own — see make_luc_server's renderUI grid for the plain <div> that
# draws this above a Chord widget instead).
luc_title_text <- function(scen, start_year = 2020, end_year = 2050)
  paste0(start_year, " → ", end_year, " | ", scen)

make_luc_sankey <- function(scen, classes = LUC_CLASSES, start_year = 2020, end_year = 2050) {
  flows <- get_luc_flows(scen, classes, start_year, end_year)
  title_txt  <- paste0("<b>", luc_title_text(scen, start_year, end_year), "</b>")
  title_font <- list(color = "black", size = 15)

  if (nrow(flows) == 0) {
    msg <- if (end_year <= start_year) "End year must be after start year"
           else "No land-use transitions in this window"
    return(plot_ly() %>%
      layout(title = list(text = title_txt, font = title_font),
             xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
             annotations = list(text = msg, showarrow = FALSE, font = list(size = 13))))
  }

  n <- length(classes)
  # plotly's Sankey trace has a rendering bug when explicit node x/y positions
  # are supplied (with or without arrangement = "fixed"): the right-side node
  # whose left-side counterpart sits at 0-based array index 0 gets rendered at
  # the wrong x (the left column's offset instead of its own) — reproduced and
  # confirmed via direct SVG <g class="node"> transform inspection. Confirmed
  # index-0-specific, not tied to any one class (reordering `classes` moved
  # the bug to whichever class then sat at position 0) and not tied to
  # arrangement = "fixed" specifically (removing just that string, keeping
  # x/y, reproduced the bug identically). A dummy-node-at-index-0 workaround
  # was also tried and did NOT fix it (bug just moved to the new index 0).
  # The only fix that worked: omit x/y entirely and let plotly's default
  # "snap" arrangement auto-layout the nodes. Trade-off: node vertical order
  # is no longer pinned to `classes`' order and may differ slightly across
  # scenario panels, since "snap" optimizes each panel's layout independently.
  plot_ly(
    type = "sankey", orientation = "h",
    node = list(
      label = c(classes, classes),
      color = unname(LUC_CLASS_COLORS[c(classes, classes)]),
      pad = 12, thickness = 16,
      line = list(color = "white", width = 0.5)
    ),
    link = list(
      source = match(flows$from, classes) - 1,
      target = match(flows$to,   classes) - 1 + n,
      value  = flows$value,
      color = mapply(function(from, to) {
        hex_to_rgba(unname(LUC_CLASS_COLORS[from]), 0.45)
      }, flows$from, flows$to),
      hovertemplate = "%{source.label} → %{target.label}: %{value:.2f} Mha<extra></extra>"
    )
  ) %>%
    layout(title = list(text = title_txt, font = title_font, y = 0.95, yanchor = "top"),
           font = list(size = 11),
           margin = list(t = 45, b = 10, l = 10, r = 10))
}

# Square from/to matrix for chorddiag — same flows get_luc_flows already
# returns (including the "stayed as X" diagonal), reshaped into a plain
# numeric matrix: row i -> column j.
luc_flow_matrix <- function(scen, classes = LUC_CLASSES, start_year = 2020, end_year = 2050) {
  flows <- get_luc_flows(scen, classes, start_year, end_year)
  n <- length(classes)
  m <- matrix(0, n, n, dimnames = list(classes, classes))
  if (nrow(flows) > 0)
    m[cbind(match(flows$from, classes), match(flows$to, classes))] <- flows$value
  m
}

# type = "directional" is chorddiag's own support for an asymmetric matrix
# (row -> column flows, tapered/coloured by source). Label line-wrap + keeping
# every label horizontal (rather than tangent-to-the-arc, chorddiag's default)
# is handled globally by a MutationObserver in tags$head() — see that
# comment (wrapChordLabel) for why a JS-side observer was needed instead of
# htmlwidgets::onRender(). classes defaults to LUC_CHORD_CLASSES (its own
# circular order, NewForest next to Forest) rather than LUC_CLASSES.
make_luc_chord <- function(scen, classes = LUC_CHORD_CLASSES, start_year = 2020, end_year = 2050) {
  m <- luc_flow_matrix(scen, classes, start_year, end_year)
  chorddiag(m, type = "directional",
           groupNames = unname(LUC_CLASS_LABELS[classes]), groupColors = unname(LUC_CLASS_COLORS[classes]),
           groupnamePadding = 50, groupnameFontsize = 11,
           tooltipUnit = " Mha", precision = 1, margin = 100)
}

# Fixed periods: the Stacked Bar always shows the full trajectory, no
# start/end window to pick. Unlike MAgPIE (whose cumulative-since-1995
# series needs a diff() and so has a bar for 2000 vs. the 1995 baseline),
# df_luc_stock has no year before 2000 to diff against, so the first bar here
# is 2005 (change vs. 2000), not 2000 — an intentional structural difference,
# not a bug.
LUC_NET_CHANGE_PERIODS <- seq(2005, 2050, by = 5)

get_luc_net_change <- function(scen, classes = LUC_CLASSES, periods = LUC_NET_CHANGE_PERIODS) {
  sub <- df_luc_stock %>% filter(scenario == scen, class %in% classes, year %in% c(periods[1] - 5, periods))
  do.call(rbind, lapply(classes, function(cl) {
    d <- sub[sub$class == cl, ]
    d <- d[order(d$year), ]
    if (nrow(d) < 2) return(data.frame(class = cl, year = periods, value_mha = 0))
    data.frame(class = cl, year = d$year[-1], value_mha = diff(d$area) / 1000)
  }))
}

# Shared y-axis range across every currently-shown scenario's Stacked Bar,
# always on (not an optional toggle) — sum just the positive-valued classes
# (top of that bar's stack) and just the negative ones (bottom) per
# scenario/period; the overall max positive sum and min negative sum across
# every scenario/period become one fixed [min, max] range, so bar heights are
# directly comparable across scenarios.
calc_luc_bar_y_range_shared <- function(scenarios, classes = LUC_BAR_CLASSES, periods = LUC_NET_CHANGE_PERIODS) {
  stacks <- unlist(lapply(scenarios, function(s) {
    df <- get_luc_net_change(s, classes, periods)
    sapply(periods, function(yr) sum(df$value_mha[df$year == yr & df$value_mha > 0]))
  }))
  neg_stacks <- unlist(lapply(scenarios, function(s) {
    df <- get_luc_net_change(s, classes, periods)
    sapply(periods, function(yr) sum(df$value_mha[df$year == yr & df$value_mha < 0]))
  }))
  pos_max <- if (length(stacks) == 0) 1 else max(stacks, 0)
  neg_min <- if (length(neg_stacks) == 0) 0 else min(neg_stacks, 0)
  pad <- max(pos_max - neg_min, 1) * 0.05
  c(neg_min - pad, pos_max + pad)
}

# Diverging stacked bar: barmode = "relative" stacks positive segments above
# zero and negative ones below it. Zero line drawn as a bold black dotted
# `shapes` line (more prominent than plotly's default faint zeroline) to make
# gains vs losses unambiguous. showlegend = FALSE — the per-class legend is
# shown once, shared above the whole grid (make_luc_class_legend_html()).
make_luc_stackedbar <- function(scen, classes = LUC_BAR_CLASSES, periods = LUC_NET_CHANGE_PERIODS,
                                y_range = NULL) {
  df <- get_luc_net_change(scen, classes, periods)

  p <- plot_ly()
  for (cl in classes) {
    d <- df[df$class == cl, ]
    p <- add_trace(p, data = d, x = ~year, y = ~value_mha, type = "bar", name = cl,
                   marker = list(color = LUC_CLASS_COLORS[[cl]], line = list(color = "black", width = 1)),
                   hovertemplate = paste0(cl, ": %{y:.2f} Mha<extra></extra>"))
  }
  p %>% layout(
    barmode = "relative", showlegend = FALSE,
    title  = list(text = paste0("<b>", scen, "</b>"), font = list(color = "black", size = 15)),
    xaxis  = list(title = "", tickvals = periods,
                  ticks = "outside", ticklen = 6, tickcolor = "white", gridcolor = "#CCCCCC"),
    yaxis  = list(title = "Change vs. previous period (Mha)", tickformat = ".1f", range = y_range,
                  ticks = "outside", ticklen = 6, tickcolor = "white", gridcolor = "#CCCCCC"),
    shapes = list(
      list(type = "line", xref = "paper", yref = "y", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
           line = list(color = "black", dash = "dot", width = 2)),
      list(type = "rect", xref = "paper", yref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 1,
           line = list(color = "black", width = 1), fillcolor = "rgba(0,0,0,0)")
    ),
    margin = list(l = 70, r = 20, t = 50, b = 50),
    plot_bgcolor = "white", paper_bgcolor = "white"
  )
}

# Shared class-colour legend for the Stacked Bar diagram, shown once above
# the whole grid instead of repeating it on every scenario's panel.
make_luc_class_legend_html <- function(classes = LUC_BAR_CLASSES) {
  items <- lapply(classes, function(cl) {
    col <- LUC_CLASS_COLORS[[cl]]
    tags$span(
      style = "display:inline-flex; align-items:center; gap:5px; margin:0 14px;",
      tags$span(style = sprintf(
        "display:inline-block; width:12px; height:12px; background:%s; border:1px solid black; border-radius:2px;",
        col)),
      tags$span(cl, style = "font-size:12px;")
    )
  })
  div(style = paste("display:flex; justify-content:center; flex-wrap:wrap;",
                    "padding:6px; border:1px solid black; border-radius:4px;",
                    "background:white; margin-bottom:8px;"),
      items)
}

luc_sidebar_ui <- function() {
  tagList(
    scenario_switches_ui("luc"),
    # Hidden for Stacked Bar — that chart always shows the full trajectory
    # (see make_luc_stackedbar), so a start/end window has no meaning there.
    conditionalPanel(
      condition = "input.luc_diagram != 'Stacked Bar'",
      sliderInput("luc_years", "Period", min = 2000, max = 2050, step = 5,
                 value = c(2020, 2050), sep = "", ticks = FALSE),
      div(style = "position:relative; height:14px; margin-top:-6px; font-size:11px; color:#666;",
          span("2000", style = "position:absolute; left:0;"),
          span("2020", style = "position:absolute; left:40%; transform:translateX(-50%);"),
          span("2050", style = "position:absolute; right:0;")
      )
    ),
    chart_type_ui("luc_diagram", choices = LUC_DIAGRAM_CHOICES, icons = LUC_DIAGRAM_ICONS,
                  default = "Chord", label = "Diagram type")
  )
}

# One Sankey/Chord/Stacked Bar per selected scenario, side by side — same
# "toggle a scenario, see its own chart appear" idea as elsewhere in the app.
# Render functions are pre-registered once per available_scenarios (not
# inside the dynamic UI) — only the currently-selected ones get a placeholder
# in the DOM to bind to.
make_luc_server <- function(input, output, session) {
  luc_scen_sel <- reactive(get_selected_scenarios_r(input, "luc"))

  bar_y_range_r <- reactive({
    req(length(luc_scen_sel()) > 0)
    calc_luc_bar_y_range_shared(luc_scen_sel())
  })

  output$luc_grid <- renderUI({
    req(length(luc_scen_sel()) > 0)
    scens   <- luc_scen_sel()
    ncol    <- min(length(scens), 3)
    w       <- floor(12 / ncol)
    diagram <- input$luc_diagram %||% "Chord"
    yrs     <- input$luc_years %||% c(2020, 2050)

    # chorddiagOutput/plotlyOutput default to a small fixed height, which
    # bottlenecks the diagram into a small circle/chart even inside a much
    # wider column — height tiered by ncol as a stand-in for "how wide is
    # this column actually going to be" (fewer scenarios -> wider columns ->
    # a bigger height cap makes sense).
    panel_h <- if (ncol == 1) "760px" else if (ncol == 2) "620px" else "480px"

    grid <- fluidRow(lapply(scens, function(s) {
      sid <- make.names(s)
      widget <- if (diagram == "Chord") {
        tagList(
          div(luc_title_text(s, yrs[1], yrs[2]),
              style = "font-weight:bold; color:black; font-size:15px; text-align:center; margin-bottom:4px;"),
          chorddiagOutput(paste0("luc_chord_", sid), width = "100%", height = panel_h)
        )
      } else if (diagram == "Stacked Bar") {
        plotlyOutput(paste0("luc_bar_", sid), width = "100%", height = panel_h)
      } else {
        plotlyOutput(paste0("luc_plot_", sid), height = "420px")
      }
      column(width = w, div(class = "luc-frame", style = "flex-direction:column;", widget))
    }))

    if (diagram == "Stacked Bar") tagList(make_luc_class_legend_html(), grid) else grid
  })

  for (s in available_scenarios) {
    local({
      scen <- s
      sid  <- make.names(scen)
      on_r <- reactive(isTRUE(input[[paste0("luc_scen_", sid)]]))

      output[[paste0("luc_plot_", sid)]] <- renderPlotly({
        req(on_r(), input$luc_years)
        make_luc_sankey(scen, start_year = input$luc_years[1], end_year = input$luc_years[2])
      })

      output[[paste0("luc_chord_", sid)]] <- renderChorddiag({
        req(on_r(), input$luc_years)
        make_luc_chord(scen, start_year = input$luc_years[1], end_year = input$luc_years[2])
      })

      output[[paste0("luc_bar_", sid)]] <- renderPlotly({
        req(on_r())
        make_luc_stackedbar(scen, y_range = bar_y_range_r())
      })
    })
  }
}

# ── Static assets ─────────────────────────────────────────────────────────────
addResourcePath("images", normalizePath("data/images", mustWork = FALSE))
addResourcePath("maps",   normalizePath("data/maps",   mustWork = FALSE))

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = HTML('<span style="display:inline-flex; align-items:center; gap:8px;"><img src="images/fable_logo.png" height="26" style="border-radius:4px;">FABLE-Calculator Brazil</span>'),
  window_title = "FABLE-Calculator Brazil",
  header = tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "images/favicon.svg"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Raleway:wght@600&family=Montserrat:wght@400;500&display=swap"),
    tags$style(HTML("
      .navbar { background: linear-gradient(90deg, #2E7D32 0%, #C8A000 25%, #002776 50%, #C8A000 75%, #2E7D32 100%) !important; }
      .navbar-brand, .navbar .nav-link, .navbar .navbar-text { text-shadow: 0 1px 3px rgba(0,0,0,0.55) !important; }
      .navbar-brand span { font-family: 'Raleway', sans-serif !important; font-weight: 600 !important; letter-spacing: 0.02em; }
      .navbar .nav-link  { font-family: 'Montserrat', sans-serif !important; font-weight: 500 !important; letter-spacing: 0.01em; }
      .navbar .nav-link         { color: rgba(255,255,255,0.85) !important; }
      .navbar .nav-link:hover   { color: rgba(255,255,255,0.97) !important; }
      .navbar .nav-link.active  { color: rgba(255,255,255,1.00) !important; }
      #fcid-logo-wrap {
        position: absolute !important;
        right: 16px;
        top: 0;
        bottom: 0;
        display: flex !important;
        align-items: center;
        pointer-events: none;
      }
      #fcid-logo { height: 68px; opacity: 0.92; }
      .navbar .container-fluid {
        display: flex !important;
        flex-wrap: wrap !important;
      }
      .navbar-brand {
        flex: 0 0 100% !important;
        padding-bottom: 2px;
      }
      .navbar-collapse {
        flex: 0 0 100% !important;
      }
      .navbar-nav {
        flex-direction: row !important;
      }
      .navbar-nav .nav-link {
        font-size: 1rem;
      }
      .tab-pane > br:first-child {
        display: none;
      }
      .tab-content {
        padding-top: 8px;
      }
      .bslib-sidebar-layout > .sidebar {
        font-size: 0.72rem;
      }
      .bslib-sidebar-layout > .sidebar .control-label {
        font-weight: 600;
      }
      .selectize-input,
      .selectize-dropdown {
        font-size: 0.72rem !important;
      }
      /* bslib's sidebar-content is a flex column with gap: 1rem between every
         top-level child, ON TOP OF each control's own margin — that (not the
         individual control margins) is the main source of large sidebar gaps. */
      .bslib-sidebar-layout > .sidebar > .sidebar-content {
        gap: 8px;
      }
      .bslib-sidebar-layout > .sidebar .shiny-input-container {
        margin-bottom: 0;
      }
      .scen-group .shiny-input-container {
        margin-bottom: 2px;
      }
      /* Maps tab — year button-group */
      #map_year.shiny-input-container { margin-bottom: 0; }
      #map_prev_year, #map_next_year {
        font-size: 0.9rem !important;
        line-height: 1.4;
        padding: 5px 10px;
      }
      #map_year .shiny-options-group {
        display: inline-flex !important;
        border: 1.5px solid #007B8A;
        border-radius: 4px;
        overflow: hidden;
      }
      #map_year .radio,
      #map_year .radio-inline,
      #map_year .form-check {
        margin: 0 !important;
        padding: 0 !important;
      }
      #map_year input[type='radio'] {
        position: absolute !important;
        opacity: 0 !important;
        width: 0 !important;
        height: 0 !important;
        margin: 0 !important;
        padding: 0 !important;
      }
      #map_year label,
      #map_year .form-check-label {
        margin: 0 !important;
        padding: 0 !important;
        display: block;
      }
      #map_year label span {
        display: block;
        padding: 5px 14px;
        cursor: pointer;
        background: white;
        color: #007B8A;
        font-weight: 500;
        font-size: 0.9rem;
        white-space: nowrap;
        line-height: 1.4;
      }
      #map_year label:hover span { background: #e0f2f4; }
      #map_year input[type='radio']:checked + span {
        background: #007B8A;
        color: white;
      }
      /* Maps tab — images fit viewport */
      .maps-img {
        display: block;
        margin: 0 auto;
        max-width: 100%;
        max-height: calc(100vh - 210px);
        width: auto;
        height: auto;
      }
      /* Breathing room before Years in the chart tabs, same as Layers gets
         in the map tabs of the sister MAgPIE app. */
      .years-group { margin-top: 8px; }
      /* chart_type_ui() — icon-only Chart type dropdown (mirrors the sister
         MAgPIE app's UI), replacing the old text-labelled radioButtons group.
         The real Shiny-bound radioButtons is still there (see that function's
         comment) but visually hidden; only the styled toggle button + dropdown
         menu are visible. */
      .chart-type-hidden-radio { display: none; }
      .chart-type-toggle {
        width: 34px; height: 30px; padding: 0; display: inline-flex;
        align-items: center; justify-content: center;
      }
      .chart-type-toggle::after { display: none; }
      .chart-type-dropdown .dropdown-item.active,
      .chart-type-dropdown .dropdown-item:active {
        background-color: #007B8A; color: white;
      }
      .chart-type-dropdown .dropdown-item i { width: 16px; text-align: center; margin-right: 4px; }
      /* Land Use Change tab: each scenario's diagram sits in its own box, but
         an invisible one on purpose — no border/background, just containment
         (a chart can't visually bleed into its neighbour when the grid
         rebuilds on every scenario toggle). Flex centering keeps the diagram
         (and, for Chord, its title div) centered in the box by default. */
      .luc-frame {
        position: relative; overflow: hidden;
        display: flex; align-items: center; justify-content: center;
      }
    ")),
    tags$script(HTML("
      // Icon-only Chart type dropdown (chart_type_ui()) — the real Shiny
      // input is a hidden radioButtons(); this just keeps the visible
      // dropdown toggle/menu in sync with it. Every chart_type_ui() instance
      // is part of the static sidebar UI (never (re)created via renderUI),
      // so a single one-time binding on shiny:connected is enough.
      $(document).on('shiny:connected', function() {
        document.querySelectorAll('.chart-type-dropdown').forEach(function(dd) {
          var fullId = dd.getAttribute('data-chart-id');
          var toggle = dd.querySelector('.chart-type-toggle');
          dd.querySelectorAll('.chart-type-item').forEach(function(item) {
            item.addEventListener('click', function(e) {
              e.preventDefault();
              var val   = item.getAttribute('data-value');
              var icon  = item.getAttribute('data-icon');
              var radio = dd.querySelector('input[name=\"' + fullId + '\"][value=\"' + val + '\"]');
              if (radio) { radio.checked = true; $(radio).trigger('change'); }
              toggle.innerHTML = '<i class=\"fas fa-' + icon + '\"></i>';
              dd.querySelectorAll('.chart-type-item').forEach(function(i2) { i2.classList.remove('active'); });
              item.classList.add('active');
            });
          });
        });

        // Land Use Change tab — Chord diagrams: chorddiag draws each group
        // name as a single-line SVG text element with no wrap support at all
        // (see its own chorddiag.js — plain .text(d.label)), so a long name
        // like 'OtherLand' can crowd its neighbour on a small arc. A
        // MutationObserver reacts to the actual DOM change (a fresh
        // single-line text element appearing under g.names) instead of a
        // render-lifecycle hook — chorddiag's own resize() method (fired on
        // every container-size change, e.g. toggling a scenario) rebuilds
        // the SVG directly, bypassing htmlwidgets' render-complete dispatch
        // that a onRender() hook would depend on. Idempotent (skips a
        // <text> that already has a tspan child) so it can't double-wrap or
        // loop on its own mutations.
        // De-rotation: chorddiag positions each label's parent <g> with
        // rotate(angle-90) translate(r, 0) — a polar-coordinates trick that
        // also rotates the label text to follow the arc tangentially. We
        // want the SAME position but no rotation, so the label stays upright
        // and readable everywhere on the circle: read r back out of the
        // existing transform, recompute the equivalent Cartesian (x, y) from
        // the datum's own angle, and replace the transform with a plain
        // translate — no rotate. Guarded by data-derotated so re-running
        // this (the observer fires on every fresh render) doesn't re-derive
        // from an already-rewritten transform.
        function wrapChordLabel(textEl) {
          var g = textEl.parentNode;
          if (g && !g.getAttribute('data-derotated')) {
            var datum = d3.select(g).datum();
            var m = /translate\\(\\s*([-\\d.]+)/.exec(g.getAttribute('transform') || '');
            if (datum && m) {
              var r   = parseFloat(m[1]);
              var phi = datum.angle - Math.PI / 2;
              g.setAttribute('transform', 'translate(' + (r * Math.cos(phi)) + ',' + (r * Math.sin(phi)) + ')');
              g.setAttribute('data-derotated', '1');
            }
            d3.select(textEl).attr('transform', null);
          }
          var text = d3.select(textEl);
          if (!text.select('tspan').empty()) return;
          var words = text.text().split(' ');
          if (words.length < 2) return;
          var bestI = 1, bestDiff = Infinity;
          for (var i = 1; i < words.length; i++) {
            var diff = Math.abs(words.slice(0, i).join(' ').length - words.slice(i).join(' ').length);
            if (diff < bestDiff) { bestDiff = diff; bestI = i; }
          }
          text.text(null);
          text.append('tspan').attr('x', 0).attr('dy', '-0.3em').text(words.slice(0, bestI).join(' '));
          text.append('tspan').attr('x', 0).attr('dy', '1.1em').text(words.slice(bestI).join(' '));
        }
        new MutationObserver(function(mutations) {
          mutations.forEach(function(m) {
            m.addedNodes.forEach(function(node) {
              if (node.nodeType !== 1) return;
              if (node.matches && node.matches('g.names text')) wrapChordLabel(node);
              if (node.querySelectorAll)
                node.querySelectorAll('g.names text').forEach(wrapChordLabel);
            });
          });
        }).observe(document.body, { childList: true, subtree: true });
      });
    "))
  ),
  theme = bs_theme(primary = "#007B8A", version = 5),
  bg = "#007B8A",

  nav_panel(HTML("&#x1F33F; Land Use"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput("class_sel", "Landuse Class",
                    choices  = names(landuse_map),
                    selected = "Forest"),
        scenario_switches_ui("lu"),
        div(class = "years-group",
            tags$label("Years", class = "control-label"),
            input_switch("years_sel", "Calibration Only", value = FALSE)),
        checkboxInput("zero_base", "Start y-axis at zero", value = TRUE),
        chart_type_ui("chart_type")
      ),
      uiOutput("charts_ui")
    )
  ),

  nav_panel(HTML("&#x1F504; Land Use Change"),
    layout_sidebar(
      sidebar = sidebar(luc_sidebar_ui()),
      uiOutput("luc_grid")
    )
  ),

  nav_panel(HTML("🌫️ Emissions"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput("emiss_sel", "Emission",
                    choices  = names(emissions_map),
                    selected = "CO₂ AFOLU"),
        scenario_switches_ui("emiss"),
        div(class = "years-group",
            tags$label("Years", class = "control-label"),
            input_switch("emiss_years", "Calibration Only", value = FALSE)),
        checkboxInput("emiss_zero_base", "Start y-axis at zero", value = FALSE),
        chart_type_ui("emiss_chart_type")
      ),
      uiOutput("emiss_charts_ui")
    )
  ),

  nav_panel(HTML("🌾 Crops"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput("crop_name", "Crop",
                    choices  = c("Soybeans", "Corn", "Sugarcane"),
                    selected = "Soybeans"),
        selectInput("crop_type", "Type",
                    choices  = c("Area", "Production", "Yield"),
                    selected = "Area"),
        scenario_switches_ui("crop"),
        div(class = "years-group",
            tags$label("Years", class = "control-label"),
            input_switch("crop_years", "Calibration Only", value = FALSE)),
        checkboxInput("crop_zero_base", "Start y-axis at zero", value = TRUE),
        chart_type_ui("crop_chart_type")
      ),
      uiOutput("crop_charts_ui")
    )
  ),

  nav_panel(HTML("🐄 Livestock"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput("live_product", "Variable",
                    choices  = c("Beef Production", "Milk Production",
                                 "Chicken Production", "Pork Production",
                                 "Cattle Herd", "Cattle Stocking Rate"),
                    selected = "Beef Production"),
        scenario_switches_ui("live"),
        div(class = "years-group",
            tags$label("Years", class = "control-label"),
            input_switch("live_years", "Calibration Only", value = FALSE)),
        checkboxInput("live_zero_base", "Start y-axis at zero", value = TRUE),
        chart_type_ui("live_chart_type")
      ),
      uiOutput("live_charts_ui")
    )
  ),

  nav_panel(HTML("🚢 Trade"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput("trade_type", "Type",
                    choices  = c("Exports", "Imports"),
                    selected = "Exports"),
        selectInput("trade_product", "Product",
                    choices  = c("Soybeans (all)", "Soybeans (grain)", "Soybeans (cake)",
                                 "Soybeans (oil)", "Corn", "Beef"),
                    selected = "Soybeans (all)"),
        scenario_switches_ui("trade"),
        div(class = "years-group",
            tags$label("Years", class = "control-label"),
            input_switch("trade_years", "Calibration Only", value = FALSE)),
        checkboxInput("trade_zero_base", "Start y-axis at zero", value = TRUE),
        chart_type_ui("trade_chart_type")
      ),
      uiOutput("trade_charts_ui")
    )
  ),

  nav_panel(HTML("🍽️ Food"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput("food_variable", "Variable",
                    choices  = names(food_map),
                    selected = "Food Consumption"),
        scenario_switches_ui("food"),
        div(class = "years-group",
            tags$label("Years", class = "control-label"),
            input_switch("food_years", "Calibration Only", value = FALSE)),
        checkboxInput("food_zero_base", "Start y-axis at zero", value = TRUE),
        chart_type_ui("food_chart_type")
      ),
      uiOutput("food_charts_ui")
    )
  ),
  nav_panel(HTML("🌎 Maps"),
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("map_type", "Map Type",
                     choices  = c("Land Cover", "Outflows", "Transitions"),
                     selected = "Land Cover"),
        scenario_switches_ui("maps", scenarios = maps_available_scenarios, default_on = maps_default_scenarios),
        tags$p("Pick up to 2 scenarios to compare.",
               style = "font-size:0.75rem; color:#666; margin-top:-8px;"),
        uiOutput("map_var_ui")
      ),
      tagList(
        div(style = "display:flex; align-items:center; justify-content:center; gap:12px; padding:6px 0 10px 0;",
          actionButton("map_prev_year", HTML("&#9664;"),
                       class = "btn btn-primary btn-sm",
                       style = "padding:4px 10px; font-size:1rem;"),
          radioButtons("map_year", NULL,
                       choices  = as.character(seq(2020, 2050, 5)),
                       selected = "2020",
                       inline   = TRUE),
          actionButton("map_next_year", HTML("&#9654;"),
                       class = "btn btn-primary btn-sm",
                       style = "padding:4px 10px; font-size:1rem;")
        ),
        uiOutput("maps_ui")
      )
    )
  ),

  nav_item(
    tags$div(
      id = "fcid-logo-wrap",
      tags$img(src = "images/fcidlogo.png", id = "fcid-logo")
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Scenario switches, synced across every tab ──────────────────────────────
  # Each tab renders its own copy of the scenario switches (scenario_switches_ui
  # is called once per tab, giving each a distinct input id like
  # "lu_scen_UP50...Current.Trends" / "emiss_scen_UP50...Current.Trends" / ...).
  # Toggling one on any tab should reflect everywhere else, so a single shared
  # reactiveValues (keyed by scenario label) acts as the source of truth: every
  # tab's switch pushes its changes into it, and it pushes back out to every
  # tab's switch whenever it changes. The `identical()` guards on both sides
  # stop that round-trip from looping forever.
  #
  # `force(s)` (and `force(prefix)`) below are load-bearing, not defensive
  # style: `observeEvent()`'s handler only runs later, on a deferred reactive
  # flush — by then the `for` loop that called this helper has long since
  # finished, so an unforced `s` parameter is still an unevaluated promise
  # pointing at the *loop variable itself*, which every call shares. Forcing
  # it up front freezes each call's own copy before the loop can move on.
  SCENARIO_TAB_PREFIXES <- c("lu", "luc", "emiss", "crop", "live", "trade", "food")
  scenario_state <- reactiveValues()
  for (s in available_scenarios) scenario_state[[s]] <- s %in% default_scenarios

  sync_switch_to_state <- function(prefix, s) {
    force(prefix); force(s)
    input_id <- paste0(prefix, "_scen_", make.names(s))
    observeEvent(input[[input_id]], {
      if (!identical(scenario_state[[s]], input[[input_id]]))
        scenario_state[[s]] <- input[[input_id]]
    }, ignoreInit = TRUE)
  }
  for (prefix in SCENARIO_TAB_PREFIXES) for (s in available_scenarios) sync_switch_to_state(prefix, s)

  sync_state_to_switches <- function(s) {
    force(s)
    observeEvent(scenario_state[[s]], {
      for (prefix in SCENARIO_TAB_PREFIXES) {
        input_id <- paste0(prefix, "_scen_", make.names(s))
        if (!identical(input[[input_id]], scenario_state[[s]]))
          update_switch(input_id, value = scenario_state[[s]], session = session)
      }
    }, ignoreInit = TRUE)
  }
  for (s in available_scenarios) sync_state_to_switches(s)

  x_max <- reactive({
    if (isTRUE(input$years_sel)) 2020L else 2050L
  })

  y_range <- reactive({
    calc_y_range(input$class_sel, x_max(), input$zero_base)
  })

  lu_scen_sel <- reactive(get_selected_scenarios_r(input, "lu"))

  output$charts_ui <- renderUI({
    y_label <- landuse_map[[input$class_sel]]$y_label

    right_col <- if (isTRUE(input$years_sel)) {
      tagList(
        div(style = "display:flex; align-items:center; gap:8px;",
            strong(y_label),
            downloadButton("landuse_dl", "CSV",
                           style = "padding:2px 8px; font-size:11px; height:24px; line-height:20px;")),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("data_table")),
        strong("Absolute Difference (Mha)"),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("abs_diff_table")),
        strong("Relative Difference (%)"),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("rel_diff_table"))
      )
    } else {
      tagList(
        div(style = "display:flex; align-items:center; gap:8px;",
            strong(y_label),
            downloadButton("landuse_dl", "CSV",
                           style = "padding:2px 8px; font-size:11px; height:24px; line-height:20px;")),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("data_table"))
      )
    }

    req(length(lu_scen_sel()) > 0)
    chart_out <- plotlyOutput("plot_main", height = "460px")
    if (x_max() == 2050) {
      tagList(
        fluidRow(column(6, chart_out)),
        fluidRow(column(12, right_col))
      )
    } else {
      fluidRow(column(6, chart_out), column(6, right_col))
    }
  })

  output$data_table <- renderTable(
    make_table_data(input$class_sel, lu_scen_sel(), x_max()),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )

  output$abs_diff_table <- renderTable(
    make_diff_data(input$class_sel, lu_scen_sel(), "absolute"),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )

  output$rel_diff_table <- renderTable(
    make_rel_diff_colored(input$class_sel, lu_scen_sel()),
    sanitize.text.function = identity,
    na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$landuse_dl <- downloadHandler(
    filename = function() {
      nm <- gsub(" ", "_", input$class_sel)
      sc <- if (length(lu_scen_sel()) == length(available_scenarios)) "All"
            else paste(gsub(" ", "_", lu_scen_sel()), collapse = "_")
      paste0("landuse_", nm, "_", sc, "_", x_max(), ".csv")
    },
    content = function(file) {
      write.csv(make_table_data(input$class_sel, lu_scen_sel(), x_max()),
                file, row.names = FALSE)
    }
  )

  output$plot_main <- renderPlotly({
    req(length(lu_scen_sel()) > 0)
    make_plot(input$class_sel, lu_scen_sel(), x_max(), y_range(), input$chart_type)
  })

  # ── Crops tab ────────────────────────────────────────────────────────────────
  crop_x_max <- reactive({
    if (isTRUE(input$crop_years)) 2020L else 2050L
  })

  crop_y_range <- reactive({
    req(length(crops_map) > 0, input$crop_name %in% names(crops_map))
    calc_crop_y_range(input$crop_name, input$crop_type, crop_x_max(), input$crop_zero_base)
  })

  crop_scen_sel <- reactive(get_selected_scenarios_r(input, "crop"))

  output$crop_charts_ui <- renderUI({
    req(length(crops_map) > 0)
    y_label   <- crop_y_label[[input$crop_type]]
    abs_unit  <- crop_hover_unit[[input$crop_type]]
    abs_title <- paste0("Absolute Difference (", abs_unit, ")")

    right_col <- if (isTRUE(input$crop_years)) {
      tagList(
        div(style = "display:flex; align-items:center; gap:8px;",
            strong(y_label),
            downloadButton("crop_dl", "CSV",
                           style = "padding:2px 8px; font-size:11px; height:24px; line-height:20px;")),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("crop_data_table")),
        strong(abs_title),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("crop_abs_diff_table")),
        strong("Relative Difference (%)"),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("crop_rel_diff_table"))
      )
    } else {
      tagList(
        div(style = "display:flex; align-items:center; gap:8px;",
            strong(y_label),
            downloadButton("crop_dl", "CSV",
                           style = "padding:2px 8px; font-size:11px; height:24px; line-height:20px;")),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("crop_data_table"))
      )
    }

    req(length(crop_scen_sel()) > 0)
    chart_out <- plotlyOutput("crop_plot_main", height = "460px")
    if (crop_x_max() == 2050) {
      tagList(
        fluidRow(column(6, chart_out)),
        fluidRow(column(12, right_col))
      )
    } else {
      fluidRow(column(6, chart_out), column(6, right_col))
    }
  })

  output$crop_data_table <- renderTable(
    make_crop_table_data(input$crop_name, input$crop_type, crop_scen_sel(), crop_x_max()),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$crop_abs_diff_table <- renderTable(
    make_crop_diff_data(input$crop_name, input$crop_type, crop_scen_sel(), "absolute"),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$crop_rel_diff_table <- renderTable(
    make_crop_rel_diff_colored(input$crop_name, input$crop_type, crop_scen_sel()),
    sanitize.text.function = identity,
    na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$crop_dl <- downloadHandler(
    filename = function() {
      sc <- if (length(crop_scen_sel()) == length(available_scenarios)) "All"
            else paste(gsub(" ", "_", crop_scen_sel()), collapse = "_")
      paste0("crops_", gsub(" ", "_", input$crop_name), "_",
             input$crop_type, "_", sc, "_", crop_x_max(), ".csv")
    },
    content = function(file) {
      write.csv(make_crop_table_data(input$crop_name, input$crop_type,
                                     crop_scen_sel(), crop_x_max()),
                file, row.names = FALSE)
    }
  )
  output$crop_plot_main <- renderPlotly({
    req(length(crop_scen_sel()) > 0)
    make_crop_plot(input$crop_name, input$crop_type, crop_scen_sel(), crop_x_max(), crop_y_range(), input$crop_chart_type)
  })

  # ── Livestock tab ─────────────────────────────────────────────────────────────
  live_x_max <- reactive({
    if (isTRUE(input$live_years)) 2020L else 2050L
  })

  live_y_range <- reactive({
    req(length(livestock_map) > 0, input$live_product %in% names(livestock_map))
    calc_live_y_range(input$live_product, live_x_max(), input$live_zero_base)
  })

  live_scen_sel <- reactive(get_selected_scenarios_r(input, "live"))

  output$live_charts_ui <- renderUI({
    req(length(livestock_map) > 0)
    has_hist <- livestock_map[[input$live_product]]$has_hist

    y_lbl     <- livestock_map[[input$live_product]]$y_label
    unit_lbl  <- livestock_map[[input$live_product]]$unit_label
    right_col <- if (isTRUE(input$live_years) && has_hist) {
      tagList(
        div(style = "display:flex; align-items:center; gap:8px;",
            strong(y_lbl),
            downloadButton("live_dl", "CSV",
                           style = "padding:2px 8px; font-size:11px; height:24px; line-height:20px;")),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("live_data_table")),
        strong(paste0("Absolute Difference (", unit_lbl, ")")),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("live_abs_diff_table")),
        strong("Relative Difference (%)"),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("live_rel_diff_table"))
      )
    } else {
      tagList(
        div(style = "display:flex; align-items:center; gap:8px;",
            strong(y_lbl),
            downloadButton("live_dl", "CSV",
                           style = "padding:2px 8px; font-size:11px; height:24px; line-height:20px;")),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("live_data_table"))
      )
    }

    req(length(live_scen_sel()) > 0)
    chart_out <- plotlyOutput("live_plot_main", height = "460px")
    if (live_x_max() == 2050) {
      tagList(
        fluidRow(column(6, chart_out)),
        fluidRow(column(12, right_col))
      )
    } else {
      fluidRow(column(6, chart_out), column(6, right_col))
    }
  })

  output$live_data_table <- renderTable(
    make_live_table_data(input$live_product, live_scen_sel(), live_x_max()),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$live_abs_diff_table <- renderTable(
    make_live_diff_data(input$live_product, live_scen_sel(), "absolute"),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$live_rel_diff_table <- renderTable(
    make_live_rel_diff_colored(input$live_product, live_scen_sel()),
    sanitize.text.function = identity,
    na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$live_dl <- downloadHandler(
    filename = function() {
      sc <- if (length(live_scen_sel()) == length(available_scenarios)) "All"
            else paste(gsub(" ", "_", live_scen_sel()), collapse = "_")
      paste0("livestock_", gsub(" ", "_", input$live_product), "_", sc, "_", live_x_max(), ".csv")
    },
    content = function(file) {
      write.csv(make_live_table_data(input$live_product, live_scen_sel(), live_x_max()),
                file, row.names = FALSE)
    }
  )
  output$live_plot_main <- renderPlotly({
    req(length(live_scen_sel()) > 0)
    make_live_plot(input$live_product, live_scen_sel(), live_x_max(), live_y_range(), input$live_chart_type)
  })

  # ── Trade tab ─────────────────────────────────────────────────────────────────
  observeEvent(input$trade_type, {
    choices <- trade_map[[input$trade_type]]$products
    updateSelectInput(session, "trade_product",
                      choices  = choices,
                      selected = choices[1])
  })

  trade_x_max <- reactive({
    if (isTRUE(input$trade_years)) 2020L else 2050L
  })

  trade_y_range <- reactive({
    req(input$trade_type %in% names(trade_map),
        input$trade_product %in% trade_map[[input$trade_type]]$products)
    calc_trade_y_range(input$trade_type, input$trade_product, trade_x_max(), input$trade_zero_base)
  })

  trade_scen_sel <- reactive(get_selected_scenarios_r(input, "trade"))

  output$trade_charts_ui <- renderUI({
    req(input$trade_type %in% names(trade_map),
        input$trade_product %in% trade_map[[input$trade_type]]$products)

    right_col <- if (isTRUE(input$trade_years)) {
      tagList(
        div(style = "display:flex; align-items:center; gap:8px;",
            strong(paste0(input$trade_type, " (Mt)")),
            downloadButton("trade_dl", "CSV",
                           style = "padding:2px 8px; font-size:11px; height:24px; line-height:20px;")),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("trade_data_table")),
        strong("Absolute Difference (Mt)"),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("trade_abs_diff_table")),
        strong("Relative Difference (%)"),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("trade_rel_diff_table"))
      )
    } else {
      tagList(
        div(style = "display:flex; align-items:center; gap:8px;",
            strong(paste0(input$trade_type, " (Mt)")),
            downloadButton("trade_dl", "CSV",
                           style = "padding:2px 8px; font-size:11px; height:24px; line-height:20px;")),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("trade_data_table"))
      )
    }

    req(length(trade_scen_sel()) > 0)
    chart_out <- plotlyOutput("trade_plot_main", height = "460px")
    if (trade_x_max() == 2050) {
      tagList(
        fluidRow(column(6, chart_out)),
        fluidRow(column(12, right_col))
      )
    } else {
      fluidRow(column(6, chart_out), column(6, right_col))
    }
  })

  output$trade_data_table <- renderTable(
    make_trade_table_data(input$trade_type, input$trade_product,
                          trade_scen_sel(), trade_x_max()),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$trade_abs_diff_table <- renderTable(
    make_trade_diff_data(input$trade_type, input$trade_product, trade_scen_sel(), "absolute"),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$trade_rel_diff_table <- renderTable(
    make_trade_rel_diff_colored(input$trade_type, input$trade_product, trade_scen_sel()),
    sanitize.text.function = identity,
    na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$trade_dl <- downloadHandler(
    filename = function() {
      prod <- gsub("[() ]", "_", input$trade_product)
      sc <- if (length(trade_scen_sel()) == length(available_scenarios)) "All"
            else paste(gsub(" ", "_", trade_scen_sel()), collapse = "_")
      paste0("trade_", input$trade_type, "_", prod, "_", sc, "_", trade_x_max(), ".csv")
    },
    content = function(file) {
      write.csv(make_trade_table_data(input$trade_type, input$trade_product,
                                      trade_scen_sel(), trade_x_max()),
                file, row.names = FALSE)
    }
  )
  output$trade_plot_main <- renderPlotly({
    req(length(trade_scen_sel()) > 0)
    make_trade_plot(input$trade_type, input$trade_product,
                    trade_scen_sel(), trade_x_max(), trade_y_range(), input$trade_chart_type)
  })

  # ── Food tab ──────────────────────────────────────────────────────────────────
  food_x_max <- reactive({
    if (isTRUE(input$food_years)) 2020L else 2050L
  })

  food_y_range <- reactive({
    req(input$food_variable %in% names(food_map))
    calc_food_y_range(input$food_variable, food_x_max(), input$food_zero_base)
  })

  food_scen_sel <- reactive(get_selected_scenarios_r(input, "food"))

  output$food_charts_ui <- renderUI({
    req(input$food_variable %in% names(food_map))
    y_label <- food_map[[input$food_variable]]$y_label

    right_col <- tagList(
      div(style = "display:flex; align-items:center; gap:8px;",
          strong(y_label),
          downloadButton("food_dl", "CSV",
                         style = "padding:2px 8px; font-size:11px; height:24px; line-height:20px;")),
      div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
          tableOutput("food_data_table"))
    )

    req(length(food_scen_sel()) > 0)
    chart_out <- plotlyOutput("food_plot_main", height = "460px")
    if (food_x_max() == 2050) {
      tagList(
        fluidRow(column(6, chart_out)),
        fluidRow(column(12, right_col))
      )
    } else {
      fluidRow(column(6, chart_out), column(6, right_col))
    }
  })

  output$food_data_table <- renderTable(
    make_food_table_data(input$food_variable, food_scen_sel(), food_x_max()),
    digits = 0, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$food_dl <- downloadHandler(
    filename = function() {
      sc <- if (length(food_scen_sel()) == length(available_scenarios)) "All"
            else paste(gsub(" ", "_", food_scen_sel()), collapse = "_")
      paste0("food_", gsub(" ", "_", input$food_variable), "_", sc, "_", food_x_max(), ".csv")
    },
    content = function(file) {
      write.csv(make_food_table_data(input$food_variable, food_scen_sel(), food_x_max()),
                file, row.names = FALSE)
    }
  )
  output$food_plot_main <- renderPlotly({
    req(length(food_scen_sel()) > 0)
    make_food_plot(input$food_variable, food_scen_sel(), food_x_max(), food_y_range(), input$food_chart_type)
  })

  # ── Emissions tab ─────────────────────────────────────────────────────────────
  emiss_x_max <- reactive({
    if (isTRUE(input$emiss_years)) 2020L else 2050L
  })

  emiss_y_range <- reactive({
    req(length(emissions_map) > 0, input$emiss_sel %in% names(emissions_map))
    calc_emiss_y_range(input$emiss_sel, emiss_x_max(), input$emiss_zero_base)
  })

  emiss_scen_sel <- reactive(get_selected_scenarios_r(input, "emiss"))

  observeEvent(input$emiss_sel, {
    updateCheckboxInput(session, "emiss_zero_base",
                        value = input$emiss_sel != "CO₂ AFOLU")
  })

  output$emiss_charts_ui <- renderUI({
    req(length(emissions_map) > 0)
    y_label <- emissions_map[[input$emiss_sel]]$y_label

    right_col <- if (isTRUE(input$emiss_years)) {
      tagList(
        div(style = "display:flex; align-items:center; gap:8px;",
            strong(y_label),
            downloadButton("emiss_dl", "CSV",
                           style = "padding:2px 8px; font-size:11px; height:24px; line-height:20px;")),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("emiss_data_table")),
        strong("Absolute Difference (MtCO₂e)"),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("emiss_abs_diff_table")),
        strong("Relative Difference (%)"),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("emiss_rel_diff_table"))
      )
    } else {
      tagList(
        div(style = "display:flex; align-items:center; gap:8px;",
            strong(y_label),
            downloadButton("emiss_dl", "CSV",
                           style = "padding:2px 8px; font-size:11px; height:24px; line-height:20px;")),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("emiss_data_table"))
      )
    }

    req(length(emiss_scen_sel()) > 0)
    chart_out <- plotlyOutput("emiss_plot_main", height = "460px")
    if (emiss_x_max() == 2050) {
      tagList(
        fluidRow(column(6, chart_out)),
        fluidRow(column(12, right_col))
      )
    } else {
      fluidRow(column(6, chart_out), column(6, right_col))
    }
  })

  output$emiss_data_table <- renderTable(
    make_emiss_table_data(input$emiss_sel, emiss_scen_sel(), emiss_x_max()),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$emiss_abs_diff_table <- renderTable(
    make_emiss_diff_data(input$emiss_sel, emiss_scen_sel(), "absolute"),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$emiss_rel_diff_table <- renderTable(
    make_emiss_rel_diff_colored(input$emiss_sel, emiss_scen_sel()),
    sanitize.text.function = identity,
    na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$emiss_dl <- downloadHandler(
    filename = function() {
      nm <- gsub(" ", "_", input$emiss_sel)
      sc <- if (length(emiss_scen_sel()) == length(available_scenarios)) "All"
            else paste(gsub(" ", "_", emiss_scen_sel()), collapse = "_")
      paste0("emissions_", nm, "_", sc, "_", emiss_x_max(), ".csv")
    },
    content = function(file) {
      write.csv(make_emiss_table_data(input$emiss_sel, emiss_scen_sel(), emiss_x_max()),
                file, row.names = FALSE)
    }
  )
  output$emiss_plot_main <- renderPlotly({
    req(length(emiss_scen_sel()) > 0)
    make_emiss_plot(input$emiss_sel, emiss_scen_sel(), emiss_x_max(), emiss_y_range(), input$emiss_chart_type)
  })

  # ── Maps tab ──────────────────────────────────────────────────────────────────

  map_years_vec <- as.character(seq(2020, 2050, 5))

  observeEvent(input$map_prev_year, {
    idx <- which(map_years_vec == input$map_year)
    if (idx > 1) updateRadioButtons(session, "map_year", selected = map_years_vec[idx - 1])
  })

  observeEvent(input$map_next_year, {
    idx <- which(map_years_vec == input$map_year)
    if (idx < length(map_years_vec))
      updateRadioButtons(session, "map_year", selected = map_years_vec[idx + 1])
  })

  output$map_var_ui <- renderUI({
    if (input$map_type == "Transitions") {
      radioButtons("map_transition", "Transition",
                   choices  = c("Forest → Cropland", "Forest → Pasture",
                                "Cropland → Forest", "Pasture → Cropland",
                                "OtherLand → Cropland"),
                   selected = "Forest → Cropland")
    } else {
      radioButtons("map_class", "Class",
                   choices  = c("Forest", "Cropland", "Pasture", "OtherLand", "Urban"),
                   selected = "Forest")
    }
  })

  # Maps' own scenario switches are deliberately NOT part of the shared
  # cross-tab scenario_state/SCENARIO_TAB_PREFIXES sync — this tab's static
  # PNG pipeline structurally assumes at most a left/right pair + a diff, a
  # constraint no other tab has, so letting another tab's "check a 3rd
  # scenario" leak in here would violate it. Cap enforced below: if a 3rd
  # switch is turned on, it's immediately reverted and the user is notified.
  MAPS_MAX_SELECTED <- 2
  for (s in maps_available_scenarios) {
    local({
      scen     <- s
      input_id <- paste0("maps_scen_", make.names(scen))
      observeEvent(input[[input_id]], {
        if (isTRUE(input[[input_id]]) &&
            length(get_selected_scenarios_r(input, "maps", maps_available_scenarios)) > MAPS_MAX_SELECTED) {
          update_switch(input_id, value = FALSE, session = session)
          showNotification("Only 2 scenarios can be compared at a time on the Maps tab.",
                           type = "warning", duration = 4)
        }
      }, ignoreInit = TRUE)
    })
  }

  maps_scen_sel <- reactive(get_selected_scenarios_r(input, "maps", maps_available_scenarios))

  # Maps out a scenario label to its static-PNG folder slug, e.g.
  # "UP50 - Current Trends" -> "UP50_ct" — matches 04_generate_maps.R's own
  # dir_out naming (derived the same way: up column + NDC-in-filename check).
  scenario_map_dir <- function(label) {
    row <- scenario_meta[scenario_meta$label == label, ]
    if (nrow(row) == 0) return(NA_character_)
    pathway <- if (grepl("NDC", row$file[1], ignore.case = TRUE)) "ndc" else "ct"
    sprintf("UP%d_%s", row$up[1], pathway)
  }
  scenario_up <- function(label) scenario_meta$up[match(label, scenario_meta$label)]

  # FABLE Calculator's own aggregate total (Mha) for a land-use class/year,
  # reusing the same landuse_map config + to_mha() the Land Use tab uses —
  # lets the user sanity-check the downscaled map's spatial total against the
  # Calculator's own number for that class. map_class uses "OtherLand" (no
  # space, matching the radioButtons choices) while landuse_map's key is
  # "Other Land" (with space, matching the Land Use tab's selectInput) — same
  # class, different casing convention in each tab, so normalize here.
  fable_landuse_total_mha <- function(scenario_label, map_class, year) {
    cfg <- landuse_map[[if (map_class == "OtherLand") "Other Land" else map_class]]
    if (is.null(cfg)) return(NA_real_)
    row <- df_scenarios[df_scenarios$scenario == scenario_label & df_scenarios$Year == as.integer(year), ]
    if (nrow(row) == 0) return(NA_real_)
    to_mha(as.numeric(row[[cfg$fable_col]][1]), cfg$fable_col, cfg$fable_unit)
  }

  # Only appends the FABLE Calculator total for Land Cover (Outflows/
  # Transitions have no single matching aggregate column to compare against).
  maps_tile_label <- function(scenario_label, type_sel, map_class, year) {
    if (type_sel != "Land Cover") return(scenario_label)
    total <- fable_landuse_total_mha(scenario_label, map_class, year)
    if (is.na(total)) return(scenario_label)
    sprintf("%s (FABLE-C: %.2f Mha)", scenario_label, total)
  }

  # Whether this scenario's downscaled LUC source file exists at all — not
  # every scenario will ever get one (e.g. some UPs won't have NDC downscaled
  # data), so this is checked separately from "the PNG for this specific
  # class/year just hasn't been rendered yet" to give a more honest message
  # ("data not available" vs. the actionable "run 04_generate_maps.R").
  # Matched case-insensitively (same as 04_generate_maps.R's own
  # find_downscaled_rds()) since the provided .rds files have used both
  # "ct"/"ndc" and "CT"/"NDC" naming at different times.
  scenario_has_downscaled_data <- function(label) {
    length(list.files("data/luc",
                      pattern = sprintf("^downscaled_LUC_%s\\.rds$", scenario_map_dir(label)),
                      ignore.case = TRUE)) > 0
  }

  map_placeholder <- function(icon_html, title_text, subtitle) {
    div(style = paste("display:flex; flex-direction:column; align-items:center;",
                      "justify-content:center;",
                      "aspect-ratio:820/780; width:100%; max-height:calc(100vh - 210px);",
                      "border:1px solid #ddd; border-radius:4px;",
                      "background:white; color:#555;",
                      "font-size:0.9rem; text-align:center; padding:1rem;"),
        tags$div(style = "font-size:2.2rem; margin-bottom:0.4rem;", HTML(icon_html)),
        tags$p(style = "margin:0; font-weight:600;", title_text),
        tags$p(style = "margin:0.25rem 0 0 0; font-size:0.8rem; color:#888;", subtitle))
  }

  make_map_img <- function(sc_dir, type_sel, var_sel, year, data_available = TRUE) {
    if (type_sel == "Land Cover") {
      rel_path <- sprintf("maps/%s/landcover/landcover_%s_%s.png", sc_dir, var_sel, year)
    } else if (type_sel == "Outflows") {
      rel_path <- sprintf("maps/%s/transitions/outflow_%s_%s.png",  sc_dir, var_sel, year)
    } else {
      label    <- sub(" → ", "_to_", var_sel)
      rel_path <- sprintf("maps/%s/transitions/transition_%s_%s.png", sc_dir, label, year)
    }
    disk_path    <- paste0("data/", rel_path)
    nodiff_path  <- sub("\\.png$", ".nodiff", disk_path)
    if (startsWith(sc_dir, "diff/") && file.exists(nodiff_path)) {
      map_placeholder("&#x2261;", "No difference",
                      "Both scenarios are identical for this variable and year.")
    } else if (file.exists(disk_path)) {
      tags$img(src   = rel_path,
               class = "maps-img",
               style = "border:1px solid #ddd; border-radius:4px;",
               alt   = paste(sc_dir, type_sel, var_sel, year))
    } else if (!data_available) {
      map_placeholder("&#x1F6AB;", "Map data not available",
                      "Downscaled data for this scenario doesn't exist yet.")
    } else {
      map_placeholder("", "Maps not generated yet.",
                      HTML(as.character(tags$code("Rscript 04_generate_maps.R"))))
    }
  }

  output$maps_ui <- renderUI({
    type_sel <- input$map_type
    year     <- input$map_year
    var_sel  <- if (type_sel == "Transitions") {
      req(input$map_transition)
      input$map_transition
    } else {
      req(input$map_class)
      input$map_class
    }

    sel <- maps_scen_sel()
    if (length(sel) == 0)
      return(map_placeholder("", "Select 1 or 2 scenarios", "Turn on a Scenario switch in the sidebar to see maps."))

    tile1 <- tagList(tags$strong(maps_tile_label(sel[1], type_sel, var_sel, year)),
                     make_map_img(scenario_map_dir(sel[1]), type_sel, var_sel, year,
                                 data_available = scenario_has_downscaled_data(sel[1])))
    tile2 <- if (length(sel) >= 2) {
      tagList(tags$strong(maps_tile_label(sel[2], type_sel, var_sel, year)),
             make_map_img(scenario_map_dir(sel[2]), type_sel, var_sel, year,
                         data_available = scenario_has_downscaled_data(sel[2])))
    } else {
      tagList(tags$strong(""), map_placeholder("", "Select a second scenario",
                                                "Turn on another Scenario switch to compare."))
    }
    diff_tile <- if (length(sel) >= 2) {
      up1 <- scenario_up(sel[1]); up2 <- scenario_up(sel[2])
      both_available <- scenario_has_downscaled_data(sel[1]) && scenario_has_downscaled_data(sel[2])
      if (!identical(up1, up2)) {
        tagList(tags$strong("Difference"),
                map_placeholder("&#x1F6A7;", "Not available yet",
                                "Difference maps between different UP calibrations aren't supported yet — planned for a future update."))
      } else if (!both_available) {
        tagList(tags$strong("Difference"),
                map_placeholder("&#x1F6AB;", "Map data not available",
                                "Downscaled data is missing for one or both selected scenarios."))
      } else {
        tagList(tags$strong("Difference"), make_map_img(sprintf("diff/UP%d", up1), type_sel, var_sel, year))
      }
    } else {
      tagList(tags$strong("Difference"),
              map_placeholder("", "Select 2 scenarios", "A difference map needs exactly 2 scenarios selected."))
    }

    fluidRow(
      column(4, tile1),
      column(4, tile2),
      column(4, diff_tile)
    )
  })

  make_luc_server(input, output, session)
}

shinyApp(ui, server)

