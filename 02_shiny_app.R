library(shiny)
library(dplyr)
library(plotly)

# Run 01_process_data.R first if any processed file is missing
if (!file.exists("data/processed/df_scenarios.rds") ||
    !file.exists("data/processed/fable_units.rds")  ||
    !file.exists("data/processed/df_crops.rds")) {
  source("01_process_data.R")
}

df_scenarios <- readRDS("data/processed/df_scenarios.rds")
df_hist      <- readRDS("data/processed/df_hist.rds")
fable_units  <- readRDS("data/processed/fable_units.rds")
df_crops     <- readRDS("data/processed/df_crops.rds")

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
base_layout <- function(p, title_text, title_color = "black", x_max, y_range,
                        y_label = "Area (Mha)", barmode = NULL) {
  is_bar  <- !is.null(barmode)
  x_range <- if (is_bar) c(1996, x_max + 4) else c(1999, x_max + 1)

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

  p %>% plotly::layout(
    title = list(text = paste0("<b>", title_text, "</b>"),
                 font = list(color = title_color, size = 15)),
    xaxis = list(
      title     = "",
      tickvals  = seq(2000, x_max, 5),
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
  } else {
    add_trace(p, data = hist_data, x = ~year, y = ~value,
              type = "scatter", mode = "lines+markers",
              name = trace_name,
              line   = list(color = COL_HIST, width = 2),
              marker = list(color = COL_HIST, size = 7),
              hovertemplate = hover)
  }
}

make_combined_plot <- function(class_name, x_max, y_range, chart_type = "Line chart") {
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

  if (chart_type == "Bar chart") {
    p <- plot_ly() %>%
      add_trace(data = ct, x = ~year, y = ~value,
                type = "bar", name = "Current Trends",
                marker = list(color = COL_CT,
                              line  = list(color = "black", width = 1)),
                hovertemplate = "%{x}: <b>%{y:.2f} Mha</b><extra>Current Trends</extra>") %>%
      add_trace(data = ndc, x = ~year, y = ~value,
                type = "bar", name = "NDC Commitments",
                marker = list(color = COL_NDC,
                              line  = list(color = "black", width = 1)),
                hovertemplate = "%{x}: <b>%{y:.2f} Mha</b><extra>NDC Commitments</extra>") %>%
      add_hist_trace(hist_data, source_label, chart_type)
  } else {
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
      add_hist_trace(hist_data, source_label, chart_type)
  }

  base_layout(p, paste0(class_name, ": Current Trends vs NDC Commitments"),
              x_max = x_max, y_range = y_range,
              y_label = landuse_map[[class_name]]$y_label,
              barmode = if (chart_type == "Bar chart") "group" else NULL)
}

make_single_plot <- function(class_name, scenario_name, bar_color, x_max, y_range,
                             chart_type = "Line chart") {
  col  <- landuse_map[[class_name]]$fable_col

  scen <- df_scenarios %>%
    filter(scenario == scenario_name, Year <= x_max) %>%
    select(year = Year, value = all_of(col)) %>%
    mutate(value = to_mha(as.numeric(value), col, landuse_map[[class_name]]$fable_unit))

  hist_data    <- get_hist(class_name, x_max)
  source_label <- landuse_map[[class_name]]$hist_source
  hover        <- paste0("%{x}: <b>%{y:.2f} Mha</b><extra>", scenario_name, "</extra>")

  if (chart_type == "Bar chart") {
    p <- plot_ly() %>%
      add_trace(data = scen, x = ~year, y = ~value,
                type = "bar", name = scenario_name,
                marker = list(color = bar_color,
                              line  = list(color = "black", width = 1)),
                hovertemplate = hover) %>%
      add_hist_trace(hist_data, source_label, chart_type)
  } else {
    p <- plot_ly() %>%
      add_trace(data = scen, x = ~year, y = ~value,
                type = "scatter", mode = "lines+markers",
                name = scenario_name,
                line   = list(color = bar_color, width = 2),
                marker = list(color = bar_color, size = 7),
                hovertemplate = hover) %>%
      add_hist_trace(hist_data, source_label, chart_type)
  }

  base_layout(p, paste0(class_name, ": ", scenario_name),
              bar_color, x_max = x_max, y_range = y_range,
              y_label = landuse_map[[class_name]]$y_label,
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

  if (scenario_sel %in% c("Both", "Current Trends"))
    rows[["Current Trends"]] <- pull_scenario("Current Trends")

  if (scenario_sel %in% c("Both", "NDC Commitments"))
    rows[["NDC Commitments"]] <- pull_scenario("NDC Commitments")

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
  if (scenario_sel %in% c("Both", "Current Trends")) {
    v <- pull_scenario("Current Trends")
    rows[["Current Trends"]] <- if (type == "absolute") abs(v - hist_vals)
                                else (v - hist_vals) / hist_vals * 100
  }
  if (scenario_sel %in% c("Both", "NDC Commitments")) {
    v <- pull_scenario("NDC Commitments")
    rows[["NDC Commitments"]] <- if (type == "absolute") abs(v - hist_vals)
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

calc_crop_y_range <- function(crop_name, type_sel, x_max) {
  scen_vals <- bind_rows(
    get_crop_fable(crop_name, type_sel, "Current Trends",  x_max),
    get_crop_fable(crop_name, type_sel, "NDC Commitments", x_max)
  )$value
  hist_vals <- get_crop_hist_data(crop_name, type_sel, x_max)$value
  all_vals  <- c(scen_vals, hist_vals)
  pad <- diff(range(all_vals, na.rm = TRUE)) * 0.05
  c(0, max(all_vals, na.rm = TRUE) + pad)
}

# ── Crop plot builders ────────────────────────────────────────────────────────
make_crop_combined_plot <- function(crop_name, type_sel, x_max, y_range, chart_type) {
  y_label    <- crop_y_label[[type_sel]]
  unit_label <- crop_hover_unit[[type_sel]]
  hover      <- function(nm) paste0("%{x}: <b>%{y:.2f} ", unit_label, "</b><extra>", nm, "</extra>")

  ct        <- get_crop_fable(crop_name, type_sel, "Current Trends",  x_max)
  ndc       <- get_crop_fable(crop_name, type_sel, "NDC Commitments", x_max)
  hist_data <- get_crop_hist_data(crop_name, type_sel, x_max)

  if (chart_type == "Bar chart") {
    p <- plot_ly() %>%
      add_trace(data = ct,  x = ~year, y = ~value, type = "bar", name = "Current Trends",
                marker = list(color = COL_CT,  line = list(color = "black", width = 1)),
                hovertemplate = hover("Current Trends")) %>%
      add_trace(data = ndc, x = ~year, y = ~value, type = "bar", name = "NDC Commitments",
                marker = list(color = COL_NDC, line = list(color = "black", width = 1)),
                hovertemplate = hover("NDC Commitments"))
  } else {
    p <- plot_ly() %>%
      add_trace(data = ct,  x = ~year, y = ~value, type = "scatter", mode = "lines+markers",
                name = "Current Trends",
                line = list(color = COL_CT, width = 2), marker = list(color = COL_CT, size = 7),
                hovertemplate = hover("Current Trends")) %>%
      add_trace(data = ndc, x = ~year, y = ~value, type = "scatter", mode = "lines+markers",
                name = "NDC Commitments",
                line = list(color = COL_NDC, width = 2), marker = list(color = COL_NDC, size = 7),
                hovertemplate = hover("NDC Commitments"))
  }
  p <- p %>% add_hist_trace(hist_data, "IBGE", chart_type, unit_label)
  base_layout(p, paste0(crop_name, ": Current Trends vs NDC Commitments"),
              x_max = x_max, y_range = y_range, y_label = y_label,
              barmode = if (chart_type == "Bar chart") "group" else NULL)
}

make_crop_single_plot <- function(crop_name, type_sel, scenario_name, bar_color,
                                  x_max, y_range, chart_type) {
  y_label    <- crop_y_label[[type_sel]]
  unit_label <- crop_hover_unit[[type_sel]]
  hover_tmpl <- paste0("%{x}: <b>%{y:.2f} ", unit_label, "</b><extra>", scenario_name, "</extra>")

  scen      <- get_crop_fable(crop_name, type_sel, scenario_name, x_max)
  hist_data <- get_crop_hist_data(crop_name, type_sel, x_max)

  if (chart_type == "Bar chart") {
    p <- plot_ly() %>%
      add_trace(data = scen, x = ~year, y = ~value, type = "bar", name = scenario_name,
                marker = list(color = bar_color, line = list(color = "black", width = 1)),
                hovertemplate = hover_tmpl)
  } else {
    p <- plot_ly() %>%
      add_trace(data = scen, x = ~year, y = ~value, type = "scatter", mode = "lines+markers",
                name = scenario_name,
                line = list(color = bar_color, width = 2), marker = list(color = bar_color, size = 7),
                hovertemplate = hover_tmpl)
  }
  p <- p %>% add_hist_trace(hist_data, "IBGE", chart_type, unit_label)
  base_layout(p, paste0(crop_name, ": ", scenario_name),
              bar_color, x_max = x_max, y_range = y_range, y_label = y_label,
              barmode = if (chart_type == "Bar chart") "group" else NULL)
}

# ── Crop table builders ───────────────────────────────────────────────────────
make_crop_table_data <- function(crop_name, type_sel, scenario_sel, x_max) {
  years <- seq(2000, x_max, 5)
  rows  <- list()

  pull_fable <- function(scen_name)
    get_crop_fable(crop_name, type_sel, scen_name, x_max) %>%
      filter(year %in% years) %>% arrange(year) %>% pull(value)

  if (scenario_sel %in% c("Both", "Current Trends"))
    rows[["Current Trends"]] <- pull_fable("Current Trends")
  if (scenario_sel %in% c("Both", "NDC Commitments"))
    rows[["NDC Commitments"]] <- pull_fable("NDC Commitments")

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
  if (scenario_sel %in% c("Both", "Current Trends")) {
    v <- pull_fable("Current Trends")
    rows[["Current Trends"]] <- if (type == "absolute") abs(v - hist_vals)
                                else (v - hist_vals) / hist_vals * 100
  }
  if (scenario_sel %in% c("Both", "NDC Commitments")) {
    v <- pull_fable("NDC Commitments")
    rows[["NDC Commitments"]] <- if (type == "absolute") abs(v - hist_vals)
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

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "FABLE Calculator — Brazil",

  tabPanel("Land-use",
    br(),
    fluidRow(
      column(3,
        selectInput("class_sel", "Landuse Class:",
                    choices  = names(landuse_map),
                    selected = "Cropland")
      ),
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
        selectInput("chart_type", "Chart type:",
                    choices  = c("Line chart", "Bar chart"),
                    selected = "Line chart")
      )
    ),
    uiOutput("charts_ui")
  ),

  tabPanel("Crops",
    br(),
    fluidRow(
      column(2,
        selectInput("crop_name", "Crop:",
                    choices  = c("Soybeans", "Corn", "Sugarcane"),
                    selected = "Soybeans")
      ),
      column(2,
        selectInput("crop_type", "Type:",
                    choices  = c("Area", "Production", "Yield"),
                    selected = "Area")
      ),
      column(2,
        selectInput("crop_scenario", "Scenario:",
                    choices  = c("Both", "Current Trends", "NDC Commitments"),
                    selected = "Both")
      ),
      column(3,
        selectInput("crop_years", "Years:",
                    choices  = c("Calibration & Projections", "Calibration"),
                    selected = "Calibration & Projections")
      ),
      column(2,
        selectInput("crop_chart_type", "Chart type:",
                    choices  = c("Line chart", "Bar chart"),
                    selected = "Line chart")
      )
    ),
    uiOutput("crop_charts_ui")
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
    y_label <- landuse_map[[input$class_sel]]$y_label

    right_col <- if (input$years_sel == "Calibration") {
      tagList(
        strong(y_label),
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
        strong(y_label),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("data_table"))
      )
    }

    switch(input$scenario_sel,
      "Both"            = fluidRow(
        column(6, plotlyOutput("plot_both", height = "460px")),
        column(6, right_col)
      ),
      "Current Trends"  = fluidRow(
        column(6, plotlyOutput("plot_ct",   height = "460px")),
        column(6, right_col)
      ),
      "NDC Commitments" = fluidRow(
        column(6, plotlyOutput("plot_ndc",  height = "460px")),
        column(6, right_col)
      )
    )
  })

  output$data_table <- renderTable(
    make_table_data(input$class_sel, input$scenario_sel, x_max()),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )

  output$abs_diff_table <- renderTable(
    make_diff_data(input$class_sel, input$scenario_sel, "absolute"),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )

  output$rel_diff_table <- renderTable(
    make_rel_diff_colored(input$class_sel, input$scenario_sel),
    sanitize.text.function = identity,
    na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )

  output$plot_both <- renderPlotly({
    make_combined_plot(input$class_sel, x_max(), y_range(), input$chart_type)
  })
  output$plot_ct <- renderPlotly({
    make_single_plot(input$class_sel, "Current Trends",  COL_CT,  x_max(), y_range(), input$chart_type)
  })
  output$plot_ndc <- renderPlotly({
    make_single_plot(input$class_sel, "NDC Commitments", COL_NDC, x_max(), y_range(), input$chart_type)
  })

  # ── Crops tab ────────────────────────────────────────────────────────────────
  crop_x_max <- reactive({
    if (input$crop_years == "Calibration") 2020L else 2050L
  })

  crop_y_range <- reactive({
    req(length(crops_map) > 0, input$crop_name %in% names(crops_map))
    calc_crop_y_range(input$crop_name, input$crop_type, crop_x_max())
  })

  output$crop_charts_ui <- renderUI({
    req(length(crops_map) > 0)
    y_label   <- crop_y_label[[input$crop_type]]
    abs_unit  <- crop_hover_unit[[input$crop_type]]
    abs_title <- paste0("Absolute Difference (", abs_unit, ")")

    right_col <- if (input$crop_years == "Calibration") {
      tagList(
        strong(y_label),
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
        strong(y_label),
        div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
            tableOutput("crop_data_table"))
      )
    }

    switch(input$crop_scenario,
      "Both"            = fluidRow(
        column(6, plotlyOutput("crop_plot_both", height = "460px")),
        column(6, right_col)
      ),
      "Current Trends"  = fluidRow(
        column(6, plotlyOutput("crop_plot_ct",   height = "460px")),
        column(6, right_col)
      ),
      "NDC Commitments" = fluidRow(
        column(6, plotlyOutput("crop_plot_ndc",  height = "460px")),
        column(6, right_col)
      )
    )
  })

  output$crop_data_table <- renderTable(
    make_crop_table_data(input$crop_name, input$crop_type, input$crop_scenario, crop_x_max()),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$crop_abs_diff_table <- renderTable(
    make_crop_diff_data(input$crop_name, input$crop_type, input$crop_scenario, "absolute"),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$crop_rel_diff_table <- renderTable(
    make_crop_rel_diff_colored(input$crop_name, input$crop_type, input$crop_scenario),
    sanitize.text.function = identity,
    na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$crop_plot_both <- renderPlotly({
    make_crop_combined_plot(input$crop_name, input$crop_type, crop_x_max(), crop_y_range(), input$crop_chart_type)
  })
  output$crop_plot_ct <- renderPlotly({
    make_crop_single_plot(input$crop_name, input$crop_type, "Current Trends",  COL_CT,  crop_x_max(), crop_y_range(), input$crop_chart_type)
  })
  output$crop_plot_ndc <- renderPlotly({
    make_crop_single_plot(input$crop_name, input$crop_type, "NDC Commitments", COL_NDC, crop_x_max(), crop_y_range(), input$crop_chart_type)
  })
}

shinyApp(ui, server)
