library(shiny)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(bslib))

# Run 01_process_data.R first if any processed file is missing
if (!file.exists("data/processed/df_scenarios.rds") ||
    !file.exists("data/processed/fable_units.rds")  ||
    !file.exists("data/processed/df_crops.rds")     ||
    !file.exists("data/processed/df_livestock.rds")) {
  source("01_process_data.R")
}

df_scenarios <- readRDS("data/processed/df_scenarios.rds")
df_hist      <- readRDS("data/processed/df_hist.rds")
fable_units  <- readRDS("data/processed/fable_units.rds")
df_crops     <- readRDS("data/processed/df_crops.rds")
df_livestock <- readRDS("data/processed/df_livestock.rds")

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
COL_HIST <- "#000000"
COL_CT   <- "#1565C0"
COL_NDC  <- "#009C3B"

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

  if ("Current Trends" %in% scenario_sel)
    rows[["Current Trends"]] <- pull_scenario("Current Trends")

  if ("NDC Commitments" %in% scenario_sel)
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
  if ("Current Trends" %in% scenario_sel) {
    v <- pull_scenario("Current Trends")
    rows[["Current Trends"]] <- if (type == "absolute") abs(v - hist_vals)
                                else (v - hist_vals) / hist_vals * 100
  }
  if ("NDC Commitments" %in% scenario_sel) {
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

calc_crop_y_range <- function(crop_name, type_sel, x_max, zero_base = TRUE) {
  scen_vals <- bind_rows(
    get_crop_fable(crop_name, type_sel, "Current Trends",  x_max),
    get_crop_fable(crop_name, type_sel, "NDC Commitments", x_max)
  )$value
  hist_vals <- get_crop_hist_data(crop_name, type_sel, x_max)$value
  all_vals  <- c(scen_vals, hist_vals)
  pad <- diff(range(all_vals, na.rm = TRUE)) * 0.05
  y_min <- if (zero_base && min(all_vals, na.rm = TRUE) >= 0) 0
            else min(all_vals, na.rm = TRUE) - pad
  c(y_min, max(all_vals, na.rm = TRUE) + pad)
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

  if ("Current Trends" %in% scenario_sel)
    rows[["Current Trends"]] <- pull_fable("Current Trends")
  if ("NDC Commitments" %in% scenario_sel)
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
  if ("Current Trends" %in% scenario_sel) {
    v <- pull_fable("Current Trends")
    rows[["Current Trends"]] <- if (type == "absolute") abs(v - hist_vals)
                                else (v - hist_vals) / hist_vals * 100
  }
  if ("NDC Commitments" %in% scenario_sel) {
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

# ── Livestock configuration ───────────────────────────────────────────────────
# Entries with fable_product come from df_crops (ProdQ_feas, 1000 t → Mt).
# Entries with fable_col come from df_livestock (5_feas_livestock sheet).
livestock_map <- list(
  "Beef Production"    = list(fable_product = "beef",    prod_unit = "1000 t",
                              hist_type = "Ruminant Meat", hist_source = "FAOSTAT", has_hist = TRUE,
                              y_label = "Production (Mt)", unit_label = "Mt"),
  "Milk Production"    = list(fable_product = "milk",    prod_unit = "1000 t", has_hist = FALSE,
                              y_label = "Production (Mt)", unit_label = "Mt"),
  "Chicken Production" = list(fable_product = "chicken", prod_unit = "1000 t", has_hist = FALSE,
                              y_label = "Production (Mt)", unit_label = "Mt"),
  "Pork Production"    = list(fable_product = "pork",    prod_unit = "1000 t", has_hist = FALSE,
                              y_label = "Production (Mt)", unit_label = "Mt"),
  "Cattle Herd" = list(fable_col = "FeasHerd", unit_divisor = 1000, has_hist = FALSE,
                        y_label = "Cattle Herd (Million TLU)", unit_label = "Million TLU"),
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
  scen_vals <- bind_rows(
    get_live_fable(product, "Current Trends",  x_max),
    get_live_fable(product, "NDC Commitments", x_max)
  )$value
  hist_vals <- get_live_hist(product, x_max)$value
  all_vals  <- c(scen_vals, hist_vals)
  pad <- diff(range(all_vals, na.rm = TRUE)) * 0.05
  y_min <- if (zero_base && min(all_vals, na.rm = TRUE) >= 0) 0
            else min(all_vals, na.rm = TRUE) - pad
  c(y_min, max(all_vals, na.rm = TRUE) + pad)
}

# ── Livestock plot builders ───────────────────────────────────────────────────
make_live_combined_plot <- function(product, x_max, y_range, chart_type) {
  cfg       <- livestock_map[[product]]
  unit_lbl  <- cfg$unit_label
  y_lbl     <- cfg$y_label
  hover     <- function(nm) paste0("%{x}: <b>%{y:.2f} ", unit_lbl, "</b><extra>", nm, "</extra>")
  ct        <- get_live_fable(product, "Current Trends",  x_max)
  ndc       <- get_live_fable(product, "NDC Commitments", x_max)
  hist_data <- get_live_hist(product, x_max)
  src_label <- if (cfg$has_hist) cfg$hist_source else ""

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
                line = list(color = COL_CT,  width = 2), marker = list(color = COL_CT,  size = 7),
                hovertemplate = hover("Current Trends")) %>%
      add_trace(data = ndc, x = ~year, y = ~value, type = "scatter", mode = "lines+markers",
                name = "NDC Commitments",
                line = list(color = COL_NDC, width = 2), marker = list(color = COL_NDC, size = 7),
                hovertemplate = hover("NDC Commitments"))
  }
  p <- p %>% add_hist_trace(hist_data, src_label, chart_type, unit_lbl)
  base_layout(p, paste0(product, ": Current Trends vs NDC Commitments"),
              x_max = x_max, y_range = y_range, y_label = y_lbl,
              barmode = if (chart_type == "Bar chart") "group" else NULL)
}

make_live_single_plot <- function(product, scenario_name, bar_color, x_max, y_range, chart_type) {
  cfg        <- livestock_map[[product]]
  unit_lbl   <- cfg$unit_label
  y_lbl      <- cfg$y_label
  hover_tmpl <- paste0("%{x}: <b>%{y:.2f} ", unit_lbl, "</b><extra>", scenario_name, "</extra>")
  scen      <- get_live_fable(product, scenario_name, x_max)
  hist_data <- get_live_hist(product, x_max)
  src_label <- if (cfg$has_hist) cfg$hist_source else ""

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
  p <- p %>% add_hist_trace(hist_data, src_label, chart_type, unit_lbl)
  base_layout(p, paste0(product, ": ", scenario_name),
              bar_color, x_max = x_max, y_range = y_range, y_label = y_lbl,
              barmode = if (chart_type == "Bar chart") "group" else NULL)
}

# ── Livestock table builders ──────────────────────────────────────────────────
make_live_table_data <- function(product, scenario_sel, x_max) {
  years <- seq(2000, x_max, 5)
  rows  <- list()

  pull_fable <- function(scen_name)
    get_live_fable(product, scen_name, x_max) %>%
      filter(year %in% years) %>% arrange(year) %>% pull(value)

  if ("Current Trends" %in% scenario_sel)
    rows[["Current Trends"]] <- pull_fable("Current Trends")
  if ("NDC Commitments" %in% scenario_sel)
    rows[["NDC Commitments"]] <- pull_fable("NDC Commitments")

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
  if ("Current Trends" %in% scenario_sel) {
    v <- pull_fable("Current Trends")
    rows[["Current Trends"]] <- if (type == "absolute") abs(v - hist_vals)
                                else (v - hist_vals) / hist_vals * 100
  }
  if ("NDC Commitments" %in% scenario_sel) {
    v <- pull_fable("NDC Commitments")
    rows[["NDC Commitments"]] <- if (type == "absolute") abs(v - hist_vals)
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
# No historical series — values table only, no diff tables.
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
    )
  ),
  "Imports" = list(
    fable_col = "Import_quantity",
    unit      = "1000 t",
    products  = c("Wheat"),
    fable_product = list("Wheat" = "wheat")
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

calc_trade_y_range <- function(trade_type, product, x_max, zero_base = TRUE) {
  all_vals <- bind_rows(
    get_trade_fable(trade_type, product, "Current Trends",  x_max),
    get_trade_fable(trade_type, product, "NDC Commitments", x_max)
  )$value
  pad <- diff(range(all_vals, na.rm = TRUE)) * 0.05
  y_min <- if (zero_base && min(all_vals, na.rm = TRUE) >= 0) 0
            else min(all_vals, na.rm = TRUE) - pad
  c(y_min, max(all_vals, na.rm = TRUE) + pad)
}

# ── Trade plot builders ───────────────────────────────────────────────────────
make_trade_combined_plot <- function(trade_type, product, x_max, y_range, chart_type) {
  hover <- function(nm) paste0("%{x}: <b>%{y:.2f} Mt</b><extra>", nm, "</extra>")
  ct    <- get_trade_fable(trade_type, product, "Current Trends",  x_max)
  ndc   <- get_trade_fable(trade_type, product, "NDC Commitments", x_max)

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
                line = list(color = COL_CT,  width = 2), marker = list(color = COL_CT,  size = 7),
                hovertemplate = hover("Current Trends")) %>%
      add_trace(data = ndc, x = ~year, y = ~value, type = "scatter", mode = "lines+markers",
                name = "NDC Commitments",
                line = list(color = COL_NDC, width = 2), marker = list(color = COL_NDC, size = 7),
                hovertemplate = hover("NDC Commitments"))
  }
  base_layout(p, paste0(product, " ", trade_type, ": Current Trends vs NDC Commitments"),
              x_max = x_max, y_range = y_range, y_label = paste0(trade_type, " (Mt)"),
              barmode = if (chart_type == "Bar chart") "group" else NULL)
}

make_trade_single_plot <- function(trade_type, product, scenario_name, bar_color,
                                   x_max, y_range, chart_type) {
  hover_tmpl <- paste0("%{x}: <b>%{y:.2f} Mt</b><extra>", scenario_name, "</extra>")
  scen <- get_trade_fable(trade_type, product, scenario_name, x_max)

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
  base_layout(p, paste0(product, " ", trade_type, ": ", scenario_name),
              bar_color, x_max = x_max, y_range = y_range,
              y_label = paste0(trade_type, " (Mt)"),
              barmode = if (chart_type == "Bar chart") "group" else NULL)
}

# ── Trade table builder ───────────────────────────────────────────────────────
make_trade_table_data <- function(trade_type, product, scenario_sel, x_max) {
  years <- seq(2000, x_max, 5)

  pull_fable <- function(scen_name)
    get_trade_fable(trade_type, product, scen_name, x_max) %>%
      filter(year %in% years) %>% arrange(year) %>% pull(value)

  rows <- list()
  if ("Current Trends" %in% scenario_sel)
    rows[["Current Trends"]] <- pull_fable("Current Trends")
  if ("NDC Commitments" %in% scenario_sel)
    rows[["NDC Commitments"]] <- pull_fable("NDC Commitments")

  mat <- do.call(rbind, rows)
  df  <- as.data.frame(mat)
  colnames(df) <- as.character(years)
  cbind(` ` = rownames(df), df, stringsAsFactors = FALSE)
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
  all_vals <- bind_rows(
    get_food_fable(variable, "Current Trends",  x_max),
    get_food_fable(variable, "NDC Commitments", x_max)
  )$value
  pad <- diff(range(all_vals, na.rm = TRUE)) * 0.05
  y_min <- if (zero_base && min(all_vals, na.rm = TRUE) >= 0) 0
            else min(all_vals, na.rm = TRUE) - pad
  c(y_min, max(all_vals, na.rm = TRUE) + pad)
}

# ── Food plot builders ────────────────────────────────────────────────────────
make_food_combined_plot <- function(variable, x_max, y_range, chart_type) {
  cfg   <- food_map[[variable]]
  hover <- function(nm) paste0("%{x}: <b>%{y:.0f} Intake (kcal/cap/day)</b><extra>", nm, "</extra>")
  ct    <- get_food_fable(variable, "Current Trends",  x_max)
  ndc   <- get_food_fable(variable, "NDC Commitments", x_max)

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
                line = list(color = COL_CT,  width = 2), marker = list(color = COL_CT,  size = 7),
                hovertemplate = hover("Current Trends")) %>%
      add_trace(data = ndc, x = ~year, y = ~value, type = "scatter", mode = "lines+markers",
                name = "NDC Commitments",
                line = list(color = COL_NDC, width = 2), marker = list(color = COL_NDC, size = 7),
                hovertemplate = hover("NDC Commitments"))
  }
  base_layout(p, paste0(variable, ": Current Trends vs NDC Commitments"),
              x_max = x_max, y_range = y_range, y_label = cfg$y_label,
              barmode = if (chart_type == "Bar chart") "group" else NULL)
}

make_food_single_plot <- function(variable, scenario_name, bar_color, x_max, y_range, chart_type) {
  cfg        <- food_map[[variable]]
  hover_tmpl <- paste0("%{x}: <b>%{y:.0f} Intake (kcal/cap/day)</b><extra>", scenario_name, "</extra>")
  scen       <- get_food_fable(variable, scenario_name, x_max)

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
  base_layout(p, paste0(variable, ": ", scenario_name),
              bar_color, x_max = x_max, y_range = y_range, y_label = cfg$y_label,
              barmode = if (chart_type == "Bar chart") "group" else NULL)
}

# ── Food table builder ────────────────────────────────────────────────────────
make_food_table_data <- function(variable, scenario_sel, x_max) {
  years <- seq(2000, x_max, 5)

  pull_fable <- function(scen_name)
    get_food_fable(variable, scen_name, x_max) %>%
      filter(year %in% years) %>% arrange(year) %>% pull(value)

  rows <- list()
  if ("Current Trends" %in% scenario_sel)
    rows[["Current Trends"]] <- pull_fable("Current Trends")
  if ("NDC Commitments" %in% scenario_sel)
    rows[["NDC Commitments"]] <- pull_fable("NDC Commitments")

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
  scen_vals <- bind_rows(
    get_emiss_fable(emiss_name, "Current Trends",  x_max),
    get_emiss_fable(emiss_name, "NDC Commitments", x_max)
  )$value
  hist_vals <- get_emiss_hist(emiss_name, x_max)$value
  all_vals  <- c(scen_vals, hist_vals)
  pad <- diff(range(all_vals, na.rm = TRUE)) * 0.05
  y_min <- min(all_vals, na.rm = TRUE)
  c(if (zero_base && y_min >= 0) 0 else y_min - pad, max(all_vals, na.rm = TRUE) + pad)
}

make_emiss_combined_plot <- function(emiss_name, x_max, y_range, chart_type) {
  cfg       <- emissions_map[[emiss_name]]
  ct        <- get_emiss_fable(emiss_name, "Current Trends",  x_max)
  ndc       <- get_emiss_fable(emiss_name, "NDC Commitments", x_max)
  hist_data <- get_emiss_hist(emiss_name, x_max)
  hover     <- function(nm) paste0("%{x}: <b>%{y:.2f} MtCO₂e</b><extra>", nm, "</extra>")

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
  p <- p %>% add_hist_trace(hist_data, "SEEG13", chart_type, "MtCO₂e")
  year_min <- if (!is.null(cfg$year_min)) cfg$year_min else 2000L
  base_layout(p, paste0(emiss_name, ": Current Trends vs NDC Commitments"),
              x_max = x_max, y_range = y_range, y_label = cfg$y_label,
              barmode = if (chart_type == "Bar chart") "group" else NULL,
              x_min = year_min,
              zero_line = emiss_name == "CO₂ AFOLU")
}

make_emiss_single_plot <- function(emiss_name, scenario_name, bar_color, x_max, y_range, chart_type) {
  cfg        <- emissions_map[[emiss_name]]
  scen       <- get_emiss_fable(emiss_name, scenario_name, x_max)
  hist_data  <- get_emiss_hist(emiss_name, x_max)
  hover_tmpl <- paste0("%{x}: <b>%{y:.2f} MtCO₂e</b><extra>", scenario_name, "</extra>")

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
  p <- p %>% add_hist_trace(hist_data, "SEEG13", chart_type, "MtCO₂e")
  year_min <- if (!is.null(cfg$year_min)) cfg$year_min else 2000L
  base_layout(p, paste0(emiss_name, ": ", scenario_name),
              bar_color, x_max = x_max, y_range = y_range, y_label = cfg$y_label,
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

  if ("Current Trends" %in% scenario_sel)
    rows[["Current Trends"]] <- pull_fable("Current Trends")
  if ("NDC Commitments" %in% scenario_sel)
    rows[["NDC Commitments"]] <- pull_fable("NDC Commitments")

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
  if ("Current Trends" %in% scenario_sel) {
    v <- pull_fable("Current Trends")
    rows[["Current Trends"]] <- if (type == "absolute") abs(v - hist_vals)
                                else (v - hist_vals) / hist_vals * 100
  }
  if ("NDC Commitments" %in% scenario_sel) {
    v <- pull_fable("NDC Commitments")
    rows[["NDC Commitments"]] <- if (type == "absolute") abs(v - hist_vals)
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

# ── Static assets ─────────────────────────────────────────────────────────────
addResourcePath("images", normalizePath("data/images", mustWork = FALSE))
addResourcePath("maps",   normalizePath("data/maps",   mustWork = FALSE))

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = HTML('<span style="display:inline-flex; align-items:center; gap:8px;"><img src="images/fable_logo.png" height="26" style="border-radius:4px;">FABLE-Calculator Brazil v50</span>'),
  window_title = "FABLE-Calculator Brazil v50",
  header = tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "images/favicon.svg"),
    tags$style(HTML("
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
        font-size: 0.8rem;
      }
      .bslib-sidebar-layout > .sidebar .control-label {
        font-weight: 600;
      }
      .selectize-input,
      .selectize-dropdown {
        font-size: 0.8rem !important;
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
    "))
  ),
  theme = bs_theme(primary = "#007B8A", version = 5),
  bg = "#007B8A",

  nav_panel(HTML("🗺️ Land Use"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput("class_sel", "Landuse Class",
                    choices  = names(landuse_map),
                    selected = "Forest"),
        checkboxGroupInput("scenario_sel", "Scenario",
                    choices  = c("Current Trends", "NDC Commitments"),
                    selected = c("Current Trends", "NDC Commitments")),
        radioButtons("years_sel", "Years",
                    choices  = c("Calibration & Projections", "Calibration"),
                    selected = "Calibration & Projections"),
        radioButtons("chart_type", "Chart type",
                    choices  = c("Line chart", "Bar chart"),
                    selected = "Line chart"),
        checkboxInput("zero_base", "Start y-axis at zero", value = TRUE)
      ),
      uiOutput("charts_ui")
    )
  ),

  nav_panel(HTML("🌫️ Emissions"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput("emiss_sel", "Emission",
                    choices  = names(emissions_map),
                    selected = "CO₂ AFOLU"),
        checkboxGroupInput("emiss_scenario", "Scenario",
                    choices  = c("Current Trends", "NDC Commitments"),
                    selected = c("Current Trends", "NDC Commitments")),
        radioButtons("emiss_years", "Years",
                    choices  = c("Calibration & Projections", "Calibration"),
                    selected = "Calibration & Projections"),
        radioButtons("emiss_chart_type", "Chart type",
                    choices  = c("Line chart", "Bar chart"),
                    selected = "Line chart"),
        checkboxInput("emiss_zero_base", "Start y-axis at zero", value = FALSE)
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
        checkboxGroupInput("crop_scenario", "Scenario",
                    choices  = c("Current Trends", "NDC Commitments"),
                    selected = c("Current Trends", "NDC Commitments")),
        radioButtons("crop_years", "Years",
                    choices  = c("Calibration & Projections", "Calibration"),
                    selected = "Calibration & Projections"),
        radioButtons("crop_chart_type", "Chart type",
                    choices  = c("Line chart", "Bar chart"),
                    selected = "Line chart"),
        checkboxInput("crop_zero_base", "Start y-axis at zero", value = TRUE)
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
        checkboxGroupInput("live_scenario", "Scenario",
                    choices  = c("Current Trends", "NDC Commitments"),
                    selected = c("Current Trends", "NDC Commitments")),
        radioButtons("live_years", "Years",
                    choices  = c("Calibration & Projections", "Calibration"),
                    selected = "Calibration & Projections"),
        radioButtons("live_chart_type", "Chart type",
                    choices  = c("Line chart", "Bar chart"),
                    selected = "Line chart"),
        checkboxInput("live_zero_base", "Start y-axis at zero", value = TRUE)
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
        checkboxGroupInput("trade_scenario", "Scenario",
                    choices  = c("Current Trends", "NDC Commitments"),
                    selected = c("Current Trends", "NDC Commitments")),
        radioButtons("trade_years", "Years",
                    choices  = c("Calibration & Projections", "Calibration"),
                    selected = "Calibration & Projections"),
        radioButtons("trade_chart_type", "Chart type",
                    choices  = c("Line chart", "Bar chart"),
                    selected = "Line chart"),
        checkboxInput("trade_zero_base", "Start y-axis at zero", value = TRUE)
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
        checkboxGroupInput("food_scenario", "Scenario",
                    choices  = c("Current Trends", "NDC Commitments"),
                    selected = c("Current Trends", "NDC Commitments")),
        radioButtons("food_years", "Years",
                    choices  = c("Calibration & Projections", "Calibration"),
                    selected = "Calibration & Projections"),
        radioButtons("food_chart_type", "Chart type",
                    choices  = c("Line chart", "Bar chart"),
                    selected = "Line chart"),
        checkboxInput("food_zero_base", "Start y-axis at zero", value = TRUE)
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
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  x_max <- reactive({
    if (input$years_sel == "Calibration") 2020L else 2050L
  })

  y_range <- reactive({
    calc_y_range(input$class_sel, x_max(), input$zero_base)
  })

  output$charts_ui <- renderUI({
    y_label <- landuse_map[[input$class_sel]]$y_label

    right_col <- if (input$years_sel == "Calibration") {
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

    req(length(input$scenario_sel) > 0)
    chart_out <- if (all(c("Current Trends", "NDC Commitments") %in% input$scenario_sel))
      plotlyOutput("plot_both", height = "460px")
    else if ("Current Trends" %in% input$scenario_sel)
      plotlyOutput("plot_ct",   height = "460px")
    else
      plotlyOutput("plot_ndc",  height = "460px")
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
  output$landuse_dl <- downloadHandler(
    filename = function() {
      nm <- gsub(" ", "_", input$class_sel)
      sc <- if (length(input$scenario_sel) == 2) "Both" else gsub(" ", "_", input$scenario_sel)
      paste0("landuse_", nm, "_", sc, "_", x_max(), ".csv")
    },
    content = function(file) {
      write.csv(make_table_data(input$class_sel, input$scenario_sel, x_max()),
                file, row.names = FALSE)
    }
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
    calc_crop_y_range(input$crop_name, input$crop_type, crop_x_max(), input$crop_zero_base)
  })

  output$crop_charts_ui <- renderUI({
    req(length(crops_map) > 0)
    y_label   <- crop_y_label[[input$crop_type]]
    abs_unit  <- crop_hover_unit[[input$crop_type]]
    abs_title <- paste0("Absolute Difference (", abs_unit, ")")

    right_col <- if (input$crop_years == "Calibration") {
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

    req(length(input$crop_scenario) > 0)
    chart_out <- if (all(c("Current Trends", "NDC Commitments") %in% input$crop_scenario))
      plotlyOutput("crop_plot_both", height = "460px")
    else if ("Current Trends" %in% input$crop_scenario)
      plotlyOutput("crop_plot_ct",   height = "460px")
    else
      plotlyOutput("crop_plot_ndc",  height = "460px")
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
  output$crop_dl <- downloadHandler(
    filename = function() {
      sc <- if (length(input$crop_scenario) == 2) "Both" else gsub(" ", "_", input$crop_scenario)
      paste0("crops_", gsub(" ", "_", input$crop_name), "_",
             input$crop_type, "_", sc, "_", crop_x_max(), ".csv")
    },
    content = function(file) {
      write.csv(make_crop_table_data(input$crop_name, input$crop_type,
                                     input$crop_scenario, crop_x_max()),
                file, row.names = FALSE)
    }
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

  # ── Livestock tab ─────────────────────────────────────────────────────────────
  live_x_max <- reactive({
    if (input$live_years == "Calibration") 2020L else 2050L
  })

  live_y_range <- reactive({
    req(length(livestock_map) > 0, input$live_product %in% names(livestock_map))
    calc_live_y_range(input$live_product, live_x_max(), input$live_zero_base)
  })

  output$live_charts_ui <- renderUI({
    req(length(livestock_map) > 0)
    has_hist <- livestock_map[[input$live_product]]$has_hist

    y_lbl     <- livestock_map[[input$live_product]]$y_label
    unit_lbl  <- livestock_map[[input$live_product]]$unit_label
    right_col <- if (input$live_years == "Calibration" && has_hist) {
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

    req(length(input$live_scenario) > 0)
    chart_out <- if (all(c("Current Trends", "NDC Commitments") %in% input$live_scenario))
      plotlyOutput("live_plot_both", height = "460px")
    else if ("Current Trends" %in% input$live_scenario)
      plotlyOutput("live_plot_ct",   height = "460px")
    else
      plotlyOutput("live_plot_ndc",  height = "460px")
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
    make_live_table_data(input$live_product, input$live_scenario, live_x_max()),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$live_abs_diff_table <- renderTable(
    make_live_diff_data(input$live_product, input$live_scenario, "absolute"),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$live_rel_diff_table <- renderTable(
    make_live_rel_diff_colored(input$live_product, input$live_scenario),
    sanitize.text.function = identity,
    na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$live_dl <- downloadHandler(
    filename = function() {
      sc <- if (length(input$live_scenario) == 2) "Both" else gsub(" ", "_", input$live_scenario)
      paste0("livestock_", gsub(" ", "_", input$live_product), "_", sc, "_", live_x_max(), ".csv")
    },
    content = function(file) {
      write.csv(make_live_table_data(input$live_product, input$live_scenario, live_x_max()),
                file, row.names = FALSE)
    }
  )
  output$live_plot_both <- renderPlotly({
    make_live_combined_plot(input$live_product, live_x_max(), live_y_range(), input$live_chart_type)
  })
  output$live_plot_ct <- renderPlotly({
    make_live_single_plot(input$live_product, "Current Trends",  COL_CT,  live_x_max(), live_y_range(), input$live_chart_type)
  })
  output$live_plot_ndc <- renderPlotly({
    make_live_single_plot(input$live_product, "NDC Commitments", COL_NDC, live_x_max(), live_y_range(), input$live_chart_type)
  })

  # ── Trade tab ─────────────────────────────────────────────────────────────────
  observeEvent(input$trade_type, {
    choices <- trade_map[[input$trade_type]]$products
    updateSelectInput(session, "trade_product",
                      choices  = choices,
                      selected = choices[1])
  })

  trade_x_max <- reactive({
    if (input$trade_years == "Calibration") 2020L else 2050L
  })

  trade_y_range <- reactive({
    req(input$trade_type %in% names(trade_map),
        input$trade_product %in% trade_map[[input$trade_type]]$products)
    calc_trade_y_range(input$trade_type, input$trade_product, trade_x_max(), input$trade_zero_base)
  })

  output$trade_charts_ui <- renderUI({
    req(input$trade_type %in% names(trade_map),
        input$trade_product %in% trade_map[[input$trade_type]]$products)

    right_col <- tagList(
      div(style = "display:flex; align-items:center; gap:8px;",
          strong(paste0(input$trade_type, " (Mt)")),
          downloadButton("trade_dl", "CSV",
                         style = "padding:2px 8px; font-size:11px; height:24px; line-height:20px;")),
      div(style = "overflow-x: auto; margin-top: 6px; font-size: 11px;",
          tableOutput("trade_data_table"))
    )

    req(length(input$trade_scenario) > 0)
    chart_out <- if (all(c("Current Trends", "NDC Commitments") %in% input$trade_scenario))
      plotlyOutput("trade_plot_both", height = "460px")
    else if ("Current Trends" %in% input$trade_scenario)
      plotlyOutput("trade_plot_ct",   height = "460px")
    else
      plotlyOutput("trade_plot_ndc",  height = "460px")
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
                          input$trade_scenario, trade_x_max()),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$trade_dl <- downloadHandler(
    filename = function() {
      prod <- gsub("[() ]", "_", input$trade_product)
      sc <- if (length(input$trade_scenario) == 2) "Both" else gsub(" ", "_", input$trade_scenario)
      paste0("trade_", input$trade_type, "_", prod, "_", sc, "_", trade_x_max(), ".csv")
    },
    content = function(file) {
      write.csv(make_trade_table_data(input$trade_type, input$trade_product,
                                      input$trade_scenario, trade_x_max()),
                file, row.names = FALSE)
    }
  )
  output$trade_plot_both <- renderPlotly({
    make_trade_combined_plot(input$trade_type, input$trade_product,
                             trade_x_max(), trade_y_range(), input$trade_chart_type)
  })
  output$trade_plot_ct <- renderPlotly({
    make_trade_single_plot(input$trade_type, input$trade_product, "Current Trends",
                           COL_CT, trade_x_max(), trade_y_range(), input$trade_chart_type)
  })
  output$trade_plot_ndc <- renderPlotly({
    make_trade_single_plot(input$trade_type, input$trade_product, "NDC Commitments",
                           COL_NDC, trade_x_max(), trade_y_range(), input$trade_chart_type)
  })

  # ── Food tab ──────────────────────────────────────────────────────────────────
  food_x_max <- reactive({
    if (input$food_years == "Calibration") 2020L else 2050L
  })

  food_y_range <- reactive({
    req(input$food_variable %in% names(food_map))
    calc_food_y_range(input$food_variable, food_x_max(), input$food_zero_base)
  })

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

    req(length(input$food_scenario) > 0)
    chart_out <- if (all(c("Current Trends", "NDC Commitments") %in% input$food_scenario))
      plotlyOutput("food_plot_both", height = "460px")
    else if ("Current Trends" %in% input$food_scenario)
      plotlyOutput("food_plot_ct",   height = "460px")
    else
      plotlyOutput("food_plot_ndc",  height = "460px")
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
    make_food_table_data(input$food_variable, input$food_scenario, food_x_max()),
    digits = 0, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$food_dl <- downloadHandler(
    filename = function() {
      sc <- if (length(input$food_scenario) == 2) "Both" else gsub(" ", "_", input$food_scenario)
      paste0("food_", gsub(" ", "_", input$food_variable), "_", sc, "_", food_x_max(), ".csv")
    },
    content = function(file) {
      write.csv(make_food_table_data(input$food_variable, input$food_scenario, food_x_max()),
                file, row.names = FALSE)
    }
  )
  output$food_plot_both <- renderPlotly({
    make_food_combined_plot(input$food_variable, food_x_max(), food_y_range(), input$food_chart_type)
  })
  output$food_plot_ct <- renderPlotly({
    make_food_single_plot(input$food_variable, "Current Trends",  COL_CT,  food_x_max(), food_y_range(), input$food_chart_type)
  })
  output$food_plot_ndc <- renderPlotly({
    make_food_single_plot(input$food_variable, "NDC Commitments", COL_NDC, food_x_max(), food_y_range(), input$food_chart_type)
  })

  # ── Emissions tab ─────────────────────────────────────────────────────────────
  emiss_x_max <- reactive({
    if (input$emiss_years == "Calibration") 2020L else 2050L
  })

  emiss_y_range <- reactive({
    req(length(emissions_map) > 0, input$emiss_sel %in% names(emissions_map))
    calc_emiss_y_range(input$emiss_sel, emiss_x_max(), input$emiss_zero_base)
  })

  observeEvent(input$emiss_sel, {
    updateCheckboxInput(session, "emiss_zero_base",
                        value = input$emiss_sel != "CO₂ AFOLU")
  })

  output$emiss_charts_ui <- renderUI({
    req(length(emissions_map) > 0)
    y_label <- emissions_map[[input$emiss_sel]]$y_label

    right_col <- if (input$emiss_years == "Calibration") {
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

    req(length(input$emiss_scenario) > 0)
    chart_out <- if (all(c("Current Trends", "NDC Commitments") %in% input$emiss_scenario))
      plotlyOutput("emiss_plot_both", height = "460px")
    else if ("Current Trends" %in% input$emiss_scenario)
      plotlyOutput("emiss_plot_ct",   height = "460px")
    else
      plotlyOutput("emiss_plot_ndc",  height = "460px")
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
    make_emiss_table_data(input$emiss_sel, input$emiss_scenario, emiss_x_max()),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$emiss_abs_diff_table <- renderTable(
    make_emiss_diff_data(input$emiss_sel, input$emiss_scenario, "absolute"),
    digits = 2, na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$emiss_rel_diff_table <- renderTable(
    make_emiss_rel_diff_colored(input$emiss_sel, input$emiss_scenario),
    sanitize.text.function = identity,
    na = "", striped = TRUE, bordered = TRUE, rownames = FALSE
  )
  output$emiss_dl <- downloadHandler(
    filename = function() {
      nm <- gsub(" ", "_", input$emiss_sel)
      sc <- if (length(input$emiss_scenario) == 2) "Both" else gsub(" ", "_", input$emiss_scenario)
      paste0("emissions_", nm, "_", sc, "_", emiss_x_max(), ".csv")
    },
    content = function(file) {
      write.csv(make_emiss_table_data(input$emiss_sel, input$emiss_scenario, emiss_x_max()),
                file, row.names = FALSE)
    }
  )
  output$emiss_plot_both <- renderPlotly({
    make_emiss_combined_plot(input$emiss_sel, emiss_x_max(), emiss_y_range(), input$emiss_chart_type)
  })
  output$emiss_plot_ct <- renderPlotly({
    make_emiss_single_plot(input$emiss_sel, "Current Trends",  COL_CT,  emiss_x_max(), emiss_y_range(), input$emiss_chart_type)
  })
  output$emiss_plot_ndc <- renderPlotly({
    make_emiss_single_plot(input$emiss_sel, "NDC Commitments", COL_NDC, emiss_x_max(), emiss_y_range(), input$emiss_chart_type)
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

  make_map_img <- function(sc_dir, type_sel, var_sel, year) {
    if (type_sel == "Land Cover") {
      rel_path <- sprintf("maps/%s/landcover/landcover_%s_%s.png", sc_dir, var_sel, year)
    } else if (type_sel == "Outflows") {
      rel_path <- sprintf("maps/%s/transitions/outflow_%s_%s.png",  sc_dir, var_sel, year)
    } else {
      label    <- sub(" → ", "_to_", var_sel)
      rel_path <- sprintf("maps/%s/transitions/transition_%s_%s.png", sc_dir, label, year)
    }
    disk_path <- paste0("data/", rel_path)
    if (file.exists(disk_path)) {
      tags$img(src   = rel_path,
               class = "maps-img",
               style = "border:1px solid #ddd; border-radius:4px;",
               alt   = paste(sc_dir, type_sel, var_sel, year))
    } else {
      div(style = paste("display:flex; align-items:center; justify-content:center;",
                        "height:300px; border:1px dashed #bbb; border-radius:4px;",
                        "background:#f8f8f8; color:#888; font-size:0.85rem; text-align:center; padding:1rem;"),
          tags$p("Maps not generated yet.", tags$br(),
                 tags$code("Rscript 04_generate_maps.R")))
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
    fluidRow(
      column(4, make_map_img("ct",   type_sel, var_sel, year)),
      column(4, make_map_img("ndc",  type_sel, var_sel, year)),
      column(4, make_map_img("diff", type_sel, var_sel, year))
    )
  })
}

shinyApp(ui, server)
