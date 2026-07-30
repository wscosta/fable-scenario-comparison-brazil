# 03_generate_report.R
# Generates a Word report: cover, TOC, then one landscape page per variable
# with a "Both" line chart (CT vs NDC vs Historical) and values table (2000–2050).
#
# Required packages: dplyr, ggplot2, officer, flextable
# Run from project root or via Rscript 03_generate_report.R

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(officer)
  library(flextable)
})

# ── Working directory ─────────────────────────────────────────────────────────
local({
  flag <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(flag)) setwd(dirname(normalizePath(sub("--file=", "", flag))))
})

# ── Data loading ──────────────────────────────────────────────────────────────
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
to_mt <- function(values, col_name, unit_override = NULL) {
  unit <- if (!is.null(unit_override)) unit_override
          else trimws(tolower(fable_units[col_name]))
  if (is.na(unit))                           return(values)
  if (grepl("^mt$|^million.*t", unit))       return(values)
  if (grepl("^1[,.]?000\\s*t$|^kt$", unit)) return(values / 1000)
  if (grepl("^t$|^tonne", unit))             return(values / 1e6)
  values
}

# ── Map configs ───────────────────────────────────────────────────────────────
fable_cols <- names(df_scenarios)

landuse_map <- list(
  "Cropland"   = list(fable_col = "CalcCropland",  fable_unit = "1000 ha",
                      hist_type = "Cropland",                hist_source = "IBGE",
                      y_label = "Area (Mha)"),
  "Pasture"    = list(fable_col = "CalcPasture",   fable_unit = "1000 ha",
                      hist_type = "Pastures and Rangelands", hist_source = "LAPIG",
                      y_label = "Area (Mha)"),
  "Forest"     = list(fable_col = "CalcForest",    fable_unit = "1000 ha",
                      hist_type = "Forest",                  hist_source = "Mapbiomas",
                      y_label = "Area (Mha)"),
  "Other Land" = list(fable_col = "CalcOtherLand", fable_unit = "1000 ha",
                      hist_type = "Other Land",              hist_source = "Mapbiomas",
                      y_label = "Area (Mha)"),
  "Urban"      = list(fable_col = "CalcUrban",     fable_unit = "1000 ha",
                      hist_type = "Urban",                   hist_source = "Mapbiomas",
                      y_label = "Area (Mha)")
)
landuse_map <- Filter(function(cfg) cfg$fable_col %in% fable_cols, landuse_map)

emissions_map <- list(
  "CO₂ AFOLU" = list(
    fable_col = "CalcAllLandCO2e",
    hist_type = "CO2 AFOLU", hist_source = "SEEG13", hist_gwp = 1,
    year_min = 2005L, y_label = "Emissions (MtCO₂e)"
  ),
  "CH₄ Enteric Fermentation" = list(
    fable_col = "CalcLiveCH4",
    hist_type = "CH4 Enteric Fermentation", hist_source = "SEEG13", hist_gwp = 27.2,
    y_label = "Emissions (MtCO₂e)"
  ),
  "CH₄ Rice" = list(
    fable_col = "CalcCropCH4",
    hist_type = "CH4 Rice", hist_source = "SEEG13", hist_gwp = 27.2,
    y_label = "Emissions (MtCO₂e)"
  ),
  "N₂O from Agriculture" = list(
    fable_cols = c("CalcLiveN2O", "CalcCropN2O"),
    hist_types = c("N2O Animal Waste Management", "N2O Burning of Crop Residues",
                   "N2O Decay of Crop Residues", "N2O Inorganic Fertilizers",
                   "N2O Manure Applied to Croplands", "N2O Pasture",
                   "N2O Peatland", "N2O Soil Organic Matter Loss"),
    hist_source = "SEEG13", hist_gwp = 273,
    y_label = "Emissions (MtCO₂e)"
  )
)
emissions_map <- Filter(function(cfg) {
  cols <- if (!is.null(cfg$fable_cols)) cfg$fable_cols else cfg$fable_col
  all(cols %in% fable_cols)
}, emissions_map)

crops_map <- list(
  "Soybeans"  = list(fable_product = "soyabean",  area_unit = "1000 ha", prod_unit = "1000 t",
                     hist_area_type = "Soybean Area",        hist_area_src = "IBGE",
                     hist_prod_type = "Soybean Production",  hist_prod_src = "IBGE"),
  "Corn"      = list(fable_product = "corn",       area_unit = "1000 ha", prod_unit = "1000 t",
                     hist_area_type = "Maize Area",          hist_area_src = "IBGE",
                     hist_prod_type = "Maize Production",    hist_prod_src = "IBGE"),
  "Sugarcane" = list(fable_product = "sugarcane",  area_unit = "1000 ha", prod_unit = "1000 t",
                     hist_area_type = "Sugarcane Area",      hist_area_src = "IBGE",
                     hist_prod_type = "Sugarcane Production", hist_prod_src = "IBGE")
)
crops_map <- Filter(function(cfg) cfg$fable_product %in% df_crops$Product, crops_map)

livestock_map <- list(
  "Beef Production"      = list(fable_product = "beef",    prod_unit = "1000 t",
                                hist_type = "Ruminant Meat", hist_source = "FAOSTAT",
                                has_hist = TRUE,  y_label = "Production (Mt)", unit_label = "Mt"),
  "Milk Production"      = list(fable_product = "milk",    prod_unit = "1000 t",
                                has_hist = FALSE, y_label = "Production (Mt)", unit_label = "Mt"),
  "Chicken Production"   = list(fable_product = "chicken", prod_unit = "1000 t",
                                has_hist = FALSE, y_label = "Production (Mt)", unit_label = "Mt"),
  "Pork Production"      = list(fable_product = "pork",    prod_unit = "1000 t",
                                has_hist = FALSE, y_label = "Production (Mt)", unit_label = "Mt"),
  "Cattle Herd"          = list(fable_col = "FeasHerd",   unit_divisor = 1000,
                                has_hist = FALSE, y_label = "Cattle Herd (Million TLU)", unit_label = "Million TLU"),
  "Cattle Stocking Rate" = list(fable_col = "RumDensity", unit_divisor = 1,
                                has_hist = FALSE, y_label = "Density (TLU/ha)", unit_label = "TLU/ha")
)
livestock_map <- Filter(function(cfg) {
  if (!is.null(cfg$fable_product)) cfg$fable_product %in% df_crops$Product
  else if (!is.null(cfg$fable_col)) cfg$fable_col %in% names(df_livestock)
  else FALSE
}, livestock_map)

trade_map <- list(
  "Exports" = list(
    fable_col = "Export_quantity", unit = "1000 t",
    products  = c("Soybeans (all)", "Soybeans (grain)", "Soybeans (cake)", "Soybeans (oil)", "Corn", "Beef"),
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
    fable_col = "Import_quantity", unit = "1000 t",
    products  = "Wheat",
    fable_product = list("Wheat" = "wheat")
  )
)

food_map <- list(
  "Food Consumption" = list(fable_col = "kcal_feas", y_label = "Intake (kcal/cap/day)")
)

# ── Data getters ──────────────────────────────────────────────────────────────
get_landuse_fable <- function(class_name, scenario_name, x_max) {
  cfg <- landuse_map[[class_name]]
  df_scenarios %>%
    filter(scenario == scenario_name, Year <= x_max) %>%
    select(year = Year, value = all_of(cfg$fable_col)) %>%
    mutate(value = to_mha(as.numeric(value), cfg$fable_col, cfg$fable_unit)) %>%
    arrange(year)
}

get_landuse_hist <- function(class_name, x_max) {
  cfg <- landuse_map[[class_name]]
  df_hist %>%
    filter(trimws(type) == cfg$hist_type, trimws(source) == cfg$hist_source,
           year > 1995, year <= x_max) %>%
    select(year, value) %>%
    mutate(value = as.numeric(value))
}

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
    filter(trimws(type) %in% hist_types, trimws(source) == cfg$hist_source,
           year >= year_min, year <= hmax) %>%
    group_by(year) %>%
    summarise(value = sum(as.numeric(value), na.rm = TRUE) * cfg$hist_gwp, .groups = "drop")
}

get_crop_fable <- function(crop_name, type_sel, scenario_name, x_max) {
  cfg  <- crops_map[[crop_name]]
  rows <- df_crops %>%
    filter(scenario == scenario_name, trimws(Product) == cfg$fable_product, Year <= x_max) %>%
    arrange(Year)
  area <- to_mha(rows$FeasHarvarea, "FeasHarvarea", cfg$area_unit)
  prod <- to_mt(rows$ProdQ_feas,   "ProdQ_feas",   cfg$prod_unit)
  if (type_sel == "Area")       return(tibble(year = rows$Year, value = area))
  if (type_sel == "Production") return(tibble(year = rows$Year, value = prod))
  tibble(year = rows$Year, value = prod / area)
}

get_crop_hist_data <- function(crop_name, type_sel, x_max) {
  cfg  <- crops_map[[crop_name]]
  hmax <- min(x_max, 2020)
  get_metric <- function(hist_type, hist_src)
    df_hist %>%
      filter(trimws(type) == hist_type, trimws(source) == hist_src,
             year > 1995, year <= hmax) %>%
      select(year, value) %>% mutate(value = as.numeric(value))
  if (type_sel == "Area")       return(get_metric(cfg$hist_area_type, cfg$hist_area_src))
  if (type_sel == "Production") return(get_metric(cfg$hist_prod_type, cfg$hist_prod_src))
  a <- get_metric(cfg$hist_area_type, cfg$hist_area_src)
  p <- get_metric(cfg$hist_prod_type, cfg$hist_prod_src)
  inner_join(a, p, by = "year", suffix = c("_a", "_p")) %>%
    mutate(value = value_p / value_a) %>% select(year, value)
}

get_live_fable <- function(product, scenario_name, x_max) {
  cfg <- livestock_map[[product]]
  if (!is.null(cfg$fable_col)) {
    divisor <- if (!is.null(cfg$unit_divisor)) cfg$unit_divisor else 1
    df_livestock %>%
      filter(scenario == scenario_name, Year <= x_max) %>% arrange(Year) %>%
      select(year = Year, value = all_of(cfg$fable_col)) %>%
      mutate(value = as.numeric(value) / divisor)
  } else {
    rows <- df_crops %>%
      filter(scenario == scenario_name, trimws(Product) == cfg$fable_product, Year <= x_max) %>%
      arrange(Year)
    tibble(year = rows$Year, value = to_mt(rows$ProdQ_feas, "ProdQ_feas", cfg$prod_unit))
  }
}

get_live_hist <- function(product, x_max) {
  cfg <- livestock_map[[product]]
  if (!cfg$has_hist) return(tibble(year = integer(), value = numeric()))
  hmax <- min(x_max, 2020)
  df_hist %>%
    filter(trimws(type) == cfg$hist_type, trimws(source) == cfg$hist_source,
           year > 1995, year <= hmax) %>%
    select(year, value) %>% mutate(value = as.numeric(value))
}

get_trade_fable <- function(trade_type, product, scenario_name, x_max) {
  cfg   <- trade_map[[trade_type]]
  col   <- cfg$fable_col
  prods <- cfg$fable_product[[product]]
  df_crops %>%
    filter(scenario == scenario_name, trimws(Product) %in% prods, Year <= x_max) %>%
    group_by(Year) %>%
    summarise(value = to_mt(sum(.data[[col]], na.rm = TRUE), col, cfg$unit), .groups = "drop") %>%
    arrange(Year) %>% rename(year = Year)
}

get_food_fable <- function(variable, scenario_name, x_max) {
  cfg <- food_map[[variable]]
  df_scenarios %>%
    filter(scenario == scenario_name, Year <= x_max) %>%
    select(year = Year, value = all_of(cfg$fable_col)) %>%
    mutate(value = as.numeric(value)) %>% arrange(year)
}

# ── Table data builders ───────────────────────────────────────────────────────
# All return a data.frame: first col = " " (row label), rest = year columns.

build_table_df <- function(rows_list, years) {
  mat <- do.call(rbind, rows_list)
  df  <- as.data.frame(mat)
  colnames(df) <- as.character(years)
  cbind(` ` = rownames(df), df, stringsAsFactors = FALSE)
}

hist_row <- function(hist_data, years) {
  sapply(years, function(y) {
    if (y > 2020) return(NA_real_)
    v <- hist_data$value[hist_data$year == y]
    if (length(v) == 0) NA_real_ else v[1]
  })
}

make_landuse_tbl <- function(class_name, x_max) {
  years  <- seq(2000, x_max, 5)
  get_sc <- function(sc) get_landuse_fable(class_name, sc, x_max) %>%
              filter(year %in% years) %>% arrange(year) %>% dplyr::pull(value)
  build_table_df(list("Current Trends"  = get_sc("Current Trends"),
                      "NDC Commitments" = get_sc("NDC Commitments"),
                      "Historical"      = hist_row(get_landuse_hist(class_name, 2020), years)),
                 years)
}

make_emiss_tbl <- function(emiss_name, x_max) {
  year_min <- if (!is.null(emissions_map[[emiss_name]]$year_min))
                emissions_map[[emiss_name]]$year_min else 2000L
  years  <- seq(year_min, x_max, 5)
  get_sc <- function(sc) get_emiss_fable(emiss_name, sc, x_max) %>%
              filter(year %in% years) %>% arrange(year) %>% dplyr::pull(value)
  h      <- get_emiss_hist(emiss_name, 2020)
  build_table_df(list("Current Trends"  = get_sc("Current Trends"),
                      "NDC Commitments" = get_sc("NDC Commitments"),
                      "Historical"      = hist_row(h, years)),
                 years)
}

make_crop_tbl <- function(crop_name, type_sel, x_max) {
  years  <- seq(2000, x_max, 5)
  get_sc <- function(sc) get_crop_fable(crop_name, type_sel, sc, x_max) %>%
              filter(year %in% years) %>% arrange(year) %>% dplyr::pull(value)
  h      <- get_crop_hist_data(crop_name, type_sel, 2020)
  build_table_df(list("Current Trends"  = get_sc("Current Trends"),
                      "NDC Commitments" = get_sc("NDC Commitments"),
                      "Historical"      = hist_row(h, years)),
                 years)
}

make_live_tbl <- function(product, x_max) {
  years  <- seq(2000, x_max, 5)
  get_sc <- function(sc) get_live_fable(product, sc, x_max) %>%
              filter(year %in% years) %>% arrange(year) %>% dplyr::pull(value)
  rows   <- list("Current Trends"  = get_sc("Current Trends"),
                 "NDC Commitments" = get_sc("NDC Commitments"))
  if (livestock_map[[product]]$has_hist)
    rows[["Historical"]] <- hist_row(get_live_hist(product, 2020), years)
  build_table_df(rows, years)
}

make_trade_tbl <- function(trade_type, product, x_max) {
  years  <- seq(2000, x_max, 5)
  get_sc <- function(sc) get_trade_fable(trade_type, product, sc, x_max) %>%
              filter(year %in% years) %>% arrange(year) %>% dplyr::pull(value)
  build_table_df(list("Current Trends"  = get_sc("Current Trends"),
                      "NDC Commitments" = get_sc("NDC Commitments")),
                 years)
}

make_food_tbl <- function(variable, x_max) {
  years  <- seq(2000, x_max, 5)
  get_sc <- function(sc) get_food_fable(variable, sc, x_max) %>%
              filter(year %in% years) %>% arrange(year) %>% dplyr::pull(value)
  build_table_df(list("Current Trends"  = get_sc("Current Trends"),
                      "NDC Commitments" = get_sc("NDC Commitments")),
                 years)
}

# ── ggplot2 chart builder ─────────────────────────────────────────────────────
COL_CT   <- "#1565C0"
COL_NDC  <- "#009C3B"
COL_HIST <- "#000000"

SERIES_COLORS <- c("Current Trends" = COL_CT, "NDC Commitments" = COL_NDC, "Historical" = COL_HIST)
SERIES_SHAPES <- c("Current Trends" = 16, "NDC Commitments" = 17, "Historical" = 15)

make_chart <- function(ct_data, ndc_data, hist_data,
                       title, y_label,
                       zero_base = TRUE, zero_line = FALSE,
                       year_min = 2000L, x_max = 2050L) {
  all_data <- bind_rows(
    ct_data  %>% mutate(series = "Current Trends"),
    ndc_data %>% mutate(series = "NDC Commitments")
  )
  if (!is.null(hist_data) && nrow(hist_data) > 0)
    all_data <- bind_rows(all_data, hist_data %>% mutate(series = "Historical"))

  all_data$series <- factor(all_data$series,
                            levels = c("Current Trends", "NDC Commitments", "Historical"))

  vals  <- all_data$value[!is.na(all_data$value)]
  pad   <- max(diff(range(vals)) * 0.05, 0.01)
  y_min <- if (zero_base && min(vals) >= 0) 0 else min(vals) - pad
  y_max <- max(vals) + pad

  ggplot(all_data, aes(x = year, y = value,
                       color = series, shape = series, group = series)) +
    geom_vline(xintercept = 2020, linetype = "dashed", color = "grey65", linewidth = 0.5) +
    { if (zero_line) geom_hline(yintercept = 0, linetype = "dashed",
                                color = "grey50", linewidth = 0.4) } +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    geom_point(size = 2.5, na.rm = TRUE) +
    scale_color_manual(values = SERIES_COLORS) +
    scale_shape_manual(values = SERIES_SHAPES) +
    scale_x_continuous(breaks = seq(year_min, x_max, 5),
                       limits = c(year_min - 1L, x_max + 1L),
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(y_min, y_max), expand = c(0, 0)) +
    labs(title = title, y = y_label, x = NULL, color = NULL, shape = NULL) +
    theme_bw(base_size = 10) +
    theme(
      plot.title        = element_text(face = "bold", size = 11),
      legend.position   = "right",
      legend.title      = element_blank(),
      legend.key.size   = unit(0.45, "cm"),
      legend.text       = element_text(size = 9),
      legend.background = element_rect(color = "black", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(color = "#DDDDDD"),
      axis.text.x       = element_text(size = 8, angle = 45, hjust = 1),
      axis.text.y       = element_text(size = 8),
      panel.border      = element_rect(color = "black", fill = NA, linewidth = 0.6)
    )
}

save_chart_png <- function(p, width = 9.5, height = 3.6) {
  tmp <- tempfile(fileext = ".png")
  ggsave(tmp, plot = p, width = width, height = height, dpi = 150, bg = "white")
  tmp
}

# ── flextable builder ─────────────────────────────────────────────────────────
# Text width on landscape A4 with 0.8" margins: 11.69 - 1.6 = 10.09"
TBL_TOTAL_W  <- 10.09
TBL_LABEL_W  <- 1.70
ROW_BG <- c("Current Trends" = "#DDEEFF", "NDC Commitments" = "#DDEEDC", "Historical" = "#F0F0F0")

make_ft <- function(df, digits = 2) {
  year_cols <- names(df)[-1]
  n_yr      <- length(year_cols)
  yr_w      <- (TBL_TOTAL_W - TBL_LABEL_W) / n_yr

  for (col in year_cols) {
    vals     <- as.numeric(df[[col]])
    df[[col]] <- ifelse(is.na(vals), "—", formatC(vals, digits = digits, format = "f"))
  }

  ft <- flextable(df) %>%
    set_header_labels(` ` = "") %>%
    bold(part = "header") %>%
    fontsize(size = 8.5, part = "all") %>%
    font(fontname = "Calibri", part = "all") %>%
    align(j = seq_len(n_yr + 1)[-1], align = "center", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    bold(j = 1, part = "body") %>%
    width(j = 1,                    width = TBL_LABEL_W) %>%
    width(j = seq_len(n_yr + 1)[-1], width = yr_w) %>%
    border_inner_h(border = fp_border(color = "grey75", width = 0.5), part = "body") %>%
    border_outer(border  = fp_border(color = "black",   width = 1.0), part = "all")

  for (i in seq_len(nrow(df))) {
    lbl <- df[[1]][i]
    if (lbl %in% names(ROW_BG))
      ft <- bg(ft, i = i, bg = ROW_BG[[lbl]], part = "body")
  }
  ft
}

# ── Page / section properties ─────────────────────────────────────────────────
ps_portrait <- prop_section(
  page_size    = page_size(orient = "portrait",  width = 8.27,  height = 11.69),
  page_margins = page_mar(top = 1, bottom = 1, left = 1, right = 1,
                          header = 0.5, footer = 0.5),
  type = "nextPage"
)
ps_landscape <- prop_section(
  page_size    = page_size(orient = "landscape", width = 11.69, height = 8.27),
  page_margins = page_mar(top = 0.8, bottom = 0.8, left = 0.8, right = 0.8,
                          header = 0.3, footer = 0.3),
  type = "nextPage"
)
end_portrait  <- function(doc) body_end_block_section(doc, block_section(ps_portrait))
end_landscape <- function(doc) body_end_block_section(doc, block_section(ps_landscape))

# ── Content page helper ───────────────────────────────────────────────────────
# section_title: Heading 1 text (NULL = omit); var_title: Heading 2 text
add_page <- function(doc, chart_path, table_df, digits,
                     section_title = NULL, var_title) {
  if (!is.null(section_title))
    doc <- body_add_par(doc, section_title, style = "heading 1")
  doc <- body_add_par(doc, var_title, style = "heading 2")
  doc <- body_add_img(doc, src = chart_path, width = 9.5, height = 3.6)
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_flextable(doc, make_ft(table_df, digits = digits))
  end_landscape(doc)
}

# ═════════════════════════════════════════════════════════════════════════════
# Document assembly
# ═════════════════════════════════════════════════════════════════════════════
message("Building report...")
doc <- read_docx()

# ── Cover page ────────────────────────────────────────────────────────────────
ctr <- fp_par(text.align = "center")

cover_title <- fpar(
  ftext("FABLE Calculator Brazil v50",
        prop = fp_text(font.size = 28, bold = TRUE, font.family = "Calibri")),
  fp_p = ctr
)
cover_sub <- fpar(
  ftext("Scenario Comparison Report",
        prop = fp_text(font.size = 16, font.family = "Calibri", color = "#444444")),
  fp_p = ctr
)
cover_vs <- fpar(
  ftext("Current Trends vs NDC Commitments",
        prop = fp_text(font.size = 13, italic = TRUE, font.family = "Calibri")),
  fp_p = ctr
)
cover_date <- fpar(
  ftext(format(Sys.Date(), "%B %Y"),
        prop = fp_text(font.size = 11, font.family = "Calibri", color = "#666666")),
  fp_p = ctr
)

doc <- doc %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(cover_title) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(cover_sub) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(cover_vs) %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(cover_date)
doc <- end_portrait(doc)

# ── Table of contents ─────────────────────────────────────────────────────────
# The TOC is a Word field — open the document and press Ctrl+A then F9 to update it.
doc <- doc %>%
  body_add_par("Table of Contents", style = "heading 1") %>%
  body_add_toc(level = 2)
doc <- end_portrait(doc)

# ── Land Use (5 pages) ────────────────────────────────────────────────────────
for (i in seq_along(landuse_map)) {
  nm  <- names(landuse_map)[i]
  cfg <- landuse_map[[nm]]
  message("  Land Use: ", nm)

  ct_d   <- get_landuse_fable(nm, "Current Trends",  2050L)
  ndc_d  <- get_landuse_fable(nm, "NDC Commitments", 2050L)
  hist_d <- get_landuse_hist(nm, 2050L)

  p <- make_chart(ct_d, ndc_d, hist_d,
                  title    = paste0(nm, ": Current Trends vs NDC Commitments"),
                  y_label  = cfg$y_label,
                  year_min = 2000L, x_max = 2050L)

  doc <- add_page(doc, save_chart_png(p), make_landuse_tbl(nm, 2050L), digits = 2,
                  section_title = if (i == 1L) "Land Use" else NULL,
                  var_title = nm)
}

# ── Emissions (4 pages) ───────────────────────────────────────────────────────
for (i in seq_along(emissions_map)) {
  nm  <- names(emissions_map)[i]
  cfg <- emissions_map[[nm]]
  message("  Emissions: ", nm)

  year_min <- if (!is.null(cfg$year_min)) cfg$year_min else 2000L
  is_co2   <- !is.null(cfg$year_min)

  ct_d   <- get_emiss_fable(nm, "Current Trends",  2050L)
  ndc_d  <- get_emiss_fable(nm, "NDC Commitments", 2050L)
  hist_d <- get_emiss_hist(nm, 2050L)

  p <- make_chart(ct_d, ndc_d, hist_d,
                  title     = paste0(nm, ": Current Trends vs NDC Commitments"),
                  y_label   = cfg$y_label,
                  zero_base = !is_co2,
                  zero_line = is_co2,
                  year_min  = year_min, x_max = 2050L)

  doc <- add_page(doc, save_chart_png(p), make_emiss_tbl(nm, 2050L), digits = 2,
                  section_title = if (i == 1L) "Emissions" else NULL,
                  var_title = nm)
}

# ── Crops (9 pages: 3 crops × 3 types) ───────────────────────────────────────
crop_y_labels <- c("Area" = "Area (Mha)", "Production" = "Production (Mt)", "Yield" = "Yield (t/ha)")
first_crop <- TRUE

for (crop_nm in names(crops_map)) {
  for (type_sel in c("Area", "Production", "Yield")) {
    message("  Crops: ", crop_nm, " — ", type_sel)

    ct_d   <- get_crop_fable(crop_nm, type_sel, "Current Trends",  2050L)
    ndc_d  <- get_crop_fable(crop_nm, type_sel, "NDC Commitments", 2050L)
    hist_d <- get_crop_hist_data(crop_nm, type_sel, 2050L)
    digits <- if (type_sel == "Yield") 3L else 2L

    p <- make_chart(ct_d, ndc_d, hist_d,
                    title    = paste0(crop_nm, " — ", type_sel, ": Current Trends vs NDC Commitments"),
                    y_label  = crop_y_labels[[type_sel]],
                    year_min = 2000L, x_max = 2050L)

    doc <- add_page(doc, save_chart_png(p), make_crop_tbl(crop_nm, type_sel, 2050L),
                    digits = digits,
                    section_title = if (first_crop) "Crops" else NULL,
                    var_title = paste0(crop_nm, " — ", type_sel))
    first_crop <- FALSE
  }
}

# ── Livestock (6 pages) ───────────────────────────────────────────────────────
for (i in seq_along(livestock_map)) {
  nm  <- names(livestock_map)[i]
  cfg <- livestock_map[[nm]]
  message("  Livestock: ", nm)

  ct_d   <- get_live_fable(nm, "Current Trends",  2050L)
  ndc_d  <- get_live_fable(nm, "NDC Commitments", 2050L)
  hist_d <- get_live_hist(nm, 2050L)
  digits <- if (nm == "Cattle Stocking Rate") 3L else 2L

  p <- make_chart(ct_d, ndc_d, hist_d,
                  title    = paste0(nm, ": Current Trends vs NDC Commitments"),
                  y_label  = cfg$y_label,
                  year_min = 2000L, x_max = 2050L)

  doc <- add_page(doc, save_chart_png(p), make_live_tbl(nm, 2050L), digits = digits,
                  section_title = if (i == 1L) "Livestock" else NULL,
                  var_title = nm)
}

# ── Trade (7 pages: 6 exports + 1 import) ────────────────────────────────────
first_trade <- TRUE
for (trade_type in names(trade_map)) {
  cfg <- trade_map[[trade_type]]
  for (product in cfg$products) {
    message("  Trade: ", trade_type, " — ", product)

    ct_d  <- get_trade_fable(trade_type, product, "Current Trends",  2050L)
    ndc_d <- get_trade_fable(trade_type, product, "NDC Commitments", 2050L)
    empty <- tibble(year = integer(), value = numeric())

    p <- make_chart(ct_d, ndc_d, empty,
                    title    = paste0(product, " ", trade_type, ": Current Trends vs NDC Commitments"),
                    y_label  = paste0(trade_type, " (Mt)"),
                    year_min = 2000L, x_max = 2050L)

    doc <- add_page(doc, save_chart_png(p), make_trade_tbl(trade_type, product, 2050L),
                    digits = 2,
                    section_title = if (first_trade) "Trade" else NULL,
                    var_title = paste0(product, " — ", trade_type))
    first_trade <- FALSE
  }
}

# ── Food (1 page) ─────────────────────────────────────────────────────────────
for (i in seq_along(food_map)) {
  nm  <- names(food_map)[i]
  cfg <- food_map[[nm]]
  message("  Food: ", nm)

  ct_d  <- get_food_fable(nm, "Current Trends",  2050L)
  ndc_d <- get_food_fable(nm, "NDC Commitments", 2050L)
  empty <- tibble(year = integer(), value = numeric())

  p <- make_chart(ct_d, ndc_d, empty,
                  title    = paste0(nm, ": Current Trends vs NDC Commitments"),
                  y_label  = cfg$y_label,
                  year_min = 2000L, x_max = 2050L)

  doc <- add_page(doc, save_chart_png(p), make_food_tbl(nm, 2050L), digits = 0,
                  section_title = if (i == 1L) "Food" else NULL,
                  var_title = nm)
}

# ── Save ──────────────────────────────────────────────────────────────────────
out_path <- "FABLE_Report_BRA_v50.docx"
print(doc, target = out_path)
message("Done. Report saved to: ", normalizePath(out_path))

