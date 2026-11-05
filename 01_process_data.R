library(readxl)
library(dplyr)
library(tidyr)

path_hist <- "data/csv/histdatabrazil.csv"

# ── Scenario metadata ──────────────────────────────────────────────────────────
# data/xlsx/scenarios.csv drives which xlsx files are read and what they're
# labelled — add a row (+ drop the xlsx in data/xlsx/) to add a scenario,
# no code changes needed.
scenario_meta <- read.csv("data/xlsx/scenarios.csv", stringsAsFactors = FALSE)
if (nrow(scenario_meta) == 0) stop("data/xlsx/scenarios.csv has no rows")
scenario_meta$path <- file.path("data/xlsx", scenario_meta$file)
missing_xlsx <- scenario_meta$path[!file.exists(scenario_meta$path)]
if (length(missing_xlsx) > 0)
  stop("Missing xlsx file(s) listed in scenarios.csv: ", paste(missing_xlsx, collapse = ", "))

# ── Locating tables by their header text, not by cell address ─────────────────
# The Calculator's sheets get rows/columns inserted from one version to the
# next (the LAND sheet's table moved down two rows in UP52, which broke a
# hardcoded "K13:AK24"). Every table read below is therefore *found* by the
# text of its header row — a run of consecutive header cells (the "signature")
# searched for in the top-left of the sheet — and its extent is measured from
# there, instead of assuming "row 29" / "BK31:BX91".
probe_cache <- new.env()
probe_sheet <- function(path, sheet, range = "A1:EZ200") {
  key <- paste(path, sheet, range, sep = "|")
  if (is.null(probe_cache[[key]])) {
    m <- as.matrix(suppressMessages(
      read_excel(path, sheet = sheet, range = range, col_names = FALSE, col_types = "text")))
    m[] <- trimws(m)
    dimnames(m) <- NULL
    probe_cache[[key]] <- m
  }
  probe_cache[[key]]
}

# Topmost / leftmost place where `signature` appears as consecutive cells in one
# row. Returns the header's row, first column, and its extent: n_cols = how many
# header cells follow without a gap, n_rows = how many non-blank rows sit below
# it in the first column (0-based: header excluded).
locate_header <- function(path, sheet, signature) {
  m    <- probe_sheet(path, sheet)
  n    <- length(signature)
  hits <- which(m == signature[1], arr.ind = TRUE)   # NA cells never match
  hits <- hits[order(hits[, 1], hits[, 2]), , drop = FALSE]
  for (i in seq_len(nrow(hits))) {
    r <- hits[i, 1]; cc <- hits[i, 2]
    if (cc + n - 1 > ncol(m) || !identical(m[r, cc:(cc + n - 1)], signature)) next
    row_cells <- m[r, cc:ncol(m)]
    col_cells <- m[r:nrow(m), cc]
    return(list(row    = r, col = cc,
                n_cols = which(is.na(c(row_cells, NA)))[1] - 1,
                n_rows = which(is.na(c(col_cells, NA)))[1] - 2))
  }
  stop("Could not find a table headed '", paste(signature, collapse = " | "),
       "' on sheet '", sheet, "' of ", path,
       " — has the Calculator's layout changed?")
}

# Range object for read_excel(): from the header cell, `rows`/`cols` = NA means "to the end".
block_range <- function(h, rows = NA, cols = NA, header_offset = 0) {
  cellranger::cell_limits(
    ul = c(h$row + header_offset, h$col),
    lr = c(if (is.na(rows)) NA else h$row + rows, if (is.na(cols)) NA else h$col + cols - 1))
}

# ── Read SCENATHON_report ─────────────────────────────────────────────────────
# Header row (Location | Year | ...) has the column names, the row above it the
# units; data rows follow. Each row = one year (2000, 2005, ..., 2050),
# columns = output variables.
scenathon_header <- function(path)
  locate_header(path, "SCENATHON_report", c("Location", "Year"))

read_units <- function(path) {
  h            <- scenathon_header(path)
  unit_row     <- read_excel(path, sheet = "SCENATHON_report", col_names = FALSE,
                             range = cellranger::cell_limits(c(h$row - 1, h$col), c(h$row - 1, NA)))
  header_names <- names(read_excel(path, sheet = "SCENATHON_report", n_max = 0,
                                   range = block_range(h)))
  unit_vals    <- trimws(as.character(unlist(unit_row[1, ])))
  length(unit_vals) <- length(header_names)  # pad with NA if row is shorter
  setNames(unit_vals, header_names)
}

read_scenathon <- function(path, scenario_label) {
  raw <- read_excel(path, sheet = "SCENATHON_report", range = block_range(scenathon_header(path)))
  names(raw) <- trimws(names(raw))

  year_col <- names(raw)[grepl("^year$", names(raw), ignore.case = TRUE)][1]
  if (is.na(year_col)) stop("Year column not found in: ", path)

  raw %>%
    rename(Year = all_of(year_col)) %>%
    mutate(Year = suppressWarnings(as.integer(Year))) %>%
    filter(!is.na(Year), Year >= 2000, Year <= 2050) %>%
    mutate(scenario = scenario_label)
}

df_scenarios <- bind_rows(Map(read_scenathon, scenario_meta$path, scenario_meta$label))

# Print all column names once (helpful for mapping variables)
message("── SCENATHON_report columns ──────────────────────────────────")
print(names(df_scenarios))

# ── Read crop table (second table in SCENATHON_report, headed Location|Product|Year) ──
# Columns used: Product, Year, ProdQ_feas, FeasHarvarea, Export/Import_quantity
read_crop_table <- function(path, scenario_label) {
  h          <- locate_header(path, "SCENATHON_report", c("Location", "Product", "Year"))
  raw        <- read_excel(path, sheet = "SCENATHON_report", range = block_range(h))
  names(raw) <- trimws(names(raw))
  raw %>%
    filter(!is.na(Product), !is.na(Year)) %>%
    mutate(Year     = suppressWarnings(as.integer(Year)),
           scenario = scenario_label) %>%
    filter(!is.na(Year), Year >= 2000, Year <= 2050) %>%
    mutate(ProdQ_feas        = as.numeric(ProdQ_feas),
           FeasHarvarea      = as.numeric(FeasHarvarea),
           Export_quantity   = as.numeric(Export_quantity),
           Import_quantity   = as.numeric(Import_quantity)) %>%
    select(scenario, Product, Year, ProdQ_feas, FeasHarvarea, Export_quantity, Import_quantity)
}

df_crops <- bind_rows(Map(read_crop_table, scenario_meta$path, scenario_meta$label))

message("── Crop products found ───────────────────────────────────────")
print(sort(unique(df_crops$Product)))

# ── Read cattle herd/density table (5_feas_livestock, headed ANIMAL_GLOBIOM|ANIMAL|YEAR|FeasHerd) ──
# Filter ANIMAL == "cattle"; two rows per year (BOVO + BOVD) → sum FeasHerd,
# first RumDensity (identical across sub-types within a year).
read_livestock_table <- function(path, scenario_label) {
  # Sheet has multiple side-by-side tables causing duplicate column names.
  # Read only this table's first 5 columns and assign names explicitly.
  h   <- locate_header(path, "5_feas_livestock", c("ANIMAL_GLOBIOM", "ANIMAL", "YEAR", "FeasHerd"))
  raw <- read_excel(path, sheet = "5_feas_livestock", col_names = FALSE, col_types = "text",
                    range = cellranger::cell_limits(c(h$row + 1, h$col), c(NA, h$col + 4)))
  names(raw) <- c("ANIMAL_GLOBIOM", "ANIMAL", "YEAR", "FeasHerd", "RumDensity")
  raw %>%
    filter(!is.na(ANIMAL), !is.na(YEAR),
           trimws(tolower(ANIMAL)) == "cattle") %>%
    mutate(Year     = suppressWarnings(as.integer(YEAR)),
           scenario = scenario_label) %>%
    filter(!is.na(Year), Year >= 2000, Year <= 2050) %>%
    mutate(FeasHerd   = as.numeric(FeasHerd),
           RumDensity = as.numeric(RumDensity)) %>%
    group_by(scenario, Year) %>%
    summarise(
      FeasHerd   = sum(FeasHerd,    na.rm = TRUE),
      RumDensity = first(RumDensity),
      .groups    = "drop"
    )
}

df_livestock <- bind_rows(Map(read_livestock_table, scenario_meta$path, scenario_meta$label))

# ── Read land transition matrix (4_calc_land sheet, calc_landmatrix table) ────
# 60 rows = 6 source classes (LandCoverInit) x 10 five-year periods
# (YearStart/YearEnd, 2000-2050). To{Class} columns are gross flow that period
# (1000 ha/5 year); AreaStart/AreaEnd are stock (1000 ha) for that class/period.
read_land_matrix <- function(path, scenario_label) {
  h   <- locate_header(path, "4_calc_land", c("LandCoverInit", "YearStart", "YearEnd"))
  raw <- read_excel(path, sheet = "4_calc_land", col_names = TRUE,
                    range = block_range(h, rows = h$n_rows, cols = h$n_cols))
  raw %>%
    mutate(scenario   = scenario_label,
           YearStart  = as.integer(YearStart),
           YearEnd    = as.integer(YearEnd))
}

df_luc_matrix <- bind_rows(Map(read_land_matrix, scenario_meta$path, scenario_meta$label))

# ── Read land cover stock (LAND sheet, ResultsLand table) ─────────────────────
# One row per year (2000-2050); stock (1000 ha) for the same 6 classes as
# calc_landmatrix. Reshaped to long format (scenario, year, class, area) —
# 02_shiny_app.R derives period-over-period change via diff() of `area`
# rather than reading this sheet's own *Change columns (verified those are
# exactly diff() of the stock columns; deriving it ourselves avoids an
# inconsistent class->column-name mapping and gives NewForest the same
# treatment as the other 5 classes, which the sheet's own columns don't).
#
# The block's header row isn't at a fixed row across Calculator versions (it
# sits on row 13 up to UP51b, row 15 in UP52 — two rows were added above it),
# so it's located by header text (see locate_header()): the row whose first cell
# is "Year" and whose second is "NATCropland" (the *names* row; the row above it
# holds the human-readable labels and also starts with "Year").
read_land_stock <- function(path, scenario_label) {
  h   <- locate_header(path, "LAND", c("Year", "NATCropland"))
  raw <- read_excel(path, sheet = "LAND", col_names = TRUE,
                    range = block_range(h, rows = h$n_rows, cols = h$n_cols))
  raw %>%
    mutate(scenario = scenario_label, Year = suppressWarnings(as.integer(Year))) %>%
    filter(!is.na(Year), Year >= 2000, Year <= 2050) %>%
    select(scenario, year = Year, Cropland, Pasture, Forest, OtherLand, Urban, NewForest) %>%
    pivot_longer(c(Cropland, Pasture, Forest, OtherLand, Urban, NewForest),
                 names_to = "class", values_to = "area")
}

df_luc_stock <- bind_rows(Map(read_land_stock, scenario_meta$path, scenario_meta$label))

# ── Read historical data ──────────────────────────────────────────────────────
df_hist <- read.csv(path_hist, check.names = FALSE, stringsAsFactors = FALSE)
colnames(df_hist) <- trimws(colnames(df_hist))
df_hist <- df_hist %>%
  mutate(year  = as.integer(trimws(as.character(year))),
         value = as.numeric(value))

# ── Read units (row 10, same structure in all files) ──────────────────────────
fable_units <- read_units(scenario_meta$path[1])

# ── Save ──────────────────────────────────────────────────────────────────────
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(df_scenarios,  "data/processed/df_scenarios.rds")
saveRDS(df_hist,       "data/processed/df_hist.rds")
saveRDS(fable_units,   "data/processed/fable_units.rds")
saveRDS(df_crops,      "data/processed/df_crops.rds")
saveRDS(df_livestock,  "data/processed/df_livestock.rds")
saveRDS(df_luc_matrix, "data/processed/df_luc_matrix.rds")
saveRDS(df_luc_stock,  "data/processed/df_luc_stock.rds")
message("Saved: df_scenarios.rds, df_hist.rds, fable_units.rds, df_crops.rds, df_livestock.rds, df_luc_matrix.rds, df_luc_stock.rds")

