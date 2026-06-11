library(readxl)
library(dplyr)
library(tidyr)

path_ct   <- "data/xlsx/FABLECalculator_BRA_UP50_CurrentTrends.xlsx"
path_ndc  <- "data/xlsx/FABLECalculator_BRA_UP50_NDC.xlsx"
path_hist <- "data/csv/histdatabrazil.csv"

# ── Read SCENATHON_report ─────────────────────────────────────────────────────
# Row 10 = units; row 11 = column headers; data rows start at row 12.
# Each row = one year (2000, 2005, ..., 2050), columns = output variables.

read_units <- function(path) {
  unit_row     <- read_excel(path, sheet = "SCENATHON_report", skip = 9, n_max = 1, col_names = FALSE)
  header_names <- names(read_excel(path, sheet = "SCENATHON_report", skip = 10, n_max = 0))
  unit_vals    <- trimws(as.character(unlist(unit_row[1, ])))
  length(unit_vals) <- length(header_names)  # pad with NA if row is shorter
  setNames(unit_vals, header_names)
}

read_scenathon <- function(path, scenario_label) {
  raw <- read_excel(path, sheet = "SCENATHON_report", skip = 10)
  names(raw) <- trimws(names(raw))

  year_col <- names(raw)[grepl("^year$", names(raw), ignore.case = TRUE)][1]
  if (is.na(year_col)) stop("Year column not found in: ", path)

  raw %>%
    rename(Year = all_of(year_col)) %>%
    mutate(Year = suppressWarnings(as.integer(Year))) %>%
    filter(!is.na(Year), Year >= 2000, Year <= 2050) %>%
    mutate(scenario = scenario_label)
}

df_ct  <- read_scenathon(path_ct,  "Current Trends")
df_ndc <- read_scenathon(path_ndc, "NDC Commitments")

# Print all column names once (helpful for mapping variables)
message("── SCENATHON_report columns ──────────────────────────────────")
print(names(df_ct))

df_scenarios <- bind_rows(df_ct, df_ndc)

# ── Read crop table (second table in SCENATHON_report, header at row 29) ──────
# Columns used: B=Product, C=Year, G=ProdQ_feas, H=FeasHarvarea
read_crop_table <- function(path, scenario_label) {
  raw        <- read_excel(path, sheet = "SCENATHON_report", skip = 28)
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

df_crops <- bind_rows(
  read_crop_table(path_ct,  "Current Trends"),
  read_crop_table(path_ndc, "NDC Commitments")
)

message("── Crop products found ───────────────────────────────────────")
print(sort(unique(df_crops$Product)))

# ── Read cattle herd/density table (5_feas_livestock, header at row 29) ───────
# Filter ANIMAL == "cattle"; two rows per year (BOVO + BOVD) → sum FeasHerd,
# first RumDensity (identical across sub-types within a year).
read_livestock_table <- function(path, scenario_label) {
  # Sheet has multiple side-by-side tables causing duplicate column names.
  # Read only the first 5 columns (A:E) and assign names explicitly.
  raw <- read_excel(path, sheet = "5_feas_livestock", skip = 28,
                    col_names = FALSE, col_types = "text")
  raw <- raw[, 1:5]
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

df_livestock <- bind_rows(
  read_livestock_table(path_ct,  "Current Trends"),
  read_livestock_table(path_ndc, "NDC Commitments")
)

# ── Read historical data ──────────────────────────────────────────────────────
df_hist <- read.csv(path_hist, check.names = FALSE, stringsAsFactors = FALSE)
colnames(df_hist) <- trimws(colnames(df_hist))
df_hist <- df_hist %>%
  mutate(year  = as.integer(trimws(as.character(year))),
         value = as.numeric(value))

# ── Read units (row 10, same structure in both files) ─────────────────────────
fable_units <- read_units(path_ct)

# ── Save ──────────────────────────────────────────────────────────────────────
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(df_scenarios,  "data/processed/df_scenarios.rds")
saveRDS(df_hist,       "data/processed/df_hist.rds")
saveRDS(fable_units,   "data/processed/fable_units.rds")
saveRDS(df_crops,      "data/processed/df_crops.rds")
saveRDS(df_livestock,  "data/processed/df_livestock.rds")
message("Saved: df_scenarios.rds, df_hist.rds, fable_units.rds, df_crops.rds, df_livestock.rds")
