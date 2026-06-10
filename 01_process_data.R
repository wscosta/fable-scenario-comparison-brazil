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
saveRDS(df_scenarios, "data/processed/df_scenarios.rds")
saveRDS(df_hist,      "data/processed/df_hist.rds")
saveRDS(fable_units,  "data/processed/fable_units.rds")
message("Saved: df_scenarios.rds, df_hist.rds, fable_units.rds")
