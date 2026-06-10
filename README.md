# FABLE Calculator — Historical Comparisons (Brazil)

An R/Shiny tool to compare two [FABLE Calculator](https://fableconsortium.org/tools/fablecalculators/) scenarios for Brazil against observed historical data.

## Overview

The FABLE Calculator is a spreadsheet-based land-use modelling tool developed by the FABLE Consortium to explore national pathways toward sustainable food and land-use systems. This project reads two Brazil-specific scenario files and overlays their outputs with historical reference data (1995–2020), enabling side-by-side visual comparison.

| Scenario | Description |
|----------|-------------|
| **Current Trends** | Business-as-usual trajectory |
| **NDC Commitments** | Nationally Determined Contribution targets |

## Repository structure

```
HistoricalComparisons/
├── data/
│   ├── csv/
│   │   └── histdatabrazil.csv          # Historical observations (1995–2020)
│   └── xlsx/
│       ├── FABLECalculator_BRA_UP50_CurrentTrends.xlsx
│       └── FABLECalculator_BRA_UP50_NDC.xlsx
├── 01_process_data.R                   # Data ingestion and processing
├── 02_shiny_app.R                      # R Shiny comparison app
└── README.md
```

## Data sources

### Historical data (`histdatabrazil.csv`)

Long-format CSV with columns `type`, `source`, `year`, `value`, `unit`. Coverage: 1995–2020 (5-year steps).

| Category | Variables | Source |
|----------|-----------|--------|
| Land use | Forest, Cropland, Pastures, Secondary Forest, Urban, Other Land | MapBiomas, LAPIG, IBGE |
| Crop area | Soybean, Maize, Sugarcane, Permanent Crops | IBGE |
| Production | Ruminant Meat, Soybean, Maize, Sugarcane, Forest Products | FAOSTAT, IBGE |
| Emissions | CO₂, CH₄, N₂O (AFOLU, enteric fermentation, crop residues, etc.) | SEEG v13 |

### Scenario data (`xlsx/`)

Read from the `SCENATHON_report` sheet (column headers at row 11, `skip = 10`). Each row represents one 5-year time step from 2000 to 2050.

## Requirements

```r
install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "shiny"))
```

## Usage

**Terminal (one command)**

```bash
Rscript app.R
```

Or double-click `run.bat` on Windows. Both work from any directory — the app auto-detects its own location and opens the browser automatically.

**RStudio**

Double-click `HistoricalComparisons.Rproj`, then run `shiny::runApp()` in the console, or open `app.R` and click the **Run App** button.

On first run, `01_process_data.R` executes automatically and saves processed files to `data/processed/`. Subsequent launches skip that step and load from cache.

To force a full re-read of the source Excel and CSV files:

```r
source("01_process_data.R")
```

## Shiny app

**Charts tab** — side-by-side line charts for each selected variable:

- **Left panel**: Current Trends scenario (blue)
- **Right panel**: NDC Commitments scenario (green)
- **Black line**: observed historical data from `histdatabrazil.csv`
- **Dashed vertical line** at 2020 marks the boundary between observed and projected values

## License

To be defined.
