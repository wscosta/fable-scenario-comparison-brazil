# FABLE Calculator — Scenario Comparison (Brazil)

An interactive R Shiny app to compare two [FABLE Calculator](https://fableconsortium.org/tools/fablecalculators/) land-use scenarios for Brazil against observed historical data.

## Overview

The FABLE Calculator is a spreadsheet-based land-use modelling tool developed by the FABLE Consortium to explore national pathways toward sustainable food and land-use systems. This project reads two Brazil-specific scenario files and overlays their outputs with historical reference data (2000–2020), enabling visual comparison across land-use classes and time periods.

| Scenario | Description |
|----------|-------------|
| **Current Trends** | Business-as-usual trajectory |
| **NDC Commitments** | Nationally Determined Contribution targets |

## Repository structure

```
fable-scenario-comparison-brazil/
├── data/
│   ├── csv/
│   │   └── histdatabrazil.csv               # Historical observations (1995–2020)
│   └── xlsx/
│       ├── FABLECalculator_BRA_UP50_CurrentTrends.xlsx
│       └── FABLECalculator_BRA_UP50_NDC.xlsx
├── 01_process_data.R                        # Data ingestion and processing
├── 02_shiny_app.R                           # Shiny app logic
├── app.R                                    # Entry point (sources 02_shiny_app.R)
├── run.bat                                  # Windows one-click launcher
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

Read from the `SCENATHON_report` sheet (column headers at row 11, `skip = 10`). Each row is a 5-year time step from 2000 to 2050.

## Requirements

```r
install.packages(c("readxl", "dplyr", "tidyr", "shiny", "plotly"))
```

## Usage

**Terminal (one command)**

```bash
Rscript app.R
```

Or double-click `run.bat` on Windows. Both work from any directory — the app auto-detects its own location and opens the browser automatically.

**RStudio**

Double-click `HistoricalComparisons.Rproj`, then run `shiny::runApp()` in the console, or open `app.R` and click the **Run App** button.

On first run, `01_process_data.R` executes automatically and saves processed files to `data/processed/`. Subsequent launches load from cache.

To force a full re-read of the source files:

```r
source("01_process_data.R")
```

## Shiny app — Land-use tab

The app has three filter controls:

| Control | Options |
|---------|---------|
| **Scenario** | Both · Current Trends · NDC Commitments |
| **Years** | Calibration & Projections (2000–2050) · Calibration (2000–2020) |
| **Landuse Class** | Cropland · Pasture · Forest · Other Land · Urban |

### Chart behaviour

- **Both** — single chart with all three series overlaid
- **Current Trends / NDC Commitments** — single chart for the selected scenario
- Historical reference line shown only when data is available for the selected class
- Both scenarios always share the same y-axis scale for direct comparison
- Hover over any marker to see the exact value (2 decimal places)
- Dashed vertical line at 2020 marks the calibration / projection boundary

### Visual encoding

| Series | Colour |
|--------|--------|
| Current Trends | Blue `#1565C0` |
| NDC Commitments | Green `#009C3B` (Brazilian flag) |
| Historical | Black `#000000` |

## License

To be defined.
