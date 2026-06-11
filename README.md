# 🌿 FABLE Calculator — Scenario Comparison (Brazil)

An interactive R Shiny app to compare two [FABLE Calculator](https://fableconsortium.org/tools/fablecalculators/) land-use scenarios for Brazil against observed historical data.

## 📋 Overview

The FABLE Calculator is a spreadsheet-based land-use modelling tool developed by the FABLE Consortium to explore national pathways toward sustainable food and land-use systems. This project reads two Brazil-specific scenario files and overlays their outputs with historical reference data (2000–2020), enabling visual comparison across land-use classes and time periods.

| Scenario | Description |
|----------|-------------|
| **Current Trends** | Business-as-usual trajectory |
| **NDC Commitments** | Nationally Determined Contribution targets |

## ⚙️ Requirements

R (≥ 4.1) with the following packages:

| Package | Purpose |
|---------|---------|
| `readxl` | Read FABLE Calculator Excel files |
| `dplyr` | Data wrangling |
| `tidyr` | Data reshaping |
| `shiny` | Web app framework |
| `plotly` | Interactive charts |

Install all at once from the R console:

```r
install.packages(c("readxl", "dplyr", "tidyr", "shiny", "plotly"))
```

## 📁 Repository structure

```
fable-scenario-comparison-brazil/
├── data/
│   ├── csv/
│   │   └── histdatabrazil.csv               # Historical observations (1995–2020)
│   ├── images/
│   │   ├── fable_logo.png                   # Navbar logo
│   │   └── favicon.svg                      # Browser tab icon (Brazil flag)
│   └── xlsx/
│       ├── FABLECalculator_BRA_UP50_CurrentTrends.xlsx
│       └── FABLECalculator_BRA_UP50_NDC.xlsx
├── 01_process_data.R                        # Data ingestion and processing
├── 02_shiny_app.R                           # Shiny app logic
├── app.R                                    # Entry point (sources 02_shiny_app.R)
├── run.bat                                  # Windows one-click launcher
└── README.md
```

## 🗄️ Data sources

### Historical data (`histdatabrazil.csv`)

Long-format CSV with columns `type`, `source`, `year`, `value`, `unit`. Coverage: 1995–2020 (5-year steps).

| Category | Variables | Source |
|----------|-----------|--------|
| Land use | Forest, Cropland, Pastures, Secondary Forest, Urban, Other Land | MapBiomas, LAPIG, IBGE |
| Crop area | Soybean, Maize, Sugarcane, Permanent Crops | IBGE |
| Production | Ruminant Meat, Soybean, Maize, Sugarcane, Forest Products | FAOSTAT, IBGE |
| Emissions | CO₂, CH₄, N₂O (AFOLU, enteric fermentation, crop residues, etc.) | SEEG v13 |

### Scenario data (`xlsx/`)

Two tables are read from the `SCENATHON_report` sheet:

- **Aggregate table** (header row 11, `skip = 10`) — one row per 5-year time step (2000–2050), wide format with land-use, emissions and other aggregate variables.
- **Crop table** (header row 29, `skip = 28`) — long format with columns `Product`, `Year`, `ProdQ_feas` (production, 1000 t) and `FeasHarvarea` (harvested area, 1000 ha), one row per product per year.

## 🚀 Usage

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

## 📊 Shiny app — Land-use tab

The controls are displayed in this order:

| Control | Options |
|---------|---------|
| **Landuse Class** | Cropland · Pasture · Forest · Other Land · Urban |
| **Scenario** | Both · Current Trends · NDC Commitments |
| **Years** | Calibration & Projections (2000–2050) · Calibration (2000–2020) |
| **Chart type** | Line chart · Bar chart |

### Layout

Each view shows an interactive chart on the left and a data panel on the right, both inside the same row.

### Chart behaviour

- **Both** — single chart with all three series overlaid (Current Trends, NDC Commitments, Historical)
- **Current Trends / NDC Commitments** — single chart for the selected scenario + historical reference
- Both scenarios always share the same y-axis scale for direct comparison
- Hover over any point or bar to see the exact value (2 decimal places)
- Dashed vertical line at 2020 marks the calibration / projection boundary (line chart only, Calibration & Projections mode)

### Bar chart

When **Bar chart** is selected, bars are grouped by year with the same colour scheme as the line chart:

| Series | Fill | Border |
|--------|------|--------|
| Current Trends | Blue `#1565C0` | Black |
| NDC Commitments | Green `#009C3B` | Black |
| Historical | Near-black `#1a1a1a` | Black |

### Data tables (right panel)

The right panel always shows a **values table** with years as columns and one row per selected series (scenarios + historical). Historical values are left blank for years after 2020.

When **Calibration (2000–2020)** is selected, two additional tables appear beneath the values table:

| Table | Content |
|-------|---------|
| **Absolute Difference** | `abs(scenario − historical)` for each year, in the same unit as the chart (e.g. Mha) |
| **Relative Difference (%)** | `(scenario − historical) / historical × 100` for each year |

All values are shown with 2 decimal places. If historical data is unavailable for a given year, the difference cells are left blank.

Values in the **Relative Difference** table are colour-coded by magnitude:

| Range | Colour |
|-------|--------|
| \|val\| ≤ 10% | <font color="#009C3B">**Green**</font> |
| 10% < \|val\| ≤ 20% | <font color="#cc6600">**Orange**</font> |
| \|val\| > 20% | <font color="#cc0000">**Red**</font> |

### Visual encoding (line chart)

| Series | Colour |
|--------|--------|
| Current Trends | Blue `#1565C0` |
| NDC Commitments | Green `#009C3B` |
| Historical | Black `#000000` |

## 🌾 Shiny app — Crops tab

| Control | Options |
|---------|---------|
| **Crop** | Soybeans · Corn · Sugarcane |
| **Type** | Area · Production · Yield |
| **Scenario** | Both · Current Trends · NDC Commitments |
| **Years** | Calibration & Projections (2000–2050) · Calibration (2000–2020) |
| **Chart type** | Line chart · Bar chart |

### Metrics

| Type | Source | Unit | Notes |
|------|--------|------|-------|
| Area | `FeasHarvarea` (FABLE crop table) | Mha | Converted from 1 000 ha |
| Production | `ProdQ_feas` (FABLE crop table) | Mt | Converted from 1 000 t |
| Yield | Derived | t/ha | Production (Mt) ÷ Area (Mha) |

Historical reference comes from `histdatabrazil.csv` (IBGE). Layout, chart behaviour, data tables and colour encoding are identical to the Land-use tab, with the y-axis label updating per metric (Area (Mha) · Production (Mt) · Yield (t/ha)).

## 🌫️ Shiny app — Emissions tab

| Control | Options |
|---------|---------|
| **Emission** | CO2 AFOLU · CH4 Enteric Fermentation · CH4 Rice · N2O from Agriculture |
| **Scenario** | Both · Current Trends · NDC Commitments |
| **Years** | Calibration & Projections (2000–2050) · Calibration (2000–2020) |
| **Chart type** | Line chart · Bar chart |

All values are in **MtCO2e**. Historical data from SEEG v13 is reported in million tonnes of the respective gas and converted to CO2e using IPCC AR6 GWP100 factors. FABLE Calculator columns are already in MtCO2e.

### Emission types — source mapping

| Emission | FABLE Calculator | Historical (SEEG v13) | Conversion |
|----------|------------------|-----------------------|------------|
| **CO2 AFOLU** | `CalcAllLandCO2e` | `CO2 AFOLU` | × 1 (already CO2e) |
| **CH4 Enteric Fermentation** | `CalcLiveCH4` | `CH4 Enteric Fermentation` | × 27.2 (GWP100 AR6) |
| **CH4 Rice** | `CalcCropCH4` | `CH4 Rice` | × 27.2 (GWP100 AR6) |
| **N2O from Agriculture** | `CalcLiveN2O` + `CalcCropN2O` | Sum of 8 subcategories (see below) | × 273 (GWP100 AR6) |

#### N2O from Agriculture — aggregation

The FABLE Calculator value is the **sum of two columns**: `CalcLiveN2O` (livestock N2O) and `CalcCropN2O` (crop N2O), both already in MtCO2e.

The historical series aggregates the following SEEG v13 subcategories and multiplies the total by 273:

- N2O Animal Waste Management
- N2O Burning of Crop Residues
- N2O Decay of Crop Residues
- N2O Inorganic Fertilizers
- N2O Manure Applied to Croplands
- N2O Pasture
- N2O Peatland
- N2O Soil Organic Matter Loss

#### Note on CO2 AFOLU time range

Charts and tables for **CO2 AFOLU start at 2005** (not 2000). The Calculator reports a value of 0 for the year 2000 in this column, which reflects a model calibration boundary rather than an observed or projected emission, so that data point is suppressed.

## 🐄 Shiny app — Livestock tab

| Control | Options |
|---------|---------|
| **Product** | Beef · Milk · Chicken · Pork |
| **Scenario** | Both · Current Trends · NDC Commitments |
| **Years** | Calibration & Projections (2000–2050) · Calibration (2000–2020) |
| **Chart type** | Line chart · Bar chart |

All values are in **Mt** (million tonnes). Source: `ProdQ_feas` column from the FABLE crop table, converted from 1 000 t.

### Historical reference

Only **Beef** has a historical series (Ruminant Meat, FAOSTAT). For Milk, Chicken and Pork the chart shows scenario lines only; the values table omits the Historical row and no difference tables are shown even in Calibration mode.

| Product | FABLE product | Historical source |
|---------|--------------|-------------------|
| Beef | `beef` | Ruminant Meat — FAOSTAT |
| Milk | `milk` | — |
| Chicken | `chicken` | — |
| Pork | `pork` | — |

## 🚢 Shiny app — Trade tab

| Control | Options |
|---------|---------|
| **Type** | Exports · Imports |
| **Product** | Depends on Type (see below) |
| **Scenario** | Both · Current Trends · NDC Commitments |
| **Years** | Calibration & Projections (2000–2050) · Calibration (2000–2020) |
| **Chart type** | Line chart · Bar chart |

The Product selector updates automatically when Type changes. All values are in **Mt** (million tonnes), converted from the `Export_quantity` / `Import_quantity` columns in the FABLE crop table (1 000 t).

No historical data is available for any trade variable — the chart shows FABLE scenario lines only and the right panel shows a values table with no difference tables.

### Products per type

| Type | Products | FABLE product key |
|------|----------|-------------------|
| Exports | Soybeans | `soyabean` |
| Exports | Corn | `corn` |
| Exports | Beef | `beef` |
| Imports | Wheat | `wheat` |
