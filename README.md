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
| `bslib` | Bootstrap 5 theming |
| `terra` | Raster processing and map generation |
| `RColorBrewer` | Colour palettes for maps |
| `ggplot2` | Static charts (report only) |
| `officer` | Word document generation (report only) |
| `flextable` | Formatted tables in Word (report only) |

Install all at once from the R console:

```r
install.packages(c("readxl", "dplyr", "tidyr", "shiny", "plotly", "bslib",
                   "terra", "RColorBrewer",
                   "ggplot2", "officer", "flextable"))
```

## ▶️ One-click launchers

Double-click the file for your operating system to start the app directly — no terminal needed:

| OS | File | Notes |
|----|------|-------|
| **Windows** | `open_app_windows.bat` | Double-click in File Explorer |
| **macOS** | `open_app_mac.command` | First time: right-click → Open (Gatekeeper). After that, double-click works. If it still doesn't run, open a terminal in the project folder and run `chmod +x open_app_mac.command` once. |
| **Ubuntu** | `open_app_ubuntu.sh` | In Nautilus: Edit → Preferences → Behaviour → "Run executable text files when they are opened". Then double-click. Alternatively, right-click → Run as a Program. If needed, run `chmod +x open_app_ubuntu.sh` once in a terminal first. |

All three launchers auto-detect their own location and open the browser automatically on first run. If the downscaling maps have not been generated yet, the launcher runs `04_generate_maps.R` automatically before opening the app (this takes a few minutes on first run).

## 📂 Input data files

| File | Location | Description |
|------|----------|-------------|
| FABLE Calculator — Current Trends | `data/xlsx/` | FABLE Calculator spreadsheet (Current Trends pathway) |
| FABLE Calculator — NDC Commitments | `data/xlsx/` | FABLE Calculator spreadsheet (NDC Commitments pathway) |
| Historical reference data | `data/csv/` | Long-format CSV (`histdatabrazil.csv`) with observed data for Brazil |
| LUC transition matrix — CT | `data/luc/` | Downscaled land-use change data for Current Trends (`downscaled_LUC_brazil_all_ct.rds`) |
| LUC transition matrix — NDC | `data/luc/` | Downscaled land-use change data for NDC Commitments (`downscaled_LUC_brazil_all_ndc.rds`) |
| Cell ID raster | `data/luc/` | `id_raster.tif` — maps FABLE cell IDs to a 0.05° raster grid |
| State and biome boundaries | `data/shapefiles/` | `br_states.shp`, `br_biomes.shp` — shapefile overlays for maps |

File names for the FABLE Calculator spreadsheets are configured at the top of `01_process_data.R`.

## 📁 Repository structure

```
fable-scenario-comparison-brazil/
├── data/
│   ├── csv/
│   │   └── histdatabrazil.csv               # Historical observations (1995–2020)
│   ├── images/
│   │   ├── fable_logo.png                   # Navbar logo
│   │   └── favicon.svg                      # Browser tab icon (Brazil flag)
│   ├── luc/
│   │   ├── downscaled_LUC_brazil_all_ct.rds # LUC transition matrix — Current Trends
│   │   ├── downscaled_LUC_brazil_all_ndc.rds# LUC transition matrix — NDC Commitments
│   │   └── id_raster.tif                    # Cell ID raster (0.05° resolution)
│   ├── maps/                                # Auto-generated PNGs (gitignored)
│   │   ├── ct/
│   │   │   ├── landcover/
│   │   │   └── transitions/
│   │   └── ndc/
│   │       ├── landcover/
│   │       └── transitions/
│   ├── shapefiles/
│   │   ├── br_states.shp                    # Brazilian state boundaries
│   │   └── br_biomes.shp                    # Brazilian biome boundaries
│   └── xlsx/
│       ├── FABLECalculator_BRA_UP50_CurrentTrends.xlsx
│       └── FABLECalculator_BRA_UP50_NDC.xlsx
├── 01_process_data.R                        # Data ingestion and processing
├── 02_shiny_app.R                           # Shiny app logic
├── 03_generate_report.R                     # Word report generator
├── 04_generate_maps.R                       # PNG map generator (downscaling)
├── app.R                                    # Entry point (sources 02_shiny_app.R)
├── open_app_windows.bat                     # Windows one-click launcher
├── open_app_mac.command                     # macOS one-click launcher
├── open_app_ubuntu.sh                       # Ubuntu one-click launcher
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

## 📄 Word report

`03_generate_report.R` generates a static Word document (`FABLE_Report_BRA_v50.docx`) with:

- **Cover page** — title, subtitle, date
- **Table of Contents** — auto-generated Word TOC field (update with Ctrl+A → F9 in Word)
- **32 content pages** (landscape, one per variable) — each contains a line chart comparing Current Trends vs NDC Commitments (+ Historical where available), followed by a values table for 2000–2050

Sections: Land Use (5) · Emissions (4) · Crops (9) · Livestock (6) · Trade (7) · Food (1)

Run from the project root:

```r
source("03_generate_report.R")
```

Or from the terminal:

```bash
Rscript 03_generate_report.R
```

Requires `ggplot2`, `officer`, and `flextable` in addition to the packages used by the Shiny app.

## 📥 Downloading data

Every tab has a **CSV** button next to the values table header. Clicking it downloads the currently displayed table (selected variable, scenario, and year range) as a `.csv` file. The filename encodes the current selection, e.g. `landuse_Forest_Both_2050.csv`.

## 🚀 Usage

**Terminal (one command)**

```bash
Rscript app.R
```

Or use one of the one-click launchers (`open_app_windows.bat`, `open_app_mac.command`, `open_app_ubuntu.sh`). All work from any directory — the app auto-detects its own location and opens the browser automatically.

**RStudio**

Double-click `HistoricalComparisons.Rproj`, then run `shiny::runApp()` in the console, or open `app.R` and click the **Run App** button.

On first run, `01_process_data.R` executes automatically and saves processed files to `data/processed/`. Subsequent launches load from cache.

To force a full re-read of the source files:

```r
source("01_process_data.R")
```

## 🎨 UI theme

The app uses **Bootstrap 5** via the `bslib` package. The navbar displays in two rows — app title on the first line and tab names on the second — with a turquoise background (`#007B8A`) and white text. Interactive elements (active tabs, focused inputs, buttons) use the same turquoise accent.

Controls are grouped in a **sidebar panel** on the left of each tab. Scenario, Years, and Chart type are shown as radio buttons; the variable selector (land-use class, emission type, crop, etc.) uses a dropdown. Sidebar labels are bold without trailing colons. A **Start y-axis at zero** checkbox controls whether the y-axis is forced to start at zero (checked by default) or auto-scaled to the data range. For CO₂ AFOLU, the checkbox is unchecked by default and resets automatically when switching to that emission, since its values can be negative.

## 📊 Shiny app — Land Use tab

The controls are displayed in this order:

| Control | Type | Options |
|---------|------|---------|
| **Landuse Class** | Dropdown | Cropland · Pasture · Forest · Other Land · Urban |
| **Scenario** | Radio | Both · Current Trends · NDC Commitments |
| **Years** | Radio | Calibration & Projections (2000–2050) · Calibration (2000–2020) |
| **Chart type** | Radio | Line chart · Bar chart |

### Layout

The chart always appears on the left. The data table position depends on the Years selection:

- **Calibration & Projections** — table appears below the chart at full width, so all years (2000–2050) are visible without horizontal scrolling
- **Calibration** — table appears to the right of the chart (2000–2020 fits comfortably in the available space)

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
| **CO₂ AFOLU** | `CalcAllLandCO2e` | `CO2 AFOLU` | × 1 (already CO2e) |
| **CH₄ Enteric Fermentation** | `CalcLiveCH4` | `CH4 Enteric Fermentation` | × 27.2 (GWP100 AR6) |
| **CH₄ Rice** | `CalcCropCH4` | `CH4 Rice` | × 27.2 (GWP100 AR6) |
| **N₂O from Agriculture** | `CalcLiveN2O` + `CalcCropN2O` | Sum of 8 subcategories (see below) | × 273 (GWP100 AR6) |

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

#### Note on CO2 AFOLU time range and negative values

Charts and tables for **CO2 AFOLU start at 2005** (not 2000). The Calculator reports a value of 0 for the year 2000 in this column, which reflects a model calibration boundary rather than an observed or projected emission, so that data point is suppressed.

CO2 AFOLU values can go negative (net carbon sink). When they do, the y-axis extends below zero and a dashed horizontal line is drawn at y = 0 for reference. This zero line is shown only on the CO2 AFOLU chart.

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

| Type | Products | FABLE product key(s) |
|------|----------|----------------------|
| Exports | Soybeans (all) | `soyabean` + `soycake` + `soyoil` (sum) |
| Exports | Soybeans (grain) | `soyabean` |
| Exports | Soybeans (cake) | `soycake` |
| Exports | Soybeans (oil) | `soyoil` |
| Exports | Corn | `corn` |
| Exports | Beef | `beef` |
| Imports | Wheat | `wheat` |

## 🐄 Shiny app — Livestock tab (additional variables)

In addition to production (Mt) for Beef, Milk, Chicken and Pork, the Livestock tab includes two cattle-specific variables sourced from the `5_feas_livestock` sheet:

| Variable | Source column | Unit | Data source |
|----------|--------------|------|-------------|
| Cattle Herd | `FeasHerd` | Million TLU | `5_feas_livestock`, cattle rows summed (BOVO + BOVD), converted from 1 000 TLU |
| Cattle Stocking Rate | `RumDensity` | TLU/ha | `5_feas_livestock`, cattle rows (same value across sub-types, first taken) |

Neither variable has a historical series — the chart shows scenario lines only with no difference tables.

## 🌎 Shiny app — Maps tab

Displays static PNG maps generated from the FABLE downscaling model for both scenarios side by side.

| Control | Options |
|---------|---------|
| **Map Type** | Land Cover · Outflows · Transitions |
| **Class / Transition** | Depends on Map Type (see below) |
| **Year** | 2020 · 2025 · 2030 · 2035 · 2040 · 2045 · 2050 |

The year selector appears as a navigation bar above the maps with `◀` / `▶` arrow buttons. Current Trends is shown on the left, NDC Commitments on the right. Images are sized to fit the browser viewport without scrolling.

### Map types

| Type | Description | Classes / pairs |
|------|-------------|-----------------|
| **Land Cover** | Total area per class per year | Forest · Cropland · Pasture · OtherLand · Urban |
| **Outflows** | Total area lost from each class | Forest · Cropland · Pasture · OtherLand · Urban |
| **Transitions** | Area converted between two specific classes | Forest→Cropland · Forest→Pasture · Cropland→Forest · Pasture→Cropland · OtherLand→Cropland |

Maps are generated by `04_generate_maps.R` and saved to `data/maps/` (gitignored). The launchers regenerate them automatically if missing. To regenerate manually:

```bash
Rscript 04_generate_maps.R          # both scenarios
Rscript 04_generate_maps.R ct       # Current Trends only
Rscript 04_generate_maps.R ndc      # NDC Commitments only
```

> Interactive (Leaflet-based) maps are planned for a future branch (`downscaling-dynamic`).

## 🍽️ Shiny app — Food tab

| Control | Options |
|---------|---------|
| **Variable** | Food Consumption |
| **Scenario** | Both · Current Trends · NDC Commitments |
| **Years** | Calibration & Projections (2000–2050) · Calibration (2000–2020) |
| **Chart type** | Line chart · Bar chart |

Values are in **kcal/cap/day**. Source: `kcal_feas` column from the aggregate SCENATHON_report table. No historical data — the chart shows scenario lines only and the right panel shows a values table rounded to whole numbers.

## 📄 Generating the Word report

> **Experimental** — this feature is still under development and may produce incomplete or inconsistent output.

`03_generate_report.R` generates a static Word document (`FABLE_Report_BRA_v50.docx`) with one landscape page per variable, each containing a line chart (Current Trends vs NDC Commitments + Historical where available) and a values table for 2000–2050.

Additional packages required (beyond the Shiny app):

```r
install.packages(c("ggplot2", "officer", "flextable"))
```

Run from the project root:

```r
source("03_generate_report.R")
```

Or from the terminal:

```bash
Rscript 03_generate_report.R
```

The output file is saved in the project root. To update the table of contents in Word, press **Ctrl+A → F9**.

---

👤 **Author**
Wanderson Costa (wcosta.comp@gmail.com)

🤖 This application was developed with the assistance of [Claude Code](https://claude.ai/code).
