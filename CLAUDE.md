# FABLE Calculator — Historical Comparisons

## Project goal

Compare two FABLE Calculator scenarios for Brazil side by side, overlaid with observed historical data (1995–2020). Outputs are two R files: a data-processing script and an R Shiny app.

The FABLE Calculator is a spreadsheet-based land-use modeling tool developed by the FABLE Consortium to explore pathways for sustainable food and land-use systems. See https://fableconsortium.org/tools/fablecalculators/ for context.

## Scenarios

| File | Scenario label |
|------|---------------|
| `data/xlsx/FABLECalculator_BRA_UP50_CurrentTrends.xlsx` | Current Trends |
| `data/xlsx/FABLECalculator_BRA_UP50_NDC.xlsx` | NDC Commitments |

Both are Brazil-specific adaptations (`BRA`) with the UP50 calibration.

## Historical reference data

`data/csv/histdatabrazil.csv` — long-format CSV, columns: `type`, `source`, `year`, `value`, `unit`.

Coverage: 1995, 2000, 2005, 2010, 2015, 2020.

### Variable groups

**Land use (million ha)**
- Pastures and Rangelands — LAPIG
- Forest — Mapbiomas; also FAOSTAT
- Secondary Forest — Mapbiomas
- Other Land — Mapbiomas
- Cropland — IBGE; also FAOSTAT (Production and Landuse variants)
- Permanent Crops — IBGE
- Temporary Crops — IBGE
- Soybean Area, Maize Area, Sugarcane Area — IBGE
- Agriculture (total) — IBGE + LAPIG
- Urban — Mapbiomas

**Production (million tonnes or Mt DM/yr)**
- Ruminant Meat — FAOSTAT
- Soybean Production, Maize Production, Sugarcane Production — IBGE
- Forest Products, Industrial Roundwood, Wood Fuel — IBGE

**Emissions (million tonnes)**
- CO2 AFOLU, N2O AFOLU, CH4 AFOLU — SEEG13
- CH4: Rice, Enteric Fermentation, Animal Waste Management, Burning of Crop Residues — SEEG13
- N2O: Animal Waste Management, Burning of Crop Residues, Decay of Crop Residues, Inorganic Fertilizers, Manure Applied to Croplands, Pasture, Peatland, Soil Organic Matter Loss — SEEG13

## SCENATHON_report sheet structure

- **76 columns (A–BX), ~1 043 rows total**
- Rows 1–10: metadata / section headers
- **Row 11: column headers** → read with `skip = 10`
- **Column A**: Location ("Brazil")
- **Column B**: Year (2000, 2005, 2010, …, 2050)
- Columns C–BX: output variables (GDP, Population, emissions, land-use areas, production quantities, etc.)
- Cropland area: likely column `FeasPlantarea` (feasible planted area) — confirmed at runtime via grepl search

## Planned file structure

```
HistoricalComparisons/
├── CLAUDE.md
├── data/
│   ├── csv/histdatabrazil.csv
│   └── xlsx/
│       ├── FABLECalculator_BRA_UP50_CurrentTrends.xlsx
│       └── FABLECalculator_BRA_UP50_NDC.xlsx
├── 01_process_data.R      # reads, cleans, and reshapes all sources
└── 02_shiny_app.R         # Shiny app (or app/ folder with ui.R + server.R)
```

## Script 1 — `01_process_data.R`

Responsibilities:
- Read both Excel files and extract the relevant output sheets/tables
- Read `histdatabrazil.csv`
- Align variable names between FABLE Calculator outputs and historical CSV types
- Produce tidy long-format data frames ready for plotting:
  - `df_hist` — historical observations
  - `df_scenarios` — both scenarios stacked with a `scenario` column (`"Current Trends"`, `"NDC Commitments"`)
- Save intermediate `.rds` files if needed for Shiny performance

## Script 2 — `02_shiny_app.R` (or `app/`)

### Layout

Side-by-side panel:
- **Left panel**: Current Trends scenario
- **Right panel**: NDC Commitments scenario
- Both panels show the same selected variable; scenario curves + historical reference line

### Chart design

Each chart:
- Scenario line/ribbon from FABLE Calculator (2000–2050, 5-year time steps)
- Historical reference line in a neutral color (e.g., dark grey) from `df_hist`, ending at 2020
- Clear axis labels with units from the CSV
- A vertical dashed line at 2020 to mark the historical/projection boundary

### UI controls

- Variable selector (dropdown or radio): pick which `type` to display
- Option to show/hide historical data
- Potentially: overlay both scenarios on a single chart (toggle)

## Key R packages (expected)

- `readxl` — read FABLE Calculator Excel files
- `dplyr`, `tidyr` — data wrangling
- `ggplot2` — charts
- `shiny`, `bslib` or `shinydashboard` — app layout
- `patchwork` or `cowplot` — if side-by-side plots are built outside Shiny layout

## Development notes

- The Excel structure of FABLE Calculator files needs to be explored first (`readxl::excel_sheets()`) to understand which sheets hold scenario output data before building the extraction logic.
- Variable name mapping between FABLE output columns and historical CSV `type` values will need a lookup table.
- Historical data has multiple sources for the same variable (e.g., Cropland from IBGE and FAOSTAT); the processing script should allow choosing a preferred source per variable or display all sources.
- Code will be published on GitHub — keep scripts self-contained and reproducible (relative paths from the project root).
