# 🌿 FABLE Calculator — Scenario Comparison (Brazil)

An interactive app for exploring how Brazil's land use, agriculture and emissions could evolve up to 2050 under different scenarios — and how those scenarios compare with what has actually been observed since the 1990s.

## 📋 What is this?

The [FABLE Calculator](https://fableconsortium.org/tools/fablecalculators/) is a modelling tool developed by the FABLE Consortium to explore pathways toward sustainable food and land-use systems. This app takes the Calculator's results for Brazil and lets you compare several scenarios side by side, next to observed historical data — without opening a single spreadsheet.

Everything is interactive: switch scenarios on and off, hover over a chart for exact values, change the time window, and download any table.

### The scenarios

There are two kinds of pathway:

- **Current Trends** — business as usual: what happens if today's trends continue.
- **NDC Commitments** — what happens if Brazil delivers on its climate commitments under the Paris Agreement (its *Nationally Determined Contribution*).

Each pathway comes in several **calibrations** (UP46, UP48, UP50, UP51, UP51a, UP51b). A calibration is a version of the model's setup, refined over time — a higher number (and, within the same number, a later letter) means a more recent one. **UP51b is the newest.** UP51a was UP51 with an updated land-use baseline (MapBiomas Collection 10 and IBGE).

| Calibration | Current Trends | NDC Commitments |
|-------------|:--------------:|:---------------:|
| UP51b | ✅ | ✅ |
| UP51a | ✅ | ✅ |
| UP51 | ✅ | ✅ |
| UP50 | ✅ | ✅ |
| UP48 | ✅ | ✅ |
| UP46 | ✅ | — |

When the app opens, only the newest calibration (UP51b) is switched on; every other scenario is one click away.

### Quick glossary

| Term | Meaning |
|------|---------|
| **Calibration** | The period (2000–2020) in which the model is checked against observed data. After 2020 the lines are projections. |
| **Mha** | Million hectares |
| **Mt** | Million tonnes |
| **MtCO₂e** | Million tonnes of CO₂ equivalent — all greenhouse gases expressed in one common unit |
| **AFOLU** | Agriculture, Forestry and Other Land Use |
| **NDC** | Nationally Determined Contribution — a country's climate commitment under the Paris Agreement |

## ⚙️ Requirements and installation

The app runs on **R, version 4.1 or newer** (a free download from [r-project.org](https://www.r-project.org/); [RStudio](https://posit.co/download/rstudio-desktop/) is optional but handy). On top of R you need a few add-on packages, installed once:

| Package | What it is used for |
|---------|---------------------|
| `shiny` | The web app itself |
| `bslib` | The app's look and feel |
| `plotly` | Interactive charts |
| `chorddiag` | The Chord diagram on the Land Use Change tab |
| `readxl` | Reading the FABLE Calculator Excel files |
| `dplyr`, `tidyr` | Preparing the data |
| `terra`, `RColorBrewer` | Building the Maps tab's maps |
| `ggplot2`, `officer`, `flextable` | Only for the optional Word report |

Paste this into the R console (the last line installs `chorddiag`, which is only available from GitHub):

```r
install.packages(c("readxl", "dplyr", "tidyr", "shiny", "plotly", "bslib",
                   "terra", "RColorBrewer", "ggplot2", "officer", "flextable"))
remotes::install_github("mattflor/chorddiag")
```

The launchers below do not install packages for you, so do this step first.

## ▶️ Opening the app

Double-click the file for your computer — no terminal needed. The app opens in your web browser.

| System | File |
|--------|------|
| **Windows** | `open_app_windows.bat` |
| **macOS** | `open_app_mac.command` (first time: right-click → Open) |
| **Ubuntu** | `open_app_ubuntu.sh` |

The very first launch prepares the data and maps, which can take a few minutes. After that it opens quickly.

## 🧭 A tour of the app

The app has ten tabs along the top. On each one, the controls are in the panel on the left.

**Controls you will see on most tabs**

- **Scenario** — one on/off switch per scenario. Any number can be on at the same time. The selection is shared between the chart tabs, so a scenario you switch on in one tab is on in the others too. *(The two map tabs have their own controls.)*
- **Years** — the **Calibration Only** switch shows 2000–2020 (model vs. observed data); off shows the full 2000–2050.
- **Chart type** — the small icon button switches between line, bar and area charts.
- **Start y-axis at zero** — tick it to start the vertical axis at zero, untick it to zoom in on the data.
- **CSV** — the button next to a table downloads exactly what is on screen.

### 🌿 Land Use

How much land each class occupies over time: **Cropland, Pasture, Forest, Other Land and Urban**. Each selected scenario is drawn as a line, with the observed historical series on top (black).

When you tick **Calibration Only**, two extra tables appear showing how far each scenario is from the observed data — in absolute terms and as a percentage. The percentage is colour-coded: <font color="#009C3B">**green**</font> up to 10% off, <font color="#cc6600">**orange**</font> up to 20%, <font color="#cc0000">**red**</font> beyond that.

*Other Land* includes land that was recently converted to "other" uses, so it can grow substantially in the projections.

### 🔄 Land Use Change

Instead of how much land each class has, this tab shows **where land moves to**. Pick a period with the slider and a diagram type:

- **Sankey** *(default)* — a flow diagram: classes on the left at the start of the period, on the right at the end, with a ribbon for every conversion.
- **Chord** — a circular diagram where each class is an arc and each conversion a band between arcs.
- **Stacked Bar** — one bar per 5 years showing the net gain (above zero) or loss (below zero) of each class.

Sankey and Chord also show land that **stayed** in its class, so you can see each class's full starting area and not only the part that changed. One diagram is shown per selected scenario.

### 🌫️ Emissions

Greenhouse-gas emissions from agriculture and land use, in MtCO₂e: **CO₂ from AFOLU, methane from cattle digestion, methane from rice, and nitrous oxide from agriculture**. Observed emissions come from SEEG (Brazil's national emissions estimates).

CO₂ AFOLU starts in 2005, and it can go **below zero** when forests and land absorb more carbon than is emitted (a net carbon sink) — in that case a dashed line marks zero.

### 🌾 Crops

**Soybeans, Corn, Sugarcane, Rice, Dry Beans and Cassava** — choose whether to see **Area** (Mha), **Production** (Mt) or **Yield** (tonnes per hectare). Observed data come from IBGE.

### 🐄 Livestock

**Beef, Milk, Chicken and Pork production** (Mt), the **Cattle Herd** (million head) and the **Cattle Stocking Rate** (livestock units per hectare). Observed data come from IBGE; the stocking rate has no observed series, so only the scenarios are shown.

### 🚢 Trade

**Exports** (soybeans — in total, as grain, cake or oil —, corn and beef) and **Imports** (wheat), in Mt. Observed data come from Comex, Brazil's official foreign-trade statistics.

When you look all the way to 2050, a separate series of salmon-coloured diamonds shows the **Ministry of Agriculture's (MAPA) own projections** for 2025, 2030 and 2035, plus a 2050 point extended from that trend (marked as an extrapolation when you hover over it). These are forecasts, not observations, so they are kept apart from the historical line.

### 🍽️ Food

Average **food consumption** per person (kilocalories per person per day) for each scenario.

### 🔁 Transition Matrix

Another way to read land-use change: a **grid of "from → to"**. Each row is what the land was at the start of the period, each column is what it became, and each cell is how much area (Mha) moved between the two. The darker the cell, the more land moved.

The diagonal (land that did not change) is left blank, crossed by a "/", so only real change shows. Transitions the model does not allow appear as 0.

One grid is shown per selected scenario, and **all grids use the same colour scale** — so shades can be compared directly between scenarios, and the scale updates automatically as you add or remove a scenario or move the period slider.

### 🌎 Maps

Maps of **where** land-use change happens across Brazil, for the scenarios that have been spatially downscaled (currently UP51b, UP51a, UP50, UP48 and UP46 — UP48 and UP46 for Current Trends only). The newest calibration with both pathways (UP51b) is shown when the tab opens.

- **Map Type** — *Land Cover* (area of a class in each year), *Outflows* (area lost by a class) or *Transitions* (area converted from one class to another).
- **Scenario** — choose up to two. When both belong to the same calibration, a third map shows the **difference** between them (blue = the second scenario has more, red = less).
- **Year** — 2020 to 2050, using the `◀` `▶` buttons or the year bar.

On Land Cover maps, each scenario's header also shows the FABLE Calculator's own national total for that class and year, as a quick check on the map.

### 🕰️ Historical Maps

Maps of what has actually been **observed** in Brazil — not scenario results — from **IBGE** (cropland) and **MapBiomas Collection 10** (land cover). The maps were converted to a 0.5° grid.

- **Source** — choose **IBGE** (which shows Cropland) or **MapBiomas C10**, one at a time. The tab opens on IBGE.
- **MapBiomas classes** — once MapBiomas is selected, a switch appears for each of its 32 classes. **Forest Formation** is shown first; switch on more classes to see them side by side. The maps resize to fit however many you choose.
- **Timeline** — 1995 to 2020, every 5 years. Some classes did not exist in every year (cotton only from 2000, for example); those years say so instead of showing a map.

## 📚 Where the historical data come from

| Topic | Source |
|-------|--------|
| Land use | MapBiomas, LAPIG, IBGE |
| Crop area and production | IBGE |
| Livestock | IBGE |
| Trade | Comex (observed) · MAPA (projections to 2035) |
| Emissions | SEEG |

## 🔧 For maintainers

**Launchers on macOS/Ubuntu.** If a launcher won't run, make it executable once from a terminal in the project folder (`chmod +x open_app_mac.command` or `chmod +x open_app_ubuntu.sh`).

**Running without the launchers.** From the project folder: `Rscript app.R`, or open the project in RStudio and click *Run App*. On the first run the app reads the spreadsheets and caches the results in `data/processed/`; it refreshes that cache by itself whenever a source file changes.

**Adding a scenario.** Put the new `.xlsx` in `data/xlsx/` and add one row to `data/xlsx/scenarios.csv` (`file`, `label`, and an optional `up` number — only the highest `up` starts switched on; a revision of an existing calibration can use a decimal such as `51.1`). No code changes are needed, and the row order sets the display order and colours.

**Map data.**
- The *Maps* tab reads PNGs that `04_generate_maps.R` builds from the downscaled files in `data/luc/` (`downscaled_LUC_UP<n>_<ct|ndc>.rds`). The launchers run it automatically the first time; to rerun by hand: `Rscript 04_generate_maps.R`. Scenarios without a downscaled file simply show "Map data not available".
- The *Historical Maps* tab reads the ready-made PNGs in `data/historical_maps/` (copied from the MAgPIE scenario viewer; they are not generated here).

**Historical observations** live in `data/csv/histdatabrazil.csv` (`type`, `source`, `year`, `value`, `unit`).

**Word report (experimental).** `03_generate_report.R` produces a static Word document comparing the UP50 Current Trends and NDC pathways. It is still under development.

---

👤 **Author**
Wanderson Costa (wcosta.comp@gmail.com)

🤖 This application was developed with the assistance of [Claude Code](https://claude.ai/code).
