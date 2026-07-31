# =============================================================================
# 04_generate_maps.R
# Generate PNG maps from FABLE downscaled Land Use Change (LUC) data
# =============================================================================
#
# Usage:
#   Rscript 04_generate_maps.R                      # every scenario with downscaled data + all diffs
#   Rscript 04_generate_maps.R "UP50 - Current Trends"   # one scenario (label from scenarios.csv) + all diffs
#   Rscript 04_generate_maps.R diff                 # diffs only, skip scenario maps
#
# Scenarios are discovered from data/xlsx/scenarios.csv — a scenario is
# processed only if its downscaled LUC file exists on disk:
#   data/luc/downscaled_LUC_UP<up>_<ct|ndc>.rds
# (<ct|ndc> derived from whether the scenario's xlsx filename contains "NDC").
# Adding a new UP's downscaled data later needs no code changes here — the
# next run just picks it up. A difference map is generated for a given UP
# only when BOTH its Current Trends and NDC downscaled files exist.
#
# Input:
#   data/luc/downscaled_LUC_UP<n>_ct.rds / _ndc.rds  (one pair per UP that has them)
#   data/luc/id_raster.tif
#   data/shapefiles/br_states.shp
#   data/shapefiles/br_biomes.shp
#
# Output:
#   data/maps/UP<n>_ct/landcover/    landcover_<Class>_<year>.png
#   data/maps/UP<n>_ct/transitions/  outflow_<Class>_<year>.png
#                                    transition_<From>_to_<To>_<year>.png
#   data/maps/UP<n>_ndc/landcover/, /transitions/          (same shape)
#   data/maps/diff/UP<n>/landcover/  landcover_<Class>_<year>.png  (NDC - CT, diverging)
#   data/maps/diff/UP<n>/transitions/ outflow_<Class>_<year>.png
#                                     transition_<From>_to_<To>_<year>.png
#
# Required packages:
#   install.packages(c("terra", "dplyr", "RColorBrewer"))
# =============================================================================

library(terra)
library(dplyr)
library(RColorBrewer)

# =============================================================================
# 0.  Configuration
# =============================================================================

# ── Discover scenarios with downscaled LUC data ───────────────────────────────
# Matched case-insensitively (list.files(ignore.case=TRUE)) rather than a
# hardcoded-case sprintf() path — the person providing these .rds files has
# used both "ct"/"ndc" and "CT"/"NDC" naming at different times, and this
# also keeps the script portable to the case-sensitive Mac/Ubuntu launchers.
find_downscaled_rds <- function(up, pathway) {
  files <- list.files("data/luc",
                      pattern    = sprintf("^downscaled_LUC_UP%d_%s\\.rds$", up, pathway),
                      full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) NA_character_ else files[1]
}

scenario_meta <- read.csv("data/xlsx/scenarios.csv", stringsAsFactors = FALSE)
scenario_meta$pathway <- ifelse(grepl("NDC", scenario_meta$file, ignore.case = TRUE), "ndc", "ct")
scenario_meta$rds     <- mapply(find_downscaled_rds, scenario_meta$up, scenario_meta$pathway)
scenario_meta$dir_out <- sprintf("data/maps/UP%d_%s", scenario_meta$up, scenario_meta$pathway)

available_meta <- scenario_meta[!is.na(scenario_meta$rds), ]
if (nrow(available_meta) == 0)
  stop("No scenarios have downscaled LUC data yet (data/luc/downscaled_LUC_UP<n>_<ct|ndc>.rds not found for any scenario in scenarios.csv).")

scenarios <- setNames(
  lapply(seq_len(nrow(available_meta)), function(i) {
    list(rds = available_meta$rds[i], dir_out = available_meta$dir_out[i], label = available_meta$label[i])
  }),
  sprintf("UP%d_%s", available_meta$up, available_meta$pathway)
)

cat("Scenarios with downscaled data:", paste(names(scenarios), collapse = ", "), "\n")
missing_meta <- scenario_meta[is.na(scenario_meta$rds), ]
if (nrow(missing_meta) > 0)
  cat("Skipping (no downscaled data yet):", paste(missing_meta$label, collapse = ", "), "\n")

args <- commandArgs(trailingOnly = TRUE)
diff_only <- FALSE
if (length(args) > 0) {
  arg <- trimws(args[1])
  if (tolower(arg) == "diff") {
    diff_only <- TRUE
  } else {
    match_label <- available_meta$label[tolower(available_meta$label) == tolower(arg)]
    if (length(match_label) == 1) {
      key <- names(scenarios)[sapply(scenarios, function(s) s$label == match_label)]
      scenarios <- scenarios[key]
    } else {
      stop("Unknown argument '", arg, "'. Use: diff | a scenario label from scenarios.csv (with downscaled data), e.g. \"UP50 - Current Trends\"")
    }
  }
}

brazil_ext <- ext(-75, -33, -36, 6.75)

scale_breaks <- c(0, 0.001, 5.9, 16.8, 37.3, 71.1, 130.4, 211.5, 260, 310)

class_palettes <- list(
  Forest    = brewer.pal(9, "Greens"),
  Cropland  = brewer.pal(9, "Reds"),
  Pasture   = brewer.pal(9, "Purples"),
  OtherLand = brewer.pal(9, "RdPu"),
  Urban     = brewer.pal(9, "Greys")
)

luc_classes <- names(class_palettes)

key_transitions <- list(
  list(from = "Forest",    to = "Cropland",  pal = brewer.pal(9, "Reds")),
  list(from = "Forest",    to = "Pasture",   pal = brewer.pal(9, "Purples")),
  list(from = "Cropland",  to = "Forest",    pal = brewer.pal(9, "Greens")),
  list(from = "Pasture",   to = "Cropland",  pal = brewer.pal(9, "Reds")),
  list(from = "OtherLand", to = "Cropland",  pal = brewer.pal(9, "Reds"))
)

# Difference map (NDC - CT): diverging RdBu, blue = NDC more, red = NDC less
# 1001 colours so white sits exactly at 0 (each step = 0.62 * 1000 ha)
diff_ceil <- 310
diff_pal  <- colorRampPalette(brewer.pal(11, "RdBu"))(1001)

# =============================================================================
# 1.  Shared spatial objects
# =============================================================================

id_raster    <- rast("data/luc/id_raster.tif")
brazil_states <- vect("data/shapefiles/br_states.shp")
brazil_biomes <- vect("data/shapefiles/br_biomes.shp")

cat("ID raster loaded:", nrow(id_raster), "rows x", ncol(id_raster), "cols\n")

# =============================================================================
# 2.  Helper: classify id_raster with a data frame of id_c -> value
# =============================================================================

to_raster <- function(df) {
  reclass_mat <- as.matrix(df[, c("id_c", "value")])
  r <- classify(id_raster, reclass_mat, others = NA)
  terra::extend(r, brazil_ext)
}

# =============================================================================
# 3.  Map generation (one scenario at a time)
# =============================================================================

run_scenario <- function(sc) {

  cat("\n============================================================\n")
  cat(" Scenario:", sc$label, "\n")
  cat(" Output:  ", sc$dir_out, "\n")
  cat("============================================================\n")

  dir.create(file.path(sc$dir_out, "landcover"),   showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(sc$dir_out, "transitions"), showWarnings = FALSE, recursive = TRUE)

  luc <- readRDS(sc$rds) |>
    rename(year = times) |>
    mutate(
      id_c = ifelse(grepl("_a$", ns),
                    as.numeric(sub("_a$", "", ns)) + 1e6,
                    as.numeric(ns)),
      year = as.integer(year)
    )

  cat("LUC rows:", nrow(luc), "| Years:", paste(sort(unique(luc$year)), collapse = ", "), "\n")

  # --- Part 1: Land Cover Maps ---
  cat("\n--- Part 1: Land Cover ---\n")

  for (cls in luc_classes) {
    pal <- class_palettes[[cls]]
    for (yr in sort(unique(luc$year))) {
      df_yr <- luc |>
        filter(lu.to == cls, year == yr) |>
        group_by(id_c) |>
        summarise(value = sum(value, na.rm = TRUE) * 0.001, .groups = "drop")

      total_mha <- sum(df_yr$value) / 1000
      r         <- to_raster(df_yr)
      out_file  <- file.path(sc$dir_out, "landcover",
                             sprintf("landcover_%s_%d.png", cls, yr))

      png(out_file, width = 820, height = 780, res = 100)
      plot(r,
           col    = pal,
           breaks = scale_breaks,
           legend = "bottomright",
           mar    = c(1.5, 0.5, 4, 0.5),
           plg    = list(title = "1000 ha", cex = 0.8))
      title(main = sprintf("%s\n%s | %d | Total: %.2f Mha", sc$label, cls, yr, total_mha),
            adj = 1, cex.main = 0.9)
      terra::plot(brazil_states, border = "lightgray", lwd = 0.1, add = TRUE)
      terra::plot(brazil_biomes, border = "black",     lwd = 1,   add = TRUE)
      dev.off()
      cat("  Saved:", out_file, "\n")
    }
  }

  # --- Part 2a: Total outflow FROM each class ---
  cat("\n--- Part 2a: Outflows ---\n")

  for (cls in luc_classes) {
    pal <- class_palettes[[cls]]
    for (yr in sort(unique(luc$year))) {
      df_yr <- luc |>
        filter(lu.from == cls, lu.to != cls, year == yr) |>
        group_by(id_c) |>
        summarise(value = sum(value, na.rm = TRUE) * 0.001, .groups = "drop")

      total_mha <- sum(df_yr$value) / 1000
      r         <- to_raster(df_yr)
      out_file  <- file.path(sc$dir_out, "transitions",
                             sprintf("outflow_%s_%d.png", cls, yr))

      png(out_file, width = 820, height = 780, res = 100)
      plot(r,
           col    = pal,
           breaks = scale_breaks,
           legend = "bottomright",
           mar    = c(1.5, 0.5, 4, 0.5),
           plg    = list(title = "1000 ha", cex = 0.8))
      title(main = sprintf("%s\nLoss of %s | %d | Total: %.2f Mha", sc$label, cls, yr, total_mha),
            adj = 1, cex.main = 0.9)
      terra::plot(brazil_states, border = "lightgray", lwd = 0.1, add = TRUE)
      terra::plot(brazil_biomes, border = "black",     lwd = 1,   add = TRUE)
      dev.off()
      cat("  Saved:", out_file, "\n")
    }
  }

  # --- Part 2b: Key transition pairs ---
  cat("\n--- Part 2b: Key transitions ---\n")

  for (tr in key_transitions) {
    for (yr in sort(unique(luc$year))) {
      df_yr <- luc |>
        filter(lu.from == tr$from, lu.to == tr$to, year == yr) |>
        group_by(id_c) |>
        summarise(value = sum(value, na.rm = TRUE) * 0.001, .groups = "drop")

      total_mha <- sum(df_yr$value) / 1000
      r         <- to_raster(df_yr)
      label     <- sprintf("%s_to_%s", tr$from, tr$to)
      out_file  <- file.path(sc$dir_out, "transitions",
                             sprintf("transition_%s_%d.png", label, yr))

      png(out_file, width = 820, height = 780, res = 100)
      plot(r,
           col    = tr$pal,
           breaks = scale_breaks,
           legend = "bottomright",
           mar    = c(1.5, 0.5, 4, 0.5),
           plg    = list(title = "1000 ha", cex = 0.8))
      title(main = sprintf("%s\n%s -> %s | %d | Total: %.2f Mha", sc$label, tr$from, tr$to, yr, total_mha),
            adj = 1, cex.main = 0.9)
      terra::plot(brazil_states, border = "lightgray", lwd = 0.1, add = TRUE)
      terra::plot(brazil_biomes, border = "black",     lwd = 1,   add = TRUE)
      dev.off()
      cat("  Saved:", out_file, "\n")
    }
  }

  cat("\nScenario", sc$label, "done.\n")
}

# =============================================================================
# 4.  Difference maps (NDC - CT), one per UP that has both pathways downscaled
# =============================================================================

load_luc <- function(rds_path) {
  readRDS(rds_path) |>
    rename(year = times) |>
    mutate(
      id_c = ifelse(grepl("_a$", ns),
                    as.numeric(sub("_a$", "", ns)) + 1e6,
                    as.numeric(ns)),
      year = as.integer(year)
    )
}

run_diff_for_up <- function(up, ct_rds, ndc_rds) {

  dir_diff <- sprintf("data/maps/diff/UP%d", up)
  cat("\n============================================================\n")
  cat(" Difference maps (NDC - CT), UP", up, "\n")
  cat(" Output:  ", dir_diff, "\n")
  cat("============================================================\n")

  dir.create(file.path(dir_diff, "landcover"),   showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(dir_diff, "transitions"), showWarnings = FALSE, recursive = TRUE)

  luc_ct  <- load_luc(ct_rds)
  luc_ndc <- load_luc(ndc_rds)

  years <- sort(unique(luc_ct$year))

  plot_diff <- function(r_diff, title_text, out_file) {
    mm <- as.numeric(minmax(r_diff, compute = TRUE))
    if (is.finite(mm[1]) && mm[1] == mm[2]) {
      writeLines(character(0), sub("\\.png$", ".nodiff", out_file))
      cat("  No diff:", sub("\\.png$", ".nodiff", out_file), "\n")
      return(invisible(NULL))
    }

    png(out_file, width = 820, height = 780, res = 100)
    plot(r_diff,
         col    = diff_pal,
         range  = c(-diff_ceil, diff_ceil),
         legend = "bottomright",
         mar    = c(1.5, 0.5, 4, 0.5),
         plg    = list(title = "1000 ha", cex = 0.8))
    title(main = title_text, adj = 1, cex.main = 0.9)
    terra::plot(brazil_states, border = "lightgray", lwd = 0.1, add = TRUE)
    terra::plot(brazil_biomes, border = "black",     lwd = 1,   add = TRUE)
    dev.off()
    cat("  Saved:", out_file, "\n")
  }

  # --- Part 1: Land Cover ---
  cat("\n--- Part 1: Land Cover ---\n")

  for (cls in luc_classes) {
    for (yr in years) {
      df_ct  <- luc_ct  |> filter(lu.to == cls, year == yr) |>
                group_by(id_c) |> summarise(value = sum(value) * 0.001, .groups = "drop")
      df_ndc <- luc_ndc |> filter(lu.to == cls, year == yr) |>
                group_by(id_c) |> summarise(value = sum(value) * 0.001, .groups = "drop")

      r_diff    <- to_raster(df_ndc) - to_raster(df_ct)
      delta_mha <- (sum(df_ndc$value) - sum(df_ct$value)) / 1000

      plot_diff(r_diff,
                sprintf("Difference (NDC - CT)\n%s | %d | %+.2f Mha", cls, yr, delta_mha),
                file.path(dir_diff, "landcover", sprintf("landcover_%s_%d.png", cls, yr)))
    }
  }

  # --- Part 2a: Outflows ---
  cat("\n--- Part 2a: Outflows ---\n")

  for (cls in luc_classes) {
    for (yr in years) {
      df_ct  <- luc_ct  |> filter(lu.from == cls, lu.to != cls, year == yr) |>
                group_by(id_c) |> summarise(value = sum(value) * 0.001, .groups = "drop")
      df_ndc <- luc_ndc |> filter(lu.from == cls, lu.to != cls, year == yr) |>
                group_by(id_c) |> summarise(value = sum(value) * 0.001, .groups = "drop")

      r_diff    <- to_raster(df_ndc) - to_raster(df_ct)
      delta_mha <- (sum(df_ndc$value) - sum(df_ct$value)) / 1000

      plot_diff(r_diff,
                sprintf("Difference (NDC - CT)\nLoss of %s | %d | %+.2f Mha", cls, yr, delta_mha),
                file.path(dir_diff, "transitions", sprintf("outflow_%s_%d.png", cls, yr)))
    }
  }

  # --- Part 2b: Key transitions ---
  cat("\n--- Part 2b: Key transitions ---\n")

  for (tr in key_transitions) {
    label <- sprintf("%s_to_%s", tr$from, tr$to)
    for (yr in years) {
      df_ct  <- luc_ct  |> filter(lu.from == tr$from, lu.to == tr$to, year == yr) |>
                group_by(id_c) |> summarise(value = sum(value) * 0.001, .groups = "drop")
      df_ndc <- luc_ndc |> filter(lu.from == tr$from, lu.to == tr$to, year == yr) |>
                group_by(id_c) |> summarise(value = sum(value) * 0.001, .groups = "drop")

      r_diff    <- to_raster(df_ndc) - to_raster(df_ct)
      delta_mha <- (sum(df_ndc$value) - sum(df_ct$value)) / 1000

      plot_diff(r_diff,
                sprintf("Difference (NDC - CT)\n%s -> %s | %d | %+.2f Mha", tr$from, tr$to, yr, delta_mha),
                file.path(dir_diff, "transitions", sprintf("transition_%s_%d.png", label, yr)))
    }
  }

  cat("\nDifference maps for UP", up, "done.\n")
}

run_diff <- function() {
  ups <- sort(unique(available_meta$up))
  for (up in ups) {
    rows <- available_meta[available_meta$up == up, ]
    ct_row  <- rows[rows$pathway == "ct", ]
    ndc_row <- rows[rows$pathway == "ndc", ]
    if (nrow(ct_row) == 1 && nrow(ndc_row) == 1) {
      run_diff_for_up(up, ct_row$rds[1], ndc_row$rds[1])
    } else {
      cat("\nSkipping diff for UP", up, "— needs both Current Trends and NDC downscaled data.\n")
    }
  }
}

# =============================================================================
# 5.  Run
# =============================================================================

if (!diff_only) for (sc in scenarios) run_scenario(sc)
run_diff()

cat("\nAll done.\n")
