# scripts/12_visualize_ODEBT_H2S_NH4_confidence_thresholds.R
# This script visualizes volume-specific oxygen debt including H2S + NH4,
# calculates reference-period threshold values from 1900-1959,
# adds 2011-2016 and 2016-2021 period averages,
# and saves one two-panel yearly figure with threshold and period-average lines.

options(project_clean_workspace = FALSE)
# This prevents accidental workspace wiping when this script is run by itself.

source("scripts/01_header.R")
# Loads packages via 02_setup.R if needed.

if (is.null(getOption("project_assessment"))) {
  source("scripts/03_define_assessment.R")
}
# Defines the assessment period and output folder if not already defined.

assessment <- getOption("project_assessment")

# Define paths explicitly so the script works when run alone.
inputPath <- "Input/master"
outputPath <- assessment$output_dir
years <- assessment$start_year:assessment$end_year

#------------------------------------------------------------------------------#
# User-controlled settings

reference_start_year <- 1900
reference_end_year <- 1959
reference_years <- reference_start_year:reference_end_year

# Threshold definition.
# Default: mean value during 1900-1959 by Basin.
# Other supported options: "median", "percentile_95".
threshold_stat <- "mean"

# Main ODEBT column produced in script 10.
value_col <- "ODEBT_H2S_NH4_INTERPOLATED_volsp"

#------------------------------------------------------------------------------#
# Helper functions

check_file <- function(path) {
  if (!file.exists(path)) {
    stop("Required file not found: ", path, call. = FALSE)
  }
}

safe_name <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

calc_threshold <- function(x, method = "mean") {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)

  if (method == "mean") {
    return(mean(x))
  }
  if (method == "median") {
    return(median(x))
  }
  if (method == "percentile_95") {
    return(as.numeric(stats::quantile(x, probs = 0.95, names = FALSE, type = 7)))
  }

  stop("Unsupported threshold_stat: ", method, call. = FALSE)
}

confidence_class <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x >= 75 ~ "HIGH",
    x >= 50 ~ "MODERATE",
    x < 50 ~ "LOW"
  )
}

rolling_mean_5 <- function(year, value, window = 5, min_n = 3) {
  # Centered rolling mean using a 5-year calendar window.
  # At the beginning/end of the series, a partial window is used if at least
  # min_n non-missing values are available.
  out <- rep(NA_real_, length(value))
  half_window <- floor(window / 2)

  for (i in seq_along(value)) {
    idx <- which(year >= (year[i] - half_window) & year <= (year[i] + half_window))
    x <- value[idx]
    x <- x[!is.na(x)]

    if (length(x) >= min_n) {
      out[i] <- mean(x)
    }
  }

  out
}

#------------------------------------------------------------------------------#
# Read yearly ODEBT results

results_path <- file.path(outputPath, "ODEBT_by_year_and_basin.csv")
check_file(results_path)

results <- read.csv(results_path, stringsAsFactors = FALSE)

required_cols <- c("Year", "Basin", value_col)
missing_cols <- setdiff(required_cols, names(results))
if (length(missing_cols) > 0) {
  stop(
    "ODEBT results file is missing required column(s): ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

results <- results %>%
  dplyr::mutate(
    Year = as.integer(Year),
    Basin = as.character(Basin),
    ODEBT_H2S_NH4_volsp = .data[[value_col]]
  ) %>%
  dplyr::filter(Year %in% years)

#------------------------------------------------------------------------------#
# Calculate threshold values by Basin from 1900-1959

thresholds <- results %>%
  dplyr::filter(Year %in% reference_years) %>%
  dplyr::group_by(Basin) %>%
  dplyr::summarise(
    threshold_year_start = reference_start_year,
    threshold_year_end = reference_end_year,
    threshold_stat = threshold_stat,
    n_reference_years = sum(!is.na(ODEBT_H2S_NH4_volsp)),
    threshold_ODEBT_H2S_NH4_volsp = calc_threshold(ODEBT_H2S_NH4_volsp, threshold_stat),
    reference_mean = mean(ODEBT_H2S_NH4_volsp, na.rm = TRUE),
    reference_sd = stats::sd(ODEBT_H2S_NH4_volsp, na.rm = TRUE),
    .groups = "drop"
  )

# Avoid NaN from all-missing reference data.
thresholds <- thresholds %>%
  dplyr::mutate(
    dplyr::across(
      c(threshold_ODEBT_H2S_NH4_volsp, reference_mean, reference_sd),
      ~ ifelse(is.nan(.x), NA_real_, .x)
    )
  )

write.csv(
  thresholds,
  file.path(outputPath, "ODEBT_H2S_NH4_thresholds_1900_1959.csv"),
  row.names = FALSE
)

#------------------------------------------------------------------------------#
# Read and prepare confidence information

confidence_yearly_path <- file.path(outputPath, "confidence_yearly.csv")
confidence_period_path <- file.path(outputPath, "confidence_per_period.csv")

confidence_yearly <- NULL
confidence_period <- NULL

if (file.exists(confidence_yearly_path)) {
  confidence_yearly <- read.csv(confidence_yearly_path, stringsAsFactors = FALSE)

  # Create yearly confidence score from the components calculated in script 11.
  # Expected components: spatial_yr, temporal_months, methodical, p_correct_pct.
  confidence_components <- intersect(
    c("spatial_yr", "temporal_months", "methodical", "p_correct_pct"),
    names(confidence_yearly)
  )

  if (length(confidence_components) > 0) {
    confidence_yearly$confidence_yr <- rowMeans(
      confidence_yearly[, confidence_components, drop = FALSE],
      na.rm = TRUE
    )
    confidence_yearly$confidence_yr[is.nan(confidence_yearly$confidence_yr)] <- NA_real_
  }

  if (!"confidence_yr" %in% names(confidence_yearly)) {
    confidence_yearly$confidence_yr <- NA_real_
  }

  confidence_yearly <- confidence_yearly %>%
    dplyr::mutate(
      Year = as.integer(Year),
      Basin = as.character(Basin),
      confidence_class_yr = confidence_class(confidence_yr)
    ) %>%
    dplyr::select(
      Year,
      Basin,
      dplyr::any_of(c(
        "spatial_sal_model",
        "un_stations",
        "spatial_yr",
        "temporal_months",
        "methodical",
        "p_correct_pct",
        "confidence_yr",
        "confidence_class_yr"
      ))
    )
} else {
  message("No confidence_yearly.csv found. Figures will be produced without yearly confidence classes.")
}

if (file.exists(confidence_period_path)) {
  confidence_period <- read.csv(confidence_period_path, stringsAsFactors = FALSE) %>%
    dplyr::mutate(Basin = as.character(Basin))

  if ("class" %in% names(confidence_period)) {
    confidence_period <- confidence_period %>%
      dplyr::rename(period_confidence_class = class)
  } else {
    confidence_period$period_confidence_class <- NA_character_
  }

  # Recalculate the class in the correct order; script 11 checks >=50 before >=75,
  # which can classify high-confidence rows as moderate.
  if ("confidence" %in% names(confidence_period)) {
    confidence_period$period_confidence_class <- confidence_class(confidence_period$confidence)
  }

  confidence_period <- confidence_period %>%
    dplyr::select(
      Basin,
      dplyr::any_of(c(
        "spatial",
        "temporal",
        "methodical",
        "accuracy",
        "confidence",
        "period_confidence_class"
      ))
    )
} else {
  message("No confidence_per_period.csv found. Period confidence will not be joined.")
}

#------------------------------------------------------------------------------#
# Join results, thresholds, and confidence information

plot_data <- results %>%
  dplyr::left_join(thresholds, by = "Basin")

if (!is.null(confidence_yearly)) {
  plot_data <- plot_data %>%
    dplyr::left_join(confidence_yearly, by = c("Year", "Basin"))
} else {
  plot_data$confidence_yr <- NA_real_
  plot_data$confidence_class_yr <- NA_character_
}

if (!is.null(confidence_period)) {
  plot_data <- plot_data %>%
    dplyr::left_join(confidence_period, by = "Basin")
}

#------------------------------------------------------------------------------#
# Prepare data for the single two-panel yearly figure

period_1_start_year <- 2011
period_1_end_year <- 2016
period_2_start_year <- 2016
period_2_end_year <- 2021

fmt_num <- function(x, digits = 2) {
  ifelse(
    is.na(x),
    "NA",
    formatC(x, format = "f", digits = digits)
  )
}

plot_data <- plot_data %>%
  dplyr::mutate(
    Basin_panel = dplyr::case_when(
      grepl("central", tolower(Basin)) ~ "Central Baltic",
      grepl("bornholm", tolower(Basin)) ~ "Bornholm",
      TRUE ~ Basin
    ),
    threshold_label = paste0(
      "Threshold (", reference_start_year, "-", reference_end_year, ", ",
      threshold_stat, ")"
    )
  )

# Keep the requested two-panel layout when both target areas are present.
target_panels <- c("Central Baltic", "Bornholm")
if (all(target_panels %in% unique(plot_data$Basin_panel))) {
  plot_data <- plot_data %>%
    dplyr::filter(Basin_panel %in% target_panels)
}

basin_panel_levels <- c(
  target_panels,
  setdiff(sort(unique(plot_data$Basin_panel)), target_panels)
)

plot_data <- plot_data %>%
  dplyr::mutate(
    Basin_panel = factor(Basin_panel, levels = basin_panel_levels)
  ) %>%
  dplyr::group_by(Basin) %>%
  dplyr::arrange(Year, .by_group = TRUE) %>%
  dplyr::mutate(
    ODEBT_H2S_NH4_volsp_ma5 = rolling_mean_5(
      Year,
      ODEBT_H2S_NH4_volsp,
      window = 5,
      min_n = 3
    )
  ) %>%
  dplyr::ungroup()

# Basin-specific period averages for the requested recent assessment periods.
period_average_data <- plot_data %>%
  dplyr::group_by(Basin, Basin_panel) %>%
  dplyr::summarise(
    avg_2011_2016 = mean(
      ODEBT_H2S_NH4_volsp[
        Year >= period_1_start_year & Year <= period_1_end_year
      ],
      na.rm = TRUE
    ),
    avg_2016_2021 = mean(
      ODEBT_H2S_NH4_volsp[
        Year >= period_2_start_year & Year <= period_2_end_year
      ],
      na.rm = TRUE
    ),
    threshold_ODEBT_H2S_NH4_volsp = dplyr::first(threshold_ODEBT_H2S_NH4_volsp),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    avg_2011_2016 = ifelse(is.nan(avg_2011_2016), NA_real_, avg_2011_2016),
    avg_2016_2021 = ifelse(is.nan(avg_2016_2021), NA_real_, avg_2016_2021),
    annotation_label = paste0(
      "Threshold value: ", fmt_num(threshold_ODEBT_H2S_NH4_volsp),
      "\n", period_1_start_year, "-", period_1_end_year,
      " average: ", fmt_num(avg_2011_2016),
      "\n", period_2_start_year, "-", period_2_end_year,
      " average: ", fmt_num(avg_2016_2021)
    )
  )

period_line_data <- dplyr::bind_rows(
  period_average_data %>%
    dplyr::transmute(
      Basin_panel,
      period = paste0(period_1_start_year, "-", period_1_end_year),
      x_start = period_1_start_year,
      x_end = period_1_end_year,
      period_average = avg_2011_2016
    ),
  period_average_data %>%
    dplyr::transmute(
      Basin_panel,
      period = paste0(period_2_start_year, "-", period_2_end_year),
      x_start = period_2_start_year,
      x_end = period_2_end_year,
      period_average = avg_2016_2021
    )
)

threshold_plot_data <- plot_data %>%
  dplyr::distinct(Basin_panel, threshold_ODEBT_H2S_NH4_volsp)

write.csv(
  plot_data,
  file.path(outputPath, "ODEBT_H2S_NH4_with_confidence_thresholds.csv"),
  row.names = FALSE
)

write.csv(
  period_average_data,
  file.path(outputPath, "ODEBT_H2S_NH4_period_averages_2011_2016_2016_2021.csv"),
  row.names = FALSE
)

#------------------------------------------------------------------------------#
# Make one yearly figure with two vertical panels

fig_dir <- file.path(outputPath, "Figures_ODEBT_H2S_NH4")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

p_all <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = Year, y = ODEBT_H2S_NH4_volsp)
) +
  ggplot2::geom_hline(
    data = threshold_plot_data,
    ggplot2::aes(yintercept = threshold_ODEBT_H2S_NH4_volsp),
    linetype = "dashed",
    na.rm = TRUE
  ) +
  ggplot2::geom_point(
    color = "black",
    alpha = 0.35,
    size = 1.8,
    na.rm = TRUE
  ) +
  ggplot2::geom_segment(
    data = period_line_data,
    ggplot2::aes(
      x = x_start,
      xend = x_end,
      y = period_average,
      yend = period_average
    ),
    color = "blue",
    size = 1.1,
    na.rm = TRUE
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      y = ODEBT_H2S_NH4_volsp_ma5,
      group = Basin_panel
    ),
    color = "red",
    size = 0.9,
    na.rm = TRUE
  ) +
  ggplot2::geom_label(
    data = period_average_data,
    ggplot2::aes(
      x = -Inf,
      y = Inf,
      label = annotation_label
    ),
    hjust = -0.02,
    vjust = 1.08,
    size = 3.2,
    label.size = 0.25,
    na.rm = TRUE
  ) +
  ggplot2::facet_grid(Basin_panel ~ ., scales = "free_y") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    panel.spacing.y = grid::unit(1.1, "lines")
  ) +
  ggplot2::labs(
    title = "Volume-specific oxygen debt including H2S + NH4",
    subtitle = paste0(
      "Points: yearly values; red line: centered 5-year moving average; ",
      "blue segments: period averages; dashed line: ", threshold_stat,
      " threshold from ", reference_start_year, "-", reference_end_year
    ),
    x = "Year",
    y = "ODEBT incl. H2S + NH4 (mg/l)"
  )

all_fig_path <- file.path(
  fig_dir,
  "ODEBT_H2S_NH4_volsp_yearly_thresholds_period_averages_two_panel.jpg"
)

ggplot2::ggsave(
  filename = all_fig_path,
  plot = p_all,
  device = "jpg",
  width = 11,
  height = 8,
  dpi = 300
)

message("Saved threshold table: ", file.path(outputPath, "ODEBT_H2S_NH4_thresholds_1900_1959.csv"))
message("Saved period averages: ", file.path(outputPath, "ODEBT_H2S_NH4_period_averages_2011_2016_2016_2021.csv"))
message("Saved joined results: ", file.path(outputPath, "ODEBT_H2S_NH4_with_confidence_thresholds.csv"))
message("Saved figure: ", all_fig_path)
