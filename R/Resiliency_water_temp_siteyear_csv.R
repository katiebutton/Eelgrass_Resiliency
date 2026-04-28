# -----------------------------
# Load packages
# -----------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(openxlsx)
library(readr)
library(tidyr)    # for replace_na
library(stringr)  # for safe filename handling

# -----------------------------
# Site info
# -----------------------------
site_name  <- "CACO_Seagrass_MA20_1"
record_name <- "Water Temp.A@CACO_Seagrass_MA20_1"
Tth <- 25

# -----------------------------
# Read CSV (LONG TIME SERIES SITES)
# -----------------------------
temp_data <- read_csv(
  "Water_Temp.A@CACO_Seagrass_MA20_1.EntireRecord.csv",
  show_col_types = FALSE
)

# -----------------------------
# Clean & format (MATCHES AQUARIUS OUTPUT)
# -----------------------------
temp_data <- temp_data %>%
  mutate(
    date_time = mdy_hm(timestamp, tz = "UTC"),
    value = as.numeric(value),
    site = site_name
  ) %>%
  filter(!is.na(date_time), !is.na(value))

# -----------------------------
# Function: Calculate site metrics (May–Sep, hourly aggregation)
# -----------------------------
calculate_site_metrics <- function(site_name,
                                   record_name,
                                   csv_path,
                                   Tth = 25,        # set to 25 to match standardized threshold; change if site-specific
                                   tz  = "UTC") {
  
  # ---- Read & clean Aquarius CSV ----
  temp_data <- read_csv(csv_path, show_col_types = FALSE) %>%
    mutate(
      date_time = mdy_hm(timestamp, tz = tz),   # matches Aquarius export
      value     = as.numeric(value),
      site      = site_name
    ) %>%
    filter(!is.na(date_time), !is.na(value))
  
  # ---- Filter May–September + aggregate hourly ----
  temp_hourly <- temp_data %>%
    filter(month(date_time) %in% 5:9) %>%     # May–September
    group_by(Hour = floor_date(date_time, "hour")) %>%
    summarise(hourly_temp = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Year         = year(Hour),
      HDH_hour     = pmax(hourly_temp - Tth, 0),
      CDH_hour     = pmin(hourly_temp - Tth, 0),
      above_thresh = hourly_temp > Tth,
      Date         = as.Date(Hour)
    )
  
  # ---- Site-level % time above threshold (standardized over May–Sep) ----
  site_pct_time_above <- mean(temp_hourly$above_thresh) * 100
  
  # ---- DAILY aggregation (with Year) ----
  daily_data <- temp_hourly %>%
    group_by(Year, Date) %>%
    summarise(
      daily_HDH     = sum(HDH_hour, na.rm = TRUE),
      daily_CDH     = sum(CDH_hour, na.rm = TRUE),
      max_temp      = max(hourly_temp, na.rm = TRUE),
      min_temp      = min(hourly_temp, na.rm = TRUE),
      mean_temp     = mean(hourly_temp, na.rm = TRUE),
      pct_time_above= mean(above_thresh) * 100,
      .groups       = "drop"
    ) %>%
    filter(month(Date) %in% 5:9)  # ensure daily rows are May–Sep
  
  # ---- Identify continuous warming events per year (any hourly > Tth) ----
  event_hours <- temp_hourly %>%
    arrange(Hour) %>%
    group_by(Year) %>%
    mutate(event_id = cumsum(above_thresh & !lag(above_thresh, default = FALSE))) %>%
    filter(above_thresh)
  
  # ---- Event-level summaries per year ----
  warming_events <- event_hours %>%
    group_by(Year, event_id) %>%
    summarise(
      site           = site_name,
      event_start    = min(Hour),
      event_end      = max(Hour),
      duration_hours = n(),
      duration_days  = n() / 24,
      Total_HDH      = sum(HDH_hour, na.rm = TRUE),
      max_temp       = max(hourly_temp, na.rm = TRUE),
      mean_temp      = mean(hourly_temp, na.rm = TRUE),
      .groups        = "drop"
    ) %>%
    arrange(Year, desc(Total_HDH))
  
  # ---- Site-level seasonal summary (across all years in the data) ----
  n_events      <- nrow(warming_events)
  mean_duration <- if (n_events > 0) mean(warming_events$duration_days) else 0
  se_duration   <- if (n_events > 0) sd(warming_events$duration_days) / sqrt(n_events) else 0
  
  duration_warmest_period <- if (nrow(daily_data) > 0) {
    rle_vals <- rle(daily_data$daily_HDH > 0)
    if (any(rle_vals$values)) max(rle_vals$lengths[rle_vals$values]) else 0
  } else 0
  
  site_summary <- daily_data %>%
    summarise(
      site                                = site_name,
      date_start                          = min(Date),
      date_end                            = max(Date),
      Total_HDH                           = sum(daily_HDH),
      Total_CDH                           = sum(daily_CDH),
      Mean_HDH_daily                      = mean(daily_HDH),
      Mean_CDH_daily                      = mean(daily_CDH),
      Max_HDH_daily                       = max(daily_HDH),
      mean_temp                           = mean(mean_temp),
      max_temp                            = max(max_temp),
      min_temp                            = min(min_temp),
      Duration_warmest_period_days        = duration_warmest_period,
      Number_warming_events               = n_events,
      Mean_warming_event_duration_days    = mean_duration,
      SE_warming_event_duration_days      = se_duration,
      n_days                              = n(),
      Site_pct_time_above                 = site_pct_time_above
    )
  
  # ---- Annual (per Year) summaries: % time above Tth, event metrics, warmest period ----
  site_pct_time_above_by_year <- temp_hourly %>%
    group_by(Year) %>%
    summarise(
      Site_pct_time_above = mean(above_thresh) * 100,
      .groups = "drop"
    )
  
  event_summary_by_year <- daily_data %>%
    distinct(Year) %>%
    left_join(
      warming_events %>%
        group_by(Year) %>%
        summarise(
          Number_warming_events               = n(),
          Mean_warming_event_duration_days    = if (n() > 0) mean(duration_days) else 0,
          SE_warming_event_duration_days      = if (n() > 0) sd(duration_days) / sqrt(n()) else 0,
          .groups = "drop"
        ),
      by = "Year"
    ) %>%
    mutate(
      Number_warming_events               = replace_na(Number_warming_events, 0),
      Mean_warming_event_duration_days    = replace_na(Mean_warming_event_duration_days, 0),
      SE_warming_event_duration_days      = replace_na(SE_warming_event_duration_days, 0)
    )
  
  duration_warmest_period_by_year <- daily_data %>%
    group_by(Year) %>%
    summarise(
      Duration_warmest_period_days = {
        rle_vals <- rle(daily_HDH > 0)
        if (any(rle_vals$values)) max(rle_vals$lengths[rle_vals$values]) else 0
      },
      .groups = "drop"
    )
  
  site_summary_annual <- daily_data %>%
    group_by(Year) %>%
    summarise(
      site             = site_name,
      date_start       = min(Date),
      date_end         = max(Date),
      Total_HDH        = sum(daily_HDH),
      Total_CDH        = sum(daily_CDH),
      Mean_HDH_daily   = mean(daily_HDH),
      Mean_CDH_daily   = mean(daily_CDH),
      Max_HDH_daily    = max(daily_HDH),
      mean_temp        = mean(mean_temp),
      max_temp         = max(max_temp),
      min_temp         = min(min_temp),
      n_days           = n(),
      .groups          = "drop"
    ) %>%
    left_join(duration_warmest_period_by_year, by = "Year") %>%
    left_join(event_summary_by_year,          by = "Year") %>%
    left_join(site_pct_time_above_by_year,    by = "Year") %>%
    relocate(n_days, .after = SE_warming_event_duration_days) %>%
    arrange(Year)
  
  # ---- Return all outputs ----
  list(
    raw_data            = temp_data,
    hourly_data         = temp_hourly,
    daily_data          = daily_data,
    warming_events      = warming_events,
    site_summary        = site_summary,
    site_summary_annual = site_summary_annual
  )
}

# -----------------------------
# Example RUN (matches your second script pattern)
# -----------------------------
site_name   <- "CACO_Seagrass_MA20_1"
record_name <- "Water Temp.A@CACO_Seagrass_MA20_1"
csv_path    <- "Water_Temp.A@CACO_Seagrass_MA20_1.EntireRecord.csv"  # <-- set to your actual file

results <- calculate_site_metrics(
  site_name   = site_name,
  record_name = record_name,
  csv_path    = csv_path,
  Tth         = 25  # use 25°C to match the standardized annual threshold
)

# -----------------------------
# VIEW RESULTS
# -----------------------------
View(results$hourly_data)
View(results$daily_data)
View(results$warming_events)
print(results$site_summary,        width = Inf)
print(results$site_summary_annual, width = Inf)

# -----------------------------
# PLOT: Hourly temps + threshold (red when > Tth)
# -----------------------------
temp_plot <- ggplot(results$hourly_data, aes(Hour, hourly_temp)) +
  geom_line(color = "grey60") +
  geom_line(
    data = results$hourly_data %>% filter(hourly_temp > Tth),
    aes(Hour, hourly_temp),
    color = "firebrick",
    linewidth = 1
  ) +
  geom_hline(yintercept = Tth, linetype = "dashed") +
  labs(
    title = paste("Hourly Water Temperature —", record_name),
    y = "Hourly Mean Water Temperature (°C)",
    x = "Date"
  ) +
  theme_minimal()

print(temp_plot)

# -----------------------------
# EXPORT to Excel (with plot, metadata, and data tabs)
# -----------------------------
wb <- createWorkbook()

# Data tabs
addWorksheet(wb, "Hourly Data")
writeData(wb, "Hourly Data", results$hourly_data)

addWorksheet(wb, "Daily Data")
writeData(wb, "Daily Data", results$daily_data)

addWorksheet(wb, "Warming Events")
writeData(wb, "Warming Events", results$warming_events)

addWorksheet(wb, "Site Summary")
writeData(wb, "Site Summary", results$site_summary)

addWorksheet(wb, "Site Summary Annual")
writeData(wb, "Site Summary Annual", results$site_summary_annual)

# Metadata tab (aligned to your description, but parameterized for Tth and months)
metadata <- data.frame(
  Metric = c("HDH","CDH","Warming Event","Duration warmest period",
             "Number of warming events","Mean warming event duration",
             "SE warming event duration","May–September filter","Aggregation","Threshold","%_abv_Tth"),
  Description = c(
    sprintf("Hourly degrees above %.1f°C summed over each day", Tth),
    sprintf("Hourly degrees below %.1f°C summed over each day", Tth),
    sprintf("Any continuous sequence of hours where hourly temperature > %.1f°C", Tth),
    "Maximum number of consecutive days with HDH > 0 (i.e., daily mean temperatures exceeded threshold)",
    "Count of distinct daily HDH > 0 sequences (i.e., each sequence above threshold counts as one warming event)",
    "Mean length (days) of warming events",
    "Standard error of warming event durations",
    "Only data from months 5–9 included",
    "15–30 min data averaged to hourly prior to calculations",
    sprintf("%.1f°C (e.g., Berget et al. 2024 for seagrass)", Tth),
    sprintf("The percentage of all recorded hours when water temperature exceeded %.1f°C during the monitoring period.", Tth)
  )
)
addWorksheet(wb, "Metadata")
writeData(wb, "Metadata", metadata)

# Plot tab
addWorksheet(wb, "Hourly Plot")
insertPlot(
  wb, sheet = "Hourly Plot",
  width = 8, height = 5,
  fileType = "png",
  startRow = 1, startCol = 1
)

# Save workbook with safe filename
safe_record <- gsub("[:@ ]", "_", record_name)
out_file    <- paste0(safe_record, "_Metrics_Annual", Tth, ".xlsx")
saveWorkbook(wb, out_file, overwrite = TRUE)
