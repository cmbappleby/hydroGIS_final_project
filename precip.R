# Load libraries
library(here)
library(tidyverse)
library(magrittr)
library(lubridate)
library(readxl)
library(cder)

### FUNCTIONS ###

# Retrieve daily precip data from CDEC DWR https://cdec.water.ca.gov/
# Clean up data
get_daily_data <- function(start_date, end_date, station_codes) {
  daily_precip <- list()
  
  for (i in seq_along(station_codes)) {
    daily_precip[[i]] <- cdec_query(c(station_codes[i]),
                                    c(45),
                                    c('D'),
                                    start_date,
                                    end_date)
  }
  
  #Clean up data frame
  clean_daily_precip <- list()
  
  for (i in seq_along(daily_precip)) {
    df <- daily_precip[[i]]
    clean_df <- df %>%
      rename(Rain_in = Value) %>%
      mutate(Rain_mm = Rain_in * 25.4,
             mon = month(ObsDate),
             WY = if_else(mon >= 9,
                          year(ObsDate) + 1,
                          year(ObsDate))) %>%
      select(c(StationID, ObsDate, WY, mon, Rain_mm))
    
    clean_daily_precip[[i]] <- clean_df
  }
  
  names(clean_daily_precip) <- station_codes
  
  return(clean_daily_precip)
}

# Read monthly CSVs, clean up data, and calculate annual means
get_annual_mean <- function(folder_path) {
  csv_files <- list.files(folder_path, pattern = '*2021.csv', full.names = TRUE)
  
  annual_precip <- tibble(
    StationID = character(),
    WY = integer(),
    AnnualPrecip_mm = double()
  )
  
  for (i in seq_along(csv_files)) {
    fn <- csv_files[i]
    csv <- read_csv(fn)
    
    if (!'RAIN INCHES' %in% colnames(csv)) {
      csv %<>% rename(`RAIN INCHES` = `PPT ADJ INCHES`)
    }
    
    clean_csv <- csv %>%
      mutate(
        `RAIN INCHES` = as.numeric(`RAIN INCHES`),
        StationID = str_sub(basename(fn), start = 1L, end = 3L),
        ParsedDate = my(DATE),
        Mon = month(ParsedDate),
        WY = if_else(Mon >= 9,
                     year(ParsedDate) + 1,
                     year(ParsedDate))
      ) %>%
      group_by(StationID, Mon) %>%
      mutate(
        precip_filled = if_else(
          is.na(`RAIN INCHES`),
          median(`RAIN INCHES`, na.rm = TRUE),
          `RAIN INCHES`
        ),
        Precip_mm = precip_filled * 25.4
      ) %>%
      ungroup() %>%
      group_by(StationID, WY) %>%
      summarize(AnnualPrecip_mm = sum(Precip_mm, na.rm = TRUE))
    
    annual_precip %<>% bind_rows(clean_csv)
  }
  
  site_annual_means <- annual_precip %>%
    group_by(StationID) %>%                   # group by site
    summarize(
      MeanAnnualPrecip_mm = mean(AnnualPrecip_mm, na.rm = TRUE),
      .groups = "drop"
    )
}

# Count number of rain days per year
count_rain_days <- function(clean_daily_precip) {
  rain_mm_days <- tibble(
    StationID = character(),
    WY = integer(),
    n_rain_days = integer()
  )
  
  for (i in seq_along(clean_daily_precip)) {
    df <- clean_daily_precip[[i]]
    
    sum_df <- df %>%
      group_by(StationID, WY) %>%
      summarize(
        n_days = n(),
        n_missing = sum(is.na(Rain_mm)),
        n_rain_days = sum(Rain_mm > 0, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        valid_year = n_missing < 0.9 * 365
      ) %>%
      filter(valid_year) %>%
      select(StationID, WY, n_rain_days)
    
    rain_mm_days %<>% bind_rows(sum_df)
  }
  
  site_rain_means <- rain_mm_days %>%
    group_by(StationID) %>%
    summarize(
      MeanRainyDays = round(mean(n_rain_days, na.rm = TRUE)),
      .groups = 'drop',
    )
}

### USER-SET VARIABLES ###

start_date <- ymd('2012-09-01')
end_date <- ymd('2021-08-31')
station_codes <- c('8SI', 'BRS', 'BUP', 'DSB', 'CHL', 
                   'PVL', 'QNC', 'SBY', 'CNY')


### PROCESS DATA ###

# Get data
clean_daily_precip <- get_daily_data(start_date, end_date, station_codes)

# Calculate rain days
station_rainy_days_means <- count_rain_days(clean_daily_precip)

# Calculate annual means from monthly CSVs
station_annual_means <- get_annual_mean(here('data'))

# Save as CSVs
write.csv(station_rainy_days_means, 
          file = here('data/precip/station_rainy_days_means.csv'), 
          row.names = FALSE)

write.csv(station_annual_means,
          file = here('data/precip/station_annual_means.csv'),
          row.names = FALSE)
