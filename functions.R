# Functions for the tamsdpoa paper

### Load libraries
print("Loading libraries...")
suppressPackageStartupMessages({
  library(tidyverse)
  library(maps)
  library(mapdata)
  library(patchwork)
  library(timeplyr)
  library(ggrepel)
  library(broom)
  library(gt)
  library(knitr)
  library(scales)
  library(paletteer)
  library(zoo)
  library(sf)
  sf_use_s2(FALSE)
  library(slider)
  library(ggrepel)
  library(forcats)
})
print("Libraries loaded.")



#### Part 1: data analysis ####


# We presume that the data being used has already been quality controlled 
# and processed into annual / monthly / seasonal maxima.
#  
# An example data set - that used in the paper - is available from:
# https://doi.org/10.25919/s398-1p95
#  
# Full citation: 
# Rafter, T. (2025). Annual and seasonal maxima of sub-daily precipitation accumulations (RxNhr)
#     at observation stations in Australia [Dataset]. CSIRO. https://doi.org/10.25919/s398-1p95 

#' Read time series data from GSDR
#'
#' @param input_basedir The directory where the data is located
#' @param period_type A string indicating the type of time series
#'        to read in. Options are "Annual", "Seasonal", and "Monthly".
#' @param duration A string indicating the duration of time series
#'        to read in. Options are "1", "3", "6", "12", and "24".
#'
#' @return A dataframe containing the time series data
#' @export
#' 
#' This function reads in the time series data from the GSDR project.
#' It takes in the directory where the data is located, the type of
#' time series to read in, and the duration of time series to read
#' in. It returns a dataframe containing the time series data.
read_ts <- function(input_basedir = "./data", #TODO make sure that code that calls this function has a valid input_basedir
                    period_type = c("Annual", "Seasonal", "Monthly"),
                    duration = c("1", "3", "6", "12", "24")) {
  
  # match argument values to ensure they are valid
  period_type <- match.arg(period_type)
  duration <- match.arg(duration)
  
  # lists of seasons and months
  season_list <- c("DJF", "MAM", "JJA", "SON")
  month_list <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  # read in 1min data
  infile_1min <- paste0(input_basedir,"/Indices_1min/TimeSeries/",period_type,
                        "/TimeSeries_",period_type,"_Rx",duration,"hr.csv")
  ts_1min <- read_csv(infile_1min, col_types = cols(Folder = col_skip()))
  
  # read in 5min data
  infile_5min <- paste0(input_basedir,"/Indices_5min/TimeSeries/",period_type,
                        "/TimeSeries_",period_type,"_Rx",duration,"hr.csv")
  ts_5min <- read_csv(infile_5min, col_types = cols(Folder = col_skip()))

  # combine into one ts
  ts <- bind_rows(ts_1min, ts_5min, .id = "DataSource") %>%
    mutate(DataSource = as.factor(ifelse(DataSource == 2,"5min","1min")))

  # return the combined time series data
  ts

}


#' Filter stations by record length and period type
#'
#' @param df A data frame containing the time series data
#' @param recordlength The minimum number of years required to include a station
#' @param period_type One of "Annual", "Seasonal", or "Monthly", indicating the period type to filter by
#' @return A data frame containing the filtered time series data
#' @export
filter_stations <- function(df = ts_gtN_80pc, recordlength = record_length,
                            period_type = c("Annual", "Seasonal", "Monthly")) {

  # Match the given period type and duration to the correct indices
  period_type <- match.arg(period_type)
  recordlength <- as.numeric(recordlength)

  # Filter the data by record length and period type
  df_mod <- df %>% ungroup() %>%
    {
      # If period type is annual, group the data by station and year
      if (period_type == "Annual") group_by(., Station) else 
        # If period type is seasonal, group the data by season, station, and year
        if (period_type == "Seasonal") group_by(., Season, Station) else
          # If period type is monthly, group the data by month, station, and year
          if (period_type == "Monthly") group_by(., Month, Station) else .
    } %>%
    # Arrange the data by station and year
    arrange(Station, Year) %>%
    # Remove duplicate rows
    unique() %>%
    # Calculate the number of years for each station
    summarise(n_years = n()) %>%
    # Arrange the data by station
    arrange(Station) %>%
    # Move the station column to the front of the data frame
    relocate(Station) %>%
    # Filter the data by the minimum number of years required
    filter(n_years >= recordlength)

  # Return the filtered data
  df_mod

}


#' Relabel aggregations
#'
#' Changes integer representations of seasons and months to label form
#' 
#' @param df A data frame containing the time series data
#' @param period_type One of "Annual", "Seasonal", or "Monthly", indicating the period type to filter by
#' @return A data frame containing the relabeled time series data
#' @export
relabel_aggregations <- function(df, period_type) {

  # Lists of season and month labels
  season_list <- c("DJF", "MAM", "JJA", "SON")
  month_list <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  # Relabel the aggregations based on period type
  df %>% ungroup() %>%
    {if (period_type == "Annual")
      # If period type is annual, add an annual label
      mutate(., Annual = "Annual") else .
    } %>%
    {if (period_type == "Seasonal")
      # If period type is seasonal, relabel the season column
      mutate(., Season = lvls_revalue(as.factor(Season), season_list))
      else .
    } %>%
    {if (period_type == "Monthly")
      # If period type is monthly, relabel the month column
      mutate(., Month = lvls_revalue(as.factor(Month), month_list))
      else .
    }

}


#' Filter out records with less than N years of data
#' 
#' @param df A data frame containing the time series data
#' @param Nyears The minimum number of years required
#' @param valid_prop The minimum proportion of years required to be valid
#' @param periodtype One of "Annual", "Seasonal", or "Monthly", indicating the period type to filter by
#' @param oldest The first year to consider
#' @param newest The last year to consider
#' @return A data frame containing the filtered time series data
#' @export
valid_years_in_period <- function(df,
                                  Nyears = record_length,
                                  valid_prop = proportion,
                                  periodtype = period_type,
                                  oldest = first_year,
                                  newest = last_year) {
  

  # Filter out records before $oldest and after $newest
  df <- df %>%
    filter(Year >= oldest, Year <= newest)
  
  # Check that we have an adequate number of records (Nyears+):
  n_per_station <- df %>%
    ungroup() %>%
    {if (periodtype == "Annual") group_by(., Station) else . } %>%
    {if (periodtype == "Seasonal") group_by(., Station, Season) else . } %>%
    {if (periodtype == "Monthly") group_by(., Station, Month) else . } %>%
    summarise(n_years = n()) %>%
    filter(n_years >= Nyears)
  
  # How many year observations in the period?
  # newest
  # oldest
  period_length <- (as.integer(newest) - as.integer(oldest)) + 1
  # What is the minimum years to meet proportion target?
  if (! is.null(valid_prop)) {
    prop_years <- ceiling((as.numeric(valid_prop) / 100) * period_length)
    n_per_station <- n_per_station %>%
      filter(n_years >= prop_years)
  }
  
  if (nrow(n_per_station) > 0) {
    # Extract stations with adequate records:
    test_df <- n_per_station %>%
      {if (periodtype == "Annual")
        # Keep the stations with adequate records
        select(., Station) %>%
          left_join(df, by = "Station") %>%
          group_by(Station) %>%
          # Sort by station number
          arrange(Station) %>%
          # Add label for maximum value within each group (Station)
          mutate(ts_max_index = if_else(Value == max(Value, na.rm = T), "max", NA_character_),
                 # Convert Year to a date
                 Year = as_date(paste0(Year,"-01-01"))) %>%
          # Add index to each group
          mutate(group_id = cur_group_id(),
                 station_index = cur_group_id()) %>%
          ungroup() %>%
          # Fill in any missing years
          time_complete(Year, time_by = "year", fill = list(Value = NA)) %>%
          group_by(Station)
  
        else . } %>%
      {if (periodtype == "Seasonal")
        select(., Station, Season) %>%
          left_join(df, by = c("Station", "Season")) %>%
          group_by(Station) %>%
          mutate(station_index = cur_group_id()) %>%
          group_by(Station, Season) %>%
          arrange(Station, Season, Year) %>%
          mutate(ts_max_index = if_else(Value == max(Value, na.rm = T), "max", NA_character_),
                 Year = as_date(paste(Year,"01","01",sep="-"))) %>%
          mutate(group_id = cur_group_id()) %>%
          ungroup() %>%
          time_complete(Year, time_by = "year", fill = list(Value = NA)) %>%
          group_by(., Station, Season) %>%
          arrange(Station, Season, Year) %>%
          relocate(Year, .after = Station)
        else . } %>%
      {if (periodtype == "Monthly")
        select(., Station, Month) %>%
          left_join(df, by = c("Station", "Month")) %>%
          group_by(Station) %>%
          mutate(station_index = cur_group_id()) %>%
          group_by(Station, Month) %>%
          arrange(Station, Month, Year) %>%
          mutate(ts_max_index = if_else(Value == max(Value, na.rm = T), "max", NA_character_),
                 Year = as_date(paste(Year,Month,"01",sep="-"))) %>%
          mutate(group_id = cur_group_id()) %>%
          ungroup() %>%
          time_complete(Year, time_by = "year", fill = list(Value = NA)) %>%
          group_by(., Station, Month) %>%
          arrange(Station, Month, Year) %>%
          relocate(Year, .after = Station)
        else . }
  
    # Return modified df
    test_df
  
  } else {
    stop("No stations meet the criteria")
  }

}


# function to create summary stats table of time series
#' Summarise time series data
#'
#' Summarise the time series data into a table of minimum, median, mean, maximum
#' values for each station, along with the start and end years of the data.
#' 
#' @param df A data frame containing the time series data
#' @param oldest The first year to consider
#' @param newest The last year to consider
#' @param periodtype One of "Annual", "Seasonal", or "Monthly", indicating the period type to filter by
#' @return A data frame containing the summary statistics
#' @export
summarise_filtered_ts <- function(df = ts_gtN_80pc_period,
                                  oldest = first_year,
                                  newest = last_year,
                                  periodtype = period_type) {

  # Set the groupings for the summary based on the periodtype
  if (periodtype == "Annual") {groupings <- c("Station")}
  if (periodtype == "Seasonal") {groupings <- c("Station", "Season")}
  if (periodtype == "Monthly") {groupings <- c("Station", "Month")}

  # Summarise the data
  summary_table <- df %>%
    group_by(across(groupings)) %>%
    summarise(minimum = round(min(Value, na.rm = T), digits = 1),
              median = round(median(Value, na.rm = T), digits = 1),
              mean = round(mean(Value, na.rm = T), digits = 1),
              maximum = round(max(Value, na.rm = T), digits = 1)) %>%
    mutate(year_start = oldest, year_end = newest, .after = Station) %>%
    # Add the lats/lons back to each station
    left_join(df %>% summarise(Latitude = max(Latitude, na.rm = TRUE),
                               Longitude = max(Longitude, na.rm = TRUE)),
              by = groupings) %>%
    relocate(c(Latitude, Longitude), .after = Station)

}


#' Add a temperature covariate to a dataframe
#'
#' Read in temperature data from a file, smooth the data with a rolling mean
#' and add it to the input dataframe
#' 
#' @param df A dataframe containing the time series data
#' @param covariate One of "global" or "SH", indicating whether to use global or Southern Hemisphere temperature data
#' @param temperature_file The path to the file containing the temperature data
#' @param smoothing_period The number of years to use for smoothing the temperature data
#' @return A dataframe with the added temperature covariate
#' @export
add_temp_covariate <- function(df,
                               covariate = c("global", "SH"),
                               temperature_file = c("./data/temperature/HadCRUT5_1850-2022.global_t.global.0112.19811.raw.txt",
                                                    "./data/temperature/HadCRUT5_1850-2022.global_t.sh.0112.35323.raw.txt"),
                               #TODO make sure that code that calls this function has a valid temperature_file set
                               smoothing_period = 11) {
  
  require(zoo)
  
  # function to read in temperature data to use instead of year for rainfall covariate
  # and add it to the input dataframe
  
  # Read in the temperature data
  if (covariate == "global") {
    annual_temp <- readr::read_delim(
      file = temperature_file,
      col_names = c("daterange", "temperature"))
  }
  if (covariate == "SH") {
    annual_temp <- readr::read_delim(
      file = temperature_file,
      col_names = c("daterange", "temperature"))
  }
  
  # Smooth the temperature data using a rolling mean
  annual_temp_mod <- annual_temp %>%
    mutate(year_only = as.numeric(substr(x = daterange, start = 1, stop = 4)),
           # Year = ymd(paste0(year_only,"-01-01")),
           .before = daterange,
           temperature = round(
             zoo::rollmean(x = temperature, k = smoothing_period, fill = NA),
             digits = 4)) %>%
    select(-daterange)
  
  # add the annual temperature values to the DF
  df_temp <- df %>%
    mutate(year_only = year(Year)) %>%
    left_join(annual_temp_mod) %>%
    select(-year_only)
  
  df_temp

}


#' Add domain ownership information to a dataframe
#'
#' Add information from each shapefile in list to the supplied df (suggest summary tables)
#' 
#' @param df A dataframe containing the time series data
#' @param shapefile_list A list of shapefiles containing the domain boundaries
#' @return A dataframe with the added location columns for each station
#' @export
add_domain_ownership <- function(df, shapefile_list) {
  # function to add information about which domains each location belongs to,
  # e.g. NCRA region, NRM clusters/sub-/super-clusters, state/territory

  # load required libraries
  require(sf)
  sf_use_s2(FALSE)

  # define state list for mapping state names to abbreviations
  state_list <- list(
    "Australian Capital Territory" = "ACT",
    "New South Wales" = "NSW",
    "Northern Territory" = "NT",
    "Queensland" = "QLD",
    "South Australia" = "SA",
    "Tasmania" = "TAS",
    "Victoria" = "VIC",
    "Western Australia" = "WA",
    "unknown" = "other",
    "Other Territories" = "other"
  )

  # add information from each shapefile in list to the supplied df (suggest summary tables)
  for (shape in names(shapefile_list)) {
    print(shape)
    s <- read_sf(shapefile_list[shape])
    # select lats and lons from df
    points_df <- tibble(id = df$Station, Latitude = df$Latitude, Longitude = df$Longitude)
    # convert to sf geometry
    points_sf <- st_as_sf(points_df, coords = c("Longitude", "Latitude"), crs = 4326)
    # ensure they're on the same projection:
    points_sf <- st_transform(points_sf, crs = st_crs(s))
    points_with_domains <- st_join(points_sf, s) %>%
      # add information from each shapefile in list to the supplied df
      {if (shape == "ncra")
        transmute(., Station = id, NCRA_name = regionname, NCRA_label = label) %>%
          st_drop_geometry() else . } %>%
      {if (shape == "nrm_clust")
        transmute(., Station = id, NRM_cluster_name = label, NRM_cluster_label = code) %>%
          st_drop_geometry() else . } %>%
      {if (shape == "nrm_clust_sub")
        transmute(., Station = id, NRM_subcluster_name = label, NRM_subcluster_label = code) %>%
          st_drop_geometry() else . } %>%
      {if (shape == "nrm_clust_super")
        transmute(., Station = id, NRM_supercluster_name = label, NRM_supercluster_label = code) %>%
          st_drop_geometry() else . } %>%
      {if (shape == "state")
        transmute(., Station = id, State_name = STE_NAME21) %>%
          mutate(State_name = replace_na(State_name, "unknown")) %>%
          mutate(State_label = map_chr(State_name, ~ state_list[[.x]])) %>%
          st_drop_geometry() else . }

    # join with df:
    df <- df %>% left_join(points_with_domains, by = "Station", multiple = "first")

  }

  # return the df with added location columns for each station
  df

}


#' Test for estimates of linear trend
#'
#' @param df A dataframe containing the time series data
#' @param periodtype A string indicating the period type
#' @param oldest The oldest year in the period
#' @param newest The newest year in the period
#' @param trend_type A string indicating whether the trend is by year or temperature
#' @return A dataframe containing the estimates of the linear trend
#' @export
test_estimates_linear <- function(df,
                                  periodtype = period_type,
                                  oldest = first_year,
                                  newest = last_year,
                                  trend_type = "year") {
  
  # Check whether the trend type is "year" or "temperature"
  if (trend_type == "year") {
    # If trend type is "year", use year as the trend var
    trend_var <- sym("Year")
  }
  if (trend_type == "temperature") {
    # If trend type is "temperature", use temperature as the trend var
    trend_var <- sym("temperature")
  }
  
  # Load required libraries
  require(broom)
  
  # Group the data by station and period type
  # and estimate the linear trend
  df %>%
    # If trend type is "year", use year as the trend var
    {if(trend_type == "year") mutate(., Year = year(Year)) else .} %>%
    group_by(Station) %>%
    # If period type is seasonal, group by season as well
    {if(periodtype == "Seasonal") group_by(., Season, .add = TRUE) else .} %>%
    # If period type is monthly, group by month as well
    {if(periodtype == "Monthly") group_by(., Month, .add = TRUE) else .} %>%
    do(tidy(lm(Value ~ !!trend_var, .))) %>%
    # Filter out the results where the term is not equal to the trend var
    filter(term == trend_var) %>%
    # Select all columns except term
    select(-term) %>%
    # Round the p-value to 5 decimal places
    mutate(p.value = round(p.value, digits = 5)) %>%
    # Add the year start and end to the dataframe
    mutate(year_start = oldest, year_end = newest, .after = Station) %>%
    # Arrange the dataframe by station
    arrange(Station)
  
}


# modified_mk_test: Modified Mann-Kendall test
# 
# A wrapper around the Mann-Kendall test from the Kendall package.
# 
# @param x A data frame containing the data to test.
# @param ... Additional arguments to pass to the Mann-Kendall test.
# @return A tibble containing the p-value of the test.
modified_mk_test <- function(x, ...) {
  require(zoo)
  require(Kendall)
  
  # Convert the data frame to a zoo object with the year as the time
  ts <- read.zoo(mutate(x, Year = year(Year)) %>% ungroup() %>% select(Year, Value))
  
  # Perform the Mann-Kendall test
  result <- MannKendall(ts, ...)
  
  # Return the p-value of the test
  tibble(
    p.value_mk = round(result$sl[1], digits = 5)
  )

}


# test_estimates_mk: Test for trend using the Mann-Kendall test
# 
# This function tests for a trend in the data using the modified Mann-Kendall
# test.
# 
# @param df A data frame containing the data to test.
# @param periodtype The type of period over which the data is tested.
# @param oldest The oldest year in the data.
# @param newest The newest year in the data.
# @return A data frame containing the results of the test.
test_estimates_mk <- function(df,
                              periodtype = period_type,
                              oldest = first_year, newest = last_year) {

  # Load required libraries
  require(zoo)
  require(Kendall)
  
  # Group the data by station and period type
  # and estimate the linear trend
  df %>%
    group_by(Station) %>% # might have to change this part
    {if(periodtype == "Seasonal") group_by(., Season, .add = TRUE) else .} %>%
    {if(periodtype == "Monthly") group_by(., Month, .add = TRUE) else .} %>%
    group_modify(~ modified_mk_test(.x)) %>%
    mutate(year_start = oldest, year_end = newest, .after = Station) %>%
    arrange(Station)

}


#' make_test_table: Process test_summaries into statistics
#'
#' This function takes test_summaries (or test_summaries_temperature) df and
#' processes into statistics of all stations within specified region,
#' e.g. test_table for annual AUS:
#' Annual,n_total,n_pos,n_pos_sig,n_pos_sig_pc,n_neg,n_neg_sig,n_neg_sig_pc,
#' ratio_pos_neg_sig,mean_pc_mean,mean_pc_median,median_pc_mean,median_pc_median
#' Annual,251,144,15,6,107,6,2.4,2.5,8.64,9.49,6.72,6.98
#'
#' @param df A dataframe containing the test summaries
#' @param covariate A string indicating whether the test is by year or temperature
#' @param ys The oldest year in the period
#' @param ye The newest year in the period
#' @param periodtype A string indicating the period type
#' @param siglevel A numeric value indicating the significance level
#' @param domain A string indicating the domain of the stations
#' @param lon_lims A vector of two numeric values indicating the longitude limits
#' @param lat_lims A vector of two numeric values indicating the latitude limits
#' @param cluster_type A string indicating the type of cluster
#' @return A dataframe containing the statistics of the test
make_test_table <- function(df, covariate = c("year", "temperature"),
                            ys = first_year, ye = last_year,
                            periodtype = period_type, siglevel = sig_level,
                            domain = NULL, #c("aus", "vic", "syd", "seq"),
                            lon_lims = NULL, lat_lims = NULL,
                            cluster_type = NULL) {

  # takes test_summaries (or test_summaries_temperature) df and
  # processes into statistics of all stations within specified region,
  # e.g. test_table for annual AUS:
  # Annual,n_total,n_pos,n_pos_sig,n_pos_sig_pc,n_neg,n_neg_sig,n_neg_sig_pc,
  # ratio_pos_neg_sig,mean_pc_mean,mean_pc_median,median_pc_mean,median_pc_median
  # Annual,251,144,15,6,107,6,2.4,2.5,8.64,9.49,6.72,6.98


  if (! is.null(domain)) {
    domains <- c("aus", "vic", "syd", "seq")
    domain <- toupper(match.arg(domain, domains))
    if (is.null(lon_lims) | is.null(lat_lims)) {
      stop("Need to specify lon_lims and lat_lims for this domain.")
    }
    print(paste("Domain: ",domain))
  } else {
    if (! is.null(cluster_type)) {
      cluster_list <- list("nrm_clust" = "NRM_cluster",
                           "nrm_clust_sub" = "NRM_subcluster",
                           "nrm_clust_super" = "NRM_supercluster",
                           "state" = "State",
                           "ncra" = "NCRA")
      cluster_types <- names(cluster_list)
      cluster_type <- match.arg(cluster_type, cluster_types)
      print(paste("Cluster type: ",cluster_type))
      cluster_label <- paste0(cluster_list[[cluster_type]],"_label")
      # cluster_label <- quo(cluster_label)
      cluster_name <- paste0(cluster_list[[cluster_type]],"_name")
      # cluster_name <- quo(cluster_name)
    } else {
      stop("Need to specify either 'domain' or 'cluster_type'.")
    }
  }
  covariate <- match.arg(covariate)

  df %>%
    {if (! is.null(domain))
      filter(., between(Longitude, lon_lims[1], lon_lims[2]),
             between(Latitude, lat_lims[1], lat_lims[2])) %>%
        mutate(domain = domain) %>%
        group_by(domain) else . } %>%
    {if (! is.null(cluster_type))
      group_by(., !! sym(cluster_label), !! sym(cluster_name))
      else .} %>%
    mutate(time_period = paste(ys,ye,sep="-"),
           aggregation = periodtype) %>%
    group_by(time_period, .add = TRUE) %>%
    {if (period_type == "Seasonal")
      group_by(., Season, .add = TRUE) else . } %>%
    {if (period_type == "Monthly")
      group_by(., Month, .add = TRUE) else . } %>%
    {if (covariate == "year")
      mutate(., trend = trend * 10,
             trend_pc_mean = trend_pc_mean * 10,
             trend_pc_median = trend_pc_median * 10)
      else . } %>%
    summarise(n_total = n(),
              n_pos = sum(trend > 0),
              n_pos_sig = sum(trend > 0 & p.value_mk <= siglevel),
              n_pos_sig_pc = round(100 * n_pos_sig / n_total, digits = 1),
              n_neg = sum(trend < 0),
              n_neg_sig = sum(trend < 0 & p.value_mk <= siglevel),
              n_neg_sig_pc = round(100 * n_neg_sig / n_total, digits = 1),
              ratio_pos_neg_sig = round(n_pos_sig / n_neg_sig, digits = 2),
              # some percentage values are +/- infinity due to div-by-0
              mean_pc_mean = round(mean(
                if_else(trend_pc_mean == Inf, NA,
                        if_else(trend_pc_mean == -Inf, NA, trend_pc_mean)),
                na.rm=TRUE), digits = 2),
              mean_pc_median = round(mean(
                if_else(trend_pc_median == Inf, NA,
                        if_else(trend_pc_median == -Inf, NA, trend_pc_median)),
                na.rm=TRUE), digits = 2),
              median_pc_mean = round(median(trend_pc_mean, na.rm=TRUE), digits = 2),
              median_pc_median = round(median(trend_pc_median, na.rm=TRUE), digits = 2)) %>%
    relabel_aggregations(periodtype) %>%
    {if (! is.null(cluster_type)) filter(., ! is.na(!! sym(cluster_label))) else . }

}




#### Part 2: data visualization ####


plot_rxNday_trends <- function(df,
                               oldest = first_year, newest = last_year,
                               periodtype = period_type,
                               duration = dur,
                               valid_prop = proportion,
                               type = "location",
                               sig_only = FALSE,
                               siglevel = sig_level,
                               sig_type = c("t", "MK"),
                               trend_period = c("decade", "year"),
                               trend_measure = c("mm", "percent_median", "percent_mean"),
                               covariate = c("year", "temperature"),
                               domain = c("aus", "vic", "syd", "seq"),
                               title_fig = TRUE, caption_fig = TRUE,
                               lon_lims, lat_lims) {

  df <- df %>% filter(year_start == oldest, year_end == newest)

  season_list <- c("DJF", "MAM", "JJA", "SON")
  month_list <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  df <- df %>% ungroup() %>%
    {if (periodtype == "Seasonal")
      mutate(., Season = lvls_revalue(as.factor(Season), season_list)) else . } %>%
    {if (periodtype == "Monthly")
      mutate(., Month = lvls_revalue(as.factor(Month), month_list)) else . }

  trend_period <- match.arg(trend_period)
  sig_type <- match.arg(sig_type)
  periodtype <- match.arg(periodtype)
  domain <- match.arg(domain)
  covariate <- match.arg(covariate)
  trend_measure <- match.arg(trend_measure)

  if (covariate == "year") {
    if (trend_period == "decade") {
      df <- df %>% mutate(trend = trend * 10,
                          trend_pc_median = trend_pc_median * 10,
                          trend_pc_mean = trend_pc_mean * 10)
      trend_time_unit <- "decade"
    } else {
      trend_time_unit <- "year"
    }

    if (trend_measure == "mm") {
      # trend_label <- "Trend\n(mm/\nyear)"
      trend_label <- paste0("Trend\n(mm/\n",trend_time_unit,")")
    }
    if (trend_measure == "percent_median") {
      # trend_label <- "Trend\n(% of median\nper year)"
      trend_label <- paste0("Trend\n(% of median\nper ",trend_time_unit,")")
    }
    if (trend_measure == "percent_mean") {
      trend_label <- paste0("Trend\n(% per\n",trend_time_unit,")")
      # trend_label <- paste0("Trend\n(% of mean\nper ",trend_time_unit,")")
    }
  }

  if (covariate == "temperature") {
    if (trend_measure == "mm") {
      trend_label <- "Trend\n(mm/\u00B0C)"
    }
    if (trend_measure == "percent_median") {
      trend_label <- "Trend\n(% of median\nper \u00B0C)"
    }
    if (trend_measure == "percent_mean") {
      # trend_label <- "Trend\n(% of mean\nper \u00B0C)"
      trend_label <- "Trend\n(% / \u00B0C)"
    }
  }

  if (sig_type == "MK") {
    p_var <- sym("p.value_mk")
    sig_label <- "Mann-Kendall test"
  } else {
    if (sig_type == "t") {
      p_var <- sym("p.value")
      sig_label <- "t-test"
    } else {
      stop("Invalid value for 'sig_type': Must be one of 'MK', 't'")
    }
  }

  if (domain == "aus") {
    dom <- "Australia"
    lon_breaks <- seq(110,156,10)
    lat_breaks <- seq(-45,-10,10)
  }
  if (domain == "vic") {
    dom <- "Victoria"
    lon_breaks <- seq(140,151,2)
    lat_breaks <- seq(-40,-30,2)
  }
  if (domain == "syd") {
    dom <- "Sydney"
    lon_breaks <- seq(148,153,1)
    lat_breaks <- seq(-36,-31,1)
  }
  if (domain == "seq") {
    dom <- "SE Qld"
    lon_breaks <- seq(150,154,1)
    lat_breaks <- seq(-29,-24,1)
  }

  if (trend_measure == "mm") {
    trend_var <- sym("trend")
    metric_label <- ""
  }
  if (trend_measure == "percent_median") {
    trend_var <- sym("trend_pc_median")
    metric_label <- paste0("Percentage of median ",tolower(periodtype)," maxima")
  }
  if (trend_measure == "percent_mean") {
    trend_var <- sym("trend_pc_mean")
    metric_label <- paste0("Percentage of mean ",tolower(periodtype)," maxima")
  }


  baseplot <- df %>% ungroup() %>%
    filter(!!trend_var > 0, !!p_var > siglevel) %>%
    ggplot(aes(x = Longitude, y = Latitude, size = abs(!!trend_var))) +
    annotation_map(map_data("worldHires"), fill = "antiquewhite", col = "grey25") +
    coord_sf(xlim = lon_lims, ylim = lat_lims, expand = FALSE) +
    scale_x_continuous(breaks = lon_breaks) +
    scale_y_continuous(breaks = lat_breaks) +
    theme(panel.background = element_rect(fill = "lightsteelblue1"))

  if (type == "magnitude" || type == "m") {

    if (sig_only) {

      plot_final <- baseplot +
        geom_point(data = filter(df, !!trend_var < 0, !!p_var <= siglevel),
                   shape = 25, alpha = 0.65, fill = "red", colour = "red") +
        geom_point(data = filter(df, !!trend_var > 0, !!p_var <= siglevel),
                   shape = 24, alpha = 0.65, fill = "navyblue", colour = "navyblue")
      if (title_fig == TRUE) {
        plot_final <- plot_final +
          ggtitle(paste0("Magnitudes of significant linear trends in ",periodtype,
                       " Rx",duration,"hr: ", dom),
                subtitle = paste0("Period ",oldest,"-",newest,"; significance at ",
                                  siglevel," level via ", sig_label))
      }

      } else {

      plot_final <- baseplot +
        geom_point(shape = 2, alpha = 0.5, colour = "navyblue") +
        geom_point(data = filter(df, !!trend_var < 0, !!p_var > siglevel),
                   shape = 6, alpha = 0.5, colour = "red") +
        geom_point(data = filter(df, !!trend_var < 0, !!p_var <= siglevel),
                   shape = 25, alpha = 0.65, fill = "red", colour = "red") +
        geom_point(data = filter(df, !!trend_var > 0, !!p_var <= siglevel),
                   shape = 24, alpha = 0.65, fill = "navyblue", colour = "navyblue")
      if (title_fig == TRUE) {
        plot_final <- plot_final +
          ggtitle(paste0("Magnitudes of linear trends in ",periodtype," Rx",duration,"hr: ", dom),
                subtitle = paste0("Period ",oldest,"-",newest,"; significance at ",
                                  siglevel," level via ", sig_label))
      }

      }

  } else if (type == "location" || type == "l") {

    if (sig_only) {

      plot_final <- baseplot +
        geom_point(data = filter(df, !!trend_var < 0, !!p_var <= siglevel),
                   shape = 25, alpha = 0.65, size = 1.5, fill = "red", colour = "red") +
        geom_point(data = filter(df, !!trend_var > 0, !!p_var <= siglevel),
                   shape = 24, alpha = 0.65, size = 1.5, fill = "navyblue", colour = "navyblue") 
      if (title_fig == TRUE) {
        plot_final <- plot_final +
          ggtitle(paste0("Significant linear trends in ",periodtype," Rx",duration,"hr: ", dom),
                subtitle = paste0("Period ",oldest,"-",newest,"; significance at ",siglevel,
                                  " level via ", sig_label))
      }

      } else {

      plot_final <- baseplot +
        geom_point(shape = 2, alpha = 0.5, size = 1.5, colour = "navyblue") +
        geom_point(data = filter(df, !!trend_var < 0, !!p_var > siglevel),
                   shape = 6, alpha = 0.5, size = 1.5, colour = "red") +
        geom_point(data = filter(df, !!trend_var < 0, !!p_var <= siglevel),
                   shape = 25, alpha = 0.65, size = 1.5, fill = "red", colour = "red") +
        geom_point(data = filter(df, !!trend_var > 0, !!p_var <= siglevel),
                   shape = 24, alpha = 0.65, size = 1.5, fill = "navyblue", colour = "navyblue")
      if (title_fig == TRUE) {
        plot_final <- plot_final +
          ggtitle(paste0("Linear trends in ",periodtype," Rx",duration,"hr: ", dom),
                subtitle = paste0("Period ",oldest,"-",newest,"; significance at ",siglevel,
                                  " level via ", sig_label))
      }

      }

  } else {

    stop("Invalid value for 'type': Must be one of 'location', 'magnitude', 'l', 'm'")

  }

  if (periodtype == "Annual") {
    # do nothing!
    # plot_final <- plot_final
    # +
    #   # facet_wrap(~ Station, scales = "free_y", nrow = 4)
    #   facet_wrap(~ Station)
  } else {
    if (periodtype == "Seasonal") {
      period_type_plotting <- sym("Season")
    } else {
      # if (periodtype == "Monthly") {
      period_type_plotting <- sym("Month")
    }
    plot_final <- plot_final +
      # facet_wrap(period_type_plotting, scales = "free_y", nrow = 4)
      facet_wrap(period_type_plotting)
  }

  if (trend_measure != "mm") {
    caption_suffix <- paste0("\n",metric_label)
  } else {
    caption_suffix <- ""
  }

  if (is.null(valid_prop)) {
 
    if (caption_fig == TRUE) {
      plot_final +
        labs(size = trend_label,
           caption = metric_label)
    } else { # don't plot caption
      plot_final +
        labs(size = trend_label)      
    }

  } else {
 
    if (caption_fig == TRUE) {
      plot_final +
        labs(size = trend_label,
           caption = paste0("Minimum proportion of years with valid data: ",
                            valid_prop,"%",caption_suffix))
    } else { # don't plot caption

      plot_final +
        labs(size = trend_label)

    }
 
  }

}


