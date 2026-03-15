# R script to run GSDR processing and plotting functions

#### Set up parameters for script run

# Choose whether to use command line (testing = FALSE) or 
# directly specify parameters here (testing = TRUE)
testing <- FALSE
# testing <- TRUE

if (testing) {
  print("Testing mode!")
  period_type <- "Annual"
  # period_type <- "Seasonal"
  dur <- "1"
  record_length <- 0
  proportion <- 67
  sig_level <- 0.05
  first_year <- 1973
  last_year <- 2009
  plotting <- TRUE
  tables <- FALSE
  choropleths <- TRUE

} else {
  print("Command line mode!")

  library(optparse)

  option_list <- list(
    # required
    make_option(c("-p", "--period_type"), type = "character", default = "Annual",
                help = "Season aggregation (Annual / Seasonal)", metavar = "character"),
    make_option(c("-d", "--duration"), type = "integer", default = 1,
                help = "Time length of precipitation duration in hours (1, 3, 6, 12, 24)", metavar = "character"),
    make_option(c("-r", "--recordlength"), type = "integer", default = NULL,
                help = "Minimum record length", metavar = "character"),
    make_option(c("-f", "--firstyear"), type = "integer", default = 1961,
                help = "First year of analysis window", metavar = "character"),
    make_option(c("-l", "--lastyear"), type = "integer", default = 2015,
                help = "Last year of analysis window", metavar = "character"),
    make_option(c("-s", "--sig_level"), type = "double", default = 0.05,
                help = "Significance level", metavar = "character"),
    make_option(c("-x", "--proportion"), type = "integer", default = NULL,
                help = "Minimum required proportion (%) of years available in analysis window", metavar = "character"),
    make_option(c("-o", "--output"), type = "character", default = "both",
                help = "Option to specify whether to produce 'figures', 'tables', or 'both' (default)", metavar = "character")
  )

  opt_parser = OptionParser(option_list=option_list);
  opt = parse_args(opt_parser);

  if (is.null(opt$period_type)){
    print_help(opt_parser)
    stop("Input 'period_type' argument must be supplied", call.=FALSE)
  } else {
    period_type <- opt$period_type
  }
  if (is.null(opt$duration)){
    print_help(opt_parser)
    stop("Source 'duration' argument must be supplied", call.=FALSE)
  } else {
    dur <- as.character(opt$duration)
  }
  if (is.null(opt$recordlength)){
      print_help(opt_parser)
    stop("Record length argument must be supplied", call.=FALSE)
  } else {
    record_length <- as.character(opt$recordlength)
  }
  if (is.null(opt$firstyear)){
    print_help(opt_parser)
    stop("Input 'firstyear' argument must be supplied", call.=FALSE)
  } else {
    first_year <- as.integer(opt$firstyear)
  }
  if (is.null(opt$lastyear)){
    print_help(opt_parser)
    stop("Input 'lastyear' argument must be supplied", call.=FALSE)
  } else {
    last_year <- as.integer(opt$lastyear)
  }
  if (is.null(opt$sig_level)){
    print_help(opt_parser)
    stop("Input 'sig_level' argument must be supplied", call.=FALSE)
  } else {
    sig_level <- as.double(opt$sig_level)
  }
  if (is.null(opt$proportion)){
    # minimum required proportion not set
    proportion <- "0"
    paste("No minimum requirement set for proportion of period with complete data.")
  } else {
    proportion <- as.character(opt$proportion)
    paste0("Minimum proportion of years required in analysis window: ",proportion,"%")
  }
  if (is.null(opt$output)){
    plotting <- TRUE
    tables <- TRUE
    choropleths <- TRUE
  } else if (opt$output == "both") {
    plotting <- TRUE
    tables <- TRUE
    choropleths <- FALSE
  } else if (opt$output == "all") {
    plotting <- TRUE
    tables <- TRUE
    choropleths <- TRUE
  } else if (opt$output == "figures") {
    plotting <- TRUE
    tables <- FALSE
    choropleths <- TRUE
  } else if (opt$output == "tables") {
    plotting <- FALSE
    tables <- TRUE
    choropleths <- FALSE
  } else if (opt$output == "choropleths") {
    plotting <- FALSE
    tables <- FALSE
    choropleths <- TRUE
  }
}

basepath <- paste0("./output/station_trends/minlength_",record_length,
                  "yr/",first_year,"-",last_year,"/",dur,"hr/")

if (tables == TRUE) {dir.create(paste0(basepath,"tables"), recursive = TRUE)}
if (plotting == TRUE) {dir.create(paste0(basepath,"figures"), recursive = TRUE)}

##### Load functions
source('functions.R')
source('helpers.R')



#### Load data and perform processing to prepare for tables and plots

ts <- read_ts(period_type, dur, input_basedir = "./data")

# Get the number of annual/seasonal maximum observations for each station
# This is grouped by station, data source, and (if applicable) season
# The result is a dataframe with the station, data source, season (if applicable),
# and the number of maxima observations
max_count <- ts %>%
  # Group the data by station, data source, and (if applicable) season
  {if (period_type == "Annual") group_by(., Station, DataSource) else . } %>%
  {if (period_type == "Seasonal") group_by(., Station, DataSource, Season) else . }%>%
  # Summarise the data to get the number of maxima observations for each station
  summarise(n_maxima = n())

# ts_gtN is a dataframe with the stations that have at least N years of over 80% completeness
# This is the result of joining the time series data with the maximum number of observations for each station
# The result is a dataframe with the station, year, season (if applicable), and the maximum value
ts_gtN <- max_count %>%
  # Ungroup the data by station
  ungroup() %>%
  # Filter out stations with less than 'record_length' (N) years
  filter(n_maxima >= as.numeric(record_length)) %>%
  # Select the station, year, and season (if applicable) columns
  # Drop the DataSource and n_maxima columns
  select(-DataSource,-n_maxima) %>%
  # Join the maximum number of observations for each station with the time series data
  # If the period type is Annual, join on Station
  # If the period type is Seasonal, join on Station and Season
  {if (period_type == "Annual") left_join(., ts, join_by(Station)) else . } %>%
  {if (period_type == "Seasonal") left_join(., ts, join_by(Station, Season)) %>%
      # Arrange the data by station, year, and season
      arrange(Station, Year, Season) %>%
      # Move the year column after the station column
      relocate(Year, .after = Station) else . }

# filter out years with low completeness: require at least 80% completeness
ts_gtN_80pc <- ts_gtN %>% filter(Completeness >= 80)

# - eliminate individual years that have poor completeness -> annmax_count_gt20y_80pc
annmax_count_gtN_80pc <- filter_stations(df = ts_gtN_80pc, period_type = period_type)
# Get the number of stations with at least 10 years of over 80% completeness
# (10 years being the minimum length of trends examined in thie study)
annmax_count_gtN_80pc_10y <- filter_stations(df = ts_gtN_80pc, period_type = period_type, recordlength = 10)

# how many stations do we have for each block (year/season)?
n_annmax_count_gtN_80pc <- annmax_count_gtN_80pc %>%
  {if (period_type == "Seasonal") group_by(., Season) else . } %>%
  summarise(n_stations = n())


# - get time series for stations meeting minimum years and completeness thresholds -> e.g. ts_gt20y_80pc
# annmax_count_gtN_80pc has stations (and season) with enough data
# NOTE: when minimum record length is set to 0, this gives ALL stations with ANY years of over 80% completeness!
# Join with time series from ts_gtN_80pc
ts_gtN_80pc_stations <- annmax_count_gtN_80pc %>%
  {if (period_type == "Annual") select(., Station) %>%
      left_join(ts_gtN_80pc, by = "Station") else . } %>%
  {if (period_type == "Seasonal") select(., Station, Season) %>%
      left_join(ts_gtN_80pc, by = c("Station", "Season")) %>%
      arrange(Station, Year, Season) %>%
      relocate(Year, .after = Station) else . }

ts_gtN_80pc_stations_10y <- annmax_count_gtN_80pc_10y %>%
  {if (period_type == "Annual") select(., Station) %>%
      left_join(ts_gtN_80pc, by = "Station") else . } %>%
  {if (period_type == "Seasonal") select(., Station, Season) %>%
      left_join(ts_gtN_80pc, by = c("Station", "Season")) %>%
      arrange(Station, Year, Season) %>%
      relocate(Year, .after = Station) else . }

if (period_type == "Annual") {
  # code to plot the number of stations per season
  if (period_type == "Annual") {
    # do nothing
    period_type_plotting <- "Annual"
  } else {
    if (period_type == "Seasonal") {
      period_type_plotting <- sym("Season")
    }
  }

  if (plotting == TRUE) {
    valid_stations_hist_10y <- ts_gtN_80pc_stations_10y %>% ggplot(aes(x = Year)) +
      geom_histogram(binwidth = 1, fill = "darkorange", alpha = 0.9) +
      scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(1900,2020,20)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), name = 'Number of stations')+
      theme_bw(base_size = 7)

    # map of locations of stations and their record lengths
    annmax_count_gtN_80pc_10y_coords <- left_join(annmax_count_gtN_80pc_10y, select(.data = ts_gtN_80pc, Station, Latitude, Longitude), by = 'Station') %>% unique()

    stations_by_length_10 <- 
      annmax_count_gtN_80pc_10y_coords %>%
      filter(n_years >= 10) %>% 
      ggplot(aes(x = Longitude, y = Latitude, colour = n_years)) + 
      # "fill" is land colour; "col" is border colour.
      # Ocean colour needs to be set as background of panel below.
      annotation_map(map_data("worldHires"), fill = "white", col = "grey25") +
        geom_point(alpha = 0.65, size = 0.25) +
      paletteer::scale_colour_paletteer_c("viridis::viridis", direction = -1, limits = c(10,60), oob = scales::oob_squish, 
            guide = "colbar", name = "Record\nlength\n(years)") +
      coord_sf(xlim = aus_lon_limits, ylim = aus_lat_limits, expand = FALSE) +
      theme_bw(base_size = 7)

    stationmap_with_hist10 <- valid_stations_hist_10y + stations_by_length_10 + plot_annotation(tag_levels = 'a')
    ggsave('results/figure1_stationmap_with_histogram_10y.pdf', plot = stationmap_with_hist10, width = 5.5, height = 2.25, units = 'in')

  }

}

# - filter time series for set epoch -> e.g. ts_gt20y_80pc_1981_2015
# this reduces the time series to those stations with adequate years - either via 'n_years' or 'proportion'
ts_gtN_80pc_period <- valid_years_in_period(df = ts_gtN_80pc)
# ts_gtN_80pc_period_temperature <- ts_gtN_80pc_period %>%
#   add_temp_covariate(covariate = "global", smoothing_period = 11, 
#                       temperature_file = "./data/temperature/HadCRUT5_1850-2022.global_t.global.0112.19811.raw.txt")

# - summary stats per station/series -> e.g. rx1hr_summary_gt20y_1981_2015
# Create summary stats tables for each station:
rx1hr_summary_gtN_period <- summarise_filtered_ts(ts_gtN_80pc_period)
# rx1hr_summary_gtN_period_temperature not required, gives same result


# - test estimates (linear trend, mann-kendall significance) -> e.g.

linear_tests <-
  test_estimates_linear(ts_gtN_80pc_period, trend_type = "year")

# linear_tests_temperature <-
#   test_estimates_linear(ts_gtN_80pc_period_temperature,
#                         trend_type = "temperature")

mk_tests <- test_estimates_mk(ts_gtN_80pc_period)
# mk_tests_temp <- test_estimates_mk(ts_gtN_80pc_period_temperature)

test_results <- linear_tests %>%
  {if (period_type == "Annual")
    left_join(., mk_tests, by = c("Station", "year_start", "year_end")) else . } %>%
  {if (period_type == "Seasonal")
    left_join(., mk_tests, by = c("Station", "year_start", "year_end", "Season")) else . }

# test_results_temp <- linear_tests_temperature %>%
#   {if (period_type == "Annual")
#     left_join(., mk_tests_temp, by = c("Station", "year_start", "year_end")) else . } %>%
#   {if (period_type == "Seasonal")
#     left_join(., mk_tests_temp, by = c("Station", "year_start", "year_end", "Season")) else . }

# join these results with the statistical summary df:
test_summaries <- test_results %>%
  {if (period_type == "Annual")
    arrange(., Station, year_start, year_end)
    else . } %>%
  {if (period_type == "Seasonal")
    arrange(., Station, year_start, year_end, Season)
    else . } %>%
  full_join(x = rx1hr_summary_gtN_period) %>%
  rename(trend = estimate) %>%
  # add percentage change in trend per year
  mutate(trend_pc_mean = trend * 100 / mean,
        trend_pc_median = trend * 100 / median,
        .after = trend) %>%
  add_domain_ownership(shapefile_list) %>%
  relabel_aggregations(period_type)

# test_summaries_temperature <- test_results_temp %>%
#   {if (period_type == "Annual")
#     arrange(., Station, year_start, year_end) else . } %>%
#   {if (period_type == "Seasonal")
#     arrange(., Station, year_start, year_end, Season) else . } %>%
#   full_join(x = rx1hr_summary_gtN_period) %>%
#   rename(trend = estimate) %>%
#   # add percentage change in trend per degree
#   mutate(trend_pc_mean = trend * 100 / mean,
#         trend_pc_median = trend * 100 / median,
#         .after = trend) %>%
#           add_domain_ownership(shapefile_list) %>%
#   relabel_aggregations(period_type)


# print("Processing steps complete!")



#########################################################
##################      TABLES      #####################
#########################################################



if (tables == TRUE) {

# - tables of significance

  # loop through various combinations of required options:
  # domains; covariate
  for (covariate in c("year")) {
  # for (covariate in c("year", "temperature")) {

    if (covariate == "year") {
      df_test <- test_summaries
      cov_label <- ""
    } else {
      df_test <- test_summaries_temperature
      cov_label <- "-temperature"
    }

    for (d in domain_list) {

      # print(paste("Creating tables for domain",toupper(d),"w/ covariate",covariate))
      outfile_domain <- 
        paste0(basepath,"tables/station_trends-mk",sig_level,cov_label,"_",dur,"hr_",first_year,
                "-",last_year,"_",toupper(d),"_",period_type,"_min",proportion,"pc.csv")
      
      if (file.exists(outfile_domain)) {next}

      lonlims <- get(eval(paste0(d,"_lon_limits")))
      latlims <- get(eval(paste0(d,"_lat_limits")))

      test_table <- make_test_table(df_test, covariate = covariate, domain = d,
                                    lon_lims = lonlims, lat_lims = latlims)
      readr::write_csv(test_table, file = outfile_domain)

    }

    # Loop through various clusters as defined by shapefiles:
    for (clustering in names(shapefile_list)) {

      outfile_cluster <- 
        paste0(basepath,"tables/station_trends-mk",sig_level,cov_label,"_",dur,
                "hr_",first_year,"-",last_year,"_",toupper(clustering),"_",
                period_type,"_min",proportion,"pc.csv")
    
      if (file.exists(outfile_cluster)) {next}
      
      test_table_cluster <-
        make_test_table(df_test, covariate = covariate, cluster_type = clustering)


      # > names(shapefile_list)
      # [1] "ncra"            "nrm_clust"       "nrm_clust_sub"   "nrm_clust_super" "state"
      if (clustering == "nrm_clust") {
        label_col <- sym("NRM_cluster_label")
        category_col <- sym("NRM_cluster_name")
      }
      if (clustering == "nrm_clust_sub") {
        label_col <- sym("NRM_subcluster_label")
        category_col <- sym("NRM_subcluster_name")
      }
      if (clustering == "nrm_clust_super") {
        label_col <- sym("NRM_supercluster_label")
        category_col <- sym("NRM_supercluster_name")
      }
      if (clustering == "state") {
        label_col <- sym("State_label")
        category_col <- sym("State_name")
        s_col <- sym("label")
      }
      if (clustering == "ncra") {
        label_col <- sym("NCRA_label")
        category_col <- sym("NCRA_name")
      }


      readr::write_csv(test_table_cluster, file = outfile_cluster)

    }
  }
}
# end tables section


#########################################################
##################     FIGURES      #####################
#########################################################

if (plotting == TRUE) {

  dom_list <- c("AUS")
  sig_list <- c(TRUE, FALSE)
  type_list <- c("magnitude") # c("location", "magnitude")
  trend_type_list <- c("percent_mean") # c("mm", "percent_median", "percent_mean")
  trend_source_list <- c("year") # c("year", "temperature")

  # plots of station trends and significance
  sigtype <- "MK"

  dom_plot_list <- list()
  for (dom in dom_list) {

    if (dom == "AUS") {
      lons <- aus_lon_limits
      lats <- aus_lat_limits
    }
    if (dom == "VIC") {
      lons <- vic_lon_limits
      lats <- vic_lat_limits
    }
    if (dom == "SYD") {
      lons <- syd_lon_limits
      lats <- syd_lat_limits
    }
    if (dom == "SEQ") {
      lons <- seq_lon_limits
      lats <- seq_lat_limits
    }

    sig_plot_list <- list()
    for (sig in sig_list) {

      if (sig) { sig_label <- "sig-only" } else { sig_label <- "all" }

      type_plot_list <- list()
      for (type in type_list) {

        trend_type_plot_list <- list()
        for (trend_type in trend_type_list) {

          if (trend_type == "mm") {
            type_label <- trend_type
          } else {
            type_label <- gsub("_", "-", trend_type)
          }

          title_plot_list <- list()
          for (plot_title in c(TRUE, FALSE)) {
  
            if (plot_title == TRUE) {
              file_title_text <- ""
              plot_title_label <- "title"
            } else {
              file_title_text <- "_notitle"
              plot_title_label <- "notitle"
            }

            trend_source_plots_list <- list()
            for (trend_source in trend_source_list) {

              if (trend_source == "year") {
                input_df <- test_summaries
                trendperiod <- "decade"

                fig <- plot_rxNday_trends(df = input_df,
                  periodtype = period_type,
                  duration = dur,
                  sig_only = sig,
                  sig_type = "MK",
                  domain = tolower(dom),
                  type = type,
                  covariate = trend_source,
                  trend_measure = trend_type,
                  trend_period = trendperiod,
                  title_fig = plot_title,
                  caption_fig = plot_title,
                  lon_lims = lons,
                  lat_lims = lats)

              }
              if (trend_source == "temperature") {
                input_df <- test_summaries_temperature

                fig <- plot_rxNday_trends(df = input_df,
                    periodtype = period_type,
                    duration = dur,
                    sig_only = sig,
                    sig_type = "MK",
                    domain = tolower(dom),
                    type = type,
                    covariate = trend_source,
                    trend_measure = trend_type,
                    title_fig = plot_title,
                    caption_fig = plot_title,
                    lon_lims = lons,
                    lat_lims = lats)

              }
                
              # set up output file name:
              if (sigtype == "MK") { mk_label <- "-mk" } else { mk_label <- "" }

              trend_source_plots_list[[trend_source]] <- fig
              
              rm(fig)

            }
            title_plot_list[[plot_title_label]] <- trend_source_plots_list
          }
          trend_type_plot_list[[trend_type]] <- title_plot_list
        }
        type_plot_list[[type]] <- trend_type_plot_list
      }
      sig_plot_list[[sig_label]] <- type_plot_list
    }
    dom_plot_list[[dom]] <- sig_plot_list
  }

  if (trend_source == "year") {
    if (period_type == "Annual") { 
      # Save Fig 3a:
      p3a <- dom_plot_list$AUS$all$magnitude$percent_mean$notitle$year +
        scale_size_continuous(breaks = seq(0,50,10), labels = seq(0,50,10)) +
        theme(legend.position="none") 
      save(p3a, file = "output/fig3a.rda")
    }
    if (period_type == "Seasonal") {
      # Create Fig 3b:
      p3b <- dom_plot_list$AUS$`sig-only`$magnitude$percent_mean$notitle$year +
      scale_size_continuous(breaks = seq(0,50,10), labels = seq(0,50,10))

      if (file.exists("output/fig3a.rda")) {

        print("Combining plots to create Fig 3...")
        
        # try combining these plots:
        load("output/fig3a.rda")

        fig3 <- p3a + p3b + plot_annotation(tag_levels = 'a')
        ggsave("results/figure3.pdf", plot = fig3, width = 5.5, height = 2.25, units = 'in', scale = 2)

        file.remove("output/fig3a.rda")

      } else {
        save(p3b, file = "output/fig3b.rda")
      }
    }

  } else if (trend_source == "temperature") {
    if (period_type == "Annual") { 
      # Save Fig 3a for temperature:
      p3a_temp <- dom_plot_list$AUS$all$magnitude$percent_mean$notitle$temperature +
        scale_size_continuous(breaks = seq(0,50,5), labels = seq(0,50,5)) +
        theme(legend.position="none") 
      save(p3a_temp, file = "output/fig3a_temp.rda")
    }
    if (period_type == "Seasonal") {
      # Save Fig 3b:
      p3b_temp <- dom_plot_list$AUS$`sig-only`$magnitude$percent_mean$notitle$temperature +
      scale_size_continuous(breaks = seq(0,50,5), labels = seq(0,50,5))

      if (file.exists("output/fig3a_temp.rda")) {
      
        print("Combining plots to create Fig 3 (temperature)...")
    
        # try combining these plots:
        load("output/fig3a_temp.rda")
      
        fig3_temp <- p3a_temp + p3b_temp + plot_annotation(tag_levels = 'a')
        ggsave("results/figure3_temp.pdf", plot = fig3_temp, width = 5.5, height = 2.25, units = 'in', scale = 2)

        file.remove("output/fig3a_temp.rda")

      } else {
        save(p3b_temp, file = "output/fig3b_temp.rda")
      }
    }
  }
  # end plotting section
}


if (choropleths) {


  for (clustering in names(shapefile_list)) {

    s <- read_sf(shapefile_list[clustering])

    if (clustering == "nrm_clust") {
      label_col <- sym("NRM_cluster_label")
      category_col <- sym("NRM_cluster_name")
      fname_prefix <- "NRM_cluster"
    }
    if (clustering == "nrm_clust_sub") {
      label_col <- sym("NRM_subcluster_label")
      category_col <- sym("NRM_subcluster_name")
      fname_prefix <- "NRM_subcluster"
    }
    if (clustering == "nrm_clust_super") {
      label_col <- sym("NRM_supercluster_label")
      category_col <- sym("NRM_supercluster_name")
      fname_prefix <- "NRM_supercluster"
    }
    if (clustering == "state") {
      label_col <- sym("State_label")
      category_col <- sym("State_name")
      s_col <- sym("label")
      fname_prefix <- "State"
    }
    if (clustering == "ncra") {
      label_col <- sym("NCRA_label")
      category_col <- sym("NCRA_name")
      fname_prefix <- "NCRA"
    }
  }


  # print(clustering)
  # s <- read_sf(shapefile_list[clustering])
  # create summary spatial statistics
  stats_spatial <- test_summaries %>%
  # stats_spatial <- test_summaries_temperature %>%
    group_by(!!label_col, !!category_col) %>%
    {if (period_type == "Seasonal")
      group_by(., Season, .add = TRUE) else . } %>%
    summarise(n = n(),
              mean_trend = round(mean(trend_pc_mean, na.rm = TRUE), digits = 1)) %>%
    drop_na()

  range_lim <- max(abs(min(stats_spatial$mean_trend)), abs(max(stats_spatial$mean_trend)))

  # choropleth_plot <-
  #   stats_spatial %>%
  #   left_join(s, by = join_by(!!label_col == "code")) %>%
  #   st_as_sf() %>%
  #   ggplot() +
  #   # colour in land masses outside Aus
  #   annotation_map(map_data("worldHires"), col = "grey25", alpha = 0.20) +
  #   geom_sf(aes(fill = mean_trend)) +
  #     scale_fill_paletteer_c(palette = "scico::vik", direction = -1,
  #                       name = "Mean trend\n(% of mean\nper decade)",
  #                       limits = c(-1*range_lim, range_lim)) +
  #   coord_sf(xlim = aus_lon_limits, ylim = aus_lat_limits, crs = sf::st_crs(4326)) +
  #   # add coastline for Aus over the top of fill
  #   annotation_map(map_data("worldHires"), col = "grey25", alpha = 0) +
  #   geom_sf_label(aes(label = paste(!!label_col, mean_trend, "\nn =", n)), size = 3) +
  #   theme_bw() +
  #     labs(y = "Latitude", x = "Longitude")
  
  #   if (period_type == "Seasonal") choropleth_plot <- choropleth_plot + facet_wrap(~ Season)

  # ggsave(paste0('results/trends_for_', fname_prefix, '_regions_', period_type, '.pdf'), plot = choropleth_plot, width = 5.5, height = 4.5)
  
  ########################################################################################
  ### NRM cluster choropleth plot:  ######################################################
  ########################################################################################
  clusters_filename <- paste0('results/figure2_', fname_prefix, '_regions.pdf')

  if (!file.exists(clusters_filename)) {
    
    clusters_plot <- 
      stats_spatial %>%
      left_join(s, by = join_by(!!label_col == "code")) %>%
      st_as_sf() %>%
      ggplot() +
        # colour in land masses outside Aus
        annotation_map(map_data("worldHires"), col = "grey25", alpha = 0.20) +
        geom_sf(aes(fill = !!label_col))   +
          scale_fill_paletteer_d(palette = "tidyquant::tq_dark", 
                            name = "NRM\ncluster") +
        coord_sf(xlim = aus_lon_limits, ylim = aus_lat_limits, crs = sf::st_crs(4326)) +
        # add coastline for Aus over the top of fill
        annotation_map(map_data("worldHires"), col = "grey25", alpha = 0) +
        geom_sf_label(aes(label = paste(!!label_col)), size = 3) +
        theme_bw(base_size = 7) +
        labs(y = "Latitude", x = "Longitude") 
      
    # clusters_plot
    ggsave(clusters_filename, plot = clusters_plot, width = 5.5, height = 4.5)
 
  }
  
# end choropleth section
}

