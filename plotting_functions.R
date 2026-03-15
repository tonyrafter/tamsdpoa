# Plotting functions for the tamsdpoa paper

### Load libraries
library(tidyverse)
library(gtable)
library(grid)
library(vroom)
library(paletteer)
library(scico)
library(patchwork)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Check if is a list
.is_list <- function(x){
  inherits(x, "list")
}


get_legend <- function(p, position = NULL){

  if(.is_list(p)){
    continue <- TRUE
    i <- 1
    while(i <= length(p) & continue){
      leg <- .get_legend(p[[i]], position = position)
      if(!is.null(leg)) continue <- FALSE
      i <- i+1
    }
  }
  else{
    leg <- .get_legend(p, position = position)
  }
  leg
}

# Return legend for one plot
.get_legend <- function(p, position = NULL){

  if(is.null(p)) return(NULL)
  if(!is.null(position)){
    p <- p + theme(legend.position = position)
  }
  tmp <- ggplot_gtable(ggplot_build(p))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if(length(leg) > 0) leg <- tmp$grobs[[leg]]
  else leg <- NULL
  leg
}

as_ggplot <- function(x){

  # Open null device to avoid blank page before plot------
  # see cowplot:::as_grob.ggplot
  null_device <- base::getOption(
    "ggpubr.null_device",
    default = cowplot::pdf_null_device
  )
  cur_dev <- grDevices::dev.cur()
  # Open null device to avoid blank page before plot
  null_device(width = 6, height = 6)
  null_dev <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(null_dev)
    if (cur_dev > 1) grDevices::dev.set(cur_dev)
  })

  # Convert to ggplot-------------
  cowplot::ggdraw() +
    cowplot::draw_grob(grid::grobTree(x))
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

trend_bars <- function(df) {
  df %>%
  mutate(n_pos_non_sig_pc = if_else(n_pos_sig_pc > 2.5, 2.5, n_pos_sig_pc),
         n_neg_non_sig_pc = if_else(n_neg_sig_pc > 2.5, 2.5, n_neg_sig_pc),
         .after = period) %>% 
  ggplot(aes(x = accumulation, y = n_pos_sig_pc)) +
  # add bar of percentage of positive significant stations
  geom_col(fill = "blue", alpha = 0.8, width = 0.8) +
  xlab("Accumulation (hr)") +
  # add bar of percentage of negative significant stations
  geom_col(
         aes(x = accumulation, y = -1 * n_neg_sig_pc),
         fill = "red", alpha = 0.8, width = 0.8) +
  # add bar of percentage of positive NON-significant stations
  geom_col(
    aes(x = accumulation, y = -1 * n_neg_non_sig_pc),
    fill = "white", alpha = 0.6, width = 0.8) +
  # add bar of percentage of negative NON-significant stations
  geom_col(
    aes(x = accumulation, y = n_pos_non_sig_pc),
    fill = "white", alpha = 0.6, width = 0.8) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = symmetric_limits) +
  scale_y_continuous(
    # Features of the first axis
    name = "Percentage of stations (%) [bars]",
    # Add a second axis and specify its features
    sec.axis = sec_axis(
      transform = ~.*coeff,
      breaks = seq(-100, 100, 20), labels = seq(-100, 100, 20),
      name=paste0("Mean trend per decade (% of station mean) [black points]"))
  ) +
  geom_point(data = filtered_df, aes(x = accumulation, y = mean_pc_mean / coeff),
             shape = 16,
             size = 3) 
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

trend_matrix_plot_by_domain_binned <- function(df, accum = c(1, 3, 6, 12, 24), 
                              seas = c("Annual", "DJF", "MAM", "JJA", "SON"),
                              min_years = 10, max_years = Inf, scaling = 7,
                              xtype = c("mid", "start", "end"), 
                              ytype = c("num_years", "end", "start"), 
                              # start = 1960, end = 2015,
                              fill_var = c("mean", "sigdiff", "nstations"),
                              lim_value = 50, 
                              lim_range_type = c("split", "positive"),
                              facet_by = c("domain", "season", "annual", "accum"),
                              dom = NULL, 
                              covariate = c("temperature", "year"),
                              domains = c("aus_nrm_clust", "nrm_clust", 
                                          "aus_nrm_sub_clust", "nrm_sub_clust",
                                          "nrm_super_clust", "state", "ncra"),
                              title = FALSE, rotate_x_labels = 0,
                              show_legend = TRUE) {

  require(tidyverse)
  require(scico)

  # match args:
  accum <- match.arg(as.character(accum[1]), choices = c(1, 3, 6, 12, 24))
  seas <- match.arg(seas)
  xtype <- match.arg(xtype)
  ytype <- match.arg(ytype)
  fill_var <- match.arg(fill_var)
  facet_by <- match.arg(facet_by)
  domains <- match.arg(domains)
  lim_range_type <- match.arg(lim_range_type)
  covariate <- match.arg(covariate)
  
  if (covariate == "temperature") {
    trend_unit <- "\u00B0C"
  }
  if (covariate == "year") {
    trend_unit <- "decade"
  }
  ### Usually use combination
  ### xtype 'start year'  and ytype 'end year', or
  ### xtype 'middle year' and ytype 'nyears' (triangle)

  # set up x and y vars
  if (xtype == "start") {
    xlabel <- "Start year"
    xvar <- sym("start_year")
  }
  if (xtype == "mid") {
    xlabel <- "Middle year"
    xvar <- sym("mid_year")
  }
  if (xtype == "end") {
    xlabel <- "End year"
    xvar <- sym("end_year")
  }
  if (ytype == "start") {
    ylabel <- "Start year"
    yvar <- sym("start_year")
  }
  if (ytype == "end") {
    ylabel <- "End year"
    yvar <- sym("end_year")
  }
  if (ytype == "num_years") {
    ylabel <- "Number of years"
    yvar <- sym("n_years")
  }

  # set up fill vars
  if (fill_var == "mean") {
    fillvar <- sym("mean_pc_mean")
    fill_label <- paste0("Mean trend\nchange\n(% / ",trend_unit,")")
    title_start <- "Trend magnitudes in"
  }
  if (fill_var == "sigdiff") {
    fillvar <- sym("sig_pc_diff")
    df <- df %>% mutate(sig_pc_diff = n_pos_sig_pc - n_neg_sig_pc)
    fill_label <- "%"
    title_start <- "Difference in percentage of stations with positive and negative trends in\n"
  }
  if (fill_var == "nstations") {
    fillvar <- sym("n_total")
    fill_label <- "Number of\nstations"
    title_start <- "Total number of stations in trends of"
  }

  # set up facetting var
  if (facet_by == "domain") {
    # also need to filter one _season_ only
    facet_var <- sym("domain")
    df <- df %>% filter(season == seas)
    if (domains == "aus_nrm_clust") {
      facet_levels <- c("AUS", "MN", "WT", "R", "CS", "EC", "SSWF", "MB", "SS")
    }
    if (domains == "nrm_clust") {
      facet_levels <- c("MN", "WT", "R", "CS", "EC", "SSWF", "MB", "SS")
    }
    if (domains == "nrm_sub_clust") {
      facet_levels <- c("MNW", "MNE", "WT", "RS", "RN", "CS", "ECN", 
                        "SSWFW", "SSWFE", "MB", "ECS", "SSTW", "SSTE", "SSVW", "SSVE")
    }
    if (domains == "aus_nrm_sub_clust") {
      facet_levels <- c("AUS", "MNW", "MNE", "WT", "RS", "RN", "CS", "ECN", 
                        "SSWFW", "SSWFE", "MB", "ECS", "SSTW", "SSTE", "SSVW", "SSVE")
    }
    if (domains == "nrm_super_clust") {
      facet_levels <- c("NA", "EA", "R", "SA")
    }
    if (domains == "ncra") {
      facet_levels <- c("WA North", "NT", "QLD North", "WA South", "SA", "QLD South", "TAS", "VIC", "NSW")
    }
    if (domains == "state") {
      facet_levels <- c("WA", "NT", "QLD", "SA", "NSW", "ACT", "TAS", "VIC", "other")
    }
  }
  if (facet_by == "season") {
    # also need to filter one _domain_ only
    facet_var <- sym(facet_by)
    df <- df %>% filter(domain == dom, season_type == "Seasonal")
    facet_levels <- c("DJF", "MAM", "JJA", "SON")
  }
  if (facet_by == "annual") {
    # also need to filter one _domain_ only
    facet_var <- sym("season")
    df <- df %>% filter(domain == dom, season == "Annual")
    facet_levels <- c("Annual")
  }
  if (facet_by == "accum") {
    # also need to filter one _domain_, _season_,  only
    facet_var <- sym("accumulation")
    df <- df %>% filter(domain == dom, season == seas)
    facet_levels <- c(1, 3, 6, 12, 24)
  }

  # set up correct facetting for domains
  lim_value_floor <- floor(lim_value / scaling) * scaling # gives us the nearest multiple of CC-scaling below our specified range
  lim_nbins <- lim_value_floor / scaling
  if (lim_range_type == "positive") {
    lim_range <- c(0, lim_value_floor + 1)
    lim_breaks <- seq(0, max(lim_value_floor), scaling)
    custom_palette <- c(paletteer::paletteer_c("viridis::inferno", n = lim_nbins))
  } else {
    lim_range <- c(-1 * (lim_value_floor + 1), lim_value_floor + 1) 
    lim_breaks <- c(-1000000, seq(lim_value_floor*-1, lim_value_floor, scaling), 1000000)
    custom_palette <- c("coral1", paletteer::paletteer_c("scico::vik", direction = -1, n = (2*lim_nbins)), "turquoise3")
  }


  # filter and add vars to df for plotting
  table_input <- df %>% 
    # all_tables_nrm_clust %>% 
    {if (facet_by != "accum") filter(., accumulation == accum) else . } %>% 
    filter(
      between(n_years, min_years, max_years)
    ) %>% 
    mutate(
      start_year = as.numeric(str_split_i(period, "-", 1)),
      end_year = as.numeric(str_split_i(period, "-", 2)),
      mid_year = (start_year + end_year)/2,
      .after = period) 
  
  # create plot
  p1 <- table_input %>% 
    ggplot(aes(x = !!xvar, y = !!yvar, fill = !!fillvar)) +
    geom_tile(width = 1, height = 1) +
    scale_fill_stepsn(
      colours = custom_palette,
      breaks = lim_breaks,
      limits = lim_range) +
    coord_fixed() + 
    labs(x = xlabel, y = ylabel, fill = fill_label) +
    facet_wrap(vars(factor({{facet_var}}, levels = facet_levels)))
    # add title if requested
  if (title == TRUE) {
    if (facet_by == "domain") {
      title_text <- paste(title_start, seas, "maximum", as.character(accum), "hr precipitation")
    } else if (facet_by == "season") {
      title_text <- paste(title_start, dom, "seasonal maximum", as.character(accum), "hr precipitation")
    } else if (facet_by == "annual") {
      title_text <- paste(title_start, dom, "annual maximum", as.character(accum), "hr precipitation")
    } else if (facet_by == "accum") {
      title_text <- paste(title_start, dom, seas, "maximum precipitation for all accumulations")
    }
    p1 <- p1 + theme_bw(base_size = 7) + 
      ggtitle(title_text)
    } else {
      p1 <- p1 + theme_bw(base_size = 7) 
  }
  if (show_legend == FALSE) {
    # remove legend from plot
    p1 <- p1 + theme(legend.position = "none")
  }
  if (rotate_x_labels != 0) {
    p1 + theme(axis.text.x = element_text(angle = rotate_x_labels, hjust = 1))
  } else {
    p1
  }
}
