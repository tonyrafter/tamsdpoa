# R script to create figures from combined tables of all periods, domains, seasons etc.
library(tidyverse)
library(vroom)
library(paletteer)
library(scico)
library(patchwork)

source('./plotting_functions.R')

all_tables_aus_nrm_clust <- vroom(file = "output/combined_tables_aus-nrm_clust_both_0.05.csv",
                    col_types = "cciiidiiddddddiicdiccc") %>%
                      # retain only trends for years:
                      filter(covariate == "year") %>% 
                      # convert trends to percent per decade:
                      mutate(mean_pc_mean = mean_pc_mean * 10,
                             mean_pc_median = mean_pc_median * 10,
                             median_pc_mean = median_pc_mean * 10,
                             median_pc_median = median_pc_median * 10)


period_list <- c("1910-2015", "1920-2015", "1930-2015", "1940-2015", "1950-2015", "1951-1980",
                 "1960-2015", "1961-1990", "1970-1992", "1970-2015", "1971-2000", "1976-2005",
                 "1980-2015", "1981-2010", "1981-2015", "1986-2015", "1990-2015", "1993-2015",
                 "1966-1989", "1990-2013", "1973-2009", "1961-1989")



##### FIG 4:

# p_list_fig4_mod <- period_list[c(22, 17, 7, 9, 18, 10)]
p_list <- period_list[c(8, 17, 7, 9, 18, 10)]

# For Fig 4 and 5a:
dom <- "AUS"
cov <- "year"
seas <- "Annual"
season_type <- "Annual"

symmetric_limits <- function (x) {
  max <- max(abs(x))
  c(-max, max)
}

# specific list of periods for annual analysis
filtered_df <- all_tables_aus_nrm_clust %>%
  filter(period %in% p_list) %>%
  mutate(period = as_factor(period)) %>%
  group_by(period) %>%
  filter(covariate == cov,
         domain == str_to_upper(dom),
         season == seas) %>%
  mutate(accumulation = as_factor(accumulation))


# plot of proportion of sig pos/neg trended stations, faceted by period
coeff <- 7.5
fig4 <- trend_bars(filtered_df) +
  facet_wrap(~factor(period, levels=p_list)) + 
  theme_bw()


# Save Fig 4:
ggsave(filename = 'results/figure4.pdf', plot = fig4, width = 5.5, height = 3.5, units = 'in', scale = 2)



##### FIG 5a:

p_list <- period_list[21] # "1973-2009"

filtered_df <- all_tables_aus_nrm_clust %>%
  filter(period %in% p_list) %>%
  mutate(period = as_factor(period)) %>%
  group_by(period) %>%
  filter(covariate == cov,
         domain == str_to_upper(dom),
         season == seas) %>%
  mutate(accumulation = as_factor(accumulation))

coeff <- 7.5
p5a <- trend_bars(filtered_df) +
  facet_wrap(~factor(season)) +
  theme_bw()


##### FIG 5b:

seas <- "Seasonal"
season_type <- "Seasonal"

# for seasonal, looking at one period at a time:
filtered_df <- all_tables_aus_nrm_clust %>%
  filter(period %in% p_list, season_type == "Seasonal") %>%
  mutate(season = as_factor(season)) %>%
  group_by(season) %>%
  filter(covariate == cov,
         domain == dom,
         season_type == seas) %>%
  mutate(accumulation = as_factor(accumulation))


coeff <- 7.5
p5b <- trend_bars(filtered_df) +
  facet_wrap(~factor(season)) +
  theme_bw()

fig5 <- p5a + p5b + plot_annotation(tag_levels = 'a')
ggsave('results/figure5.pdf', plot = fig5, width = 5.5, height = 2.5, units = 'in', scale = 2)




#####   FIG 6

# rx1hr by accumulation

lim_value <- 80   # resubmitted fig 6
fig6 <- trend_matrix_plot_by_domain_binned(all_tables_aus_nrm_clust, 
  covariate = "year", scaling = 10, xtype = 'mid', ytype = 'num_years', title = F, lim_value = lim_value, facet_by = "accum", 
  fill_var = 'mean', dom = "AUS", seas = "Annual", domains = "aus_nrm_clust", rotate_x_labels = 0)

ggsave('results/figure6.pdf', plot = fig6, width = 5.5, height = 3)


#####   FIG 7

# a) rx1hr by season
# b) rx24hr by season

# 7a
p7a <- trend_matrix_plot_by_domain_binned(all_tables_aus_nrm_clust, 
  covariate = "year", scaling = 10, xtype = 'mid', ytype = 'num_years', title = F, lim_value = lim_value, facet_by = "season", 
  fill_var = 'mean', dom = "AUS", accum = 1, rotate_x_labels = 30) + theme(legend.position = "none")
# 7b
p7b <- trend_matrix_plot_by_domain_binned(all_tables_aus_nrm_clust, 
  covariate = "year", scaling = 10, xtype = 'mid', ytype = 'num_years', title = F, lim_value = lim_value, facet_by = "season", 
  fill_var = 'mean', dom = "AUS", accum = 24, rotate_x_labels = 30)

# combine into fig 7:
fig7 <- p7a + p7b + plot_annotation(tag_levels = 'a')
ggsave('results/figure7.pdf', plot = fig7, width = 5.5, height = 3)


#####   FIG 8

# a) rx1hr by NRM region
# b) rx24hr by NRM region

p8a <- trend_matrix_plot_by_domain_binned(all_tables_aus_nrm_clust, 
  covariate = "year", scaling = 10, xtype = 'mid', ytype = 'num_years', title = F, lim_value = lim_value, facet_by = "domain", 
  fill_var = 'mean', seas = "Annual", accum = 1, rotate_x_labels = 45) + theme(legend.position = "none")
p8b <- trend_matrix_plot_by_domain_binned(all_tables_aus_nrm_clust, 
  covariate = "year", scaling = 10, xtype = 'mid', ytype = 'num_years', title = F, lim_value = lim_value, facet_by = "domain", 
  fill_var = 'mean', seas = "Annual", accum = 24, rotate_x_labels = 45)

# combine into fig 8:
fig8 <- p8a + p8b + plot_annotation(tag_levels = 'a')
ggsave('results/figure8.pdf', plot = fig8, width = 5.5, height = 3)


#####   FIG 9

# a) DJF rx1hr by NRM region
# b) MAM rx1hr by NRM region

p9a <- trend_matrix_plot_by_domain_binned(all_tables_aus_nrm_clust, 
  covariate = "year", scaling = 25, xtype = 'mid', ytype = 'num_years', title = F, lim_value = 150, facet_by = "domain", 
  fill_var = 'mean', seas = "DJF", accum = 1, domains = "aus_nrm_clust", rotate_x_labels = 45) + theme(legend.position = "none")
p9b <- trend_matrix_plot_by_domain_binned(all_tables_aus_nrm_clust, 
  covariate = "year", scaling = 25, xtype = 'mid', ytype = 'num_years', title = F, lim_value = 150, facet_by = "domain", 
  fill_var = 'mean', seas = "MAM", accum = 1, domains = "aus_nrm_clust", rotate_x_labels = 45)

# combine into fig 9:
fig9 <- p9a + p9b + plot_annotation(tag_levels = 'a')
ggsave('results/figure9.pdf', plot = fig9, width = 5.5, height = 3.3)
