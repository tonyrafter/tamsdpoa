# R script to collate information from tables in a set of sub-directories
library(tidyverse)
library(vroom)

library(foreach)
library(doParallel)

n_cores <- detectCores()
n_cores

# Register cluster
cluster <- makeCluster(n_cores - 2)
registerDoParallel(cluster)

record_length <- 0
min_period <- 10
sig_level <- "0.05"
min_pc <- "67"

base_dir <- paste0("./output/station_trends/minlength_",record_length,"yr/")

dur_list <- c(1, 3, 6, 12, 24)
dur_list_char <- as.character(dur_list)

domain_list <- c("aus", "nrm_clust")
# domain_list <- c("aus", "nrm_clust", "nrm_clust_super", "nrm_clust_sub", "ncra", "state")

dom_label <- paste(domain_list, collapse = '-')
period_type_list <- c("Annual", "Seasonal")

covariate_list <- c("year", "temperature")

first_year <- 1960
last_year <- 2015

max_len <- last_year - first_year + 1
# e.g. max_len = 2015 - 1960 = 55

# construct list of periods over which to iterate
period_list <- c()
for (period_len in seq(min_period, max_len)) {
  last_start <- last_year - period_len + 1
  temp_period_list <- paste(seq(first_year,last_start),seq(first_year+period_len-1,last_year),sep="-")
  period_list <- c(period_list, temp_period_list)
}

n_periods <- length(period_list)

t1 <- now()

new <- TRUE
missing_list <- c()
n_epoch <- 0

foreach(epoch=period_list) %dopar% {
# for (epoch in period_list) {

  epoch_len <- as.integer(str_split_i(string = epoch, pattern = "-", i = 2)) -
               as.integer(str_split_i(string = epoch, pattern = "-", i = 1)) + 1

  n_epoch <- n_epoch + 1
  print(paste0("[ epoch ", n_epoch, " / ", n_periods," ] ", epoch))

  for (cov in covariate_list) {

    if (cov == "temperature") {
      type_suffix <- "-temperature"
    } else {
      type_suffix <- ""
    }

    for (dur in dur_list_char) {

      dur_dir <- paste0(base_dir,"3/",epoch,"/",dur,"hr/tables/")

      for (period_type in period_type_list) {

        for (dom in domain_list) {

          filename <- paste0("station_trends-mk",sig_level,type_suffix,"_",dur,"hr_",
                             epoch,"_",toupper(dom),"_",period_type,"_min",min_pc,"pc.csv")
          # print(filename)

          if (length(list.files(path = dur_dir, pattern = filename)) != 0) {

            if (dom %in% c("aus", "vic", "syd", "seq")) {
              dom_type <- "rectangular"
              if (period_type == "Annual") {
                infile_coltypes <- "cciiidiiddddddc"
              } else {
                infile_coltypes <- "ccciiidiidddddd"
              }
            } else {
              dom_type <- "landmask"
              if (period_type == "Annual") {
                infile_coltypes <- "cciiiidiiddddddc" 
              } else {
                infile_coltypes <- "cccciiidiidddddd" 
              }
            }

            in_table <- vroom(file = paste0(dur_dir,filename),
              col_types = infile_coltypes) %>%
              rename(period = time_period) %>% 
              {if (period_type == "Annual")
                rename(., season = Annual) else . } %>%
              {if (period_type == "Seasonal")
                rename(., season = Season) else . } %>%
              # for landmask domains remove 'long name' version of domain
              {if (dom_type == "landmask") 
                # rename 1st col to 'domain' and remove 2nd col
                rename(., domain = 1) %>% select (-2) else . } %>% 
              mutate(period = epoch,
                n_years = as.integer(epoch_len),
                accumulation = as.integer(dur),
                covariate = cov,
                sig_level = sig_level,
                min_percent_valid = min_pc,
                domain_type = toupper(dom),
                season_type = period_type,
                .before = season
              )

            if (new == FALSE) {
              df <- bind_rows(df, in_table)
            } else {
              df <- in_table
              new <- FALSE
            }
          } else {
            print(paste("File missing:",filename))
            missing_list <- append(missing_list, filename)
          }

        }
      }
    }
  }
}

t2 <- now()
# print time taken to run through the above loops
elapsed_time <- t2 - t1
print(elapsed_time)
stopCluster(cl = cluster)

missing_list_file <- paste0("output/combined_tables_", dom_label, "_both_", sig_level, "_missing.csv")

n_missing <- length(missing_list)
print(paste("Writing list of", n_missing, "missing files to", missing_list_file))
write_lines(missing_list, file = missing_list_file)


outfile <- paste0("output/combined_tables_", dom_label, "_both_", sig_level, ".csv")

print(paste("Writing tables to",outfile))

vroom_write(df, file = outfile, delim = ",", )

