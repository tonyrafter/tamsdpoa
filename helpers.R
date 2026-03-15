# domain bounds for all Australia:
aus_lon_limits <- c(112, 156)
aus_lat_limits <- c(-45, -10)

# VIC domain bounds:
vic_lon_limits <- c(140,151.25)
vic_lat_limits <- c(-39.5,-32.8)

# SYD domain bounds:
syd_lon_limits <- c(148,153)
syd_lat_limits <- c(-36,-31.8)

# seq domain bounds:
seq_lon_limits <- c(150,154)
seq_lat_limits <- c(-29,-25)

# set default domains:
domain_list <- c("aus")
# domain_list <- c("aus", "vic", "syd", "seq")

shapefile_list <- list(
  nrm_clust = "./data/shapefiles/NRM_clusters/NRM_clusters.shp"
  # ,
  # nrm_clust_sub = "./data/shapefiles/NRM_sub_clusters/NRM_sub_clusters.shp",
  # nrm_clust_super = "./data/shapefiles/NRM_super_clusters/NRM_super_clusters.shp",
  # state = "./data/shapefiles/states/STE_2021_AUST_GDA2020.shp"
)
