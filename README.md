# tamsdpoa

*"Trends in Annual Maximum Sub-daily to Daily Precipitation Over Australia"*

This repository contains code developed in the production of materials created for and contained in the paper titled *"Trends in Annual Maximum Sub-daily to Daily Precipitation Over Australia"*, published in the journal **JGR: Atmospheres**.

Use of the software is covered under a Creative Commons CC-BY 4.0 licence; see the LICENCE file included in this repository.

## User Guide

The following documents the processing and plotting steps involved in ingesting, processing and visualising data used in the paper.


### Software Requirements

The software was run using a combination of code written in _R_ and wrapper scripts written in _csh_.

#### _R_ package requirements

The version of _R_ as well as the packages used in the processing steps are listed in the `renv.lock` file. 

Installing the identical version of _R_ is suggested, though not always required; YMMV. Having the major version the same (i.e. 'v4.x') is strongly recommended.

The list of required packages can be installed using the `renv` package, which should install the list of packages provided in the `.renv` file into the project directory and the system cache.


### Data

The below data sets are required to replicate this work. Each need to be downloaded separately, and moved to be available in the `./data` sub-directory.

#### Extreme Precipitation Indices

The precipitation data used in this study were derived from in-situ stations around Australia, which were quality controlled and processed into annual- and seasonal-maxima indices. These data are preserved in a permanent data record, available from the DOI link in the citation for Rafter (2025)[^Rafter2025].

[^Rafter2025]: Rafter, T. (2025). Annual and seasonal maxima of sub-daily precipitation accumulations (RxNhr) at observation stations in Australia [Dataset]. CSIRO. https://doi.org/10.25919/s398-1p95

NB: If you are using this data set, you will need to download the data from the above reference (Rafter 2025) and move the two `Indices_{N}min` directories to sit within the `./data` directory of this repository.

#### NRM Region Shapefiles

The regions analysed in this work were formed through the accumulation of climatologically related Natural Resource Management (NRM) regions in Australia. They are described further and can be downloaded from the Climate Change in Australia web site:

<https://www.climatechangeinaustralia.gov.au/en/overview/methodology/nrm-regions/>[^nrm_regions]

[^nrm_regions]: NRM Regions. (n.d.). Climate Change In Australia. Retrieved 16 August 2024, from https://www.climatechangeinaustralia.gov.au/en/overview/methodology/nrm-regions/

These shapefiles need to be located in the `./data/shapefiles/NRM_clusters/` directory.

#### Other regional shapefiles [optional]

The code is also capable of analysing regions based on other regional boundaries:

- NRM sub-clusters, where some of the NRM regions are split into smaller consituent regions;
- NRM super-clusters, which join NRM regions together into four 'super regions' covering Australia; and
- Australian state boundaries.

These shapefiles need to be un-commented in the `helpers.R` file.

##### NRM Sub-Clusters and NRM Super-Clusters

These shapefiles can be downloaded from the same link as for the NRM Regions above.

These files need to be extracted and placed in the `./data/shapefiles/NRM_sub_clusters/` and `./data/shapefiles/NRM_super_clusters/` directories respectively.

##### State Boundaries

The code is also capable of analysing regions based on Australian state boundaries. These are available to download as shapefiles from the Australian Bureau of Statistics at the following link:

https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/STE_2021_AUST_SHP_GDA2020.zip

These shapefiles should be extracted and placed in the `./data/shapefiles/states/` directory.

#### Global Mean Surface Temperature Index

The source of global mean surface temperature (GMST) used in the supplementary information is the HadCRUT data set, which was sourced from the UK Met Office[^HadCRUT].

These files will need to be downloaded and moved to sit within a directory `./data/temperature/` in order to be found by the scripts.

[^HadCRUT]: Morice, C. P., Kennedy, J. J., Rayner, N. A., Winn, J. P., Hogan, E., Killick, R. E., Dunn, R. J. H., Osborn, T. J., Jones, P. D., & Simpson, I. R. (2021). An Updated Assessment of Near-Surface Temperature Change From 1850: The HadCRUT5 Data Set. Journal of Geophysical Research: Atmospheres, 126(3), e2019JD032361. https://doi.org/10.1029/2019JD032361


### Software used in creating data sets

The data were produced using software created as part of the Global Sub-Daily Rainfall (GSDR) project, which is 
described in Pritchard et al. (2023)[^Pritchard2023].

[^Pritchard2023]:Pritchard, D., Lewis, E., Blenkinsop, S., Patino Velasquez, L., Whitford, A., & Fowler, H. J. (2023). An Observation-Based Dataset of Global Sub-Daily Precipitation Indices (GSDR-I). Scientific Data, 10(1), Article 1. https://doi.org/10.1038/s41597-023-02238-4

The software used in our work and in the above source paper is provided via Pritchard et al. (2022)[^Pritchard2022].

[^Pritchard2022]: Pritchard, D., Lewis, E., Blenkinsop, S., Patino Velasquez, L., Whitford, A., & Fowler, H. (2022). GSDR-I Global Sub-Daily Precipitation Indices—Code [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.7492877


### Outputs

The software in this repository is used to create the following outputs:

#### Figures:
1. Count over time and geographical distribution of in-situ station observation locations.
2. Map of the Australian Natural Resource Management (NRM) region clusters.
3. Spatial distribution and magnitude of trends in Rx1hr, expressed as % of the mean station Rx1hr per decade, for the period 1973-2009, shown for annual and broken down by each season.
4. Summary of proportion of significant positive and negative trends and mean of all trends in annual maxima for each precipitation duration, shown for several time periods.
5. As for 4, but for one time period (1973-2009) for annual and each season.
6. Summary of nationally-averaged trend magnitudes in annual maxima for all combinations of start and end year, displayed for each accumulation.
7. As for 6, but broken into seasonal Rx1hr and Rx24hr maxima.
8. Summary of regionally-averaged trend magnitudes in annual maxima of Rx1hr and Rx24hr for all combinations of start and end year, displayed for each NRM cluster region.
9. As for 8, but for summer (DJF) and autumn (MAM) seasonal maxima Rx1hr.


### Processing pipeline

1. Place annual/seasonal maximum precipitation data in the `./data` directory as described above. 
1. Create Figures 1 and 2 by running `run_processing_scripts.csh --figures`.  
This produces a station map alongside histogram of station availability over time (Fig 1); and choropleth (filled maps) labelling the regions analysed (Fig 2), and a (bonus!) map showing average trends within those regions.
1. Calculate and create tables of trend magnitudes, p-values etc. using `run_processing_scripts.csh --tables`, which calls `processing_and_plotting.R` over all specified combinations, e.g. of start and end years (and thus period), accumulation time, block length (annual/seasonal). (These and other options may be set manually within the script.)
1. Collate the array of tables created in previous step into a single file using `gather_tables.R`.
1. Use the collated table file to produce the remaining figures using `figure_plotting_4-9.R`. 
1. Locate figures created in `./results` directory.