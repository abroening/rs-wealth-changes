#normalize by change time 
#percentage increase of home value not raw
#percent change ndvi as well
#then try filtering tracts that have changed a lot

#https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch10.pdf
## ^^ DOLLAR AMOUNT CONVERSION GUIDANCE ^^
# https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm
## INFLATION DATA


install.packages(c("tidyverse", "tidycensus", "dplyr", "sf", "utils",  "stats", "cowplot", "Hmisc", "ggcorrplot", "Cairo", "corrplot", "extrafont"))

library(tidycensus)
library(tidyverse)
library(dplyr)
library(sf)
library(utils)
library(stats)
library(cowplot)
library(Hmisc)
library(ggcorrplot)
library(Cairo)
library(corrplot)
library(extrafont)

font_import(prompt = FALSE)
loadfonts(device = 'win')
options(scipen = 999)
setwd("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/full_mergeddata")
census_api_key("753915d84691b3657721d57a715b97feafbbf81a")

################################################################################
### READ SATTELITE DATA
################################################################################

#NDVI
ndvi_1990 <- read.csv("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/aggedsat_bygeo/ndvi_1990.csv") %>% 
  select(-system.index, -.geo) %>% 
  rename_with(., ~ paste0("ndvi_1990_", .x, recycle0 = TRUE), !starts_with("GEOID"))
ndvi_2000 <- read.csv("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/aggedsat_bygeo/ndvi_2000.csv") %>%
  select(-system.index, -.geo) %>%
  rename_with(., ~ paste0("ndvi_2000_", .x, recycle0 = TRUE), !starts_with("GEOID"))
ndvi_2007 <- read.csv("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/aggedsat_bygeo/ndvi_2007.csv") %>%
  select(-system.index, -.geo) %>%
  rename_with(., ~ paste0("ndvi_2007_", .x, recycle0 = TRUE), !starts_with("GEOID"))
ndvi_2010 <- read.csv("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/aggedsat_bygeo/ndvi_2010.csv") %>%
  select(-system.index, -.geo) %>%
  rename_with(., ~ paste0("ndvi_2010_", .x, recycle0 = TRUE), !starts_with("GEOID"))
ndvi_2012 <- read.csv("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/aggedsat_bygeo/ndvi_2012.csv") %>%
  select(-system.index, -.geo) %>%
  rename_with(., ~ paste0("ndvi_2012_", .x, recycle0 = TRUE), !starts_with("GEOID"))
ndvi_2017 <- read.csv("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/aggedsat_bygeo/ndvi_2017.csv") %>%
  select(-system.index, -.geo) %>%
  rename_with(., ~ paste0("ndvi_2017_", .x, recycle0 = TRUE), !starts_with("GEOID"))
ndvi_2022 <- read.csv("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/aggedsat_bygeo/ndvi_2022.csv") %>%
  select(-system.index, -.geo) %>%
  rename_with(., ~ paste0("ndvi_2022_", .x, recycle0 = TRUE), !starts_with("GEOID"))

#NIGHTIME LIGHTS
ntl_2012 <- read.csv("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/aggedsat_bygeo/ntl_2012.csv") %>%
  select(-system.index, -.geo) %>%
  rename_with(., ~ paste0("ntl_2012_", .x, recycle0 = TRUE), !starts_with("GEOID"))
ntl_2017 <- read.csv("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/aggedsat_bygeo/ntl_2017.csv") %>%
  select(-system.index, -.geo) %>%
  rename_with(., ~ paste0("ntl_2017_", .x, recycle0 = TRUE), !starts_with("GEOID"))
ntl_2022 <- read.csv("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/aggedsat_bygeo/ntl_2022.csv") %>%
  select(-system.index, -.geo) %>%
  rename_with(., ~ paste0("ntl_2022_", .x, recycle0 = TRUE), !starts_with("GEOID"))

## MERGE THEM 

ndvi <- ndvi_1990 %>%
  left_join(ndvi_2000, by = "GEOID") %>%
  left_join(ndvi_2007, by = "GEOID") %>% 
  left_join(ndvi_2010, by = "GEOID") %>% 
  left_join(ndvi_2012, by = "GEOID") %>%
  left_join(ndvi_2017, by = "GEOID") %>%
  left_join(ndvi_2022, by = "GEOID") %>% 
  mutate(
    GEOID = as.character(GEOID),
    GEOID = ifelse(nchar(GEOID) < 11, paste0("0", GEOID), GEOID)
  )

ntl <- ntl_2012 %>%
  left_join(ntl_2017, by = "GEOID") %>%
  left_join(ntl_2022, by = "GEOID") %>% 
  mutate(
    GEOID = as.character(GEOID),
    GEOID = ifelse(nchar(GEOID) < 10, paste0("0", GEOID), GEOID)
  )

################################################################################
### ADD SHAPEFILES
################################################################################
# READ
countysub_geo <- st_read("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/Base Shapefiles/geo_countysub_2022.shp")
tract_geo <- st_read("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/Base Shapefiles/geo_tract_2022.shp")

# JOIN
countysub_ntl <- countysub_geo %>% 
  left_join(ntl, by = "GEOID")
tract_ndvi <- tract_geo %>% 
  left_join(ndvi, by = "GEOID")

# GET CBSAs
cbsa_geo <- get_acs(
  year = 2022,
  survey = "acs5",
  variables = c("dummy" = "B01003_001"),
  geography = "cbsa",
  geometry = T
)

# Get urban areas
ua_geo <- get_acs(
  year = 2022,
  survey = "acs5",
  variables = c("dummy" = "B01003_001"),
  geography = "urban area",
  geometry = T
)

################################################################################
### JOIN DEMO DATA
################################################################################
# ALL DATA ADJUSTED TO 2022 DOLLARS USING THE CENSUS BUREAU'S SUGGESTED METHOD.
# USING CPI-U-RS
# 1990-2022 Factor = 431.5 / 197.6 = 2.1837
# 2000-2022 Factor = 431.5 / 252.5 = 1.7089
# 2007-2022 Factor = 431.5 / 304.1 = 1.4189
# 2010-2022 Factor = 431.5 / 319.8 = 1.3493
# 2012-2022 Factor = 431.5 / 336.9 = 1.2808
# 2017-2022 Factor = 431.5 / 360.3 = 1.1976

# Read Demo Data
countysub_demo <- read.csv("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/rsproject_countysubdemo_12.09.2024.csv") %>% 
  mutate(GEOID = as.character(GEOID),
         GEOID = ifelse(nchar(GEOID) < 10, paste0("0", GEOID), GEOID))
tract_demo <- read.csv("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/rsproject_tractdemo_12.09.2024.csv") %>% 
  mutate(GEOID = as.character(GEOID),
         GEOID = ifelse(nchar(GEOID) < 11, paste0("0", GEOID), GEOID))


# Join to Sat/GEo
countysub_join <- countysub_ntl %>% 
  left_join(countysub_demo, by = "GEOID") %>% 
  select(-X) %>% 
  #ADJUST FOR INFLATION
  mutate(
   mgr_2017 = mgr_2017 * 1.1976,
   ahv_2017 = ahv_2017 * 1.1976,
   mhi_2017 = mhi_2017 * 1.1976,
   mgr_2012 = mgr_2012 * 1.2808,
   ahv_2012 = ahv_2012 * 1.2808,
   mhi_2012 = mhi_2012 * 1.2808
  )
tract_join <- tract_ndvi %>% 
  left_join(tract_demo, by = "GEOID") %>% 
  select(-X) %>% 
  #FIX NAMING OF COLS
  rename(ahv_2022 = ahv2022_) %>% 
  rename(mgr_2022 = mgr2022_) %>% 
  rename(mhi_2022 = mhi2022_) %>% 
  #ADJUST FOR INFLATION
  mutate(
    mgr_2017 = mgr_2017 * 1.1976,
    ahv_2017 = ahv_2017 * 1.1976,
    mhi_2017 = mhi_2017 * 1.1976,
    mgr_2012 = mgr_2012 * 1.2808,
    ahv_2012 = ahv_2012 * 1.2808,
    mhi_2012 = mhi_2012 * 1.2808,
    mgr_2010 = mgr_2010 * 1.3493,
    ahv_2010 = ahv_2010 * 1.3493,
    mhi_2010 = mhi_2010 * 1.3493,
    mgr_2000 = mgr_2000 * 1.7089,
    ahv_2000 = ahv_2000 * 1.7089,
    mhi_2000 = mhi_2000 * 1.7089,
    mgr_1990 = mgr_1990 * 2.1837,
    ahv_1990 = ahv_1990 * 2.1837,
    mhi_1990 = mhi_1990 * 2.1837
  )


################################################################################
### FILTER NTL DATA
################################################################################

### INCOMPLETE

filtered_ntl <- countysub_join %>% 
  select(starts_with("ntl_2012_median") |
           starts_with("ntl_2017_median") |
           starts_with("ntl_2022_median") |
           starts_with("ahv") | 
           starts_with("mgr") |
           starts_with("mhi"), 
         GEOID, 
         geometry) %>% 
  select(!starts_with("ntl_2012_median_mask")) %>% 
  select(!starts_with("ntl_2017_median_mask")) %>% 
  select(!starts_with("ntl_2022_median_mask"))


################################################################################
### ANALYSIS
################################################################################
setwd("//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Presentation")


#### CLEAN AND REARRANGE

filtered_ndvi <- tract_join %>% 
  #SELECT ONLY MEDIAN SAT VALS
  select(
    GEOID,
    starts_with("ahv"),
    starts_with("mgr"),
    starts_with("mhi"),
    ends_with("median"),
    # REMOVE NDVI 2007 - NO DEMO DATA
    -starts_with("ndvi_2007")
  ) %>%
  ### FIXING NAMES
  pivot_longer(
    cols = !c(GEOID, geometry),
    names_to = "variable_year",
    values_to = "value"
  ) %>% 
  mutate(variable_year = case_when(
    endsWith(variable_year, "_median") ~ substr(variable_year, 1, 9),
    .default = variable_year
  )
  ) %>% 
  # MOVE YEAR TO KEY VALUE
  separate(variable_year, into = c("variable", "year"), sep = "_") %>%  
  pivot_wider(
    values_from = value, 
    names_from = variable 
  ) %>% 
  # year to numeric
  mutate(
    year = as.numeric(year)
  )

##### TOTAL SLOPES
## BUILD
slopes_ndvi <- filtered_ndvi %>% 
  group_by(GEOID) %>% 
  mutate(
    ahv_slope = ifelse(sum(!is.na(ahv)) == 0, NA, coef(lm(ahv ~ year, na.action = na.exclude))["year"]),
    mgr_slope = ifelse(sum(!is.na(mgr)) == 0, NA, coef(lm(mgr ~ year, na.action = na.exclude))["year"]),
    mhi_slope = ifelse(sum(!is.na(mhi)) == 0, NA, coef(lm(mhi ~ year, na.action = na.exclude))["year"]),
    ndvi_slope = ifelse(sum(!is.na(ndvi)) == 0, NA, coef(lm(ndvi ~ year, na.action = na.exclude))["year"])
  ) %>% 
  ungroup()
##ANALYZE

## SCATTER OVERALL
# AHV
slopes_ndvi %>% 
  #filter so no repeated points
  filter(year == 2022) %>% 
  ggplot() +
  geom_point(aes(x = ahv_slope, y = ndvi_slope))
# MGR
slopes_ndvi %>% 
  #filter so no repeated points
  filter(year == 2022) %>% 
  ggplot() +
  geom_point(aes(x = mgr_slope, y = ndvi_slope))
slopes_ndvi %>% 
  #filter so no repeated points
  filter(year == 2022) %>% 
  ggplot() +
  geom_point(aes(x = mhi_slope, y =))

##COR
slopes_ndvi %>% 
  as.data.frame() %>% 
  select(!geometry) %>% 
  select(!GEOID) %>% 
  #optional - ??
  filter(year == 2022) %>% 
  cor(., use = "pairwise.complete.obs")

##MAP
# Filter Areas
rva_ndvi <- slopes_ndvi %>% 
  filter(st_intersects(geometry, cbsa_geo %>% filter(GEOID == 40060), sparse = FALSE)[,1])
dc_ndvi<- slopes_ndvi %>% 
  filter(st_intersects(geometry, cbsa_geo %>% filter(GEOID == 47900), sparse = FALSE)[,1])
sf_ndvi<- slopes_ndvi %>% 
  filter(st_intersects(geometry, cbsa_geo %>% filter(GEOID == 41860), sparse = FALSE)[,1])
austin_ndvi<- slopes_ndvi %>% 
  filter(st_intersects(geometry, cbsa_geo %>% filter(GEOID == 12420), sparse = FALSE)[,1])
seattle_ndvi<- slopes_ndvi %>% 
  filter(st_intersects(geometry, cbsa_geo %>% filter(GEOID == 42660), sparse = FALSE)[,1])
chicago_ndvi<- slopes_ndvi %>% 
  filter(st_intersects(geometry, cbsa_geo %>% filter(GEOID == 16980), sparse = FALSE)[,1])

# Map Them  
# Richmond
rva1 <- rva_ndvi %>% 
  ggplot() +
  geom_sf(aes(fill = mgr_slope)) +
  scale_fill_gradient2()
rva2 <- rva_ndvi %>% 
  ggplot() +
  geom_sf(aes(fill = ndvi_slope)) +
  scale_fill_gradient2()
plot_grid(rva1, rva2, ncol = 2, nrow = 1)

# DC
dc1 <- dc_ndvi %>% 
  ggplot() +
  geom_sf(aes(fill = mgr_slope)) +
  scale_fill_gradient2()
dc2 <- dc_ndvi %>% 
  ggplot() +
  geom_sf(aes(fill = ndvi_slope)) +
  scale_fill_gradient2()
plot_grid(dc1, dc2, ncol = 2, nrow = 1)

# San Francisco
sf1 <- sf_ndvi %>% 
  ggplot() +
  geom_sf(aes(fill = mgr_slope)) +
  scale_fill_gradient2()
sf2 <- sf_ndvi %>% 
  ggplot() +
  geom_sf(aes(fill = ndvi_slope)) +
  scale_fill_gradient2()
plot_grid(sf1, sf2, ncol = 2, nrow = 1)

# Austin
austin1 <- austin_ndvi %>% 
  ggplot() +
  geom_sf(aes(fill = mgr_slope)) +
  scale_fill_gradient2()
austin2 <- austin_ndvi %>% 
  ggplot() +
  geom_sf(aes(fill = ndvi_slope)) +
  scale_fill_gradient2()
plot_grid(austin1, austin2, ncol = 2, nrow = 1)

# Seattle
seattle1 <- seattle_ndvi %>% 
  ggplot() +
  geom_sf(aes(fill = mgr_slope)) +
  scale_fill_gradient2()
seattle2<- seattle_ndvi %>% 
  ggplot() +
  geom_sf(aes(fill = ndvi_slope)) +
  scale_fill_gradient2()
plot_grid(seattle1, seattle2, ncol = 2, nrow = 1)

# Chicago
chicago1 <- chicago_ndvi %>% 
  ggplot() +
  geom_sf(aes(fill = mgr_slope)) +
  scale_fill_gradient2()
chicago2 <- chicago_ndvi %>% 
  ggplot() +
  geom_sf(aes(fill = ndvi_slope)) +
  scale_fill_gradient2()
plot_grid(chicago1, chicago2, ncol = 2, nrow = 1)



##### TIME SERIES - YEAR BY YEAR CHANGES
# NEED WIDE DATA - REMOVE YEAR FROM KEY
filtered_ndvi_wide <- filtered_ndvi %>% 
  pivot_wider(names_from = year, values_from = c(ahv, mgr, mhi, ndvi))

## MUTATE YEAR CHANGE COLS, SMALLEST INTERVAL, NORMALIZED FOR TIME GAP
ndvi_timeseries_small <- filtered_ndvi_wide %>% 
  mutate(
    #names are differences between last 2 dig
    mgr_9000 = (mgr_2000 - mgr_1990) / 10,
    mgr_0010 = (mgr_2010 - mgr_2000) / 10,
    mgr_1012 = (mgr_2012 - mgr_2010) / 2,
    mgr_1217 = (mgr_2017 - mgr_2012) / 5,
    mgr_1722 = (mgr_2022 - mgr_2017) / 5,
    ahv_9000 = (ahv_2000 - ahv_1990) / 10,
    ahv_0010 = (ahv_2010 - ahv_2000) / 10,
    ahv_1012 = (ahv_2012 - ahv_2010) / 2,
    ahv_1217 = (ahv_2017 - ahv_2012) / 5,
    ahv_1722 = (ahv_2022 - ahv_2017) / 5,
    mhi_9000 = (mhi_2000 - mhi_1990) / 10,
    mhi_0010 = (mhi_2010 - mhi_2000) / 10,
    mhi_1012 = (mhi_2012 - mhi_2010) / 2,
    mhi_1217 = (mhi_2017 - mhi_2012) / 5,
    mhi_1722 = (mhi_2022 - mhi_2017) / 5,
    ndvi_9000 = (ndvi_2000 - ndvi_1990) / 10,
    ndvi_0010 = (ndvi_2010 - ndvi_2000) / 10,
    ndvi_1012 = (ndvi_2012 - ndvi_2010) / 2,
    ndvi_1217 = (ndvi_2017 - ndvi_2012) / 5,
    ndvi_1722 = (ndvi_2022 - ndvi_2017) / 5,
    # percent change NORMALIZED FOR TIME
    mgr_9000_pt = ((mgr_2000 - mgr_1990) / mgr_1990) /  10,
    mgr_0010_pt = ((mgr_2010 - mgr_2000) / mgr_2000) /  10,
    mgr_1012_pt = ((mgr_2012 - mgr_2010) / mgr_2010) / 2,
    mgr_1217_pt = ((mgr_2017 - mgr_2012) / mgr_2012) / 5,
    mgr_1722_pt = ((mgr_2022 - mgr_2017) / mgr_2017) / 5,
    ahv_9000_pt = ((ahv_2000 - ahv_1990) / ahv_1990) / 10,
    ahv_0010_pt = ((ahv_2010 - ahv_2000) / ahv_2000) / 10,
    ahv_1012_pt = ((ahv_2012 - ahv_2010) / ahv_2010) / 2,
    ahv_1217_pt = ((ahv_2017 - ahv_2012) / ahv_2012) / 5,
    ahv_1722_pt = ((ahv_2022 - ahv_2017) / ahv_2017) / 5,
    mhi_9000_pt = ((mhi_2000 - mhi_1990) / mhi_1990) / 10,
    mhi_0010_pt = ((mhi_2010 - mhi_2000) / mhi_2000) / 10,
    mhi_1012_pt = ((mhi_2012 - mhi_2010) / mhi_2010) / 2,
    mhi_1217_pt = ((mhi_2017 - mhi_2012) / mhi_2012) / 5,
    mhi_1722_pt = ((mhi_2022 - mhi_2017) / mhi_2017) / 5,
    ndvi_9000_pt = ((ndvi_2000 - ndvi_1990) / ndvi_1990) / 10,
    ndvi_0010_pt = ((ndvi_2010 - ndvi_2000) / ndvi_2000) / 10,
    ndvi_1012_pt = ((ndvi_2012 - ndvi_2010) / ndvi_2010) / 2,
    ndvi_1217_pt = ((ndvi_2017 - ndvi_2012) / ndvi_2012) / 5,
    ndvi_1722_pt = ((ndvi_2022 - ndvi_2017) / ndvi_2017) / 5) %>% 
  mutate(across(mgr_9000_pt:ndvi_1722_pt, ~ ifelse(!is.finite(.), NA, .)))

## COR
ndvi_timeseries_small %>% 
  as.data.frame() %>% 
  select(!geometry) %>% 
  select(!GEOID) %>% 
  select(mgr_9000:ndvi_1722) %>% 
  as.matrix() %>% 
  rcorr()
#with ggcorrplot

#rename cols for graphic
ggrename_pt <- c(
  "Rent Change '90- '00" = "mgr_9000_pt",
  "Rent Change '00 - '10" = "mgr_0010_pt",
  "Rent Change '10 - '12" = "mgr_1012_pt",
  "Rent Change '12 - '17" = "mgr_1217_pt",
  "Rent Change '17 - '22" = "mgr_1722_pt",
  "House Value Change '90- '00" = "ahv_9000_pt",
  "House Value Change '00 - '10" = "ahv_0010_pt",
  "House Value Change '10 - '12" = "ahv_1012_pt",
  "House Value Change '12 - '17" = "ahv_1217_pt",
  "House Value Change '17 - '22" = "ahv_1722_pt",
  "NDVI Change '90- '00" = "ndvi_9000_pt",
  "NDVI Change '00 - '10" = "ndvi_0010_pt",
  "NDVI Change '10 - '12" = "ndvi_1012_pt",
  "NDVI Change '12 - '17" = "ndvi_1217_pt",
  "NDVI Change '17 - '22" = "ndvi_1722_pt",
  "MHI Change '90 - '00" = "mhi_9000_pt",
  "MHI Change '00 - '10" = "mhi_0010_pt",
  "MHI Change '10 - '12" = "mhi_1012_pt",
  "MHI Change '12 - '17" = "mhi_1217_pt",
  "MHI Change '17 - '22" = "mhi_1722_pt"
) 

ggrename_nopt <- c(
  "Rent Change '90- '00" = "mgr_9000",
  "Rent Change '00 - '10" = "mgr_0010",
  "Rent Change '10 - '12" = "mgr_1012",
  "Rent Change '12 - '17" = "mgr_1217",
  "Rent Change '17 - '22" = "mgr_1722",
  "House Value Change '90- '00" = "ahv_9000",
  "House Value Change '00 - '10" = "ahv_0010",
  "House Value Change '10 - '12" = "ahv_1012",
  "House Value Change '12 - '17" = "ahv_1217",
  "House Value Change '17 - '22" = "ahv_1722",
  "NDVI Change '90- '00" = "ndvi_9000",
  "NDVI Change '00 - '10" = "ndvi_0010",
  "NDVI Change '10 - '12" = "ndvi_1012",
  "NDVI Change '12 - '17" = "ndvi_1217",
  "NDVI Change '17 - '22" = "ndvi_1722",
  "MHI Change '90 - '00" = "mhi_9000",
  "MHI Change '00 - '10" = "mhi_0010",
  "MHI Change '10 - '12" = "mhi_1012",
  "MHI Change '12 - '17" = "mhi_1217",
  "MHI Change '17 - '22" = "mhi_1722"
) 
### FOR PERCENT CHANGE
ndvi_ggcor_pt <- ndvi_timeseries_small %>% 
  as.data.frame() %>% 
  select(!geometry) %>% 
  select(!GEOID) %>% 
  select(mgr_9000_pt:ndvi_1722_pt) %>% 
  rename(., all_of(ggrename_pt)) %>% 
  as.matrix() %>% 
  stats::cor(., use = "pairwise.complete.obs", method = "pearson")

ndvi_ggpmat_pt <- ndvi_timeseries_small %>% 
  as.data.frame() %>% 
  select(!geometry) %>% 
  select(!GEOID) %>% 
  select(mgr_9000_pt:ndvi_1722_pt) %>%  
  rename(., all_of(ggrename_pt)) %>% 
  as.matrix() %>% 
  ggcorrplot:::cor_pmat()

ggcorrplot(round(ndvi_ggcor_pt, digits = 2), 
           type = "upper", 
           ggtheme = ggplot2::theme_minimal(base_family = "Roboto Thin"),
           colors = c("#E46726", "white", "#6D9EC1"), 
           lab = TRUE, 
           p.mat = ndvi_ggpmat_pt, 
           insig = "blank", 
           legend.title = "Correlation\nCoefficient", 
           lab_size = 2.5,
           title = "Correlation Table - Yearly Rates of Percentage Change Compared",
           method = "circle",
           tl.cex = 8,
           tl.col = "black")
ggsave("rsproject_corrmatrix_pt_12.09.2024.png", width = 3000, height = 3000, units = "px", type = "cairo-png")

### FOR RAW CHANGE
ndvi_ggcor_pt <- ndvi_timeseries_small %>% 
  as.data.frame() %>% 
  select(!geometry) %>% 
  select(!GEOID) %>% 
  select(mgr_9000:ndvi_1722) %>% 
  rename(., all_of(ggrename_nopt)) %>% 
  as.matrix() %>% 
  stats::cor(., use = "pairwise.complete.obs", method = "pearson")

ndvi_ggpmat <- ndvi_timeseries_small %>% 
  as.data.frame() %>% 
  select(!geometry) %>% 
  select(!GEOID) %>%
  select(mgr_9000:ndvi_1722) %>%  
  rename(., all_of(ggrename_nopt)) %>% 
  as.matrix() %>% 
  ggcorrplot:::cor_pmat()

ggcorrplot(round(ndvi_ggcor, digits = 2), 
           type = "upper", 
           ggtheme = ggplot2::theme_minimal(base_family = "Roboto Thin"),
           colors = c("#E46726", "white", "#6D9EC1"), 
           lab = TRUE, 
           p.mat = ndvi_ggpmat, 
           insig = "blank", 
           legend.title = "Correlation\nCoefficient", 
           lab_size = 2.5,
           title = "Correlation Table - Yearly Rates of Percentage Change Compared",
           method = "circle",
           tl.cex = 8,
           tl.col = "black")

ggsave("rsproject_corrmatrix_nopt_12.09.2024.png", width = 3000, height = 3000, units = "px", type = "cairo-png")


## POINT CLOUD
# Highest COrr No Percent
ndvi_point_nopt <- ndvi_timeseries_small %>% 
  ggplot(aes(y = ndvi_1217, x = ahv_0010)) +
  geom_point(size = .3, alpha = .1)+
  coord_cartesian(xlim = c(-25000, 100000), ylim = c(-.05, .08))+
  geom_smooth(method = "lm", se = F)+
  labs(
    x = "Annual Change of Average House Value between 2000 and 2010",
    y = "Annual Change of NDVI between 2012 and 2017",
    title = "Scatter Plot of Highest Correlation Comparison, Linear Regression",
    subtitle = "Points represent individual tracts, n = 42,833"
  )+
  theme_minimal(base_family = "Roboto") +
  theme(
    plot.subtitle = element_text(size = 10)
  )
ggsave("rsproject_highscatternopt_12.04.2024.png", width = 1800, height = 1800, units = "px", type = "cairo-png")

# Highest Corr Percent
ndvi_point_pt <- ndvi_timeseries_small %>% 
  ggplot(aes(y = ndvi_9000_pt, x = ahv_0010_pt)) +
  geom_point(size = .3, alpha = 0.1)+
  coord_cartesian(xlim = c(-.25, 1), ylim = c(-.1, .33))+
  geom_smooth(method = "lm", se = F)+
  labs(
    x = "Percent Annual Change of Average House Value between 2000 and 2010",
    y = "Percent Annual Change of NDVI between 2012 and 2017",
    title = "Scatter Plot of Highest Correlation Comparison, Linear Regression",
    subtitle = "Points represent individual tracts, n = 42,793"
  )+
  theme_minimal(base_family = "Roboto") +
  theme(
    plot.subtitle = element_text(size = 10)
  )+
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent)
ggsave("rsproject_highscatterpt_12.04.2024.png", width = 1800, height = 1800, units = "px", type = "cairo-png")

################################################################################
### FOR NTL
ntl_analysis <- countysub_join %>% 
  #SELECT ONLY MEDIAN SAT VALS
  select(
    GEOID,
    ends_with("median_median"),
    starts_with("ahv"),
    starts_with("mgr"),
    starts_with("mhi")
  ) %>% 
  ### FIXING NAMES
  rename(ntl_2012 = ntl_2012_median_median, 
         ntl_2017 = ntl_2017_median_median, ntl_2022 = 
           ntl_2022_median_median)


ntl_timeseries_small <- ntl_analysis %>% 
  mutate(
    #names are differences between last 2 dig
    mgr_1217 = (mgr_2017 - mgr_2012) / 5,
    mgr_1722 = (mgr_2022 - mgr_2017) / 5,
    ahv_1217 = (ahv_2017 - ahv_2012) / 5,
    ahv_1722 = (ahv_2022 - ahv_2017) / 5,
    mhi_1217 = (mhi_2017 - mhi_2012) / 5,
    mhi_1722 = (mhi_2022 = mhi_2017) / 5,
    ntl_1217 = (ntl_2017 - ntl_2012) / 5,
    ntl_1722 = (ntl_2022 - ntl_2017) / 5,
    # percent change NORMALIZED FOR TIME
    mgr_1217_pt = ((mgr_2017 - mgr_2012) / mgr_2012) / 5,
    mgr_1722_pt = ((mgr_2022 - mgr_2017) / mgr_2017) / 5,
    ahv_1217_pt = ((ahv_2017 - ahv_2012) / ahv_2012) / 5,
    ahv_1722_pt = ((ahv_2022 - ahv_2017) / ahv_2017) / 5,
    mhi_1217_pt = ((mhi_2017 - mhi_2012) / mhi_2012) / 5,
    mhi_1722_pt = ((mhi_2022 - mhi_2017) / mhi_2017) / 5,
    ntl_1217_pt = ((ntl_2017 - ntl_2012) / ntl_2012) / 5,
    ntl_1722_pt = ((ntl_2022 - ntl_2017) / ntl_2017) / 5) %>% 
  mutate(across(mgr_1217_pt:ntl_1722_pt, ~ ifelse(!is.finite(.), NA, .)))

ntl_timeseries_small %>% 
  as.data.frame() %>% 
  select(!geometry) %>% 
  select(!GEOID) %>% 
  select(mgr_1217_pt:ntl_1722_pt) %>% 
  as.matrix() %>% 
  rcorr()

ggntlrename_pt <- c(
  "Rent Change '12 - '17" = "mgr_1217_pt",
  "Rent Change '17 - '22" = "mgr_1722_pt",
  "House Value Change '12 - '17" = "ahv_1217_pt",
  "House Value Change '17 - '22" = "ahv_1722_pt",
  "NTL Change '12 - '17" = "ntl_1217_pt",
  "NTL Change '17 - '22" = "ntl_1722_pt",
  "MHI Change '12 - '17" = "mhi_1217_pt",
  "MHI Change '17 - '22" = "mhi_1722_pt"
) 


ntl_ggcor <- ntl_timeseries_small %>% 
  as.data.frame() %>% 
  select(!geometry) %>% 
  select(!GEOID) %>% 
  select(mgr_1217_pt:ntl_1722_pt) %>% 
  rename(., all_of(ggntlrename_pt)) %>% 
  as.matrix() %>% 
  stats::cor(., use = "pairwise.complete.obs", method = "pearson")

ntl_ggpmat <- ntl_timeseries_small %>% 
  as.data.frame() %>% 
  select(!geometry) %>% 
  select(!GEOID) %>% 
  select(mgr_1217_pt:ntl_1722_pt) %>%  
  rename(., all_of(ggntlrename_pt)) %>% 
  as.matrix() %>% 
  ggcorrplot:::cor_pmat()

ggcorrplot(round(ntl_ggcor, digits = 2), 
           type = "upper", 
           ggtheme = ggplot2::theme_minimal(base_family = "Roboto Thin"),
           colors = c("#E46726", "white", "#6D9EC1"), 
           lab = TRUE, 
           p.mat = ntl_ggpmat, 
           insig = "blank", 
           legend.title = "Correlation\nCoefficient", 
           lab_size = 2.5,
           title = "Correlation Table - Yearly Rates of Percentage Change Compared",
           method = "circle",
           tl.cex = 8,
           tl.col = "black")

ggsave("rsproject_ntlcorrmatrix_pt_12.09.2024.png", width = 2000, height = 2000, units = "px", type = "cairo-png")

## Same thing, no results ]
test <- lm(ntl_1722_pt ~ ahv_1217_pt, data = ntl_timeseries_small, na.action = na.exclude)
## SCATTER OF HIGHEST
ntl_timeseries_small %>% 
  ggplot(aes(x = ahv_1217_pt, y = ntl_1722_pt))+
  geom_point(size = 1, alpha = 0.1)+
  labs(
    x = "Average Home Value Annual Percent Change 2012-2017",
    y = "Average Nighttime Light Annual Percent Change 2017-2022",
    subtitle = "Points represent county subdivisions, n = 32,166, after 3,332 subdivisions removed for missing values"
  )+
  theme(text = element_text(family = "Roboto Light"),
        axis.title.x = element_text(margin = margin(t = 5)),
        axis.title.y = element_text(margin = margin(r = 5)),
        plot.subtitle = element_text(face = "italic"))
#  coord_cartesian(xlim = c(-150000, 150000), ylim = c(-5, 7))
ggsave("rsproject_scatterahvntl_12.09.2024.png", width = 3000, height = 1800, units = "px", type = "cairo-png")

ndvi_timeseries_small %>% 
  ggplot(aes(x = ahv_9000_pt, y = ndvi_9000_pt))+
  geom_point(size = .25, alpha = 0.1)+
  labs(
    x = "Average Home Value Annual Percent Change 1990 - 2000",
    y = "Average NDVI Annual Percent Change 1990 - 2000",
    subtitle = "Points represent census tracts, n = 33,912, after 50,208 subdivisions removed for missing values"
  )+
  theme(text = element_text(family = "Roboto Light"),
        axis.title.x = element_text(margin = margin(t = 5)),
        axis.title.y = element_text(margin = margin(r = 5)),
        plot.subtitle = element_text(face = "italic"))+
  coord_cartesian(xlim = c(-.25, .5), ylim = c(-.5,.5))
test <- lm(ndvi_9000_pt ~ ahv_9000_pt, data = ndvi_timeseries_small, na.action = na.exclude)

ggsave("rsproject_scatterahvndvi_12.09.2024.png", width = 3000, height = 1800, units = "px", type = "cairo-png")


################################################################################
## EXPORT TO MAP

ndvi_timeseries_small %>% 
  select(GEOID, geometry, mgr_9000_pt:ndvi_1722_pt) %>% 
  mutate(across(mgr_9000_pt:ndvi_1722_pt, ~ ifelse(is.na(.), -9999, .))) %>% 
  st_write(., "rsproject_ndvimappingdata_12.04.2024.shp")

#msas to map
st_write(cbsa_geo, "rsproject_cbsamappingdata_12.04.2024.shp")

#cbsas to map
st_write(ua_geo, "rsproject_cbsamappingdata_12.04.2024.shp")

################################################################################
# FILTER BY JUST PLACES WITH MOST CHANGE
# FILTER BY JUST URBAN AREAS
# "dyear" is used to specify the Year Change Gap - Delta Year - eg. 2017-2022

## 1 - pivot long - tract:changegap:percentdnvichange:percentahvchange:percentmgrchange
## 2 - Filter for large change
## 3 - COR - no TEMPORAL LAG

test <- ndvi_timeseries_small %>% 
  pivot_longer(
    cols = starts_with("ahv") | starts_with("mgr") | starts_with("ndvi"),
    names_to = "variable_dyear",
    values_to = "value"
  ) %>% 
  filter(endsWith(variable_dyear, "_pt")) %>% 
  mutate(
    variable_dyear = gsub("_pt", "pt", variable_dyear)
  ) %>% 
  separate(variable_dyear, c("variable", "dyear"), sep = "_") %>% 
  pivot_wider(names_from = variable, values_from = value)

test %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  mutate(dyear = as.character(substr(dyear, 1, 4))) %>% 
  group_by(GEOID) %>% 
  mutate(
    dyear_lag1 = dplyr::lag(dyear),
    ahv_lag1 = dplyr::lag(ahv),
    mgr_lag1 = dplyr::lag(mgr),
    dyear_lag2 = dplyr::lag(dyear, 2),
    ahv_lag2 = dplyr::lag(ahv, 2),
    mgr_lag2 = dplyr::lag(mgr, 2) 
  ) %>% 
  ungroup() %>% 
  filter(ahv_lag1 >= 0) %>%
  select(-GEOID) %>% 
  mutate(
    dyear = as.numeric(dyear),
    dyear_lag1 = as.numeric(dyear_lag1),
    dyear_lag2 = as.numeric(dyear_lag2)
  ) %>% 
  as.matrix() %>% 
  stats::cor(., use = "pairwise.complete.obs", method = "pearson")





# filter(ahv >= 0.05) %>%   

  as.matrix() %>% 
  stats::cor(., use = "pairwise.complete.obs", method = "pearson")

## LAGGED:



## URBAN FILTER - URBAN WHEN - at what point? in 2022 as proxy?  


## FILTER BY BOTH?













################################################################################
###PIVOT LONGER

###
#### NDVI ONLY


slopes_ndvi <- filtered_ndvi %>% 
  group_by(GEOID) %>% 
  mutate(
    ahv_slope = ifelse(sum(!is.na(ahv)) == 0, NA, coef(lm(ahv ~ year, na.action = na.exclude))["year"]),
    mgr_slope = ifelse(sum(!is.na(mgr)) == 0, NA, coef(lm(mgr ~ year, na.action = na.exclude))["year"]),
    ndvi_slope = ifelse(sum(!is.na(ndvi)) == 0, NA, coef(lm(ndvi ~ year, na.action = na.exclude))["year"])
  ) %>% 
  ungroup()


slopes_ndvi %>% 
  filter(startsWith(GEOID, "51")) %>% 
  filter(mgr_slope <= 100) %>% 
  ggplot() +
  geom_sf(aes(fill = mgr_slope), color = NA)
  


slopes_ndvi %>% 
  filter(startsWith(GEOID, "51760")) %>% 
  ggplot() +
  geom_sf(aes(fill = ndvi_slope))+
  scale_fill_gradient2()
slopes_ndvi %>% 
  filter(startsWith(GEOID, "51760")) %>% 
  ggplot() +
  geom_sf(aes(fill = mgr_slope))+
  scale_fill_gradient2()




slopes_ndvi %>% 
  filter() %>% 
  ggplot() +
  geom_point(aes(x = ahv_slope, y = ndvi_slope))+
  scale_fill_gradient2()


write.csv(as.data.frame(slopes_ndvi), "rsproject_ndvislopesworking_12.03.2024.csv")



test <- filtered_ndvi %>% 
  filter(startsWith(GEOID, "51760")) %>% 
  group_by(GEOID) %>% 
  


test
class(coef(lm(ahv ~ year, test))["year"])

#### OPTIONS - 
#  lag - what does that do?
## only useful for year over year changes? so calc all those changes  - one new col? change since previous? 
#  filter by urban areas?





 ###
# NEW COLUMNS FOR YEAR OVER YEAR (5YEARs)
# NEW COLUMNS FOR CHANGE SINCE START - to account for temporal lag?
## BOTH FOR NTL/NDVI and DEMO
# then regress the change in physical against he change in demo 
### CORR matrix? - one axis have all the change in physical, other axis have all the change in demographic?
  ## OOH YES
  

#### HOW TO DELAY - WANT TO LOOK AT CHANGES TO TOPOLGY AFTER CHANGE IN HOUSING PRICE?

### YEAR OVER YEAR

### Pivot Wide
filtered_ndvi_wide <- filtered_ndvi %>% 
  pivot_wider(names_from = year, values_from = c(ahv, mgr, ndvi))

ndvi_timeseries <- filtered_ndvi_wide %>% 
  mutate(
    mgr_1012 = mgr_2012 - mgr_2010,
    mgr_1217 = mgr_2017 - mgr_2012,
    mgr_1722 = mgr_2022 - mgr_2017,
    ahv_1012 = ahv_2012 - ahv_2010,
    ahv_1217 = ahv_2017 - ahv_2012,
    ahv_1722 = ahv_2022 - ahv_2017,
    ndvi_1012 = ndvi_2012 - ndvi_2010,
    ndvi_1217 = ndvi_2017 - ndvi_2012,
    ndvi_1722 = ndvi_2022 - ndvi_2017
  )
test <- as.data.frame(ndvi_timeseries) %>% select(mgr_1012:ndvi_1722)
cor(test, use = "complete.obs")
### 
#What Am I trying to do
# regress ndvi change against value/rent change? between each sample? between each possible combination?
# map one city: 4 maps - before and after each of 