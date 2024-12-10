 ###
### OWNER-OCCUPIED VALUE DP04_0089, except EARLIER - EARLIER WAS DP04_088
### FIND WHEN CHANGED

### DECENNIAL: TRY "H076001"
###


install.packages(c("tidyverse", "tidycensus", "sf", "dplyr"))
library(tidyverse)
library(tidycensus)
library(sf)
library(dplyr)

census_api_key("753915d84691b3657721d57a715b97feafbbf81a")

acs_call <- function(yr = 2022, varlist = var_list, states = state_list, level = "tract", surv = "acs5", geo = TRUE, dur = FALSE) {
  start <- Sys.time()
  output <- data.frame()
  for (state in states) {
    temp <- get_acs(
      geography = level,
      variables = varlist,
      survey = surv,
      year = yr,
      state = state,
      output = "wide",
      geometry = geo
    )
    output <- rbind(output, temp)
  }
  if (dur == TRUE) {print(Sys.time()-start)}
  return(output)
}

dec_call <- function(yr = 2022, varlist = var_list, states = state_list, level = "tract", surv = "acs5", geo = TRUE, dur = FALSE) {
  start <- Sys.time()
  output <- data.frame()
  for (state in states) {
    temp <- get_decennial(
      geography = level,
      variables = varlist,
      year = yr,
      state = state,
      output = "wide",
      geometry = geo
    )
    output <- rbind(output, temp)
  }
  if (dur == TRUE) {print(Sys.time()-start)}
  return(output)
}

##FIX PATH FUNCTION FOR MAC/WINDOWS
fix_path <- function(fullpath) {
  #if windows path on mac
  if (startsWith(fullpath, "//hemisphere") & 
      Sys.info()[['sysname']] == "Darwin") {
    return(gsub("//hemisphere", "/Volumes", fullpath))
    #if mac path on pc
  } else if (startsWith(fullpath, "/Volumes") & Sys.info()[['sysname']] == "Windows") {
    return(gsub("/Volumes", "//hemisphere", fullpath))
  } else {
    return(fullpath)
  }
}
### Define Statelist Function
## User can filter by desired states to be used in index. Defaults to contiguous US, excluding DC. 
## Other options are: "all", "all_states", "all_pr", "all_dc", "all_dc_pr", "contig_dc"
get_state_list <- function(states = "contig") {
  #Loads all state fips codes
  all <- c("01","02","04","05","06","08","09","10","11","12","13","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","44","45","46","47","48","49","50","51","53","54","55","56","60","66","69","72","74","78")
  if (states == "contig") {
    #removes non states, and hawaii and alaska
    return(all[-which(all %in% c(alaska = "02", 
                                 hawaii = "15",
                                 dc = "11",
                                 puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "all") {
    #removes nothing
    return(all)  
  } else if (states == "all_states") {
    #removes nonstates
    return(all[-which(all %in% c(dc = "11",
                                 puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "all_pr") {
    #removes non states, leaving Puerto Rico
    return(all[-which(all %in% c(dc = "11",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "all_dc") {
    #removes non states, leaving District of Columbia
    return(all[-which(all %in% c(puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "all_dc_pr") {
    #removes non states, leaving Puerto Rico and District of Columbia
    return(all[-which(all %in% c(guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "contig_dc") {
    #removes non states, removes Hawaii and Alaska, leaving District of Columbia
    return(all[-which(all %in% c(alaska = "02", 
                                 hawaii = "15",
                                 puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  }
}

state_list <- get_state_list(state = "all_dc")

us_tract_geos <- acs_call(yr = 2022, varlist = c("dummy" = "B01003_001"), dur = T)

us_tract_geos <- us_tract_geos %>% 
  select(GEOID, geometry)

us_tract_geos_noempty <- us_tract_geos %>% 
  filter(!st_is_empty(.))


us_cousub_geos <- acs_call(yr = 2022, level = "county subdivision", varlist = c("dummy" = "B01003_001"), dur = T)
us_cousub_geos_noempty <- us_cousub_geos %>% 
  select(GEOID, geometry) %>% 
  filter(!st_is_empty(.))

st_write(us_cousub_geos_noempty, "//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/Base Shapefiles/geo_countysub_2022.shp")
st_write(us_tract_geos_noempty, "//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/Data/Base Shapefiles/geo_tract_2022.shp")


us_zcta_geos <- st_read("C://Users/ab5nm/Downloads/tl_2022_us_zcta520/tl_2022_us_zcta520.shp") %>% 
  select(GEOID20, geometry)

################################################################################
##########     DEMO DATA     ###################################################
################################################################################
acs <- load_variables(2022, "acs5")
acsprofile 
dec <- 
dec_1990 <- load_variables(2010, "sf3")
dec

#ACS : MEDIAN GROSS RENT, renter OCCUPIED HOUSING UNITS, CASH RENT: B25064_001
# ACS : Median Value Owner Occupied  HOUSING UNITS B25077_001
### DECENNIAL: MEDIAN GROSS RENT: H063001
### DECENNIAL: MEDIAN VALUE ALL OWNER OCCUPIED: H085001

###PULL TRACTS

# 1990
filename <- file.choose()
print(filename)
tract_1990_mgr <- read.csv("H:\\STUDENTS\\GEOG360\\Alex Broening\\FinalProject\\Data\\censusdata_1990\\nhgis0002_ds123_1990_tract.csv") %>% 
  mutate(fips = substr(paste0(substr(GISJOIN, 2, 3), substr(GISJOIN, 5,7), substr(GISJOIN, 9, nchar(GISJOIN)), "00"), 1, 11)) %>% 
  select(fips, EYU001) %>% 
  rename(., mgr_1990 = EYU001)
  
tract_1990_ahv <- read.csv("H:\\STUDENTS\\GEOG360\\Alex Broening\\FinalProject\\Data\\censusdata_1990\\nhgis0002_ds120_1990_tract.csv") %>% 
  mutate(fips = substr(paste0(substr(GISJOIN, 2, 3), substr(GISJOIN, 5,7), substr(GISJOIN, 9, nchar(GISJOIN)), "00"), 1, 11)) %>% 
  select(fips, EST001) %>% 
  rename(., ahv_1990 = EST001) 

tract_1990_mhi <- read.csv("H:\\STUDENTS\\GEOG360\\Alex Broening\\FinalProject\\Data\\censusdata_1990\\nhgis0003_ds123_1990_tract_mhi.csv") %>% 
  mutate(fips = substr(paste0(substr(GISJOIN, 2, 3), substr(GISJOIN, 5,7), substr(GISJOIN, 9, nchar(GISJOIN)), "00"), 1, 11)) %>% 
  select(fips, E4U001) %>% 
  rename(., mhi_1990 = E4U001) 

tract_1990 <- tract_1990_mgr %>% 
  left_join(tract_1990_ahv, by = "fips") %>% 
  left_join(tract_1990_mhi, by = "fips")

# 2000

tract_2000 <- dec_call(
  yr = 2000, 
  varlist = c("mgr"="H063001", "ahv" = "H085001", "mhi" = "P053001"),
  geo = F,
  level = "tract"
  )
tract_2000 <- tract_2000 %>% 
  select(-NAME) %>% 
  rename_with(., ~ paste0(.x, "_2000", recycle0 = TRUE), !starts_with("GEOID"))


### 2010
tract_2010 <- acs_call(
  yr = 2010, 
  varlist = c("mgr" = "B25064_001", "ahv" = "B25077_001", "mhi" = "B19013_001"),
  geo = F, 
  surv = "acs5",
  level = "tract" 
)
tract_2010 <- tract_2010 %>% 
  select(-mgrM, -ahvM, -mhiM, -NAME) %>% 
  rename_with(., ~ paste0(substr(.x, 1, 3), "_2010", recycle0 = TRUE), !starts_with("GEOID"))

# 2012
tract_2012 <- acs_call(
  yr = 2012, 
  varlist = c("mgr" = "B25064_001", "ahv" = "B25077_001", "mhi" = "B19013_001"),
  geo = F, 
  surv = "acs5",
  level = "tract" 
)
tract_2012 <- tract_2012 %>% 
  select(-mgrM, -ahvM, -mhiM, -NAME) %>% 
  rename_with(., ~ paste0(substr(.x, 1, 3), "_2012", recycle0 = TRUE), !starts_with("GEOID"))


# 2017
tract_2017 <- acs_call(
  yr = 2017, 
  varlist = c("mgr" = "B25064_001", "ahv" = "B25077_001", "mhi" = "B19013_001"),
  geo = F, 
  surv = "acs5",
  level = "tract" 
)
tract_2017 <- tract_2017 %>% 
  select(-mgrM, -ahvM, -mhiM, -NAME) %>% 
  rename_with(., ~ paste0(substr(.x, 1, 3), "_2017", recycle0 = TRUE), !starts_with("GEOID"))

# 2022
tract_2022 <- acs_call(
  yr = 2022, 
  varlist = c("mgr" = "B25064_001", "ahv" = "B25077_001", "mhi" = "B19013_001"),
  geo = F, 
  surv = "acs5",
  level = "tract" 
)
tract_2022 <- tract_2022 %>% 
  select(-mgrM, -ahvM, -mhiM, -NAME) %>% 
  rename_with(., ~ paste0(substr(.x, 1, 3), "2022_", recycle0 = TRUE), !starts_with("GEOID"))


### MERGE TRACTS

tract_demo <- tract_2022 %>% 
  left_join(tract_2017, by = "GEOID") %>% 
  left_join(tract_2012, by = "GEOID") %>% 
  left_join(tract_2010, by = "GEOID") %>% 
  left_join(tract_2000, by = "GEOID") %>% 
  left_join(tract_1990, by = c("GEOID"= "fips"))


write.csv(tract_demo, file.choose())
### "rsproject_tractdemo_11.30.2024"



##########################
#COUNTYSUB DEMO
countysub_2012 <- acs_call(
  yr = 2012, 
  varlist = c("mgr" = "B25064_001", "ahv" = "B25077_001", "mhi" = "B19013_001"),
  geo = F, 
  surv = "acs5",
  level = "county subdivision" 
)

countysub_2017 <- acs_call(
  yr = 2017, 
  varlist = c("mgr" = "B25064_001", "ahv" = "B25077_001", "mhi" = "B19013_001"),
  geo = F, 
  surv = "acs5",
  level = "county subdivision" 
)

countysub_2022 <- acs_call(
  yr = 2022, 
  varlist = c("mgr" = "B25064_001", "ahv" = "B25077_001", "mhi" = "B19013_001"),
  geo = F, 
  surv = "acs5",
  level = "county subdivision" 
)

countysub_2012 <- countysub_2012 %>% 
  select(-mgrM, -ahvM, -mhiM, -NAME) %>% 
  rename_with(., ~ paste0(substr(.x, 1, 3), "_2012", recycle0 = TRUE), !starts_with("GEOID"))

countysub_2017 <- countysub_2017 %>% 
  select(-mgrM, -ahvM, -mhiM, -NAME) %>% 
  rename_with(., ~ paste0(substr(.x, 1, 3), "_2017", recycle0 = TRUE), !starts_with("GEOID"))

countysub_2022 <- countysub_2022 %>% 
  select(-mgrM, -ahvM, -mhiM, -NAME) %>% 
  rename_with(., ~ paste0(substr(.x, 1, 3), "_2022", recycle0 = TRUE), !starts_with("GEOID"))

countysub_demo <- countysub_2022 %>% 
  left_join(countysub_2017, by = "GEOID") %>% 
  left_join(countysub_2012, by = "GEOID")

write.csv(countysub_demo, file.choose())
### "rsproject_countysubdemo_11.30.2024"



zct### 1990, 2000, 2007, 2012, 2017, 2022

### Housing Value 
###     Owner occupied value
###


acsvars_2022 <- load_variables(2022, "acs5")



#####GEOGRAPHIES
va_tracts_geo <- get_acs(
  geography = "tract", 
  variables = c("dummy" = "B01003_001"),
  geometry = T,
  state = "VA", 
  year = 2022
) %>% 
  select(-variable, -estimate, -moe) %>% 
  filter(!st_is_empty(geometry))

st_write(va_tracts_geo, "//hemisphere/geopower/STUDENTS/GEOG360/Alex Broening/FinalProject/va_tract_geo.shp")
