## Libraries ##
library(tidyverse)
library(geojsonsf)
library(sf)
library(mapview)
library(RCurl)
library(RJSONIO)
library(rgdal)
library(raster)
library(lubridate)
library(devtools)
library(jsonlite)
library(tidycensus)
library(DBI)
library(RPostgres)

## Read in data ##
# NOTES: 
# To do this, you'll need a RSocrata token
# You will also need to download a couple files from NYC Open Data
  # NYCHA GeoJson: https://data.cityofnewyork.us/Housing-Development/Map-of-NYCHA-Developments/i9rv-hdr5
        # I named this "nycha_map.geojson"
  # NYC Air Quality Data: https://data.cityofnewyork.us/Environment/NYCCAS-Air-Pollution-Rasters/q68s-8qxv
        # Unzip this file in the workspace
# Also, download the raster_data_guide CSV available in this repository

# Data in Dataset: 
  # Health Data (CDC) -- DONE
  # NYCHA Population/Developments -- DONE
  # Air Quality -- DONE
  # Income (ACS) -- DONE
  # Population (ACS) -- DONE
  # Residential Units (PUMA) -- DONE 
  # Residential Zoning (PUMA) -- DONE

nycha <- RSocrata::read.socrata("https://data.cityofnewyork.us/resource/evjd-dqpz.json",
                       app_token = getPass::getPass("socrata token:"))

nycha_sf <- geojsonsf::geojson_sf("nycha_map.geojson") %>% 
  rename(development = developmen)

nycha_sf <- nycha_sf %>% left_join(nycha, by = "development") %>% write_rds("nycha_sf.RDS")

health <- RSocrata::read.socrata("https://chronicdata.cdc.gov/resource/6vp6-wxuq.json",
                                 getPass::getPass("socrata token:"))

ny_health <- health %>% filter(cityname == "New York" & geographiclevel == "Census Tract") %>% 
  write_rds("ny_health.RDS")

raster_guide <- read_csv("raster_data_guide.csv")


# Convert raster air quality data to points and aggregate within the census tracts to get yearly averages by tract

for (i in 1:nrow(raster_guide)) {
  DEM <- raster(paste0("AnnAvg1_9_300mRaster/AnnAvg1_9_300mRaster/" ,raster_guide[i , 1] , "/w001001.adf"))
  DEM <- rasterToPoints(DEM , spatial = T)
  DEM <- sf::st_as_sf(DEM)
  DEM <- st_join(DEM, tract_sf, join = st_within)
  if (crs(DEM) != crs(tract_sf)) {DEM <- st_transform(DEM , tract_sf)}
  DEM <- DEM %>% group_by(GEOID) %>% 
    summarise(new_col = mean(w001001)) %>% as_tibble() %>% 
    select(GEOID , new_col)
  names(DEM)[2] <- paste(raster_guide[i , 1])
  tract_sf <- tract_sf %>% left_join(DEM , by = "GEOID")
}

# Some tracts do not directly overlap with the raster centroids
# For these tracts, take the average of the neighbor tracts

na_vals <- tract_sf %>% filter(is.na(aa1_pm300m)) 
neighbor_tracts <- st_touches(na_vals , tract_sf)

for (i in 1:length(neighbor_tracts)) {
  tf <- tract_sf[neighbor_tracts[[i]],] %>% na.omit() %>% summarise_at(raster_guide$dataset , mean) %>% as_tibble() %>% 
    mutate(GEOID = na_vals[i,]$GEOID) %>% select(GEOID , everything() , -geometry)
  
  tract_sf[tract_sf$GEOID == na_vals[i,]$GEOID , raster_guide$dataset] <- tf[,raster_guide$dataset]
  
}

rm(neighbor_tracts , na_vals)


nycha_sf <- st_transform(nycha_sf , crs(tract_sf))
tf <- st_join(nycha_sf , tract_sf , largest = T) 

tf <- tf %>% mutate(total_population = as.numeric(str_remove_all(total_population , ",")) ,
                    population_public_housing = as.numeric(str_remove_all(total_population , ",")) ,
                    population_section_8_transition = as.numeric(str_remove_all(population_section_8_transition , ",")) ,
                    number_of_current_apartments = as.numeric(str_remove_all(number_of_current_apartments , ",")) ,
                    number_of_section_8_transition_apartments = as.numeric(str_remove_all(number_of_section_8_transition_apartments , ",")) ,
                    total_area_sq_ft = as.numeric(str_remove_all(total_area_sq_ft , ",")) , 
                    development_cost = as.numeric(str_remove_all(development_cost , "[,\\$]")) ,
                    per_rental_room = as.numeric(str_remove_all(per_rental_room , "[,\\$]")) ,
                    avg_monthly_gross_rent = as.numeric(str_remove_all(avg_monthly_gross_rent , "[,\\$]")))

nycha_tracts <- tf %>% 
  group_by(GEOID) %>% summarise_at(c("total_population" , "population_public_housing" , "population_section_8_transition" , 
                                    "number_of_section_8_transition_apartments" , "number_of_current_apartments" , "total_area_sq_ft" , 
                                    "development_cost") , sum ) %>% 
  left_join(tf %>% group_by(GEOID) %>% 
              summarise(developments = list(development) , 
                        hud_amp = list(hud_amp_) ,
                        electric = list(electricity_paid_by_residents) ,
                        private = list(private_management) ,
                        senior = list(senior_development)) %>% 
              as_tibble() %>% select(-geometry), by = "GEOID") %>% 
  as_tibble() %>% select(-geometry) %>% 
  mutate(nycha = 1) %>% 
  rename(nycha_population = total_population) %>% 
  select(GEOID , nycha , everything())

tract_sf <- tract_sf %>% 
  left_join(nycha_tracts , by = "GEOID") %>% 
  replace_na(list(
    nycha = 0 ,
    nycha_population = 0 ,
    population_public_housing = 0 ,
    population_section_8_transition = 0 ,
    number_of_section_8_transition_apartments = 0 , 
    number_of_current_apartments = 0 , 
    total_area_sq_ft = 0 , 
    development_cost = 0
  ))


health_data_dictionary <- ny_health %>% 
  group_by(measure) %>% 
  select(measure , 
         data_value_unit ,
         data_value_type , 
         category , measureid , 
         short_question_text) %>% 
  distinct()

tf <- ny_health %>% 
  reshape2::dcast(tractfips ~ measureid , value.var = "data_value") %>% 
  mutate_at(health_data_dictionary$measureid , as.numeric)

tract_sf <- tract_sf %>% left_join(tf , by = c("GEOID" = "tractfips"))  
         
## Get some census data 

census_api_key(getPass::getPass("census key:"))

# NOTE: 2018 Census 5-year estimates will be released December 19, 2019

#v17 <- load_variables(2017, "acs5", cache = TRUE)
#View(load_variables(2017, "acs5", cache = TRUE))

# Household Income Variable ACS 2017: "B19013_001"
# Total Population Variable ACS 2017: "B01003_001"
# Total Population White: B02001_002
# Total Population Black: B02001_003
# Total Population Asian: B02001_005
# Total Population Two or More Races: B02001_008
# Total Population Latino: B03003_003

## Low Income Census Tracts Defined as: 

#the poverty rate is at least 20 percent, or
#the median family income does not exceed 80 percent of statewide median family income or, 
#if in a metropolitan area, the great of 80 percent statewide median family income 
#or 80 percent of metropolitan area median family income

pov <- readxl::read_excel("census_poverty.xlsx")

pov <- pov %>% select(GEOID = GEO.id2 , 
                      poverty_pct = HC03_EST_VC01) %>% 
  mutate(poverty_pct = as.numeric(poverty_pct) , 
         GEOID = as.character(as.numeric(GEOID)))

tf <- get_acs(geography = "tract", 
                      variables = c(medincome = "B19013_001" ,
                                    population = "B01003_001" , 
                                    white = "B02001_002" ,
                                    black = "B02001_003" , 
                                    asian = "B02001_005" , 
                                    mixed = "B02001_008" , 
                                    latino = "B03003_003"), 
                      state = "NY" ,
                      county = c("new york" , "bronx" , "kings" , "queens" , "richmond"))

tf <- tf %>% 
  reshape2::dcast(GEOID ~ variable , value.var = "estimate")

tf <- tf %>% left_join(pov , by = "GEOID")

medinc_state <- get_acs("state" , variable = c(medincome = "B19013_001") , state = "NY")$estimate
medinc_met <- get_acs("metropolitan statistical area/micropolitan statistical area" , 
                      variable = c(medincome = "B19013_001")
                      ) %>% filter(NAME == "New York-Newark-Jersey City, NY-NJ-PA Metro Area") %>% pull(estimate)

tf <- tf %>% mutate(low_income = if_else(poverty_pct >= 20 | medincome <= 0.8*medinc_met , 1 , 0) , 
                    low_income = factor(low_income))

tract_sf <- tract_sf %>% 
  left_join(tf , by = "GEOID")


# Now some more housing data #

con <- dbConnect(
  RPostgres::Postgres(),
  user = getPass::getPass("NYC-DB USER:"),
  password = getPass::getPass("NYC-DB PASSWORD:"),
  host = getPass::getPass("NYC-DB HOST:"),
  port = 5432,
  dbname = "nycdb"
)

#dbListTables(con)

tf <- tbl(con , "pluto_19v1")

tf <- tf %>% 
  select(borough , block , lot , bbl , ct2010 , tract2010 , zonedist1 , zonedist2 , zonedist3 , zonedist4 , overlay1 , overlay2 , spdist1 , spdist2 , spdist3 ,
         landuse , lotarea , bldgarea , resarea , officearea , retailarea , comarea , builtfar , residfar , commfar , facilfar , unitsres , unitstotal , 
         zonemap , lat , lng )

tf <- collect(tf)

dbDisconnect(con) ; rm(con)

# Try to find if the development in the census tract is predominately residential

tf <- tf %>% mutate(
  zone1 = str_sub(zonedist1 , 1 , 1) ,
  zone2 = str_sub(zonedist2 , 1 , 1) ,
  zone3 = str_sub(zonedist3 , 1 , 1) ,
  zone4 = str_sub(zonedist4 , 1 , 1) 
) %>% replace_na(list(bldgarea = 0 , 
                      resarea = 0 ,
                      unitsres = 0 ,
                      unitstotal = 0)) %>% 
  group_by(borough , ct2010) %>% 
  summarise_at(c("bldgarea" , "resarea" , "unitsres" , "unitstotal") , sum)

tf <- tf %>% mutate(res_tract = if_else( ( unitsres > unitstotal/2 | resarea > bldgarea/2 ) & unitsres != 0, T , F)) %>% 
  left_join(
    bind_cols(borough = c("BK" , "QN" , "MN" , "BX" , "SI") , 
              county = c("Kings County" , 
                         "Queens County" ,
                         "New York County" ,
                         "Bronx County" ,
                         "Richmond County" )) , by = "borough" )

tract_sf <- tract_sf %>% 
  left_join(tf , by = c(c("county" = "county") , c("NAME" = "ct2010")))


#write_rds(tract_sf , "nycha_project_data_clean.RDS")
