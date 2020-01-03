## The Challenge: OurHomes.nyc ##

## Helpful Links ## ----

# http://a816-dohbesp.nyc.gov/IndicatorPublic/traffic/index.html

# https://ssrc-static.s3.amazonaws.com/moa/OurHome.NYC%20data%20challenge%20one-pager%20CBOs.pdf
# https://www.politico.com/states/new-york/albany/story/2018/04/06/the-long-term-health-consequences-of-living-at-nycha-352931
# https://medium.com/asthma-in-the-south-bronx/asthma-by-the-numbers-73553b2c9621
# https://nccd.cdc.gov/500_Cities/rdPage.aspx?rdReport=DPH_500_Cities.InteractiveMap&islCategories=HLTHOUT&islMeasures=ARTHRITIS&islStates=36&rdRnd=55175
# https://www.health.ny.gov/environmental/indoors/air/pmq_a.htm
# https://www.dec.ny.gov/chemical/8539.html
# http://a816-dohbesp.nyc.gov/IndicatorPublic/traffic/index.html


# CDC Chronic Health Data
# https://chronicdata.cdc.gov/resource/6vp6-wxuq.json

# Air polution Raster
# https://data.cityofnewyork.us/Environment/NYCCAS-Air-Pollution-Rasters/q68s-8qxv
# https://www1.nyc.gov/assets/doh/downloads/pdf/environmental/comm-air-survey-08-16.pdf

## Libraries ## ----

#devtools::install_github("nsgrantham/ggdark")

library(tidyverse)
library(geojsonsf)
library(sf)
library(mapview)
library(lubridate)
library(caret)
library(ggridges)
library(ggdark)
library(scales)

## Load Data ## ----

tract_sf <- read_rds("nycha_project_data_clean.RDS")

df <- tract_sf %>% as_tibble() %>% 
  select(-geometry) %>% mutate(nycha = factor(nycha)) %>% 
  na.omit(CASTHMA) %>% 
  mutate(nycha2_ = if_else(number_of_current_apartments > unitsres/2 , 1 , 0) , 
         nycha2_ = factor(nycha2_))

df.linc <- df %>% filter(low_income == 1)

## Stats ## ----

corMatrix <- cor(df.linc[c("aa9_no2300m" , "aa9_no300m" , "aa9_pm300m" , "w8_so2300m" , "aa9_bc300m" , 
                           "s8_o3300m" , "CASTHMA" )])

print(corMatrix)

fit <- lm(formula = CASTHMA ~  CSMOKING + medincome + black * nycha2_, data = df) ; summary(fit) 

# https://uc-r.github.io/t_test

ggplot(df, aes(CASTHMA)) + 
  geom_histogram(fill = "white", color = "grey30") +
  scale_x_log10()

t.test(CASTHMA ~ nycha2_ , df)
wilcox.test(CASTHMA ~ nycha2_ , df)

# Paired T Test 

df.t <- bind_rows(sample_n(df.linc %>% filter(nycha2_ == 1) , 50) , 
                  sample_n(df.linc %>% filter(nycha2_ == 0) , 50)
                  )

t.test(CASTHMA ~ nycha2_ , df.t , paired = T)
wilcox.test(CASTHMA ~ nycha2_ , df.t , paired = T)


df.t<- function() {
  bind_rows(sample_n(df.linc %>% filter(nycha2_ == 1) , 50) , 
            sample_n(df.linc %>% filter(nycha2_ == 0) , 50)) 
  } 

estimates <- replicate(n = 10 , t.test(CASTHMA ~ nycha2_ , df.t() , paired = T)$estimate ) ; plot(estimates) ; summary(estimates)
 
## Data Visualizations ## ----

## Asthma Rate by Median Income ##----

## All Census Tracts

ggplot(df %>% mutate(x = log(medincome) , y = log(CASTHMA)) , aes(x , y)) + 
  geom_hex(aes(colour = factor(nycha2_) , weight = 2.5 ) , bins = 30 ) + 
  scale_color_manual(values = c("cyan") , limits = "1" , labels = c("NYCHA Tract") , name = "" ) + 
  ggdark::dark_mode(theme_ridges()) +
  labs(title = "Asthma rates across Census Tracts in NYC" , 
       subtitle = "Current asthma among adults aged >=18 Years/Median Income" , 
       x = "Median Income" , y = "Asthma Rate" , fill = "# of Tracts" ,
       caption = "Sources: Center for Disease Control and Prevention 500 Cities Project, 2019\nAmerican Community Survey, 2017 5-year Estimates") +
  scale_x_continuous(labels = function(x) paste0( "$" , round(exp(x) , 0)) ) +
  scale_y_continuous(labels = function(x) paste0( round(exp(x) , 0) , "%") ) +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6) ,
    plot.caption = element_text(face = "italic" , size = 8)
  ) +
  guides(colour = guide_legend(order = 1))

## Low Income Census Tracts

ggplot(df.linc %>% mutate(x = log(medincome) , y = log(CASTHMA)) , aes(x , y)) + 
  geom_hex(aes(colour = factor(nycha2_) , weight = 2.5 ) , bins = 30 ) + 
  scale_color_manual(values = c("cyan") , limits = "1" , labels = c("NYCHA Tract") , name = "" ) + 
  ggdark::dark_mode(theme_ridges()) +
  labs(title = "Asthma rates across Low Income Census Tracts in NYC" , 
       subtitle = "Current asthma among adults aged >=18 Years/Median Income" , 
       x = "Median Income" , y = "Asthma Rate" , fill = "# of Tracts" ,
       caption = "Sources: Center for Disease Control and Prevention 500 Cities Project, 2019\nAmerican Community Survey, 2017 5-year Estimates") +
  scale_x_continuous(labels = function(x) paste0( "$" , round(exp(x) , 0)) ) +
  scale_y_continuous(labels = function(x) paste0( round(exp(x) , 0) , "%") ) +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6) ,
    plot.caption = element_text(face = "italic" , size = 8)
  ) +
  guides(colour = guide_legend(order = 1))

## Asthma Rate by Air Quality ##----

## All Census Tracts

ggplot(df %>% filter(res_tract == T) %>% 
         group_by(GEOID) %>% 
         mutate(x = mean(aa1_pm300m , aa2_pm300m , aa3_pm300m , aa4_pm300m , 
                         aa5_pm300m , aa6_pm300m , aa7_pm300m , aa8_pm300m ,
                         aa9_pm300m) , 
                y = CASTHMA) , aes(x , y)) + 
  geom_hex(aes(colour = factor(nycha2_) , weight = 2.5 ) , bins = 30 ) + 
  scale_color_manual(values = c("cyan") , limits = "1" , labels = c("NYCHA Tract") , name = "" ) + 
  ggdark::dark_mode(theme_ridges()) +
  labs(title = "Asthma rates across Census Tracts in NYC" , 
       subtitle = "Current asthma among adults aged >=18 Years/Air Quality" , 
       x = "Average Fine Particulate Matter (Dec. 2008 - Dec. 2017)" , y = "Asthma Rate" , fill = "# of Tracts" ,
       caption = "Sources: Center for Disease Control and Prevention 500 Cities Project, 2019\nNYC Community Air Survey, 2019") +
  scale_x_continuous(labels = function(x) paste(x , "ug/m3") ) +
  scale_y_continuous(labels = function(x) paste0(x , "%") ) +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6) ,
    plot.caption = element_text(face = "italic" , size = 8)
  ) +
  guides(colour = guide_legend(order = 1))

## Low Income Census Tracts 

ggplot(df.linc %>% filter(res_tract == T) %>% 
         group_by(GEOID) %>% 
         mutate(x = mean(aa1_pm300m , aa2_pm300m , aa3_pm300m , aa4_pm300m , 
                         aa5_pm300m , aa6_pm300m , aa7_pm300m , aa8_pm300m ,
                         aa9_pm300m) , 
                y = CASTHMA) , aes(x , y)) + 
  geom_hex(aes(colour = factor(nycha2_) , weight = 2.5 ) , bins = 30 ) + 
  scale_color_manual(values = c("cyan") , limits = "1" , labels = c("NYCHA Tract") , name = "" ) + 
  ggdark::dark_mode(theme_ridges()) +
  labs(title = "Asthma rates across Low Income Census Tracts in NYC" , 
       subtitle = "Current asthma among adults aged >=18 Years/Air Quality" , 
       x = "Average Fine Particulate Matter (Dec. 2008 - Dec. 2017)" , y = "Asthma Rate" , fill = "# of Tracts" ,
       caption = "Sources: Center for Disease Control and Prevention 500 Cities Project, 2019\nNYC Community Air Survey, 2019") +
  scale_x_continuous(labels = function(x) paste(x , "ug/m3") ) +
  scale_y_continuous(labels = function(x) paste0(x , "%") ) +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6) ,
    plot.caption = element_text(face = "italic" , size = 8)
  ) +
  guides(colour = guide_legend(order = 1))

## Asthma Rates by Race ##----

# All Census Tracts

ggplot(df %>% filter(res_tract == T) %>% 
         mutate(x = black/population , 
                y = CASTHMA) , aes(x , y)) + 
  geom_hex(aes(colour = factor(nycha2_) , weight = 2.5 ) , bins = 30 ) + 
  scale_color_manual(values = c("cyan") , limits = "1" , labels = c("NYCHA Tract") , name = "" ) + 
  ggdark::dark_mode(theme_ridges()) +
  labs(title = "Asthma rates across Census Tracts in NYC" , 
       subtitle = "Current asthma among adults aged >=18 Years/Black Population" , 
       x = "African-American/Black Population" , y = "Asthma Rate" , fill = "# of Tracts" ,
       caption = "Sources: Center for Disease Control and Prevention 500 Cities Project, 2019\nAmerican Community Survey, 2017 5-year Estimates") +
  scale_x_continuous(labels = function(x) paste(x*100 , "%") ) +
  scale_y_continuous(labels = function(x) paste0(x , "%") ) +
  theme(
    legend.position = c(.95, .1),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6) ,
    plot.caption = element_text(face = "italic" , size = 8)
  ) +
  guides(colour = guide_legend(order = 1))

## Low Income CT

ggplot(df.linc %>% filter(res_tract == T) %>% 
         mutate(x = black/population , 
                y = CASTHMA) , aes(x , y)) + 
  geom_hex(aes(colour = factor(nycha2_) , weight = 2.5 ) , bins = 30 ) + 
  scale_color_manual(values = c("cyan") , limits = "1" , labels = c("NYCHA Tract") , name = "" ) + 
  ggdark::dark_mode(theme_ridges()) +
  labs(title = "Asthma rates across Low Income Census Tracts in NYC" , 
       subtitle = "Current asthma among adults aged >=18 Years/Black Population" , 
       x = "African-American/Black Population" , y = "Asthma Rate" , fill = "# of Tracts" ,
       caption = "Sources: Center for Disease Control and Prevention 500 Cities Project, 2019\nAmerican Community Survey, 2017 5-year Estimates") +
  scale_x_continuous(labels = function(x) paste(x*100 , "%") ) +
  scale_y_continuous(labels = function(x) paste0(x , "%") ) +
  theme(
    legend.position = c(.95, .1),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6) ,
    plot.caption = element_text(face = "italic" , size = 8)
  ) +
  guides(colour = guide_legend(order = 1))



