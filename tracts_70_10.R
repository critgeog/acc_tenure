# homeownership in Athens-Clarke County for Claire Bolton
# created: October 23, 2018
# last updated: November 17, 2018

library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(tmap)
library(tmaptools)
library(ggplot2)
options(tigris_use_cache = TRUE)

#th_api_acs <- '45544f0d114cfaa037a5566745d18bb8d4778cfa'

#v16 <- load_variables(year = 2010, dataset = "acs5") %>% View
#load_variables(year = 2015, dataset = "acs5") %>% View
#load_variables(year = 2010, dataset = "acs5") %>% View


# Various stakeholder types in Athens' historically Black neighborhoods have requested data 
# on Black homeownership in Athens and how it has changed over several decades. 
# Specifically, the national trend is that it went down significantly post-Recession, 
# and folks want to know these figures for Athens Clarke County.

# It would be great to have the most detail possible, e.g. block groups.
# Maps and summary of trends very helpful.

# How hard is it to get data from different periods, e.g. the late 1960s or 1970s? 
# On this question

#I can get it for each of the  decennial census years. 
# Data from 1960,70, 80 is available at the census tract 
# (although census tracts were much larger then). 
# From 1990 on I think it is available at block group level. 
# starting in 2005 I can annual estimates

# Athens-Clarke County homeownership, 2012-16 ACS
acc16t <- get_acs(geography = "tract",
                  county = 'Clarke',
                  state = 'GA',
                  variables = c('B25001_001E',
                                'B25002_002E',
                                'B25003_001E',
                                'B25003_002E'),
                  survey = "acs5",
                  year = 2016,
                  geometry = TRUE,
                  output = 'wide') %>%
  rename(tothu16 = 'B25001_001E',
         totocc16 = 'B25002_002E',
         tentot16 = 'B25003_001E',
         ownocc16 = 'B25003_002E') %>%
  mutate(hopct16 = round(100 * (ownocc16/tothu16),1),
         hopct2 = round(100 * (ownocc16/tentot16),1),
         occpct16 = round(100 * (totocc16/tothu16), 1))

# Athens-Clarke County homeownership, 2010 decennial census
acc10t <- get_decennial(geography = "tract",
                    county = 'Clarke',
                    state = 'GA',
                  variables = c('H00010001',
                                'H0040001',
                                'H0040002',
                                'H0040003'),
                  geometry = TRUE,
                  year = 2010,
                  output = 'wide') %>%
  rename(tothu10 = 'H00010001',
         tentot10 = 'H0040001',
         ownfree10 = 'H0040003',
         ownmort10 = 'H0040002')  %>%
  mutate(hopct10 = round(100 * ((ownfree10+ownmort10)/tothu10), 1),
         hopct2 = round(100 * ((ownfree10+ownmort10)/tentot10),1))

#load_variables(year = 2000, dataset = "sf1") %>% View

# Athens-Clarke County homeownership, 2000 Decennial census
acc00t <- get_decennial(geography = "tract",
                    county = 'Clarke',
                    state = 'GA',
                    variables = c('H001001',
                                  'H004001',
                                  'H004002',
                                  'H004003'),
                    year = 2000,
                    geometry = TRUE,
                    output = 'wide') %>%
  rename(tothu00 = 'H001001',
         totten00 = 'H004001', 
         ownocc00 = 'H004002',
         rentocc00 = 'H004003')  %>%
  mutate(hopct00 = round(100 * (ownocc00/tothu00), 2),
         hopct2 = round(100 * (ownocc00/totten00), 2),
         rntpct00 = round(100 * (rentocc00/tothu00), 2))


acc90t <- get_decennial(geography = "tract",
                        variables = c('H0010001',
                                      'H0030001',
                                      'H0020001'),
                        year = 1990,
                        county = 'Clarke',
                        state = 'GA',
                        geometry = TRUE,
                        output = 'wide') %>%
  rename(tothu90 = 'H0010001',
         ownocc90 = 'H0030001',
         occ90 = 'H0020001') %>%
  mutate(hopct90 = round(100 * (ownocc90/tothu90), 1),
         hopct2 = round(100 * (ownocc90/occ90), 1))

# acc_1980 <- read_csv("../ct_ga80.csv")

# maps
rds <- roads('GA', 'Clarke')

ct90 <- tm_shape(acc90t) +
  tm_fill("hopct2", breaks = c(0, 10, 30, 60, 80, 100),
          title = 'Census Tract, Homeownership Rate', palette = "Blues") +
  tm_layout(title = 'Athens-Clarke County \n1990',
            legend.title.size = .8) +
  tm_shape(rds) + 
  tm_lines(col = 'black', alpha = .2)

ct00 <- tm_shape(acc00t) +
  tm_fill("hopct2", breaks = c(0, 10, 30, 60, 80, 100),
          title = 'Census Tract, Homeownership Rate', palette = "Blues") +
  tm_layout(title = 'Athens-Clarke County \n2000',
            legend.title.size = .8) +
  tm_shape(rds) + 
  tm_lines(col = 'black', alpha = .2)

ct10 <- tm_shape(acc10t) +
  tm_fill("hopct2", breaks = c(0, 10, 30, 60, 80, 100),
          title = 'Census Tract, Homeownership Rate', palette = "Blues") +
  tm_layout(title = 'Athens-Clarke County \n2010',
            legend.title.size = .8) +
  tm_shape(rds) + 
  tm_lines(col = 'black', alpha = .2)

ct16 <- tm_shape(acc16t) +
  tm_fill("hopct2", breaks = c(0, 10, 30, 60, 80, 100),
          title = 'Census Tract, Homeownership Rate', palette = "Blues") +
  tm_layout(title = 'Athens-Clarke County \n2012-16 ACS',
            legend.title.size = .8) +
  tm_shape(rds) + 
  tm_lines(col = 'black', alpha = .2)

# read in 1970 and 1980 census tracts from NHGIS
acc_1980 <- read_csv("../ct_ga80.csv")

acc_1980sf <- read_sf("../nhgis0065_shape/nhgis0065_shapefile_tl2000_us_tract_1980/US_tract_1980.shp") %>%
  filter(NHGISST == 130,
         NHGISCTY == '0590') %>%
  st_transform(4269)

# acc90b <- acc1990b %>%
#   st_transform(4269)

acc_1980d <- inner_join(acc_1980sf, acc_1980, by = "GISJOIN") %>%
  mutate(ho_pct = C7W001/(C7W001+C7W002)*100)

ct80 <- tm_shape(acc_1980d) +
  tm_polygons("ho_pct", breaks = c(0, 10, 30, 60, 80, 100),
              title = 'Census Tract, Homeownership Rate', palette = "Blues") +
  tm_layout(title = 'Athens-Clarke County \n1980',
            legend.title.size = .8) +
  tm_shape(rds) + 
  tm_lines(col = 'black', alpha = .2)

##

acc_1970 <- read_csv("../ct_ga70.csv")

acc_1970sf <- read_sf("../nhgis0065_shape/nhgis0065_shapefile_tl2008_us_tract_1970") %>%
  filter(NHGISST == 130,
         NHGISCTY == '0590') %>%
  st_transform(4269)

acc_1970d <- inner_join(acc_1970sf, acc_1970, by = "GISJOIN") %>%
  mutate(ho_pct = CFA001/(CFA001 + CFA003)*100)

tm_shape(acc_1970d) +
  tm_polygons("ho_pct", breaks = c(0, 10, 30, 60, 80, 100),
              title = 'Census Tract, Homeownership Rate', palette = "Blues",
              border.col = 'black')

ct70 <- tm_shape(acc_1970d) +
  tm_polygons("ho_pct", breaks = c(0, 10, 30, 60, 80, 100),
              title = 'Census Tract, Homeownership Rate', palette = "Blues",
              border.alpha = .3) +
  tm_borders(col = 'black') +
  tm_layout(title = 'Athens-Clarke County \n1970',
            legend.title.size = .8) +
  tm_shape(rds) + 
  tm_lines(col = 'black', alpha = .2)
ct70

ct16
save_tmap(ct70, "maps/ct70.png")
save_tmap(ct80, "maps/ct80.png")
save_tmap(ct90, "maps/ct90.png")
save_tmap(ct00, "maps/ct00.png")
save_tmap(ct10, "maps/ct10.png")
save_tmap(ct16, "maps/ct16.png")

acc16t %>%
  summarize(mean = mean(hopct2),
            median = median(hopct2),
            min = min(hopct2),
            max = max(hopct2))

boxplot(acc90t$hopct2)
boxplot(acc_1970d$ho_pct)
# need to work on this and below

smlcounty <- st_join(sml10, sml16) %>%
  mutate(pctchange = round(100 * ((hopct16 - hopct10) / hopct10), 1),
         diff = hopct16 - hopct10,
         owndiff = ownocc16 - ownocc10) %>%
  mutate(type = case_when(
    pctchange < 0 ~ "decline", 
    pctchange > 0 ~ "growth",
    diff < 0 ~ "d decline", 
    diff > 0 ~ "d growth"
  ))


ggplot(smlcounty) +
  geom_boxplot(mapping = aes(x = hopct16, y = diff, color = GEOID.x))

ggplot(data = smlcounty) + 
  geom_boxplot(mapping = aes(x = pctchange, y = hopct16, color = GEOID.x))