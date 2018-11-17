library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(tmap)
library(tmaptools)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(viridisLite)
options(tigris_use_cache = TRUE)

# Athens-Clarke County homeownership, 2010 decennial census
acc16b <- get_acs(geography = "block group",
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
acc10b <- get_decennial(geography = "block group",
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

acc00b <- get_decennial(geography = "block group",
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


# Athens-Clarke County homeownership, 1990 Decennial census
# acc90b2 <- get_decennial(geography = "block group",
#                        variables = c('H0010001',
#                                      'H0040001',
#                                      'H0080001'),
#                        year = 1990,
#                        sumfile = sf1,
#                        county = 'Clarke',
#                        state = 'GA',
#                        geometry = TRUE,
#                        output = 'wide') %>%
#   rename(tothu90 = 'H0010001',
#          occ90 = 'H0040001',
#          ownocc90 = 'H0080001') %>%
#   mutate(hopct90 = round(100 * (ownocc90/tothu90), 1),
#          hopct2 = round(100 * (ownocc90/occ90), 1))

acc1990b <- read_sf("../clarke_90.shp")


# maps
tm_shape(acc1990b) +
  tm_fill("ho_pct", breaks = c(0, 10, 30, 60, 80, 100),
          title = '1990', palette = "Blues")

tm_shape(acc00b) +
  tm_fill("hopct2", breaks = c(0, 10, 30, 60, 80, 100),
          title = '2000', palette = "Blues")

tm_shape(acc10b) +
  tm_fill(col = "hopct2", breaks = c(0, 10, 30, 60, 80, 100),
          title = '2010', palette = "Blues")

tm_shape(acc16b) +
  tm_fill("hopct2", n = 4,
          title = '2012-16 ACS', palette = "Blues")
  
class(acc1990b)
class(acc16b)

(acc1990b)


# I have county data for 1990 - 2016 ACS
# I have bg 1990-2016 ACS
# I have ct 1970-2016 ACS

# need to make maps (block group and census tract)
# write up