# homeownership in Athens-Clarke County for Claire Bolton
# created: October 23, 2018
# Taylor

# load necessary libraries

library(tidyverse)
library(tidycensus)
#library(magrittr)
#library(stringr)
library(tigris)
#library(viridis)
library(sf)
#library(sp)
#library(rgdal)
library(tmap)
library(tmaptools)
library(ggplot2)
#library(janitor)
#library(maptools)
#library(htmlwidgets)
#library(leaflet)
options(tigris_use_cache = TRUE)

#th_api_acs <- '45544f0d114cfaa037a5566745d18bb8d4778cfa'

#v16 <- load_variables(year = 2010, dataset = "acs5") %>% View
#load_variables(year = 2015, dataset = "acs5") %>% View
#load_variables(year = 2010, dataset = "acs5") %>% View


# Various stakeholder types in Athens' historically Black neighborhoods have requested data 
# on Black homeownership in Athens and how it has changed over several decades. 
# Specifically, the national trend is that it went down significantly post-Recession, 
# and folks want to know these figures for Athens Clarke County.

# It would be great to have the most detail possible, e.g. block groups. Maps and summary of trends very helpful.

# How hard is it to get data from different periods, e.g. the late 1960s or 1970s? On this question

#I can get it for each of the  decennial census years. Data from 1960,70, 80 is available at the census tract (although census tracts were much larger then). From 1990 on I think it is available at block group level. starting in 2005 I can annual estimates


county10 <- get_acs(geography = "tract",
                    county = 'Clarke',
                    state = 'GA',
                    variables = c("B19013_001E",
                                  'B25001_001E',
                                  'B25002_002E',
                                  'B25002_003E',
                                  'B25003_001E',
                                  'B25003_002E',
                                  'B25003_003E'),
                    survey = "acs5",
                    year = 2010,
                    output = 'wide') %>%
  rename(hhincome10 = "B19013_001E",
         tothu10 = 'B25001_001E',
         totocc10 = 'B25002_002E',
         totvac10 = 'B25002_003E',
         tottenure10 = 'B25003_001E',
         ownocc10 = 'B25003_002E',
         rentocc10 = 'B25003_003E')  %>%
  mutate(hopct10 = round(100 * (ownocc10/tothu10), 1),
         rntpct10 = round(100 * (rentocc10/tothu10), 1),
         occpct10 = round(100 * (totocc10/tothu10), 1),
         vacpct10 = round(100 * (totvac10/tothu10), 1))

county16 <- get_acs(geography = "tract",
                    county = 'Clarke',
                    state = 'GA',
                    variables = c("B19013_001E",'B25001_001E','B25002_002E',
                                  'B25002_003E','B25003_001E','B25003_002E',
                                  'B25003_003E'),
                    survey = "acs5",
                    output = 'wide') %>%
  rename(hhincome16 = "B19013_001E",
         tothu16 = 'B25001_001E',
         totocc16 = 'B25002_002E',
         totvac16 = 'B25002_003E',
         tottenure16 = 'B25003_001E',
         ownocc16 = 'B25003_002E',
         rentocc16 = 'B25003_003E') %>%
  mutate(hopct16 = round(100 * (ownocc16/tothu16),1),
         rntpct16 = round(100 * (rentocc16/tothu16),1),
         occpct16 = round(100 * (totocc16/tothu16), 1),
         vacpct16 = round(100 * (totvac16/tothu16), 1))


load_variables(year = 2000, dataset = "sf1") %>% View

?get_decennial

county00 <- get_decennial(geography = "tract",
                    county = 'Clarke',
                    state = 'GA',
                    variables = c('H004001',
                                  'H004002',
                                  'H004003'),
                    year = 2000,
                    output = 'wide') %>%
  rename(tothu00 = 'H004001',
         ownocc00 = 'H004002',
         rentocc00 = 'H004003')  %>%
  mutate(hopct00 = round(100 * (ownocc00/tothu00), 1),
         rntpct00 = round(100 * (rentocc00/tothu00), 1))

county90 <- get_decennial(geography = "tract",
                          county = 'Clarke',
                          state = 'GA',
                          variables = c('H0010001',
                                        'H0030001',
                                        'H0030002'),
                          year = 1990,
                          output = 'wide') %>%
  rename(tothu90 = 'H0010001',
         ownocc90 = 'H0030001',
         rentocc90 = 'H0030002')  %>%
  mutate(hopct90 = round(100 * (ownocc90/tothu90), 1),
         rntpct90 = round(100 * (rentocc90/tothu90), 1))


acc_1980 <- read_csv("../ct_ga80.csv")

acc_1980sf <- read_sf("../nhgis0065_shape/nhgis0065_shapefile_tl2000_us_tract_1980/US_tract_1980.shp") %>%
  filter(NHGISST == 130,
         NHGISCTY == '0590')

tm_shape(acc_1980sf) +
  tm_borders()

##

acc_1970 <- read_csv("../ct_ga70.csv")

acc_1970sf <- read_sf("../nhgis0065_shape/nhgis0065_shapefile_tl2008_us_tract_1970") %>%
  filter(NHGISST == 130,
         NHGISCTY == '0590')

tm_shape(acc_1970sf) +
  tm_borders()







county16 <- county16 %>%
  mutate(state = str_sub(GEOID,1,2))









# EDA
county16 %>%
  filter(state == c(13, 38, 14,19, 23)) %>%
  ggplot() +
  geom_point(mapping = aes(x = hhincome16, y = hopct16, color = state))

sml16 <- county16 %>%
  arrange(hopct16) %>%
  select(GEOID, hhincome16, hopct16)

sml10 <- county10 %>%
  arrange(hopct10) %>%
  select(GEOID, hhincome10, hopct10)

county10 %>%
  filter(hopct10 < 60)
county16 %>%
  filter(hopct16 < 55)

smlcounty <- inner_join(county10, county16, by = "GEOID") %>%
  mutate(pctchange = round(100 * ((hopct16 - hopct10) / hopct10), 1),
         diff = hopct16 - hopct10,
         owndiff = ownocc16 - ownocc10) %>%
  filter(GEOID != "41980") %>%
  mutate(type = case_when(
    GEOID == "13059" ~ "Athens", 
    pctchange < 0 ~ "decline", 
    pctchange > 0 ~ "growth",
    diff < 0 ~ "d decline", 
    diff > 0 ~ "d growth"
  ))


ggplot(smlcounty) +
  geom_boxplot(mapping = aes(x = hopct16, y = diff, color = state))

ggplot(data = smlcounty) + 
  geom_boxplot(mapping = aes(x = pctchange, y = hopct16, color = state))


smlcounty %>%
  arrange(owndiff) %>%
  select(GEOID, NAME.x, owndiff, ownocc16, diff, hhincome16, hopct16, hopct10) %>%
  print(n = 40)

smlcounty %>%
  arrange(pctchange) %>%
  select(GEOID, pctchange, diff, hhincome16, hopct16, hopct10, NAME.x) %>%
  print(n = 40)

summary(smlcounty$owndiff)
summary(smlcounty$tothu16)





# Atlanta below
df_acs16 <- get_acs(geography = "tract",
                    county = c("Fulton","DeKalb","Gwinnett","Cherokee",
                               "Cobb","Douglas","Fayette","Clayton","Henry","Rockdale"),
                    state = "GA",
                    variables = c("B19013_001E",'B25001_001E','B25002_002E',
                                  'B25002_003E','B25003_001E','B25003_002E',
                                  'B25003_003E'),
                    survey = "acs5",
                    output = 'wide') %>%
  rename(hhincome16 = "B19013_001E",
         tothu16 = 'B25001_001E',
         totocc16 = 'B25002_002E',
         totvac16 = 'B25002_003E',
         tottenure16 = 'B25003_001E',
         ownocc16 = 'B25003_002E',
         rentocc16 = 'B25003_003E') %>%
  mutate(hopct16 = round(100 * (ownocc16/tothu16),1),
         rntpct16 = round(100 * rentocc16/tothu16),1,
         occpct16 = round(100 * totocc16/tothu16), 1,
         vacpct16 = round(100 * totvac16/tothu16), 1)









# St. Louis County

il2 <- get_acs(geography = "tract", 
               variables = c(hhincome = "B19013_001"), 
               state = "IL", 
               geometry = TRUE) %>%
  st_transform(4326)


county10 <- get_acs(geography = "tract",
                    variables = c("B19013_001E",
                                  'B25001_001E',
                                  'B25002_002E',
                                  'B25002_003E',
                                  'B25003_001E',
                                  'B25003_002E',
                                  'B25003_003E',
                                  'B02001_001E',
                                  'B02001_002E',
                                  'B02001_003E'),
                    county = "189",
                    state = "MO",
                    survey = "acs5",
                    year = 2010,
                    output = 'wide') %>%
  rename(hhincome10 = "B19013_001E",
         tothu10 = 'B25001_001E',
         totocc10 = 'B25002_002E',
         totvac10 = 'B25002_003E',
         tottenure10 = 'B25003_001E',
         ownocc10 = 'B25003_002E',
         rentocc10 = 'B25003_003E',
         tpop10 = 'B02001_001E',
         white10 = 'B02001_002E',
         black10 = 'B02001_003E')  %>%
  mutate(hopct10 = round(100 * (ownocc10/tothu10),1),
         rntpct10 = round(100 * (rentocc10/tothu10),1),
         occpct10 = round(100 * (totocc10/tothu10), 1),
         vacpct10 = round(100 * (totvac10/tothu10), 1),
         whtpct10 = round(100 * (white10/tpop10), 1),
         blkpct10 = round(100 * (black10/tpop10), 1))


# race, housing tenure, housing occupancy
county16 <- get_acs(geography = "tract", 
                    variables = c("B19013_001E",'B25001_001E','B25002_002E',
                                  'B25002_003E','B25003_001E','B25003_002E',
                                  'B25003_003E',  'B02001_001E',
                                  'B02001_002E',
                                  'B02001_003E',
                                  'B17001_001E',
                                  'B17001_002E'),
                    county = "189",
                    state = "MO",
                    survey = "acs5",
                    output = 'wide') %>%
  rename(hhincome16 = "B19013_001E",
         tothu16 = 'B25001_001E',
         totocc16 = 'B25002_002E',
         totvac16 = 'B25002_003E',
         tottenure16 = 'B25003_001E',
         ownocc16 = 'B25003_002E',
         rentocc16 = 'B25003_003E',
         tpop16 = 'B02001_001E',
         white16 = 'B02001_002E',
         black16 = 'B02001_003E',
         tpov16 = 'B17001_001E',
         ipov16 = 'B17001_002E') %>%
  mutate(hopct16 = round(100 * (ownocc16/tothu16),1),
         rntpct16 = round(100 * rentocc16/tothu16),1,
         occpct16 = round(100 * totocc16/tothu16), 1,
         vacpct16 = round(100 * totvac16/tothu16), 1,
         whtpct16 = round(100 * (white16/tpop16), 1),
         blkpct16 = round(100 * (black16/tpop16), 1),
         povrt16 = round(100 * (ipov16/tpov16),1))

#county16 <- county16 %>%
#  mutate(state = str_sub(GEOID,1,2))

# EDA
ggplot(county16, mapping = aes(x = blkpct16, y = hopct16)) +
  geom_point() +
  geom_smooth()

ggplot(county16) +
  geom_point(mapping = aes(x = blkpct16, y = povrt16))

ggplot(county16, mapping = aes(x = blkpct16, y = hhincome16)) +
  geom_point() +
  geom_smooth()

ggplot(county16, mapping = aes(x = blkpct16, y = povrt16, size = hopct16, alpha = 0.5)) +
  geom_point()

ggplot(county16) +
  geom_point(mapping = aes(x = blkpct16, y = vacpct16))

ggplot(county16, mapping = aes(x = blkpct16, y = povrt16, size = vacpct16, alpha = 0.5)) +
  geom_point()

sml16 <- county16 %>%
  arrange(hopct16) %>%
  select(GEOID, hhincome16, hopct16)

sml10 <- county10 %>%
  arrange(hopct10) %>%
  select(GEOID, hhincome10, hopct10)

county10 %>%
  filter(hopct10 < 60)
county16 %>%
  filter(hopct16 < 55)

smlcounty <- inner_join(county10, county16, by = "GEOID") %>%
  mutate(pctchange = round(100 * ((hopct16 - hopct10) / hopct10), 1),
         diff = hopct16 - hopct10,
         owndiff = ownocc16 - ownocc10) %>%
  filter(GEOID != "41980") %>%
  mutate(type = case_when(
    GEOID == "13059" ~ "Athens", 
    pctchange < 0 ~ "decline", 
    pctchange > 0 ~ "growth",
    diff < 0 ~ "d decline", 
    diff > 0 ~ "d growth"
  ))

ggplot(smlcounty) +
  geom_boxplot(mapping = aes(x = hopct16, y = diff, color = state))

ggplot(data = smlcounty) + 
  geom_boxplot(mapping = aes(x = pctchange, y = hopct16, color = state))

smlcounty %>%
  arrange(owndiff) %>%
  select(GEOID, NAME.x, owndiff, ownocc16, diff, hhincome16, hopct16, hopct10) %>%
  print(n = 40)

smlcounty %>%
  arrange(pctchange) %>%
  select(GEOID, pctchange, diff, hhincome16, hopct16, hopct10, NAME.x) %>%
  print(n = 40)

summary(smlcounty$owndiff)
summary(smlcounty$tothu16)
