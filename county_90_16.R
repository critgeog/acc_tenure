library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(tmap)
library(tmaptools)
library(ggplot2)
options(tigris_use_cache = TRUE)

#load_variables(2016, "acs5") %>% View()
#load_variables(2010, "sf1") %>% View()
#load_variables(2000, "sf1") %>% View()
#load_variables(1990, 'sf3') %>% View()

# Athens-Clarke County homeownership, 2012-16 ACS
acc16 <- get_acs(geography = "county",
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

# Athens-Clarke County homeownership, 2006-10 ACS; this number is higher than 2010 Census (higher number is 06-09)
# acc10co <- get_acs(geography = "county",
#                   county = 'Clarke',
#                   state = 'GA',
#                   variables = c('B25001_001E',
#                                 'B25002_002E',
#                                 'B25003_001E',
#                                 'B25003_002E'),
#                   survey = "acs5",
#                   geometry = TRUE,
#                   year = 2010,
#                   output = 'wide') %>%
#   rename(tothu10 = 'B25001_001E',
#          totocc10 = 'B25002_002E',
#          tentot10 = 'B25003_001E',
#          ownocc10 = 'B25003_002E')  %>%
#   mutate(hopct10 = round(100 * (ownocc10/tothu10), 1),
#          hopct2 = round(100 * (ownocc10/tentot10),1),
#          occpct10 = round(100 * (totocc10/tothu10), 1))

# Athens-Clarke County homeownership, 2010 decennial census
acc10 <- get_decennial(geography = "county",
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


# Athens-Clarke County homeownership, 2000 Decennial census
acc00 <- get_decennial(geography = "county",
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
acc90 <- get_decennial(geography = "county",
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

tm_shape(acc16) +
  tm_fill("hopct2",title = '2012-16 ACS', palette = "Blues")

fivenum(acc16$hopct2)
