# Exporting Data



## Weather Variability
fourteen <- read.csv("/Users/calpape/Downloads/StormEvents2014.csv")
fifteen <- read.csv("/Users/calpape/Downloads/StormEvents2015.csv")
sixteen <- read.csv("/Users/calpape/Downloads/StormEvents2016.csv")
seventeen <- read.csv("/Users/calpape/Downloads/StormEvents2017.csv")
eighteen <- read.csv("/Users/calpape/Downloads/StormEvents2018.csv")

saveRDS(fourteen, "StormEvents2014.rds")
saveRDS(fifteen, "StormEvents2015.rds")
saveRDS(sixteen, "StormEvents2016.rds")
saveRDS(seventeen, "StormEvents2017.rds")
saveRDS(eighteen, "StormEvents2018.rds")


## County Business Patterns
cbp2 <- read.csv2("/Users/calpape/Downloads/cbp.txt", sep = ",")
saveRDS(cbp2, "CBP.rds")


## Change in Employment and Establishments
library(readxl)
janfebmar <- read_xlsx("/Users/calpape/Downloads/Establishments/janfebmar.xlsx")
aprmayjun <- read_xlsx("/Users/calpape/Downloads/Establishments/aprmayjun.xlsx")
julaugsep <- read_xlsx("/Users/calpape/Downloads/Establishments/julaugsep.xlsx")
octnovdem <- read_xlsx("/Users/calpape/Downloads/Establishments/octnovdem.xlsx")

saveRDS(janfebmar, "janfebmar.rds")
saveRDS(aprmayjun, "aprmayjun.rds")
saveRDS(julaugsep, "julaugsep.rds")
saveRDS(octnovdem, "octnovdem.rds")


## Households Under 200% FPL
library(crayon)
library(tidycensus)
library(tidyverse)
library(janitor)
library(tigris)

acsvars <- load_variables(2017, "acs5", cache = T) %>% 
  mutate(level = str_count(label, pattern = "!!")) %>% 
  rowwise() %>% 
  mutate(levlab = str_split(label, pattern = "!!") %>% unlist() %>% .[level + 1]) %>% 
  ungroup() %>% 
  mutate(concept = str_to_title(concept)) %>% 
  rename(variable = name)

us <- unique(fips_codes$state)[1:51]

totalpoppov <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B17026_001", 
          state = x)
})
saveRDS(totalpoppov, "totalpop.rds")

popunderhalf <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B17026_002", 
          state = x)
})
saveRDS(popunderhalf, "popunderhalf.rds")

pophalftopointsevenfive <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B17026_003", 
          state = x)
})
saveRDS(pophalftopointsevenfive, "2HalfToThreeFourths.rds")

popThreeFourthsToOne <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B17026_004", 
          state = x)
})
saveRDS(popThreeFourthsToOne, "3ThreeFourthsToOne.rds")

popOneToFiveFourths <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B17026_005", 
          state = x)
})
saveRDS(popOneToFiveFourths, "4OneToFiveFourths.rds")

popFiveFourthsToThreeHalves <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B17026_006", 
          state = x)
})
saveRDS(popFiveFourthsToThreeHalves, "5FiveFourthsToThreeHalves.rds")

popThreeHalvesToSevenFourths <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B17026_007", 
          state = x)
})
saveRDS(popThreeHalvesToSevenFourths, "6ThreeHalvesToSevenFourths.rds")

popSevenFourthsToOnePointEightFive <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B17026_008", 
          state = x)
})
saveRDS(popSevenFourthsToOnePointEightFive, "7SevenFourthsToOnePointEightFive.rds")

popPointEightFiveToTwo <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B17026_009", 
          state = x)
})
saveRDS(popPointEightFiveToTwo, "PointEightFiveToTwo.rds")


## Vacancy Status
VacancyTotal <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B25004_001", 
          state = x)
})
saveRDS(VacancyTotal, "VacancyTotal.rds")

RentedNotOccupied <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B25004_003", 
          state = x)
})
saveRDS(RentedNotOccupied, "RentedNotOccupied.rds")

SoldNotOccupied <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B25004_005", 
          state = x)
})
saveRDS(RentedNotOccupied, "SoldNotOccupied.rds")

OtherVacant <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B25004_008", 
          state = x)
})
saveRDS(RentedNotOccupied, "OtherVacant.rds")

## Median Household Rent
Median <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B25058_001", 
          state = x)
})
saveRDS(Median, "MedianRentDollars.rds")










si_acs <- function(table, county = NULL, state = NULL, summary_var = "universe total") {
  cat(yellow(bold("Reminder: You must stay within the same level for any summary to be valid!\n")))
  
  if(summary_var == "universe total") summary_var = paste0(table, "_001")
  summary_label = acsvars %>% filter(variable == summary_var) %>% pull(levlab) 
  
  get_acs(geography = "tract", 
          table = table, 
          county = county,
          state = state,
          output = "tidy",
          year = 2017,
          cache_table = T,
          summary_var = summary_var) %>% 
    clean_names() %>% 
    left_join(acsvars) %>% 
    select(-summary_moe, -variable) %>% 
    select(geoid, county = name, level, levlab, estimate, everything()) %>% 
    rename(!!summary_label := summary_est)
}
