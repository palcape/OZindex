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


tracts <- tracts()

test <- get_acs(geography = "tract", 
                table = "B17026_001", 
                output = "tidy",
                state = x,
                year = 2017,
                cache_table = T,
                summary_var = summary_var) %>% 
  clean_names() %>% 
  left_join(acsvars) %>% 
  select(-summary_moe, -variable) %>% 
  select(geoid, county = name, level, levlab, estimate, everything()) %>% 
  rename(!!summary_label := summary_est)


c<- si_acs("B17026")
B01001B <- si_acs("B01001B")
B01001I <- si_acs("B01001I")
B01001C <- si_acs("B01001C")
B01001D <- si_acs("B01001D")
B01001E <- si_acs("B01001E")
B01001F <- si_acs("B01001F")
B01001G <- si_acs("B01001G")

totalage <- B01001 %>%
  filter(levlab %in% c("18 and 19 years", "20 years", "21 years", "22 to 24 years")) %>%
  select(geoid, county, estimate, label) %>%
  spread(label, estimate) %>%
  clean_names() %>%
  mutate(total18to24 = estimate_total_female_18_and_19_years + estimate_total_female_20_years + estimate_total_female_21_years + estimate_total_female_22_to_24_years + estimate_total_male_18_and_19_years + estimate_total_male_20_years + estimate_total_male_21_years + estimate_total_male_22_to_24_years) %>%
  select(geoid, county, total18to24)

agefunc <- function(data) {
  data %>%
  filter(levlab %in% c("18 and 19 years", "20 to 24 years")) %>%
  select(geoid, county, estimate, label) %>%
  spread(label, estimate) %>%
  clean_names() %>%
  mutate(total18to24 = estimate_total_female_18_and_19_years + estimate_total_female_20_to_24_years + estimate_total_male_18_and_19_years + estimate_total_male_20_to_24_years) %>%
      select(geoid, county, total18to24)

}

blackage <- B01001B %>% agefunc() %>% rename(black_18to24 = total18to24)
hispage <- B01001I %>% agefunc() %>% rename(hisp_18to24 = total18to24)
amindage <- B01001C %>% agefunc() %>% rename(amind_18to24 = total18to24)
asianage <- B01001D %>% agefunc() %>% rename(asian_18to24 = total18to24)
piage <- B01001E %>% agefunc() %>% rename(pi_18to24 = total18to24)
otherage <- B01001F %>% agefunc() %>% rename(otherage_18to24 = total18to24)
twomoreage <- B01001G %>% agefunc() %>% rename(twomore_18to24 = total18to24)

acsage <- totalage %>% full_join(blackage) %>% full_join(hispage) %>% full_join(amindage) %>% full_join(asianage) %>% full_join(piage) %>% full_join(otherage) %>% full_join(twomoreage)

acsage <- acsage %>% separate(county, into = c("county", "state"), sep = ", ")

gradpercent <- read_rds(path(datapath, "ACS/gradsbyrace.rds")) %>%
  select(-state) %>% rename(state = state2)

acsage <- acsage %>% full_join(gradpercent)

acsage <- acsage %>%
  rowwise() %>%
  mutate(avgpercentother = mean(c(`American Indian-Alaska Native`, Asian, `Asian-Pacific Islander Total`, `Pacific Islander`, `Two or more races`), na.rm = T)) %>%
  ungroup %>%
  mutate(nonwhite_18to24 = amind_18to24 + asian_18to24 + pi_18to24 + otherage_18to24 + twomore_18to24) %>%
  mutate(avgpercentother = avgpercentother*.01) %>%
  mutate(totalpercent = gen_pop*.01) %>%
  mutate(blackpercent = Black*.01) %>%
  mutate(hisppercent = Hispanic*.01)

acsage <- acsage %>%
  mutate(totalhsgrad_18to24 = total18to24*totalpercent) %>%
  mutate(blackhsgrad_18to24 = black_18to24*blackpercent) %>%
  mutate(hisphsgrad_18to24 = hisp_18to24*hisppercent) %>%
  mutate(otherhsgrad_18to24 = otherage_18to24*avgpercentother) %>%
  mutate(percent_black_18to24_acs = blackhsgrad_18to24/totalhsgrad_18to24) %>%
  mutate(percent_hisp_18to24_acs = hisphsgrad_18to24/totalhsgrad_18to24) %>%
  mutate(percent_other_18to24_acs = otherhsgrad_18to24/totalhsgrad_18to24)


