library(tidyverse)
library(here)

load(here("data/shapefile.rda"))
shdf <- sh32 %>% as.data.frame %>% select(-geometry)

#' # District population projections
#' * Malawi NSO projections (obtained from DHA)

nso <- here::here("data-raw", "population", "Pop projections  2008-2030 Master file_transformed.xlsx") %>%
  readxl::read_excel("dataset") %>%
  gather(key, value, -year, -agegr) %>%
  mutate(district32 = sub("(.*) - (.*)", "\\1", key),
         sex = tolower(sub("(.*) - (.*)", "\\2", key)),
         agegri = match(agegr, c(paste0(0:15*5, "-", 0:15*5+4), "80+"))) %>%
  filter(district32 != "Total",
         agegr != "Total",
         sex != "total") %>%
  mutate(district = sub(" City", "", district32),
         district = sub("Mzuzu", "Mzimba", district)) %>%
  group_by(district, district32, sex, agegr, agegri, year) %>%
  summarise(pop = sum(value))


#' Interpolate mid-year population estimates to quarterly

pop_agesex <- nso %>%
  mutate(year = year + 0.5) %>%
  complete(year = full_seq(year, 0.25),
           nesting(district, district32, sex, agegr, agegri)) %>%
  group_by(district, district32, sex, agegr, agegri) %>%
  mutate(pop = zoo::na.approx(pop, year))



#' # Adjust population projections to 2018 census results.
#' * Source: http://www.nsomalawi.mw/images/stories/data_on_line/demography/census_2018/2018%20Population%20and%20Housing%20Census%20Preliminary%20Report.pdf
#'
#' ## Calculate district adjustment factor based on 2018 census results
#'
#' * Distribute the age 15+ census population according to distribution of age 18+
#' adults in census.
#' * Scale NSO projections proportionally at all ages

#' ### Total district population by sex

tab1 <- readxl::read_excel("Malawi Census 2018.xlsx",  sheet  = "Tab1")

distpop2018 <- tab1 %>%
  gather(sex, pop, 2:4) %>%
  rename(district32  = `Region and District`) %>%
  filter(!district32 %in% c("Malawi", "Northern", "Central", "Southern"),
         sex != "Total") %>%
  mutate(sex = tolower(sex)) %>%
  mutate(district = sub(" City", "", district32),
         district = sub("Mzuzu", "Mzimba", district)) %>%
  group_by(district, district32, sex) %>%
  summarise(pop = sum(pop)) %>%
  ungroup



#' ### District population age 18+ by sex

tab5 <- readxl::read_excel("Malawi Census 2018.xlsx",  sheet  = "Tab5")

pop18pl <- tab5 %>%
  gather(sex, pop, 2:4) %>%
  rename(district32 = `Region and District`) %>%
  filter(!district32 %in% c("Malawi", "Northern", "Central", "Southern"),
         sex != "Total 18+") %>%
  mutate(sex = tolower(sub(" 18\\+", "", sex))) %>%
  mutate(district = sub(" City", "", district32),
         district = sub("Mzuzu", "Mzimba", district)) %>%
  group_by(sex, district, district32) %>%
  summarise(pop = sum(pop)) %>%
  group_by(sex) %>%
  mutate(proportion = prop.table(pop)) %>%
  ungroup

#' ### National population by age and sex

tab6 <- readxl::read_excel("Malawi Census 2018.xlsx",  sheet  = "Tab6", skip = 1)

censagesex <-  tab6 %>%
  gather(lbl,  pop,  -1) %>%
  rename(agegr = `Age Group`) %>%
  mutate(restype = fct_collapse(lbl, "total" = c("Both Sexes..2", "Male..3", "Female..4"),
                                "urban" = c("Both Sexes..5", "Male..6", "Female..7"),
                                "rural" = c("Both Sexes..8", "Male..9", "Female..10")),
         sex = sub("\\.\\.[0-9+]", "",  lbl) %>% tolower,
         agegr = fct_collapse(agegr, "0-4" = c("<1 yr",  "1-4"),
                              "80+" = c("80-84", "85-89", "90-94", "95+"))) %>%
  group_by(restype, sex, agegr) %>%
  summarise(pop = sum(pop)) %>%
  ungroup

#' Calculate district-level scalar to adjust 2018 population projection to
#' match 2018 census population

popadjust <- pop18pl %>%
  left_join(censagesex %>%
            filter(restype == "total",
                   agegr %in% c(paste0(3:15*5, "-", 3:15*5+4), "80+"),
                   sex %in% c("male", "female")) %>%
            group_by(sex) %>%
            summarise(nat15pl = sum(pop))) %>%
  mutate(cens15pl = proportion * nat15pl) %>%
  left_join(
    pop_agesex %>%
    filter(year == 2018.5,
           agegr %in% c(paste0(3:15*5, "-", 3:15*5+4), "80+")) %>%
    group_by(district, district32, sex) %>%
    summarise(nso15pl = sum(pop))
  ) %>%
  mutate(popadjust2018 = cens15pl / nso15pl)

#' Log-linear interpolation of adjustment factors from 2008 to 2018
pop_agesex_adult <- pop_agesex %>%
  filter(agegri >= 4) %>%
  left_join(popadjust %>% select(district, district32, sex, popadjust2018)) %>%
  mutate(popadjust = exp(log(popadjust2018) * pmax((year - 2008.5) / (2018.5 - 2008.5), 0.0)),
         pop_cens18adj = pop * popadjust)


#' ## Paediatric population adjustment

#' 1. Adjust by age group

popadjust_agegr <- pop_agesex %>%
  filter(year == 2018.5,
         agegr %in% c("0-4", "5-9", "10-14")) %>%
  group_by(sex, agegr) %>%
  summarise(nsopop = sum(pop)) %>%
  left_join(
    censagesex %>%
    ungroup %>%
    filter(restype == "total",
           sex %in% c("male", "female"),
           agegr %in% c("0-4", "5-9", "10-14")) %>%
    select(sex, agegr, cens18pop = pop) 
  ) %>%
  mutate(popadjust_agegr2018 = cens18pop / nsopop)

pop_agesex_paed <- pop_agesex %>%
  filter(agegr %in% c("0-4", "5-9", "10-14")) %>%
  left_join(popadjust_agegr %>% select(agegr, sex, popadjust_agegr2018)) %>%
  mutate(popadjust_agegr = exp(log(popadjust_agegr2018) *
                               pmax((year - 2008.5) / (2018.5 - 2008.5), 0.0)),
         pop_cens18adj = pop * popadjust_agegr)
  


#' 2. Adjust below 15 population by district

popadjust_district <- left_join(distpop2018, rename(pop18pl, pop18pl = pop)) %>%
  group_by(sex, district, district32) %>%
  summarise(pop = sum(pop),
            pop18pl = sum(pop18pl)) %>%
  group_by(sex) %>%
  mutate(popbel18 = (pop - pop18pl),
         prop_bel18 = popbel18/ pop,
         proportion = prop.table(popbel18)) %>%
  left_join(
    censagesex %>%
            filter(restype == "total",
                   agegr %in% c("0-4", "5-9", "10-14"),
                   sex %in% c("male", "female")) %>%
    group_by(sex) %>%
    summarise(natbel15 = sum(pop))
  ) %>%
  mutate(censbel15 = proportion * natbel15) %>%
  left_join(
    pop_agesex_paed %>%
    filter(year == 2018.5,
           agegr %in% c("0-4", "5-9", "10-14")) %>%
    group_by(district, district32, sex) %>%
    summarise(nsobel15 = sum(pop_cens18adj))
  ) %>%
  mutate(popadjust_district2018 = censbel15 / nsobel15) %>%
  as.data.frame

pop_agesex_paed <- pop_agesex_paed %>%
  left_join(popadjust_district %>%
            select(district, district32, sex, popadjust_district2018)) %>%
  mutate(popadjust = exp(log(popadjust_district2018) * pmax((year - 2018.5) / (2018.5 - 2008.5), 0.0)),
         pop_cens18adj = popadjust * pop_cens18adj)


## Check that these are pretty close
censagesex %>%
  filter(restype == "total",
         sex %in% c("male", "female"),
         agegr %in% c("0-4", "5-9", "10-14"))

filter(pop_agesex_paed, year == 2018.5) %>%
  group_by(sex, agegr) %>%
  summarise(sum(pop), sum(pop_cens18adj))

pop_agesex <-
  bind_rows(
    pop_agesex_paed %>%
    select(-popadjust_agegr2018, -popadjust_agegr, -popadjust_district2018, -popadjust),
    pop_agesex_adult %>% select(-popadjust2018, -popadjust)
  ) %>%
  ungroup %>%
  right_join(shdf %>% select(district32, area_id), .) %>%
  select(-district, -district32)

pop_aggr <- pop_agesex %>%
  group_by(area_id, year) %>%
  summarise(pop15pl = sum(pop * (agegri %in% 4:17)),
            pop15to64 = sum(pop * (agegri %in% 4:13)),
            pop15to49 = sum(pop * (agegri %in% 4:10)),
            pop15pl_cens18adj = sum(pop_cens18adj * (agegri %in% 4:17)),
            pop15to64_cens18adj = sum(pop_cens18adj * (agegri %in% 4:13)),
            pop15to49_cens18adj = sum(pop_cens18adj * (agegri %in% 4:10))) %>%
  ungroup


save(pop_agesex, pop_aggr, file = here::here("data", "population.rda"))
