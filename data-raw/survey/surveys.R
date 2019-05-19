library(survey)
library(tidyverse)
library(lubridate)
library(haven)
library(sf)
library(rdhs)

load(here::here("data", "shapefile.rda"))
load(here::here("data", "population.rda"))

#' Outcomes:
#' * prevalence
#' * SE
#' * unweighted counts
#'
#' Indicators
#' * HIV prevalence
#' * ART coverage among HIV+
#' * Proportion recently infected
#'
#' Stratification
#' * Age 15-49 for HIV prevalence and recency
#' * Age 15-64 for ARV coverage


#' # PHIA ART coverage measured by ARV biomarker

bio <- rdhs::read_zipdata("~/Documents/Data/PHIA/release/Malawi/datasets/02_MPHIA 2015-2016 Adult Biomarker Dataset (DTA).zip", readfn = haven::read_dta)
ind <- rdhs::read_zipdata("~/Documents/Data/PHIA/release/Malawi/datasets/02_MPHIA 2015-2016 Adult Interview Dataset (DTA).zip",
                          readfn = haven::read_dta)

phia <- bio %>%
  left_join(ind %>% select(personid, zone, urban))

zone_labels <- c("Northern" = 1,
                 "Central East" = 2,
                 "Central West" = 3,
                 "Lilongwe City" = 4,
                 "South East" = 5,
                 "South West" = 6,
                 "Blantyre City" = 7)

#' Aggregate area_ids and population size by zone 
zone_sample <- sh32 %>%
  as.data.frame %>%
  mutate(stratum = if_else(district32 %in% c("Blantyre City", "Lilongwe City"), as.character(district32), as.character(zone))) %>%
  left_join(
    pop_aggr %>%
    filter(year == 2016.25) %>%
    select(area_id, pop15to64)
  ) %>%
  group_by(stratum) %>%
  summarise(area_ids = list(area_id),
            area_pops = list(pop15to64))

#' PHIA survey cluster ids
ge <- phia %>%
  mutate(stratum = factor(zone, zone_labels, names(zone_labels)),
         restype = factor(urban, 1:2, c("urban", "rural"))) %>%
  group_by(stratum, restype, varstrat, varunit) %>%
  summarise(n_hh = n_distinct(householdid),
            n_ind = n_distinct(personid)) 

#' Sample area_id for each cluster proportional to population size
sample2 <- function(x, size, replace = FALSE, prob = NULL) {
  x[sample.int(length(x), size, replace, prob)]
}

set.seed(3261722)
ge <- ge %>%
  left_join(zone_sample) %>%
  mutate(area_id = Map(sample2, area_ids, 1, TRUE, area_pops) %>% unlist) %>%
  ungroup %>%
  left_join(
    sh32 %>% as.data.frame %>%
    select(area_id, region, zone, district, district32)
  )

#' Check to confirm area_id is in correct zone
ge %>%
  left_join(sh32 %>% as.data.frame %>% select(area_id, zone)) %>%
  count(stratum, zone)

#' Number of clusters by district
ge %>%
  count(stratum, zone, district32, area_id) %>%
  arrange(n) %>%
  as.data.frame
  

phia <- ge %>%
  select(varstrat, varunit, region, zone, stratum,
         district, district32, restype) %>%
  right_join(bio %>% filter(!is.na(varstrat))) %>%
  transmute(surveyid = "MW2016PHIA",
            year = 2016,
            stratum = stratum,
            district,
            district32,
            cluster = 10*varstrat + varunit,
            restype = restype,
            sex = labelled(gender, c("male" = 1, "female" = 2)) %>% as_factor,
            age = age,
            hivweight = btwt0,
            prev = ifelse(hivstatusfinal %in% 1:2, hivstatusfinal == 1, NA) %>% as.integer,
            arv  = ifelse(arvstatus %in% 1:2, arvstatus == 1, NA) %>% as.integer,
            vls = na_if(vls, 99) == 1 %>% as.integer,
            artself = na_if(artselfreported, 99) == 1 %>% as.integer,
            artall = ifelse(!is.na(arv) & !is.na(artself), arv | artself,
                     ifelse(is.na(arv) & is.na(artself), FALSE,
                     ifelse(is.na(arv), artself,
                     ifelse(is.na(artself), arv, NA)))) %>% as.integer,
            recent = ifelse(recentlagvlarv %in% 1:2, recentlagvlarv == 1, NA) %>% as.integer)
         
                          

#' # DHS survey data



#' ## 2004,

ar04 <- rdhs::get_datasets("MWar4Afl.zip")[[1]] %>% readRDS
ir04 <- rdhs::get_datasets("MWIR4EFL.ZIP")[[1]] %>% readRDS
mr04 <- rdhs::get_datasets("MWMR4EFL.ZIP")[[1]] %>% readRDS
ge04 <- rdhs::get_datasets("MWGE4BFL.zip")[[1]] %>% readRDS %>% st_as_sf

ge04 <- ge04 %>%
  left_join(
    ir04 %>% 
    transmute(DHSCLUST = v001,
              district = as_factor(sdist2) %>%
                sub("nkhota kota", "nkhotakota", .) %>%
                str_to_title) %>%
    unique
  )

setdiff(ge04$district, sh32$district)

par(ask = TRUE)

lapply(c("Blantyre", "Lilongwe", "Mzimba", "Zomba"),
       function(str) {
         ggplot() +
           ## geom_sf(data = filter(mwsh, district == str), col = "blue") +
           geom_sf(data = filter(sh32, district == str)) +
           geom_sf(aes(color = URBAN_RURA, fill = NULL),
                   filter(ge04, district == str, LATNUM != 0))
       })
         

ggplot() +
  ## geom_sf(data = filter(mwsh, district == c("Mwanza", "Neno")), col = "blue") +
  geom_sf(data = filter(sh32, district == c("Mwanza", "Neno"))) +
  geom_sf(aes(color = URBAN_RURA, fill = NULL),
          filter(ge04, district == "Mwanza"))

ge04 <- st_join(ge04, sh32, join = st_intersects, suffix = c("", "_sh32"))

## Check that all Mwanza clusters are in either Mwanza or Neno
ge04 %>%
  filter(district == "Mwanza") %>%
  count(district_sh32)
  
ge04 <- ge04 %>%
  mutate(district = if_else(district == "Mwanza",
                            as.character(district_sh32),
                            as.character(district)),
         district32 = if_else(district %in% c("Blantyre", "Lilongwe", "Mzimba", "Zomba") &
                              URBAN_RURA == "U",
                              paste(district, "City"), district),
         district32 = fct_recode(district32, "Mzuzu City" = "Mzimba City"))

ge04 %>% select(-geometry) %>% count(district32) %>% as.data.frame

setdiff(ge04$district32, sh32$district32)


#' 2010

ar10 <- rdhs::get_datasets("MWAR61fl.zip")[[1]] %>% readRDS
ir10 <- rdhs::get_datasets("MWIR61FL.ZIP")[[1]] %>% readRDS
mr10 <- rdhs::get_datasets("MWMR61FL.ZIP")[[1]] %>% readRDS
ge10 <- rdhs::get_datasets("MWGE62FL.ZIP")[[1]] %>% readRDS %>% st_as_sf

ge10 <- ge10 %>%
  mutate(district = fct_recode(DHSREGNA,
                               "nkhotakota" = "nkhota kota",
                               "nkhata bay" = "nkhatabay") %>%
           str_to_title)
setdiff(ge10$district, sh32$district)

par(ask = TRUE)

lapply(c("Blantyre", "Lilongwe", "Mzimba", "Zomba"),
       function(str) {
         ggplot() +
           ## geom_sf(data = filter(mwsh, district == str), col = "blue") +
           geom_sf(data = filter(sh32, district == str)) +
           geom_sf(aes(color = URBAN_RURA, fill = NULL),
                   filter(ge10, district == str, LATNUM != 0))
       })

#' In Mzimba, all urban clusters are in Mzimba Boma, none in Mzuzu City
#' Others look all in cities

ge10 <- ge10 %>%
  mutate(district32 = if_else(district %in% c("Blantyre", "Lilongwe", "Zomba") &
                              URBAN_RURA == "U",
                              paste(district, "City"), district))
setdiff(ge10$district32, sh32$district32)


#' ## 2015

ar15 <- rdhs::get_datasets("MWAR7AFL.ZIP")[[1]] %>% readRDS
ir15 <- rdhs::get_datasets("MWIR7HFL.ZIP")[[1]] %>% readRDS
mr15 <- rdhs::get_datasets("MWMR7HFL.ZIP")[[1]] %>% readRDS
ge15 <- rdhs::get_datasets("MWGE7AFL.ZIP")[[1]] %>% readRDS %>% st_as_sf

ir15 <- ir15 %>%
  mutate(district = sub("(.*) - (.*)", "\\1", as_factor(v022)),
         district = fct_recode(district,
                    "nkhotakota" = "nkhota kota",
                    "nkhata bay" = "nkhatabay") %>%
           str_to_title)

setdiff(ir15$district, sh32$district)

ge15 <- ge15 %>%
  left_join(ir15 %>% select(DHSCLUST = v001, district) %>% unique)


par(ask = TRUE)

lapply(c("Blantyre", "Lilongwe", "Mzimba", "Zomba"),
       function(str) {
         ggplot() +
           ## geom_sf(data = filter(mwsh, district == str), col = "blue") +
           geom_sf(data = filter(sh32, district == str)) +
           geom_sf(aes(color = URBAN_RURA, fill = NULL),
                   filter(ge15, district == str, LATNUM != 0)) +
           ggtitle(str)
       })

#' All urban clusters in cities

ge15 <- ge15 %>%
  mutate(district32 = if_else(district %in% c("Blantyre", "Lilongwe", "Mzimba", "Zomba") &
                              URBAN_RURA == "U",
                              paste(district, "City"), district),
         district32 = fct_recode(district32, "Mzuzu City" = "Mzimba City"))

setdiff(ge15$district32, sh32$district32)

ir_vars <- c("v001", "v002", "v003", "v012", "v022", "v024", "v025")
mr_vars <- c("mv001", "mv002", "mv003", "mv012", "mv022", "mv024", "mv025") %>%
  setNames(., sub("^m", "", .))
ar_vars <- c("v001" = "hivclust", "v002" = "hivnumb", "v003" = "hivline", "hiv03", "hiv05")
ge_vars <- c("v001" = "DHSCLUST", "district", "district32")

dhs04 <- rdhs::rbind_labelled(
  ir04 %>% select(ir_vars) %>% mutate(sex = "female"),
  mr04 %>% select(mr_vars) %>% mutate(sex = "male")
) %>%
  left_join(ar04 %>% select(ar_vars)) %>%
  left_join(ge04 %>% as.data.frame %>% select(ge_vars)) %>%
  mutate(surveyid = "MW2004DHS",
         year = 2004,
         stratum = paste(as_factor(v024), as_factor(v025)))

dhs10 <- rdhs::rbind_labelled(
  ir10 %>% select(ir_vars) %>% mutate(sex = "female"),
  mr10 %>% select(mr_vars) %>% mutate(sex = "male")
) %>%
  left_join(ar10 %>% select(ar_vars)) %>%
  left_join(ge10 %>% as.data.frame %>% select(ge_vars)) %>%
  mutate(surveyid = "MW2010DHS",
         year = 2010,
         stratum = paste(as_factor(v024), as_factor(v025)))

dhs15 <- rdhs::rbind_labelled(
  ir15 %>% select(ir_vars) %>% mutate(sex = "female"),
  mr15 %>% select(mr_vars) %>% mutate(sex = "male")
) %>%
  left_join(ar15 %>% select(ar_vars)) %>%
  left_join(ge15 %>% as.data.frame %>% select(ge_vars)) %>%
  mutate(surveyid = "MW2015DHS",
         year = 2015,
         stratum = as_factor(v022))

dhs <- rdhs::rbind_labelled(dhs04, dhs10, dhs15) %>%
  transmute(surveyid,
            year,
            stratum,
            district,
            district32,
            cluster = v001,
            restype = as_factor(v025),
            sex,
            age = v012,
            hivweight = hiv05 / 1e6,
            prev = if_else(hiv03 %in% 0:3, as.integer(hiv03 %in% 1:3), NA_integer_)) %>%
  filter(!is.na(hivweight))


#' # Pool datasets

dat <- bind_rows(dhs, phia) %>%
  mutate(agegr = cut(age, c(0:16*5, Inf), c(paste0(0:15*5, "-", 0:15*5 + 4), "80+"), TRUE, FALSE),
         agegr3 = cut(age, c(15, 30, 50, Inf), c("15-29", "30-49", "50+"), TRUE, FALSE),
         agegr4 = cut(age, c(15, 25, 35, 50, Inf), c("15-24", "25-34", "35-49", "50+"), TRUE, FALSE)) %>%
  left_join(sh32 %>% as.data.frame %>%
             select(region, zone, district, district_code,
                    district32, district32_code, area_id))

saveRDS(dat, "pooled-survey-data.rds")

## surveyid
des <- svydesign(~cluster, strata=~surveyid+stratum, data=dat, weights=~hivweight, nest = TRUE)

svyby(~prev, ~surveyid, subset(des, age %in% 15:49),
      svyciprop, na.rm=TRUE, vartype = "ci")
svyby(~arv, ~surveyid, subset(des, age %in% 15:49 & !is.na(arv)),
      svyciprop, na.rm=TRUE, vartype = "ci")

svyby(~artall, ~surveyid, subset(des, age %in% 15:64 & !is.na(artall) & prev == 1),
      svyciprop, na.rm=TRUE, vartype = "ci")
svyby(~artall, ~surveyid, subset(des, age %in% 50:64 & !is.na(artall) & prev == 1),
      svyciprop, na.rm=TRUE, vartype = "ci")
svyby(~vls, ~surveyid, subset(des, age %in% 50:64 & !is.na(artall) & prev == 1),
      svyciprop, na.rm=TRUE, vartype = "ci")

#' National

prev_15to49_national <- subset(des, age %in% 15:49) %>%
  {inner_join(
    svyby(~prev, ~surveyid+year, ., unwtd.count) %>% select(-se),
    svyby(~prev, ~surveyid+year, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
  )} %>% 
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agesex_national <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agegr3_national <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agegr4_national <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))


#' By urban/rural residence

prev_15to49_restype <- subset(des, age %in% 15:49) %>%
  {inner_join(
    svyby(~prev, ~surveyid+year+restype, ., unwtd.count) %>% select(-se),
    svyby(~prev, ~surveyid+year+restype, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
  )} %>% 
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agesex_restype <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+restype+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+restype+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agegr3_restype <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+restype+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+restype+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agegr4_restype <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+restype+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+restype+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))


#' By 28 districts
prev_15to49_district <- subset(des, age %in% 15:49) %>%
  {inner_join(
    svyby(~prev, ~surveyid+year+district, ., unwtd.count) %>% select(-se),
    svyby(~prev, ~surveyid+year+district, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
  )} %>% 
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agesex_district <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+district+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+district+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agegr3_district <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+district+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+district+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agegr4_district <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+district+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+district+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

#' By 32 districts
prev_15to49_district32 <- subset(des, age %in% 15:49) %>% 
  {inner_join(
     svyby(~prev, ~surveyid+year+district32+area_id, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+district32+area_id, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agesex_district32 <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+district32+area_id+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+district32+area_id+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agegr3_district32 <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+district32+area_id+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+district32+area_id+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agegr4_district32 <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+district32+area_id+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+district32+area_id+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agesex_region <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+region+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+region+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agegr3_region <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+region+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+region+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))

prev_agegr4_region <- subset(des, age >= 15) %>%
  {inner_join(
     svyby(~prev, ~surveyid+year+region+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~prev, ~surveyid+year+region+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (prev*(1-prev) / n),
         prev = ifelse(prev < 1e-5, 0, prev))


#' ## ARV biomarker presence

#' National
arv_15to64_national <- subset(des, age %in% 15:64 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

arv_agesex_national <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

arv_agegr3_national <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

arv_agegr4_national <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

#' By restype

arv_15to64_restype <- subset(des, age %in% 15:64 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+restype, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+restype, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

arv_agesex_restype <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+restype+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+restype+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

arv_agegr3_restype <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+restype+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+restype+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

arv_agegr4_restype <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+restype+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+restype+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

#' By district

arv_15to64_district <- subset(des, age %in% 15:64 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+district, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+district, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

arv_agesex_district <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+district+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+district+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

arv_agegr3_district <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+district+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+district+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

arv_agegr4_district <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+district+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+district+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

#' By 32 districts

arv_15to64_district32 <- subset(des, age %in% 15:64 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+district32+area_id, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+district32+area_id, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

arv_agesex_district32 <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+district32+area_id+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+district32+area_id+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

arv_agegr3_district32 <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+district32+area_id+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+district32+area_id+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

arv_agegr4_district32 <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+district32+area_id+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+district32+area_id+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

#' ARV by region
arv_agesex_region <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+region+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+region+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

arv_agegr3_region <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+region+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+region+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))

arv_agegr4_region <- subset(des, age >= 15 & prev == 1 & !is.na(arv)) %>%
  {inner_join(
     svyby(~arv, ~surveyid+year+region+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~arv, ~surveyid+year+region+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (arv*(1-arv) / n),
         arv = ifelse(arv < 1e-5, 0, arv))


#' ## ART self report or ARV

#' National

artall_15to64_national <- subset(des, age %in% 15:64 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))

artall_agesex_national <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))


artall_agegr3_national <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))

artall_agegr4_national <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))


#' By urban/rural residence

artall_15to64_restype <- subset(des, age %in% 15:64 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+restype, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+restype, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))

artall_agesex_restype <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+restype+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+restype+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))

artall_agegr3_restype <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+restype+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+restype+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))

artall_agegr4_restype <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+restype+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+restype+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))


#' By district
artall_15to64_district <- subset(des, age %in% 15:64 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+district, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+district, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))

artall_agesex_district <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+district+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+district+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall)) 

artall_agegr3_district <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+district+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+district+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))

artall_agegr4_district <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+district+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+district+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall)) 

#' 32 districts

artall_15to64_district32 <- subset(des, age %in% 15:64 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+district32+area_id, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+district32+area_id, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))

artall_agesex_district32 <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+district32+area_id+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+district32+area_id+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))

artall_agegr3_district32 <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+district32+area_id+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+district32+area_id+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))

artall_agegr4_district32 <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+district32+area_id+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+district32+area_id+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))


#' By region
artall_agesex_region <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+region+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+region+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))

artall_agegr3_region <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+region+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+region+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))

artall_agegr4_region <- subset(des, age >= 15 & prev == 1 & !is.na(artall)) %>%
  {inner_join(
     svyby(~artall, ~surveyid+year+region+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~artall, ~surveyid+year+region+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (artall*(1-artall) / n),
         artall = ifelse(artall < 1e-5, 0, artall))

#' ## Viral load suppression

#' National

vls_15to64_national <- subset(des, age %in% 15:64 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

vls_agesex_national <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

vls_agegr3_national <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

vls_agegr4_national <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

#' By urban/rural residence

vls_15to64_restype <- subset(des, age %in% 15:64 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+restype, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+restype, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

vls_agesex_restype <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+restype+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+restype+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

vls_agegr3_restype <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+restype+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+restype+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

vls_agegr4_restype <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+restype+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+restype+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))


#' By district
vls_15to64_district <- subset(des, age %in% 15:64 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+district, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+district, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

vls_agesex_district <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+district+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+district+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls)) 

vls_agegr3_district <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+district+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+district+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

vls_agegr4_district <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+district+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+district+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls)) 

#' 32 districts

vls_15to64_district32 <- subset(des, age %in% 15:64 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+district32+area_id, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+district32+area_id, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

vls_agesex_district32 <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+district32+area_id+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+district32+area_id+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

vls_agegr3_district32 <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+district32+area_id+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+district32+area_id+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

vls_agegr4_district32 <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+district32+area_id+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+district32+area_id+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

#' By region
vls_agesex_region <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+region+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+region+sex+agegr, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

vls_agegr3_region <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+region+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+region+sex+agegr3, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

vls_agegr4_region <- subset(des, age >= 15 & prev == 1 & !is.na(vls)) %>%
  {inner_join(
     svyby(~vls, ~surveyid+year+region+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~vls, ~surveyid+year+region+sex+agegr4, ., svyciprop, vartype = c("se", "ci"), na.rm=TRUE)
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (vls*(1-vls) / n),
         vls = ifelse(vls < 1e-5, 0, vls))

#' ## Proportion recent


recent_15to49_national <- subset(des, age %in% 15:49 &
                                  prev == 1 &
                                  !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

recent_agesex_national <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+sex+agegr, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

recent_agegr3_national <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+sex+agegr3, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

recent_agegr4_national <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+sex+agegr4, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))


#' By urban/rural

recent_15to49_restype <- subset(des, age %in% 15:49 &
                                  prev == 1 &
                                  !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+restype, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+restype, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

recent_agesex_restype <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+restype+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+restype+sex+agegr, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

recent_agegr3_restype <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+restype+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+restype+sex+agegr3, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

recent_agegr4_restype <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+restype+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+restype+sex+agegr4, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))


#' By district

recent_15to49_district <- subset(des, age %in% 15:49 &
                                  prev == 1 &
                                  !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+district, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+district, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

recent_agesex_district <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+district+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+district+sex+agegr, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

recent_agegr3_district <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+district+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+district+sex+agegr3, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

recent_agegr4_district <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+district+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+district+sex+agegr4, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

#' By 32 districts

recent_15to49_district32 <- subset(des, age %in% 15:49 &
                                  prev == 1 &
                                  !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+district32+area_id, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+district32+area_id, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

recent_agesex_district32 <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+district32+area_id+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+district32+area_id+sex+agegr, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

recent_agegr3_district32 <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+district32+area_id+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+district32+area_id+sex+agegr3, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

recent_agegr4_district32 <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+district32+area_id+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+district32+area_id+sex+agegr4, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

#' By region
recent_agesex_region <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+region+sex+agegr, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+region+sex+agegr, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

recent_agegr3_region <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+region+sex+agegr3, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+region+sex+agegr3, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))

recent_agegr4_region <- subset(des, age >= 15 & prev == 1 & !is.na(recent)) %>%
  {inner_join(
     svyby(~recent, ~surveyid+year+region+sex+agegr4, ., unwtd.count) %>% select(-se),
     svyby(~recent, ~surveyid+year+region+sex+agegr4, ., svyciprop, vartype = c("se", "ci"))
   )} %>%
  "names<-"(sub("se\\..*", "se", names(.))) %>%
  rename(n = counts) %>%
  mutate(deff = se^2 / (recent*(1-recent) / n),
         recent = ifelse(recent < 1e-5, 0, recent))





save(prev_15to49_national, prev_agesex_national, prev_agegr3_national, prev_agegr4_national,
     prev_15to49_restype, prev_agesex_restype, prev_agegr3_restype, prev_agegr4_restype,
     prev_15to49_district, prev_agesex_district, prev_agegr3_district, prev_agegr4_district,
     prev_15to49_district32, prev_agesex_district32, prev_agegr3_district32, prev_agegr4_district32,
     prev_agesex_region, prev_agegr3_region, prev_agegr4_region,
     arv_15to64_national, arv_agesex_national, arv_agegr3_national, arv_agegr4_national,
     arv_15to64_restype, arv_agesex_restype, arv_agegr3_restype, arv_agegr4_restype,
     arv_15to64_district, arv_agesex_district, arv_agegr3_district, arv_agegr4_district,
     arv_15to64_district32, arv_agesex_district32, arv_agegr3_district32, arv_agegr4_district32,
     arv_agesex_region, arv_agegr3_region, arv_agegr4_region,
     artall_15to64_national, artall_agesex_national, artall_agegr3_national, artall_agegr4_national,
     artall_15to64_restype, artall_agesex_restype, artall_agegr3_restype, artall_agegr4_restype,
     artall_15to64_district, artall_agesex_district, artall_agegr3_district, artall_agegr4_district,
     artall_15to64_district32, artall_agesex_district32, artall_agegr3_district32, artall_agegr4_district32,
     artall_agesex_region, artall_agegr3_region, artall_agegr4_region,
     vls_15to64_national, vls_agesex_national, vls_agegr3_national, vls_agegr4_national,
     vls_15to64_restype, vls_agesex_restype, vls_agegr3_restype, vls_agegr4_restype,
     vls_15to64_district, vls_agesex_district, vls_agegr3_district, vls_agegr4_district,
     vls_15to64_district32, vls_agesex_district32, vls_agegr3_district32, vls_agegr4_district32,
     vls_agesex_region, vls_agegr3_region, vls_agegr4_region,
     recent_15to49_national, recent_agesex_national, recent_agegr3_national, recent_agegr4_national,
     recent_15to49_restype, recent_agesex_restype, recent_agegr3_restype, recent_agegr4_restype,
     recent_15to49_district, recent_agesex_district, recent_agegr3_district, recent_agegr4_district,
     recent_15to49_district32, recent_agesex_district32, recent_agegr3_district32, recent_agegr4_district32,
     recent_agesex_region, recent_agegr3_region, recent_agegr4_region,
     file = here::here("data", "survey.rda"))
