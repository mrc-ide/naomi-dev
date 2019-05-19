library(tidyverse)
library(sf)
library(here)

load(here::here("data/shapefile.rda"))

mfl <- here("data-raw", "programme", "Malawi_ART_sites_rururb.xlsx") %>%
  readxl::read_excel(sheet = "ART_site_edited") %>%
  rename(site_id = Site_ID,
         site_mfl = hfacility_name,
         district = District,
         site_loc = `Location rural/urban`) %>%
  mutate(site_mfl = tolower(site_mfl),
         district = sub("Mzimba.*", "Mzimba", district),
         district32 = if_else(district %in% c("Blantyre", "Lilongwe", "Mzimba", "Zomba") &
                              site_loc == "Urban",
                              paste(district, "City"), as.character(district)),
         district32 = fct_recode(district32, "Mzuzu City" = "Mzimba City"))


site_codes <- here("data-raw", "programme", "Malawi_ART_sites_2017Q4_2018Q3.xlsx") %>%
    readxl::read_excel() %>%
    "names<-"(tolower(names(.))) %>%
    select(district, site_id, site_name) %>%
    unique %>%
    mutate(district = sub("Mzimba.*", "Mzimba", district))

mfl <- full_join(mfl, site_codes) %>%
  mutate(site = if_else(is.na(site_name), site_mfl, tolower(site_name)),
         district32 = if_else(site_id %in% c(845, 3057), factor("Blantyre City", levels(district32)), district32))

mfl_missing <- data.frame(site = c("pace clinic", 
                                   "press cooperation clinic",
                                   "achikondi women community friendly  services clinic",
                                   "auction holdings clinic kanengo", 
                                   "neno",
                                   "zalewa dapp (dapp hope)",
                                   "st anne's hospital",
                                   "trinity mission hospitalka",
                                   "matanda dispensary", 
                                   "sharpe valley maternity",
                                   "mkhwayi health centre",
                                   "muluzi barracks changalume clinic",
                                   "kalulu health centre", 
                                   "newa / mpasazi health centre",
                                   "daeyang luke hospital private", 
                                   "chimwamkango health centre",
                                   "nsabwe health centre",
                                   "chisi health centre", 
                                   "state house dispensary"),
                          district = c("Blantyre", "Blantyre",
                                       "Lilongwe", "Lilongwe",
                                       "Neno", "Neno", "Nkhotakota", "Nsanje", 
                                       "Ntcheu", "Ntcheu", "Phalombe", "Zomba",
                                       "Dedza", 
                                       "Kasungu", "Lilongwe", "Mchinji", "Thyolo",
                                       "Zomba", "Zomba"),
                          district32 = c("Blantyre City", "Blantyre City",
                                       "Lilongwe City", "Lilongwe City",
                                       "Neno", "Neno", "Nkhotakota", "Nsanje", 
                                       "Ntcheu", "Ntcheu", "Phalombe", "Zomba",
                                       "Dedza", 
                                       "Kasungu", "Lilongwe", "Mchinji", "Thyolo",
                                       "Zomba City", "Zomba City"))

mfl <- bind_rows(mfl, mfl_missing)

setdiff(tolower(artdat$site), mfl$site)
setdiff(tolower(ancrt$site), mfl$site)
filter(mfl, duplicated(site))

#' # Read programme data

setdiff(artlong$site_id, mfl$site_id)

artdat <- here("data-raw", "programme", "Malawi_ART_sites_alive_2004-2019.xlsx") %>%
  readxl::read_excel() %>%
  rename(site_id = site_ID,
         quarter = Quarter,
         district = District,
         onart = AliveOnART) %>%
  mutate(period = quarter,
         year = type.convert(substr(quarter, 1, 4)),
         quarter = type.convert(substr(quarter, 7, 7)))


artdat <- artdat %>% left_join(mfl %>% rename(district28 = district))

filter(artdat, district == "Chiradzulu") %>%
  mutate(site = fct_reorder2(site_mfl, period, onart)) %>%
  ggplot(aes(year + (quarter-1)/4, onart)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, scales = "free_y") +
  expand_limits(y=c(0, 500))


artdat <- artdat %>%
  mutate(district28 = if_else(!is.na(district28),
                              factor(district28),
                              fct_recode(district, "Mzimba" = "Mzimba North")),
         district32 = if_else(!is.na(district32),
                              factor(district32),
                              fct_recode(district,
                                         "Lilongwe City" = "Lilongwe",
                                         "Blantyre City" = "Blantyre",
                                         "Zomba City" = "Zomba",
                                         "Mzuzu City" = "Mzimba North")))

#' Get proportion ART clients >15 years from Spectrum 2018 outputs

sr <- eppasm::read_hivproj_output(here("data-raw/spectrum/", "Malawi_2019_v18_epp.PJNZ"))

srl <- rbind(
  as.data.frame.table(sr$hivpop) %>%
  mutate(var = "hivpop"),
  as.data.frame.table(sr$totpop) %>%
  mutate(var = "totpop"),
  as.data.frame.table(sr$artpop) %>%
  mutate(var = "artpop")
) %>%
  mutate(age = type.convert(age),
         year = type.convert(year),
         agegr = cut(age, c(0:13*5, Inf), c(paste0(0:12*5, "-", 0:12*5+4), "65+"), TRUE, FALSE)) %>%
  rename(value = Freq)

prop15pl <- srl %>%
  filter(var == "artpop") %>%
  group_by(year) %>%
  summarise(prop15pl = weighted.mean(age >= 15, value)) %>%
  mutate(period = year + 0.5)

artdat <- artdat %>%
  mutate(period = year + (quarter - 1) / 4,
         quarteri = quarter,
         quarter = paste0(year, " Q", quarteri),
         prop15pl = approx(prop15pl$period, prop15pl$prop15pl, period, rule = 2)$y) %>%
  mutate(art15pl = onart * prop15pl,
         distrct = district28) %>%
  group_by(district, district32, year, quarter, quarteri) %>%
  summarise(art_tot= sum(onart),
            art15pl = sum(art15pl))


#' ANC routine testing data

ancrt18 <- readxl::read_excel("Malawi_ANC_PMTCT_2017_1_2018_3.xlsx") %>%
  rename(district = District,
         site = Site) %>%
  mutate(indicator = fct_recode(`Data element`,
                                "no_status" = "HIV status not ascertained",
                                prev_neg = "Previous negative",
                                new_neg = "New negative",
                                new_pos = "New positive",
                                prev_pos = "Previous positive",
                                already_ART = "Already on ART when starting ANC",
                                NULL = "Started ART at 28+ weeks of preg.", 
                                NULL = "Started ART at 0-27 weeks of pregnancy",
                                NULL = "No ARVs"),
         district = sub("Mzimba.*", "Mzimba", district),
         site = tolower(site)) %>%
  left_join(mfl %>% select(site, district, district32)) %>%
  group_by(year, quarter, indicator, district, district32) %>%
  summarise(value = sum(data_value)) %>%
  spread(indicator, value) %>%
  mutate(anc_clients = no_status + prev_neg + new_neg + prev_pos + new_pos,
         ancrt_n = prev_neg + new_neg + prev_pos + new_pos,
         ancrt_pos = prev_pos + new_pos,
         ancrt_art = already_ART) %>%
  select(district, district32, year, quarter, anc_clients, ancrt_n, ancrt_pos, ancrt_art)

ancrt <- here::here("data-raw", "programme", "mw_ancrt_2018-10-26.rds") %>%
  readRDS %>%
  "names<-"(tolower(names(.))) %>%
  mutate(site = tolower(site)) %>%
  left_join(mfl %>% select(site = site_mfl, district, district32)) %>%
  group_by(district, district32, year, quarter) %>%
  summarise(anc_clients = sum(anc_clients, na.rm=TRUE),
            ancrt_n = sum(prev_neg+new_neg+prev_pos+new_pos, na.rm=TRUE),
            ancrt_pos = sum(prev_pos+new_pos, na.rm=TRUE),
            ancrt_art = sum(already_art, na.rm=TRUE))


ancrt <- rbind(filter(ancrt, year < 2018),
               filter(ancrt18, year >= 2018)) %>%
  mutate(ancrt_pos_noart = ancrt_pos - ancrt_art) %>%
  as.data.frame


#' # Data cleaning

#' ## Summary plots

#' Plot number on ART by district

pdf("art15pl-inputs.pdf", h=8, w=10)
artdat %>%
  mutate(quarter = sub(".*Q(.)", "\\1", quarter) %>% type.convert,
         period = year + quarter / 4) %>%
  split(., .$region) %>%
  lapply(function(df) {
    df %>% 
      ggplot(aes(period, art15pl)) +
      geom_point() +
      geom_line() +
      geom_vline(xintercept = c(2016.25, 2018.75), linetype = "dashed") + 
      ## geom_line(alpha=0.25) +
      facet_wrap(~district32, scales = "free_y") +
      ## expand_limits(y=0) +
      theme(axis.text.x=element_text(angle = 90, vjust = 0.5, size = 8))
  })
dev.off()

#' Identify outliers for the two relevant time points 2016.25; 2018.75.
#' * Dowa: 2016.25
#' * Nkhata Bay: 2016.25
#' * Likoma: 2016.25 (no data)
#' * Mwanza: 2016.25
#' * Phalombe: 2016.25

artdat <- artdat %>%
  complete(nesting(district, district32),
           nesting(year, quarter)) %>%
  mutate(quarteri = sub(".*Q(.)", "\\1", quarter) %>% type.convert,
         period = year + quarteri / 4,
         art15pl_cleaned = ifelse(district %in% c("Dowa",
                                                  "Nkhata Bay",
                                                  "Likoma",
                                                  "Mwanza",
                                                  "Phalombe") &
                                  quarter == "2016 Q1",
                                  NA, art15pl)
         ) %>%
  group_by(district, district32) %>%
  mutate(art15pl_cleaned = approx(period,
                                  art15pl_cleaned,
                                  period)$y)

pdf("art15pl-inputs_cleaned.pdf", h=8, w=10)
artdat %>%
  mutate(region = fct_relevel(region, "Northern", "Central", "Southern")) %>%
  select(region, zone, district, district_idx, period,
         raw = art15pl, cleaned = art15pl_cleaned) %>%
  gather(source, art15pl, raw, cleaned) %>%
  mutate(source = fct_relevel(source, "cleaned")) %>%
  arrange(source) %>%
  split(., .$region) %>%
  lapply(function(df) {
    df %>% 
      ggplot(aes(period, art15pl, color = source)) +
      geom_vline(xintercept = c(2016.25, 2018.75), linetype = "dashed") + 
      geom_point() +
      geom_line() +
      ## geom_line(alpha=0.25) +
      scale_color_manual(values = c(raw = "black", cleaned = "red")) +
      facet_wrap(~district, scales = "free_y") +
      ## expand_limits(y=0) +
      theme(axis.text.x=element_text(angle = 90, vjust = 0.5, size = 8))
  })
dev.off()


#' ## ANC-RT data


#' Plot number of ANC clients by district

ancrt <- ancrt %>%
  mutate(period = year + (quarter-1)/4,
         ## region = fct_relevel(region, "Northern", "Central", "Southern"),
         ## district = fct_reorder(district, district_idx),
         data_used = factor(year %in% c(2016, 2018),
                            c(TRUE, FALSE), c("used", "not used")))

ancrt_long <- ancrt %>%
  gather(indicator, value, anc_clients:ancrt_pos_noart) %>%
  filter(indicator != "ancrt_pos_noart") %>%
  mutate(indicator = fct_reorder2(indicator, period, value, sum))

## Plot number of clients
filter(ancrt_long, indicator == "anc_clients") %>%
  split(., .$region) %>%
  lapply(function(df){
    df %>%
      ggplot(aes(period, value, color = data_used)) +
      geom_line(color = "black", alpha=0.25) +
      geom_point() +
      ## geom_vline(xintercept = c(2016.0, 2018.5), linetype = "dashed") + 
      facet_wrap(~district, scales = "free_y") +
      expand_limits(y=0) +
      scale_color_manual(values = c("not used" = "grey", "used" = "black")) +
      ylab("ANC clients") +
      theme(axis.text.x=element_text(angle = 90, vjust = 0.5, size = 8)) +
      ggtitle("Total number of ANC clients (quarterly)")
  })



filter(ancrt_long, indicator %in% c("anc_clients", "ancrt_n")) %>%
  split(., .$region) %>%
  lapply(function(df){
    df %>%
      ggplot(aes(period, value, color = data_used, group = indicator, shape = indicator)) +
      geom_vline(xintercept = c(2016.0, 2018.5), linetype = "dashed") + 
      geom_line(aes(color = NULL), alpha=0.25) +
      geom_point() +
      scale_color_manual(values = c("not used" = "grey", "used" = "black")) +
      facet_wrap(~district, scales = "free_y") +
      expand_limits(y=0) +
      theme(axis.text.x=element_text(angle = 90, vjust = 0.5, size = 8)) +
      ggtitle("Number of ANC clients with HIV status (quarterly)")
  })



filter(ancrt_long, indicator %in% c("ancrt_pos", "ancrt_art")) %>%
  split(., .$region) %>%
  lapply(function(df){
    df %>%
      ggplot(aes(period, value, color = data_used,
                 shape = indicator, group = indicator)) +
      geom_vline(xintercept = c(2016.0, 2018.5), linetype = "dashed") + 
      geom_line(alpha=0.25) +
      geom_point() +
      scale_color_manual(values = c("not used" = "grey", "used" = "black")) +
      facet_wrap(~district, scales = "free_y") +
      expand_limits(y=0) +
      theme(axis.text.x=element_text(angle = 90, vjust = 0.5, size = 8)) +
      ggtitle("Number HIV positive and already ART among ANC clients (quarterly)")
  })

ancrt %>%
  mutate(prev = ancrt_pos / ancrt_n,
         se = sqrt(prev * (1-prev) / ancrt_n),
         ci_l = prev - qnorm(0.975) * se,
         ci_u = prev + qnorm(0.975) * se,
         period = year + (quarter-1)/4,
         district = fct_reorder(district, district_idx)) %>%
  split(., .$region) %>%
  lapply(function(df){
    df %>% 
      ggplot(aes(period, prev, ymin = ci_l, ymax = ci_u,
                 color = data_used)) +
      ## geom_vline(xintercept = c(2016.0, 2018.5), linetype = "dashed") + 
      geom_line(color = NA, alpha=0.2) +
      geom_ribbon(alpha = 0.1, color = NA) +
      geom_point() +
      scale_color_manual(values = c("not used" = "grey", "used" = "black")) +
      facet_wrap(~district, scales = "free_y") +
      expand_limits(y=0) +
      theme(axis.text.x=element_text(angle = 90, vjust = 0.5, size = 8)) +
      ggtitle("HIV prevalence among ANC attendees")
  })

ancrt %>%
  mutate(art_coverage = ancrt_art / ancrt_pos,
         lse = sqrt(1 / (art_coverage * (1-art_coverage) * ancrt_art)),
         ci_l = plogis(qlogis(art_coverage) - qnorm(0.975) * lse),
         ci_u = plogis(qlogis(art_coverage) + qnorm(0.975) * lse),
         period = year + (quarter-1)/4,
         district = fct_reorder(district, district_idx)) %>%
  split(., .$region) %>%
  lapply(function(df){
    df %>%
    ggplot(aes(period, art_coverage, ymin = ci_l, ymax = ci_u,
               color = data_used)) +
      ## geom_vline(xintercept = c(2016.0, 2018.5), linetype = "dashed") +
      geom_line(color = 1, alpha=0.2) +
      geom_ribbon(alpha = 0.1, color = NA) +
      geom_point() +
      scale_color_manual(values = c("not used" = "grey", "used" = "black")) +
      facet_wrap(~district, scales = "free_y") +
      expand_limits(y=0) +
      theme(axis.text.x=element_text(angle = 90, vjust = 0.5, size = 8)) +
      ggtitle("Proportion already on ART before first ANC visit")
  })

         
#' # ANC-RT outlier data-points
#'
#' * Ntchisi: Q1 2016 (Y)
#' * Dowa Q1 2018 (Y)


ancrt <- ancrt %>%
  mutate(data_used = fct_expand(data_used, "excluded"),
         data_used = if_else(district == "Ntchisi" & period == 2016.0 |
                             district == "Dowa" & period == 2018.0,
                             factor("excluded", levels(data_used)), data_used))

ancrt <- ancrt %>%
  ungroup %>%
  right_join(sh32 %>% as.data.frame %>% select(district, district32, area_id), .)
artdat <- artdat %>%
  ungroup %>%
  right_join(sh32 %>% as.data.frame %>% select(district, district32, area_id), .)


save(artdat, ancrt, file = here::here("data", "programme.rda"))


distlist <- c("Blantyre", "Zomba", "Thyolo", "Mulanje", "Chiradzulu", "Phalombe", "Mangochi", "Machinga")
filter(sh32, region == "Southern") %>%
  ggplot() + geom_sf() + geom_sf_text(aes(label = district))

filter(sh32, district %in% distlist) %>%
  ggplot() + geom_sf() + geom_sf_text(aes(label = district))

#' Plot ART coverage before current pregnancy
ancrt %>%
  mutate(alreadyART = ancrt_art / ancrt_pos) %>%
  left_join(sh32) %>% 
  filter(region == "Southern",
         year %in% 2016:2018,
         district %in% distlist) %>%
  mutate(district32 = fct_reorder2(district32, year, alreadyART)) %>%
  ggplot(aes(year + (quarter-1)/4, alreadyART, color = district32)) +
  geom_point(alpha=0.25) +
  geom_line(alpha=0.25) +
  geom_smooth(alpha=0.1)
