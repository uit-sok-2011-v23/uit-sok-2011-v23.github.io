library(WDI)
library(tidyverse)

df_gdp_wdi <- WDI(
  country = "all",
  indicator = c('gdppc' = "NY.GDP.PCAP.PP.KD"),
  start = 2000,
  end = 2019,
  extra = TRUE
)

df_gdp <- df_gdp_wdi %>%
  mutate(year = as.numeric(year)) %>%
  filter(iso3c != "", income != "Aggregates") %>%
  drop_na(gdppc) %>%
  group_by(country, year) %>%
  slice(which.max(gdppc)) %>%
  ungroup()

df_gdp <- df_gdp %>%
  group_by(country) %>%
  filter(year == min(year)) %>%
  select(country, gdppc0 = gdppc) %>%
  left_join(df_gdp, by = c("country")) %>%
  ungroup()

df_gdp <- df_gdp %>%
  group_by(country) %>%
  mutate(
    gdpgrowth = (log(gdppc) - lag(log(gdppc))) * 100,
    avg_gdpgrowth = mean(gdpgrowth, na.rm = TRUE)
  ) %>%
  arrange(desc(year)) %>%
  slice(1) %>%
  select(-status,
         -lastupdated,
         -capital,
         -longitude,
         -latitude,
         -lending,
         -gdpgrowth) %>%
  ungroup()

df_edu_wdi <- WDI(
  country = "all",
  indicator = c('educ' = "BAR.SCHL.15UP"),
  start = 2000,
  end = 2019,
  extra = TRUE
)

df_edu <- df_edu_wdi %>%
  drop_na(educ) %>%
  mutate(educ = as.numeric(educ)) %>%
  select(country, iso2c, iso3c, year, educ, region, income) %>%
  group_by(iso3c) %>%
  mutate(avg_educ = mean(educ)) %>%
  ungroup() %>%
  select(-year,-educ) %>%
  distinct(country, .keep_all = TRUE)

df_nsy_wdi <- WDI(
  country = "all",
  indicator = c('nsy' = "NY.ADJ.NNAT.GN.ZS"),
  start = 2000,
  end = 2015,
  extra = TRUE
)

df_nsy <- df_nsy_wdi %>%
  select(country, region, income, iso2c, iso3c, year, nsy) %>%
  arrange(iso3c, year) %>%
  drop_na(nsy, iso3c, region) %>%
  filter(region != "Aggregates") %>%
  mutate(nsy = as.numeric(nsy, na.rm = TRUE)) %>%
  group_by(country) %>%
  mutate(avg_nsy = mean(nsy, na.rm = TRUE)) %>%
  select(-year,-nsy) %>%
  distinct(country, .keep_all = TRUE) %>%
  ungroup()

df_lf_wdi <- WDI(
  country = "all",
  indicator = c('lf' = "JI.TLF.TOTL"),
  start = 2000,
  end = 2019,
  extra = TRUE
)

df_lf <- df_lf_wdi %>%
  select(country, region, income, iso3c = iso2c, year, lf) %>%
  drop_na(lf) %>%
  filter(lf != 0) %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  mutate(deltaYear = year - lag(year),
         growth = log(lf) - log(lag(lf))) %>%
  drop_na(deltaYear, growth) %>%
  mutate(n = growth / deltaYear, avg_n = mean(n, na.rm = TRUE)) %>%
  filter(year == max(year)) %>%
  ungroup()

df_rest_wdi <- WDI(
  country = "all",
  indicator = c(
    'poptot' = "SP.POP.TOTL",
    'gi' = "NE.GDI.FTOT.KD.ZG",
    'gx' = "NE.EXP.GNFS.KD.ZG",
    'nry' = "NY.ADJ.DRES.GN.ZS",
    'p' = "SP.POP.GROW"
  ),
  start = 2000,
  end = 2019,
  extra = TRUE
)

df_rest <- df_rest_wdi %>%
  drop_na(iso3c, gi, gx, nry) %>%
  filter(region != "Aggregates") %>%
  select(-longitude,
         -latitude,
         -capital,
         -lending,
         -status,
         -lastupdated) %>%
  group_by(iso3c) %>%
  mutate(
    avg_p = mean(p, na.rm = TRUE),
    avg_gi = mean(gi, na.rm = TRUE),
    avg_gx = mean(gx, na.rm = TRUE),
    avg_nry = mean(nry, na.rm = TRUE)
  ) %>%
  filter(year == max(year)) %>%
  ungroup(iso3c)

df_growth <-
  df_gdp %>%
  inner_join(select(df_edu, iso3c, avg_educ), by = "iso3c") %>%
  inner_join(select(df_nsy, iso3c, avg_nsy), by = "iso3c") %>%
  inner_join(select(df_lf, iso3c, avg_n), by = "iso3c") %>%
  inner_join(select(df_rest, iso3c, poptot, avg_p, avg_gi, avg_gx, avg_nry),
             by = "iso3c") %>%
  mutate(ln_gdppc0 = log(gdppc0),
         ln_gdppc = log(gdppc),
         avg_n = avg_n * 100) %>%
  select(-year)
