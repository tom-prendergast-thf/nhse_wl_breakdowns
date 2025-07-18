library(dplyr)
library(tidyr)


geog <- read.csv('data/demographics_by_geography.csv')

spec <- read.csv('data/demographics_by_specialty.csv')

timeseries <- read.csv('data/demographics_timeseries.csv')


pivoted_perc_function <- function(df){
  df$Count[df$Count == '*'] <- 0
  
  df$Count <- as.numeric(df$Count)
  
  df_pivoted <- df %>%
    pivot_wider(names_from = Waiting.Bands, values_from = Count) %>%
    mutate(perc_18_weeks = `01 To 18 Weeks`/Total) %>%
    mutate(perc_52_weeks = `Over 52 Weeks`/Total)
}

piv_spec <- pivoted_perc_function(spec)

piv_geog <- pivoted_perc_function(geog)

levels(as.factor(piv_geog$Code))

piv_geog_ICB <- piv_geog %>% filter(grepl('ICB', Name))

piv_spec_IMD_18 <- piv_spec %>% filter(Metric == 'IMD') %>%
  select(Treatment.Function.Code, Category, perc_18_weeks) %>%
  pivot_wider(names_from = Category, values_from = perc_18_weeks) %>%
  mutate(diff = `10` - `1`)

piv_spec_IMD_52 <- piv_spec %>% filter(Metric == 'IMD') %>%
  select(Treatment.Function.Code, Category, perc_52_weeks) %>%
  pivot_wider(names_from = Category, values_from = perc_52_weeks) %>%
  mutate(diff = `10` - `1`)
  
  
  
  
  
  
  
  