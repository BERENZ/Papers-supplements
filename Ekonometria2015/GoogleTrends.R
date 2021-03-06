# load packages ------------------------------------------------------------

library(ggplot2) ## vis
library(googletrend) ## get data directly from google docs
library(dplyr)
library(tidyr)
library(stringi)
library(readxl)

### read short-term statistics

stats <- read_excel(
  path = 'Ekonometria2015/Wybrane_miesieczne_wskazniki_makroekonomiczne__cz_i.xls',
  sheet = 3,
  skip = 3,
  col_names = F
) %>%
  .[c(4,9),-c(1,2)] %>%
  t() %>%
  apply(.,2,as.numeric) %>%
  as.data.frame() %>%
  mutate(date = stri_datetime_create(rep(2000:2015,each = 12),
                                     rep(1:12,times = 16),1)) %>%
  tbl_df() %>%
  na.omit() %>%
  rename(przecietne_zatrudnienie = V1,
         bezrobotni_indeks = V2)  %>%
  gather(stat,value,-date)

### Google data

google_trends <-  googletrend::gettrend(keyword = 'praca',
                                        geo = 'PL',
                                        category = '0-958-60',
                                        use.monthly = F,
                                        plot = F)

google_trends <- google_trends %>%
  na.omit() %>%
  mutate(stat = 'google',
         week = stri_datetime_parse(week,'%Y-%M-%d')) %>%
  rename(value = index,
         date = week)


# prepare data to code ----------------------------------------------------

to_plot <- bind_rows(stats,google_trends) %>%
  group_by(stat) %>%
  mutate(value_normalized = (value - min(value)) / (max(value) - min(value))) %>%
  ungroup() %>%
  mutate(stat = factor(
    x = stat,
    levels = c('bezrobotni_indeks','przecietne_zatrudnienie','google'),
    labels = c(
      'Bezrobotni zarejestrowani (stan w końcu okresu, okres poprzedni = 100)',
      'Przeciętne zatrudnienie w sektorze przedsiębiorstw (okres poprzedni = 100)',
      'Google Trends - hasło praca (dane tygodniowe, stan na koniec tygodnia)'
    )
  ))

### reference line with 31th December
vline_data <- data_frame(vline = stri_datetime_create(seq(2003,2015,1),12,31))


# final plot --------------------------------------------------------------

ggplot(data = to_plot,
       aes(x = date,
           y = value_normalized,
           group = 1)) +
  geom_line() +
  facet_wrap( ~ stat,ncol = 1) +
  theme_bw() +
  ylab('Wartość indeksu (dane znormalizowane)') +
  xlab('Okres') +
  geom_vline(data = vline_data,
             aes(xintercept = as.numeric(vline)),
             linetype = 3) +
  theme(axis.title = element_text(size = 15),
        strip.text = element_text(size = 15))

