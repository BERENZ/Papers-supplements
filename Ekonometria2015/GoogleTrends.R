### new plot
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(readxl)
library(stringi)

### read short-term statistics

stats <- read_excel(
  path = 'Ekonometria2015/Wybrane_miesieczne_wskazniki_makroekonomiczne__cz_i.xls',
  sheet = 3,
  skip = 3,
  col_names = F
)


selected_stats <- stats[c(4,9),-c(1,2)] %>%
  t() %>%
  apply(.,2,as.numeric) %>%
  as.data.frame() %>%
  mutate(date = stri_datetime_create(rep(2000:2015,each = 12),
                                     rep(1:12,times = 16),1)) %>%
  tbl_df() %>%
  na.omit()

names(selected_stats)[1:2] <- c('przecietne_zatrudnienie','bezrobotni_indeks')

selected_stats <- selected_stats %>%
  gather(stat,value,-date)

### Google data

google_trends <- read.csv(file = 'Ekonometria2015/google_trends.csv',
                          header= T,
                          skip = 4,
                          nrows = 599)

google_trends <- google_trends %>%
  mutate(date = stri_extract_last_regex(`Tydzień`,'\\d{4}-\\d{2}-\\d{2}'),
         date = stri_datetime_parse(date,format='%Y-%M-%d')) %>%
  select(-`Tydzień`) %>%
  mutate(stat = 'google') %>%
  rename(value = praca)

### files to plot

to_plot <- bind_rows(selected_stats,google_trends) %>%
  group_by(stat) %>%
  mutate(value_normalized = (value - min(value))/(max(value)-min(value))) %>%
  ungroup() %>%
  mutate(stat = factor(x = stat,
                       levels = c('bezrobotni_indeks','przecietne_zatrudnienie','google'),
                       labels = c('Bezrobotni zarejestrowani (stan w końcu okresu)',
                                  'Przeciętne zatrudnienie w sektorze przedsiębiorstw',
                                  'Google Trends - hasło praca')))

vline_data <- data_frame(vline = stri_datetime_create(seq(2003,2015,1),12,31))

ggplot(data = to_plot,
       aes(x = date,
           y = value_normalized,
           group = 1)) +
  geom_line() +
  facet_wrap(~stat,ncol=1) +
  theme_bw() +
  ylab('Wartość indeksu (dane znormalizowane)') +
  xlab('Okres') +
  geom_vline(data = vline_data,
             aes(xintercept = as.numeric(vline)),
             linetype = 3)

