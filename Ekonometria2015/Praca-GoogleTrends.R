### Porównanie stopy bezrobocia rejestrowanego, ofert pracy i google trends
library(ggplot2)
library(googletrend)
library(dplyr)
library(tidyr)
library(stringi)

# google trend ------------------------------------------------------------

praca_google <- googletrend::gettrend(keyword = 'praca',
                                      geo = 'PL',
                                      category = '0-958-60',
                                      use.monthly = T,
                                      plot = F)
praca_google <- praca_google %>%
  mutate(year = format(week,'%Y'),
         year = as.numeric(year)) %>%
  filter(year %in% 2011:2015) %>%
  mutate(index = (index-min(index))/(max(index)-min(index)))


# oferty pracy ------------------------------------------------------------

oferty <- read.csv2(file='Ekonometria2015/oferty_pracy.csv',
                    header=T,
                    skip=6,
                    stringsAsFactors = F)

oferty <- oferty %>%
  mutate(value=as.numeric(`Wartość`),
         month = 12,
         month=ifelse(`Miesiące`=='styczeń',01,month),
         month=ifelse(`Miesiące`=='luty',02,month),
         month=ifelse(`Miesiące`=='marzec',03,month),
         month=ifelse(`Miesiące`=='kwiecień',04,month),
         month=ifelse(`Miesiące`=='maj',05,month),
         month=ifelse(`Miesiące`=='czerwiec',06,month),
         month=ifelse(`Miesiące`=='lipiec',07,month),
         month=ifelse(`Miesiące`=='sierpień',08,month),
         month=ifelse(`Miesiące`=='wrzesień',09,month),
         month=ifelse(`Miesiące`=='październik',10,month),
         month=ifelse(`Miesiące`=='listopad',11,month),
         date = paste('1',month,Lata,sep='-'),
         date = stri_datetime_parse(date,'d-M-yyyy')) %>%
  select(date,value) %>%
  arrange(date) %>%
  na.omit()

# bezrobocie rejestrowane -------------------------------------------------

bezrobocie <- read.csv2(file='Ekonometria2015/stopa_bezrobocia.csv',
                    header=T,
                    skip=6,
                    stringsAsFactors = F)

bezrobocie <- bezrobocie %>%
  mutate(value = stri_replace_all_fixed(`Wartość`,',','.'),
         value=as.numeric(value),
         month = 12,
         month=ifelse(`Miesiące`=='styczeń',01,month),
         month=ifelse(`Miesiące`=='luty',02,month),
         month=ifelse(`Miesiące`=='marzec',03,month),
         month=ifelse(`Miesiące`=='kwiecień',04,month),
         month=ifelse(`Miesiące`=='maj',05,month),
         month=ifelse(`Miesiące`=='czerwiec',06,month),
         month=ifelse(`Miesiące`=='lipiec',07,month),
         month=ifelse(`Miesiące`=='sierpień',08,month),
         month=ifelse(`Miesiące`=='wrzesień',09,month),
         month=ifelse(`Miesiące`=='październik',10,month),
         month=ifelse(`Miesiące`=='listopad',11,month),
         date = paste('1',month,Lata,sep='-'),
         date = stri_datetime_parse(date,'d-M-yyyy')) %>%
  select(date,value) %>%
  arrange(date) %>%
  na.omit()


# merge -------------------------------------------------------------------

rynek_pracy <- bezrobocie %>%
  mutate(date = format(date,'%Y-%m-%d')) %>%
  rename(bezrobocie = value) %>%
  left_join(
    oferty %>%
      mutate(date = format(date,'%Y-%m-%d')) %>%
      rename(oferty = value)
  ) %>%
  left_join(
    praca_google %>%
      select(-year) %>%
      mutate(week = format(week,'%Y-%m-%d')) %>%
      rename(google = index) ,
    by = c('date'='week')
  ) %>%
  mutate_each(funs((.-min(.))/(max(.)-min(.))),bezrobocie,oferty)

### plot
rynek_pracy %>%
  mutate(dates = as.Date(date)) %>%
  gather(stat,value,bezrobocie:google) %>%
  ggplot(data = .,
         aes(x = dates,
             y = value,
             group = stat)) +
  geom_line(alpha=0.7) +
  geom_smooth(method='loess',se=F,col='black') +
  theme_bw() +
  facet_wrap(~stat) +
  xlab('Okres') +
  ylab('Wartości (dane znormalizowane)') +
  scale_x_date()


# decompose ---------------------------------------------------------------
decomposed <- lapply(rynek_pracy[,-1], function(x) {
  x <- ts(x,frequency = 12,start=c(2011,1,1))
  stl(x,'periodic')
})


### seasonal

to_plot <- sapply(decomposed, function(x) x$time.series[,1]) %>%
  data.frame() %>%
  mutate(date = as.Date(rynek_pracy$date),
         facet = 'sezonowosc')


do_wykresu <- rynek_pracy %>%
  mutate(facet = 'oryginalne',
         date = as.Date(date)) %>%
  bind_rows(to_plot)


## compare

do_wykresu %>%
  gather(stat,value,bezrobocie:google) %>%
  ggplot(data = .,
         aes(x = date,
             y = value,
             group = stat)) +
  geom_line(alpha=0.7) +
  geom_smooth(method='loess',se=F,col='black') +
  theme_bw() +
  facet_grid(stat~facet) +
  xlab('Okres') +
  ylab('Wartości (dane znormalizowane)') +
  scale_x_date()




