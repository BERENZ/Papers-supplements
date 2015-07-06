### extra - seasonality

decomposed <- to_plot %>%
  group_by(stat) %>%
  do(ts = ts(.$value_normalized,start=c(1,2,2005),frequency=12)) %>%
  do(stl = stl(.$ts,'periodic'))

decomposed$stl[[3]]$time.series[,2] %>%
  stl(.,'periodic') %>%
  plot()
