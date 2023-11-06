library(dplyr)
library(lubridate)

load("./data-raw/weather-data/weather-June-2019-tb.rda")

weather_June_2019.tb  |>
  filter(month(time) == 6L & day(with_tz(time, tzone = "EET")) == 18) |>
  select(1:18) |>
  as.data.frame() -> weather_18_june_2019.df

summary(weather_18_june_2019.df)
save(weather_18_june_2019.df, file = "./data/weather-18-june-2019-df.rda", compress = "xz", compression_level = 3)
