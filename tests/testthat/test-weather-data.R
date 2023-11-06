context("weather_data")

test_that("test size and sums", {
expect_identical(nrow(weather_18_june_2019.df), 1440L)
expect_identical(ncol(weather_18_june_2019.df), 18L)
expect_named(weather_18_june_2019.df,
             c("time", "PAR_umol", "PAR_diff_fr", "global_watt", "day_of_year",
               "month_of_year", "month_name", "calendar_year", "solar_time",
               "sun_elevation", "sun_azimuth", "was_sunny", "wind_speed",
               "wind_direction", "air_temp_C", "air_RH", "air_DP", "air_pressure" ))
})
