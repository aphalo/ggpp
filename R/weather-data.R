#' Weather data
#'
#' A data set containing weather data measured in Viikki, Helsinki,
#' Finland. Values for all variables are means of 12 readings at 5 seconds
#' intervals. Sun angles were computed with R package 'photobiology'.
#'
#' The variables are as follows:
#' \itemize{
#'   \item time (yyyy-mm-dd hh:mm:ss)
#'   \item PAR_umol (umol m-2 s-1)
#'   \item PAR_diff_fr (/1)
#'   \item global_watt (W m-2)
#'   \item day_of_year
#'   \item month_of_year
#'   \item month_name
#'   \item calendar_year
#'   \item solar_time (h)
#'   \item sun_elevation (degrees above horizon)
#'   \item sun_azimuth (degrees)
#'   \item was_sunny (T/F)
#'   \item wind_speed (m s-1)
#'   \item wind_direction (degrees)
#'   \item air_temperature_C (C)
#'   \item air_RH (%)
#'   \item air_DP (C)
#'   \item air_pressure
#' }
#'
#' @docType data
#' @keywords datasets
#' @format A tibble with 18 columns and 1440 rows.
#'
#' @examples
#' names(weather_18_june_2019.df)
#' head(weather_18_june_2019.df)
#' nrow(weather_18_june_2019.df)
#'
#' @references P. J. Aphalo, unpublished data.
#'
"weather_18_june_2019.df"
