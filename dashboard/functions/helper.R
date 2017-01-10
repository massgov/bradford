# Miscellaneous Convenience functions for bradford dashboard
library(magrittr)

factorPercentage <- function(factor.vec = "", factor.value = "") {
  # calculates the percentage of a vector that a given factor level represents
  # Args:
  #   factor.vec - a factor vector
  #   factor.value - a level of factor.vec for which the percentage will be calculated 
  #   is.percent.points - boolean indicating whether input is in percentage points(T) or not (F)
  #                       if F then we coerce num * 100
  # Returns:
  #   a scalar in percentage points
  if (!is.factor(factor.vec)) { 
   stop("factor.vec must be of class factor!")
  }
  if (factor.value %in% factor.vec == F) {
    stop("factor level not found in vector")
  }
  ifelse(factor.vec == factor.value, 1, 0) %>%
    mean(.) * 100
}

prettyPercent <- function(num, round.n = 1, is.percent.points = T) {
  # takes a scalar, coerces to character and appends a % symbol
  # Args:
  #   num = a scalar to coerce
  #   round.n = the number of digits to round to
  #   is.percent.points = boolean indicating whether num is in percentage points
  # Returns:
  #   an atomic character vector
  if (!is.numeric(num) | !is.numeric(round.n)) {
    stop("num and round.n must be of class numeric!")
  } 
  if (round.n < 0) {
    stop("round.n must be greater than or equal to 0")
  }
  if (num > 100) {
    warning("num > 100")
  } 
  if (is.percent.points) {
    paste(as.character(round(num, round.n)), "%")
  } else {
    num = num * 100
    paste(as.character(round(num, round.n)), "%")
  }
}
meanCount <- function(grouped.df, round.n = 0) {
  # takes a grouped df and counts according to the groups and calculates a mean for the n vector
  # Args:
  #   grouped.df = a grouped data frame created via dplyr::group_by()
  #   round.n = a scalar which will round the mean to the nth digit
  # Returns:
  #   a scalar 
  if (!dplyr::is.grouped_df(grouped.df)) {
    stop("grouped.df must be a grouped data frame. see ?dplyr::group_by")
  }
  if (!is.numeric(round.n)) {
    stop("round.n must be of class numeric!")
  }
  grouped.df %>%
    dplyr::count() %>%
    .[["n"]] %>%
    mean() %>%
    round(digits = round.n)
}

flagIncompleteTimeperiod <- function(reference.vector, time.unit) {
  # checks reference.vector to see if it belongs to a floored time period which is not yet complete
  # Args:
  #   reference.vector = a vector of class date to floor and check for incomplete time periods
  #   time.unit = the unit arg to floor_date, the period to floor the reference.vector to
  # Returns:
  #   a vector of booleans indicating which entries are part of a complete time period (TRUE) 
  if (!lubridate::is.Date(reference.vector) & !is.POSIXct(reference.vector)) {
    stop("reference.vector must be of class date or POSIXct!")
  } 
  if (!any(time.unit %in% c("day", "week", "month"))) {
    stop("unit can only be day, week, or month. If a more granular view is needed please file an issue.")
  }
  ifelse(reference.vector > lubridate::floor_date(lubridate::now(), unit = time.unit), F, T)
}