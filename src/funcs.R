# Functions for the analysis of formstack data

createTimeBucket <- function(x) {
  # takes a vector of hours in the range of 0-23 and creates categorical time of day factor vector
  # Args: x = a numeric vector taking on values 0-23 
  # Returns: a factor vector taking the values "Early AM", "AM", "Afternoon", "Evening", "Late Night"
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  if (any(is.na(x))) {
    warning("the supplied vector contains NAs")
  }
  if (any(x < 0, na.rm = T) | any(x > 24, na.rm = T)) {
    warning("the supplied vector contains values falling outside of 0-23, NAs will be returned")
  }
  factor(
    ifelse(x < 6 & x > 0, "Early AM", 
           ifelse(x > 5 & x < 12, "AM",
                  ifelse(x > 11 & x < 18, "Afternoon", 
                         ifelse(x > 17  & x < 9, "Evening", 
                                ifelse(x < 24 & x > 0, "Late Night", NA
                                       )
                                )
                         )
                  )
           )
    )
}
