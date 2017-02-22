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
  if (round.n < 0) {
    stop("round.n must be a positive integer")
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
  if (!lubridate::is.Date(reference.vector) & !lubridate::is.POSIXct(reference.vector)) {
    stop("reference.vector must be of class date or POSIXct!")
  }
  if (!any(time.unit %in% c("day", "week", "month"))) {
    stop("unit can only be day, week, or month. If a more granular view is needed please file an issue.")
  }
  if (time.unit == "day") {
    reference.vector > lubridate::floor_date(lubridate::now(), unit = time.unit)
  } else {
    reference.vector >= lubridate::floor_date(lubridate::now(), unit = time.unit)
  }
}

getTopOrBottomK <- function(df, group.col, data.col, k, get.top = TRUE){

  if (is.numeric(df[[data.col]]) == F) {
    stop("data.col must be numeric")
  }

  if (class(df[[group.col]]) %in% c("factor", "character") == F) {
    stop("group.col must be character or factor")
  }
  
  if(class(k) != 'numeric'){
    stop("k must be numeric")
  }
  if(class(as.logical(get.top)) != 'logical'){
    print(class(get.top))
    stop("get top must be a boolean")
  }

 groups = df %>% dplyr::group_by_(group.col) %>%
            dplyr::rename_(n = paste(data.col), group = paste(group.col)) %>%
            dplyr::summarise(n = sum(n)) %>%
            dplyr::arrange(dplyr::desc(n))
 
 if(k > nrow(groups)){
  
   k = nrow(groups)
   
 }
       if(get.top){
         slice.from = 1
         slice.to = k
       } else{
         slice.from = nrow(groups) - k + 1
         slice.to = nrow(groups)
       }
 
  top.groups = groups %>% 
                    dplyr::slice(slice.from:slice.to) %>%
                    select(group) %>%
                    unlist(.)
  
  # Return data frame with only certain columns
  df[df[[group.col]] %in% top.groups,]

}

groupAndOrder <- function(df, group.col, data.col, percent = TRUE,top.pct = 1, 
                            filter.na = TRUE, top.k = NULL, get.top.k = TRUE){
  
  # Returns dataframe grouped by group.col with data.col as a sum and ordered
  #   returned dataframe includes columns named total, cumul and group
  # Args:
  #   df = dataframe
  #   group.col = column that will be grouped along x-axis
  #   data.col = numeric column
  #   percent = boolean indicating whether total should be a percentage or not
  #   top.pct = a numeric which filters all entries in total <= top.pct
  
  # guardrails
  if (is.numeric(df[[data.col]]) == F) {
    stop("data.col must be numeric")
  } 
  if (top.pct > 1) {
    stop("top.pct cannot be > 1")
  }

  if (top.pct < 1 & get.top.k == FALSE){
    stop("filtering and taking from the bottom")
  }
 
  if (class(df[[group.col]]) %in% c("factor", "character") == F) {
    stop("group.col must be character or factor")
  }
  
  
  grouped.df = df %>% 
    dplyr::group_by_(.dots = group.col) %>% 
    dplyr::summarise_(total = paste('sum( ', data.col,")")) %>%
    dplyr::arrange(., desc(total)) %>% 
    dplyr::rename_(group = paste(group.col)) %>% 
    {
      if (filter.na) {
        dplyr::filter(., ifelse(is.na(group), F, T))  # filter nas if filter.na is true
      } else {
        return(.)
      }
    }
  
  # Make cumulative column
  grouped.df$cumul <- cumsum(grouped.df$total)
  
  if(percent){
    data.total = sum(grouped.df$total)
    grouped.df$total = grouped.df$total / data.total
    grouped.df$cumul = cumsum(grouped.df$total)
  }
  
  
  grouped.df = grouped.df[top.pct >= (grouped.df$cumul / sum(grouped.df$total)), ]
  
  if(is.null(top.k)){
    return(grouped.df)
  } else{
   return(getTopOrBottomK(grouped.df, 'group', 'total', k = top.k, get.top = get.top.k))
  }
                

  
}
