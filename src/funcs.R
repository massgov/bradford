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
    ifelse(x <= 5 & x >= 0, "Early AM", 
           ifelse(x >= 6 & x <= 11, "AM",
                  ifelse(x > 11 & x <= 16, "Afternoon", 
                         ifelse(x >= 17  & x <= 20, "Evening", 
                                ifelse(x < 24 & x > 0, "Late Night", NA
                                       )
                                )
                         )
                  )
           )
    )
}

# Bayesian funcs
# perhaps create a calculateAlpha/calculateBeta function for lines 34-35 and 42-43
betaPosterior <- function(df, prior.mean, prior.n, sample.n = "", affirm.n = "") {
  # calculates an approximate beta posterior given the mean and n of a beta prior as well as 
  # the n and n sucesses from a binomial 
  # Args: 
  #   df = a data frame which contains vectors of sample  sizes and number of successful trials
  #   prior.mean = mean of the prior distribution 
  #   prior.n = support for the prior
  #   sample.n = n observations in the treatment population
  #   affirm.n = n successes in treatment population
  # Returns: a data frame which approximates the posterior distribution 
  a = df[[affirm.n]] + (prior.n * prior.mean) - 1
  b = df[[sample.n]] - df[[affirm.n]] + (prior.n * (1 - prior.mean)) - 1
  domain = seq(0, 1, 0.005)
  val = dbeta(domain, a, b)
  data.frame("domain" = domain, "prob_mass" = val)
}

betaPosteriorMean <- function(df, prior.mean, prior.n, sample.n = "", affirm.n = "") {
  # calculates the mean of the beta posterior given the mean and n of a beta prior as well as 
  # the n and n sucesses from a binomial 
  # Args:
  #   df = a data frame which contains vectors of sample  sizes and number of successful trials
  #   prior.mean = mean of the prior distribution 
  #   prior.n = support for the prior
  #   sample.n = n observations in the treatment population
  #   affirm.n = n successes in treatment population
  # Returns: a float, the mean of the posterior 
  a = df[[affirm.n]] + (prior.n * prior.mean) - 1
  b = df[[sample.n]] - df[[affirm.n]] + (prior.n * (1 - prior.mean)) - 1
  a / (a + b)
}