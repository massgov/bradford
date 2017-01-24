#### PLOTS ####
makeBreakoutPlot <- function(dat, breakouts, x, y, plot.title = "") {
  # makes a timeseries line plot with vertical red bars indicating the date a breakout is detetected
  # Args:
  #   dat = data frame which is the output of detect breakouts in bradford. This df contains point
  #         estimates for user satisfaction as well as a vector indicating standard error for the point
  #         estimate and dates for each
  #   breakouts = a vector of dates for which a breakout was detected
  #   x = the vector to plot along the x axis
  #   y = the vector to plot along the y axis
  #   plot.title = atomic character or factor vector which will be the plot title
  # Returns:
  #   a ggplot object
  limits = aes(ymax = prop_affirmative + prop_affirmative_se,
                ymin = prop_affirmative - prop_affirmative_se)
  if (length(breakouts) > 0) {
    plt = ggplot(dat, aes_string(x = x, y = y)) +
      geom_vline(xintercept = as.numeric(breakouts), color = "red", linetype = "dashed") +
      geom_line(group = 1) +
      geom_errorbar(limits) +
      theme_bw() +
      xlab("") +
      ggtitle(plot.title) +
      ylab("Proportion Finding Desired Content") +
      scale_x_date()
    return(plt)
  } else {
    plt = ggplot(dat, aes_string(x = x, y = y)) +
      geom_line(group = 1) +
      geom_errorbar(limits) +
      theme_bw() +
      ggtitle(plot.title) +
      xlab("") +
      ylab("Proportion Finding Desired Content")
    return(plt)
  }
}

makeVolumeAreaPlot <- function(df, x, y, fill, plot.title = "", xlab = "", ylab = "") {
  # makes a stacked area plot for traffic/response volumes according to a given categorical
  # Args:
  #   df = a data frame containing dates and volumes to plot along with a categorial to fill by
  #   x = the vector to plot along the x axis
  #   y = the vector to plot along the y axis
  #   fill = the categorical which will constitute the "stacks" in the area chart
  #   plot.title = atomic character or factor vector which will be the plot title
  #   xlab = atomic character or factor vector which will be the x axis label
  #   ylab = atomic character or factor vector which will be the y axis label
  # Returns:
  #   a ggplot object
 df %>%
    ggplot(aes_string(x = x, y = y, fill = fill)) +
      geom_area() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(plot.title) +
      xlab(xlab) +
      ylab(ylab)
}

makeVolumeBarPlot <- function(df, x, y, plot.title = "", xlab = "", ylab = "") {
  # makes a bar plot which shows the volume of a supplied categorial over time
  # Args:
  #   df = a data frame of counts and categorical values
  #   x = the vector to plot along the x axis (should be a vector of dates)
  #   y = the vector to plot along the y axis
  #   plot.title = atomic character or factor vector which will be the plot title
  #   xlab = atomic character or factor vector which will be the x axis label
  #   ylab = atomic character or factor vector which will be the y axis label
  # Returns:
  #   a ggplot object
  df %>%
  ggplot(aes_string(x = x, y = y)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_bw() +
    ggtitle(plot.title) +
    xlab(xlab) +
    ylab(ylab)
}

makeAffirmativeBarPlot <- function(df, x, y, plot.title = "", xlab = "", ylab = "") {
  # makes a bar plot which shows the volume of a supplied categorial
  # Args:
  #   df = a data frame of counts and categorical values
  #   x = the vector to plot along the x axis (should be a categorical)
  #   y = the vector to plot along the y axis
  #   plot.title = atomic character or factor vector which will be the plot title
  #   xlab = atomic character or factor vector which will be the x axis label
  #   ylab = atomic character or factor vector which will be the y axis label
  # Returns:
  #   a ggplot object
  df %>%
  ggplot(aes_string(x = x, y = y)) +
    geom_bar(stat = "identity") +
    xlab(xlab) +
    ggtitle(plot.title) +
    ylab(ylab) +
    theme_bw()
}

#### PLOT HELPERS ####
printGGplotly <- function(plt) {
  # coerces a ggplot object to a plotly object and prints that objects contents for rendering
  # Args:
  #   plt = a ggplot object to coerce to a plotly object and print
  # Returns:
  #   printed contents of the plotly object
  print(plotly::ggplotly(plt))
}
