#### PLOTS ####
makeBreakoutPlot <- function(df, breakouts, x, y, plot.title = "") {
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
  if (nrow(df) == 0) {
    ggplot(data.frame()) +  # pass an empty data frame 
      geom_blank() + 
      theme_bw() + 
      geom_label() + 
      geom_text() +
      theme(axis.line = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      annotate("text", label = "No Data, yet!",x = 50, y = 50, size = 8, colour = "black")
  } else {
    limits = aes(ymax = prop_affirmative + prop_affirmative_se,
                 ymin = prop_affirmative - prop_affirmative_se)
    if (length(breakouts) > 0) {
      plt = ggplot(df, aes_string(x = x, y = y)) +
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
      plt = ggplot(df, aes_string(x = x, y = y)) +
        geom_line(group = 1) +
        geom_errorbar(limits) +
        theme_bw() +
        ggtitle(plot.title) +
        xlab("") +
        ylab("Proportion Finding Desired Content")
      return(plt)
    }  
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
  if (nrow(df) == 0) {
    ggplot(data.frame()) +  # pass an empty data frame 
      geom_blank() + 
      theme_bw() + 
      geom_label() + 
      geom_text() +
      theme(axis.line = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      annotate("text", label = "No Data, yet!",x = 50, y = 50, size = 8, colour = "black")
  } else {
    df %>%
      ggplot(aes_string(x = x, y = y, fill = fill)) +
      geom_area() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(plot.title) +
      xlab(xlab) +
      ylab(ylab) 
  }
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
  if (nrow(df) == 0) {
    ggplot(data.frame()) +  # pass an empty data frame 
      geom_blank() + 
      theme_bw() + 
      geom_label() + 
      geom_text() +
      theme(axis.line = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      annotate("text", label = "No Data, yet!",x = 50, y = 50, size = 8, colour = "black")
  } else {
    df %>%
      ggplot(aes_string(x = x, y = y)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_bw() +
      ggtitle(plot.title) +
      xlab(xlab) +
      ylab(ylab)
  }
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
  if (nrow(df) == 0) {
    ggplot(data.frame()) +  # pass an empty data frame 
      geom_blank() + 
      theme_bw() + 
      geom_label() + 
      geom_text() +
      theme(axis.line = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      annotate("text", label = "No Data, yet!",x = 50, y = 50, size = 8, colour = "black")
  } else {
    df %>%
      ggplot(aes_string(x = x, y = y)) +
      geom_bar(stat = "identity") +
      xlab(xlab) +
      ggtitle(plot.title) +
      ylab(ylab) +
      theme_bw() 
  }
}

makeGroupedPareto <- function(df, x, y, fill, plot.title = "", xlab = "", ylab = "") {
  if (nrow(df) == 0) {
    ggplot(data.frame()) +  # pass an empty data frame 
      geom_blank() + 
      theme_bw() + 
      geom_label() + 
      geom_text() +
      theme(axis.line = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      annotate("text", label = "No Data, yet!", x = 50, y = 50, size = 8, colour = "black")
  } else {
    return(NA)
  }
}

makeGroupedTimeseries <- function(df, x, y, fill, plot.title = "", xlab = "", ylab = "") {
  if (nrow(df) == 0) {
    ggplot(data.frame()) +  # pass an empty data frame 
      geom_blank() + 
      theme_bw() + 
      geom_label() + 
      geom_text() +
      theme(axis.line = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      annotate("text", label = "No Data, yet!", x = 50, y = 50, size = 8, colour = "black")
  } else {
    df %>%
    ggplot(aes_string(x = x, y = y, color = fill)) +
      geom_line(group = 1) +
      theme_bw() +
      xlab(xlab) +
      ylab(ylab) +
      ggtitle(plot.title)
  }
}

#### PLOT HELPERS ####
padXlim <- function(plot.item.count, item.limit = 4,  offset = .5) {
  # conditional logic function to pad xlim values in ggplot plot object creation
  # Args:
  #   plot.item.count = the number of items to plot along a given axis. corresponds to the number of levels in a factor
  #   item.limit = the lower bound beyond which no padding will occur
  #   offset = the value with which to pad the xlim with
  # Returns:
  #   a scalar offset
  if (!any(purrr::map(c(plot.item.count, item.limit, offset), is.numeric))) {
    stop("all args to padXlim must be numeric!")
  }
  if (plot.item.count > item.limit) {
    return(plot.item.count + offset)
  } else {
    return(plot.item.count)
  }
}

printGGplotly <- function(plt) {
  # coerces a ggplot object to a plotly object and prints that objects contents for rendering
  # Args:
  #   plt = a ggplot object to coerce to a plotly object and print
  # Returns:
  #   printed contents of the plotly object
  print(plotly::ggplotly(plt))
}
