#### PLOTS ####
makeBreakoutPlot <- function(dat, breakouts, x, y, plot.title = "") {
  limits <- aes(ymax = prop_affirmative + prop_affirmative_se, 
                ymin = prop_affirmative - prop_affirmative_se)
  if (length(breakouts) > 0) {
    plt <- ggplot(dat, aes_string(x = x, y = y)) +
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
    plt <- ggplot(dat, aes_string(x = x, y = y)) +
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
  df %>% 
  ggplot(aes_string(x = x, y = y)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_bw() +
    ggtitle(plot.title) +
    xlab(xlab) +
    ylab(ylab)
}

makeVolumeProportionPlot <- function(df, x, y, fill, plot.title = "", xlab = "", ylab = "") {
  # THIS DOES NOT WORK BECAUSE OF THE AES_STRING AND REORDER
  df %>% 
  ggplot(aes_string(x = reorder(x, y), y = y, fill = fill)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_continuous(name = "% Found <br>Content") +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(plot.title) +
    theme_bw()
}

makeAffirmativeBarPlot <- function(df, x, y, plot.title = "", xlab = "", ylab = "") {
  df %>%
  ggplot(aes_string(x = x, y = y)) +
    geom_bar(stat = "identity") +
    xlab(xlab) +
    ggtitle(plot.title) +
    ylab(ylab) +
    theme_bw()
}

#### PLOT HELPERS ####
padXlim <- function(plot.item.count, item.limit = 4,  offset = .5) {
  if (plot.item.count > item.limit) {
    return(plot.item.count + offset)
  } else {
    return(plot.item.count)
  }
}