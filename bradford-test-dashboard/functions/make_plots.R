makeBreakoutPlot <- function(dat, breakouts, x, y) {
  limits <- aes(ymax = prop_affirmative + prop_affirmative_se, ymin = prop_affirmative - prop_affirmative_se)
  if (length(breakouts) > 0) {
    plt <- ggplot(dat, aes_string(x = x, y = y)) +
      geom_line(group = 1) +
      geom_errorbar(limits) +
      theme_bw() +
      xlab("") +
      ylab("Proportion Finding Desired Content") + 
      geom_vline(xintercept = as.numeric(breakouts), color = "red", linetype = "dashed") +
      scale_x_date()
    return(plt)
  } else {
    plt <- ggplot(dat, aes_string(x = x, y = y)) +
      geom_line(group = 1) +
      geom_errorbar(limits) +
      theme_bw() +
      xlab("") +
      ylab("Proportion Finding Desired Content")
    return(plt)
  }
}