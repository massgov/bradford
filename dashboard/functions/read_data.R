readIntoList <- function(data.dir, pattern, gsub.pattern) {
  # reads a list of RDS files from a directory according to pattern into a single list
  # Args:
  #   data.dir = a directory to read files from
  #   pattern = a pattern to look for in the file name
  #   gsub.pattern = a pattern to remove from the file name, usually the file extension
  # Returns:
  #   a list of length n where n = length of files in dir who match pattern
  file.list = list.files(data.dir, pattern = pattern) %>%
    paste0(data.dir, .) %>%
    purrr::map(readRDS)
  # set names for slots
  names(file.list) = list.files(data.dir, pattern = pattern) %>%
    gsub(x = ., pattern = gsub.pattern, replacement = "")
  return(file.list)
}
