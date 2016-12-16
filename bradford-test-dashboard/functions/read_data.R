readIntoList <- function(data.dir, pattern, gsub.pattern) {
  file.list <- list.files(DATA.DIR, pattern = pattern) %>%
    paste0(DATA.DIR, .) %>%
    purrr::map(readRDS) 
  # set names for slots 
  names(file.list) <- list.files(DATA.DIR, pattern = pattern) %>%
    gsub(x = ., pattern = gsub.pattern, replacement = "")
  return(file.list)
}

