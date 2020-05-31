library(tidyverse)

psyquest_dict_raw <-
  map_dfr(list.files("./data_raw/dicts/", full.names = TRUE), function(filepath) {
    read.csv(filepath, sep = ";", stringsAsFactors = FALSE, header = TRUE) %>%
      filter(nchar(DE) != 0, nchar(EN) != 0)
  })

psyquest_dict <- psychTestR::i18n_dict$new(psyquest_dict_raw)

usethis::use_data(psyquest_dict, overwrite = TRUE)
