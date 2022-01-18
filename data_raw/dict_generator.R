library(tidyverse)

psyquest_dict_raw <-
  map_dfr(list.files("./data_raw/dicts/", full.names = TRUE), function(filepath) {
    #dict file must be UTF8 encoded!
    tmp <- read.table(filepath, sep = ";", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "utf8") %>%
      filter(nchar(de) != 0, nchar(en) != 0)
    if(!("de_f" %in% names(tmp))){
      tmp <- tmp  %>% mutate(de_f = de)
    }
    tmp
  })

psyquest_dict <- psychTestR::i18n_dict$new(psyquest_dict_raw)

usethis::use_data(psyquest_dict, overwrite = TRUE)
