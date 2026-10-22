library(tidyverse)

psyquest_dict_raw <-
  map_dfr(list.files("./data_raw/dicts/",  pattern = "*.csv$", full.names = TRUE), function(filepath) {
    #dict file must be UTF8 encoded!
     if(str_detect(filepath, "BMR")){
       #browser()
     }
    if(str_detect(filepath, "dummy_debug")){
      return(NULL)
    }
    message(sprintf("Reading: %s", basename(filepath)))
    #tmp <- read.table(filepath, sep = ";", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "utf8")
    tmp <- suppressMessages(readr::read_csv2(filepath, col_types = cols()))
    #browser()
    if(nrow(problems(tmp)) > 0){
      browser()
      print(problems(tmp))
    }
    if(str_detect(filepath, "zzz")){
      tmp <- tmp %>% filter(!key %in% c("OTHER_COUNTRY", "OTHER_LANGUAGE", "OTHER_NATIONALITY"))
    }

    if(!("de" %in% names(tmp))){
      tmp <- tmp  %>% mutate(de = en)
    }
    if(!("de_f" %in% names(tmp))){
      tmp <- tmp  %>% mutate(de_f = de)
    }
    if(!("it" %in% names(tmp))){
      tmp <- tmp  %>% mutate(it = en)
    }
    if(!("es" %in% names(tmp))){
      tmp <- tmp  %>% mutate(es = en)
    }
    if(!("lv" %in% names(tmp))){
      #print(tmp$key[1])
      tmp <- tmp  %>% mutate(lv = en)
    }
    if(!("ar" %in% names(tmp))){
      #print(tmp$key[1])
      tmp <- tmp  %>% mutate(ar = en)
    }

    tmp %>% filter(nchar(de) != 0, nchar(en) != 0)
  }) %>% distinct()

psyquest_dict <- psychTestR::i18n_dict$new(psyquest_dict_raw)
psyquest_dict_df <- psyquest_dict %>% as.data.frame()
usethis::use_data(psyquest_dict, overwrite = TRUE)
usethis::use_data(psyquest_dict_df, overwrite = TRUE)
