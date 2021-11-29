library(tidyverse)

read_external_dicts <- function(data_dir, external_data_dir, output_dir, language = "it"){
  dict_files <- list.files(data_dir, full.names = TRUE)
  dicts <-
    map(dict_files, function(filepath) {
    #dict file must be UTF8 encoded!
    message(sprintf("Reading %s", filepath))
    tmp <- read.csv(filepath, sep = ";", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "utf8") %>%
      as_tibble() %>%
      filter(nchar(de) != 0, nchar(en) != 0)
    if(!("de_f" %in% names(tmp))){
      tmp <- tmp  %>% mutate(de_f = de)
    }

    message(sprintf("Reading external dict %s", filepath))
    fname <- file.path(external_data_dir, basename(filepath))
    if(!file.exists(fname)){
      tmp <- tmp %>% mutate(!!language := en)
      return(tmp)
    }
    ext <- read.csv(fname, sep = ";", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "utf8") %>%
      as_tibble() %>%
      select(key, all_of(tolower(language)))
    if(nrow(ext) == 0){
      tmp <- tmp %>% mutate(!!language := en)
      return(tmp)
    }
    if(nrow(ext) == nrow(tmp)){
      tmp2 <- tmp %>% left_join(ext, by = "key")
      if(nrow(ext) != nrow(tmp2) & !any(is.na(tmp2[[language]]))){
        tmp <- tmp %>% mutate(!!language := en)
      }
      else{
        message(sprintf("->Successfully added '%s' to  %s", language, filepath))
        tmp <- tmp2
      }
    }
    return(tmp)

    tmp
  })
  names(dicts) <- basename(dict_files)
  map(names(dicts), function(fname){
    #browser()
    message(sprintf("Writing '%s' to  %s", fname, output_dir))
    write.table(dicts[[fname]],
                file.path(output_dir, basename(fname)), sep = ";",
                row.names = F,
                quote = T,
                col.names = T,
                fileEncoding = "utf8")

  })
  return(NULL)
}
