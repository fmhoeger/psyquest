library(tidyverse)

read_external_dicts <- function(data_dir, external_data_dir, output_dir, language = "it", dry_run = T){
  dict_files <- list.files(data_dir, pattern = ".csv", full.names = TRUE)
  language <- tolower(language)
  bad_files <- c()
  dicts <-
    map(dict_files, function(filepath) {
      #dict file must be UTF8 encoded!
      message(sprintf("Reading %s", filepath))
      # if(str_detect(filepath, "zzz-lang")){
      #   browser()
      # }
      tmp <- read.csv(filepath, sep = ";", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "utf8") %>%
        as_tibble() %>%
        filter(nchar(de) != 0, nchar(en) != 0)
      if(!("de_f" %in% names(tmp))){
        tmp <- tmp  %>% mutate(de_f = de)
      }
      #browser()
      fname <- file.path(external_data_dir, basename(filepath))
      message(sprintf("Try reading external dict '%s'...", fname))
      if(!file.exists(fname)){
        message(sprintf("...failed (missing). Using 'en' instead"))
        tmp <- tmp %>% mutate(!!language := en)
        bad_files <<- c(bad_files, fname)
        return(tmp)
      }
      ext <- read.csv(fname, sep = ";", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "utf8") %>%
        as_tibble() %>%
        select(key, all_of(language))

      if(nrow(ext) == 0){
        message(sprintf("...failed (empty). Using 'en' instead"))
        tmp <- tmp %>% mutate(!!language := en)
        bad_files <<- c(bad_files, fname)
        return(tmp)
      }
      if(nrow(ext) == nrow(tmp)){
        # missing_languages <- setdiff(names(ext), c("key", names(tmp)))
        # message(sprintf("Addding '%s'", paste(missing_languages, collapse = ";")))
        if(language %in% names(tmp)){
          tmp2 <- tmp
        }
        else{
          tmp2 <- tmp %>% left_join(ext, by = "key")
        }
        if(nrow(ext) != nrow(tmp2) & !any(is.na(tmp2[[language]]))){
          message(sprintf("...failed (wrong format or language missing). Using 'en' instead"))
          bad_files <<- c(bad_files, fname)
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
    message(sprintf("Writing '%s' to  %s (dry run = %s)", fname, output_dir, dry_run))
    if(!dry_run)
      write.table(dicts[[fname]],
                  file.path(output_dir, basename(fname)), sep = ";",
                  row.names = F,
                  quote = T,
                  col.names = T,
                  fileEncoding = "utf8")

  })
  invisible(basename(bad_files))
}
