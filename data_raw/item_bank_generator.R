library(tidyverse)

psyquest_item_bank_raw <-
  map_dfr(list.files("./data_raw/item_banks", full.names = TRUE), function(filepath) {
    read.csv(filepath, sep = ";", stringsAsFactors = FALSE, header = TRUE)
  })

psyquest_item_bank <-
  psyquest_item_bank_raw %>%
  as_tibble() %>%
  filter(str_detect(language, "en"), str_detect(score_func, "", negate = FALSE)) %>%
  select(prompt_id = main_id, option_type = template, score_func, subscales, layout, audio_file)

usethis::use_data(psyquest_item_bank, overwrite = TRUE)
