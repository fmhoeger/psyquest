psyquest_item_bank <-
  map_dfr(list.files("./data_raw/item_banks/", full.names = TRUE), function(x) readRDS(x))

psyquest_item_bank <- psyquest_item_bank %>% as_tibble() %>% filter(str_detect(language, "en"), str_detect(score_func, "NA", negate = TRUE)) %>% select(main_id, score_func, subscales)

usethis::use_data(psyquest_item_bank, overwrite = TRUE)
