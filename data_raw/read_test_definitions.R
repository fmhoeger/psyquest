library(tidyverse)
library(data.table)

printf <- function(...) print(sprintf(...))
messagef <- function(...) message(sprintf(...))
data_dir <- "data_raw"
output_dir <- "./output"
file_ids_listenings_test <- "ids_listening_tests.xlsx"
file_ids_questionnaires <- "ids_questionnaire.xlsx"
file_scoring_scheme <- "longgold_scoring_codes.xlsx"

get_main_id<-function(id){
  if (length(id) > 1){
    return(unlist(lapply(id, get_main_id)))
  }
  elements <- unlist(strsplit(id, "_"))
  paste(elements[1:2], collapse="_")

}

get_domain<-function(id){
  if (length(id) > 1){
    return(unlist(lapply(id, get_domain)))
  }
  substr(unlist(strsplit(id, "_"))[1], 2, 4)

}

parse_short_IDs<-function(id){
  if (length(id) > 1){
    return(rbindlist(lapply(id, parse_short_IDs)))
  }
  elements <- unlist(strsplit(id, "_"))
  if(is.na(id)){
    return(NULL)
  }
  le<-length(elements)
  entity_id <- elements[1]
  if(nchar(entity_id) != 4){
    stop(messagef("Found entity ID '%s' of invalid length %d", entity_id, nchar(entity_id)))
  }
  domain<-substr(entity_id, 2, 4)
  general_type <- substr(entity_id, 1, 1)
  main_id<-  paste(elements[1:2], collapse="_")

  item_id <-elements[2]
  element_type <- elements[3]
  if (length(elements)>3){
    element_id <- elements[4]
    vers_no <- elements[5]
  }
  else{
    element_id <- NA
    vers_no <- NA
  }
  ret <-tibble(id=id,
                   main_id=main_id,
                   entity_id=entity_id,
                   domain=domain,
                   general_type=general_type,
                   item_id=item_id,
                   element_type=element_type,
                   element_id=element_id,
                   vers_no=vers_no)
  ret
}

read_language_file <- function(fname, sheets = NULL){
  #reads language files of a certain format.
  #each sheet contains one test
  #special sheet "GEN" contains general definitions
  #used all over the place for quick reference

  fname <- file.path(data_dir, fname)
  if(is.null(sheets)){
    sheets <- readxl::excel_sheets(fname)
  }
  ids <- list()
  for (s in sheets){
    messagef("Reading sheet %s", s)
    if(s == "MEDIA"){
      #Skip media for the moment, no language IDs there actually
      next
    }
    tab <- readxl::read_excel(fname, sheet = s) %>% as.data.frame()
    #print(sprintf("Sheet: %s, columns: %d", s, length(names(tab))))
    tab[,which(is.na(names(tab)))]<-NULL
    tab[,which(nchar(names(tab)) == 0)]<-NULL

    # commented because never called
    # if (s == "GEN" & str_detect(fname, "questionmaire")){
    #   tab <- tab[!is.na(tab$ID), ]
    # }

    #print(sprintf("Sheet: %s, columns: %d", s, length(names(tab))))
    #unify column names
    names(tab) <- gsub(" ", "_", tolower(names(tab)))
    #there are extra spaces in the DE column head
    names(tab) <- gsub("de_", "de", tolower(names(tab)))
    if (s == "GEN" & str_detect(fname, "questionnaire")){
      tab[is.na(tab)] <- ""
      tab <- tab[tab$entity_id != "",]
      tab$item_id <-sprintf("%04d", as.numeric(tab$item_id))
      #GEN and MEDIA sheets have columns for each ID part
      #other sheets containd short ids in a format as provided here.
      #see some document somewhere,
      #where the IDS system is defined
      tab$id <- paste(tab$entity_id,
                      tab$item_id,
                      tab$elementtype,
                      tab$element_id, tab$vers_no,
                      sep="_")
      tab$id <- gsub("[_]+$", "", tab$id)
    }
    #entries in language columns (currently DE and EN) are either plain text
    #or IDs that refer to some definition from GEN type.
    #We need to know that so add a column of logical markers
    tab$is_ref <- grepl("[A-Z]{4}[_][0-9]{4}[_][A-Z]*", tab$en)
    tab$main_id <- get_main_id(tab$id)
    #print(names(tab))
    ids[[s]] <- tab
  }
  #if only one sheet is requested, return it instead of length-1 list.
  if(length(sheets) == 1){
    return(ids[[1]])
  }
  ids
}

make_score_func <- function(weight, transform, no_responses){
  if(length(weight)>1){
    ret <-list()
    for(i in seq_along(weight)){
      ret[[i]] <- make_score_func(weight[i], transform[i], no_responses[i])
    }
    return(unlist(ret))
  }
  if (weight >0 ){
    sprintf("function(x) %s", transform)
  }
  else{
    if(transform != "x"){
      no_responses <- eval(parse(text=sprintf("function(x) %s", transform)))(no_responses)
      ret <- sprintf("%s", transform)
      kernel <- sprintf("(%d-(%s))", no_responses+1, transform)
      ret <- gsub("x", kernel, ret)
      ret <- sprintf("function(x) %s", ret)
      return(ret)
    }
    sprintf("function(x) %d-(%s)", no_responses+1, transform)

  }
}
read_test_definitions <- function(fname  = file_scoring_scheme, sheets = NULL){
  #read test definitions from excel file *fname*,
  #assumes that each test in defined in one sheet
  #param: sheets, if NULL (default) all sheets are read in to list
  fname <- file.path(data_dir, fname)
  if(is.null(sheets)){
    sheets <- readxl::excel_sheets(fname)
    no_sheets <- length(sheets)
  }
  else{
    no_sheets <- length(sheets)
  }
  messagef("Found %d sheets", length(sheets))
  ids <- list()
  for (s in sheets){
    messagef("Reading sheet %s", s)
    if (tolower(s) == "master")
      next
    #message(sprintf("Readin sheet %s", s))
    tab <- readxl::read_excel(fname, sheet  = s)

    tab[,which(is.na(names(tab)))] <- NULL
    tab <- tab[!is.na(tab$ID), ]

    names(tab) <- gsub(" ", "_", tolower(names(tab)))
    names(tab) <- gsub("number_responseoptions", "no_responses", tolower(names(tab)))

    tab$measurement_level <- tolower(tab$measurement_level)
    names(tab) <- gsub("measurement_level", "m_level", tolower(names(tab)))
    tab <- tab %>% mutate(score_func = make_score_func(weight, transform, no_responses))
    #tab$score_func <- sprintf("function(x) %d*(%s)", tab$weight, tab$transform)
    tab$score_func <- gsub("-\\(x\\)", "-x", tab$score_func)
    #tab$score_func <- gsub("\\(x\\+0\\)", "x", tab$score_func)
    tab$transform <- NULL
    tab$weight <- NULL
    tab$main_id <- get_main_id(tab$id)
    if (!("type" %in% names(tab))){
      tab$type <- "multiple choice"
    }
    #tab$score_func<- unlist(lapply(tab$score_func, function(func) eval(parse(text=func))))
    ids[[s]] <- tab

  }
  #if one sheet specifically requested, return data.frame instead of length-1 list
  if(no_sheets == 1){
    return(ids[[1]])
  }
  return(ids)
}

excel_table_to_csv<-function(fname, out_dir = ".", output_format = "csv"){
  #read test definitions from excel file *fname*,
  #assumes that each test in defined in one sheet
  #param: output, determings
  fname <- file.path(data_dir, fname)
  sheets <- readxl::excel_sheets(fname)
  messagef("Found %d sheets", length(sheets))
  ids <- list()
  for (s in sheets){
    messagef("Reading sheet %s", s)
    #message(sprintf("Readin sheet %s", s))
    tab <- readxl::read_excel(fname, sheet=s)
    tab[,which(is.na(names(tab)))]<-NULL
    tab<- tab[!is.na(tab[, 1]), ]
    sheet_fname <- file.path(out_dir, sprintf("%s.csv", s))
    messagef("Writing  %s", sheet_fname)
    write.table(tab, sheet_fname, sep = ";", row.names = F, quote = T)
  }
}

setup_workspace <- function(what = c("defs", "quest", "listen")){
  if("defs" %in% what){
    message("Reading test definitions...")
    td  <<- read_test_definitions()
  }
  if("quest" %in% what){
    message("Reading language file for questionnaires...")
    idq <<- read_language_file(file_ids_questionnaires)
  }
  if("listen" %in% what){
    message("Reading language file for listening tests...")
    idl <<- read_language_file(file_ids_listenings_test)
  }
}

export_test_csv <- function(test_id, fname){
  test <- td[[test_id]]
  if(is.null(test)){
    stop(sprintf("Unknown test ID: '%s'", test_id))
  }
  #test <- test %>% select(-no_response, -weight)
  #names(test) <- gsub("^id", "response_id", names(test))
  test$id <- 1:nrow(test)
  test <- test[, c("id", "main_id",setdiff(names(test), c("id", "main_id")))]
  fname <- file.path(output_dir, fname)
  write.table(test, fname, quote=TRUE, sep=",", row.names=FALSE)
}
make_button_texts <- function(n, text_data, max_btns){
  generics <- idq$GEN

  if (text_data$is_ref[1]){
    gen_id <- paste(strsplit(text_data$de[1], "_")[[1]][1:2], collapse="_")
    tmp <- generics %>% filter(main_id == gen_id)
  }
  else{
    tmp <-text_data
    #print(str(text_data))
    #stop()
  }
  #print(sprintf("nrow(tmp)=%d, n=%d", nrow(tmp), n))
  #print(tmp)
  #print(names(tmp))
  assertthat::assert_that(nrow(tmp) == n)
  ret <- data.frame(de = tmp$de, en = tmp$en)
  #print("")
  #print(str(tmp$de))
  #stop("HERE")
  ret <- data.frame(t(ret)[c("de", "en"), ])
  names(ret) <- sprintf("btn%d_text", 1:nrow(tmp))
  ret$language <- row.names(ret)
  row.names(ret) <- NULL
  if (n < max_btns){
    extra_rows <- sprintf("btn%d_text", seq(n+1, max_btns))
    ret[, extra_rows] <-""
  }
  #print(names(ret))
  ret
}
gather_free_item_data <- function(text_data, aux_data, id, max_btns){
  tmp        <- gather(text_data[which(aux_data$element_type == "I"), c("en", "de")])
  ret <- tibble(id = id,
                question = tmp$value,
                language = tmp$key,
                template = NA,
                score_func = NA,
                subscales = NA,
                layout = NA

  )
  #print("FREEEEEEEEE")
  #print(ret)
  dummy_text <- data.frame(value = rep("", max_btns))
  row.names(dummy_text) <- sprintf("btn%d_text", 1:max_btns)
  btns <- as.data.frame(t(dummy_text))
  btns$id <- id
  ret<-merge(ret, btns)
  ret
}
gather_item_data <- function(test_data, text_data, aux_data, id, max_btns){
  message("Enter gather_item_data")
  messagef("Gathering information for %s (%d, %d, %d)", id, nrow(test_data), nrow(text_data), nrow(aux_data))
  if(nrow(test_data) == 0){
    free_item_data <- gather_free_item_data(text_data, aux_data, id, max_btns)
    #print(sprintf("Found free item: %s", id))
    #print(free_item_data)
    #stop()
    return(free_item_data)
  }
  #print(text_data)
  #print(aux_data)
  #print(text_data[which(aux_data$element_type == "R"),])
  assertthat::assert_that(length(which(aux_data$element_type == "I")) == 1)

  tmp        <- gather(text_data[which(aux_data$element_type == "I"), c("en", "de")])
  template   <- sprintf("%d-option %s", test_data$no_responses[1], test_data$type)
  score_func <- test_data$score_func[1]
  layout     <- NA
  if("layout" %in% names(test_data)) layout <- test_data$layout[1]
  subscales  <- paste(test_data$scale, collapse=";")
  btns       <- make_button_texts(test_data$no_responses[1],
                                  text_data[which(aux_data$element_type == "R"),],
                                  max_btns)
  #print(str(btns))
  #stop("Eins reicht")
  #print(text_data[which(aux_data$element_type == "R"),])
  #browser()
  ret <- tibble(id = id,
                question = tmp$value,
                language = tmp$key,
                template = template,
                score_func = score_func,
                subscales = subscales,
                layout    = layout
                )
  ret <- merge(ret, btns)
  ret
}
export_item_banks <- function(test_ids, name_tag = "items", language = NULL, ignore_free_items = F, setup = T){
  names <- sprintf("%s_%s.csv", test_ids, name_tag)
  if(setup){
    setup_workspace()
  }
  for(i in 1:length(test_ids)){
    export_item_bank(test_ids[i], names[i], language = language, ignore_free_items = ignore_free_items)
  }
}
export_item_bank <- function(test_id, fname, language = NULL, ignore_free_items = F, output_format=c("csv", "RDS")){
  #browser()
  test <- td[[test_id]]
  if(is.null(test)){
    stop(sprintf("Unknown test ID: '%s'", test_id))
  }
  specifics <- idq[[test_id]]
  if(is.null(test)){
    stop(sprintf("Could not find strings for: '%s'", test_id))
  }
  ret      <- NULL
  item_ids <- unique(test$main_id)
  aux_ids  <- parse_short_IDs(specifics$id)
  free_items <- setdiff(as.character(specifics$main_id), as.character(test$main_id))
  if(!ignore_free_items){
    item_ids <- c(item_ids, free_items)
  }
  #print(aux_ids)
  assertthat::assert_that(length(unique(aux_ids$entity_id)) == 1)
  text_ids <- unique(specifics$main_id)
  ret <- NULL
  max_btns <- max(test$no_responses)
  #browser()
  for(ID in item_ids){
    text_data <- specifics[specifics$main_id == ID,]
    test_data <- test[test$main_id == ID,]
    aux_data  <- aux_ids[aux_ids$main_id == ID,]
    entry     <- gather_item_data(test_data, text_data, aux_data, ID, max_btns)
    if(any(is.na(names(entry)))){
      messagef("Error for ID %s", ID)
      print(entry)
      stop()
    }
    #print("")
    #print("===== ENTRY ========")
    #print(entry)
    #print("STR RET")
    #print(names(ret))
    #print("STR ENtRY")
    #print(names(entry))
    #print(intersect(names(ret), names(entry)))
    ret       <- rbind(ret, entry)
  }
  if(nrow(ret) == 0){
    stop(sprintf("Gathering information for test %s failed", test_id))
  }
  #if(length(free_items) > 0){
  #  print(sprintf("Found %d free items", length(free_items)))
  #}
  #ret
  ret$main_id <- ret$id
  ret$id <- 1:nrow(ret)
  ret <- ret[, c("id", "main_id",setdiff(names(ret), c("id", "main_id")))]
  fname <- file.path(fname)
  if (output_format[1] == "csv"){
    write.table(ret, fname, quote = TRUE, sep=",", row.names=FALSE)
  }
  else{
    saveRDS(ret, fname)
  }
  messagef("Item bank for test '%s' written to %s ", test_id, fname)
  ret
}

export_listening_test <- function(test_id, fname, output_format=c("csv", "RDS"), languages=NULL){
  test<-idl[[test_id]]

  if(is.null(test)){
    stop(sprintf("Unknown test ID: '%s'", test_id))
  }
  if(sum(test$is_ref)>0){
    stop(sprintf("References in listening test not supported yet"))
  }
  ret<- NULL
  #gather_vars <- intersect(names(test), c("id", "en", "de","ru", "mid"))
  gather_vars <- intersect(names(test), c("id", "en", "de", "mid"))
  ret <- gather(test[, gather_vars], key = language, value = content, -id)
  #print(names(ret))
  if(nrow(ret) == 0){
    stop(sprintf("Gathering information for test %s failed", test_id))
  }
  ret$main_id <- ret$id
  ret <- ret[!(is.na(ret$main_id)),]
  ret$id <- 1:nrow(ret)
  ret <- ret[, c("id", "main_id", setdiff(names(ret), c("id", "main_id")))]
  fname <- file.path(fname)
  if(output_format[1] == "csv"){
    write.table(ret, fname, quote = TRUE, sep = ",", row.names = FALSE, fileEncoding = "latin1")
  }
  else {
    saveRDS(ret, fname)
  }
  messagef("Item bank for test '%s' written to %s ", test_id, fname)
  ret
}

show_n_option_multiple_choice<-function(n, wid, question_data, explanatory_text="", as_string=F){
  if (n<2 || n > 7){
    error(sprintf("n should be between 2 and 7 got : %d", n))
  }
  template_name <-sprintf("%d-option multiple choice", n)
  params <- list(
    question = as.character(question_data[1, "question"]),
    explanatory.text = explanatory_text
  )
  for(i in 1:n){
    btn_text <-sprintf('btn%d_text', i)
    params[[btn_text]] <- as.character(question_data[1, btn_text])
  }
  if(as_string){
    params <- paste(deparse(params), collapse="")
    #print(params)
    out <- sprintf('concerto.template.show("%s", workSpaceID = %d, params = %s)',
                   template_name,
                   wid,
                   deparse(params))
  }
  else{
    out <- concerto.template.show(template_name,
                                workspaceID = wid,
                                params = params)
  }
  out
}
set_global_options <- function(opt_list){
  stopifnot(is.list(opt_list))
  for (opt in names(opt_list)){
    if (!exists(opt)){ assign(opt, opt_list[[opt]], .GlobalEnv)}
  }
}
get_actual_n<-function(question_data){
  qd <- question_data
  btns <- names(qd)[grepl("btn[0-9]", names(qd))]
  ret <- 0
  for (i in seq_along(btns)){
    if (nchar(as.character(qd[1, btns[i]])) == 0){
      ret <- i-1
    }
    else{
      ret <- i
    }
  }
  ret
}
make_multi_checkbox<-function(items, values, as_td = T){
  template <- '<tr>
                 <td style="text-align: center">
                  <input id="%s" name="%s" type="checkbox" value="%d" />
                  <label for="%s">%s</label>
                 </td>
               </tr>'
  if (length(values) == 1){
    values <- rep(values[1], length(items))
  }
  rows <- NULL
  for (i in seq_along(items)){
    item_id <-sprintf("box%d", i)
    row <- sprintf(template, item_id, item_id, values[i], item_id, items[i])
    rows <- c(rows, row)
  }
  ret <- paste(rows, collapse="")
  ret
}
to_psychTestR_format <- function(fname, outname = NULL){
  ext <- tolower(tools::file_ext(fname))
  #browser()
  if(ext == "csv"){
    labels <- readr::read_csv(fname)
  }
  else if(ext == "rds"){
    labels <- readr::read_rds(fname)
  }
  else {
    stop(sprintf("Unrecognized files extension: %s, expected csv or RDS", ext))
  }
  if (is.null(outname)){
    outname <- fname
  }
  #browser()
  labels <-
    labels %>%
    select(-id) %>%
    spread(key = language, value = content)
  #browser()
  if("ru" %in% names(labels)){
    labels <- labels %>% select(key = mid, DE = de, EN = en, RU = ru, -main_id)
  }
  else{
    labels <- labels %>% select(key = mid, DE = de, EN = en,  -main_id)

  }
  ext <- tolower(tools::file_ext(outname))
  if (ext == "csv"){
    write_csv(labels, outname)
  }
  else if (ext == "rds"){
    write_rds(labels, outname)
  }
  labels
}
extract_psychTestR_dict_from_item_bank <- function(item_bank, outname = NULL){
  dict <-
    item_bank %>%
    select(-id, -template, -score_func, -subscales, -layout) %>%
    gather(key, value,  -main_id, -language) %>%
    mutate(key = sprintf("%s_%s", main_id, key)) %>%
    select(-main_id) %>%
    spread(language, value) %>%
    #print() %>%
    select(key, DE = de, EN = en) %>%
    distinct(key, .keep_all = T) %>%
    mutate(key = gsub("btn", "CHOICE", key)) %>%
    mutate(key = gsub("_text", "", key)) %>%
    mutate(key = gsub("question", "PROMPT", key)) %>%
    #mutate(key_type = c("answer", "question")[ 1L + grepl( "PROMPT", key)]) %>%
    #mutate(item_number = gsub("_[A-Z0-9]*$", "", gsub("^[A-Z]*_", "", key)) %>% as.integer()) %>%
    filter(nchar(EN) != 0)
  ext <- tolower(tools::file_ext(outname))
  if(is.null(ext) || length(ext) == 0){
    ext <- ""
  }
  if (ext == "csv"){
    write_csv(dict, outname)
  }
  else if (ext == "rds"){
    write_rds(dict, outname)
  }
  dict
}
quest_defs <-
  tribble(
    ~test_id, ~type, ~no_scales, ~transform, ~scoring,
    "TPI", "homogeneous", 5, "polarity", "average",
    "DEG", "heterogenous", 11, "as_is", "as_is",
    "GMS", "heterogenous", 8, "polarity", "mixed",
    "TOI", "heterogeneous", 2, "polarity", "average",
    "TOM", "homogeneous", 6, "as_is", "average",
    "SOS", "homogeneous", 2, "polarity", "average",
    "SES", "heterogeneous", 2, "polarity", "algorithm",
    "MHE", "heterogeneous", 2, "polarity", "algorithm",
    "PAC", "homogeneous", 1, "as_is", "average",
    "DAC", "homogeneous", 1, "as_is","average",
    "GRT", "homogeneous", 1, "as_is","average",
    "HOP", "homogeneous", 1, "as_is","average",
    "SCA", "homogeneous", 1, "polarity", "table",
    "SCS", "homogeneous", 1, "polarity", "table",
    "CCM", "heterogeneous", 2, "as_is", "algorithm",
    "SEM", "homogeneous", 8, "polarity", "average",
    "SDQ", "homogeneous", 5, "affine", "average",)



export_all_psychtestR_dicts <- function(outdir = "data_raw"){
  setup_workspace()
  map(quest_defs %>% pull(test_id), function(test_id){
    labels_file <- file.path(outdir, "item_banks", sprintf("%s_item_bank.RDS", test_id))
    messagef("Exporting labels to to %s", labels_file)
    tmp_labels <- export_item_bank(test_id, labels_file, output_format = "RDS")
    dict_file <- file.path(outdir, "dicts", sprintf("%s_dict.RDS", test_id))
    messagef("Exporting dict to to %s", dict_file)
    extract_psychTestR_dict_from_item_bank(tmp_labels, dict_file)
  }) %>% invisible()
}

export_listenting_test_psychTestR_dict <- function(tests = "MIQ", out_dir = "data_raw/dicts"){
  setup_workspace()
  map(tests, function(t) {
    labels_file <- sprintf("data_raw/%s_labels.RDS", t)
    messagef("Exporting labels to to %s", labels_file)
    dict_file <- file.path(out_dir, sprintf("%s_dict.RDS", t))
    export_listening_test(test_id = t, fname = labels_file, output_format = "RDS")
    messagef("Exporting dict to to %s", dict_file)
    to_psychTestR_format(fname = labels_file, outname = dict_file)
  }) %>% invisible()
}
