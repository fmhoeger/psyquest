library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/CCM_EN", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Please read the following list of musical activities and select the ones that you have done during the last three months. Please tick all that apply. play in an orchestra sometimes play music with friends sometimes make music at events or special occasions receive individual lessons on an instrument (or voice) receive group lessons on an instrument (or voice) have music classes in school attend music-related after school clubs sometimes compile playlists for myself or others none of the above Continue")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
cb <- app$findElement(css = "input[value=choice3]")
cb$click()
cb <- app$findElement(css = "input[value=choice5]")
cb$click()
cb <- app$findElement(css = "input[value=choice7]")
cb$click()
app$click_next()

app$expect_ui_text("In the last three months I had 0 hours of music class per week 1 hour of music class per week 2 hours of music class per week 3 hours of music class per week 4 or more hours of music class per week Continue")
rb <- app$findElement(css = "input[value=choice2]")
rb$click()
app$click_next()

app$expect_ui_text("In the last three months I had 0 hours of music-related after school class per week 1 hour of music-related after school class per week 2 hours of music-related after school class per week 3 hours of music-related after school class per week 4 or more hours of music-related after school class per week Continue")
rb <- app$findElement(css = "input[value=choice3]")
rb$click()
app$click_next()

app$expect_ui_text("How much do you currently practice your instrument(s)? 0 minutes per day 0-15 minutes per day 15-30 minutes per day 30-60 minutes per day 1-1.5 hours per day 1.5 -2 hours per day more than 2 hours per day Continue")
rb <- app$findElement(css = "input[value=choice4]")
rb$click()
app$click_next()

app$expect_ui_text("Taken all together (own practice, rehearsals, instrument lessons, gigs, etc.) how much time do you spend making music per week at the moment? 0 hours per week 0-1 hours per week 1-2 hours per week 2-4 hours per week 4-7 hours per week 7-14 hours per week more than 14 hours per week Continue")
rb <- app$findElement(css = "input[value=choice5]")
rb$click()
app$click_next()

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("CCM"))
expect_equal(
  results[["CCM"]],
  list(
    q1 = "choice1,choice3,choice5,choice7",
    q2 = "choice2",
    q3 = "choice3",
    q4 = "choice4",
    q5 = "choice5",
    General = 3.7918376,
    Extra =2.5
  )
)

app$stop()
