library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/DAC_EN", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("We are trying to find out about your engagement with drama over the last three months. This includes theatre, improvisation, role-play, etc. Remember: There are no right and wrong answers – this is not a test. Please answer all the questions as honestly and accurately as you can – this is very important. Continue")
app$click_next()

app$expect_ui_text("Question 1 out of 4 In the last three months, how many times per week did you do drama right after school, e.g. as part of an after school club? None 1 time per week 2 or 3 times per week 4 times per week 5 times per week")
app$click("btn5_text")

app$expect_ui_text("Question 2 out of 4 In the last three months, how many evenings per week did you spend doing drama? None 1 time per week 2 or 3 times per week 4 or 5 times per week 6 or 7 times per week")
app$click("btn5_text")

app$expect_ui_text("Question 3 out of 4 In the last three months, how often did you do drama on weekends? Never Very few weekends Some weekends Most weekends Every weekend")
app$click("btn5_text")

app$expect_ui_text("Question 4 out of 4 In the last three months, I spent my free time doing drama … Not at all Sometimes (1-2 times a week) Often (3-4 times a week) Quite often (5-6 times a week) Very often (7 or more times a week)")
app$click("btn5_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("DAC"))
expect_equal(
  results[["DAC"]],
  list(
    q1 = "btn5_text",
    q2 = "btn5_text",
    q3 = "btn5_text",
    q4 = "btn5_text",
    General = 5
  )
)

app$stop()
