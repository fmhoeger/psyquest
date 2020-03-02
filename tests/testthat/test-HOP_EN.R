library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/HOP_EN", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Please select the answer that best describes how you think about yourself right now. Please take a few moments to focus on yourself and what is going on in your life at this moment. Continue")
app$click_next()

app$expect_ui_text("Question 1 out of 6 I think I am doing pretty well. None of the time A little of the time Some of the time A lot of the time Most of the time All of the time")
app$click("btn1_text")

app$click("btn2_text")

app$click("btn3_text")

app$click("btn4_text")

app$click("btn5_text")

app$click("btn6_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("HOP"))
expect_equal(
  results[["HOP"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    q5 = "btn5_text",
    q6 = "btn6_text",
    General = 3.5
  )
)

app$stop()
