library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/GRT_EN", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Please respond to the following 8 items. Be honest - there are no right or wrong answers! Continue")
app$click_next()

app$expect_ui_text("Question 1 out of 8 New ideas and projects sometimes distract me from previous ones. Very much like me Mostly like me Somewhat like me Not much like me Not like me at all")
app$click("btn1_text")

app$click("btn2_text")

app$click("btn3_text")

app$click("btn4_text")

app$click("btn5_text")

app$click("btn1_text")

app$click("btn2_text")

app$click("btn3_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("GRT"))
expect_equal(
  results[["GRT"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    q5 = "btn5_text",
    q6 = "btn1_text",
    q7 = "btn2_text",
    q8 = "btn3_text",
    General = 2.875
  )
)

app$stop()
