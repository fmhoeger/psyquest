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

app$expect_ui_text("Question 2 out of 8 Setbacks don't discourage me. Very much like me Mostly like me Somewhat like me Not much like me Not like me at all")
app$click("btn5_text")

app$expect_ui_text("Question 3 out of 8 I have been obsessed with a certain idea or project for a short time but later lost interest. Very much like me Mostly like me Somewhat like me Not much like me Not like me at all")
app$click("btn1_text")

app$expect_ui_text("Question 4 out of 8 I am a hard worker. Very much like me Mostly like me Somewhat like me Not much like me Not like me at all")
app$click("btn5_text")

app$expect_ui_text("Question 5 out of 8 I often set a goal but later choose to pursue a different one. Very much like me Mostly like me Somewhat like me Not much like me Not like me at all")
app$click("btn1_text")

app$expect_ui_text("Question 6 out of 8 I have difficulty maintaining my focus on projects that take more than a few months to complete. Very much like me Mostly like me Somewhat like me Not much like me Not like me at all")
app$click("btn1_text")

app$expect_ui_text("Question 7 out of 8 I finish whatever I begin. Very much like me Mostly like me Somewhat like me Not much like me Not like me at all")
app$click("btn5_text")

app$expect_ui_text("Question 8 out of 8 I am diligent. Very much like me Mostly like me Somewhat like me Not much like me Not like me at all")
app$click("btn5_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("GRT"))
expect_equal(
  results[["GRT"]],
  list(
    q1 = "btn1_text",
    q2 = "btn5_text",
    q3 = "btn1_text",
    q4 = "btn5_text",
    q5 = "btn1_text",
    q6 = "btn1_text",
    q7 = "btn5_text",
    q8 = "btn5_text",
    General = 1
  )
)

app$stop()
