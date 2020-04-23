library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/SCS_EN", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir, phantomTimeout = 5000)

# Enter id
app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Please rate the following statements according to how well the statement applies to you. There are no right or wrong answers, but it is important that you rate each statement according to how you honestly feel. Be sure to be honest with yourself as you consider the statement you are rating. To mark your answer, simply click on the item that corresponds with your feelings towards the statement. Continue")
app$click_next()

app$expect_ui_text("Question 1 out of 25 I'm usually a lot of fun to be with Strongly disagree Disagree Agree Strongly agree")
app$click("btn1_text")

app$expect_ui_text("Question 2 out of 25 People do not seem interested in talking with me Strongly disagree Disagree Agree Strongly agree")
app$click("btn2_text")

app$click("btn3_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn1_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SCS"))
expect_equal(
  results[["SCS"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    q5 = "btn1_text",
    q6 = "btn2_text",
    q7 = "btn3_text",
    q8 = "btn4_text",
    q9 = "btn1_text",
    q10 = "btn2_text",
    q11 = "btn3_text",
    q12 = "btn4_text",
    q13 = "btn1_text",
    q14 = "btn2_text",
    q15 = "btn3_text",
    q16 = "btn4_text",
    q17 = "btn1_text",
    q18 = "btn2_text",
    q19 = "btn3_text",
    q20 = "btn4_text",
    q21 = "btn1_text",
    q22 = "btn2_text",
    q23 = "btn3_text",
    q24 = "btn4_text",
    q25 = "btn1_text",
    General = 85
  )
)

app$stop()
