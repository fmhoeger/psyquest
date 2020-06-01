context("SCS")
library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/SCS_EN", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

# Enter id
app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Please rate the following statements according to how well the statement applies to you. There are no right or wrong answers, but it is important that you rate each statement according to how you honestly feel. Be sure to be honest with yourself as you consider the statement you are rating. To mark your answer, simply click on the item that corresponds with your feelings towards the statement. Continue")
app$click_next()

app$expect_ui_text("Question 1 out of 25 I'm usually a lot of fun to be with Strongly disagree Disagree Agree Strongly agree")
app$click("btn4_text")

app$expect_ui_text("Question 2 out of 25 People do not seem interested in talking with me Strongly disagree Disagree Agree Strongly agree")
app$click("btn1_text")

#3
app$click("btn1_text")

#4
app$click("btn4_text")

#5
app$click("btn1_text")

#6
app$click("btn1_text")

#7
app$click("btn1_text")

#8
app$click("btn4_text")

#9
app$click("btn4_text")

#10
app$click("btn1_text")

#11
app$click("btn4_text")

#12
app$click("btn1_text")

#13
app$click("btn4_text")

#14
app$click("btn1_text")

#15
app$click("btn1_text")

#16
app$click("btn4_text")

#17
app$click("btn1_text")

#18
app$click("btn1_text")

#19
app$click("btn4_text")

#20
app$click("btn1_text")

#21
app$click("btn1_text")

#22
app$click("btn4_text")

#23
app$click("btn1_text")

#24
app$click("btn1_text")

#25
app$click("btn1_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SCS"))
expect_equal(
  results[["SCS"]],
  list(
    q1 = "btn4_text",
    q2 = "btn1_text",
    q3 = "btn1_text",
    q4 = "btn4_text",
    q5 = "btn1_text",
    q6 = "btn1_text",
    q7 = "btn1_text",
    q8 = "btn4_text",
    q9 = "btn4_text",
    q10 = "btn1_text",
    q11 = "btn4_text",
    q12 = "btn1_text",
    q13 = "btn4_text",
    q14 = "btn1_text",
    q15 = "btn1_text",
    q16 = "btn4_text",
    q17 = "btn1_text",
    q18 = "btn1_text",
    q19 = "btn4_text",
    q20 = "btn1_text",
    q21 = "btn1_text",
    q22 = "btn4_text",
    q23 = "btn1_text",
    q24 = "btn1_text",
    q25 = "btn1_text",
    General = 145
  )
)

app$stop()
