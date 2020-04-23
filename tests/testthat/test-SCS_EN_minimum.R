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
app$click("btn4_text")

app$click("btn4_text")
app$click("btn1_text")
app$click("btn4_text")
app$click("btn4_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn4_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn4_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn4_text")
app$click("btn4_text")
app$click("btn4_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SCS"))
expect_equal(
   results[["SCS"]],
   list(
       q1 = "btn1_text",
       q2 = "btn4_text",
       q3 = "btn4_text",
       q4 = "btn1_text",
       q5 = "btn4_text",
       q6 = "btn4_text",
       q7 = "btn4_text",
       q8 = "btn1_text",
       q9 = "btn1_text",
       q10 = "btn4_text",
       q11 = "btn1_text",
       q12 = "btn4_text",
       q13 = "btn1_text",
       q14 = "btn4_text",
       q15 = "btn4_text",
       q16 = "btn1_text",
       q17 = "btn4_text",
       q18 = "btn1_text",
       q19 = "btn1_text",
       q20 = "btn4_text",
       q21 = "btn4_text",
       q22 = "btn1_text",
       q23 = "btn4_text",
       q24 = "btn4_text",
       q25 = "btn4_text",
       General = 45
     )
 )

  app$stop()
