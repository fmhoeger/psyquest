context("SDQ")
library(psychTestR)
library(psyquest)
library(testthat)

app <- AppTester$new("apps/SDQ_EN")

# Enter id
app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Please read the following statements very carefully and rate how strongly you agree or disagree with them. There are no right or wrong answers or trick questions. Continue")
app$click_next()

app$expect_ui_text("Question 1 out of 25 I try to be nice to other people. I care about their feelings Not true Somewhat true Certainly true")
app$click("btn1_text")

app$expect_ui_text("Question 2 out of 25 I am restless, I cannot stay still for long Not true Somewhat true Certainly true")
app$click("btn2_text")

app$click("btn3_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SDQ"))
expect_equal(
  results[["SDQ"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn1_text",
    q5 = "btn2_text",
    q6 = "btn3_text",
    q7 = "btn1_text",
    q8 = "btn2_text",
    q9 = "btn3_text",
    q10 = "btn1_text",
    q11 = "btn2_text",
    q12 = "btn3_text",
    q13 = "btn1_text",
    q14 = "btn2_text",
    q15 = "btn3_text",
    q16 = "btn1_text",
    q17 = "btn2_text",
    q18 = "btn3_text",
    q19 = "btn1_text",
    q20 = "btn2_text",
    q21 = "btn3_text",
    q22 = "btn1_text",
    q23 = "btn2_text",
    q24 = "btn3_text",
    q25 = "btn1_text",
    Prosocial = 0.8,
    Difficulties = 1.1,
    Hyperactivity = 1,
    Externalising = 1.2,
    `Emotional problems` = 1,
    Internalising = 1,
    `Conduct problems` = 1.4,
    `Peer problems` = 1
  )
)

app$stop()
