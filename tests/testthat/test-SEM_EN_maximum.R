context("SEM")
library(psychTestR)
library(psyquest)
library(testthat)

app <- AppTester$new("apps/SEM_EN")

# Enter id
app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Please rate the following statements according to how well the statement applies to you. There are no right or wrong answers, but it is important to answer each question as honestly as possible. Continue")
app$click_next()

app$expect_ui_text("Question 1 out of 23 How often do you have trouble paying attention in classes? 1 (Almost) never 2 3 4 5 Almost always")
app$click("btn1_text")

app$expect_ui_text("Question 2 out of 23 How often do you get schoolwork done on time? 1 (Almost) never 2 3 4 5 Almost always")
app$click("btn5_text")

app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SEM"))
expect_equal(
  results[["SEM"]],
  list(
    q1 = "btn1_text",
    q2 = "btn5_text",
    q3 = "btn1_text",
    q4 = "btn1_text",
    q5 = "btn1_text",
    q6 = "btn1_text",
    q7 = "btn1_text",
    q8 = "btn5_text",
    q9 = "btn5_text",
    q10 = "btn5_text",
    q11 = "btn5_text",
    q12 = "btn1_text",
    q13 = "btn1_text",
    q14 = "btn5_text",
    q15 = "btn5_text",
    q16 = "btn5_text",
    q17 = "btn5_text",
    q18 = "btn5_text",
    q19 = "btn5_text",
    q20 = "btn5_text",
    q21 = "btn5_text",
    q22 = "btn5_text",
    q23 = "btn5_text",
    Attentiveness = 5,
    `Behavioral Engagement` = 5,
    `School Compliance` = 5,
    `School belonging` = 5,
    `Emotional Engagement` = 5,
    `Valuing of School Education` = 5,
    `Self-regulated Learning` = 5,
    `Cognitive Engagement` = 5,
    `Cognitive Strategy Use` = 5
  )
)

app$stop()
