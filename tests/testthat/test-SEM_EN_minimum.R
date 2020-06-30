context("SEM")
library(psychTestR)

app <- AppTester$new("apps/SEM_EN")

# Enter id
app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Please rate the following statements according to how well the statement applies to you. There are no right or wrong answers, but it is important to answer each question as honestly as possible. Continue")
app$click_next()

app$expect_ui_text("Question 1 out of 23 How often do you have trouble paying attention in classes? 1 (Almost) never 2 3 4 5 Almost always")
app$click("btn5_text")

app$expect_ui_text("Question 2 out of 23 How often do you get schoolwork done on time? 1 (Almost) never 2 3 4 5 Almost always")
app$click("btn1_text")

app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SEM"))
expect_equal(
  results[["SEM"]],
  list(
    q1 = "btn5_text",
    q2 = "btn1_text",
    q3 = "btn5_text",
    q4 = "btn5_text",
    q5 = "btn5_text",
    q6 = "btn5_text",
    q7 = "btn5_text",
    q8 = "btn1_text",
    q9 = "btn1_text",
    q10 = "btn1_text",
    q11 = "btn1_text",
    q12 = "btn5_text",
    q13 = "btn5_text",
    q14 = "btn1_text",
    q15 = "btn1_text",
    q16 = "btn1_text",
    q17 = "btn1_text",
    q18 = "btn1_text",
    q19 = "btn1_text",
    q20 = "btn1_text",
    q21 = "btn1_text",
    q22 = "btn1_text",
    q23 = "btn1_text",
    Attentiveness = 1,
    `Behavioral Engagement` = 1,
    `School Compliance` = 1,
    `School belonging` = 1,
    `Emotional Engagement` = 1,
    `Valuing of School Education` = 1,
    `Self-regulated Learning` = 1,
    `Cognitive Engagement` = 1,
    `Cognitive Strategy Use` = 1
  )
)

app$stop()
