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
app$click("btn1_text")

app$expect_ui_text("Question 2 out of 23 How often do you get schoolwork done on time? 1 (Almost) never 2 3 4 5 Almost always")
app$click("btn2_text")

app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
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
expect_equal(names(results), c("SEM"))
expect_equal(
  results[["SEM"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    q5 = "btn5_text",
    q6 = "btn1_text",
    q7 = "btn2_text",
    q8 = "btn3_text",
    q9 = "btn4_text",
    q10 = "btn5_text",
    q11 = "btn1_text",
    q12 = "btn2_text",
    q13 = "btn3_text",
    q14 = "btn4_text",
    q15 = "btn5_text",
    q16 = "btn1_text",
    q17 = "btn2_text",
    q18 = "btn3_text",
    q19 = "btn4_text",
    q20 = "btn5_text",
    q21 = "btn1_text",
    q22 = "btn2_text",
    q23 = "btn3_text",
    Attentiveness = 3.3333333,
    `Behavioral Engagement` = 3.1428571,
    `School Compliance` = 3,
    `School belonging` = 4,
    `Emotional Engagement` = 3.625,
    `Valuing of School Education` = 3.4,
    `Self-regulated Learning` = 2.5,
    `Cognitive Engagement` = 2.625,
    `Cognitive Strategy Use` = 2.75
  )
)

app$stop()
