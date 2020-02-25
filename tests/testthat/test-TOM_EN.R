library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/TOM_EN", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

# Enter id
app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcden")
app$click_next()

# Intro
app$expect_ui_text("We are interested in your opinion on musical ability. Read each sentence and select the one option that shows how much you agree with it. There are no right or wrong answers. Continue")
app$click_next()

 app$expect_ui_text("Question 1 out of 12 You have a certain level of musical ability and you cannot really do much to change it. Strongly Disagree Disagree Neutral Agree Strongly agree")
app$click("btn5_text")

app$expect_ui_text("Question 2 out of 12 To be successful in music you need to learn and regularly practice techniques and skills on your instrument. Strongly Disagree Disagree Neutral Agree Strongly agree")
app$click("btn2_text")

app$click("btn1_text")
app$click("btn1_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn2_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("TOM"))
expect_equal(
  results[["TOM"]],
  list(
    q1 = "btn5_text",
    q2 = "btn2_text",
    q3 = "btn1_text",
    q4 = "btn1_text",
    q5 = "btn3_text",
    q6 = "btn4_text",
    q7 = "btn2_text",
    q8 = "btn4_text",
    q9 = "btn5_text",
    q10 = "btn2_text",
    q11 = "btn3_text",
    q12 = "btn1_text",
    Entity = 2.3333333,
    Stable = 2.6666667,
    Incremental = 3.1666667,
    Learning = 3,
    Gift = 2,
    Improvement = 3.3333333
  )
)

app$stop()
