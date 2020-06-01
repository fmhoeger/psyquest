context("SDQ")
library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/SDQ_EN", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

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
app$click("btn3_text")

# 3 I get a lot of headaches, stomach-aches or sickness
app$click("btn3_text")

# 4 I usually share with others (food, games, pens etc.)
app$click("btn1_text")

# 5 I get very angry and often lose my temper
app$click("btn3_text")

# 6 I am usually on my own. I generally play alone or keep to myself
app$click("btn3_text")

# 7 I usually do as I am told
app$click("btn1_text")

# 8 I worry a lot
app$click("btn3_text")

# 9 I am helpful if someone is hurt, upset or feeling ill
app$click("btn1_text")

#10 I am constantly fidgeting or squirming
app$click("btn3_text")

# 11 I have one good friend or more
app$click("btn1_text")

# 12 I fight a lot. I can make other people do what I want
app$click("btn3_text")

# 13 I am often unhappy, down-hearted or tearful
app$click("btn3_text")

# 14 Other people my age generally like me
app$click("btn1_text")

# 15 I am easily distracted, I find it difficult to concentrate
app$click("btn3_text")

# 16 I am nervous in new situations. I easily lose confidence
app$click("btn3_text")

# 17 I am kind to younger children
app$click("btn1_text")

# 18 I am often accused of lying or cheating
app$click("btn3_text")

# 19 Other children or young people pick on me or bully me
app$click("btn3_text")

# 20 I often volunteer to help others (parents, teachers, children)
app$click("btn1_text")

# 21 I think before I do things
app$click("btn1_text")

# 22 I take things that are not mine from home, school or elsewhere
app$click("btn3_text")

# 23 I get on better with adults than with people my own age
app$click("btn3_text")

# 24 I have many fears, I am easily scared
app$click("btn3_text")

# 25 I finish the work I'm doing. My attention is good
app$click("btn1_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SDQ"))
expect_equal(
  results[["SDQ"]],
  list(
    q1 = "btn1_text",
    q2 = "btn3_text",
    q3 = "btn3_text",
    q4 = "btn1_text",
    q5 = "btn3_text",
    q6 = "btn3_text",
    q7 = "btn1_text",
    q8 = "btn3_text",
    q9 = "btn1_text",
    q10 = "btn3_text",
    q11 = "btn1_text",
    q12 = "btn3_text",
    q13 = "btn3_text",
    q14 = "btn1_text",
    q15 = "btn3_text",
    q16 = "btn3_text",
    q17 = "btn1_text",
    q18 = "btn3_text",
    q19 = "btn3_text",
    q20 = "btn1_text",
    q21 = "btn1_text",
    q22 = "btn3_text",
    q23 = "btn3_text",
    q24 = "btn3_text",
    q25 = "btn1_text",
    Prosocial = 0,
    Difficulties = 2,
    Hyperactivity = 2,
    Externalising = 2,
    `Emotional problems` = 2,
    Internalising = 2,
    `Conduct problems` = 2,
    `Peer problems` = 2
  )
)

app$stop()
