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
app$click("btn3_text")

app$expect_ui_text("Question 2 out of 25 I am restless, I cannot stay still for long Not true Somewhat true Certainly true")
app$click("btn1_text")

# 3 I get a lot of headaches, stomach-aches or sickness
app$click("btn1_text")

# 4 I usually share with others (food, games, pens etc.)
app$click("btn3_text")

# 5 I get very angry and often lose my temper
app$click("btn1_text")

# 6 I am usually on my own. I generally play alone or keep to myself
app$click("btn1_text")

# 7 I usually do as I am told
app$click("btn3_text")

# 8 I worry a lot
app$click("btn1_text")

# 9 I am helpful if someone is hurt, upset or feeling ill
app$click("btn3_text")

#10 I am constantly fidgeting or squirming
app$click("btn1_text")

# 11 I have one good friend or more
app$click("btn3_text")

# 12 I fight a lot. I can make other people do what I want
app$click("btn1_text")

# 13 I am often unhappy, down-hearted or tearful
app$click("btn1_text")

# 14 Other people my age generally like me
app$click("btn3_text")

# 15 I am easily distracted, I find it difficult to concentrate
app$click("btn1_text")

# 16 I am nervous in new situations. I easily lose confidence
app$click("btn1_text")

# 17 I am kind to younger children
app$click("btn3_text")

# 18 I am often accused of lying or cheating
app$click("btn1_text")

# 19 Other children or young people pick on me or bully me
app$click("btn1_text")

# 20 I often volunteer to help others (parents, teachers, children)
app$click("btn3_text")

# 21 I think before I do things
app$click("btn3_text")

# 22 I take things that are not mine from home, school or elsewhere
app$click("btn1_text")

# 23 I get on better with adults than with people my own age
app$click("btn1_text")

# 24 I have many fears, I am easily scared
app$click("btn1_text")

# 25 I finish the work I'm doing. My attention is good
app$click("btn3_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SDQ"))
expect_equal(
  results[["SDQ"]],
  list(
    q1 = "btn3_text",
    q2 = "btn1_text",
    q3 = "btn1_text",
    q4 = "btn3_text",
    q5 = "btn1_text",
    q6 = "btn1_text",
    q7 = "btn3_text",
    q8 = "btn1_text",
    q9 = "btn3_text",
    q10 = "btn1_text",
    q11 = "btn3_text",
    q12 = "btn1_text",
    q13 = "btn1_text",
    q14 = "btn3_text",
    q15 = "btn1_text",
    q16 = "btn1_text",
    q17 = "btn3_text",
    q18 = "btn1_text",
    q19 = "btn1_text",
    q20 = "btn3_text",
    q21 = "btn3_text",
    q22 = "btn1_text",
    q23 = "btn1_text",
    q24 = "btn1_text",
    q25 = "btn3_text",
    Prosocial = 2,
    Difficulties = 0,
    Hyperactivity = 0,
    Externalising = 0,
    `Emotional problems` = 0,
    Internalising = 0,
    `Conduct problems` = 0,
    `Peer problems` = 0
  )
)

app$stop()
