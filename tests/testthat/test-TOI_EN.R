context("TOI")
library(psychTestR)

app <- AppTester$new("apps/TOI_EN")

# Enter id
app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Read each sentence below and select the one option that shows how much you agree with it. There are no right or wrong answers. Continue")
app$click_next()

app$expect_ui_text("Question 1 out of 7 You have a certain amount of intelligence, and you really can't do much to change it. Strongly agree Agree Mostly agree Mostly disagree Disagree Strongly disagree")
app$click("btn1_text")

app$expect_ui_text("Question 2 out of 7 You can learn new things, but you can't really change your basic intelligence. Strongly agree Agree Mostly agree Mostly disagree Disagree Strongly disagree")
app$click("btn2_text")

app$expect_ui_text("Question 3 out of 7 If I knew I wasn't going to do well at a task, I probably wouldn't do it even if I might learn a lot from it. Strongly agree Agree Mostly agree Mostly disagree Disagree Strongly disagree")
app$click("btn3_text")

app$expect_ui_text("Question 4 out of 7 Your intelligence is something about you that you can't change very much. Strongly agree Agree Mostly agree Mostly disagree Disagree Strongly disagree")
app$click("btn4_text")

app$expect_ui_text("Question 5 out of 7 It's much more important for me to learn things in my class than it is to get the best grades. Strongly agree Agree Mostly agree Mostly disagree Disagree Strongly disagree")
app$click("btn5_text")

app$expect_ui_text("Question 6 out of 7 Although I hate to admit it, I sometimes would rather do well in a class than learn a lot. Strongly agree Agree Mostly agree Mostly disagree Disagree Strongly disagree")
app$click("btn6_text")

app$expect_ui_text("Question 7 out of 7 If I had to choose between getting a good grade and being challenged in class, I would choose … getting a good grade being challenged")
app$click("btn1_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("TOI"))
expect_equal(
  results[["TOI"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    q5 = "btn5_text",
    q6 = "btn6_text",
    q7 = "btn1_text",
    `Theory of Inteligence` = 4.6666667,
    `Goals Choice` = 3.25
  )
)

app$stop()
