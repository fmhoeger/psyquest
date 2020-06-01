context("PAC")
library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/PAC_EN", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("We are trying to find out about your engagement with sports and your level of physical activity over the last three months. This includes sports or dance that make you sweat or make your legs feel tired. Remember: There are no right or wrong answers – this is not a test. Please answer all the questions as honestly and accurately as you can – this is very important. Continue")
app$click_next()

app$expect_ui_text("Question 1 out of 5 In the last three months during your physical education (PE) classes, how often were you very active (playing hard, running, jumping, throwing)? I don't do PE. Hardly ever Sometimes Quite often Always")
app$click("btn1_text")

app$expect_ui_text("Question 2 out of 5 In the last three months, how many times per week did you do sports or dance right after school, e.g. as part of an after school club? None 1 time per week 2 or 3 times per week 4 times per week 5 times per week")
app$click("btn2_text")

app$expect_ui_text("Question 3 out of 5 In the last three months, how many evenings per week did you spend doing sports or dance outside school? None 1 time per week 2 or 3 times per week 4 or 5 times per week 6 or 7 times per week")
app$click("btn3_text")

app$expect_ui_text("Question 4 out of 5 In the last three months, how often did you do sports or dance on weekends? Never Very few weekends Some weekends Most weekends Every weekend")
app$click("btn4_text")

app$expect_ui_text("Question 5 out of 5 In the last three months, I spent my free time doing sports, dance or playing games … Not at all Sometimes (1-2 times a week) Often (3-4 times a week) Quite often (5-6 times a week) Very often (7 or more times a week)")
app$click("btn1_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("PAC"))
expect_equal(
  results[["PAC"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    q5 = "btn1_text",
    General = 2.2
  )
)

app$stop()
