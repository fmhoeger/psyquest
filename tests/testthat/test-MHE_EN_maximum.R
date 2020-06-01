context("MHE")
library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/MHE_EN", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Please indicate the degree of current musical activities for the following members of your family. (Tick all that apply.) My mother … is playing an instrument. … is singing in a choir. Continue")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
cb <- app$findElement(css = "input[value=choice2]")
cb$click()
app$click_next()

app$expect_ui_text("Please indicate the degree of current musical activities for the following members of your family. (Tick all that apply.) My father … is playing an instrument. … is singing in a choir. Continue")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
cb <- app$findElement(css = "input[value=choice2]")
cb$click()
app$click_next()

app$expect_ui_text("During holiday times and weekends, how often do your parents or someone else at home do the following activities with you? Help me with my homework Every day or nearly every day Once or twice per week Once or twice per month Never or hardly ever Continue")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
app$click_next()

app$expect_ui_text("During holiday times and weekends, how often do your parents or someone else at home do the following activities with you? Talk with me about a topic we've learned about in school Every day or nearly every day Once or twice per week Once or twice per month Never or hardly ever Continue")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
app$click_next()

app$expect_ui_text("During holiday times and weekends, how often do your parents or someone else at home do the following activities with you? Talk about school related problems Every day or nearly every day Once or twice per week Once or twice per month Never or hardly ever Continue")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
app$click_next()

app$expect_ui_text("During holiday times and weekends, how often do your parents or someone else at home do the following activities with you? Encourage me to make music Every day or nearly every day Once or twice per week Once or twice per month Never or hardly ever Continue")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
app$click_next()

app$expect_ui_text("During holiday times and weekends, how often do your parents or someone else at home do the following activities with you? Support me with practicing my instrument Every day or nearly every day Once or twice per week Once or twice per month Never or hardly ever Continue")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
app$click_next()

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("MHE"))
expect_equal(
  results[["MHE"]],
  list(
    q1 = "choice1,choice2",
    q2 = "choice1,choice2",
    q3 = "choice1",
    q4 = "choice1",
    q5 = "choice1",
    q6 = "choice1",
    q7 = "choice1",
    General = 4.0335039
  )
)

app$stop()
