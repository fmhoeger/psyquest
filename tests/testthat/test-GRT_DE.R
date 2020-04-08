library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/GRT_DE", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Bitte gebe Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Bitte lese die folgenden Aussagen sorgfältig und entscheide, inwieweit du ihnen zustimmst. Sei bitte ehrlich - es gibt keine richtigen oder falschen Antworten oder Fangfragen. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 8 Neue Ideen und Projekte lenken mich manchmal von früheren ab. Passt sehr gut auf mich Passt auf mich Passt etwas auf mich Passt nicht auf mich Passt überhaupt nicht auf mich")
app$click("btn1_text")

app$click("btn2_text")

app$click("btn3_text")

app$click("btn4_text")

app$click("btn5_text")

app$click("btn1_text")

app$click("btn2_text")

app$click("btn3_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("GRT"))
expect_equal(
  results[["GRT"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    q5 = "btn5_text",
    q6 = "btn1_text",
    q7 = "btn2_text",
    q8 = "btn3_text",
    General = 2.875
  )
)

app$stop()
