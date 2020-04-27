library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/SOS_DE", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

# Enter id
app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Bitte denke über die Hörtests und Fragen nach, die du gerade beantwortet hast. Klicke auf die Aussagen, die deiner Meinung nach am besten zutreffen. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 10 Es war mir wichtig, diese Tests gut zu machen. Stimme überhaupt nicht zu Stimme nicht zu Weder noch Stimme zu Stimme sehr zu")
app$click("btn1_text")

app$expect_ui_text("Frage 2 von 10 Ich habe mir bei diesen Tests Mühe gegeben. Stimme überhaupt nicht zu Stimme nicht zu Weder noch Stimme zu Stimme sehr zu")
app$click("btn2_text")

app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SOS"))
expect_equal(
  results[["SOS"]],
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
    Importance = 2.8,
    Effort = 2.8
  )
)

app$stop()
