library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/TOM_DE_subscales_Stable", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

# Enter id
app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Wir interessieren uns für deine Meinung zu musikalischen Fähigkeiten. Lies die folgenden Sätze und wähle aus, wie sehr du mit der Aussage übereinstimmst. Es gibt keine richtigen oder falschen Antworten. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 3 Man hat ein gewisses Level an musikalischen Fähigkeiten und kann nicht viel tun, um das zu ändern. Stimme überhaupt nicht zu Stimme nicht zu Weder noch Stimme zu Stimme sehr zu")
app$click("btn5_text")

app$click("btn5_text")
app$click("btn5_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("TOM"))
expect_equal(
  results[["TOM"]],
  list(
    q1 = "btn5_text",
    q3 = "btn5_text",
    q10 = "btn5_text",
    Stable = 5
  )
)

app$stop()
