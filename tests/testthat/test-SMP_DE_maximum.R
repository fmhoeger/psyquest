context("SMP")
library(psychTestR)

app <- AppTester$new("apps/SMP_DE")

# Enter id
app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Bitte geben Sie Ihre Präferenzen für jedes, der folgenden Musikgenres anhand der gegebenen Skalen an. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 23 Alternative Gefällt mir überhaupt nicht Gefällt mir größtenteils nicht Gefällt mir eher nicht Weder noch Gefällt mir eher Gefällt mir größtenteils Gefällt mir voll und ganz")
app$click("btn7_text")

app$expect_ui_text("Frage 2 von 23 Bluegrass Gefällt mir überhaupt nicht Gefällt mir größtenteils nicht Gefällt mir eher nicht Weder noch Gefällt mir eher Gefällt mir größtenteils Gefällt mir voll und ganz")
app$click("btn7_text")

app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SMP"))
expect_equal(
  results[["SMP"]],
  list(
    q1 = "btn7_text",
    q2 = "btn7_text",
    q3 = "btn7_text",
    q4 = "btn7_text",
    q5 = "btn7_text",
    q6 = "btn7_text",
    q7 = "btn7_text",
    q8 = "btn7_text",
    q9 = "btn7_text",
    q10 = "btn7_text",
    q11 = "btn7_text",
    q12 = "btn7_text",
    q13 = "btn7_text",
    q14 = "btn7_text",
    q15 = "btn7_text",
    q16 = "btn7_text",
    q17 = "btn7_text",
    q18 = "btn7_text",
    q19 = "btn7_text",
    q20 = "btn7_text",
    q21 = "btn7_text",
    q22 = "btn7_text",
    q23 = "btn7_text",
    "Intense & Rebellious" = 7,
    "Intense" = 7,
    "Reflective & Complex" = 7,
    "Sophisticated" = 7,
    "Upbeat & Conventional" = 7,
    "Unpretentious" = 7,
    "Energetic & Rhythmic" = 7,
    "Mellow" = 7,
    "Contemporary" = 7
  )
)

app$stop()
