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
app$click("btn1_text")

app$expect_ui_text("Frage 2 von 23 Bluegrass Gefällt mir überhaupt nicht Gefällt mir größtenteils nicht Gefällt mir eher nicht Weder noch Gefällt mir eher Gefällt mir größtenteils Gefällt mir voll und ganz")
app$click("btn1_text")

app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SMP"))
expect_equal(
  results[["SMP"]],
  list(
    q1 = "btn1_text",
    q2 = "btn1_text",
    q3 = "btn1_text",
    q4 = "btn1_text",
    q5 = "btn1_text",
    q6 = "btn1_text",
    q7 = "btn1_text",
    q8 = "btn1_text",
    q9 = "btn1_text",
    q10 = "btn1_text",
    q11 = "btn1_text",
    q12 = "btn1_text",
    q13 = "btn1_text",
    q14 = "btn1_text",
    q15 = "btn1_text",
    q16 = "btn1_text",
    q17 = "btn1_text",
    q18 = "btn1_text",
    q19 = "btn1_text",
    q20 = "btn1_text",
    q21 = "btn1_text",
    q22 = "btn1_text",
    q23 = "btn1_text",
    "Intense & Rebellious" = 1,
    "Intense" = 1,
    "Reflective & Complex" = 1,
    "Sophisticated" = 1,
    "Upbeat & Conventional" = 1,
    "Unpretentious" = 1,
    "Energetic & Rhythmic" = 1,
    "Mellow" = 1,
    "Contemporary" = 1
  )
)

app$stop()
