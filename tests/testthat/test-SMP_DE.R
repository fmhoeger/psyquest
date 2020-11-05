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
app$click("btn2_text")

app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn6_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn6_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn6_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn2_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SMP"))
expect_equal(
  results[["SMP"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    q5 = "btn5_text",
    q6 = "btn6_text",
    q7 = "btn7_text",
    q8 = "btn1_text",
    q9 = "btn2_text",
    q10 = "btn3_text",
    q11 = "btn4_text",
    q12 = "btn5_text",
    q13 = "btn6_text",
    q14 = "btn7_text",
    q15 = "btn1_text",
    q16 = "btn2_text",
    q17 = "btn3_text",
    q18 = "btn4_text",
    q19 = "btn5_text",
    q20 = "btn6_text",
    q21 = "btn7_text",
    q22 = "btn1_text",
    q23 = "btn2_text",
    "Intense & Rebellious" = 3.5,
    "Intense" = 3.5,
    "Reflective & Complex" = 4,
    "Sophisticated" = 3.42857142857143,
    "Upbeat & Conventional" = 4,
    "Unpretentious" = 4.33333333333333,
    "Energetic & Rhythmic" = 3.4,
    "Mellow" = 5.33333333333333,
    "Contemporary" = 2.75
  )
)

app$stop()
