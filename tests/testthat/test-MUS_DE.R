context("MUS")
library(psychTestR)

app <- AppTester$new("apps/MUS_DE")

# Enter id
app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Frage 1 von 25 Bitte höre dir den Musikausschnitt an und entscheide, wie gut er dir gefällt. Click here to play Gefällt mir überhaupt nicht Gefällt mir nicht Gefällt mir größtenteils nicht Gefällt mir eher nicht Weder noch Gefällt mir eher Gefällt mir größtenteils Gefällt mir Gefällt mir voll und ganz")
app$click("btn1_text")

app$expect_ui_text("Frage 2 von 25 Bitte höre dir den Musikausschnitt an und entscheide, wie gut er dir gefällt. Click here to play Gefällt mir überhaupt nicht Gefällt mir nicht Gefällt mir größtenteils nicht Gefällt mir eher nicht Weder noch Gefällt mir eher Gefällt mir größtenteils Gefällt mir Gefällt mir voll und ganz")
app$click("btn2_text")

app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn6_text")
app$click("btn7_text")
app$click("btn8_text")
app$click("btn9_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn6_text")
app$click("btn7_text")
app$click("btn8_text")
app$click("btn9_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn6_text")
app$click("btn7_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("MUS"))
expect_equal(
  results[["MUS"]],
  list(
    q2 = "btn1_text",
    q3 = "btn2_text",
    q4 = "btn3_text",
    q5 = "btn4_text",
    q6 = "btn5_text",
    q7 = "btn6_text",
    q8 = "btn7_text",
    q9 = "btn8_text",
    q10 = "btn9_text",
    q11 = "btn1_text",
    q12 = "btn2_text",
    q13 = "btn3_text",
    q14 = "btn4_text",
    q15 = "btn5_text",
    q16 = "btn6_text",
    q17 = "btn7_text",
    q18 = "btn8_text",
    q19 = "btn9_text",
    q20 = "btn1_text",
    q21 = "btn2_text",
    q22 = "btn3_text",
    q23 = "btn4_text",
    q24 = "btn5_text",
    q25 = "btn6_text",
    q26 = "btn7_text",

    "Contemporary" = 5.2,
    "Unpretentious" = 5,
    "Intense" = 4.8,
    "Sophisticated" = 5.16666666666667,
    "Mellow" = 3.4
  )
)

app$stop()
