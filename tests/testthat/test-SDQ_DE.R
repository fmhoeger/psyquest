library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/SDQ_DE", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

# Enter id
app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Bitte lies die folgenden Aussagen sorgfältig und entscheide, inwieweit du ihnen zustimmst. Es gibt keine richtigen oder falschen Antworten oder Fangfragen. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 25 Ich versuche, nett zu anderen Menschen zu sein. Ihre Gefühle sind mir wichtig. Nicht zutreffend Teilweise zutreffend Eindeutig zutreffend")
app$click("btn1_text")

app$expect_ui_text("Frage 2 von 25 Ich bin oft unruhig; ich kann nicht lange stillsitzen. Nicht zutreffend Teilweise zutreffend Eindeutig zutreffend")
app$click("btn2_text")

app$click("btn3_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn1_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SDQ"))
expect_equal(
  results[["SDQ"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn1_text",
    q5 = "btn2_text",
    q6 = "btn3_text",
    q7 = "btn1_text",
    q8 = "btn2_text",
    q9 = "btn3_text",
    q10 = "btn1_text",
    q11 = "btn2_text",
    q12 = "btn3_text",
    q13 = "btn1_text",
    q14 = "btn2_text",
    q15 = "btn3_text",
    q16 = "btn1_text",
    q17 = "btn2_text",
    q18 = "btn3_text",
    q19 = "btn1_text",
    q20 = "btn2_text",
    q21 = "btn3_text",
    q22 = "btn1_text",
    q23 = "btn2_text",
    q24 = "btn3_text",
    q25 = "btn1_text",
    Prosocial = 0.8,
    Difficulties = 1.1,
    Hyperactivity = 1,
    Externalising = 1.2,
    `Emotional problems` = 1,
    Internalising = 1,
    `Conduct problems` = 1.4,
    `Peer problems` = 1
  )
)

app$stop()
