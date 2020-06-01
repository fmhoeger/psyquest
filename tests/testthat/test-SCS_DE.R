context("SCS")
library(psychTestR)
library(psyquest)
library(testthat)

app <- AppTester$new("apps/SCS_DE")

# Enter id
app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Bitte gib an, inwieweit die folgenden Aussagen auf dich zutreffen. Es gibt keine richtigen oder falschen Antworten, aber es ist wichtig, dass du ehrlich bist. Sei ehrlich mit dir selber, wenn du über die jeweilige Aussage nachdenkst. Um deine Antwort zu markieren, klicke auf das Feld, das am besten mit deinen Gefühlen in Bezug zur Aussage übereinstimmt. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 25 Mit mir hat man meistens eine Menge Spaß. Stimme überhaupt nicht zu Stimme nicht zu Stimme zu Stimme sehr zu")
app$click("btn1_text")

app$expect_ui_text("Frage 2 von 25 Andere scheinen nicht oft daran interessiert zu sein, mit mir zu reden. Stimme überhaupt nicht zu Stimme nicht zu Stimme zu Stimme sehr zu")
app$click("btn2_text")

app$click("btn3_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn1_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SCS"))
expect_equal(
  results[["SCS"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    q5 = "btn1_text",
    q6 = "btn2_text",
    q7 = "btn3_text",
    q8 = "btn4_text",
    q9 = "btn1_text",
    q10 = "btn2_text",
    q11 = "btn3_text",
    q12 = "btn4_text",
    q13 = "btn1_text",
    q14 = "btn2_text",
    q15 = "btn3_text",
    q16 = "btn4_text",
    q17 = "btn1_text",
    q18 = "btn2_text",
    q19 = "btn3_text",
    q20 = "btn4_text",
    q21 = "btn1_text",
    q22 = "btn2_text",
    q23 = "btn3_text",
    q24 = "btn4_text",
    q25 = "btn1_text",
    General = 85
  )
)

app$stop()
