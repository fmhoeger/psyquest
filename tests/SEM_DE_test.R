library(psychTestR)
library(psyquest)
library(testthat)

dir <- system.file("tests/SEM_DE", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir, phantomTimeout = 5000)

# Enter id
app$expect_ui_text("Bitte gebe Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Bitte gib an, in wie weit die folgenden Aussagen auf dich zutreffen. Es gibt keine richtigen oder falschen Antworten, es ist aber wichtig, dass du so ehrlich wie möglich antwortest. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 23 Wie oft hast du Schwierigkeiten, dich im Unterricht zu konzentrieren? 1 (Fast) nie 2 3 4 5 Fast immer")
app$click("btn1_text")

app$expect_ui_text("Frage 2 von 23 Wie oft machst du deine Hausaufgaben rechtzeitig? 1 (Fast) nie 2 3 4 5 Fast immer")
app$click("btn2_text")

app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
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
expect_equal(names(results), c("SEM", "results"))
expect_equal(results[[1]], list(q1="btn1_text", q2="btn2_text", q3="btn3_text", q4="btn4_text", q5="btn5_text", q6="btn1_text", q7="btn2_text", q8="btn3_text",q9="btn4_text", q10="btn5_text", q11="btn1_text", q12="btn2_text",q13="btn3_text", q14="btn4_text", q15="btn5_text", q16="btn1_text", q17="btn2_text", q18="btn3_text", q19="btn4_text", q20="btn5_text",q21="btn1_text", q22="btn2_text", q23="btn3_text"))
expect_equal(results[[2]], list(Attentiveness=3.3333333, `Behavioral Engagement`=3.1428571, `School Compliance`=3, `School belonging`=4, `Emotional Engagement`=3.625, `Valuing of School Education`=3.4, `Self-regulated Learning`=2.5, `Cognitive Engagement`=2.625, `Cognitive Strategy Use`=2.75))

app$stop()
