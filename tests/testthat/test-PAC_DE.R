context("PAC")
library(psychTestR)
library(psyquest)
library(testthat)

app <- AppTester$new("apps/PAC_DE")

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Wir versuchen herauszufinden, wie viel Sport du in den letzten drei Monaten gemacht hast. Das beinhaltet Sport, Tanz oder Dinge, bei denen du schwitzt und nach denen deine Beine sich müde anfühlen. Denk daran: Es gibt keine richtigen oder falschen Antworten – das ist kein Test. Bitte beantworte alle Fragen so ehrlich und exakt, wie du kannst – das ist sehr wichtig. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 5 Wie oft warst du während der letzten drei Monate aktiv beim Sportunterricht dabei (viel mitgespielt, gelaufen, gesprungen, geworfen)? Ich habe keinen Sportunterricht. Selten Manchmal Oft Immer")
app$click("btn1_text")

app$expect_ui_text("Frage 2 von 5 Wie oft hast du in den letzten drei Monaten nach der Schule Sport gemacht oder getanzt, z. B. in einer Schul-AG? Nie Einmal pro Woche 2- oder 3-mal pro Woche 4-mal pro Woche 5-mal pro Woche")
app$click("btn2_text")

app$expect_ui_text("Frage 3 von 5 An wie vielen Nachmittagen pro Woche hast du außerhalb der Schule Sport gemacht oder getanzt? Nie Einmal pro Woche 2- oder 3-mal pro Woche 4- oder 5-mal pro Woche 6- oder 7-mal pro Woche")
app$click("btn3_text")

app$expect_ui_text("Frage 4 von 5 Wie oft hast du in den letzten drei Monaten am Wochenende Sport gemacht oder getanzt? Nie An wenigen Wochenenden An einigen Wochenenden An den meisten Wochenenden Jedes Wochenende")
app$click("btn4_text")

app$expect_ui_text("Frage 5 von 5 Wie oft hast du in den letzten drei Monaten in deiner Freizeit Sport gemacht, getanzt, Spiele gespielt etc.? Nie Manchmal (1- bis 2-mal pro Woche) Öfters (3- bis 4-mal pro Woche) Recht oft (5- bis 6-mal pro Woche) Sehr oft (7-mal oder mehr pro Woche)")
app$click("btn1_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("PAC"))
expect_equal(
  results[["PAC"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    q5 = "btn1_text",
    General = 2.2
  )
)

app$stop()
