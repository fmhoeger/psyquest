library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/battery_DE_DAC_PAC",
              package = "psyquest",
              mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text(
  "Wir versuchen herauszufinden, inwieweit du schauspielerischen Aktivitäten nachgegangen bist. Das beinhaltet Theater, Improvisation, Rollenspiele etc. Denk daran: Es gibt keine richtigen oder falschen Antworten – dies ist kein Test. Bitte beantworte alle Fragen so ehrlich und genau wie du kannst – das ist sehr wichtig. Weiter"
)
app$click_next()

app$expect_ui_text(
  "Frage 1 von 4 Wie oft hast du in den letzten drei Monaten nach der Schule Theater gespielt, z.B. in einer Schul-AG? Kein mal 1 mal pro Woche 2 oder 3 mal Pro Woche 4 mal pro Woche 5 mal pro Woche"
)
app$click("btn1_text")

app$expect_ui_text(
  "Frage 2 von 4 An wievielen Abenden hast du in den letzten drei Monaten Theater gespielt? Kein mal 1 mal pro Woche 2 oder 3 mal Pro Woche 4 oder 5 mal pro Woche 6 oder 7 mal pro Woche"
)
app$click("btn2_text")

app$expect_ui_text(
  "Frage 3 von 4 Wie oft hast du in den letzten drei Monaten am Wochenende Theater gespielt? Nie An sehr wenigen Wochenenden An einigen Wochenenden An den meisten Wochenenden Jedes Wochenende"
)
app$click("btn3_text")

app$expect_ui_text(
  "Frage 4 von 4 Wie oft hast du in den letzten drei Monaten während deiner Freizeit Theater gespielt? Überhaupt nicht Manchmal (1-2 mal pro Woche) Oft (3-4 mal pro Woche) Recht oft (5-6 mal pro Woche) Sehr oft (7 mal oder öfter pro Woche)"
)
app$click("btn4_text")

# PAC
app$expect_ui_text(
  "Wir versuchen herauszufinden, wie viel Sport du in den letzten drei Monaten gemacht hast. Das beinhaltet Sport, Tanz oder Dinge, bei denen du schwitzt und nachdem deine Beine sich müde anfühlen. Denk daran: Es gibt keine richtigen oder falschen Antworten – das ist kein Test. Bitte beantworte alle Fragen so ehrlich und exakt wir du kannst – das ist sehr wichtig. Weiter"
)
app$click_next()

app$expect_ui_text(
  "Frage 1 von 5 Wie oft warst du während der letzten 3 Monate aktiv beim Sportunterricht dabei (viel mitgespielt, gelaufen, gesprungen, geworfen)? Ich habe kein Sportunterricht. Selten Manchmal Oft Immer"
)
app$click("btn1_text")

app$expect_ui_text(
  "Frage 2 von 5 Wie oft hast du in den letzten 3 Monaten nach der Schule Sport gemacht oder getanzt, z.B. in einer Schul-AG? Nie 1 mal pro Woche 2 oder 3 mal pro Woche 4 mal pro Woche 5 mal pro Woche"
)
app$click("btn2_text")

app$expect_ui_text(
  "Frage 3 von 5 An wie vielen Nachmittagen pro Woche hast du außerhalb der Schule Sport gemacht oder getanzt? Nie 1 mal pro Woche 2 oder 3 mal pro Woche 4 oder 5 mal pro Woche 6 oder 7 mal pro Woche"
)
app$click("btn3_text")

app$expect_ui_text(
  "Frage 4 von 5 Wie oft hast du in den letzten Monaten am Wochenende Sport gemacht oder getanzt? Nie An wenigen Wochenenden An einigen Wochenenden An den meisten Wochenenden Jedes Wochenende"
)
app$click("btn4_text")

app$expect_ui_text(
  "Frage 5 von 5 Wie oft hast du in den letzten drei Monaten in deiner Freizeit Sport gemacht, getanzt, Spiele gespielt,…? Nie Manchmal (1-2 mal pro Woche) Öfters (3-4 mal pro Woche) Recht oft (5-6 mal pro Woche) Sehr oft (7 mal oder mehr pro Woche)"
)
app$click("btn5_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("DAC", "PAC"))
expect_equal(
  results[["DAC"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    General = 2.5
  )
)
expect_equal(
  results[["PAC"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    q5 = "btn5_text",
    General = 3
  )
)

app$stop()
