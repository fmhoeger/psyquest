context("battery")
library(psychTestR)
library(psyquest)
library(testthat)

app <- AppTester$new("apps/battery_DE_DAC_TOM_with_subscales")

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text(
  "Wir versuchen herauszufinden, inwieweit du schauspielerischen Aktivitäten nachgegangen bist. Das beinhaltet Theater, Improvisation, Rollenspiele etc. Denk daran: Es gibt keine richtigen oder falschen Antworten – dies ist kein Test. Bitte beantworte alle Fragen so ehrlich und genau, wie du kannst – das ist sehr wichtig. Weiter"
)
app$click_next()

app$expect_ui_text(
  "Frage 1 von 4 Wie oft hast du in den letzten drei Monaten nach der Schule Theater gespielt, z. B. in einer Schul-AG? Keinmal Einmal pro Woche 2- oder 3-mal pro Woche 4-mal pro Woche 5-mal pro Woche"
)
app$click("btn1_text")

app$expect_ui_text(
  "Frage 2 von 4 An wievielen Abenden hast du in den letzten drei Monaten Theater gespielt? Keinmal Einmal pro Woche 2- oder 3-mal pro Woche 4- oder 5-mal pro Woche 6- oder 7-mal pro Woche"
)
app$click("btn2_text")

app$expect_ui_text(
  "Frage 3 von 4 Wie oft hast du in den letzten drei Monaten am Wochenende Theater gespielt? Nie An sehr wenigen Wochenenden An einigen Wochenenden An den meisten Wochenenden Jedes Wochenende"
)
app$click("btn3_text")

app$expect_ui_text(
  "Frage 4 von 4 Wie oft hast du in den letzten drei Monaten während deiner Freizeit Theater gespielt? Überhaupt nicht Manchmal (1- bis 2-mal pro Woche) Oft (3- bis 4-mal pro Woche) Recht oft (5- bis 6-mal pro Woche) Sehr oft (7-mal oder öfter pro Woche)"
)
app$click("btn4_text")

# TOM
app$expect_ui_text("Wir interessieren uns für deine Meinung zu musikalischen Fähigkeiten. Lies die folgenden Sätze und wähle aus, wie sehr du mit der Aussage übereinstimmst. Es gibt keine richtigen oder falschen Antworten. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 9 Man hat ein gewisses Level an musikalischen Fähigkeiten und kann nicht viel tun, um das zu ändern. Stimme überhaupt nicht zu Stimme nicht zu Weder noch Stimme zu Stimme sehr zu")
app$click("btn5_text")

app$expect_ui_text("Frage 2 von 9 Um erfolgreich Musik zu machen, muss man regelmäßig Techniken und Fertigkeiten an seinem Instrument lernen und üben. Stimme überhaupt nicht zu Stimme nicht zu Weder noch Stimme zu Stimme sehr zu")
app$click("btn2_text")

app$click("btn1_text")

app$click("btn3_text")
app$click("btn4_text")

app$click("btn4_text")
app$click("btn5_text")
app$click("btn2_text")

app$click("btn1_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("DAC", "TOM"))
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
  results[["TOM"]],
  list(
    q1 = "btn5_text",
    q2 = "btn2_text",
    q3 = "btn1_text",
    q5 = "btn3_text",
    q6 = "btn4_text",
    q8 = "btn4_text",
    q9 = "btn5_text",
    q10 = "btn2_text",
    q12 = "btn1_text",
    Stable = 2.6666667,
    Incremental = 3.1666667
  )
)

app$stop()
