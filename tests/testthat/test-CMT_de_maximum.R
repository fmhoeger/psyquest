context("CMT")
library(psychTestR)
library(psyquest)
library(testthat)

app <- AppTester$new("apps/CMT_de")

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("In diesem Fragebogen geht es um Deinen Musikunterricht in der Schule. Du wirst nach verschiedenen Tätigkeiten im Musikunterricht gefragt. Versuche Dich für jede Tätigkeit so gut es geht zu erinnern: Wie oft hast Du diese Tätigkeit gemacht? Vielleicht hast Du sie in keiner, vielleicht in mehr als zehn Unterrichtsstunden (45min) gemacht. Es gibt kein richtig und falsch. Wenn du dich nicht mehr erinnern kannst, entscheide einfach aus dem Bauch. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 12 Wie oft hast Du in den letzten 6 Monaten (vor Corona) im Musikunterricht… …Lieder mit Instrumenten begleitet? nie in 1 Unterrichtsstunde in 2 bis 5 Unterrichtsstunden in 6 bis 10 Unterrichtsstunden in mehr als 10 Unterrichtsstunden")
app$click("btn5_text")

app$expect_ui_text("Frage 2 von 12 Wie oft hast Du in den letzten 6 Monaten (vor Corona) im Musikunterricht… …verschiedene Tänze ausgeführt? nie in 1 Unterrichtsstunde in 2 bis 5 Unterrichtsstunden in 6 bis 10 Unterrichtsstunden in mehr als 10 Unterrichtsstunden")
app$click("btn5_text")

app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")
app$click("btn5_text")

app$expect_ui_text("Frage 12 von 12 Wie oft hast Du in den letzten 6 Monaten (vor Corona) im Musikunterricht… …Deine eigenen Fähigkeiten beim Musik machen mit anderen eingeschätzt? nie in 1 Unterrichtsstunde in 2 bis 5 Unterrichtsstunden in 6 bis 10 Unterrichtsstunden in mehr als 10 Unterrichtsstunden")
app$click("btn5_text")


app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("CMT"))
expect_equal(
  results[["CMT"]],
  list(
    q1 = "btn5_text",
    q2 = "btn5_text",
    q3 = "btn5_text",
    q4 = "btn5_text",
    q5 = "btn5_text",
    q6 = "btn5_text",
    q7 = "btn5_text",
    q8 = "btn5_text",
    q9 = "btn5_text",
    q10 = "btn5_text",
    q11 = "btn5_text",
    q12 = "btn5_text",
    '(Re)productive competence' = 5,
    'Reflection competence' = 5,
    'Psychomotoric competence' = 5,
    'Social competence' = 5
  )
)

app$stop()
