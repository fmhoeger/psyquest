library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/SES_DE", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

# Enter id
app$expect_ui_text("Bitte gebe Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Welchen höchsten Bildungsabschluss hat deine Mutter? Keinen Schulabschluss Haupt- oder Realschulabschluss Berufsausbildung Fachhochschulreife oder Abitur Fachhochschulabschluss, Diplom, Bachelor an Universität (3 Jahre and der Uni) höherer Universitätsabschluss (z. B. Magister, Staatsexamen, Master, Doktor, d.h. mindestens 5 Jahre an der Uni)) Weiß ich nicht Weiter")
cb <- app$findElement(css = "input[value=choice6]")
cb$click()
app$click_next()

app$expect_ui_text("Welchen höchsten Bildungsabschluss hat dein Vater? Keinen Schulabschluss Haupt- oder Realschulabschluss Berufsausbildung Fachhochschulreife oder Abitur Fachhochschulabschluss, Diplom, Bachelor an Universität (3 Jahre and der Uni) höherer Universitätsabschluss (z. B. Magister, Staatsexamen, Master, Doktor, d.h. mindestens 5 Jahre an der Uni)) Weiß ich nicht Weiter")
cb <- app$findElement(css = "input[value=choice5]")
cb$click()
app$click_next()

app$expect_ui_text("Bitte beantworte die nächsten Fragen für die Person in deiner Familie, die das meiste Geld verdient. Sollten zwei Personen gleich viel verdienen, wähle die ältere Person. Arbeitet die höchstverdienende Person im Moment nicht, beantworte die Frage gemäß der letzten Arbeitsstelle. Ist die Person freiberuflich tätig? Arbeitnehmer (Arbeiter/in, Angestellte/r, Beamte/ Beamtin etc.) Freiberuflich tätig mit Angestellten Freiberuflich/freier MitarbeiterIn ohne Angestellte Weiter")
cb <- app$findElement(css = "input[value=choice2]")
cb$click()
app$click_next()

app$expect_ui_text("Wie viele Personen arbeiten für die Person? 1 bis 9 Angestellte 10 oder mehr Angestellte Weiter")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
app$click_next()

app$expect_ui_text("Betreut die Person andere Angestellte? (z.B. ein Abteilungsleiter oder Vorarbeiter ist verantwortlich für andere Arbeitnehmer) Ja Nein Weiter")
cb <- app$findElement(css = "input[value=choice4]")
cb$click()
app$click_next()

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("SES"))
expect_equal(
  results[["SES"]],
  list(
    q1 = "choice6",
    q2 = "choice5",
    q3 = "choice2",
    q4 = "choice1",
    q5 = "choice4",
    'Educational degree' = 1.5,
    Class = 3
  )
)

print(app$get_results() %>% as.list())

app$stop()
