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
cb <- app$findElement(css = "input[value=choice3]")
cb$click()
app$click_next()

app$expect_ui_text("Bitte kreuze nun an, welche Beschreibung am Besten auf den Beruf der Person zutrifft. Arbeit die Person im Moment nicht, dann denke an die letzte Arbeitsstelle. Moderne Berufe (z.B. LehrerIn, Krankenschwester, PhysiotherapeutIn, SozialarbeiterIn, KünstlerIn, MusikerIn, Software DesignerIn, Beamter/-in im gehobenen Dienst) Angestellte und mittlere Berufe (z.B. SekretärIn, VerwaltungsangestellteR, SachbearbeiterIn, PflegerIn, Beamter/-in im mittleren Dienst, telefonischer Kundendienst) Senior Management oder Verwaltung (z.B. zuständig für Planung, Organisation und Koordination, sowie Finanzangelegenheiten, Leiter einer Firma, Direktor einer Abteilung) Technische und handwerkliche Berufe (z.B. MechanikerIn, EinrichterIn, InspekteurIn, ElektrikerIn, GärtnerIn, LandwirtIn, ZugführerIn, VorarbeiterIn) Angelernte Berufe undDiensleistungsberufe (z.B. Postangestellte/-r, KindergärtnerIn, LandwirtschaftsgehilfIn, RezeptionistIn, VerkäuferIn, mithelfende/-r Familienangehörige/-r, Beamter/-in im einfachen Dienst) Einfache Arbeiter undDiensleistungsberufe (z.B. Reinigungskraft, Portier, SchneiderIn, KellnerIn, Barista) Mittleres oder Junior Management (z.B. BüroleiterIn, BankdirektorIn, RestaurantleiterIn, AbteilungsleiterIn, LogistikmanagerIn, VerkaufsleiterIn) Traditionelle Fachberufe (z.B. SteuerberaterIn, Rechtsanwalt/-anwältin, Arzt/Ärztin, WissenschaftlerIn, IngeneurIn, Beamter/-in im höheren Dienst) Weiter")
cb <- app$findElement(css = "input[value=choice7]")
cb$click()
app$click_next()

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("SES"))
expect_equal(
  results[["SES"]],
  list(
    q1 = "choice6",
    q2 = "choice5",
    q3 = "choice3",
    q6 = "choice7",
    educational_degree = 1.5,
    class = 3
  )
)

app$stop()
