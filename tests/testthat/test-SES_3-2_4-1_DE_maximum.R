context("SES")
library(psychTestR)

app <- AppTester$new("apps/SES_DE")

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Welchen höchsten Bildungsabschluss hat deine Mutter? Keinen Schulabschluss Haupt- oder Realschulabschluss Berufsausbildung Fachhochschulreife oder Abitur Fachhochschulabschluss, Diplom, Bachelor an Universität (3 Jahre an der Uni) Höherer Universitätsabschluss (z. B. Magister, Staatsexamen, Master, Doktor, d. h. mindestens 5 Jahre an der Uni)) Weiß ich nicht Weiter")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
app$click_next()

app$expect_ui_text("Welchen höchsten Bildungsabschluss hat dein Vater? Keinen Schulabschluss Haupt- oder Realschulabschluss Berufsausbildung Fachhochschulreife oder Abitur Fachhochschulabschluss, Diplom, Bachelor an Universität (3 Jahre an der Uni) Höherer Universitätsabschluss (z. B. Magister, Staatsexamen, Master, Doktor, d. h. mindestens 5 Jahre an der Uni)) Weiß ich nicht Weiter")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
app$click_next()

app$expect_ui_text("Bitte beantworte die nächsten Fragen für den/die HauptverdienerIn in deinem Haushalt. Der/die HauptverdienerIn ist die Person in deinem Haushalt, die zum Großteil dafür verantwortlich ist, die Miete oder den Kredit zu zahlen. Wenn dafür mehrere Personen verantwortlich sind, wähle die Person, die mehr verdient. Sollten zwei Personen gleich viel verdienen, wähle die ältere Person. Arbeitet die höchstverdienende Person im Moment nicht, beantworte die Frage gemäß der letzten Arbeitsstelle. Ist die Person angestellt oder freiberuflich tätig? Arbeitnehmer (ArbeiterIn, AngestellteR, Beamte/-in etc.) Freiberuflich tätig mit Angestellten Freiberuflich/freieR MitarbeiterIn ohne Angestellte Weiter")
cb <- app$findElement(css = "input[value=choice2]")
cb$click()
app$click_next()

app$expect_ui_text("Wie viele Personen arbeiten für die Person? 1 bis 9 Angestellte 10 oder mehr Angestellte Weiter")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
app$click_next()

app$expect_ui_text("Bitte kreuze nun an, welche Beschreibung am besten auf den Beruf der Person zutrifft. Arbeitet die Person im Moment nicht, dann denke an die letzte Arbeitsstelle. Moderne Berufe (z. B. LehrerIn, Krankenschwester, PhysiotherapeutIn, SozialarbeiterIn, KünstlerIn, MusikerIn, Software DesignerIn, Beamter/-in im gehobenen Dienst) Angestellte und mittlere Berufe (z. B. SekretärIn, VerwaltungsangestellteR, SachbearbeiterIn, PflegerIn, Beamter/-in im mittleren Dienst, telefonischer Kundendienst) Senior Management oder Verwaltung (z. B. zuständig für Planung, Organisation und Koordination sowie Finanzangelegenheiten, LeiterIn einer Firma, DirektorIn einer Abteilung) Technische und handwerkliche Berufe (z. B. MechanikerIn, EinrichterIn, InspekteurIn, ElektrikerIn, GärtnerIn, LandwirtIn, ZugführerIn, VorarbeiterIn) Angelernte Berufe und Diensleistungsberufe (z. B. PostangestellteR, KindergärtnerIn, Landwirtschaftsgehilfe/-in, RezeptionistIn, VerkäuferIn, mithelfendeR FamilienangehörigeR, Beamter/-in im einfachen Dienst) Einfache Arbeiter- und Diensleistungsberufe (z. B. Reinigungskraft, PortierIn, SchneiderIn, KellnerIn, Barista) Mittleres oder Junior Management (z. B. BüroleiterIn, BankdirektorIn, RestaurantleiterIn, AbteilungsleiterIn, LogistikmanagerIn, VerkaufsleiterIn) Traditionelle Fachberufe (z. B. SteuerberaterIn, Rechtsanwalt/-anwältin, Arzt/Ärztin, WissenschaftlerIn, IngeneurIn, Beamter/-in im höheren Dienst) Weiter")
cb <- app$findElement(css = "input[value=choice6]")
cb$click()
app$click_next()

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("SES"))
expect_equal(
  results[["SES"]],
  list(
    q1 = "choice1",
    q2 = "choice1",
    q3 = "choice2",
    q4 = "choice1",
    q6 = "choice6",
    educational_degree = 6,
    class = 3
  )
)

app$stop()
