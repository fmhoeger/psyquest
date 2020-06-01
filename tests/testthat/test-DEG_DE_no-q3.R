library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/DEG_DE", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Wir möchten gerne möglichst präzise Informationen von dir bekommen. Wirst du bei dem nachfolgenden Test und den Fragen dein Bestes geben? Ja Nein")
app$click("btn1_text")

app$expect_ui_text("Hast du Probleme oder Schwierigkeiten mit dem Hören (z. B. krankheitsbedingt)? Ja Nein")
app$click("btn2_text")

app$expect_ui_text("Ich bin weiblich männlich anderes möchte ich nicht sagen")
app$click("btn1_text")

app$expect_ui_text("Was ist deine Nationalität/Staatsangehörigkeit? (In welchem Land hast du deinen Pass?) Deutsch Afghanisch Algerisch Amerikanisch Britisch Bulgarisch Chinesisch Französisch Griechisch Irakisch Iranisch Italienisch Kanadisch Kosovarisch Kroatisch Polnisch Portugiesisch Rumänisch Russisch Senegalesisch Serbisch Spanisch Syrisch Türkisch Weißrussisch Andere Nationalität Weiter")
app$click_next()

app$expect_ui_text("In welchem Land hast du die meiste Zeit deines Lebens verbracht? Deutschland Afghanistan Algerien Bulgarien China Frankreich Griechenland Großbritannien Irak Iran Italien Kanada Kosovo Kroatien Polen Portugal Rumänien Russland Senegal Serbien Spanien Syrien Türkei USA Weißrussland Anderes Land Weiter")
app$setInputs(dropdown = "TR")
app$click_next()

app$expect_ui_text("Was ist deine Muttersprache? (Welche Sprache sprichst du zu Hause?) Deutsch Albanisch Arabisch Bulgarisch Chinesisch Englisch Griechisch Italienisch Japanisch Kurdisch Paschtunisch Persisch Polnisch Rumänisch Russisch Serbisch Spanisch Türkisch Ungarisch Andere Sprache Weiter")
app$setInputs(dropdown = "tr")
app$click_next()

app$expect_ui_text("Sprichst du noch eine andere Sprache in deinem Alltag? Keine Deutsch Albanisch Arabisch Bulgarisch Chinesisch Englisch Griechisch Italienisch Japanisch Kurdisch Paschtunisch Persisch Polnisch Rumänisch Russisch Serbisch Spanisch Türkisch Ungarisch Andere Sprache Weiter")
app$setInputs(dropdown = "de")
app$click_next()

app$expect_ui_text("In welchem Monat und Jahr wurdest du geboren? Monat Bitte Monat auswählen! Januar Februar März April Mai Juni Juli August September Oktober November Dezember Jahr Bitte Jahr auswählen! 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 Weiter")
app$setInputs(month = "2")
app$setInputs(year = "1999")
app$click_next()

app$expect_ui_text("Bist Du eher rechtshändig linkshändig beidhändig")
app$click("btn1_text")

app$expect_ui_text("Mit welcher Hand schreibst Du am besten? mit der rechten mit der linken mit beiden")
app$click("btn2_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("DEG"))
expect_equal(
  results[["DEG"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q4 = "btn1_text",
    q5 = "DE",
    q6 = "TR",
    q7 = "tr",
    q8 = "de",
    q9 = c("2", "1999"),
    q10 = "btn1_text",
    q11 = "btn2_text",
    'Best Shot' = 1,
    'Hearing Impairment' = 2,
    'Type of Hearing Impairment' = "",
    Gender = 1,
    Age = 256,
    Nationality = "DE",
    'Country Formative Years' = "TR",
    'First Language' = "tr",
    'Second Language' = "de",
    Handedness = c(1, 2)
  )
)

app$stop()
