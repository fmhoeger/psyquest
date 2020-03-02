library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/DEG_DE", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Bitte gebe Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Wir möchten gerne möglichst präzise Informationen von dir bekommen. Wirst du bei dem nachfolgenden Test und den Fragen dein Bestes geben? Ja Nein")
app$click("btn1_text")

app$expect_ui_text("Hast du Probleme oder Schwierigkeiten mit dem Hören (z.B. krankheitsbedingt)? Ja Nein")
app$click("btn2_text")

app$expect_ui_text("Ich bin weiblich männlich anderes möchte ich nicht sagen")
app$click("btn1_text")

app$expect_ui_text("Was ist deine Nationalität/Staatsangehörigkeit? (In welchem Land hast du deinen Pass?) Deutsch Britisch Afghanisch Bulgarisch Chinesisch Griechisch Iranisch Irakisch Italienisch Kosovarisch Polnisch Rumänisch Russisch Serbisch Syrisch Türkisch Andere Nationalität Weiter")
app$click_next()

app$expect_ui_text("In welchem Land hast du die meiste Zeit deines Lebens verbracht? Deutschland Großbritannien Afghanistan Bulgarien China Griechenland Iran Irak Italien Kosovo Polen Rumänien Russland Serbien Syrien Türkei Anderes Land Weiter")
app$setInputs(dropdown = "TR")
app$click_next()

app$expect_ui_text("Was ist deine Muttersprache? (welche Sprache sprichst du zu Hause?) Deutsch Englisch Albanisch Arabisch Bulgarisch Chinesisch Persisch Griechisch Italienisch Kurdisch Paschtunisch Polnisch Rumänisch Russisch Serbisch Türkisch Andere Sprache Weiter")
app$setInputs(dropdown = "TR")
app$click_next()

app$expect_ui_text("Sprichst du noch eine andere Sprache in deinem Alltag? Keine Englisch Albanisch Arabisch Bulgarisch Chinesisch Persisch Deutsch Griechisch Italienisch Kurdisch Paschtunisch Polnisch Rumänisch Russisch Serbisch Türkisch Andere Sprache Weiter")
app$setInputs(dropdown = "DE")
app$click_next()

app$expect_ui_text("In welchem Monat und Jahr wurdest du geboren? Monat Januar Februar März April Mai Juni Juli August September Oktober November Dezember Jahr 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 Weiter")
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
    q7 = "TR",
    q8 = "DE",
    q9 = c("2", "1999"),
    q10 = "btn1_text",
    q11 = "btn2_text",
    'Best Shot' = 1,
    'Hearing Impairment' = 2,
    'Type of Hearing Impairment' = "",
    Gender = 1,
    Age = 253,
    Nationality = "DE",
    'Country Formative Years' = "TR",
    'First Language' = "tr",
    'Second Language' = "de",
    Handedness = c(1, 2)
  )
)

app$stop()
