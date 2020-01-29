library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/CCM_DE", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Bitte gebe Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()


app$expect_ui_text(
  "Bitte lies die folgende Liste mit musikalischen Aktivitäten und wähle die aus, die du in den vergangenen drei Monaten gemacht hast. Bitte klicke alles Zutreffende an. in einem Orchester gespielt mit Freunden Musik gemacht bei Events, Konzerten oder speziellen Gelegenheiten Musik gemacht Einzelunterricht an einem Instrument (oder Gesang) erhalten Gruppenunterricht an einem Instrument (oder Gesang) erhalten Musikunterricht in der Schule gehabt an (einer) Musik-AG(s) nach der Schule teilgenommen Playlists für mich oder andere zusammengestellt nichts der oben genannten Aktivitäten Weiter")
# app$click("input")


app$stop()
