library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/MHE_DE", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Bitte gebe Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()


app$expect_ui_text(
  "Bitte gib für die folgenden Familienmitglieder an, welche musikalischen Aktivitäten sie in letzter Zeit ausführen. (Klicke alles Zutreffende an, oder keine der Boxen, falls nichts zutrifft.) Mutter …spielt ein Instrument …singt in einem Chor Weiter")
# app$click("input")


app$stop()
