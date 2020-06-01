context("GMS")
library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/GMS_conffile_AE_3_DE", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Frage 1 von 3 Ich beschäftige mich in meiner Freizeit viel mit musikbezogenen Aktivitäten. Stimme ganz und gar nicht zu Stimme nicht zu Stimme eher nicht zu Weder noch Stimme eher zu Stimme zu Stimme voll und ganz zu")
app$click("btn1_text")
app$expect_ui_text("Frage 2 von 3 Ich lese oder suche oft im Internet nach Dingen, die mit Musik zu tun haben. Stimme ganz und gar nicht zu Stimme nicht zu Stimme eher nicht zu Weder noch Stimme eher zu Stimme zu Stimme voll und ganz zu")
app$click("btn2_text")
app$expect_ui_text("Frage 3 von 3 Musik ist für mich eine Art von Sucht – ohne sie könnte ich nicht leben. Stimme ganz und gar nicht zu Stimme nicht zu Stimme eher nicht zu Weder noch Stimme eher zu Stimme zu Stimme voll und ganz zu")
app$click("btn3_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("GMS"))
expect_equal(
  results[["GMS"]],
  list(
    q1 = "btn1_text",
    q19 = "btn2_text",
    q30 = "btn3_text",
    "Active Engagement" = 2,
    General = 2
  )
)

app$stop()
