library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/GMS_short_DE", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Frage 1 von 29 Ich beschäftige mich in meiner Freizeit viel mit musikbezogenen Aktivitäten. Stimme ganz und gar nicht zu Stimme nicht zu Stimme eher nicht zu Weder noch Stimme eher zu Stimme zu Stimme voll und ganz zu")
app$click("btn1_text")
app$expect_ui_text("Frage 2 von 29 Ich habe regelmäßig und täglich ein Instrument (einschließlich Gesang) für _ Jahre geübt. 0 1 2 3 4-5 6-9 10 oder mehr Jahre")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")
app$expect_ui_text("Frage 23 von 29 Das Instrument (einschließlich Gesang), welches ich am besten spiele ist _ . Ich spiele kein Instrument. Gesang Klavier Gitarre Schlagzeug Xylophon Flöte Oboe Klarinette Fagott Trompete Posaune Tuba Saxophon Horn Violine Cello Bratsche Kontrabass Harfe andere")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn19_text")
app$click("btn2_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("GMS"))
expect_equal(
  results[["GMS"]],
  list(
    q1 = "btn1_text",
    q2 = "btn1_text",
    q3 = "btn1_text",
    q4 = "btn1_text",
    q6 = "btn1_text",
    q8 = "btn1_text",
    q10 = "btn7_text",
    q11 = "btn1_text",
    q12 = "btn1_text",
    q13 = "btn7_text",
    q14 = "btn1_text",
    q16 = "btn7_text",
    q19 = "btn1_text",
    q20 = "btn1_text",
    q22 = "btn1_text",
    q23 = "btn1_text",
    q24 = "btn1_text",
    q26 = "btn1_text",
    q27 = "btn7_text",
    q28 = "btn1_text",
    q29 = "btn7_text",
    q30 = "btn1_text",
    q32 = "btn1_text",
    q33 = "btn7_text",
    q35 = "btn7_text",
    q36 = "btn1_text",
    q39 = "btn1_text",
    q40 = "btn19_text",
    q41 = "btn2_text",
    "Active Engagement" = 1,
    General = 1,
    "Musical Training" = 1,
    Emotions = 1,
    "Perceptual Abilities" = 1,
    "Singing Abilities" = 1,
    Instrument = 1,
    "Start Age" = NA,
    "Absolute Pitch" = 2
  )
)

app$stop()
