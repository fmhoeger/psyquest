context("GMS")
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
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn6_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn6_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn6_text")
app$click("btn7_text")
app$click("btn1_text")
app$expect_ui_text("Frage 23 von 29 Das Instrument (einschließlich Gesang), welches ich am besten spiele ist _ . Ich spiele kein Instrument. Gesang Klavier Gitarre Schlagzeug Xylophon Flöte Oboe Klarinette Fagott Trompete Posaune Tuba Saxophon Horn Violine Cello Bratsche Kontrabass Harfe andere")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn6_text")
app$click("btn7_text")
app$click("btn1_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("GMS"))
expect_equal(
  results[["GMS"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    q6 = "btn5_text",
    q8 = "btn6_text",
    q10 = "btn7_text",
    q11 = "btn1_text",
    q12 = "btn2_text",
    q13 = "btn3_text",
    q14 = "btn4_text",
    q16 = "btn5_text",
    q19 = "btn6_text",
    q20 = "btn7_text",
    q22 = "btn1_text",
    q23 = "btn2_text",
    q24 = "btn3_text",
    q26 = "btn4_text",
    q27 = "btn5_text",
    q28 = "btn6_text",
    q29 = "btn7_text",
    q30 = "btn1_text",
    q32 = "btn2_text",
    q33 = "btn3_text",
    q35 = "btn4_text",
    q36 = "btn5_text",
    q39 = "btn6_text",
    q40 = "btn7_text",
    q41 = "btn1_text",
    "Active Engagement" = 3.3333333,
    General = 2.8666667,
    "Musical Training" = 2.4,
    Emotions = 4.2,
    "Perceptual Abilities" = 4,
    "Singing Abilities" = 3.5,
    Instrument = 2,
    "Start Age" = 7,
    "Absolute Pitch" = 1
  )
)

app$stop()
