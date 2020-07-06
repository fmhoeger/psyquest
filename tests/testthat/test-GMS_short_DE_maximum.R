context("GMS")
library(psychTestR)

app <- AppTester$new("apps/GMS_short_DE")

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Frage 1 von 29 Ich beschäftige mich in meiner Freizeit viel mit musikbezogenen Aktivitäten. Stimme ganz und gar nicht zu Stimme nicht zu Stimme eher nicht zu Weder noch Stimme eher zu Stimme zu Stimme voll und ganz zu")
app$click("btn7_text")
app$expect_ui_text("Frage 2 von 29 Ich habe regelmäßig und täglich ein Instrument (einschließlich Gesang) für _ Jahre geübt. 0 1 2 3 4-5 6-9 10 oder mehr Jahre")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$expect_ui_text("Frage 23 von 29 Das Instrument (einschließlich Gesang), welches ich am besten spiele ist _ . Ich spiele kein Instrument. Gesang Klavier Gitarre Schlagzeug Xylophon Flöte Oboe Klarinette Fagott Trompete Posaune Tuba Saxophon Horn Violine Cello Bratsche Kontrabass Harfe andere")
app$click("btn15_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn1_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("GMS"))
expect_equal(
  results[["GMS"]],
  list(
    q1 = "btn7_text",
    q2 = "btn7_text",
    q3 = "btn7_text",
    q4 = "btn7_text",
    q6 = "btn7_text",
    q8 = "btn7_text",
    q10 = "btn1_text",
    q11 = "btn7_text",
    q12 = "btn7_text",
    q13 = "btn1_text",
    q14 = "btn7_text",
    q16 = "btn1_text",
    q19 = "btn7_text",
    q20 = "btn7_text",
    q22 = "btn7_text",
    q23 = "btn7_text",
    q24 = "btn7_text",
    q26 = "btn7_text",
    q27 = "btn1_text",
    q28 = "btn7_text",
    q29 = "btn1_text",
    q30 = "btn7_text",
    q32 = "btn15_text",
    q33 = "btn1_text",
    q35 = "btn1_text",
    q36 = "btn7_text",
    q39 = "btn7_text",
    q40 = "btn1_text",
    q41 = "btn1_text",
    "Active Engagement" = 7,
    General = 7,
    "Musical Training" = 7,
    Emotions = 7,
    "Perceptual Abilities" = 7,
    "Singing Abilities" = 7,
    Instrument = 15,
    "Start Age" = 1,
    "Absolute Pitch" = 1
  )
)

app$stop()
