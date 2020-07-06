context("GMS")
library(psychTestR)

app <- AppTester$new("apps/GMS_DE")

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Frage 1 von 41 Ich beschäftige mich in meiner Freizeit viel mit musikbezogenen Aktivitäten. Stimme ganz und gar nicht zu Stimme nicht zu Stimme eher nicht zu Weder noch Stimme eher zu Stimme zu Stimme voll und ganz zu")
app$click("btn1_text")
app$expect_ui_text("Frage 2 von 41 Ich habe regelmäßig und täglich ein Instrument (einschließlich Gesang) für _ Jahre geübt. 0 1 2 3 4-5 6-9 10 oder mehr Jahre")
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
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn6_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$expect_ui_text("Frage 32 von 41 Das Instrument (einschließlich Gesang), welches ich am besten spiele ist _ . Ich spiele kein Instrument. Gesang Klavier Gitarre Schlagzeug Xylophon Flöte Oboe Klarinette Fagott Trompete Posaune Tuba Saxophon Horn Violine Cello Bratsche Kontrabass Harfe andere")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn6_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn2_text")
app$click("btn3_text")
app$click("btn4_text")
app$expect_ui_text("Frage 40 von 41 Mit wie viel Jahren hast du angefangen, ein Instrument zu spielen? 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 Ich spiele kein Instrument.")
app$click("btn5_text")
app$expect_ui_text("Frage 41 von 41 Hast du ein absolutes Gehör? Ein absolutes Gehör ist die Fähigkeit, einen Ton in Isolation ohne Bezugston zu erkennen und zu benennen, z. B. in der Lage zu sein, ein Fis zu erkennen, wenn jemand ein Fis auf dem Klavier spielt. Ja Nein")
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
    q5 = "btn5_text",
    q6 = "btn6_text",
    q7 = "btn7_text",
    q8 = "btn1_text",
    q9 = "btn2_text",
    q10 = "btn3_text",
    q11 = "btn4_text",
    q12 = "btn5_text",
    q13 = "btn6_text",
    q14 = "btn7_text",
    q15 = "btn1_text",
    q16 = "btn2_text",
    q17 = "btn3_text",
    q18 = "btn4_text",
    q19 = "btn5_text",
    q20 = "btn6_text",
    q21 = "btn7_text",
    q22 = "btn1_text",
    q23 = "btn2_text",
    q24 = "btn3_text",
    q25 = "btn4_text",
    q26 = "btn5_text",
    q27 = "btn6_text",
    q28 = "btn7_text",
    q29 = "btn1_text",
    q30 = "btn2_text",
    q31 = "btn3_text",
    q32 = "btn4_text",
    q33 = "btn5_text",
    q34 = "btn6_text",
    q35 = "btn7_text",
    q36 = "btn1_text",
    q37 = "btn2_text",
    q38 = "btn3_text",
    q39 = "btn4_text",
    q40 = "btn5_text",
    q41 = "btn1_text",
    "Active Engagement" = 2.555555556,
    General = 3.444444444,
    "Musical Training" = 3.714285714,
    Emotions = 4.5,
    "Singing Abilities" = 2.857142857,
    "Perceptual Abilities" = 5.7777778,
    Instrument = 4,
    "Start Age" = 5,
    "Absolute Pitch" = 1
  )
)

app$stop()
