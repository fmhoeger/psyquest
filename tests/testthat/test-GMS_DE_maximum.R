context("GMS")
library(psychTestR)

app <- AppTester$new("apps/GMS_DE")

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Frage 1 von 41 Ich beschäftige mich in meiner Freizeit viel mit musikbezogenen Aktivitäten. Stimme ganz und gar nicht zu Stimme nicht zu Stimme eher nicht zu Weder noch Stimme eher zu Stimme zu Stimme voll und ganz zu")
app$click("btn7_text")

app$expect_ui_text("Frage 2 von 41 Ich habe regelmäßig und täglich ein Instrument (einschließlich Gesang) für _ Jahre geübt. 0 1 2 3 4-5 6-9 10 oder mehr Jahre")
app$click("btn7_text")

app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")

# 12 An dem Höhepunkt meines Interesses habe ich mein Hauptinstrument _ Stunden pro Tag geübt.
app$click("btn7_text")

app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn1_text")

# 17 Ich habe _ Live-Events oder Konzerte als Zuschauer innerhalb der letzten 12 Monate besucht.
app$click("btn7_text")
# 18 Ich habe _ Jahre Unterricht in Musiktheorie (außerhalb der Schule) erhalten.
app$click("btn7_text")

app$click("btn7_text")
app$click("btn7_text")

# 21 Ich habe _ Jahre Musikunterricht auf einem Instrument (einschließlich Gesang) in meinem bisherigen Leben gehabt.
app$click("btn7_text")
# 22 Ich kann _ verschiedene Instrumente spielen.
app$click("btn7_text")

app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")

# 31 Ich höre jeden Tag aufmerksam Musik für
app$click("btn7_text")
app$expect_ui_text("Frage 32 von 41 Das Instrument (einschließlich Gesang), welches ich am besten spiele ist _ . Ich spiele kein Instrument. Gesang Klavier Gitarre Schlagzeug Xylophon Flöte Oboe Klarinette Fagott Trompete Posaune Tuba Saxophon Horn Violine Cello Bratsche Kontrabass Harfe andere")
app$click("btn15_text")

app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")
app$click("btn7_text")

app$expect_ui_text("Frage 40 von 41 Mit wie viel Jahren hast du angefangen, ein Instrument zu spielen? 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 Ich spiele kein Instrument.")
app$click("btn1_text")
app$expect_ui_text("Frage 41 von 41 Hast du ein absolutes Gehör? Ein absolutes Gehör ist die Fähigkeit, einen Ton in Isolation ohne Bezugston zu erkennen und zu benennen, z. B. in der Lage zu sein, ein Fis zu erkennen, wenn jemand ein Fis auf dem Klavier spielt. Ja Nein")
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
    q5 = "btn7_text",
    q6 = "btn7_text",
    q7 = "btn7_text",
    q8 = "btn7_text",
    q9 = "btn7_text",
    q10 = "btn1_text",
    q11 = "btn7_text",

    q12 = "btn7_text",

    q13 = "btn1_text",
    q14 = "btn7_text",
    q15 = "btn1_text",
    q16 = "btn1_text",

    q17 = "btn7_text",
    q18 = "btn7_text",

    q19 = "btn7_text",
    q20 = "btn7_text",

    q21 = "btn7_text",
    q22 = "btn7_text",

    q23 = "btn7_text",
    q24 = "btn7_text",
    q25 = "btn7_text",
    q26 = "btn7_text",
    q27 = "btn1_text",
    q28 = "btn7_text",
    q29 = "btn1_text",
    q30 = "btn7_text",

    q31 = "btn7_text",
    q32 = "btn15_text",

    q33 = "btn1_text",
    q34 = "btn7_text",
    q35 = "btn1_text",
    q36 = "btn7_text",
    q37 = "btn7_text",
    q38 = "btn7_text",
    q39 = "btn7_text",

    q40 = "btn1_text",
    q41 = "btn1_text",
    "Active Engagement" = 7,
    General = 7,
    "Musical Training" = 7,
    Emotions = 7,
    "Singing Abilities" = 7,
    "Perceptual Abilities" = 7,
    Instrument = 15,
    "Start Age" = 1,
    "Absolute Pitch" = 1
  )
)

app$stop()
