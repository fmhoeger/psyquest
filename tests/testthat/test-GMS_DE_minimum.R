context("GMS")
library(psychTestR)

app <- AppTester$new("apps/GMS_DE")

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Frage 1 von 41 Ich beschäftige mich in meiner Freizeit viel mit musikbezogenen Aktivitäten. Stimme ganz und gar nicht zu Stimme nicht zu Stimme eher nicht zu Weder noch Stimme eher zu Stimme zu Stimme voll und ganz zu")
app$click("btn1_text")

app$expect_ui_text("Frage 2 von 41 Ich habe regelmäßig und täglich ein Instrument (einschließlich Gesang) für _ Jahre geübt. 0 1 2 3 4-5 6-9 10 oder mehr Jahre")
app$click("btn1_text")

app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")

# 12 An dem Höhepunkt meines Interesses habe ich mein Hauptinstrument _ Stunden pro Tag geübt.
app$click("btn1_text")

app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn7_text")

# 17 Ich habe _ Live-Events oder Konzerte als Zuschauer innerhalb der letzten 12 Monate besucht.
app$click("btn1_text")
# 18 Ich habe _ Jahre Unterricht in Musiktheorie (außerhalb der Schule) erhalten.
app$click("btn1_text")

app$click("btn1_text")
app$click("btn1_text")

# 21 Ich habe _ Jahre Musikunterricht auf einem Instrument (einschließlich Gesang) in meinem bisherigen Leben gehabt.
app$click("btn1_text")
# 22 Ich kann _ verschiedene Instrumente spielen.
app$click("btn1_text")

app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")

# 31 Ich höre jeden Tag aufmerksam Musik für
app$click("btn1_text")
app$expect_ui_text("Frage 32 von 41 Das Instrument (einschließlich Gesang), welches ich am besten spiele ist _ . Ich spiele kein Instrument. Gesang Klavier Gitarre Schlagzeug Xylophon Flöte Oboe Klarinette Fagott Trompete Posaune Tuba Saxophon Horn Violine Cello Bratsche Kontrabass Harfe andere")
app$click("btn1_text")

app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")
app$click("btn1_text")

app$expect_ui_text("Frage 40 von 41 Mit wie viel Jahren hast du angefangen, ein Instrument zu spielen? 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 Ich spiele kein Instrument.")
app$click("btn19_text")
app$expect_ui_text("Frage 41 von 41 Hast du ein absolutes Gehör? Ein absolutes Gehör ist die Fähigkeit, einen Ton in Isolation ohne Bezugston zu erkennen und zu benennen, z. B. in der Lage zu sein, ein Fis zu erkennen, wenn jemand ein Fis auf dem Klavier spielt. Ja Nein")
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
    q5 = "btn1_text",
    q6 = "btn1_text",
    q7 = "btn1_text",
    q8 = "btn1_text",
    q9 = "btn1_text",
    q10 = "btn7_text",
    q11 = "btn1_text",

    q12 = "btn1_text",

    q13 = "btn7_text",
    q14 = "btn1_text",
    q15 = "btn7_text",
    q16 = "btn7_text",

    q17 = "btn1_text",
    q18 = "btn1_text",

    q19 = "btn1_text",
    q20 = "btn1_text",

    q21 = "btn1_text",
    q22 = "btn1_text",

    q23 = "btn1_text",
    q24 = "btn1_text",
    q25 = "btn1_text",
    q26 = "btn1_text",
    q27 = "btn7_text",
    q28 = "btn1_text",
    q29 = "btn7_text",
    q30 = "btn1_text",

    q31 = "btn1_text",
    q32 = "btn1_text",

    q33 = "btn7_text",
    q34 = "btn1_text",
    q35 = "btn7_text",
    q36 = "btn1_text",
    q37 = "btn1_text",
    q38 = "btn1_text",
    q39 = "btn1_text",

    q40 = "btn19_text",
    q41 = "btn2_text",
    "Active Engagement" = 1,
    General = 1,
    "Musical Training" = 1,
    Emotions = 1,
    "Singing Abilities" = 1,
    "Perceptual Abilities" = 1,
    Instrument = 1,
    "Start Age" = NA,
    "Absolute Pitch" = 2
  )
)

app$stop()
