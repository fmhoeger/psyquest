context("GDS")
library(psychTestR)

app <- AppTester$new("apps/GDS_fullscale_DE")

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Frage 1 von 26 Ich finde es leicht neue Bewegungen zu erlernen. Stimme ganz und gar nicht zu Stimme nicht zu Stimme eher nicht zu Weder noch Stimme eher zu Stimme zu Stimme voll und ganz zu")
app$click("btn1_text")
app$expect_ui_text("Frage 2 von 26 Ich habe das Gefühl als hätte ich zwei linke Füße. Stimme ganz und gar nicht zu Stimme nicht zu Stimme eher nicht zu Weder noch Stimme eher zu Stimme zu Stimme voll und ganz zu")
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
app$expect_ui_text("Frage 15 von 26 Wenn ich einen tolles Musikstück höre, muss ich einfach anfangen zu tanzen. Stimme ganz und gar nicht zu Stimme nicht zu Stimme eher nicht zu Weder noch Stimme eher zu Stimme zu Stimme voll und ganz zu")
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
app$expect_ui_text("Frage 25 von 26 Ich weiß viel über Tanz und Choreografie. Stimme ganz und gar nicht zu Stimme nicht zu Stimme eher nicht zu Weder noch Stimme eher zu Stimme zu Stimme voll und ganz zu")
app$click("btn4_text")
app$expect_ui_text("Frage 26 von 26 Ich bin gewillt zu reisen, um eine Tanzaufführung zu sehen oder an Tanzstunden teilzunehmen. Stimme ganz und gar nicht zu Stimme nicht zu Stimme eher nicht zu Weder noch Stimme eher zu Stimme zu Stimme voll und ganz zu")
app$click("btn5_text")
app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("GDS"))
expect_equal(
  results[["GDS"]],
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
    "Body Awareness" = 4.166667,
    General = 4.15,
    "Social Dancing" = 4.666667,
    "Urge to Dance" = 2.6,
    "Dance Training" = 5.666667,
    "Observational Dance Experience" = 5
  )
)

app$stop()
