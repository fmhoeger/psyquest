library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/SCA_DE", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir, phantomTimeout = 5000)

# Enter id
app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Bitte gib an, inwieweit die folgenden Aussagen auf dich zutreffen. Es gibt keine richtigen oder falschen Antworten, aber es ist wichtig, dass du ehrlich bist. Sei ehrlich mit dir selber, wenn du über die jeweilige Aussage nachdenkst. Um deine Antwort zu markieren, klicke auf das Feld, das am besten mit deinen Gefühlen in Bezug zur Aussage übereinstimmt. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 25 Meine Klassenkameraden mögen meistens meine Ideen. Stimme überhaupt nicht zu Stimme nicht zu Stimme zu Stimme sehr zu")
app$click("btn4_text")

app$expect_ui_text("Frage 2 von 25 Ich fühle mich manchmal unvorbereitet für den Unterricht. Stimme überhaupt nicht zu Stimme nicht zu Stimme zu Stimme sehr zu")
app$click("btn1_text")
# I frequently feel unprepared for class
app$click("btn4_text")
# I am good at mathematics
app$click("btn1_text")
# Learning is difficult for me
app$click("btn4_text")
# I usually do well on tests
app$click("btn4_text")
# I am proud of my school work
app$click("btn4_text")
# I can spell better than most people my age
app$click("btn4_text")
# I read as well as most people my age
app$click("btn1_text")
# I don't think very quickly
app$click("btn4_text")
# I work harder than most of my classmates
app$click("btn1_text")
# I don't understand much of my classmates
app$click("btn4_text")
# I learn fairly easy
app$click("btn1_text")
# I never seem to have good ideas
app$click("btn4_text")
# My teachers like my classroom behaviour
app$click("btn1_text")
# I often feel dumb
app$click("btn4_text")
# Most of my teachers seem to like me
app$click("btn1_text")
# I have poor study habits
app$click("btn4_text")
# Science is easy for me
app$click("btn1_text")
# I am uncomfortable in school
app$click("btn4_text")
# I usually work very hard
app$click("btn4_text")
# Most people would rather work with me than someone else
app$click("btn1_text")
# My teachers have a low opinion of me
app$click("btn4_text")
# Most subjects are pretty easy for me
app$click("btn1_text")
# I am not very creative
app$click("btn4_text")
# I usually feel good about my written work

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("SCA"))
expect_equal(
  results[["SCA"]],
  list(
    q1 = "btn4_text",
    q3 = "btn1_text",
    q4 = "btn4_text",
    q5 = "btn1_text",
    q6 = "btn4_text",
    q7 = "btn4_text",
    q8 = "btn4_text",
    q9 = "btn4_text",
    q10 = "btn1_text",
    q11 = "btn4_text",
    q13 = "btn1_text",
    q14 = "btn4_text",
    q15 = "btn1_text",
    q16 = "btn4_text",
    q17 = "btn1_text",
    q18 = "btn4_text",
    q19 = "btn1_text",
    q20 = "btn4_text",
    q21 = "btn1_text",
    q22 = "btn4_text",
    q23 = "btn4_text",
    q24 = "btn1_text",
    q26 = "btn4_text",
    q27 = "btn1_text",
    q28 = "btn4_text",
    General = 145
  )
)

app$stop()
