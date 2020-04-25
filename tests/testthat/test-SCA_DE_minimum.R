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
app$click("btn1_text")

app$expect_ui_text("Frage 2 von 25 Ich fühle mich manchmal unvorbereitet für den Unterricht. Stimme überhaupt nicht zu Stimme nicht zu Stimme zu Stimme sehr zu")
app$click("btn4_text")
# I frequently feel unprepared for class
app$click("btn1_text")
# I am good at mathematics
app$click("btn4_text")
# Learning is difficult for me
app$click("btn1_text")
# I usually do well on tests
app$click("btn1_text")
# I am proud of my school work
app$click("btn1_text")
# I can spell better than most people my age
app$click("btn1_text")
# I read as well as most people my age
app$click("btn4_text")
# I don't think very quickly
app$click("btn1_text")
# I work harder than most of my classmates
app$click("btn4_text")
# I don't understand much of my classmates
app$click("btn1_text")
# I learn fairly easy
app$click("btn4_text")
# I never seem to have good ideas
app$click("btn1_text")
# My teachers like my classroom behaviour
app$click("btn4_text")
# I often feel dumb
app$click("btn1_text")
# Most of my teachers seem to like me
app$click("btn4_text")
# I have poor study habits
app$click("btn1_text")
# Science is easy for me
app$click("btn4_text")
# I am uncomfortable in school
app$click("btn1_text")
# I usually work very hard
app$click("btn1_text")
# Most people would rather work with me than someone else
app$click("btn4_text")
# My teachers have a low opinion of me
app$click("btn1_text")
# Most subjects are pretty easy for me
app$click("btn4_text")
# I am not very creative
app$click("btn1_text")
# I usually feel good about my written work

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("SCA"))
expect_equal(
  results[["SCA"]],
  list(
    q1 = "btn1_text",
    q3 = "btn4_text",
    q4 = "btn1_text",
    q5 = "btn4_text",
    q6 = "btn1_text",
    q7 = "btn1_text",
    q8 = "btn1_text",
    q9 = "btn1_text",
    q10 = "btn4_text",
    q11 = "btn1_text",
    q13 = "btn4_text",
    q14 = "btn1_text",
    q15 = "btn4_text",
    q16 = "btn1_text",
    q17 = "btn4_text",
    q18 = "btn1_text",
    q19 = "btn4_text",
    q20 = "btn1_text",
    q21 = "btn4_text",
    q22 = "btn1_text",
    q23 = "btn1_text",
    q24 = "btn4_text",
    q26 = "btn1_text",
    q27 = "btn4_text",
    q28 = "btn1_text",
    General = 45
  )
)

app$stop()
