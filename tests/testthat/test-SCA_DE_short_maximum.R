context("SCA")
library(psychTestR)

app <- AppTester$new("apps/SCA_short_DE")

# Enter id
app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Bitte gib an, inwieweit die folgenden Aussagen auf dich zutreffen. Es gibt keine richtigen oder falschen Antworten, aber es ist wichtig, dass du ehrlich bist. Sei ehrlich mit dir selber, wenn du über die jeweilige Aussage nachdenkst. Um deine Antwort zu markieren, klicke auf das Feld, das am besten mit deinen Gefühlen in Bezug zur Aussage übereinstimmt. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 4 In den meisten Schulfächern lerne ich schnell. Stimme überhaupt nicht zu Stimme nicht zu Stimme zu Stimme sehr zu")
app$click("btn4_text")

app$expect_ui_text("Frage 2 von 4 In den meisten Schulfächern schneide ich in Klassenarbeiten gut ab. Stimme überhaupt nicht zu Stimme nicht zu Stimme zu Stimme sehr zu")
app$click("btn4_text")

app$expect_ui_text("Frage 3 von 4 Ich bin in den meisten Schulfächern gut. Stimme überhaupt nicht zu Stimme nicht zu Stimme zu Stimme sehr zu")
app$click("btn4_text")

app$expect_ui_text("Frage 4 von 4 Arbeiten in den meisten Schulfächern ist einfach für mich. Stimme überhaupt nicht zu Stimme nicht zu Stimme zu Stimme sehr zu")
app$click("btn4_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("SCA"))
expect_equal(
  results[["SCA"]],
  list(
    q2 = "btn4_text",
    q12 = "btn4_text",
    q25 = "btn4_text",
    q29 = "btn4_text",
    Extra = 123
  )
)

app$stop()
