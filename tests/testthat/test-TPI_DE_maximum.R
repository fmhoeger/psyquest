context("TPI")
library(psychTestR)

app <- AppTester$new("apps/TPI_DE")

# Enter id
app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Im Folgenden findest du eine Reihe von Persönlichkeitseigenschaften, die mehr oder weniger stark auf dich zutreffen. Bitte kreuze für jede Aussage an, inwieweit sie auf dich zutrifft oder nicht. Du sollst jeweils eine Gruppe von Eigenschaften beurteilen, auch wenn möglicherweise eine Eigenschaft stärker zutrifft als die anderen. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 10 Ich sehe mich selbst als: begeistert, lebhaft, extrovertiert Trifft überhaupt nicht zu Trifft größtenteils nicht zu Trifft eher nicht zu Weder zutreffend noch unzutreffend Trifft eher zu Trifft größtenteils zu Trifft voll und ganz zu")
app$click("btn7_text")

app$expect_ui_text("Frage 2 von 10 Ich sehe mich selbst als: kritisch, streitsüchtig Trifft überhaupt nicht zu Trifft größtenteils nicht zu Trifft eher nicht zu Weder zutreffend noch unzutreffend Trifft eher zu Trifft größtenteils zu Trifft voll und ganz zu")
app$click("btn1_text")

app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")
app$click("btn7_text")
app$click("btn1_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("TPI"))
expect_equal(
  results[["TPI"]],
  list(
    q1 = "btn7_text",
    q2 = "btn1_text",
    q3 = "btn7_text",
    q4 = "btn1_text",
    q5 = "btn7_text",
    q6 = "btn1_text",
    q7 = "btn7_text",
    q8 = "btn1_text",
    q9 = "btn7_text",
    q10 = "btn1_text",
    Extraversion = 7,
    Agreeableness = 7,
    Conscientiousness = 7,
    `Emotional Stability` = 7,
    `Openness to Experiences` = 7
  )
)

app$stop()
