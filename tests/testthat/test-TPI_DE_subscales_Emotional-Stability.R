context("TPI")
library(psychTestR)
library(psyquest)
library(testthat)

app <- AppTester$new("apps/TPI_DE_subscales_Emotional-Stability")

# Enter id
app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Im Folgenden findest du eine Reihe von Persönlichkeitseigenschaften, die mehr oder weniger stark auf dich zutreffen. Bitte kreuze für jede Aussage an, inwieweit sie auf dich zutrifft oder nicht. Du sollst jeweils eine Gruppe von Eigenschaften beurteilen, auch wenn möglicherweise eine Eigenschaft stärker zutrifft als die anderen. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 2 Ich sehe mich selbst als: ängstlich, leicht aus der Fassung zu bringen Trifft überhaupt nicht zu Trifft größtenteils nicht zu Trifft eher nicht zu Weder zutreffend noch unzutreffend Trifft eher zu Trifft größtenteils zu Trifft voll und ganz zu")
app$click("btn4_text")

app$expect_ui_text("Frage 2 von 2 Ich sehe mich selbst als: ruhig, gelassen, emotional stabil Trifft überhaupt nicht zu Trifft größtenteils nicht zu Trifft eher nicht zu Weder zutreffend noch unzutreffend Trifft eher zu Trifft größtenteils zu Trifft voll und ganz zu")
app$click("btn2_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("TPI"))
expect_equal(
  results[["TPI"]],
  list(
    q4 = "btn4_text",
    q9 = "btn2_text",
    `Emotional Stability` = 3
  )
)

app$stop()
