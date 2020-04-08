library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/TPI_DE_subscales_Emotional-Stability_Extraversion", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir, phantomTimeout = 5000)

# Enter id
app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Im Folgenden findest du eine Reihe von Persönlichkeitseigenschaften, die mehr oder weniger stark auf dich zutreffen. Bitte kreuze für jede Aussage an, inwieweit sie auf dich zutrifft oder nicht. Du sollst jeweils eine Gruppe von Eigenschaften beurteilen, auch wenn möglicherweise eine Eigenschaft stärker zutrifft als die anderen. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 4 Ich sehe mich selbst als: begeistert, lebhaft, extrovertiert Trifft überhaupt nicht zu Trifft größtenteils nicht zu Trifft eher nicht zu Weder zutreffend noch unzutreffend Trifft eher zu Trifft größtenteils zu Trifft voll und ganz zu")
app$click("btn1_text")

app$click("btn4_text")
app$click("btn6_text")
app$click("btn2_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("TPI"))
expect_equal(
  results[["TPI"]],
  list(
    q1 = "btn1_text",
    q4 = "btn4_text",
    q6 = "btn6_text",
    q9 = "btn2_text",
    Extraversion = 1.5,
    `Emotional Stability` = 3
  )
)

app$stop()
