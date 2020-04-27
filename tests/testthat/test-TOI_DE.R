library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/TOI_DE", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

# Enter id
app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

# Intro
app$expect_ui_text("Lies die unten stehenden Sätze. In diesen Sätzen geht es um Intelligenz (Schlauheit/Klugheit). Gib an, inwieweit du den Sätzen zustimmst. Es gibt keine richtigen oder falschen Antworten. Weiter")
app$click_next()

app$expect_ui_text("Frage 1 von 7 Man hat einen bestimmten Grad an Intelligenz und kann nicht viel tun, um das zu ändern. Stimme sehr zu Stimme zu Stimme größtenteils zu Stimme größtenteils nicht zu Stimme nicht zu Stimme überhaupt nicht zu")
app$click("btn1_text")

app$expect_ui_text("Frage 2 von 7 Man kann viele neue Dinge lernen, aber man kann seine Intelligenz nicht wirklich ändern. Stimme sehr zu Stimme zu Stimme größtenteils zu Stimme größtenteils nicht zu Stimme nicht zu Stimme überhaupt nicht zu")
app$click("btn2_text")

app$expect_ui_text("Frage 3 von 7 Wenn ich weiß, dass ich eine Aufgabe nicht gut machen kann, mache ich sie eher nicht, auch wenn ich vielleicht dabei viel lernen würde. Stimme sehr zu Stimme zu Stimme größtenteils zu Stimme größtenteils nicht zu Stimme nicht zu Stimme überhaupt nicht zu")
app$click("btn3_text")
app$click("btn4_text")
app$click("btn5_text")
app$click("btn6_text")
app$click("btn1_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("TOI"))
expect_equal(
  results[["TOI"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    q5 = "btn5_text",
    q6 = "btn6_text",
    q7 = "btn1_text",
    `Theory of Inteligence` = 4.6666667,
    `Goals Choice` = 3.25
  )
)

app$stop()
