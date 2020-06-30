context("MHE")
library(psychTestR)

app <- AppTester$new("apps/MHE_DE")

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Bitte gib für die folgenden Familienmitglieder an, welche musikalischen Aktivitäten sie in letzter Zeit ausgeführt haben. (Klicke alles Zutreffende an oder keine der Boxen, falls nichts zutrifft.) Meine Mutter … spielt ein Instrument. … singt in einem Chor. Weiter")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
cb <- app$findElement(css = "input[value=choice2]")
cb$click()
app$click_next()

app$expect_ui_text("Bitte gib für die folgenden Familienmitglieder an, welche musikalischen Aktivitäten sie in letzter Zeit ausgeführt haben. (Klicke alles Zutreffende an oder keine der Boxen, falls nichts zutrifft.) Mein Vater … spielt ein Instrument. … singt in einem Chor. Weiter")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
cb <- app$findElement(css = "input[value=choice2]")
cb$click()
app$click_next()

app$expect_ui_text("Wie oft unternehmen deine Eltern oder jemand anderes mit dir zu Hause, während der Ferien oder am Wochenende die folgenden Aktivitäten? Helfen mir bei meinen Hausaufgaben Täglich oder fast jeden Tag Einmal oder zweimal pro Woche Einmal oder zweimal pro Monat Selten oder nie Weiter")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
app$click_next()

app$expect_ui_text("Wie oft unternehmen deine Eltern oder jemand anderes mit dir zu Hause, während der Ferien oder am Wochenende die folgenden Aktivitäten? Reden mit mir über ein Thema, das wir in der Schule gelernt haben Täglich oder fast jeden Tag Einmal oder zweimal pro Woche Einmal oder zweimal pro Monat Selten oder nie Weiter")
cb <- app$findElement(css = "input[value=choice2]")
cb$click()
app$click_next()

app$expect_ui_text("Wie oft unternehmen deine Eltern oder jemand anderes mit dir zu Hause, während der Ferien oder am Wochenende die folgenden Aktivitäten? Reden über Probleme in der Schule Täglich oder fast jeden Tag Einmal oder zweimal pro Woche Einmal oder zweimal pro Monat Selten oder nie Weiter")
cb <- app$findElement(css = "input[value=choice3]")
cb$click()
app$click_next()

app$expect_ui_text("Wie oft unternehmen deine Eltern oder jemand anderes mit dir zu Hause, während der Ferien oder am Wochenende die folgenden Aktivitäten? Motivieren mich, Musik zu machen Täglich oder fast jeden Tag Einmal oder zweimal pro Woche Einmal oder zweimal pro Monat Selten oder nie Weiter")
cb <- app$findElement(css = "input[value=choice4]")
cb$click()
app$click_next()

app$expect_ui_text("Wie oft unternehmen deine Eltern oder jemand anderes mit dir zu Hause, während der Ferien oder am Wochenende die folgenden Aktivitäten? Helfen mir, mein Instrument zu üben Täglich oder fast jeden Tag Einmal oder zweimal pro Woche Einmal oder zweimal pro Monat Selten oder nie Weiter")
cb <- app$findElement(css = "input[value=choice1]")
cb$click()
app$click_next()

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("MHE"))
expect_equal(
  results[["MHE"]],
  list(
    q1 = "choice1,choice2",
    q2 = "choice1,choice2",
    q3 = "choice1",
    q4 = "choice2",
    q5 = "choice3",
    q6 = "choice4",
    q7 = "choice1",
    General = 1.6964507
  )
)

app$stop()
