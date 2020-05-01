library(psychTestR)
library(psyquest)
library(testthat)

dir <-
  system.file("tests/DEG_EN", package = "psyquest", mustWork = TRUE)
app <- AppTester$new(dir)

app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("We are interested in getting the most accurate information possible from you today. Are you able to give your best shot with the following tests and questions today? Yes No")
app$click("btn1_text")

app$expect_ui_text("Do you have any kind of hearing impairment? (I.e., do you have any problems with your hearing?) Yes No")
app$click("btn1_text")

app$expect_ui_text("If so, please tell us what kind of hearing impairment you have. Continue")
app$setInputs(text_input = "Tinnitus")
app$click_next()

app$expect_ui_text("What is your gender? female male other rather not say")
app$click("btn1_text")

app$expect_ui_text("What is your nationality? United Kingdom USA Bulgaria China Cuba Dominican Republic El Salvador France Germany Guatemala India Ireland Italy Lithuania Mexico Netherlands Nigeria Pakistan Philippines Poland Portugal Romania Russia South Africa South Korea Spain Vietnam Other nationality Continue")
app$click_next()

app$expect_ui_text("In which country have you spent most years of your life? United Kingdom USA Bulgaria China Cuba Dominican Republic El Salvador France Germany Guatemala India Ireland Italy Lithuania Mexico Netherlands Nigeria Pakistan Philippines Poland Portugal Romania Russia South Africa South Korea Spain Vietnam Other country Continue")
app$click_next()

app$expect_ui_text("What is your first language? English Albanian Arabic Bulgarian Chinese Farsi/Dari German Greek Italian Kurdish Pashto Polish Romanian Russian Serbian Turkish Other language Continue")
app$setInputs(dropdown = "AR")
app$click_next()

app$expect_ui_text("What is your second language that is also spoken at home (if you have one)? None English Albanian Arabic Bulgarian Chinese Farsi/Dari German Greek Italian Kurdish Pashto Polish Romanian Russian Serbian Turkish Other language Continue")
app$setInputs(dropdown = "TR")
app$click_next()

app$expect_ui_text("When were you born? Month Select month, please! January February March April May June July August September October November December Year Select year, please! 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 Continue")
app$setInputs(month = "2")
app$setInputs(year = "1999")
app$click_next()

app$expect_ui_text("Are you right-handed left-handed ambidextrous (use both hands equally)")
app$click("btn1_text")

app$expect_ui_text("Which hand do you normally use for writing? the right hand the left hand both hands equally")
app$click("btn2_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("DEG"))
expect_equal(
  results[["DEG"]],
  list(
    q1 = "btn1_text",
    q2 = "btn1_text",
    q3 = "Tinnitus",
    q4 = "btn1_text",
    q5 = "UK",
    q6 = "UK",
    q7 = "AR",
    q8 = "TR",
    q9 = c("2", "1999"),
    q10 = "btn1_text",
    q11 = "btn2_text",
    'Best Shot' = 1,
    'Hearing Impairment' = 1,
    'Type of Hearing Impairment' = "Tinnitus",
    Gender = 1,
    Age = 255,
    Nationality = "UK",
    'Country Formative Years' = "UK",
    'First Language' = "ar",
    'Second Language' = "tr",
    Handedness = c(1, 2)
  )
)

app$stop()
