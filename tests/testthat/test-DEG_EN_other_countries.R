context("DEG")
library(psychTestR)
library(psyquest)
library(testthat)

app <- AppTester$new("apps/DEG_EN")

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

app$expect_ui_text("What is your nationality? UK USA Bulgaria China Cuba Dominican Republic El Salvador France Germany Guatemala India Ireland Italy Lithuania Mexico Netherlands Nigeria Pakistan Philippines Poland Portugal Romania Russia South Africa South Korea Spain Vietnam Other nationality Continue")
app$setInputs(dropdown = "VN")
app$click_next()

app$expect_ui_text("In which country have you spent most years of your life? UK USA Bulgaria China Cuba Dominican Republic El Salvador France Germany Guatemala India Ireland Italy Lithuania Mexico Netherlands Nigeria Pakistan Philippines Poland Portugal Romania Russia South Africa South Korea Spain Vietnam Other country Continue")
app$setInputs(dropdown = "OTHER")
app$click_next()

app$expect_ui_text("What is your first language? English Arabic Bulgarian Chinese Dutch French German Haitian Hindi Italian Korean Lithuanian Polish Portuguese Punjabi Romanian Russian Spanish Tagalog Vietnamese Other language Continue")
app$setInputs(dropdown = "ar")
app$click_next()

app$expect_ui_text("What is your second language that is also spoken at home (if you have one)? None English Arabic Bulgarian Chinese Dutch French German Haitian Hindi Italian Korean Lithuanian Polish Portuguese Punjabi Romanian Russian Spanish Tagalog Vietnamese Other language Continue")
app$setInputs(dropdown = "vi")
app$click_next()

app$expect_ui_text("When were you born? Month Select month, please! January February March April May June July August September October November December Year Select year, please! 1930 1931 1932 1933 1934 1935 1936 1937 1938 1939 1940 1941 1942 1943 1944 1945 1946 1947 1948 1949 1950 1951 1952 1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 Continue")
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
    q5 = "VN",
    q6 = "OTHER",
    q7 = "ar",
    q8 = "vi",
    q9 = c("2", "1999"),
    q10 = "btn1_text",
    q11 = "btn2_text",
    'Best Shot' = 1,
    'Hearing Impairment' = 1,
    'Type of Hearing Impairment' = "Tinnitus",
    Gender = 1,
    Age = 256,
    Nationality = "VN",
    'Country Formative Years' = "OTHER",
    'First Language' = "ar",
    'Second Language' = "vi",
    Handedness = c(1, 2)
  )
)

app$stop()
