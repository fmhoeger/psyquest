% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TPI.R
\name{TPI}
\alias{TPI}
\title{TPI}
\usage{
TPI(label = "TPI", dict = psyquest::psyquest_dict, subscales = c(), ...)
}
\arguments{
\item{label}{(Character scalar) Three uppercase letter acronym of the questionnaire.
This is also the label given to the results in the output file.}

\item{dict}{(i18n_dict) The psyquest dictionary used for internationalisation.}

\item{subscales}{(Character vector) The subscales to be included in the questionnaire.
Possible subscales are \code{"Agreeableness"}, \code{"Conscientiousness"}, \code{"Emotional Stability"}, \code{"Extraversion"}, and \code{"Openness to Experiences"}.
If no subscales are provided all subscales are selected.}

\item{...}{Further arguments to be passed to \code{\link{TPI}()}.}
}
\description{
This function defines a TPI module for incorporation into a
psychTestR timeline.
Use this function if you want to include the TPI in a
battery of other tests, or if you want to add custom psychTestR
pages to your test timeline.
For a standalone implementation of the TPI,
consider using \code{\link{TPI_standalone}()}.
}
