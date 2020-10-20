# Changelog

## [1.2.0] Released on 2020-10-21

### New features

- Added questionnaire GDS

### Changes

- Switch Remote for psychTestR to pmcharrison/psychTestR
- Results for checkbox questions are now exported as a comma separated list of strings, e.g.: "[""choice1"",""choice3"",""choice4""]" where before the same was a string like "choice1,choice3,choice4". Update your scripts accordingly.
