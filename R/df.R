#' Example simulated dataframe to test codedtitles package
#'
#' Sample data with multiple variables with names of varying lengths, spacing,
#' and capitalization. This dataframe also contains variables with similar names
#' which the function deals with by adding col# to the end of repeated named
#' variables
#'
#'
#' @docType data
#' @format A simulated data frame with 20 rows and 36 variables: {
#' Example variables names are included such as:
#'    "?@%#%" <- a value with all special characters,
#'    "Attack_Rate" and "Attack_Rate_Total" which share similar stems
#'    "Mort$Total%_" is a variable with a mix of meaningful word stems
#'      and special characters
#'
#' }
"df"
