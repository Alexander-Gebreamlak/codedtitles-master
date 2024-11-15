#' @title Code Variable Names of Dataframe
#' @description Take a dataset and change column names to manageable
#' coded variables names based on a desired string-length. This function also
#' removes all spaces and makes all characters lower-case. A reference vector is
#' created (coderef) which display the transformed column names and the original
#' column name.
#' @param dataframe with named columns
#' @return dataframe with recoded column names
#' @author Mohsyn Imran Malik
#' @examples
#' codetitle(data, strlength = 3)
#' @export

codevar <- function(data, strlength, tag = NULL) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input data must be a dataframe.")
  }
  if (!is.numeric(strlength) || strlength <= 0) {
    stop("strlength must be a positive numeric value.")
  }

  # Simplify transformation of column names
  transform_name <- function(name) {
    new_name <- tolower(gsub(" ", "", substr(name, 1, strlength)))
    if (!is.null(tag)) {
      new_name <- paste0(new_name, tag)
    }
    return(new_name)
  }

  # Apply transformation and handle duplicates
  new_names <- sapply(colnames(data), transform_name)
  new_names <- make.unique(new_names)

  # Create coderef dataframe
  coderef <- data.frame(Original = colnames(data), Coded = new_names, stringsAsFactors = FALSE)

  # Set new column names to the dataframe
  colnames(data) <- new_names

  # Return modified dataframe and coderef
  return(list(data = data, coderef = coderef))
}


# Example usage:

getwd()
library(here)
data <- read.csv(here("data", "covariates.csv"))
result <- codevar(data, strlength = 3)
new_data <- result$data
coderef <- result$coderef
