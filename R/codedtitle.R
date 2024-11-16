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


codevar <- function(data, columns = NULL) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input data must be a dataframe.")
  }

  # Validate and resolve column selection
  if (is.null(columns)) {
    columns <- seq_along(data)  # Default to all columns
  } else {
    # Ensure specified columns are valid indices
    if (!all(columns %in% seq_along(data))) {
      stop("Some specified columns are not valid indices in the dataframe.")
    }
  }

  # Load required packages
  if (!requireNamespace("tm", quietly = TRUE)) install.packages("tm")
  if (!requireNamespace("SnowballC", quietly = TRUE)) install.packages("SnowballC")
  library(tm)
  library(SnowballC)

  # Function to process and shorten column names using NLP
  transform_name <- function(name) {
    # Convert to lowercase
    name <- tolower(name)

    # Create a text corpus
    corpus <- VCorpus(VectorSource(name))

    # Text cleaning
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, stemDocument, language = "english")

    # Extract the processed text
    processed_name <- sapply(corpus, as.character)

    # Split into words
    words <- unlist(strsplit(processed_name, "\\s+"))

    # If no words left after processing, fallback to abbreviation
    if (length(words) == 0 || all(words == "")) {
      words <- substr(gsub("\\s+", "", name), 1, 3)
    }

    # Combine words to form the new name
    new_name <- paste(words, collapse = "_")

    # Ensure syntactic validity
    new_name <- make.names(new_name)

    return(new_name)
  }

  # Process selected columns
  original_names <- colnames(data)
  new_names <- original_names  # Keep original names for unselected columns
  selected_names <- original_names[columns]
  transformed_names <- sapply(selected_names, transform_name)

  # Replace selected columns' names with transformed names
  new_names[columns] <- transformed_names

  # Ensure uniqueness of new names after transformation
  new_names <- make.names(new_names, unique = TRUE)

  # Create a reference dataframe
  coderef <- data.frame(Original = original_names, Coded = new_names, stringsAsFactors = FALSE)

  # Set new column names to the dataframe
  colnames(data) <- new_names

  # Return the modified dataframe and the reference
  return(list(data = data, coderef = coderef))
}

# Example usage:
getwd()
library(here)
data <- read.csv(here("data", "covariates.csv"))
data <- read.csv(here("data", "df.csv"))
result <- codevar(data)
new_data <- result$data
new_data
coderef <- result$coderef

