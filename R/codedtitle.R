#' @title Code variable names of dataframe to syntactically valid names
#' @description Take a dataset in the form of a dataframe and change column names to manageable
#' and syntactically valid coded variables names. Used in conjunction with the 'tm' and
#' 'SnowballC' package, this package will create shortened and simple column names
#' using natural language processing to attempt to maintain variable meaning.
#' With a pre-specified max character length (default = 15), user can also decide
#' to shorten variables to a desired length.
#' A reference vector is created (coderef) which displays the transformed
#' column names and the original column name, as well as the class of data for
#' each column.
#'
#' @param dataframe with named columns
#' @return dataframe with recoded column names, and a reference dataframe including
#' the recoded column names, original column names and data class.
#' @author Mohsyn Imran Malik, Alex G, Felix H, Kabier I, Temoor T
#' @examples
#' codevar(data, max_length = 8, tag = "_fu")
#' @export



codevar <- function(data, max_length = 15, tag = NULL, split = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input data must be a dataframe.")
  }
  if (max_length < 1) {
    stop("max_length must be at least 1.")
  }

  if (!requireNamespace("tm", quietly = TRUE)) stop("Package 'tm' is required.")
  if (!requireNamespace("SnowballC", quietly = TRUE)) stop("Package 'SnowballC' is required.")

  library(tm)
  library(SnowballC)

  coderef <- data.frame(New = character(), Original = character(), Class = character(), stringsAsFactors = FALSE)
  namescol <- colnames(data)

  for (i in seq_along(namescol)) {
    name <- namescol[i]

    # Remove special characters
    name <- gsub('[^[:alnum:] ]', " ", name)

    # Split into words
    if (split == TRUE) {
      name <- unlist(strsplit(name, "\\s+"))
      if (is.null(name) || length(name) == 0 || all(name == "")) {
        name <- substr(gsub("\\s+", "", namescol[i]), 1, max_length)
      }
    }

    # Create and clean text corpus
    corpus <- VCorpus(VectorSource(name))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, stemDocument, language = "english")
    processed_name <- sapply(corpus, content)

    # Equal truncation of words
    total_words <- length(processed_name)
    if (total_words > 0) {
      char_per_word <- floor(max_length / total_words)
      leftover_chars <- max_length %% total_words

      truncated_words <- mapply(function(word, idx) {
        extra_char <- ifelse(idx <= leftover_chars, 1, 0)
        substr(word, 1, char_per_word + extra_char)
      }, processed_name, seq_along(processed_name))

      new_name <- paste(truncated_words, collapse = "_")
    } else {
      new_name <- substr(processed_name, 1, max_length)
    }

    new_name <- make.names(new_name)

    suffix <- 1
    base_name <- new_name
    while (new_name %in% coderef$New) {
      new_name <- paste0(base_name, "_", suffix)
      suffix <- suffix + 1
    }

    if (!is.null(tag)) {
      new_name <- paste0(new_name, tag)
    }

    coderef <- rbind(
      coderef,
      data.frame(
        New = new_name,
        Original = colnames(data)[i],
        Class = class(data[[i]]),
        stringsAsFactors = FALSE
      )
    )
  }
  assign("coderef", coderef, .GlobalEnv)
  print(coderef[, 1])
}


# Example usage:
getwd()
library(here)
data <- read.csv(here("data", "df.csv"))
codenames <- codevar(data)
codenames # Print new coded variable names
print(coderef) # Data frame containing old and new names, as well as data class

# Example 2 usage (Reducing length of characters for each variable):
new_names <- codevar(data, max_length = 6)

# Example 3 usage (Removing word splitting):
new_names <- codevar(data, max_length = 3)

# Example 4 usage (Repeat Variables):
coderef[c(25,26,35),]

# Example 5 usage (Add tag):
new_names <- codevar(data[,4:7], max_length = 3, split = FALSE, tag = "_bl")



