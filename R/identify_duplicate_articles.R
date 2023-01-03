# Adi Tantravahi

################################################################################ IDENTIFY DUPLICATES



identify_duplicate_articles <- function(df, text) {
  #require(stringdist)
  #df$text <- as.character(df$text)  # Ensure text column is character type
  duplicates <- rep(FALSE, nrow(df))  # Initialize output
  for (i in 1:(nrow(df) - 1)) {
    # Compare current row to all subsequent rows
    distances <- stringdist::stringdist(df$text[i], df$text[(i+1):nrow(df)], method = "jw")
    # If the distance between any pair of rows is less than 0.1, mark as duplicate
    df$duplicates[(i+1):nrow(df)] <- df$duplicates[(i+1):nrow(df)] | (distances < 0.1)
  }
  return(duplicates)
}
