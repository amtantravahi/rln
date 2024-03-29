# Adi Tantravahi

################################################################################ LEXIS_CORPUS_PROCESS

### Function to convert data into corpus and process
# data = dataframe
# text_var = text variable to be processed for analysis

lexis_corpus_process <- function(data, text_var) {

  `%>%` <- magrittr::`%>%`

  # Create a document id for analysis with the tm package
  data$doc_id <- 1:nrow(data)

  # Create a text column for analysis
  # Change depending on which text column you want to analyze (e.g., main_text, keywords, headline, etc.)
  data$text <- text_var

  # Arrange data for tm package so doc_id and text are first two variables
  data <- data %>%
    select(doc_id, text, everything())

  # Load stopwords
  #english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")
  english_stopwords <- RCurl::getURL("https://raw.githubusercontent.com/stopwords-iso/stopwords-en/master/stopwords-en.txt",  encoding = "UTF-8")
  english_stopwords <- read.csv(text = english_stopwords, header = F)
  english_stopwords <- english_stopwords$V1

  #spanish_stopwords <- RCurl::getURL("https://raw.githubusercontent.com/stopwords-iso/stopwords-es/master/stopwords-es.txt", encoding = "UTF-8")
  #spanish_stopwords <- read.csv(text = spanish_stopwords, skip = 11, header = F)
  #spanish_stopwords <- spanish_stopwords$V1

  #french_stopwords <- RCurl::getURL("https://raw.githubusercontent.com/stopwords-iso/stopwords-fr/master/stopwords-fr.txt", encoding = "UTF-8")
  #french_stopwords <- read.csv(text = french_stopwords, header = F)
  #french_stopwords <- french_stopwords$V1

  #german_stopwords <- RCurl::getURL("https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt", encoding = "UTF-8")
  #german_stopwords <- read.csv(text = german_stopwords, header = F)
  #german_stopwords <- german_stopwords$V1

  #italian_stopwords <- RCurl::getURL("https://raw.githubusercontent.com/stopwords-iso/stopwords-it/master/stopwords-it.txt", encoding = "UTF-8")
  #italian_stopwords <- read.csv(text = italian_stopwords, header = F)
  #italian_stopwords <- italian_stopwords$V1

  #dutch_stopwords <- RCurl::getURL("https://raw.githubusercontent.com/stopwords-iso/stopwords-nl/master/stopwords-nl.txt", encoding = "UTF-8")
  #dutch_stopwords <- read.csv(text = dutch_stopwords, header = F)
  #dutch_stopwords <- dutch_stopwords$V1

  # Create corpus object
  corpus <<- tm::Corpus(tm::DataframeSource(data),
                        readerControl = list(language = c("en", "es", "fr", "de", "it")))
  #meta(corpus)

  # Preprocessing chain
  processedCorpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
  processedCorpus <- tm::tm_map(processedCorpus, tm::removeWords, english_stopwords)
  #processedCorpus <- tm::tm_map(processedCorpus, tm::removeWords, spanish_stopwords)
  #processedCorpus <- tm::tm_map(processedCorpus, tm::removeWords, french_stopwords)
  #processedCorpus <- tm::tm_map(processedCorpus, tm::removeWords, german_stopwords)
  #processedCorpus <- tm::tm_map(processedCorpus, tm::removeWords, italian_stopwords)
  #processedCorpus <- tm::tm_map(processedCorpus, tm::removeWords, dutch_stopwords)
  #processedCorpus <- tm::tm_map(processedCorpus, tm::removePunctuation, preserve_intra_word_dashes = TRUE)
  processedCorpus <- tm::tm_map(processedCorpus, tm::removeNumbers)
  processedCorpus <- tm::tm_map(processedCorpus, tm::stemDocument, language = "en")
  #processedCorpus <- tm::tm_map(processedCorpus, tm::stemDocument, language = "es")
  #processedCorpus <- tm::tm_map(processedCorpus, tm::stemDocument, language = "fr")
  #processedCorpus <- tm::tm_map(processedCorpus, tm::stemDocument, language = "de")
  #processedCorpus <- tm::tm_map(processedCorpus, tm::stemDocument, language = "it")
  #processedCorpus <- tm::tm_map(processedCorpus, tm::stemDocument, language = "nl")
  processedCorpus <- tm::tm_map(processedCorpus, tm::stripWhitespace)

  processedCorpus <<- processedCorpus



}

