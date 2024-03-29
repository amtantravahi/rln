# Adi Tantravahi

################################################################################ TRANSFORM BATCH NEWS

lexis_transform_batchnews <- function(data, orig_data) {

  `%>%` <- magrittr::`%>%`

  # Call the pager function to create a dataset named result_clean. Once created,
  # it collapses the keywords extracted into a single string variable
  result_clean <- do.call(rbind, lapply(data, pager)) %>%
    group_by_at(vars(-keyword_ln)) %>%
    summarize(keyword_ln = toString(keyword_ln)) %>%
    dplyr::ungroup() %>%
    dplyr::select(names(.))

  # Create a dummy variable for articles tagged as negative news by Lexis Nexis
  # and languages
  # need to get full list of Lexis languages
  result_clean <- result_clean %>%
    mutate(.,
           neg_news = as.integer(stringr::str_detect(keyword_ln,"NEGATIVE NEWS")),
           french =  as.integer(stringr::str_detect(keyword_ln,"FRENCH")),
           english =  as.integer(stringr::str_detect(keyword_ln,"ENGLISH|English")),
           spanish =  as.integer(stringr::str_detect(keyword_ln,"SPANISH")),
           italian =  as.integer(stringr::str_detect(keyword_ln,"ITALIAN|Italiano|Italian")),
           german =  as.integer(stringr::str_detect(keyword_ln,"GERMAN")),
           dutch =  as.integer(stringr::str_detect(keyword_ln,"DUTCH")),
           polish =  as.integer(stringr::str_detect(keyword_ln," POLISH|POLSKI"))

    )

  # Extract article language from dummy variables into a string
  result_clean <- result_clean %>%
    mutate(language = case_when(french == 1 ~ "French",
                                english == 1 ~ "English",
                                spanish == 1 ~ "Spanish",
                                german == 1 ~ "German",
                                italian == 1 ~ "Italian",
                                dutch == 1 ~ "Dutch",
                                polish == 1 ~ "Polish",
                                TRUE ~ ""))

  # Convert pub_date to date type
  result_clean$pub_date <- stringr::str_extract(result_clean$pub_date,
                                                "[0-9]{4}[.-][0-9]{2}[.-][0-9]{2}")
  result_clean$pub_date <- lubridate::ymd(result_clean$pub_date)


  # Extract the following words from the keyword variable
  # Create new keyword list
  wordstoremove <- c("Newspaper,", "Newspapers,", "ENGLISH,", "English,", "GERMAN,", "SPANISH,", "ITALIAN,",
                     "Web Blog,", "Blogs,", "Newswire,", "Newswires & Press Releases,", "Transcript,",
                     "News Transcripts,", "English US,", "FRENCH,", "FRANÇAIS,", "Dépêche,",
                     "Agencia de información,", "ESPAÑOL,", "Journal,", "Magazines & Journals,",
                     "Web-based Publications,", "Web Publication,", "Publication internet,",
                     "Industry Trade Press,", "Zeitung,", "DEUTSCH,", "NEWSPAPER,",
                     "COLUMN,", "Magazine,", "NEGATIVE PERSONAL NEWS,", "  NEGATIVE MISC NEWS,",
                     "NEGATIVE NEWS,", "Newspaper;", "BLOGS & MESSAGE BOARDS,", "NEWSPAPER,",
                     "Newsletter,", "Newsletters,", "Giornale,", "ITALIANO,", "Agenzia Stampa,",
                     "NEWS BRIEFS,", "News,", "General News,", "DUTCH,", "Periódico,", "Italian / Italiano,",
                     "Polish,", "POLSKI,", "Country & Region Reports,"
  )

  result_clean$keyword <- sapply(result_clean$keyword_ln, function(x)
    gsub(paste(wordstoremove, collapse = '|'), '', x))

  # Remove HTML tags from main_text
  result_clean$main_text <- htm2txt::htm2txt(result_clean$main_text)

  ## We'll now clean the original result data before merging with result_clean
  drop_result <- c("Document.Citation", "Source.Id", "Source.ContentType")
  result <- orig_data[,!(names(orig_data) %in% drop_result)]
  rm(drop_result)

  result <- result %>%
    rename(pagination_num = column_label,
           lexis_id = ResultId,
           documentid_lexis = Document.DocumentId,
           documentid_type_lexis = Document.DocumentIdType,
           documentcontent_lexis = Document.Content,
           sourcename_lexis = Source.Name
    )

  result <- result %>%
    dplyr::select(lexis_id, documentid_lexis, documentid_type_lexis,
                  documentcontent_lexis, sourcename_lexis, everything())


  # Merge data together to preserve original API output while keeping cleaned data
  final_result <<- dplyr::left_join(result_clean, result, by = "lexis_id")

  # Mark duplicate titles
  final_result$title_lower <<- tolower(final_result$title)
  final_result$title_lower <<- trimws(final_result$title_lower)
  final_result$title_duplicate <<- duplicated(final_result$title_lower, fromLast = F)
  final_result <<- subset(final_result, select = -c(title_lower))
  final_result <<- final_result %>%
    dplyr::select(title, title_duplicate, everything())

  # Rearange vars
  #result <- result %>%
  #  select(title, title_duplicate, everything())


}
