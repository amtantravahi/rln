# Adi Tantravahi


################################################################################ TRANSFORM
# This function extracts data stored in XML format within the 
# result$Document.Content variable we extracted from the API
pager <- function(x) {
  
  # Parse data as XML
  doc <- read_xml(x)
  
  # Parse as HTML for any HTML stored in the string
  doc_html <- read_html(x)
  
  # Extract data into a seperate dataframe
  data.frame(title = doc %>% 
               xml_find_one(xpath = "//nitf:hedline") %>% 
               html_text(),
             pub_date = doc_html %>% 
               html_node("published") %>% 
               html_text(),
             author = doc_html %>% 
               html_node("author") %>% 
               html_text(),
             dateline = doc %>% 
               xml_find_one(xpath = "//nitf:dateline") %>% 
               html_text(),
             main_text = doc %>% 
               xml_find_one(xpath = "//bodyText") %>% 
               html_text(),
             publisher = doc %>% 
               xml_find_one(xpath = "//publicationName") %>% 
               html_text(),
             pub_date_txt = doc %>% 
               xml_find_one(xpath = "//dateText") %>% 
               html_text(),
             copyright = doc %>% 
               xml_find_one(xpath ="//copyright") %>% 
               html_text(),
             lexis_id = doc_html %>% 
               html_node("id") %>% 
               html_text(),
             keyword_ln = doc %>% 
               xml_find_all("//className") %>% 
               xml_text()
  )
  
  
}

lexis_transform <- function(data, orig_data) {
  
  # Call the pager function to create a dataset named result_clean. Once created,
  # it collapses the keywords extracted into a single string variable
  result_clean <- do.call(rbind, lapply(data, pager)) %>% 
    group_by_at(vars(-keyword_ln)) %>%
    summarize(keyword_ln = toString(keyword_ln)) %>%
    ungroup() %>% 
    select(names(.))
  
  # Create a dummy variable for articles tagged as negative news by Lexis Nexis
  # and languages
  # need to get full list of Lexis languages
  result_clean <- result_clean %>% 
    mutate(.,
           neg_news = as.integer(str_detect(keyword_ln,"NEGATIVE NEWS")),
           french =  as.integer(str_detect(keyword_ln,"FRENCH")),
           english =  as.integer(str_detect(keyword_ln,"ENGLISH|English")),
           spanish =  as.integer(str_detect(keyword_ln,"SPANISH")),
           italian =  as.integer(str_detect(keyword_ln,"ITALIAN")),
           german =  as.integer(str_detect(keyword_ln,"GERMAN"))
    )
  
  # Extract article language from dummy variables into a string
  result_clean <- result_clean %>% 
    mutate(.,
           language = ifelse(french == 1, "French",
                             ifelse(english == 1, "English",
                                    ifelse(spanish == 1, "Spanish",
                                           ifelse(german == 1, "German",
                                                  ifelse(italian == 1, "Italian", "")))))
    )
  
  # Convert pub_date to date type
  result_clean$pub_date <- str_extract(result_clean$pub_date,
                                       "[0-9]{4}[.-][0-9]{2}[.-][0-9]{2}")
  result_clean$pub_date <- ymd(result_clean$pub_date)
  
  
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
                     "Newsletter,", "Newsletters,")
  
  result_clean$keyword <- sapply(result_clean$keyword_ln, function(x) 
    gsub(paste(wordstoremove, collapse = '|'), '', x))
  
  # Remove HTML tags from main_text
  result_clean$main_text <- htm2txt(result_clean$main_text)
  
  ## We'll now clean the original result data before merging with result_clean
  drop_result <- c("Jurisdiction","Location", "WebNewsUrl", "Geography", 
                   "NegativeNews", "Language", "Industry", "People", 
                   "Subject", "Company", "PublicationType", "Publisher", 
                   "GroupDuplicates", "InternationalLocation", "LEI", 
                   "CompanyName", "LNGI", "SearchWithinResults", 
                   "IsCitationMatch", "SourcePath", "Document.Citation", 
                   "Source.Id", "Source.ContentType", "Keyword" 
  )
  result <- orig_data[,!(names(orig_data) %in% drop_result)]
  rm(drop_result)
  
  
  result <- result %>%
    rename(pagination_num = column_label,
           lexisapi_type = ContentType,
           word_length = WordLength,
           byline_lexis = Byline,
           section_lexis = Section,
           lexis_id = ResultId,
           searchtype_lexis = SearchType,
           date_lexis = Date,
           title_lexis = Title,
           overview_lexis = Overview,
           sourcename_lexis = Source.Name,
    )
  
  result <- result %>%
    select(word_length, title_lexis, lexis_id, overview_lexis, date_lexis, sourcename_lexis, 
           byline_lexis, pagination_num, section_lexis, lexisapi_type, searchtype_lexis,everything())
  
  
  # Merge data together to preserve original API output while keeping cleaned data
  final_result <<- left_join(result_clean, result, by = "lexis_id")
  
  # Mark duplicate titles
  final_result$title_duplicate <<- duplicated(final_result$title, fromLast = F)
  
  # Rearange vars
  #result <- result %>%
  #  select(title, title_duplicate, everything())
  
  
  # write data
  saveRDS(final_result, "01 - Data/LexisAPI/LexisAPI_results_combined_cleaned.RDS")
  
  
}
