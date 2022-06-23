# Adi Tantravahi

################################################################################ PAGER
# This function extracts data stored in XML format within the
# result$Document.Content variable we extracted from the API
pager <- function(x) {

  `%>%` <- magrittr::`%>%`


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
