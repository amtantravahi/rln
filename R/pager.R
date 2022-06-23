# Adi Tantravahi

################################################################################ PAGER
# This function extracts data stored in XML format within the
# result$Document.Content variable we extracted from the API
pager <- function(x) {

  `%>% <- magrittr::`%>%`


  # Parse data as XML
  doc <- xml2::read_xml(x)

  # Parse as HTML for any HTML stored in the string
  doc_html <- xml2::read_html(x)

  # Extract data into a seperate dataframe
  data.frame(title = doc `%>%`
               xml2::xml_find_one(xpath = "//nitf:hedline") `%>%`
               rvest::html_text(),
             pub_date = doc_html `%>%`
               rvest::html_node("published") `%>%`
               rvest::html_text(),
             author = doc_html `%>%`
               rvest::html_node("author") `%>%`
               rvest::html_text(),
             dateline = doc `%>%`
               xml2::xml_find_one(xpath = "//nitf:dateline") `%>%`
               rvest::html_text(),
             main_text = doc `%>%`
               xml2::xml_find_one(xpath = "//bodyText") `%>%`
               rvest::html_text(),
             publisher = doc `%>%`
               xml2::xml_find_one(xpath = "//publicationName") `%>%`
               rvest::html_text(),
             pub_date_txt = doc `%>%`
               xml2::xml_find_one(xpath = "//dateText") `%>%`
               rvest::html_text(),
             copyright = doc `%>%`
               xml2::xml_find_one(xpath ="//copyright") `%>%`
               rvest::html_text(),
             lexis_id = doc_html `%>%`
               rvest::html_node("id") `%>%`
               rvest::html_text(),
             keyword_ln = doc `%>%`
               xml2::xml_find_all("//className") `%>%`
               xml2::xml_text()
  )


}
