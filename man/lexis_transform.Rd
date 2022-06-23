\name{lexis_transform}
\alias{lexis_transform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Transform
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lexis_transform(data, orig_data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{orig_data}{
%%     ~~Describe \code{orig_data} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, orig_data)
{
    `\%>\%` <- magrittr::`\%>\%`
    result_clean <- do.call(rbind, lapply(data, pager)) \%>\% group_by_at(vars(-keyword_ln)) \%>\%
        summarize(keyword_ln = toString(keyword_ln)) \%>\% ungroup() \%>\%
        select(names(.))
    result_clean <- result_clean \%>\% mutate(., neg_news = as.integer(stringr::str_detect(keyword_ln,
        "NEGATIVE NEWS")), french = as.integer(stringr::str_detect(keyword_ln,
        "FRENCH")), english = as.integer(stringr::str_detect(keyword_ln,
        "ENGLISH|English")), spanish = as.integer(stringr::str_detect(keyword_ln,
        "SPANISH")), italian = as.integer(stringr::str_detect(keyword_ln,
        "ITALIAN")), german = as.integer(stringr::str_detect(keyword_ln,
        "GERMAN")))
    result_clean <- result_clean \%>\% mutate(., language = ifelse(french ==
        1, "French", ifelse(english == 1, "English", ifelse(spanish ==
        1, "Spanish", ifelse(german == 1, "German", ifelse(italian ==
        1, "Italian", ""))))))
    result_clean$pub_date <- stringr::str_extract(result_clean$pub_date,
        "[0-9]{4}[.-][0-9]{2}[.-][0-9]{2}")
    result_clean$pub_date <- lubridate::ymd(result_clean$pub_date)
    wordstoremove <- c("Newspaper,", "Newspapers,", "ENGLISH,",
        "English,", "GERMAN,", "SPANISH,", "ITALIAN,", "Web Blog,",
        "Blogs,", "Newswire,", "Newswires & Press Releases,",
        "Transcript,", "News Transcripts,", "English US,", "FRENCH,",
        "FRANÇAIS,", "Dépêche,", "Agencia de información,",
        "ESPAÑOL,", "Journal,", "Magazines & Journals,", "Web-based Publications,",
        "Web Publication,", "Publication internet,", "Industry Trade Press,",
        "Zeitung,", "DEUTSCH,", "NEWSPAPER,", "COLUMN,", "Magazine,",
        "NEGATIVE PERSONAL NEWS,", "  NEGATIVE MISC NEWS,", "NEGATIVE NEWS,",
        "Newspaper;", "BLOGS & MESSAGE BOARDS,", "NEWSPAPER,",
        "Newsletter,", "Newsletters,")
    result_clean$keyword <- sapply(result_clean$keyword_ln, function(x) gsub(paste(wordstoremove,
        collapse = "|"), "", x))
    result_clean$main_text <- htm2txt::htm2txt(result_clean$main_text)
    drop_result <- c("Jurisdiction", "Location", "WebNewsUrl",
        "Geography", "NegativeNews", "Language", "Industry",
        "People", "Subject", "Company", "PublicationType", "Publisher",
        "GroupDuplicates", "InternationalLocation", "LEI", "CompanyName",
        "LNGI", "SearchWithinResults", "IsCitationMatch", "SourcePath",
        "Document.Citation", "Source.Id", "Source.ContentType",
        "Keyword")
    result <- orig_data[, !(names(orig_data) \%in\% drop_result)]
    rm(drop_result)
    result <- result \%>\% rename(pagination_num = column_label,
        lexisapi_type = ContentType, word_length = WordLength,
        byline_lexis = Byline, section_lexis = Section, lexis_id = ResultId,
        searchtype_lexis = SearchType, date_lexis = Date, title_lexis = Title,
        overview_lexis = Overview, sourcename_lexis = Source.Name,
        )
    result <- result \%>\% dplyr::select(word_length, title_lexis,
        lexis_id, overview_lexis, date_lexis, sourcename_lexis,
        byline_lexis, pagination_num, section_lexis, lexisapi_type,
        searchtype_lexis, everything())
    final_result <<- dplyr::left_join(result_clean, result, by = "lexis_id")
    final_result$title_duplicate <<- duplicated(final_result$title,
        fromLast = F)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
