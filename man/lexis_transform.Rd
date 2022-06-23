\name{lexis_transform}
\alias{lexis_transform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Transform Lexis Web Services API Data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function transforms the API data from Lexis Nexis Web Services News into
readable text data. It first calls the pager function to parse the XML API response
article information and keywords. It then creates data markers for languages,
negative news, and cleans the keyword columns. All text data from the original
API call is marked with '_lexis' to maintain the data integrity. At the end,
it marks duplicate article titles. The data can always be re-transformed using
the Document.Content column. The output will be a dataframe named final_result.
}
\usage{
lexis_transform(data, orig_data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
Data argument in this function refers to the Document.Content column of the
API output, which contains the article information in XML format. In most cases,
you'll want to use result_full$Document.Content
}
  \item{orig_data}{
%%     ~~Describe \code{orig_data} here~~
orig_data refers to the dataframe being used to tranform the data. In most cases,
this will be result_full.
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
\author{
%%  ~~who you are~~
Adityamohan Tantravahi <at34@princeton.edu>
}
\note{
%%  ~~further notes~~
}



\examples{
lexis_transform(data = result_full$Document.Content,
                orig_data = result_full)
}
