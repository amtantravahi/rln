\name{lexis_extract}
\alias{lexis_extract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Extract data from the Lexis Nexis Web Services API.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This funciton extracts data from the Lexis Nexis Web Services API and turns it
into JSON format. It begins by making an initial call using the URL you
provide with keywords for the search. Since Lexis API only provides up to 50 search
results per a call, it then obtains the 50 data points from that call, the next page URL, and begins a
pagination loop to gather the data from any subsequent pages from your original
call, and binds them. It then merges whatever data you used to carry out your
search, so that each article can be traced back to a particlular API call to maintain
data integrity. The final output is a dataframe named result_full.


}
\usage{
lexis_extract(searchdata, url, searchdata_id, result_id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{searchdata}{
%%     ~~Describe \code{searchdata} here~~
searchdata refers to whatever object or dataframe contains your Lexis API URL.
}
  \item{url}{
%%     ~~Describe \code{url} here~~
url refers to the URL of your GET request. In the case of news, the url would
take the following format: "https://services-api.lexisnexis.com/v1/BatchNews?$expand=Document&$search="
}
  \item{searchdata_id}{
%%     ~~Describe \code{searchdata_id} here~~
searchdata_id refers to an id column in your searchdata.
}
  \item{result_id}{
%%     ~~Describe \code{result_id} here~~
result_id refers to whatever you want to name your search identifier in the
extracted data. In most cases, this should be the same name as searchdata_id.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\author{
%%  ~~who you are~~
Adityamohan Tantravahi <at34@princeton.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
lexis_extract(searchdata = keywords,
url = keywords,
searchdata_id = keywords$id,
result_id = "id")
}
