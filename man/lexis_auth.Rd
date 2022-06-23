\name{lexis_auth}
\alias{lexis_auth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Authorizes Access to Lexis Nexis Web Services API
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function authorizes access to the Lexis Nexis Web Services API. It will
return an environment named auth.code that will be used to authorize all your
API calls.
}

\usage{
lexis_auth(key, secret)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{key}{
%%     ~~Describe \code{key} here~~
This is your client ID key provided by Lexis Nexis.
}
  \item{secret}{
%%     ~~Describe \code{secret} here~~
This is your secret key provided by Lexis Nexis.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\author{
%%  ~~who you are~~
Adityamohan Tantravahi, at34@princeton.edu
}
\note{
%%  ~~further notes~~
}
\examples{
# NOT RUN. WONT WORK.
key <- "12345678901234567890"
secret <- "1234567890123456789012345678901234567890"

lexis_auth(key = key,
           secret = secret)
}
