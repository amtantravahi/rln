\name{lexis_corpus_process}
\alias{lexis_corpus_process}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Process extraced Lexis Nexis API data into a corpus.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function proccesses the extracted Lexis Nexis API data into a corpus
ready for analysis. It starts by cleaning stopwords from your chosen text column
in the following languages: English, Spanish, French, Italian, German, and Dutch.
It then removes punctuation, numbers, white space, and stems the words in
each document by language. The final corpus is named processedCorpus.

}
\usage{
lexis_corpus_process(data, text_var)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
data refers to the dataframe with the extracted Lexis API data. In most cases,
this is the final_result dataframe that is output from the lexis_transform
function.
}
  \item{text_var}{
%%     ~~Describe \code{text_var} here~~
text_var refers to the text variable within the data you want to process.
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

\examples{
lexis_corpus_process(data = final_result,
                     text_var = final_result$main_text)

lexis_corpus_process(data = final_result,
                     text_var = final_result$keywords)

}
