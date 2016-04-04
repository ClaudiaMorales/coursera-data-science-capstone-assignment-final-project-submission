library("tm")
library("ngramrr")

#' Download Data File
#'
#' Downloads a data file.
#'
#' @param location URL of the data file to be used formatted as a character.
#' @param data_file name of the data file to be used formatted as a character.
#' @author Michael David Gill
#' @details
#' This function downloads a data file from a specified location URL for use in
#' the package "assignment".
#' @export
#' @importFrom utils download.file

download_data_file <- function(location, data_file) {

    if (!file.exists(data_file)) {

        download.file(
            url = location,
            destfile = data_file,
            method = "curl"
        )

    }

}


#' Uncompress Data File
#'
#' Uncompresses a data file
#'
#' @param location local directory created by unpacking the data file.
#' @param data_file name of the data file to be used formatted as a character.
#' @author Michael David Gill
#' @details
#' This function uncompresses a data file for use in the package "assignment".
#' @export
#' @importFrom utils unzip

uncompress_data_file <- function(location, data_file) {

    if (!dir.exists(location)) {

        unzip(data_file)

    }

}


#' Sample Data File
#'
#' Samples a data file.
#'
#' @author Michael David Gill
#' @details
#' This function samples 1000 lines each from the data files via Mac OS X shell.
#' @export

sample_data_file <- function() {

    if (!file.exists("./final/de_DE/de_DE.blogs.sample.txt")) {

        system(
            "
            cd ./final/de_DE;
            gshuf -n 1000 de_DE.blogs.txt > de_DE.blogs.sample.txt;
            gshuf -n 1000 de_DE.news.txt > de_DE.news.sample.txt;
            gshuf -n 1000 de_DE.twitter.txt > de_DE.twitter.sample.txt;
            rm de_DE.blogs.txt;
            rm de_DE.news.txt;
            rm de_DE.twitter.txt;
            cd ..;
            cd ./en_US;
            gshuf -n 1000 en_US.blogs.txt > en_US.blogs.sample.txt;
            gshuf -n 1000 en_US.news.txt > en_US.news.sample.txt;
            gshuf -n 1000 en_US.twitter.txt > en_US.twitter.sample.txt;
            rm en_US.blogs.txt;
            rm en_US.news.txt;
            rm en_US.twitter.txt;
            cd ..;
            cd ./fi_FI;
            gshuf -n 1000 fi_FI.blogs.txt > fi_FI.blogs.sample.txt;
            gshuf -n 1000 fi_FI.news.txt > fi_FI.news.sample.txt;
            gshuf -n 1000 fi_FI.twitter.txt > fi_FI.twitter.sample.txt;
            rm fi_FI.blogs.txt;
            rm fi_FI.news.txt;
            rm fi_FI.twitter.txt;
            cd ..;
            cd ./ru_RU;
            gshuf -n 1000 ru_RU.blogs.txt > ru_RU.blogs.sample.txt;
            gshuf -n 1000 ru_RU.news.txt > ru_RU.news.sample.txt;
            gshuf -n 1000 ru_RU.twitter.txt > ru_RU.twitter.sample.txt;
            rm ru_RU.blogs.txt;
            rm ru_RU.news.txt;
            rm ru_RU.twitter.txt;
            cd ..; cd ..;
            "
        )

    }

}


#' Read Data
#'
#' Reads data into a corpus.
#'
#' @param language
#' a character giving the language as IETF language tags "de" for German, "en"
#' for English, "fi" for Finnish, or "ru" for Russian.
#' @return corpus a volatile text corpus as a "tm" package object
#' @author Michael David Gill
#' @details
#' This function reads from a data file as an object of type "corpus" from the
#' "tm" ppackage. It chooses the files corresponding to the language indicated
#' by the language parameter.
#' @export
#' @importFrom tm VCorpus
#' @importFrom tm DirSource
#' @importFrom tm readPlain

read_data <- function(language) {

    if (language == "de") {

        if (!exists("corpus")) {

            VCorpus(
                DirSource("./final/de_DE"),
                readerControl = list(reader = readPlain, language  = "de")
            )

        }

    }

    else if (language == "en") {

        if (!exists("corpus")) {

            VCorpus(
                DirSource("./final/en_US"),
                readerControl = list(reader = readPlain)
            )

        }

    }

    else if (language == "fi") {

        if (!exists("corpus")) {

            VCorpus(
                DirSource("./final/fi_FI"),
                readerControl = list(reader = readPlain, language  = "fi")
            )

        }

    }

    else if (language == "ru") {

        if (!exists("corpus")) {

            VCorpus(
                DirSource("./final/ru_RU"),
                readerControl = list(reader = readPlain, language  = "ru")
            )

        }

    }

}


#' Preprocess Corpus
#'
#' Preprocesses the corpus.
#'
#' @param corpus a volatile text corpus as a "tm" package object
#' @return a preprocessed volatile text corpus as a "tm" package object
#' @author Michael David Gill
#' @details
#' This function preprocesses a corpus by removing punctuation, numbers, and
#' English stopwords, stripping whitespace, and stemming words.
#' @export
#' @importFrom tm tm_map
#' @importFrom tm removePunctuation
#' @importFrom tm removeNumbers
#' @importFrom tm content_transformer
#' @importFrom tm stripWhitespace
#' @importFrom tm stemDocument
#' @importFrom tm removeWords

preprocess_corpus <- function(corpus) {

    # Remove punctuation from text.
    corpus_preprocessed <- tm_map(corpus, removePunctuation)

    # Remove numbers from text.
    corpus_preprocessed <- tm_map(corpus_preprocessed, removeNumbers)

    # Convert text to lowercase.
    corpus_preprocessed <-
        tm_map(corpus_preprocessed, content_transformer(tolower))

    # Strip whitespace from text.
    corpus_preprocessed <- tm_map(corpus_preprocessed, stripWhitespace)

    # Stem the text.
    corpus_preprocessed <- tm_map(corpus_preprocessed, stemDocument)

    # Remove stopwords.
    corpus_preprocessed <-
        tm_map(corpus_preprocessed, removeWords, stopwords("en"))

    # Return value.
    return(corpus_preprocessed)

}


#' Create TDM
#'
#' Creates a term-document matrix (TDM).
#'
#' @return a term-document matrix
#' @author Michael David Gill
#' @details
#' This function creates a default term-document matrix.
#' @export
#' @importFrom tm TermDocumentMatrix

create_tdm <- function() {

    TermDocumentMatrix(corpus_cleaned)

}


#' Create n-Gram
#'
#' Creates a n-gram-tokenized term-document matrix (TDM).
#'
#' @param n size of n-gram
#' @return a n-gram-tokenized term-document matrix
#' @author Michael David Gill
#' @details
#' This function creates a n-gram-tokenized term-document matrix.
#' @export
#' @importFrom ngramrr tdm2

create_ngram <- function(n) {

    if (n == 1) {

        create_tdm()

    }

    else {

        tdm2(corpus_cleaned, ngmin = n, ngmax = n)

    }

}


#' Katz's Back-Off
#'
#' Creates a Katz's back-off model.
#'
#' @param phrase a character-type input to the model
#' @return a character-type word predicted by the model
#' @author Michael David Gill
#' @details
#' This function creates a Katz's back-off model.
#' @export

katz_backoff <- function(phrase) {

    tryCatch(
        stopifnot(
            if (typeof(phrase) == "character") {

                return("word")

            }
        ),
        error = function(e) {
            "Error in katz_backoff(): non-character or null input"
        }
    )

}


# Main function calls.

download_data_file(
    "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
    "Coursera-SwiftKey.zip"
)

uncompress_data_file("final", "Coursera-SwiftKey.zip")

sample_data_file()

corpus <- read_data("en")

corpus_cleaned <- preprocess_corpus(corpus)

tdm_unigram <- create_ngram(1)
tdm_bigram <- create_ngram(2)
tdm_trigram <- create_ngram(3)

# frequent_terms_bigram <-
#     findFreqTerms(tdm_bigram, lowfreq = 3, highfreq = 48)

# frequent_terms_trigram <-
#     findFreqTerms(tdm_trigram, lowfreq = 4, highfreq = 10)
