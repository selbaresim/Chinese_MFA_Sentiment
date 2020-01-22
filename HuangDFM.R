#### HuangDFM function
#### Author: Paul Orner
#### University of Southern California
#### Political Science & International Relations PhD program

#### Takes a quanteda corpus of Chinese-language texts
#### and converts them into a Quanteda DFM
#### Function is not entirely stable, and may fail
#### if multiple texts have the same label or if there
#### is no text in a particular entry.
#### Clean your data as necessary!

#### NB This function performs NO PREPROCESSING
#### The default ("mix") jiebaR worker is used
#### for segmentation, however stop words are kept

#### Please note this function is a work in progress!
#### Most aspects of the function have yet to be vectorized,
#### and the conversion to DFM is particularly inefficient -
#### does not scale well for large datasets!

#### Special thanks to Dr. Inaki Sagarzazu of Texas Tech for
#### optimizing the code during the initial stages of the 
#### project - the code has evolved significantly since then,
#### and all inefficiencies and errors should be attributed
#### solely to Orner

library(jiebaR)
library(quanteda)
library(tidytext)

# Takes a corpus and returns a quanteda DFM
huangdfm <- function(corpus) {
  message("Inititializing JieBa worker...")
  worker.def <- worker(stop_word = 'stopwords.txt') 
  
  # Create list of tokenized documents
  message("Tokenizing texts...")
  texts.tokens <- lapply(corpus$documents$texts,
                         FUN = jiebaR::segment, 
                         jiebar = worker.def)
  
  # create vector of frequencies
  message("Getting frequencies of tokens...")
  tokens.freq <- lapply(texts.tokens, FUN = freq)
  
  dflist <- list()
  
  # attach file names to charfreq list
  # should make a function for this and lapply
  # better yet - vectorize
  message("Labelling token frequencies...")
  for (i in 1:length(tokens.freq)) {
    dflist[[i]] <- data.frame(file = i,
                              char = tokens.freq[[i]]$char,
                              freq = tokens.freq[[i]]$freq,
                              stringsAsFactors = F)
  }
  
  # combine dataframes
  message("Combining dataframes...")
  aggdfm <- do.call(rbind,dflist)
  
  # use tidy to cast as quanteda dfm 
  message("Converting to dfm...")
  aggdfm <- tidytext::cast_dfm(data = aggdfm,term = char,document = file,value = freq)
  
  return(aggdfm)
}