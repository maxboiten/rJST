#' Split a corpus into sentences
#' 
#' This method splits a \pkg{quanteda} corpus object into sentences. Each sentence is
#' then returned as a document. This method is an addition to the possibility to split
#' sentences using the tokens method. The resulting tokens object can then no longer be
#' split to words, whereas the resulting corpus from this method can be turned into a 
#' tokens object (and consequently a dfm) in which the sentences remain split.
#' 
#' @param corpus A quanteda corpus object
#' 
#' @return A quanteda corpus object with sentences split. This has the following changes:
#'         \itemize {
#'          \item{sentenceNo}{The number of the sentences within documents. The are kept in
#'          order of appearance.}
#'          \item{docId}{The old id variable (document id) is replicated for every sentence and
#'          stored under the name docId. This way the documents can be combined again later if
#'          necessary (as it is for sentence LDA or ASUM)}
#'          \item{id}{This is the new unique 'document' id within the corpus structure. It is
#'          structured as follows: [docID]-[sentenceNo]. This will from here onward be treated
#'          as the document ID, since the tokens and dfm methods do not discriminated between 
#'          documents and sentences as elements of a corpus object.}
#'         }
#' @export
corpus_sentences <- function(corpus) {
  if (!is.corpus(corpus)) {
    stop('The input variable has to be a quanteda corpus object.')
  }
  documents <- as.list(corpus$documents)
  newDocuments <- list()
  
  #These stay out of the loop.
  exceptions <- c("Mr", "Mrs", "Ms", "Dr", "Jr", "Prof", "Ph.D", "M", "MM", "St", "etc")
  findregex <- paste0("\\b(", exceptions, ")\\.")
  
  for (i in c(1:dim(corpus$documents)[1])) {
    #Take text out for convenience
    txt <- documents$texts[i]
    
    #The following three methods were blatantly copied from the quanteda tokens method.
    #So are the contents of the findregex variable. So long and thanks for all the fish. 
    
    # Replace . delimiter from common title abbreviations, with _pd_
    txt <- stri_replace_all_regex(txt, findregex, "$1_pd_", vectorize_all = FALSE)
    
    ## Remove newline chars 
    txt <- lapply(txt,stri_replace_all_fixed, "\n", " ")
    
    ## Perform the tokenization
    tok <- stri_split_boundaries(txt, type = "sentence")
    
    #Prepare for easy re-entry
    tok <- unlist(tok)
    numSentences <- length(tok)
    
    #Rebuild documents list
    for (name in names(documents)) {
      if (name == 'texts') {
        newDocuments[[name]] <- c(newDocuments[[name]],tok)
      } else {
        newDocuments[[name]] <- c(newDocuments[[name]],rep(documents[[name]][i],numSentences))
      }
    }
    newDocuments$sentenceNo <- c(newDocuments$sentenceNo,c(1:numSentences))
  }
  
  newDocuments <- as.data.frame(newDocuments)
  
  #Create an id variable with unique values
  newDocuments$docId <- newDocuments$id
  newDocuments$id <- paste(newDocuments$id,newDocuments$sentenceNo,sep='-')
  rownames(newDocuments) <- newDocuments$id
  
  #Insert new documents back into the old corpus object
  corpus$documents <- newDocuments
  
  return(corpus)
}