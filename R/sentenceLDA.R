#' @include topNwords.R

#' @export
setClass('sLDA.result',representation(theta = "data.frame", 
                                     phi = "data.frame", 
                                     numTopics = "numeric", 
                                     numIters = "numeric",
                                     docvars = "data.frame"))

#' Check if an object is a sLDA.result object
#' 
#' @param x object
#' 
#' @return Boolean. True if x is a LDA.result object.
#' 
#' @export
is.sLDA.result <- function(x) {
  return(inherits(x,'sLDA.result'))
}

#' Sentence Latent Dirichlet Allocation
#' 
#' Gibbs sampling estimation of sentence Latent Dirichlet Allocation (sLDA). Note that the
#' primary argument to this method is a dfm object in which the documents are expected to
#' be split into sentences. This can be done earlier in the process, using the method 
#' \code{\link{corpus_sentences}}. There is no check for this requirement, so keep in mind
#' to do it yourself.
#' 
#' @param dfm A quanteda dfm object, split into sentences
#' @param numTopics Integer, the number of topics
#' @param numIters Integer, the number of iterations
#' @param alpha Double, hyperparameter for the dirichlet prior of the topic
#'    distribution
#' @param beta Double, hyperparameter for the dirichlet prior of the words
#'    distribution
#' 
#' @return An sLDA.result object containing a data.frame for each estimated parameter
#' 
#' @seealso \code{\link{corpus_sentences}}
#' @export
sentencelda <- function(dfm,numTopics,numIters,alpha,beta) {
  
  if (!('docId' %in% names(dfm@docvars) && 'sentenceNo' %in% names(dfm@docvars))) {
    stop('This dfm does not contain variables \'docId\' and \'sentenceNo\'.\n
         Please make sure you use a dfm generated from a corpus generated with 
         the corpus_sentences method. See ?corpus_sentences for more info.')
  }
  
  documentVector <- as.numeric(dfm@docvars$docId)
  sentenceVector <- dfm@docvars$sentenceNo
  
  res <- sldacpp(dfm,documentVector,sentenceVector,numTopics,numIters,alpha,beta)
  
  #General variables
  docvars <- dfm@docvars
  docID <- as.data.frame(levels(docvars$docId))
  word <- dfm@Dimnames$features
  
  #Theta data.frame
  theta <- as.data.frame(res$theta)
  
  topic.names <- character(numTopics)
  for (i in c(1:numTopics)) {
    topic.names[i] <- paste('topic',i,sep='')
  }
  
  names(theta) <- topic.names
  theta <- cbind(docID,theta)
  
  #Phi data.frame
  phi <- as.data.frame(res$phi)
  
  names(phi) <- topic.names
  rownames(phi) <- word
  
  return(new('sLDA.result',theta = theta,
             phi = phi,
             numTopics = numTopics,
             numIters = numIters,
             docvars = docvars))
}

#' @rdname topNwords-method
#' @aliases topNwords,sLDA.result,numeric,numeric-method
setMethod('topNwords', c('sLDA.result','numeric','numeric'),
          function(x,N,topic) {
            colname <- paste('topic',topic,sep='')
            
            column <- sapply(x@phi[colname],as.numeric)
            
            res <- rownames(x@phi)[topNwordSeeds(column,N)]
            
            res <- as.data.frame(res,stringsAsFactors = FALSE)
            
            names(res) <- colname
            
            return(res)
          })

#' @rdname topNwords-method
#' @aliases topNwords,sLDA.result,numeric,-method
setMethod('topNwords', c('sLDA.result','numeric'),
          function(x,N) {
            res <- as.data.frame(matrix(ncol = 0, nrow = N))
            
            for (topic in c(1:x@numTopics)) {
              res <- cbind(res,topNwords(x,N,topic))
            }
            
            return(res)
          })

#' @rdname top20words-method
#' @aliases top20words,sLDA.result,numeric-method
setMethod('top20words', c('sLDA.result','numeric'),
          function(x,topic) {
            return(topNwords(x,20,topic))
          })

#' @rdname top20words-method
#' @aliases top20words,sLDA.result-method
setMethod('top20words', c('sLDA.result'),
          function(x) {
            return(topNwords(x,20))
          })

#' Tidy JST results
#' 
#' This method tidies up the results object for the selected parameter
#' and returns a data.frame that conforms to the standards from the
#' R tidyverse. See \pkg{broom} for the generic tidy method.
#' 
#' @param x A JST.result object
#' @param parameter Character. The parameter to be tidied and returned. 
#'        Note that no default is set.
#' 
#' @return A tidy data.frame.
#' 
#' @export
tidy.sLDA.result <- function(x,parameter = NULL) {
  if (is.null(parameter)) {
    stop('Please specify which parameter from the object you would like to tidy')
  } else if (parameter == 'theta') {
    return (tidy.sLDA.result.theta(x))
  } else if (parameter == 'phi') {
    return (tidy.sLDA.result.phi(x))
  } else {
    stop(paste('\'',parameter,'\' is not a valid parameter of the JST.result model.',sep=''))
  }
}

tidy.sLDA.result.theta <- function(x) {
  docvars <- x@docvars
  docvars$docID <- rownames(docvars)
  
  res <- merge(docvars,x@theta,by='docID')
  return(res)
}

tidy.sLDA.result.phi <- function(x) {
  res <- x@phi
  
  res <- melt(res,id='word')
  
  topic <- as.numeric(substr(variable,start=6,stop=nchar(variable)))
  
  res <- cbind(res,topic)
  res <- subset(res,select=c('word','topic','value'))
  
  return(res)
}