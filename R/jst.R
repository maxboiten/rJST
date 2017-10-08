#' @useDynLib rJST

#' @export
setClass('JST.result',representation(pi = "data.frame", theta = "data.frame", phi = "data.frame",phi.termScores = "data.frame",numTopics = "numeric",numSentiments = "numeric",docvars = "data.frame"))

#' @export
is.JST.result <- function(x) {
  return(inherits(x,'JST.result'))
}


#' Run a Joint Sentiment Topic model
#'
#' @param dfm A quanteda dfm object
#' @param sentiLexInput Optional: A quanteda dictionary object for semi-supervised learning
#' @param numSentiLabs Integer, the number of sentiment labels (defaults to 3)
#' @param numTopics Integer, the number of topics (defaults to 10)
#' @param numIters Integer, the number of iterations (defaults to ???)
#' @param updateParaStep Integer. The number of iterations between optimizations of hyperparameter alpha
#' @param alpha Double, hyperparameter for (defaults to)
#' @param beta Double, hyperparameter for (defaults to)
#' @param gamma Double, hyperparameter for (defaults to)
#' @return A JST.result object containing a data.frame for each estimated parameter
#' @export
jst <- function(dfm,sentiLexInput=list(),
                numSentiLabs = 3,
                numTopics = 10,
                numIters = 3,
                updateParaStep = -1,
                alpha = -1,
                beta = -1,
                gamma = -1) {
  
  if (!any(class(dfm) == 'dfmSparse')) {
    stop('Please input a sparse quanteda dfm object as data.')
  }
  
  if(is.dictionary(sentiLexInput)) {
    sentiLex <- list()
    numSentiLabs_Lex <- length(sentiLexInput)
    
    if (numSentiLabs_Lex > numSentiLabs) {
      stop('The number of sentiment labels in the lexicon is higher than the parameter for the number of sentiment labels')
    }
    
    size <- 1
    for (i in c(1:numSentiLabs_Lex)) {
      for (word in sentiLexInput[[i]]) {
        if(word %in% attributes(tokens)$types) {
          #Note below the addition of an extra colum for the neutral probability.
          #Calculation done according to Lin, Ibeke, Wyner and Guerin, 2015. (lambda = {.9 if w in lexicon, .05 if w not in lexicon})
          #Adjusted for more than two sentiments by replacing .05 by .1/numSentiLabs_Lex
          sentiLex[[size]] <- c(match(word,attributes(tokens)$types),
                                rep(0.1/numSentiLabs_Lex,i),
                                0.9,
                                rep(0.1/numSentiLabs_Lex,numSentiLabs_Lex-i))
          size <- size + 1
        }
      }
    }
    
  }
  else {
    sentiLex = list()
  }
  
  res <- jstcpp(dfm,sentiLex,numSentiLabs, numTopics, numIters, updateParaStep, alpha,beta,gamma)
  
  #prepare doc sentiment distribution data.frame
  docIDs <- attr(dfm,'Dimnames')$docs
  
  pi <- as.data.frame(res$pi)
  pi <- as.data.frame(t(pi))
  
  pi.names = character(numSentiLabs)
  for (i in c(1:numSentiLabs)) {
    pi.names[i] <- paste("sent",i,sep="")
  }
  names(pi) <- pi.names
  rownames(pi) <- docIDs
  
  #prepare doc sentiment/topic distribution data.frame
  theta <- as.data.frame(res$theta)
  theta.names <- character(length(docIDs)*numSentiLabs)
  for (i in c(1:length(docIDs))) {
    for (j in c(1:numSentiLabs)) {
      theta.names[j+numSentiLabs*(i-1)] <- paste(docIDs[i],"sent",j,sep="")
    }
  }
  names(theta) <- theta.names
  
  #prepare word topic/sentiment distribtuion data.frame
  phi <- as.data.frame(res$phi)
  phi.termScores <- as.data.frame(res$phi.termScores)
  
  phi.names = character(numSentiLabs*numTopics)
  for (i in c(1:numSentiLabs)) {
    for (j in c(1:numTopics)) {
      phi.names[j+numTopics*(i-1)] <- paste("topic",j,"sent",i,sep="")
    }
  }
  names(phi) <- phi.names
  names(phi.termScores) <- phi.names
  rownames(phi) <- attr(dfm,'Dimnames')$features
  rownames(phi.termScores) <- attr(dfm,'Dimnames')$features
  
  return(new("JST.result",
             pi = pi,
             theta = theta,
             phi = phi,
             phi.termScores = phi.termScores,
             numTopics=numTopics,
             numSentiments=numSentiLabs,
             docvars=attr(dfm,'docvars')))
}

#' Show the top 20 words for a topic/sentiment combination
#'
#' @param x A JST.result object
#' @param topic Integer
#' @param sentiment Integer
#' @param termScores Boolean. TRUE is you wish to use term scores (Lafferty and Blei, 2009)
#'        rather than the phi parameter as estimated by JST. Defaults to TRUE.
#' @return A CharacterVector containing the 20 top words of the topic/sentiment combination
#' @export
top20words <- function(x,topic,sentiment,termScores = TRUE) {
  return(topNwords(x,topic,sentiment,20,termScores))
}

#' Show the top N words for a topic/sentiment combination
#'
#' @param x A JST.result object
#' @param topic Integer
#' @param sentiment Integer
#' @param N Integer, the number of words to be returned
#' @param termScores Boolean. TRUE is you wish to use term scores (Lafferty and Blei, 2009)
#'        rather than the phi parameter as estimated by JST. Defaults to TRUE.
#' @return A CharacterVector containing the N top words of the topic/sentiment combination
#' @export
topNwords <- function(x,topic,sentiment,N,termScores = TRUE) {
  if (!is.JST.result(x)) {
    stop('The input to this function should be a JST results object')
  }
  if (topic <= 0 || sentiment <= 0) {
    stop('Topic and sentiment should be positive integers')
  }
  if (topic > x@numTopics) {
    stop(paste('The topic [',topic,'] specified is too large. The number of topics in this results object is ',x@numTopics,sep=''))
  }
  if (sentiment > x@numSentiments) {
    stop(paste('The sentiment [',sentiment,'] specified is too large. The number of sentiments in this results object is ',x@numSentiments,sep=''))
  }
  
  if (termScores) {
    data <- x@phi.termScores
  } else {
    data <- x@phi
  }
  wordScores <- data[paste('topic',topic,'sent',sentiment,sep='')]
  wordScores <- as.numeric(wordScores[,1])
  names(wordScores) <- rownames(data)
  wordScores <- sort(wordScores,decreasing=TRUE)
  
  return(names(wordScores)[1:N])
}