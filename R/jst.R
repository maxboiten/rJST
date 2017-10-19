#' @include topNwords.R

#' @export
setClass('JST.result',representation(pi = "data.frame", theta = "data.frame", phi = "data.frame",phi.termScores = "data.frame",numTopics = "numeric",numSentiments = "numeric",docvars = "data.frame"))

#' Check if an object is a JST.result object
#' 
#' @param x object
#' 
#' @return Boolean. True if x is a JST.result object.
#' 
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
  theta <- as.data.frame(t(theta))
  
  docID <- character(length(docIDs)*numSentiLabs)
  sentiment <- numeric(length(docIDs)*numSentiLabs)
  theta.names <- character(numTopics)
  
  for (i in c(1:numTopics)) {
    theta.names[i] <- paste('topic',i,sep='')
  }
  
  names(theta) <- theta.names
  
  for (i in c(1:length(docIDs))) {
    for (j in c(1:numSentiLabs)) {
      docID[j+numSentiLabs*(i-1)] <- docIDs[i]
      sentiment[j+numSentiLabs*(i-1)] <- j
    }
  }
  
  theta <- data.frame(docID,sentiment,theta,row.names=NULL)
  
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

#' @rdname topNwords-method
#' @aliases topNwords,JST.result,numeric,numeric,numeric-method
setMethod('topNwords', c('JST.result','numeric','numeric','numeric'),
          function(x,N,topic,sentiment) {
            colname <- paste('topic',topic,'sent',sentiment,sep='')
            
            res <- cbind(rownames(x@phi),x@phi[colname])
            names(res) <- c('word',colname)
            
            res <- res[order(res[colname],decreasing= TRUE),]
            
            res <- res[1:N,1]
            res <- as.character(res)
            res <- as.data.frame(res)
            names(res) <- colname
            
            return(res)
          })

#' @rdname topNwords-method
#' @aliases topNwords,JST.result,numeric,-method
setMethod('topNwords', c('JST.result','numeric'),
          function(x,N) {
            res <- as.data.frame(matrix(ncol = 0, nrow = N))
            
            for (topic in c(1:x@numTopics)) {
              for (sentiment in c(1:x@numSentiments)) {
                res <- cbind(res,topNwords(x,N,topic,sentiment))
              }
            }
            
            return(res)
          })

#' @rdname top20words-method
#' @aliases top20words,JST.result,numeric,numeric-method
setMethod('top20words', c('JST.result','numeric','numeric'),
          function(x,topic,sentiment) {
            return(topNwords(x,20,topic,sentiment))
          })

#' @rdname top20words-method
#' @aliases top20words,JST.result-method
setMethod('top20words', c('JST.result'),
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
tidy.JST.result <- function(x,parameter = NULL) {
  if (is.null(parameter)) {
    stop('Please specify which parameter from the object you would like to tidy')
  } else if (parameter == 'pi') {
    return (tidy.JST.result.pi(x))
  } else if (parameter == 'theta') {
    return (tidy.JST.result.theta(x))
  } else if (parameter == 'phi') {
    return (tidy.JST.result.phi(x))
  } else {
    stop(paste('\'',parameter,'\' is not a valid parameter of the JST.result model.',sep=''))
  }
}

tidy.JST.result.pi <- function(x) {
  docIDs <- rownames(x@pi)
  return (cbind(docIDs,x@docvars,x@pi))
}

tidy.JST.result.theta <- function(x) {
  res <- x@theta
  docvars <- x@docvars
  docvars$docID <- rownames(docvars)
  
  res <- merge(docvars,res,by='docID')
  return(res)
}

tidy.JST.result.phi <- function(x) {
  res <- x@phi
  
  res$word <- rownames(res)
  res$word <- as.factor(res$word)
  rownames(res) <- NULL
  
  res <- melt(res,id='word')
  
  variable <- as.character(res$variable)
  variable <- gsub('topic','',variable)
  topic <- as.numeric(substr(variable,start=1,stop=regexpr('s',variable)-1))
  sentiment <- as.numeric(substr(variable,start=regexpr('t',variable)+1,stop=nchar(variable)))
  
  res <- cbind(res,topic,sentiment)
  res <- subset(res,select=c('word','sentiment','topic','value'))
  
  return(res)
}