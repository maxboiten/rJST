#' @include topNwords.R

#' @export
setClass('JST.result',representation(pi = "data.frame", 
                                     theta = "data.frame", 
                                     phi = "data.frame",
                                     phi.termScores = "data.frame",
                                     numTopics = "numeric",
                                     numSentiments = "numeric",
                                     docvars = "data.frame"))

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
#' @param sentiLexInput Optional: A quanteda dictionary object for semi-supervised learning. If
#' a dictionary is used, \code{numSentiLabs} will be overridden by the number of categories in the
#' dictionary object. An extra category will by default be added for neutral words. This can be
#' turned off by setting \code{excludeNeutral = TRUE}.
#' @param numSentiLabs Integer, the number of sentiment labels (defaults to 3)
#' @param numTopics Integer, the number of topics (defaults to 10)
#' @param numIters Integer, the number of iterations (defaults to ???)
#' @param updateParaStep Integer. The number of iterations between optimizations of hyperparameter alpha
#' @param alpha Double, hyperparameter for (defaults to)
#' @param beta Double, hyperparameter for (defaults to)
#' @param gamma Double, hyperparameter for (defaults to)
#' @param excludeNeutral Boolean. If a dictionary is used, an extra category is added for neutral
#' words. Words in the dictionary receive a low probability of being allocated there. If this is set
#' to \code{TRUE}, the neutral sentiment category will be omitted. The variable is irrelevant if no
#' dictionary is used. Defaults to \code{FALSE}.
#' @return A JST.result object containing a data.frame for each estimated parameter
#' @export
jst <- function(dfm,sentiLexInput=list(),
                numSentiLabs = 3,
                numTopics = 10,
                numIters = 3,
                updateParaStep = -1,
                alpha = -1,
                beta = -1,
                gamma = -1,
                excludeNeutral = FALSE) {
  
  if (!any(class(dfm) == 'dfmSparse')) {
    stop('Please input a sparse quanteda dfm object as data.')
  }
  
  sentiWords <- integer()
  sentimentCategory <- integer()
  
  if(is.dictionary(sentiLexInput)) {
    numSentiLabs_Lex <- length(sentiLexInput)
    numSentiLabs <- numSentiLabs_Lex + 1 - excludeNeutral
    
    size <- 1
    for (i in c(1:numSentiLabs_Lex)) {
      for (word in sentiLexInput[[i]]) {
        if(word %in% featnames(dfm)) {
          sentiWords[size] <- as.integer(match(word,featnames(dfm))-1) #-1 for C++ index
          sentimentCategory[size] <- as.integer(i-excludeNeutral)
          size <- size + 1
        }
      }
    }
    
  } else {
    stop('The input lexicon needs to be a quanteda dictionary object.')
  }
  
  res <- jstcpp(dfm,sentiWords,sentimentCategory,numSentiLabs, numTopics, numIters, updateParaStep, alpha,beta,gamma)
  
  if(length(res) == 0) {return(".")}
  
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
  
  theta.names <- character(numTopics)
  
  theta.names = character(numSentiLabs*numTopics)
  for (i in c(1:numSentiLabs)) {
    for (j in c(1:numTopics)) {
      theta.names[j+numTopics*(i-1)] <- paste("topic",j,"sent",i,sep="")
    }
  }
  
  names(theta) <- theta.names
  
  theta <- data.frame(docIDs,theta,row.names=NULL)
  
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
            
            column <- sapply(x@phi[colname],as.numeric)
            
            res <- rownames(x@phi)[topNwordSeeds(column,N)]

            res <- as.data.frame(res,stringsAsFactors = FALSE)

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