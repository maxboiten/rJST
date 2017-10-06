setClass('JST.result',representation(pi = "data.frame", theta = "data.frame", phi = "data.frame",numTopics = "numeric",numSentiments = "numeric",docvars = "data.frame"))

is.JST.result <- function(x) {
  return(inherits(x,'JST.result'))
}

#tralalalalala

jst <- function(tokens,sentiLexInput=list(),
                numSentiLabs = 3,
                numTopics = 10,
                numIters = 3,
                updateParaStep = -1,
                alpha = -1,
                beta = -1,
                gamma = -1) {

  if (!any(class(tokens) != 'tokens')) {
    stop('Please input a quanteda tokens object as data.')
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
          sentiLex[[size]] <- c(match(word,attributes(tokens)$types),rep(0,i-1),1,rep(0,numSentiLabs_Lex-i))
          size <- size + 1
        }
      }
    }

  }
  else {
    sentiLex = list()
  }

  res <- jstcpp('est',tokens,sentiLex,numSentiLabs, numTopics, numIters, updateParaStep,alpha,beta,gamma)

  #prepare doc sentiment distribution data.frame
  pi <- as.data.frame(res$pi)
  pi <- as.data.frame(t(pi))

  pi.names = character(numSentiLabs)
  for (i in c(1:numSentiLabs)) {
    pi.names[i] <- paste("sent",i,sep="")
  }
  names(pi) <- pi.names
  rownames(pi) <- names(tokens)

  #prepare doc sentiment/topic distribution data.frame
  theta <- as.data.frame(res$theta)
  theta.names <- character(length(tokens)*numSentiLabs)
  docIDs <- names(tokens)
  for (i in c(1:length(tokens))) {
    for (j in c(1:numSentiLabs)) {
      theta.names[j+numSentiLabs*(i-1)] <- paste("doc",docIDs[i],"sent",j,sep="")
    }
  }
  names(theta) <- theta.names

  #prepare word topic/sentiment distribtuion data.frame
  phi <- as.data.frame(res$phi)

  phi.names = character(numSentiLabs*numTopics)
  for (i in c(1:numSentiLabs)) {
    for (j in c(1:numTopics)) {
      phi.names[j+numTopics*(i-1)] <- paste("topic",j,"sent",i,sep="")
    }
  }
  names(phi) <- phi.names
  rownames(phi) <- attributes(tokens)$types

  result <- new

  return(new("JST.result",pi = pi, theta = theta, phi = phi,numTopics=numTopics,numSentiments=numSentiLabs,docvars=attr(tokens,'docvars')))
}

top20words <- function(x,topic,sentiment) {
  return(topNwords(x,topic,sentiment,20))
}

topNwords <- function(x,topic,sentiment,N) {
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

  wordScores <- x@phi[paste('topic',topic,'sent',sentiment,sep='')]
  wordScores <- as.numeric(wordScores[,1])
  names(wordScores) <- rownames(x@phi)
  wordScores <- sort(wordScores,decreasing=TRUE)

  return(names(wordScores)[1:N])
}

plot.JST.result <- function(x,sentiment1,sentiment2,colourBy=NULL) {
  if (sentiment1 <= 0 || sentiment2 <= 0) {
    stop('Both sentiment variables need to be positive integers')
  }
  if (sentiment1 > x@numSentiments || sentiment2 > x@numSentiments) {
    stop(paste('One or both sentiment arguments are higher than the number of sentiments in the results object (',x@numSentiments,')',sep=''))
  }

  sentiment1 <- paste('sent',sentiment1,sep='')
  sentiment2 <- paste('sent',sentiment2,sep='')
  plotData <- cbind(x@pi,x@docvars)



  if (is.null(colourBy)) {
    return(ggplot(plotData,aes_string(sentiment1,sentiment2)) + geom_point())
  }
  else {
    if (!(colourBy) %in% names(plotData)) {
      stop(paste('\'',colourBy,'\' is no document variable in this results object.',sep=''))
    }
    return(ggplot(plotData,aes_string(sentiment1,sentiment2,colour=colourBy)) + geom_point())
  }
}
