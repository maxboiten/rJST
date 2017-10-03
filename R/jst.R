
jst <- function(tokens,sentiLex=list(),
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
  
  res <- jstcpp('est',tokens,sentiLex,numSentiLabs, numTopics, numIters, updateParaStep,alpha,beta,gamma)

  #prepare doc sentiment distribution data.frame
  pi <- as.data.frame(res$pi)
  pi <- as.data.frame(t(pi))
  
  pi.names = character(numSentiLabs)
  for (i in c(1:numSentiLabs)) {
    pi.names[i] <- paste("sent",i,sep="")
  }
  names(pi) <- pi.names
  rownames(pi) <- names(tokens_uk)
  
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
  
  return(phi)
}
