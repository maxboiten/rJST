#' @include topNwords.R

#' @export
setClass('ASUM.result',representation(pi = "data.frame", 
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
is.ASUM.result <- function(x) {
  return(inherits(x,'ASUM.result'))
}

#'Aspect Sentiment Unification Model
#'
#'This method performs an estimation of the Aspect Sentiment Unification Model, which performs
#'a task similar to \code{\link{jst}}, but categorises sentences rather than words. The estimation is
#'implemented with a Gibbs sampler.
#'
#'@param sfm A \pkg{\link{quanteda}} dfm object split into sentences. To split a corpus, see
#' \link{corpus_sentences}.
#'@param sentimentLexicon A \pkg{\link{quanteda}} dictionary object containing seed words for
#' the sentiments to be estimated. Prior sentiment categories for sentences are set if to a category
#' if one or more words in the sentence are found in that category and none were found in other 
#' categories. Therefore a larger dictionary does not automatically provide a better initialisation.
#' The best dictionary i one without words that could possibly be ambiguous, especially if an 
#' asymmetrical beta is used. See \code{\link{paradigm}} for the dictionary proposed by the 
#' authors of ASUM. 
#'@param numIters Integer. The number of iterations to run.
#'@param numSentiLabs Integer. The number of sentiment labels to estimate. This is overridden if
#' a dictionary with seed words is included. If you wish to add a neutral category, set 
#' \code{excludeNeutral = FALSE}. By default, ASUM includes no neutral category.
#'@param numTopics Integer. The number of topics to be estimated.
#'@param alpha Numeric. The dirichlet parameter for the sentiment distribution prior. Defaults to 0.1.
#'@param gamma Numeric. The dirichlet parameter for the topic distribution prior. Defaults to 1.
#'@param betaVec Numeric(3). This vector asks for three elements, namely:
#' \itemize{
#'   \item{\code{betaVec[1]}}{The beta parameter to be taken for all words}
#'   \item{\code{betaVec[2]}}{The multiplier for the same sentiment category as the word has in
#'   the included sentiment lexicon. If no sentiment lexicon is included or a word is not in the 
#'   lexicon, this argument is ignored.}
#'   \item{\code{betaVec[3]}}{The multiplier for the other sentiment category/categories as the word
#'   has in the included sentiment lexicon is included or a word is not in the lexicon, this 
#'   argument is ignored.}
#' }
#' Beta can be set symmetrically as \code{betaVec = c(beta,1,1)} or asymmetrically by setting for
#' example \code{betaVec = c(beta,1,0.01)}. If \code{betaVec[3] = 0}, the calculations are streamlined
#' by setting posterior probabilities to 0 for sentiment categories other than the one a sentence is
#' initialised in if it was initialised from the sentiment lexicon. Potentially, his slightly deviates 
#' from the posterior distribution, especially if combined with larger sentiment dictionaries. It is
#' more efficient, but not advised for every application. Choose the amount of asymmetry carefully.
#'@param excludeNeutral Boolean. This variable instructs whether an extra category for neutral
#' sentences should be added to the categories provided by the sentiment dictionary.
#'@return An ASUM.result object containing the parameters from the estimated model.
#'@export
asum <- function(sfm, 
                 sentimentLexicon = NULL, 
                 numIters = 3, 
                 numSentiLabs = 2, 
                 numTopics = 5, 
                 alpha = .1, 
                 gamma = 1, 
                 betaVec = c(0.001,0.9,0.1), 
                 excludeNeutral = TRUE) {
  
  sentiWords <- integer()
  sentimentCategory <- integer()
  
  if(is.dictionary(sentimentLexicon)) {
    numSentiLabs_Lex <- length(sentimentLexicon)
    numSentiLabs <- numSentiLabs_Lex + 1 - excludeNeutral
    
    size <- 1
    for (i in c(1:numSentiLabs_Lex)) {
      for (word in sentimentLexicon[[i]]) {
        if(word %in% featnames(sfm)) {
          sentiWords[size] <- as.integer(match(word,featnames(sfm))-1) #-1 for C++ index
          sentimentCategory[size] <- as.integer(i-excludeNeutral)
          size <- size + 1
        }
      }
    }
    
  } else {
    if (!is.null(sentimentLexicon)) {
      stop('The input lexicon needs to be a quanteda dictionary object.')
    }
  }
  
  documentVector <- as.numeric(sfm@docvars$docId)
  sentenceVector <- sfm@docvars$sentenceNo
  
  res <- asumcpp(sfm,
                 documentVector,
                 sentenceVector,
                 sentiWords,
                 sentimentCategory,
                 numSentiLabs,
                 numTopics,
                 numIters,
                 alpha,
                 gamma,
                 betaVec)
  
  return(res)
}