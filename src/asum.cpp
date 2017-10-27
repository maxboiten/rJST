#include "asum.h"

/*
Notes.
1 - Runs on Eigen rather than Armadillo to get a row oriented sparse matrix.
2 - See notes further on in the code for more info.
*/

// [[Rcpp::export]]
Rcpp::List asumcpp(Eigen::SparseMatrix<int,Eigen::ColMajor>& sfm,
        Rcpp::IntegerVector& documentVector,
        Rcpp::IntegerVector& sentenceVector,
        Rcpp::IntegerVector& sentiWords,
        Rcpp::IntegerVector& sentiCategory,
        int numSentiLabs,
        int numTopics,
        int numiters,
        double alpha,
        double gamma,
        Rcpp::NumericVector& betaVec) {

    asummodel * asum = new asummodel();
  
    SpMat sfmChanged = sfm;
    
    asum->numSentiLabs = numSentiLabs;
    asum->numTopics = numTopics;
    asum->numiters = numiters;
    asum->alpha = alpha;
    asum->gamma = gamma;
    for (int i = 0; i < 3; i++) {
      asum->betaInput[i] = betaVec[i];
    }    
    asum->sfm = &sfmChanged;

    asum->init(documentVector,sentenceVector,sentiWords,sentiCategory);
    if (asum->estimate()) {
      return Rcpp::List();
    }

    return Rcpp::List::create(Rcpp::Named("pi") = asum->pi_ld,
                              Rcpp::Named("phi") = asum->phi_lzw,
                              Rcpp::Named("theta") = asum->theta_lzd);
}

void asummodel::init(Rcpp::IntegerVector& documentVector, 
                    Rcpp::IntegerVector& sentenceVector,
                    Rcpp::IntegerVector& sentiWords,
                    Rcpp::IntegerVector& sentiCategory) {

  numSentences = sfm->rows();
  vocabSize = sfm->cols();

  sentenceSizes.resize(numSentences);
  for (int s = 0; s < numSentences; s++) {
    sentenceSizes[s] = sfm->innerVector(s).sum(); //innerVector is appropriate here because it's row-major
  }

  documentSentenceVec.resize(numSentences);

  std::pair<int,int> temp;
  numDocs = 1;
  std::vector<int> docSentence;
  for (int row = 0; row < documentVector.size(); row++) {
    temp = std::make_pair(documentVector[row]-1,sentenceVector[row]-1);
    if (temp.first >= numDocs) {
      numDocs = temp.first + 1;
    }
    documentSentenceVec[row] = temp;
  }

  for (int i = 0; i < sentiWords.size(); i++) {
    sentiLex.insert(std::pair<int,int>(sentiWords[i], sentiCategory[i]));
  }

  docSizes.resize(numDocs);
  std::fill(docSizes.begin(),docSizes.end(),0);
  for (std::vector<std::pair<int,int> >::iterator it = documentSentenceVec.begin(); it != documentSentenceVec.end(); it++) {
    docSizes[it->first]++; //Second element is pair which has document number as first element.
  }
  
  init_parameters();

  init_estimate();
}

void asummodel::init_parameters() {
  //Set random seed
  srand(time(NULL));

  //Topic and sentiment distributions
  topic_ds.resize(numDocs);
  sent_ds.resize(numDocs);
  for (int d = 0; d < numDocs; d++) {
    topic_ds[d].resize(docSizes[d]);
    sent_ds[d].resize(docSizes[d]);
  }

  //Count variables
  nd_s.resize(numDocs);
  std::fill(nd_s.begin(),nd_s.end(),0);

  ndl_s.resize(numDocs);
  ndlz_s.resize(numDocs);
  for (int d = 0; d < numDocs; d++) {
    ndl_s[d].resize(numSentiLabs);
    std::fill(ndl_s[d].begin(),ndl_s[d].end(),0);

    ndlz_s[d].resize(numSentiLabs);
    for (int l = 0; l < numSentiLabs; l++) {
      ndlz_s[d][l].resize(numTopics);
      std::fill(ndlz_s[d][l].begin(),ndlz_s[d][l].end(),0);
    }
  }

  nlz_w.resize(numSentiLabs);
  nlzw_w.resize(numSentiLabs);
  for (int l = 0; l < numSentiLabs; l++) {
    nlz_w[l].resize(numTopics);
    std::fill(nlz_w[l].begin(),nlz_w[l].end(),0);
    nlzw_w[l].resize(numTopics);
    for (int z = 0; z < numTopics; z++) {
      nlzw_w[l][z].resize(vocabSize);
      std::fill(nlzw_w[l][z].begin(),nlzw_w[l][z].end(),0);
    }
  }

  sentencePriorSent.resize(numSentences);

  //Posterior
  plz.resize(numSentiLabs);
  for (int l = 0; l < numSentiLabs; l++) {
    plz[l].resize(numTopics);
  }

  //Hyperparameters
  set_alpha();
  set_beta(); //Asymmetric based on sentiment Lexicon
  set_gamma();

  //Result vectors
  pi_ld.resize(numSentiLabs);
  theta_lzd.resize(numSentiLabs);
  phi_lzw.resize(numSentiLabs);

  for (int l = 0; l < numSentiLabs; l++) {
    pi_ld[l].resize(numDocs);
    theta_lzd[l].resize(numTopics);
    phi_lzw[l].resize(numTopics);

    for(int z = 0; z < numTopics; z++) {
      theta_lzd[l][z].resize(numDocs);
      phi_lzw[l][z].resize(vocabSize);
    }
  }
}

void asummodel::set_gamma() {
  gammaSum = numSentiLabs * gamma;
}
  
void asummodel::set_alpha() {
  alphaSum = numTopics * alpha;
}
  
void asummodel::set_beta() {
  std::vector<std::vector<double> > lambda_lw;

  beta_lw.resize(numSentiLabs);
  lambda_lw.resize(numSentiLabs);
  betaSum_l.resize(numSentiLabs);
  std::fill(betaSum_l.begin(),betaSum_l.end(),0);
  
  for (int l = 0; l < numSentiLabs; l++) {
    beta_lw[l].resize(vocabSize);
    std::fill(beta_lw[l].begin(),beta_lw[l].end(),betaInput[0]);

    lambda_lw[l].resize(vocabSize);
    std::fill(lambda_lw[l].begin(),lambda_lw[l].end(),1);
  }

  for (std::map<int,int>::iterator it = sentiLex.begin(); it != sentiLex.end(); it++) {
    //For each entry of the sentiment lexicon:
    for (int l = 0; l < numSentiLabs; l++) {
      if (it->second == l) {
        lambda_lw[l][it->first] = betaInput[1];
      } else {
        lambda_lw[l][it->first] = betaInput[2];
      }
    }
  }
  
  for (int l = 0; l < numSentiLabs; l++) {
    for (int w = 0; w < vocabSize; w++) {
       beta_lw[l][w] *= lambda_lw[l][w];
       betaSum_l[l] += beta_lw[l][w];
    }
  }
}

void asummodel::init_estimate() {
  int topic,sentilab,document,sentence,priorSent,numSentenceSenti;
  std::map<int,int>::iterator sentiIt;

  for (int row = 0; row < numSentences; row++) {
    document = documentSentenceVec[row].first;
    sentence = documentSentenceVec[row].second;

    priorSent = -1;
    numSentenceSenti = 0;

    /*Method for determining prior sentiment of a sentence:
    1. Check for every word if it is in the sentiment lexicon
    2. If it is, check if a previously found had the same sentiment orientation
    3. The first sentiment word will change priorSent to its sentiment orientation and set the
       number of sentiments in a sentence to 1.
    4. Any sentiment word found after that will only change the number of sentiments in the sentence if
       its sentiment orientation is different from the one previously found.
    5. A sentence is assigned a prior sentiment if its sentiment words belong to one category, so a 
       sentence with two positive words and a negative words will still be randomly assigned.
    */

    for (rowit it(*sfm,row); it; ++it) {
      sentiIt = sentiLex.find(it.col());
      if (sentiIt != sentiLex.end()) {
        if (priorSent != sentiIt->second) {
          numSentenceSenti++;
          priorSent = sentiIt->second;
        }
      }
    }
    
    if (numSentenceSenti == 1) {
      sentilab = priorSent;
      sentencePriorSent[row] = sentilab;
    } else {
      sentilab = rand() % numSentiLabs;
      sentencePriorSent[row] = -1;
    }

    topic = rand() % numTopics;

    topic_ds[document][sentence] = topic;
    sent_ds[document][sentence] = sentilab;

    ndl_s[document][sentilab]++;
    nd_s[document]++;
    ndlz_s[document][sentilab][topic]++;
    nlz_w[sentilab][topic] += sentenceSizes[row];
    for (rowit it(*sfm,row); it; ++it) {
      nlzw_w[sentilab][topic][it.col()] += it.value();
    }
  }
}

int asummodel::estimate() {
  int topic,sentilab,document,sentence;

  Progress p(numiters-1,true);

  for (int iter = 1; iter < numiters; iter++) {
    if (Progress::check_abort()) {
      Rcpp::Rcout << "Process aborted at iteration " << iter << std::endl;
      return 1;
    }
    for (int row = 0; row < numSentences; row++) {
      document = documentSentenceVec[row].first;
      sentence = documentSentenceVec[row].second;

      topic = topic_ds[document][sentence];
      sentilab = sent_ds[document][sentence];

      ndl_s[document][sentilab]--;
      nd_s[document]--;
      ndlz_s[document][sentilab][topic]--;
      nlz_w[sentilab][topic] -= sentenceSizes[row];
      for (rowit it(*sfm,row); it; ++it) {
        nlzw_w[sentilab][topic][it.col()] -= it.value();
      }

      drawsample(row, document, topic, sentilab);

      topic_ds[document][sentence] = topic;
      sent_ds[document][sentence] = sentilab;

      ndl_s[document][sentilab]++;
      nd_s[document]++;
      ndlz_s[document][sentilab][topic]++;
      nlz_w[sentilab][topic] += sentenceSizes[row];
      for (rowit it(*sfm,row); it; ++it) {
        nlzw_w[sentilab][topic][it.col()] += it.value();
      }
    }
    p.increment();
  }

  computePild();
  computeThetalzd();
  computePhilzw();

  return 0;
}

void asummodel::drawsample(int row, int document, int& topic, int& sentilab) {
  int wordToken;
  int m0 = 0;
  double u;

  for (int l = 0; l < numSentiLabs; l++) {
    std::fill(plz[l].begin(),plz[l].end(),1); //Fill the posterior vector with ones to update by multiplication
  }
  
  for (int l = 0; l < numSentiLabs; l++) {

    /* Notes on trimming the calculation
    1. If the user chooses to set the asymmetric betas in such a way that the betas for other sentiment
       categories than the word's own (if it is in the lexicon) become 0, the posterior always ends up
       being equal to 0 after the initialisation in categories. However, this is not perfect. If words 
       occur in sentences with words from different sentiment categories, we get in trouble. Then the
       sentiment is initialised randomly, so the count of that word in other sentiment categories is not 0
       and the posterior will not be 0 either, so trimming is only a quick approximation of the posterior.
       That being said, if it's quick and we know this, why not allow for it? Choosing asymmetric non-0 beta
       can cover for this. Set it very small and it works.
    2. In contrast with the code supplied by authors Jo and Oh, the prior sentiment of a 
       sentence is used (instead of prior sentiment for words), which is set to -1 if multiple categories 
       exist in the sentence. Therefore sentences that are dubious are included as usually in the posterior
       calculation. These sentences can be slightly problematic if the counts for seed words from different
       sentiment categories are 0. That would mean that the posterior could sum to 0 over all sentiment 
       categories and topics. On the off chance that it doesn't (which it might not in larger corpora) we do
       calculate the posterior as if nothing happened.
    3. Practically: if the sentence has a prior sentiment that is unequal to the sentiment in the loop, we
       set it to 0 for all topics and continue to the next iteration of the sentiment label loop. This test
       is only implemented if the multiplier for betas from different sentiments is set to 0.
    */
    if (betaInput[2] == 0.0) {
      if (sentencePriorSent[row] != -1 && sentencePriorSent[row] != l) {
        std::fill(plz[l].begin(),plz[l].end(),0);
        continue;
      }
    }
    
    //Calculating the third term of the posterior
    for (rowit it(*sfm,row); it; ++it) {
      wordToken = it.col();

      for (int z = 0; z < numTopics; z++) {
        for (int m = 0; m < it.value(); m++) {
          plz[l][z] *= (nlzw_w[l][z][wordToken] + beta_lw[l][wordToken] + m)/(nlz_w[l][z] + betaSum_l[l] + m0);
          m0++;
        }
      }
    }

    for (int z = 0; z < numTopics; z++) {
      plz[l][z] *= (ndl_s[document][l] + alpha) / (nd_s[document] + alphaSum) * 
        (ndlz_s[document][l][z] + gamma) / (ndl_s[document][l]+gammaSum);
    }
  }

  // accumulate multinomial parameters
	for (int l = 0; l < numSentiLabs; l++)  {
		for (int z = 0; z < numTopics; z++) {
			if (z==0)  {
			    if (l==0) continue;
		      else plz[l][z] += plz[l-1][numTopics-1]; // accumulate the sum of the previous array
			}
			else plz[l][z] += plz[l][z-1];
		}
	}

	// probability normalization
	u = ((double)rand() / RAND_MAX) * plz[numSentiLabs-1][numTopics-1];

	bool loopBreak=false;
	for (sentilab = 0; sentilab < numSentiLabs; sentilab++) {
		for (topic = 0; topic < numTopics; topic++) {
		    if (plz[sentilab][topic] > u) {
		        loopBreak = true;
		        break;
		    }
		}
		if (loopBreak == true) {
			break;
    }
	}

	if (sentilab == numSentiLabs) sentilab--; // to avoid over array boundary
  if (topic == numTopics) topic--;
}

void asummodel::computePild() {
  for (int l = 0; l < numSentiLabs; l++) {
    for (int d = 0; d < numDocs; d++) {
      pi_ld[l][d] = (ndl_s[d][l] + gamma) / (nd_s[d] + gammaSum);
    }
  }
}

void asummodel::computeThetalzd() {
  for (int l = 0; l < numSentiLabs; l++) {
    for (int z = 0; z < numTopics; z++) {
       for (int d = 0; d < numDocs; d++) {
        theta_lzd[l][z][d] = (ndlz_s[d][l][z] + alpha) / (ndl_s[d][l] + alphaSum);
      }
    }
  }
}

void asummodel::computePhilzw() {
  for (int l = 0; l < numSentiLabs; l++) {
    for (int z = 0; z < numTopics; z++) {
      for (int w = 0; w < vocabSize; w++) {
        phi_lzw[l][z][w] = (nlzw_w[l][z][w] + beta_lw[l][w]) / (nlz_w[l][z] + betaSum_l[l]);
      }
    }
  }
}
