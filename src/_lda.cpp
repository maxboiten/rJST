#include "_lda.h"

// [[Rcpp::export]]
Rcpp::List gibbsldacpp(arma::sp_imat& dfm,
        int numTopics,
        int numiters,
        double alpha_,
        double beta_) {

    gibbslda * lda = new gibbslda();

    lda->numTopics = numTopics;
    lda->numiters = numiters;
    lda->alpha_ = alpha_;
    lda->beta = beta_;
    lda->dfm = &dfm;

    lda->init();
    if (lda->estimate()) {
      return Rcpp::List();
    }

    return Rcpp::List::create(Rcpp::Named("phi") = lda->phitw,
                             Rcpp::Named("theta") = lda->thetatd);
}

void gibbslda::init() {

  numDocs = dfm->n_rows;
  vocabSize = dfm->n_cols;

  docSizes.resize(numDocs);
  for (int d = 0; d < numDocs; d++) {
    docSizes[d] = accu(dfm->row(d));
  }

  init_parameters();

  init_estimate();
}


void gibbslda::init_parameters() {

  //Topic allocation vector
  topic_dw.resize(numDocs);
  for (int d = 0; d < numDocs; d++) {
    topic_dw[d].resize(docSizes[d]);
  }

  //Count vectors
  nt.resize(numTopics);
  std::fill(nt.begin(),nt.end(),0);

  ntd.resize(numTopics);
  ntw.resize(numTopics);
  for (int t = 0; t < numTopics; t++) {
    ntd[t].resize(numDocs);
    std::fill(ntd[t].begin(),ntd[t].end(),0);

    ntw[t].resize(vocabSize);
    std::fill(ntw[t].begin(),ntw[t].end(),0);
  }

  //Posterior
  pt.resize(numTopics);

  //Hyperparameters
  alphat.resize(numTopics);
  std::fill(alphat.begin(),alphat.end(),alpha_);

  alphaSum = numTopics * alpha_;

  betaSum = vocabSize * beta;

  //Result vectors
  thetatd.resize(numTopics);
  phitw.resize(numTopics);
  for (int t = 0; t < numTopics; t++) {
    thetatd[t].resize(numDocs);
    phitw[t].resize(vocabSize);
  }

}

void gibbslda::init_estimate() {
  srand(time(NULL));

  int document, wordToken, topic;

  std::vector<int> locations(numDocs); //Track locations in document passed.
  std::fill(locations.begin(),locations.end(),0);

  for (arma::sp_imat::iterator it = dfm->begin(); it != dfm->end(); it++) {
    wordToken = it.col();
    document = it.row();

    for (int i = 0; i < (int)(*it); i++) {

      topic = rand() % numTopics;

      topic_dw[document][locations[document]] = topic;

      locations[document]++;

      nt[topic]++;
      ntd[topic][document]++;
      ntw[topic][wordToken]++;
    }
  }
}

int gibbslda::estimate() {
  int document, wordToken, topic;
  std::vector<int> locations(numDocs);

  Progress p(numiters,true);

  for (int iter = 1; iter <= numiters; iter++) {
    if (Progress::check_abort()) {
      Rcpp::Rcout << "Process aborted at iteration " << iter << std::endl;
      return 1;
    }
    
    std::fill(locations.begin(),locations.end(),0); //reset the locations

    for (arma::sp_imat::iterator it = dfm->begin(); it != dfm->end(); it++) {
      wordToken = it.col();
      document = it.row();

      for (int i = 0; i < (int)(*it); i++) {
        topic = topic_dw[document][locations[document]];

        //Remove word from counts
        nt[topic]--;
        ntd[topic][document]--;
        ntw[topic][wordToken]--;

        //sample from posterior. topic is changed by the method
        drawsample(document,wordToken,topic);

        topic_dw[document][locations[document]] = topic;

        //Add word back to counts with new labels
        nt[topic]++;
        ntd[topic][document]++;
        ntw[topic][wordToken]++;

        //update position within the document
        locations[document]++;
      }
    }
    p.increment();
  }

  computeThetatd();
  computePhitw();

  return 0;
}

void gibbslda::drawsample(int d, int w, int&topic) {
  double u;

  pt[0] = (alphat[0]+ntd[0][d])*(beta+ntw[0][w])/(betaSum+nt[0]);
  for (int t = 1; t < numTopics; t++) {
    pt[t] = pt[t-1] + (alphat[t]+ntd[t][d])*(beta+ntw[t][w])/(betaSum+nt[t]);
  }

  u = ((double)rand() / RAND_MAX) * pt[numTopics-1];

  for (topic = 0; topic < numTopics; topic++) {
    if (pt[topic] > u) {
      break;
    }
  }

  if (topic == numTopics) topic--;
}

void gibbslda::computeThetatd() {
  for (int t = 0; t < numTopics; t++) {
    for (int d = 0; d < numDocs; d++) {
      thetatd[t][d] = (ntd[t][d] + alphat[t]) / (docSizes[d] + alphaSum);
    }
  }
}

void gibbslda::computePhitw() {
  for (int t = 0; t < numTopics; t++) {
    for (int w = 0; w < vocabSize; w++) {
      phitw[t][w] = (ntw[t][w] + beta) / (nt[t] + betaSum);
    }
  }
}
