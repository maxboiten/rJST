#include "_slda.h"

/*
Notes.
1 - Contrary to the others, slda runs on RcppEigen. Armadillo has excellent iterators, but these
  are only efficient on a whole matrix, not on rows, which is what we need here. The eigen sparse 
  matrix offers a row-major implementation in which an inneriterator can be defined to iterate over the
  non-zero elements. This way we can iterate over rows efficiently, saving running the initialisation
  in under a second rather than over the course of several minutes (on a small test document set) 
  with occasional crashes.
2 - TODO: term scores?
*/

// [[Rcpp::export]]
Rcpp::List sldacpp(Eigen::SparseMatrix<int,Eigen::ColMajor>& sfm,
        Rcpp::NumericVector& documentVector,
        Rcpp::NumericVector& sentenceVector,
        int numTopics,
        int numiters,
        double alpha_,
        double beta_) {

    sldamodel * slda = new sldamodel();
  
    SpMat sfmChanged = sfm;

    slda->numTopics = numTopics;
    slda->numiters = numiters;
    slda->alpha = alpha_;
    slda->beta = beta_;
    slda->sfm = &sfmChanged;

    slda->init(documentVector,sentenceVector);
    if (slda->estimate()) {
      return Rcpp::List();
    }

    return Rcpp::List::create(Rcpp::Named("phi") = slda->phitw,
                             Rcpp::Named("theta") = slda->thetatd);
}

void sldamodel::init(Rcpp::NumericVector& documentVector, Rcpp::NumericVector& sentenceVector) {
  numSentences = sfm->rows();
  vocabSize = sfm->cols();

  sentenceSizes.resize(numSentences);
  for (int s = 0; s < numSentences; s++) {
    sentenceSizes[s] = sfm->innerVector(s).sum();
  }

  documentSentenceVec.resize(numSentences);
  docSentencePair temp;
  numDocs = 1;
  std::vector<int> docSentence;
  for (std::size_t row = 0; row < (std::size_t)documentVector.size(); row++) {
    temp = docSentencePair(documentVector[row]-1,sentenceVector[row]-1);
    if (temp.first >= numDocs) {
      numDocs = temp.first + 1;
    }
    documentSentenceVec[row] = temp;
  }

  docSizes.resize(numDocs);
  std::fill(docSizes.begin(),docSizes.end(),0);
  for (std::vector<docSentencePair>::iterator it = documentSentenceVec.begin(); it != documentSentenceVec.end(); it++) {
    docSizes[it->first]++; //Second element is pair which has document number as first element.
  }
  
  init_parameters();

  init_estimate();
}

void sldamodel::init_parameters() {
  //Set random seed
  srand(time(NULL));

  //Topic distribution
  topic_ds.resize(numDocs);
  for (int d = 0; d < numDocs; d++) {
    topic_ds[d].resize(docSizes[d]);
  }

  //Count variables
  ntd_s.resize(numTopics);
  ntw_w.resize(numTopics);
  nt_w.resize(numTopics);
  std::fill(nt_w.begin(),nt_w.end(),0);

  for (int t = 0; t < numTopics; t++) {
    ntd_s[t].resize(numDocs);
    std::fill(ntd_s[t].begin(),ntd_s[t].end(),0);
    ntw_w[t].resize(vocabSize);
    std::fill(ntw_w[t].begin(),ntw_w[t].end(),0);
  }

  //Posterior
  pt.resize(numTopics);

  //Hyperparameters
  alphaSum.resize(numDocs);
  for (int d = 0; d < numDocs; d++) {
    alphaSum[d] = alpha*docSizes[d];
  }

  betaSum = beta*vocabSize;

  //Result vectors
  thetatd.resize(numTopics);
  phitw.resize(numTopics);
  for (int t = 0; t < numTopics; t++) {
    thetatd[t].resize(numDocs);
    phitw[t].resize(vocabSize);
  }
}

void sldamodel::init_estimate() {
  int topic,document,sentence;


  for (int row = 0; row < numSentences; row++) {
    document = documentSentenceVec[row].first;
    sentence = documentSentenceVec[row].second;
    topic = rand() % numTopics;

    topic_ds[document][sentence] = topic;

    ntd_s[topic][document] ++;
    nt_w[topic] += sentenceSizes[row];
    for (rowit it(*sfm,row); it; ++it) {
      ntw_w[topic][it.col()] += it.value();
    }
  }
}

int sldamodel::estimate() {
  int newTopic,oldTopic,document,sentence;

  Progress p(numiters-1,true);

  for (int iter = 1; iter < numiters; iter++) {
    if (Progress::check_abort()) {
      Rcpp::Rcout << "Process aborted at iteration " << iter << std::endl;
      return 1;
    }
    for (int row = 0; row < numSentences; row++) {
      document = documentSentenceVec[row].first;
      sentence = documentSentenceVec[row].second;
      oldTopic = topic_ds[document][sentence];

      ntd_s[oldTopic][document]--;
      nt_w[oldTopic] -= sentenceSizes[row];
      for (rowit it(*sfm,row); it; ++it) {
        ntw_w[oldTopic][it.col()] -= it.value();
      }

      newTopic = drawsample(row, document);
      topic_ds[document][sentence] = newTopic;

      ntd_s[newTopic][document]++;
      nt_w[newTopic] += sentenceSizes[row];
      for (rowit it(*sfm,row); it; ++it) {
        ntw_w[newTopic][it.col()] += it.value();
      }
    }
    p.increment();
  }

  computePhitw();
  computeThetatd();

  return 0;
}

int sldamodel::drawsample(int row, int document) {
  int wordToken;
  int m0 = 0;
  double u;

  //Fill the posterior vector with ones to update by multiplication
  std::fill(pt.begin(),pt.end(),1);

  //Calculating the second term of the posterior
  for (rowit it(*sfm,row); it; ++it) {
    wordToken = it.col();

    for (int topic = 0; topic < numTopics; topic++) {
      for (int m = 0; m < it.value(); m++) {
        pt[topic] *= (ntw_w[topic][wordToken] + beta + m)/(nt_w[topic] + betaSum + m0);
        m0++;
      }
    }
  }

  //Multiply the second term of the posterior by the first term
  for (int topic = 0; topic < numTopics; topic++) {
    pt[topic] *= (ntd_s[topic][document]+alpha)/(docSizes[document] + alphaSum[document]);
  }

  //Sum posteriors cumulatiely for sampling.
  for (int topic = 1; topic < numTopics; topic++) {
    pt[topic] += pt[topic-1];
  }

  u = ((double)rand() / RAND_MAX) * pt[numTopics-1];

  int newTopic = 0;

  for (; newTopic < numTopics; newTopic++) {
    if (pt[newTopic] > u) {
      return newTopic;
    }
  }
  return newTopic-1;
}

void sldamodel::computeThetatd() {
  for (int t = 0; t < numTopics; t++) {
    for (int d = 0; d < numDocs; d++) {
      thetatd[t][d] = (ntd_s[t][d] + alpha) / (docSizes[d] + alphaSum[d]);
    }
  }
}

void sldamodel::computePhitw() {
  for (int t = 0; t < numTopics; t++) {
    for (int w = 0; w < vocabSize; w++) {
      phitw[t][w] = (ntw_w[t][w] + beta) / (nt_w[t] + betaSum);
    }
  }
}
