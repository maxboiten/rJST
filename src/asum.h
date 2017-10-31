#ifndef __ASUM_H
#define __ASUM_H

#include "RcppEigen.h"
#include <progress.hpp>
#include <progress_bar.hpp>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <vector>
#include <algorithm>
#include <utility>
#include <numeric>

// [[Rcpp::depends(RcppEigen,RcppProgress)]]

typedef Eigen::SparseMatrix<int,Eigen::RowMajor> SpMat;
typedef SpMat::InnerIterator rowit; //Just for readability.

class asummodel {

public:
  asummodel(void) {betaInput.resize(3);};
  ~asummodel(void) {
    if (sfm) delete sfm;
  };

  int numiters;
  int numTopics;
  int numSentiLabs;
  int numDocs;
  int numSentences;
  int vocabSize;

  //data
  SpMat * sfm;
  std::vector<int> sentenceSizes; //Counted in number of wordtokens
  std::vector<int> docSizes; //Counted in number of sentences
  std::vector<std::pair<int,int> > documentSentenceVec; //Map that has sentence corpus row as key and
                                             //document number + sentence number within doc as ID
  std::map<int,int> sentiLex;
  std::vector<std::vector<int> > topic_ds;
  std::vector<std::vector<int> > sent_ds;
  std::vector<int> sentencePriorSent;

  // model counts
  std::vector<std::vector<int> > ndl_s; //Number of sentences in document d and sentiment l
  std::vector<int> nd_s; //Numer of sentences in document d
  std::vector<std::vector<std::vector<int> > > ndlz_s; //Number of sentences in document d, sentiment l, topic z
  std::vector<std::vector<std::vector<int> > > nlzw_w; //Number of words w in sentiment l, topic z
  std::vector<std::vector<int> > nlz_w; //Number of words in sentiment l, topic z

  //Posterior
  std::vector<std::vector<double> > plz;

  // hyperParameters
  double alpha;
  double alphaSum;
  double gamma;
  double gammaSum;
  std::vector<double> betaInput;
  std::vector<std::vector<double > > beta_lw;
  std::vector<double > betaSum_l; //size = vocabSize

  //Result vectors
  std::vector<std::vector<double > > pi_ld;
  std::vector<std::vector<std::vector<double > > > theta_lzd;
  std::vector<std::vector<std::vector<double > > > phi_lzw;

  void init(Rcpp::IntegerVector& documentVector, 
    Rcpp::IntegerVector& sentenceVector,
    Rcpp::IntegerVector& sentiWords,
    Rcpp::IntegerVector& sentiCategory);
  int estimate();

private:
  void init_estimate();
  void init_parameters();
  void drawsample(int row, int document, int& topic, int& sentilab) ;
  void computePild();
  void computeThetalzd();
  void computePhilzw();
  void set_alpha();
  void set_beta();
  void set_gamma();
};

#endif
