#ifndef __SLDA_H
#define __SLDA_H

#include "RcppEigen.h"
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <vector>
#include <algorithm>
#include <utility>
#include <numeric>

// [[Rcpp::depends(RcppEigen)]]

typedef std::pair<int,int> docSentencePair;
typedef Eigen::SparseMatrix<int,Eigen::RowMajor> SpMat;
typedef SpMat::InnerIterator rowit; //Just for readability.

class sldamodel {

public:
  sldamodel(void) {};
  ~sldamodel(void) {
    if (sfm) delete sfm;
  };

  int numiters;
  int numTopics;
  int numDocs;
  int numSentences;
  int vocabSize;

  //data
  SpMat * sfm;
  std::vector<int> sentenceSizes; //Counted in number of wordtokens
  std::vector<int> docSizes; //Counted in number of sentences
  std::vector<docSentencePair> documentSentenceVec; //Map that has sentence corpus row as key and
                                             //document number + sentence number within doc as ID

  std::vector<std::vector<int> > topic_ds;

  // model counts
  std::vector<std::vector<int> > ntd_s; //Number of sentences in topic t and document d
  std::vector<std::vector<int> > ntw_w; //Number of words w in topic t
  std::vector<int> nt_w; //Number of words in topic t

  //Posterior
  std::vector<double> pt;

  // hyperParameters
  double alpha;
  double beta;
  std::vector<double> alphaSum; //size = numDocs
  double betaSum; //size = numSentences

  //Result vectors
  std::vector<std::vector<double> > thetatd;
  std::vector<std::vector<double> > phitw;

  void init(Rcpp::NumericVector& documentVector, Rcpp::NumericVector& sentenceVector);
  void estimate();

private:
  void init_estimate();
  void init_parameters();
  int drawsample(int row, int document) ;
  void computePhitw();
  void computeThetatd();
};

#endif
