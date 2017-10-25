#ifndef __LDA_H
#define __LDA_H

#include <RcppArmadillo.h>
#include <stdlib.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include <time.h>
#include <math.h>
#include <vector>
#include <algorithm>
#include <utility>
#include <numeric>

// [[Rcpp::depends(RcppArmadillo,RcppProgress)]]

class gibbslda {

public:
  gibbslda(void) {};
  ~gibbslda(void) {
    if (dfm) delete dfm;
  };

  int numiters;
  int numTopics;
  int numDocs;
  int vocabSize;

  double aveDocSize;
  double alpha_;

  arma::sp_imat * dfm;
  std::vector<int> docSizes;

  std::vector<std::vector<int> > topic_dw;

  // model counts
  std::vector<int> nt;
  std::vector<std::vector<int> > ntd;
  std::vector<std::vector<int> > ntw;

  //Posterior
  std::vector<double> pt;

  // hyperParameters
  std::vector<double> alphat;
  double alphaSum;
  double beta;
  double betaSum;

  //Result vectors
  std::vector<std::vector<double> > thetatd;
  std::vector<std::vector<double> > phitw;

  void init();
  void init_estimate();
  void estimate();

private:
  void init_parameters();
  void drawsample(int d, int w, int&topic);
  void computePhitw();
  void computeThetatd();

};

#endif
