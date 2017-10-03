/**********************************************************************
		        Joint Sentiment-Topic (JST) Model
***********************************************************************

(C) Copyright 2013, Chenghua Lin and Yulan He

Written by: Chenghua Lin, University of Aberdeen, chenghua.lin@abdn.ac.uk.
Part of code is from http://gibbslda.sourceforge.net/.

This file is part of JST implementation.

JST is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.

JST is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA

***********************************************************************/


#ifndef	_MODEL_H
#define	_MODEL_H

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/stat.h>
#include <math.h>
#include <sstream>
#include <iostream>
#include <vector>

#include "dataset.h"
#include "document.h"
#include "map_type.h"
#include "utils.h"
#include "math_func.h"
#include "polya_fit_simple.h"
#include "strtokenizer.h"

using namespace std;


class model {

public:
	model(void);
	~model(void);

  mapword2atr word2atr;
	mapword2prior sentiLex;

	int numTopics;
	int numSentiLabs;
	int niters;
	int liter;
	int updateParaStep;
	double _alpha;
	double _beta;
	double _gamma;

	// init functions
  void init(int numSentiLabs,
      int numTopics,
      int niters,
      int updateParaStep,
      double _alpha,
      double _beta,
      double _gamma,
      Rcpp::List& corpus,
      Rcpp::List& sentiLexList);
	int execute_model();

  vector<vector<double> > estimatesPi();
	vector<vector<vector<double> > > estimatesTheta();
	vector<vector<vector<double> > > estimatesPhi();

private:

	int numDocs;
	int vocabSize;
	int corpusSize;
	int aveDocLength;

	dataset * pdataset;
	utils * putils;

	// model counts
	vector<int> nd;
	vector<vector<int> > ndl;
	vector<vector<vector<int> > > ndlz;
	vector<vector<vector<int> > > nlzw;
	vector<vector<int> > nlz;

	// topic and label assignments
	vector<vector<double> > p;
	vector<vector<int> > z;
	vector<vector<int> > l;

	// model parameters
	vector<vector<double> > pi_dl; // size: (numDocs x L)
	vector<vector<vector<double> > > theta_dlz; // size: (numDocs x L x T)
	vector<vector<vector<double> > > phi_lzw; // size: (L x T x V)

	// hyperparameters
	vector<vector<double> > alpha_lz; // \alpha_tlz size: (L x T)
	vector<double> alphaSum_l;
	vector<vector<vector<double> > > beta_lzw; // size: (L x T x V)
	vector<vector<double> > betaSum_lz;
	vector<vector<double> > gamma_dl; // size: (numDocs x L)
	vector<double> gammaSum_d;
	vector<vector<double> > lambda_lw; // size: (L x V) -- for encoding prior sentiment information

	vector<vector<double> > opt_alpha_lz;  //optimal value, size:(L x T) -- for storing the optimal value of alpha_lz after fix point iteration

	/************************* Functions ***************************/
	int set_gamma();
	int init_model_parameters();
  void init_dataset(Rcpp::List& corpus, Rcpp::List& sentiLexList);

	inline int delete_model_parameters() {
		numDocs = 0;
		vocabSize = 0;
		corpusSize = 0;
		aveDocLength = 0;

		if (pdataset != NULL) {
			delete pdataset;
			pdataset = NULL;
		}

		return 0;
	}

	int init_estimate();
	int estimate();
	int prior2beta();
	int sampling(int m, int n, int& sentiLab, int& topic);

	// compute parameter functions
	void compute_pi_dl();
	void compute_theta_dlz();
	void compute_phi_lzw();

	// update parameter functions
	void init_parameters();
	int update_Parameters();
};

#endif
