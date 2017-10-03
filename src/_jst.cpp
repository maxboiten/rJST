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

#include "Rcpp.h"
#include "model.h"
//#include "inference.h"
#include "utils.h"
#include "stdio.h"
#include "stdlib.h"
#include "time.h"
#include <iostream>
#include <fstream>
#include <map>
using namespace std;

model * jst;

string intToString (int a)
{
    ostringstream temp;
    temp.clear();
    temp<<a;
    return temp.str();
}

// [[Rcpp::export]]
Rcpp::List jstcpp(std::string model_status_str,
        Rcpp::List corpus,
        Rcpp::List sentiLexList,
        int numSentiLabs,
        int numTopics,
        int numiters,
        int updateParaStep,
        double alpha_,
        double beta_,
        double gamma_) {

	int model_status = MODEL_STATUS_UNKNOWN;
	utils *putils = new utils();
	model_status = putils->setModelStatus(model_status_str);

	if (putils)
		delete putils;

	if (model_status == MODEL_STATUS_EST){
    model * jst = new model();

		jst->init(numSentiLabs,numTopics,numiters,updateParaStep,alpha_, beta_, gamma_, corpus,sentiLexList);

		jst->execute_model();

    vector<vector<double> > pi =  jst->estimatesPi();
    vector<vector<vector<double> > > theta =  jst->estimatesTheta();
    vector<vector<vector<double> > > phi =  jst->estimatesPhi();

    return Rcpp::List::create(Rcpp::Named("pi") = pi,
                              Rcpp::Named("theta") = theta,
                              Rcpp::Named("phi") = phi);
	}

  //TODO: Possibly extend model with learning and inference

//	else if (model_status == MODEL_STATUS_INF) {
//		Inference jst;
//
//    jst.init();
//	}

	return 0;
}
