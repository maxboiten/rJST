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

#include <stdio.h>
#include <string>
#include <map>
#include <iostream>
#include <sstream>
#include "strtokenizer.h"
#include "utils.h"
#include "model.h"
//#include "inference.h"
#include "dataset.h"
#include <sys/types.h>
#include <sys/stat.h>

using namespace std;

#undef WINDOWS
#ifdef _WIN32
    #define WINDOWS
#endif
#ifdef __WIN32__
    #define WINDOWS
#endif

#ifdef WINDOWS
	#include <direct.h>  // For _mkdir().
	#include <io.h>      // For access().
#else
	#include <unistd.h>  // For access().
#endif


utils::utils() {
	model_status = MODEL_STATUS_UNKNOWN;
  model_name = "";

  numSentiLabs = 0;
	numTopics = 0;
  niters = 0;
  twords = 0;
	updateParaStep = -1;

	alpha = -1.0;
	beta = -1.0;
  gamma = -1.0;
}

int utils::setModelStatus(string&  model_status_str) {
    if (model_status_str == "est") {
			model_status = MODEL_STATUS_EST;
		}
		else if (model_status_str == "inf") {
			model_status = MODEL_STATUS_INF;
		}

	return model_status;
}

void utils::parse_args_est(int numSentiLabs,
    int numTopics,
    int niters,
    int updateParaStep,
    double alpha,
    double beta,
    double gamma,
    model *pmodel) {

	if (numSentiLabs > 0) pmodel->numSentiLabs = numSentiLabs;
	if (numTopics > 0) pmodel->numTopics = numTopics;
	if (niters > 0)  pmodel->niters = niters;
	pmodel->updateParaStep = updateParaStep; // -1: no parameter optimization

	if (alpha > 0.0) pmodel->_alpha = alpha;
	if (beta > 0.0) pmodel->_beta = beta;
	if (gamma > 0.0) pmodel->_gamma = gamma;
}


// Here was: int utils::parse_args_inf(int argc, char ** argv, Inference * pmodel_inf)
