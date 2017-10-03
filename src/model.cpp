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


#include "model.h"
#include <Rcpp.h>
using namespace std;


model::model(void) {
	numTopics = 50;
	numSentiLabs = 3;
	vocabSize = 0;
	numDocs = 0;
	corpusSize = 0;
	aveDocLength = 0;

	niters = 1000;
	liter = 0;
	updateParaStep = 40;

	_alpha  = -1.0;
	_beta = -1.0;
	_gamma = -1.0;

	putils = new utils();
}


model::~model(void) {
	if (putils) delete putils;
	nd.clear();
	ndlz.clear();
	ndlz.clear();
	nlzw.clear();
	p.clear();
	z.clear();
	l.clear();
	pi_dl.clear();
	theta_dlz.clear();
	alpha_lz.clear();
	beta_lzw.clear();
	alphaSum_l.clear();
	betaSum_lz.clear();
	gamma_dl.clear();
	gammaSum_d.clear();
	lambda_lw.clear();
	opt_alpha_lz.clear();
	vector<int>().swap(nd);
	vector<vector<int> >().swap(ndl);
	vector<vector<vector<int> > >().swap(ndlz);
	vector<vector<vector<int> > >().swap(nlzw);
	vector<vector<int> >().swap(nlz);
	vector<vector<double> >().swap(p);
	vector<vector<int> >().swap(z);
	vector<vector<int> >().swap(l);
	vector<vector<double> >().swap(pi_dl);
	vector<vector<vector<double> > >().swap(theta_dlz);
	vector<vector<vector<double> > >().swap(phi_lzw);
	vector<vector<double> >().swap(alpha_lz);
	vector<double>().swap(alphaSum_l);
	vector<vector<vector<double> > >().swap(beta_lzw);
	vector<vector<double> >().swap(betaSum_lz);
	vector<vector<double> >().swap(gamma_dl);
	vector<double>().swap(gammaSum_d);
	vector<vector<double> >().swap(lambda_lw);
	vector<vector<double> >().swap(opt_alpha_lz);
}


void model::init(int numSentiLabs,
    int numTopics,
    int niters,
    int updateParaStep,
    double alpha_,
    double beta_,
    double gamma_,
		Rcpp::List& corpus,
		Rcpp::List& sentiLexList) {

	putils->parse_args_est(numSentiLabs,
	  numTopics,
	  niters,
	  updateParaStep,
	  alpha_,
	  beta_,
	  gamma_,
		this);

		init_dataset(corpus, sentiLexList);
}

vector<vector<double> > model::estimatesPi() {
	return pi_dl;
}

vector<vector<vector<double> > > model::estimatesTheta() {
	return theta_dlz;
}
vector<vector<vector<double> > > model::estimatesPhi() {
	return phi_lzw;
}

int model::execute_model() {
	word2atr = pdataset->word2atr;
	sentiLex = pdataset->sentiLex;

	init_model_parameters();

	if (init_estimate()) return 1;
	if(estimate()) return 1;
	delete_model_parameters();
	return 0;
}

void model::init_dataset(Rcpp::List& corpus, Rcpp::List& sentiLexList) {
	pdataset = new dataset();
	pdataset->read_senti_lexicon(sentiLexList);
	pdataset->read_corpus(corpus);
}


int model::init_model_parameters()
{
	numDocs = pdataset->numDocs;
	vocabSize = pdataset->vocabSize;
	corpusSize = pdataset->corpusSize;
	aveDocLength = pdataset->aveDocLength;

	// model counts
	nd.resize(numDocs);
	for (int m = 0; m < numDocs; m++) {
		nd[m]  = 0;
	}

	ndl.resize(numDocs);
	for (int m = 0; m < numDocs; m++) {
		ndl[m].resize(numSentiLabs);
		for (int l = 0; l < numSentiLabs; l++)
		    ndl[m][l] = 0;
	}

	ndlz.resize(numDocs);
	for (int m = 0; m < numDocs; m++) {
		ndlz[m].resize(numSentiLabs);
		for (int l = 0; l < numSentiLabs; l++) {
			ndlz[m][l].resize(numTopics);
			for (int z = 0; z < numTopics; z++)
				ndlz[m][l][z] = 0;
		}
	}

	nlzw.resize(numSentiLabs);
	for (int l = 0; l < numSentiLabs; l++) {
		nlzw[l].resize(numTopics);
		for (int z = 0; z < numTopics; z++) {
			nlzw[l][z].resize(vocabSize);
			for (int r = 0; r < vocabSize; r++)
			    nlzw[l][z][r] = 0;
		}
	}

	nlz.resize(numSentiLabs);
	for (int l = 0; l < numSentiLabs; l++) {
		nlz[l].resize(numTopics);
		for (int z = 0; z < numTopics; z++) {
		    nlz[l][z] = 0;
		}
	}

	// posterior P
	p.resize(numSentiLabs);
	for (int l = 0; l < numSentiLabs; l++) {
		p[l].resize(numTopics);
	}

	// model parameters
	pi_dl.resize(numDocs);
	for (int m = 0; m < numDocs; m++) {
		pi_dl[m].resize(numSentiLabs);
	}

	theta_dlz.resize(numDocs);
	for (int m = 0; m < numDocs; m++) {
		theta_dlz[m].resize(numSentiLabs);
		for (int l = 0; l < numSentiLabs; l++) {
			theta_dlz[m][l].resize(numTopics);
		}
	}

	phi_lzw.resize(numSentiLabs);
	for (int l = 0; l < numSentiLabs; l++) {
		phi_lzw[l].resize(numTopics);
		for (int z = 0; z < numTopics; z++) {
			phi_lzw[l][z].resize(vocabSize);
		}
	}

	// init hyperparameters
	alpha_lz.resize(numSentiLabs);
	for (int l = 0; l < numSentiLabs; l++) {
		alpha_lz[l].resize(numTopics);
	}

	alphaSum_l.resize(numSentiLabs);

	if (_alpha <= 0) {
		_alpha =  (double)aveDocLength * 0.05 / (double)(numSentiLabs * numTopics);
	}

	for (int l = 0; l < numSentiLabs; l++) {
		alphaSum_l[l] = 0.0;
	    for (int z = 0; z < numTopics; z++) {
		    alpha_lz[l][z] = _alpha;
		    alphaSum_l[l] += alpha_lz[l][z];
	    }
	}

	opt_alpha_lz.resize(numSentiLabs);
	for (int l = 0; l < numSentiLabs; l++) {
		opt_alpha_lz[l].resize(numTopics);
	}

	//beta
	if (_beta <= 0) _beta = 0.01;

	beta_lzw.resize(numSentiLabs);
	betaSum_lz.resize(numSentiLabs);
	for (int l = 0; l < numSentiLabs; l++) {
		beta_lzw[l].resize(numTopics);
		betaSum_lz[l].resize(numTopics);
		for (int z = 0; z < numTopics; z++) {
			betaSum_lz[l][z] = 0.0;
			beta_lzw[l][z].resize(vocabSize);
			for (int r = 0; r < vocabSize; r++) {
				beta_lzw[l][z][r] = _beta;
			}
		}
	}

	// word prior transformation matrix
	lambda_lw.resize(numSentiLabs);
	for (int l = 0; l < numSentiLabs; l++) {
	    lambda_lw[l].resize(vocabSize);
		for (int r = 0; r < vocabSize; r++) {
			lambda_lw[l][r] = 1;
		}
	}

	// incorporate prior information into beta
	this->prior2beta();
	this->set_gamma();

	return 0;
}


int model::set_gamma() {

	if (_gamma <= 0 ) {
		_gamma = (double)aveDocLength * 0.05 / (double)numSentiLabs;
	}

	gamma_dl.resize(numDocs);
	gammaSum_d.resize(numDocs);

	for (int d = 0; d < numDocs; d++) {
		gamma_dl[d].resize(numSentiLabs);
		gammaSum_d[d] = 0.0;
		for (int l = 0; l < numSentiLabs; l++) {
			gamma_dl[d][l] = _gamma;
			gammaSum_d[d] += gamma_dl[d][l];
		}
	}

	return 0;
}


int model::prior2beta() {

	mapword2atr::iterator wordIt;
	mapword2prior::iterator sentiIt;

	for (sentiIt = sentiLex.begin(); sentiIt != sentiLex.end(); sentiIt++) {
		wordIt = word2atr.find(sentiIt->first);
		if (wordIt != word2atr.end()) {
			for (int j = 0; j < numSentiLabs; j++)  {
				lambda_lw[j][wordIt->second.id] = sentiIt->second.labDist[j];
			}
		}
	}

	for (int l = 0; l < numSentiLabs; l++) {
		for (int z = 0; z < numTopics; z++) {
			betaSum_lz[l][z] = 0.0;
		    for (int r = 0; r < vocabSize; r++) {
			    beta_lzw[l][z][r] = beta_lzw[l][z][r] * lambda_lw[l][r];
			    betaSum_lz[l][z] += beta_lzw[l][z][r];
		    }
		}
	}

	return 0;
}


void model::compute_phi_lzw() {

	for (int l = 0; l < numSentiLabs; l++)  {
	    for (int z = 0; z < numTopics; z++) {
			for(int r = 0; r < vocabSize; r++) {
				phi_lzw[l][z][r] = (nlzw[l][z][r] + beta_lzw[l][z][r]) / (nlz[l][z] + betaSum_lz[l][z]);
			}
		}
	}
}



void model::compute_pi_dl() {

	for (int m = 0; m < numDocs; m++) {
	    for (int l = 0; l < numSentiLabs; l++) {
		    pi_dl[m][l] = (ndl[m][l] + gamma_dl[m][l]) / (nd[m] + gammaSum_d[m]);
		}
	}
}

void model::compute_theta_dlz() {

	for (int m = 0; m < numDocs; m++) {
	    for (int l = 0; l < numSentiLabs; l++)  {
			for (int z = 0; z < numTopics; z++) {
			    theta_dlz[m][l][z] = (ndlz[m][l][z] + alpha_lz[l][z]) / (ndl[m][l] + alphaSum_l[l]);
			}
		}
	}
}

int model::init_estimate() {

  int sentiLab, topic;
	srand(time(0)); // initialize for random number generation
	z.resize(numDocs);
	l.resize(numDocs);

	for (int m = 0; m < numDocs; m++) {
		int docLength = pdataset->pdocs[m]->length;
		z[m].resize(docLength);
		l[m].resize(docLength);

        for (int t = 0; t < docLength; t++) {
		    if (pdataset->pdocs[m]->words[t] < 0) {
			    printf("ERROR! word token %d has index smaller than 0 at doc[%d][%d]\n", pdataset->pdocs[m]->words[t], m, t);
				return 1;
			}

    	    if ((pdataset->pdocs[m]->priorSentiLabels[t] > -1) && (pdataset->pdocs[m]->priorSentiLabels[t] < numSentiLabs)) {
			    sentiLab = pdataset->pdocs[m]->priorSentiLabels[t]; // incorporate prior information into the model

			}
			else {
			    sentiLab = (int)(((double)rand() / RAND_MAX) * numSentiLabs);
			    if (sentiLab == numSentiLabs) sentiLab = numSentiLabs -1;  // to avoid over array boundary
			}
    	    l[m][t] = sentiLab;

			// random initialize the topic assginment
			topic = (int)(((double)rand() / RAND_MAX) * numTopics);
			if (topic == numTopics)  topic = numTopics - 1; // to avoid over array boundary
			z[m][t] = topic;

			// model count assignments
			nd[m]++;
			ndl[m][sentiLab]++;
			ndlz[m][sentiLab][topic]++;
			nlzw[sentiLab][topic][pdataset->pdocs[m]->words[t]]++;
			nlz[sentiLab][topic]++;
        }
    }

    return 0;
}



int model::estimate() {

	int sentiLab, topic;
	mapname2labs::iterator it;

	printf("Sampling %d iterations!\n", niters);
	for (liter = 1; liter <= niters; liter++) {
	    printf("Iteration %d ...\n", liter);
		for (int m = 0; m < numDocs; m++) {
		    for (int n = 0; n < pdataset->pdocs[m]->length; n++) {
					sampling(m, n, sentiLab, topic);
					l[m][n] = sentiLab;
					z[m][n] = topic;
			}
		}

		if (updateParaStep > 0 && liter % updateParaStep == 0) {
			this->update_Parameters();
		}
	}

	compute_pi_dl();
	compute_theta_dlz();
	compute_phi_lzw();
	printf("Gibbs sampling completed!\n");

	return 0;
}


int model::sampling(int m, int n, int& sentiLab, int& topic) {

	sentiLab = l[m][n];
	topic = z[m][n];
	int w = pdataset->pdocs[m]->words[n]; // the ID/index of the current word token in vocabulary
	double u;

	nd[m]--;
	ndl[m][sentiLab]--;
	ndlz[m][sentiLab][topic]--;
	nlzw[sentiLab][topic][pdataset->pdocs[m]->words[n]]--;
	nlz[sentiLab][topic]--;

	// do multinomial sampling via cumulative method
	for (int l = 0; l < numSentiLabs; l++) {
		for (int k = 0; k < numTopics; k++) {
			p[l][k] = (nlzw[l][k][w] + beta_lzw[l][k][w]) / (nlz[l][k] + betaSum_lz[l][k]) *
		   		(ndlz[m][l][k] + alpha_lz[l][k]) / (ndl[m][l] + alphaSum_l[l]) *
				(ndl[m][l] + gamma_dl[m][l]) / (nd[m] + gammaSum_d[m]);
		}
	}

	// accumulate multinomial parameters
	for (int l = 0; l < numSentiLabs; l++)  {
		for (int k = 0; k < numTopics; k++) {
			if (k==0)  {
			    if (l==0) continue;
		        else p[l][k] += p[l-1][numTopics-1]; // accumulate the sum of the previous array
			}
			else p[l][k] += p[l][k-1];
		}
	}

	// probability normalization
	u = ((double)rand() / RAND_MAX) * p[numSentiLabs-1][numTopics-1];

	// sample sentiment label l, where l \in [0, S-1]
	bool loopBreak=false;
	for (sentiLab = 0; sentiLab < numSentiLabs; sentiLab++) {
		for (topic = 0; topic < numTopics; topic++) {
		    if (p[sentiLab][topic] > u) {
		        loopBreak = true;
		        break;
		    }
		}
		if (loopBreak == true) {
			break;
		}
	}

	if (sentiLab == numSentiLabs) sentiLab = numSentiLabs - 1; // to avoid over array boundary
	if (topic == numTopics) topic = numTopics - 1;

	// add estimated 'z' and 'l' to count variables
	nd[m]++;
	ndl[m][sentiLab]++;
	ndlz[m][sentiLab][topic]++;
	nlzw[sentiLab][topic][pdataset->pdocs[m]->words[n]]++;
	nlz[sentiLab][topic]++;

  return 0;
}


int model::update_Parameters() {

	int ** data; // temp valuable for exporting 3-dimentional array to 2-dimentional
	double * alpha_temp;
	data = new int*[numTopics];
	for (int k = 0; k < numTopics; k++) {
		data[k] = new int[numDocs];
		for (int m = 0; m < numDocs; m++) {
			data[k][m] = 0;
		}
	}

	alpha_temp = new double[numTopics];
	for (int k = 0; k < numTopics; k++){
		alpha_temp[k] = 0.0;
	}

	// update alpha
	for (int j = 0; j < numSentiLabs; j++) {
		for (int k = 0; k < numTopics; k++) {
			for (int m = 0; m < numDocs; m++) {
				data[k][m] = ndlz[m][j][k]; // ntldsum[j][k][m];
			}
		}

		for (int k = 0; k < numTopics; k++) {
			alpha_temp[k] =  alpha_lz[j][k]; //alpha[j][k];
		}

		polya_fit_simple(data, alpha_temp, numTopics, numDocs);

		// update alpha
		alphaSum_l[j] = 0.0;
		for (int k = 0; k < numTopics; k++) {
			alpha_lz[j][k] = alpha_temp[k];
			alphaSum_l[j] += alpha_lz[j][k];
		}
	}

	return 0;
}
