#include "_jst.h"

//Class definition of model stored in header file

// [[Rcpp::export]]
Rcpp::List jstcpp(arma::sp_imat& dfm,
        Rcpp::List& sentiLexList,
        int numSentiLabs,
        int numTopics,
        int numiters,
        int updateParaStep,
        double alpha_,
        double beta_,
        double gamma_) {

    model * jst = new model();

    jst->numTopics = numTopics;
    jst->numSentiLabs = numSentiLabs;
    jst->numiters = numiters;
    jst->alpha_ = alpha_;
    jst->beta_ = beta_;
    jst->gamma_ = gamma_;
    jst->updateParaStep = updateParaStep;
    jst->dfm = &dfm;

    jst->init(sentiLexList);
    jst->estimate();

    return Rcpp::List::create(Rcpp::Named("pi") = jst->returnPi(),
                             Rcpp::Named("theta") = jst->returnTheta(),
                             Rcpp::Named("phi") = jst->returnPhi(),
                             Rcpp::Named("phi.termScores") = jst->termScores());
}

void model::init(Rcpp::List& sentiLexList) {
  std::vector<double> sentiLexEntry, priorProb;
  int wordToken;

  numDocs = dfm->n_rows;
  vocabSize = dfm->n_cols;

  docSizes.resize(numDocs);
  for (int d = 0; d < numDocs; d++) {
    docSizes[d] = (int)accu(dfm->row(d));
  }
  aveDocSize = (double)std::accumulate(docSizes.begin(),docSizes.end(),0)/(double)numDocs;

  for (Rcpp::List::iterator it = sentiLexList.begin(); it < sentiLexList.end(); it++) {
    sentiLexEntry = Rcpp::as<std::vector<double> >(*it);
    wordToken = (int) sentiLexEntry[0];
    std::copy(sentiLexEntry.begin()+1,sentiLexEntry.end(),priorProb.begin());
    sentiLex.insert(std::pair<int, std::vector<double> >(wordToken, priorProb));
    priorProb.clear();
  }

  init_parameters();

  init_estimate();
}

void model::init_parameters() {
  topic_dw.resize(numDocs);
  sent_dw.resize(numDocs);
  for (int d = 0; d < numDocs; d++) {
    topic_dw[d].resize(docSizes[d]);
    sent_dw[d].resize(docSizes[d]);
  }

  //model counts
  nd.resize(numDocs);
  ndl.resize(numDocs);
  ndlz.resize(numDocs);
  for (int d = 0; d < numDocs; d++) {
    nd[d] = 0;
    ndl[d].resize(numSentiLabs);
    ndlz[d].resize(numSentiLabs);
    for (int l = 0; l < numSentiLabs; l++) {
      ndl[d][l] = 0;
      ndlz[d][l].resize(numTopics);
      for (int z = 0; z < numTopics; z++) {
        ndlz[d][l][z] = 0;
      }
    }
  }

  nlzw.resize(numSentiLabs);
  nlz.resize(numSentiLabs);
  for (int l = 0; l < numSentiLabs; l++) {
    nlz[l].resize(numTopics);
    nlzw[l].resize(numTopics);
    for (int z = 0; z < numTopics; z++) {
      nlz[l][z] = 0;
      nlzw[l][z].resize(vocabSize);
      for (int w = 0; w < vocabSize; w++) {
        nlzw[l][z][w] = 0;
      }
    }
  }

  //Posterior values
  p_lz.resize(numSentiLabs);
  for (int l = 0; l < numSentiLabs; l++) {
    p_lz[l].resize(numTopics);
  }

  //Hyperparameters
  set_alpha();
  set_beta(); //Asymmetric based on sentiment Lexicon
  set_gamma();

  //Result vectors
  pi_dl.resize(numDocs); // size: (numDocs x L)
	theta_dlz.resize(numDocs); // size: (numDocs x L x T)
  for (int d = 0; d < numDocs; d++) {
    pi_dl[d].resize(numSentiLabs);
    theta_dlz[d].resize(numSentiLabs);
    for (int l = 0; l < numSentiLabs; l++) {
      theta_dlz[d][l].resize(numTopics);
    }
  }

	phi_lzw.resize(numSentiLabs); // size: (L x T x V)
  for (int l = 0; l < numSentiLabs; l++) {
    phi_lzw[l].resize(numTopics);
    for (int z = 0; z < numTopics; z++) {
      phi_lzw[l][z].resize(vocabSize);
    }
  }

}

void model::init_estimate() {
  int document, wordToken, priorSent, topic, sentilab;
  std::map<int,std::vector<double> >::iterator sentiIt;

  std::vector<int> locations(numDocs);
  std::fill(locations.begin(),locations.end(),0);

  for (arma::sp_imat::iterator it = dfm->begin(); it != dfm->end(); it++) {
    wordToken = it.col();
    document = it.row();

    priorSent = -1;
    sentiIt = sentiLex.find(wordToken);

    if (sentiIt != sentiLex.end()) { //Word token is found in the Sentiment Lexicon!
      sentilab = std::distance(sentiIt->second.begin(),
                                std::max_element(sentiIt->second.begin(),sentiIt->second.end()));
    }

    for (int i = 0; i < (int)(*it); i++) {

      if (priorSent == -1) {
        sentilab = rand() % numSentiLabs;
      }
      topic = rand() % numTopics;

      topic_dw[document][locations[document]] = topic;
      sent_dw[document][locations[document]] = sentilab;

      locations[document]++;

      nd[document]++;
      ndl[document][sentilab]++;
      ndlz[document][sentilab][topic]++;
      nlzw[sentilab][topic][wordToken]++;
      nlz[sentilab][topic]++;
    }
  }
}

void model::estimate() {
  int document, wordToken, topic, sentilab;
  std::vector<int> locations(numDocs);

  for (int iter = 1; iter <= numiters; iter++) {

    std::fill(locations.begin(),locations.end(),0); //reset the locations

    for (arma::sp_imat::iterator it = dfm->begin(); it != dfm->end(); it++) {
      wordToken = it.col();
      document = it.row();

      for (int i = 0; i < (int)(*it); i++) {
        topic = topic_dw[document][locations[document]];
        sentilab = sent_dw[document][locations[document]];

        //Remove word from counts
        nd[document]--;
        ndl[document][sentilab]--;
        ndlz[document][sentilab][topic]--;
        nlzw[sentilab][topic][wordToken]--;
        nlz[sentilab][topic]--;

        //sample from bivariate distribution. topic and sentilab are changed by the method
        drawsample(document,wordToken,topic,sentilab);

        topic_dw[document][locations[document]] = topic;
        sent_dw[document][locations[document]] = sentilab;

        //Add word back to counts with new labels
        nd[document]++;
        ndl[document][sentilab]++;
        ndlz[document][sentilab][topic]++;
        nlzw[sentilab][topic][wordToken]++;
        nlz[sentilab][topic]++;

        //update position within the document
        locations[document]++;
      }
    }

    if (updateParaStep > 0 && iter % updateParaStep == 0) {
			this->update_Parameters();
		}
  }

  //Compute parameter values
  compute_pi_dl();
  compute_theta_dlz();
  compute_phi_lzw();
}

void model::drawsample(int d, int w, int& topic, int& sentilab) {
  double u;

  // do multinomial sampling via cumulative method
	for (int l = 0; l < numSentiLabs; l++) {
		for (int z = 0; z < numTopics; z++) {
			p_lz[l][z] = (nlzw[l][z][w] + beta_lzw[l][z][w]) / (nlz[l][z] + betaSum_lz[l][z])
                    * (ndlz[d][l][z] + alpha_lz[l][z]) / (ndl[d][l] + alphaSum_l[l])
                    * (ndl[d][l] + gamma_dl[d][l]) / (nd[d] + gammaSum_d[d]);
		}
  }

	// accumulate multinomial parameters
	for (int l = 0; l < numSentiLabs; l++)  {
		for (int z = 0; z < numTopics; z++) {
			if (z==0)  {
			    if (l==0) continue;
		        else p_lz[l][z] += p_lz[l-1][numTopics-1]; // accumulate the sum of the previous array
			}
			else p_lz[l][z] += p_lz[l][z-1];
		}
	}

	// probability normalization
	u = ((double)rand() / RAND_MAX) * p_lz[numSentiLabs-1][numTopics-1];

	// sample sentiment label l, where l \in [0, S-1]
	bool loopBreak=false;
	for (sentilab = 0; sentilab < numSentiLabs; sentilab++) {
		for (topic = 0; topic < numTopics; topic++) {
		    if (p_lz[sentilab][topic] > u) {
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

void model::set_gamma() {

	if (gamma_ <= 0 ) {
		gamma_ = (double)aveDocSize * 0.05 / (double)numSentiLabs;
	}

	gamma_dl.resize(numDocs);
	gammaSum_d.resize(numDocs);

	for (int d = 0; d < numDocs; d++) {
		gamma_dl[d].resize(numSentiLabs);
    gammaSum_d[d] = 0;
    for (int l = 0; l < numSentiLabs; l++) {
      gamma_dl[d][l] = gamma_;
      gammaSum_d[d] += gamma_;
    }
	}
}

void model::set_alpha() {

  //Set alpha to default
  if (alpha_ <= 0) {
		alpha_ =  (double)aveDocSize * 0.05 / (double)(numSentiLabs * numTopics);
	}

  alpha_lz.resize(numSentiLabs);
  alphaSum_l.resize(numSentiLabs);
	for (int l = 0; l < numSentiLabs; l++) {
		alpha_lz[l].resize(numTopics);
    alphaSum_l[l] = 0;
    for (int z = 0; z < numTopics; z++) {
      alpha_lz[l][z] = alpha_;
      alphaSum_l[l] += alpha_;
    }
	}
}

void model::set_beta() {
  std::map<int,std::vector<double> >::iterator sentiIt;
  std::vector<std::vector<double> > lambda_lw;

  if (beta_ <= 0) {
    beta_ = 0.01;
  }

	beta_lzw.resize(numSentiLabs);
	betaSum_lz.resize(numSentiLabs);
  lambda_lw.resize(numSentiLabs);
	for (int l = 0; l < numSentiLabs; l++) {
		beta_lzw[l].resize(numTopics);

    lambda_lw[l].resize(vocabSize);
    for (int w = 0; w < vocabSize; w++) {
      lambda_lw[l][w] = 1;
    }

    betaSum_lz[l].resize(numTopics);

		for (int z = 0; z < numTopics; z++) {
      betaSum_lz[l][z] = 0;

			beta_lzw[l][z].resize(vocabSize);
			for (int w = 0; w < vocabSize; w++) {
        beta_lzw[l][z][w] = beta_;
      }
		}
	}

  for (sentiIt = sentiLex.begin(); sentiIt != sentiLex.end(); sentiIt++) {
    //For each entry of the sentiment lexicon:
		for (int j = 0; j < numSentiLabs; j++) {
			lambda_lw[j][sentiIt->first] = sentiIt->second[j];
		}
	}

  for (int l = 0; l < numSentiLabs; l++) {
		for (int z = 0; z < numTopics; z++) {
		    for (int w = 0; w < vocabSize; w++) {
			    beta_lzw[l][z][w] *= lambda_lw[l][w];
			    betaSum_lz[l][z] += beta_lzw[l][z][w];
		    }
		}
	}
}

void model::compute_phi_lzw() {
	for (int l = 0; l < numSentiLabs; l++)  {
	  for (int z = 0; z < numTopics; z++) {
			for(int w = 0; w < vocabSize; w++) {
				phi_lzw[l][z][w] = (nlzw[l][z][w] + beta_lzw[l][z][w]) / (nlz[l][z] + betaSum_lz[l][z]);
			}
		}
	}
}

void model::compute_pi_dl() {
	for (int d = 0; d < numDocs; d++) {
	    for (int l = 0; l < numSentiLabs; l++) {
		    pi_dl[d][l] = (ndl[d][l] + gamma_dl[d][l]) / (nd[d] + gammaSum_d[d]);
		}
	}
}

void model::compute_theta_dlz() {
	for (int d = 0; d < numDocs; d++) {
	  for (int l = 0; l < numSentiLabs; l++)  {
			for (int z = 0; z < numTopics; z++) {
			    theta_dlz[d][l][z] = (ndlz[d][l][z] + alpha_lz[l][z]) / (ndl[d][l] + alphaSum_l[l]);
			}
		}
	}
}

std::vector<std::vector<std::vector<double> > > model::termScores() {

  std::vector<std::vector<std::vector<double> > > termScores;

  std::vector<double> prod_w(vocabSize);
  std::fill(prod_w.begin(),prod_w.end(),1.0);

  for (int l = 0; l < numSentiLabs; l++) {
    for (int z = 0; z < numTopics; z++) {
      for (int w = 0; w < vocabSize; w++) {
        prod_w[w] *= phi_lzw[l][z][w];
      }
    }
  }

  for (int w = 0; w < vocabSize; w++) {
    prod_w[w] = pow(prod_w[w],1.0/(double)(numTopics*numSentiLabs));
  }

  termScores.resize(numSentiLabs);
  for (int l = 0; l < numSentiLabs; l++) {
    termScores[l].resize(numTopics);
    for (int z = 0; z < numTopics; z++) {
      termScores[l][z].resize(vocabSize);
      for (int w = 0; w < vocabSize; w++) {
        termScores[l][z][w] = phi_lzw[l][z][w] * log(phi_lzw[l][z][w]/prod_w[w]);
      }
    }
  }

  return termScores;
}

void model::update_Parameters() {

	int ** data; // temp valuable for exporting 3-dimentional array to 2-dimentional
	double * alpha_temp;
	data = new int*[numTopics];
	for (int z = 0; z < numTopics; z++) {
		data[z] = new int[numDocs];
		for (int d = 0; d < numDocs; d++) {
			data[z][d] = 0;
		}
	}

	alpha_temp = new double[numTopics];
	for (int z = 0; z < numTopics; z++){
		alpha_temp[z] = 0.0;
	}

	// update alpha
	for (int l = 0; l < numSentiLabs; l++) {
		for (int z = 0; z < numTopics; z++) {
			for (int d = 0; d < numDocs; d++) {
				data[z][d] = ndlz[d][l][z]; // ntldsum[j][k][m];
			}
		}

		for (int z = 0; z < numTopics; z++) {
			alpha_temp[z] =  alpha_lz[l][z]; //alpha[j][k];
		}

		polya_fit_simple(data, alpha_temp, numTopics, numDocs);

		// update alpha
		alphaSum_l[l] = 0.0;
		for (int z = 0; z < numTopics; z++) {
			alpha_lz[l][z] = alpha_temp[z];
			alphaSum_l[l] += alpha_lz[l][z];
		}
	}
}
