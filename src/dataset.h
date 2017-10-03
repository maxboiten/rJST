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


#ifndef	_DATASET_H
#define	_DATASET_H

#include <Rcpp.h>
#include "constants.h"
#include "document.h"
#include "map_type.h"
#include <string>
#include <vector>
#include <map>
#include <sstream>
using namespace std;


class dataset {

public:
  mapword2atr word2atr;
	mapword2prior sentiLex; // <word, polarity>

	document ** pdocs; // store training data vocab ID
	document ** _pdocs; // only use for inference, i.e., for storing the new/test vocab ID

	int numDocs;
	int aveDocLength; // average document length
	int vocabSize;
	int corpusSize;

	// functions
	dataset();
	~dataset(void);

	int read_newData(string filename);
	void read_senti_lexicon(Rcpp::List sentiLexiconFile);
  void read_corpus(Rcpp::List corpus);
	void analyzeCorpus();
  void add_doc(document * doc, int idx);

	int init_parameter();
	void deallocate();

};

#endif
