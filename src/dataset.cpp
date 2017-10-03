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

#include "dataset.h"
#include "document.h"
#include "model.h"
#include "map_type.h"
#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <stdlib.h>
using namespace std;


dataset::dataset() {
	pdocs = NULL;
	_pdocs = NULL;
	word2atr.clear();

	numDocs = 0;
	aveDocLength = 0;
	vocabSize = 0;
	corpusSize = 0;
}


dataset::~dataset(void) {
	deallocate();
}

void dataset::analyzeCorpus() {

	mapword2atr::iterator it;
	mapword2prior::iterator sentiIt;
	map<int,int>::iterator idIt;
	int lookupWord, docLength;

	for (int i = 0; i < numDocs; ++i) {
		docLength = pdocs[i]->length;
		for (int k = 0; k < docLength; k++) {

			int priorSenti = -1;
			lookupWord = pdocs[i]->words[k];
			it = word2atr.find(lookupWord);
			if (it == word2atr.end()) { //if so, this word is not part of word2atr yet.
				sentiIt = sentiLex.find(lookupWord); // check whether the word token can be found in the sentiment lexicon
				// incorporate sentiment lexicon
				if (sentiIt != sentiLex.end()) {
				    priorSenti = sentiIt->second.id;
				}

				// insert sentiment info into word2atr
				Word_atr temp = {lookupWord, priorSenti};  // vocabulary index; word polarity
				word2atr.insert(pair<int, Word_atr>(lookupWord, temp));
				pdocs[i]->priorSentiLabels[k] = priorSenti;
			}
			else { // word seen before
				pdocs[i]->priorSentiLabels[k] = it->second.polarity;
			}
		}
	}
	// update number of words
	vocabSize = word2atr.size();
}




void dataset::deallocate()
{
  if (pdocs) {
    for (int i = 0; i < numDocs; i++)
      delete pdocs[i];
    delete [] pdocs;
    pdocs = NULL;
  }

  if (_pdocs) {
    for (int i = 0; i < numDocs; i++)
      delete _pdocs[i];
    delete [] _pdocs;
    _pdocs = NULL;
  }
}

void dataset::add_doc(document * doc, int idx) {
    if (0 <= idx && idx < numDocs)  {pdocs[idx] = doc;}
}

void dataset::read_corpus(Rcpp::List corpus) {
	int docCount = 0;
	vector<int> docv;

	numDocs = corpus.size();

	if (pdocs) {
		deallocate();
		pdocs = new document*[numDocs];
	}
	else {
		pdocs = new document*[numDocs];
	}

	for (Rcpp::List::iterator it = corpus.begin(); it != corpus.end(); it++) {
		docv = *it;
		document * pdoc = new document(docv);

		add_doc(pdoc,docCount);
		vocabSize = max(vocabSize,*max_element(docv.begin(),docv.end()));
		corpusSize += docv.size();

		docCount++;
		docv.clear();
	}
	aveDocLength = corpusSize/numDocs;

	this->analyzeCorpus();
}

void dataset::read_senti_lexicon(Rcpp::List sentiLexList) {
	vector<double> sentiLexLine;
	int labID, wordID;
	vector<double> wordPrior;

	sentiLex.clear();

	for (Rcpp::List::iterator it = sentiLexList.begin(); it != sentiLexList.end(); it++) {
		sentiLexLine = *it;
		wordID = (int)sentiLexLine[0];
		
		for (int i = 1; i < (int)sentiLexLine.size();i++) {
			wordPrior.push_back(sentiLexLine[i]);
		}
		labID = 0;
		for (int i = 1; i < 3; i++) {

			if (wordPrior[i]>wordPrior[labID]) {
				labID = i;
			}
		}
		Word_Prior_Attr temp = {labID,wordPrior};
		sentiLex.insert(pair<int, Word_Prior_Attr >(wordID, temp));
		wordPrior.clear();
	}
}
