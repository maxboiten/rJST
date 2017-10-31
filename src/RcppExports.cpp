// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// jstcpp
Rcpp::List jstcpp(arma::sp_imat& dfm, Rcpp::IntegerVector& sentiWords, Rcpp::IntegerVector& sentiCategory, int numSentiLabs, int numTopics, int numiters, int updateParaStep, double alpha_, double beta_, double gamma_);
RcppExport SEXP _rJST_jstcpp(SEXP dfmSEXP, SEXP sentiWordsSEXP, SEXP sentiCategorySEXP, SEXP numSentiLabsSEXP, SEXP numTopicsSEXP, SEXP numitersSEXP, SEXP updateParaStepSEXP, SEXP alpha_SEXP, SEXP beta_SEXP, SEXP gamma_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_imat& >::type dfm(dfmSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type sentiWords(sentiWordsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type sentiCategory(sentiCategorySEXP);
    Rcpp::traits::input_parameter< int >::type numSentiLabs(numSentiLabsSEXP);
    Rcpp::traits::input_parameter< int >::type numTopics(numTopicsSEXP);
    Rcpp::traits::input_parameter< int >::type numiters(numitersSEXP);
    Rcpp::traits::input_parameter< int >::type updateParaStep(updateParaStepSEXP);
    Rcpp::traits::input_parameter< double >::type alpha_(alpha_SEXP);
    Rcpp::traits::input_parameter< double >::type beta_(beta_SEXP);
    Rcpp::traits::input_parameter< double >::type gamma_(gamma_SEXP);
    rcpp_result_gen = Rcpp::wrap(jstcpp(dfm, sentiWords, sentiCategory, numSentiLabs, numTopics, numiters, updateParaStep, alpha_, beta_, gamma_));
    return rcpp_result_gen;
END_RCPP
}
// jstcppreversed
Rcpp::List jstcppreversed(arma::sp_imat& dfm, Rcpp::IntegerVector& sentiWords, Rcpp::IntegerVector& sentiCategory, int numSentiLabs, int numTopics, int numiters, int updateParaStep, double alpha_, double beta_, double gamma_);
RcppExport SEXP _rJST_jstcppreversed(SEXP dfmSEXP, SEXP sentiWordsSEXP, SEXP sentiCategorySEXP, SEXP numSentiLabsSEXP, SEXP numTopicsSEXP, SEXP numitersSEXP, SEXP updateParaStepSEXP, SEXP alpha_SEXP, SEXP beta_SEXP, SEXP gamma_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_imat& >::type dfm(dfmSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type sentiWords(sentiWordsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type sentiCategory(sentiCategorySEXP);
    Rcpp::traits::input_parameter< int >::type numSentiLabs(numSentiLabsSEXP);
    Rcpp::traits::input_parameter< int >::type numTopics(numTopicsSEXP);
    Rcpp::traits::input_parameter< int >::type numiters(numitersSEXP);
    Rcpp::traits::input_parameter< int >::type updateParaStep(updateParaStepSEXP);
    Rcpp::traits::input_parameter< double >::type alpha_(alpha_SEXP);
    Rcpp::traits::input_parameter< double >::type beta_(beta_SEXP);
    Rcpp::traits::input_parameter< double >::type gamma_(gamma_SEXP);
    rcpp_result_gen = Rcpp::wrap(jstcppreversed(dfm, sentiWords, sentiCategory, numSentiLabs, numTopics, numiters, updateParaStep, alpha_, beta_, gamma_));
    return rcpp_result_gen;
END_RCPP
}
// gibbsldacpp
Rcpp::List gibbsldacpp(arma::sp_imat& dfm, int numTopics, int numiters, double alpha_, double beta_);
RcppExport SEXP _rJST_gibbsldacpp(SEXP dfmSEXP, SEXP numTopicsSEXP, SEXP numitersSEXP, SEXP alpha_SEXP, SEXP beta_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_imat& >::type dfm(dfmSEXP);
    Rcpp::traits::input_parameter< int >::type numTopics(numTopicsSEXP);
    Rcpp::traits::input_parameter< int >::type numiters(numitersSEXP);
    Rcpp::traits::input_parameter< double >::type alpha_(alpha_SEXP);
    Rcpp::traits::input_parameter< double >::type beta_(beta_SEXP);
    rcpp_result_gen = Rcpp::wrap(gibbsldacpp(dfm, numTopics, numiters, alpha_, beta_));
    return rcpp_result_gen;
END_RCPP
}
// sldacpp
Rcpp::List sldacpp(Eigen::SparseMatrix<int,Eigen::ColMajor>& sfm, Rcpp::NumericVector& documentVector, Rcpp::NumericVector& sentenceVector, int numTopics, int numiters, double alpha_, double beta_);
RcppExport SEXP _rJST_sldacpp(SEXP sfmSEXP, SEXP documentVectorSEXP, SEXP sentenceVectorSEXP, SEXP numTopicsSEXP, SEXP numitersSEXP, SEXP alpha_SEXP, SEXP beta_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<int,Eigen::ColMajor>& >::type sfm(sfmSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type documentVector(documentVectorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type sentenceVector(sentenceVectorSEXP);
    Rcpp::traits::input_parameter< int >::type numTopics(numTopicsSEXP);
    Rcpp::traits::input_parameter< int >::type numiters(numitersSEXP);
    Rcpp::traits::input_parameter< double >::type alpha_(alpha_SEXP);
    Rcpp::traits::input_parameter< double >::type beta_(beta_SEXP);
    rcpp_result_gen = Rcpp::wrap(sldacpp(sfm, documentVector, sentenceVector, numTopics, numiters, alpha_, beta_));
    return rcpp_result_gen;
END_RCPP
}
// asumcpp
Rcpp::List asumcpp(Eigen::SparseMatrix<int,Eigen::ColMajor>& sfm, Rcpp::IntegerVector& documentVector, Rcpp::IntegerVector& sentenceVector, Rcpp::IntegerVector& sentiWords, Rcpp::IntegerVector& sentiCategory, int numSentiLabs, int numTopics, int numiters, double alpha, double gamma, Rcpp::NumericVector& betaVec);
RcppExport SEXP _rJST_asumcpp(SEXP sfmSEXP, SEXP documentVectorSEXP, SEXP sentenceVectorSEXP, SEXP sentiWordsSEXP, SEXP sentiCategorySEXP, SEXP numSentiLabsSEXP, SEXP numTopicsSEXP, SEXP numitersSEXP, SEXP alphaSEXP, SEXP gammaSEXP, SEXP betaVecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<int,Eigen::ColMajor>& >::type sfm(sfmSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type documentVector(documentVectorSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type sentenceVector(sentenceVectorSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type sentiWords(sentiWordsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type sentiCategory(sentiCategorySEXP);
    Rcpp::traits::input_parameter< int >::type numSentiLabs(numSentiLabsSEXP);
    Rcpp::traits::input_parameter< int >::type numTopics(numTopicsSEXP);
    Rcpp::traits::input_parameter< int >::type numiters(numitersSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type betaVec(betaVecSEXP);
    rcpp_result_gen = Rcpp::wrap(asumcpp(sfm, documentVector, sentenceVector, sentiWords, sentiCategory, numSentiLabs, numTopics, numiters, alpha, gamma, betaVec));
    return rcpp_result_gen;
END_RCPP
}
// topNwordSeeds
Rcpp::NumericVector topNwordSeeds(Rcpp::NumericVector& wordParameters, int N);
RcppExport SEXP _rJST_topNwordSeeds(SEXP wordParametersSEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type wordParameters(wordParametersSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(topNwordSeeds(wordParameters, N));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rJST_jstcpp", (DL_FUNC) &_rJST_jstcpp, 10},
    {"_rJST_jstcppreversed", (DL_FUNC) &_rJST_jstcppreversed, 10},
    {"_rJST_gibbsldacpp", (DL_FUNC) &_rJST_gibbsldacpp, 5},
    {"_rJST_sldacpp", (DL_FUNC) &_rJST_sldacpp, 7},
    {"_rJST_asumcpp", (DL_FUNC) &_rJST_asumcpp, 11},
    {"_rJST_topNwordSeeds", (DL_FUNC) &_rJST_topNwordSeeds, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_rJST(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
