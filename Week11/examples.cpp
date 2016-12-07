#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double meanC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for (int i = 0; i < n; i++) {
    total += x(i);
  }
  return total;
}

// [[Rcpp::export]]
double csumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for (int i = 0; i < n; i++) {
    if (x[i] > 0) total += x[i];
  }
  return total;
}

// [[Rcpp::export]]
void fill0C(NumericVector x) {
  int n = x.size();
  for (int i = 0; i < n; i++) {
    if (x[i] <0) x[i] = 0;
  }
}

// [[Rcpp::export]]
NumericVector rowSumC(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(nrow);
  
  for (int i = 0; i < nrow; i++) {
    double total = 0;
    for (int j = 0; j < ncol; j++) {
      total += x(i, j);
    }
    out[i] = total;
  }
  return out;
}

double vacc3a(double age, bool female, bool ily){
  double p = 0.25 + 0.3 * 1 / (1 - exp(0.04 * age)) + 0.1 * ily;
  p = p * (female ? 1.25 : 0.75);
  p = std::max(p, 0.0);
  p = std::min(p, 1.0);
  return p;
}

// [[Rcpp::export]]
NumericVector vacc3(NumericVector age, LogicalVector female, 
                    LogicalVector ily) {
  int n = age.size();
  NumericVector out(n);
  
  for(int i = 0; i < n; ++i) {
    out[i] = vacc3a(age[i], female[i], ily[i]);
  }
  
  return out;
}