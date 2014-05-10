library(methods)
for (f in dir(pattern="\\.cpp$")) {
  message("*** Running ", f)
  Rcpp::sourceCpp(f, verbose=TRUE)
}
