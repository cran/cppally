## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = FALSE----------------------------------------------------------
required <- c("bench", "brio", "callr", "cli", "cpp11",
              "decor", "desc", "glue", "purrr", "readr",
              "stringr", "utils", "vctrs", "withr")
if (!all(vapply(required, requireNamespace, logical(1), quietly = TRUE))) {
  knitr::opts_chunk$set(eval = FALSE)
  knitr::knit_exit()
}

## ----include=FALSE------------------------------------------------------------
library(cppally)

## ----include=FALSE------------------------------------------------------------
cpp_source <- function(..., code, debug = FALSE, env = parent.frame()){
  preamble <- c("#include <cppally.hpp>", "using namespace cppally;")
  code <- paste(c(preamble, code), collapse = "\n")
  cppally::cpp_source(debug = debug, env = env, code = code, ...)
}
chunk_impl <- function(x, language){
  paste0("```", language, "\n", x, "\n```\n")
}
as_code_chunk <- function(x, language){
  cat(chunk_impl(x, language))
}
as_cpp_chunk <- function(x){
  as_code_chunk(x, "cpp")
}

## ----include=FALSE------------------------------------------------------------
# Compile necessary examples in one-go
# All examples are benchmarks so debug = FALSE

# cppally-only examples
examples <- c(
  bench_protect_insert_release_cppally = '
#include <cppally.hpp>
using namespace cppally;
#include <chrono>

[[cppally::register]]
double bench_protect_insert_release_cppally(int n) {
  SEXP dummy = Rf_ScalarInteger(42);
  R_PreserveObject(dummy);
  auto start = std::chrono::high_resolution_clock::now();
  for (int i = 0; i < n; ++i) {
    r_sexp x(dummy); // insert into pool
  }           // destructor → release from pool
  auto end = std::chrono::high_resolution_clock::now();
  R_ReleaseObject(dummy);
  double ns = std::chrono::duration<double, std::nano>(end - start).count();
  return ns / n; // nanoseconds per insert+release cycle
}
',
  bench_protect_copy_cppally = '
#include <cppally.hpp>
using namespace cppally;
#include <chrono>

[[cppally::register]]
double bench_protect_copy_cppally(int n) {
  SEXP dummy = Rf_ScalarInteger(42);
  r_sexp dummy2 = r_sexp(dummy);
  auto start = std::chrono::high_resolution_clock::now();
  for (int i = 0; i < n; ++i) {
    r_sexp x = dummy2; // Copy
  }
  auto end = std::chrono::high_resolution_clock::now();
  double ns = std::chrono::duration<double, std::nano>(end - start).count();
  return ns / n; // nanoseconds per copy
}
',
  cppally_na_count = '
[[cppally::register]]
int cppally_na_count(r_vec<r_str> x){
 r_size_t n = x.length();
 int na_count = 0;
 for (r_size_t i = 0; i < n; ++i){
  r_str str = x.get(i); // r_str protects the underlying CHARSXP
  na_count += is_na(str);
 }
 return na_count;
}
',
  cppally_fast_na_count = '
[[cppally::register]]
int cppally_fast_na_count(r_vec<r_str_view> x){
 r_size_t n = x.length();
 int na_count = 0;
 for (r_size_t i = 0; i < n; ++i){
  r_str_view str = x.get(i); // `r_str_view` does NOT re-protect the underlying CHARSXP
  na_count += is_na(str);
 }
 return na_count;
}
',
  cppally_fast_na_count_v2 = '
[[cppally::register]]
int cppally_fast_na_count_v2(r_vec<r_str> x){
 r_size_t n = x.length();
 int na_count = 0;
 for (r_size_t i = 0; i < n; ++i){
  // view() is safe in a short-lived read-only context
  na_count += is_na(x.view(i));
 }
 return na_count;
}
'
)

# cpp11-using examples (and the R C API baseline). Compiled separately so the
# cpp11 includes / linking_to / namespace usage stays out of the cppally-only
# translation unit.
cpp11_examples <- c(
  bench_protect_insert_release_cpp11 = '
#include <cpp11.hpp>

[[cppally::linking_to("cpp11")]]

using namespace cpp11;
#include <chrono>

[[cppally::register]]
double bench_protect_insert_release_cpp11(int n) {
  SEXP dummy = Rf_ScalarInteger(42);
  R_PreserveObject(dummy);
  auto start = std::chrono::high_resolution_clock::now();
  for (int i = 0; i < n; ++i) {
    sexp x(dummy); // insert into pool
  }           // destructor → release from pool
  auto end = std::chrono::high_resolution_clock::now();
  R_ReleaseObject(dummy);
  double ns = std::chrono::duration<double, std::nano>(end - start).count();
  return ns / n; // nanoseconds per insert+release cycle
}
',
  bench_protect_copy_cpp11 = '

#include <cpp11.hpp>
[[cppally::linking_to("cpp11")]]

using namespace cpp11;
#include <chrono>

[[cppally::register]]
double bench_protect_copy_cpp11(int n) {
  SEXP dummy = Rf_ScalarInteger(42);
  sexp dummy2 = sexp(dummy);

  auto start = std::chrono::high_resolution_clock::now();
  for (int i = 0; i < n; ++i) {
    sexp x = dummy2; // Copy
  }
  auto end = std::chrono::high_resolution_clock::now();

  double ns = std::chrono::duration<double, std::nano>(end - start).count();
  return ns / n;  // nanoseconds per copy
}
',
  C_na_count = '
// Pure R C API NA count - As fast as it can reasonably get
[[cppally::register]] // Registered via cppally for convenience
int C_na_count(SEXP x){
 r_size_t n = Rf_xlength(x);
 int na_count = 0;
 const SEXP *p_x = STRING_PTR_RO(x);
 for (r_size_t i = 0; i < n; ++i){
  SEXP str = p_x[i]; // No protection so no extra overhead
  na_count += str == NA_STRING;
 }
 return na_count;
}
',
  cpp11_na_count = '
#include <cpp11.hpp>
[[cppally::linking_to("cpp11")]]

[[cppally::register]]
int cpp11_na_count(SEXP x){
  using namespace cpp11;
  strings x_(x);
  R_xlen_t n = x_.size();

  int na_count = 0;

  for (R_xlen_t i = 0; i < n; ++i){
    r_string str = x_[i]; // r_string protects the underlying CHARSXP
    na_count += cpp11::is_na(str);
  }
  return na_count;
}
'
)

# Display-only snippets (no [[cppally::register]] tag, so not exposed to R).
# Compiled in their own batch since they are illustrative rather than benchmarked.
display_only <- c(
  view_good = '
bool good(r_str x){
  r_str_view str = x;
  return is_na(str); // str only used for temporary read-only context
}
',
  view_bad = '
r_str_view bad(){
  r_str new_str("I will be destroyed at the end of `bad()`");
  r_str_view bad_str = new_str; // A view of new_str
  return bad_str; // Points to underlying CHARSXP but nothing protecting it
}
'
)

cpp_source(code = paste(examples, collapse = "\n"))
cpp_source(code = paste(cpp11_examples, collapse = "\n"))
cpp_source(code = paste(display_only, collapse = "\n"))

## -----------------------------------------------------------------------------
library(cppally)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(cpp11_examples[["bench_protect_insert_release_cpp11"]])

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["bench_protect_insert_release_cppally"]])

## -----------------------------------------------------------------------------
insert_release_cpp11 <- replicate(10^4, bench_protect_insert_release_cpp11(10^4)) 
mean(insert_release_cpp11)
insert_release_cppally <- replicate(10^4, bench_protect_insert_release_cppally(10^4))
mean(insert_release_cppally)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(cpp11_examples[["bench_protect_copy_cpp11"]])

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["bench_protect_copy_cppally"]])

## -----------------------------------------------------------------------------
copy_sexp_cpp11 <- replicate(10^4, bench_protect_copy_cpp11(10^4))
mean(copy_sexp_cpp11)
copy_sexp_cppally <- replicate(10^4, bench_protect_copy_cppally(10^4))
mean(copy_sexp_cppally)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(cpp11_examples[["C_na_count"]])
as_cpp_chunk(cpp11_examples[["cpp11_na_count"]])
as_cpp_chunk(examples[["cppally_na_count"]])

## -----------------------------------------------------------------------------
set.seed(42)
x <- sample(letters, 10^5, TRUE)
x[sample.int(length(x), 10^3)] <- NA

## -----------------------------------------------------------------------------
library(bench)
mark(C_na_count(x))

## -----------------------------------------------------------------------------
mark(cpp11_na_count(x))

## -----------------------------------------------------------------------------
mark(cppally_na_count(x))

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cppally_fast_na_count"]])

## -----------------------------------------------------------------------------
mark(cppally_fast_na_count(x))

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cppally_fast_na_count_v2"]])

## -----------------------------------------------------------------------------
mark(cppally_fast_na_count_v2(x))

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(display_only[["view_good"]])

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(display_only[["view_bad"]])

