## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(cppally)

## ----include=FALSE------------------------------------------------------------
# Helpers to compile all examples in debug mode
cpp_source <- function(..., code, debug = TRUE, env = parent.frame()){
  preamble <- c("#include <cppally.hpp>", "using namespace cppally;")
  code <- paste(c(preamble, code), collapse = "\n")
  cppally::cpp_source(debug = debug, env = env, code = code, ...)
}

# Helpers to source and display C++/R code
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
# as it's faster when building the vignette

examples <- c(
cpp_sum = '
[[cppally::register]]
r_dbl cpp_sum(r_vector<r_dbl> x){
    return x.reduce([](auto a, auto b){ return a + b; });
}
',
cpp_sum2 = '
[[cppally::register]]
r_dbl cpp_sum2(r_vector<r_dbl> x){
    return x.reduce(std::plus<>{});
}
', 
cpp_gcd = '
[[cppally::register]]
r_int cpp_gcd(r_vector<r_int> x){
    return x.reduce([](auto acc, auto curr){
        auto res = cppally::gcd(acc, curr); // cppally has its own NA-aware gcd
        if ( (res == 1).is_true() ){
            return done(res);
        } else {
            return keep(res);
        }
    });
}
  ',
cpp_cumsum = '
[[cppally::register]]
r_vector<r_dbl> cpp_cumsum(r_vector<r_dbl> x){
    return x.cumulative_reduce(std::plus<>{});
}
  ', 
cpp_any_all_na = '
[[cppally::register]]
bool cpp_any_na(r_vector<r_dbl> x){
    return x.reduce([](auto, auto curr){ return is_na(curr) ? done(true) : keep(false); }, /*init = */ false);
}

[[cppally::register]]
bool cpp_all_na(r_vector<r_dbl> x){
    return x.reduce([](auto, auto curr){ return is_na(curr) ? keep(true) : done(false); }, /*init = */ true);
}
  ', 
cpp_pmax = '
[[cppally::register]]
r_vector<r_dbl> cpp_pmax(r_vector<r_dbl> x, r_vector<r_dbl> y){
    return pmap([](auto a, auto b){
        return max(a, b);
    }, x, y);
}
  ', 
cpp_if_else = '
template <RVector T>
[[cppally::register]]
T cpp_if_else(r_vec<r_lgl> condition, T if_true, T if_false, T if_na){
    return pmap([](r_lgl condition_, auto yes, auto no, auto missing) {
        if (condition_.is_true()){
            return yes;
        } else if (condition_.is_false()){
            return no;
        } else {
            return missing;
        }
    }, condition, if_true, if_false, if_na);
}
',
cpp_seq_along = '
template <RVector T>
[[cppally::register]]
r_vector<r_int> cpp_seq_along(T x){
    return pmap_with_index([](int i, auto){ // 2nd arg included so function can compile
        return r_int(i + 1); // R is 1-indexed
    }, x);
}
',
cpp_lag = '
[[cppally::register]]
r_vector<r_int> cpp_lag(r_vector<r_int> x, int k){
    return pmap_with_shift([&](auto a){
        return lag(a, k);
    }, x);
}
', 
cpp_diff = '
[[cppally::register]]
r_vector<r_dbl> cpp_diff(r_vector<r_dbl> x){
    return pmap_with_shift([&](auto a){
        return curr(a) - lag(a);
    }, x);
}
', 
cpp_in_place_abs = '
[[cppally::register]]
r_vector<r_dbl> cpp_in_place_abs(r_vector<r_dbl>& x){
    x.apply([](auto a){ return abs(a);});
    return x;
}
', 
cpp_in_place_lag = '
[[cppally::register]]
r_vector<r_dbl> cpp_in_place_lag(r_vector<r_dbl>& x, int k){
    x.shift(k);
    return x;
}
', 
cpp_add2 = '
[[cppally::register]]
r_vector<r_int> cpp_add2(r_vector<r_int> x, r_vector<r_int> y){
    return pmap_simd([](auto a, auto b){ return a + b; }, x, y);
}
',
cpp_pythagorean_theorem = '
[[cppally::register]]
r_vector<r_dbl> cpp_pythagorean_theorem(r_vector<r_dbl> a, r_vector<r_dbl> b){
    return (a * a) + (b * b); // Pythagorean theorem - a^2 + b^2 = c^2
}
',
cpp_rounding = '
[[cppally::register]]
r_vector<r_dbl> cpp_round(r_vector<r_dbl> x, r_vector<r_dbl> digits){
    return pmap([](auto a, auto b){ return round(a, b); }, x, digits);
}
[[cppally::register]]
r_vector<r_dbl> cpp_floor(r_vector<r_dbl> x){
    return pmap([](auto a){ return floor(a); }, x);
}
[[cppally::register]]
r_vector<r_dbl> cpp_ceiling(r_vector<r_dbl> x){
    return pmap([](auto a){ return ceiling(a); }, x);
}
'
)


cpp_source(code = paste(examples, collapse = "\n"), debug = TRUE)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_sum"]])

## -----------------------------------------------------------------------------
cpp_sum(1:10)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_sum2"]])

## -----------------------------------------------------------------------------
cpp_sum2(1:10)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_any_all_na"]])

## -----------------------------------------------------------------------------
x <- c(1, 2, NA, 4, 5)
cpp_any_na(x)
cpp_all_na(x)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_gcd"]])

## -----------------------------------------------------------------------------
cpp_gcd(c(5L, 25L, 125L))
cpp_gcd(c(5L, 25L, 1L, 125L))

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_cumsum"]])

## -----------------------------------------------------------------------------
cpp_cumsum(1:10)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_pmax"]])

## -----------------------------------------------------------------------------
x <- c(10, 20, 30)
y <- c(10, 50, 0)
cpp_pmax(x, y)

# pmap also recycles vectors

cpp_pmax(x, 15)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_if_else"]])

## -----------------------------------------------------------------------------
cpp_if_else(c(TRUE, FALSE, NA), "yes", "no", "missing")

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_seq_along"]])

## -----------------------------------------------------------------------------
cpp_seq_along(letters)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_lag"]])

## -----------------------------------------------------------------------------
# Lags
cpp_lag(1:10, k = 1)
cpp_lag(1:10, k = 2)
cpp_lag(1:10, k = 3)

# Leads
cpp_lag(1:10, k = -1)
cpp_lag(1:10, k = -2)
cpp_lag(1:10, k = -3)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_diff"]])

## -----------------------------------------------------------------------------
cpp_diff(1:10)
cpp_diff(seq(10, 100, by = 5))

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_in_place_abs"]])

## -----------------------------------------------------------------------------
x <- c(-20, -10)
cpp_in_place_abs(x)

x # Modified in-place

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_in_place_lag"]])

## -----------------------------------------------------------------------------
x <- c(1, 2, 3, 4, 5)
cpp_in_place_lag(x, k = 1)
x # lagged in-place

# keep lagging until we run out of elements to lag
cpp_in_place_lag(x, k = 1)
cpp_in_place_lag(x, k = 1)
cpp_in_place_lag(x, k = 1)
cpp_in_place_lag(x, k = 1)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_add2"]])

## -----------------------------------------------------------------------------
cpp_add2(1:5, 10)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_pythagorean_theorem"]])

## -----------------------------------------------------------------------------
cpp_pythagorean_theorem(1:10, 10:1)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_rounding"]])

## -----------------------------------------------------------------------------
x <- seq(-2, 2, by = 0.5)
cpp_round(x, digits = 0)
cpp_floor(x)
cpp_ceiling(x)

