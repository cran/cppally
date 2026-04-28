## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = FALSE
)

## ----include = FALSE----------------------------------------------------------
required <- c("bench", "brio", "callr", "cli", "decor",
              "desc", "glue", "purrr", "readr", "stringr",
              "utils", "vctrs", "withr")
if (!all(vapply(required, requireNamespace, logical(1), quietly = TRUE))) {
  knitr::opts_chunk$set(eval = FALSE)
  knitr::knit_exit()
}

## -----------------------------------------------------------------------------
library(cppally)

## ----include=FALSE------------------------------------------------------------
# Helpers to compile all examples in debug mode
cpp_source <- function(..., code, debug = TRUE, env = parent.frame()){
  preamble <- c("#include <cppally.hpp>", "using namespace cppally;")
  code <- paste(c(preamble, code), collapse = "\n")
  cppally::cpp_source(debug = debug, env = env, code = code, ...)
}
cpp_eval <- function(..., debug = TRUE, env = parent.frame()){
    cppally::cpp_eval(debug = debug, env = env, ...)
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

# Pre-register named single-line expressions so they can be referenced later
register_single_exprs <- function(exprs, env = parent.frame(), ...){
  if (is.null(names(exprs))){
    stop("`exprs` must be named")
  }
  utils::getFromNamespace("source_single_exprs", "cppally")(
    exprs, env = env, ...
    )
  wrappers <- setNames(
    lapply(seq_along(exprs), \(i) {
      fn <- get(paste0("f", i), envir = env)
      function() {
        out <- fn()
        if (out[["is_void"]]) invisible() else out$result
      }
    }),
    names(exprs)
  )
  list2env(wrappers, envir = env)
  invisible()
}

## ----include=FALSE------------------------------------------------------------
# Compile necessary examples in one-go
# as it's faster when building the vignette

examples <- c(
  hello_world = '
[[cppally::register]]
void hello_world(){
  print("Hello World!");
}',
  lgl_ops = '
[[cppally::register]]
r_vec<r_lgl> lgl_ops(){
  return make_vec<r_lgl>(
    r_true || r_false, // true
    r_true && r_false, // false
    r_na || r_true,    // true
    r_na && r_true,    // NA
    r_na && r_false,   // false
    r_na || r_na,      // NA
    r_na && r_na      // NA
  );
}
',
  bad_lgl_print = '
[[cppally::register]]
void bad_lgl_print(r_lgl condition){
  if (condition){
    print("true");
  } else {
    print("false");
  }
}
',
  good_lgl_print = '
[[cppally::register]]
void good_lgl_print(r_lgl condition){
  if (is_na(condition)){
    print("NA");
  } else if (condition){
    print("true");
  } else {
    print("false");
  }
}
',
  also_good_lgl_print = '
[[cppally::register]]
void also_good_lgl_print(r_lgl condition){
  if (condition.is_true()){
    print("true");
  } else {
    print("not true");
  }
}
',
  new_integer_vector = '
// Integer vector of size n
[[cppally::register]]
r_vec<r_int> new_integer_vector(int n){
  r_vec<r_int> int_vctr(n, /*fill = */ r_int(0));
  return int_vctr;
}
',
  all_vectors = '
[[cppally::register]]
r_vec<r_sexp> all_vectors(){
  return make_vec<r_sexp>(
    arg("logical") = r_vec<r_lgl>(),
    arg("integer") = r_vec<r_int>(),
    arg("integer64") = r_vec<r_int64>(), // Requires bit64
    arg("double") = r_vec<r_dbl>(),
    arg("character") = r_vec<r_str>(),
    arg("character") = r_vec<r_str_view>(),
    arg("raw") = r_vec<r_raw>(),
    arg("date") = r_vec<r_date>(),
    arg("date-time") = r_vec<r_psxct>(),
    arg("list") = r_vec<r_sexp>()
  );
}
',
  cpp_abs = '
template <RMathType T>
[[cppally::register]]
T cpp_abs(T x){
  if (is_na(x)) return na<T>();

  if (x < 0){
    return -x;
  } else {
    return x;
  }
}
',
  scalar_default = '
// Return the default constructor result of RScalar types

template <RScalar T>
[[cppally::register]]
T scalar_default(T ptype){
	return T();
}
',
  double_to_int = '
[[cppally::register]]
r_int double_to_int(r_dbl x){
  return as<r_int>(x);
}
',
  to_int_vec = '
[[cppally::register]]
r_vec<r_int> to_int_vec(r_vec<r_dbl> x){
  return as<r_vec<r_int>>(x);
}
',
  coercions = '
[[cppally::register]]
r_vec<r_sexp> coercions(){
    r_dbl a(4.2);
    r_vec<r_dbl> b = make_vec<r_dbl>(2.5);
    return make_vec<r_sexp>(
        as<r_vec<r_int>>(a),
        as<r_int>(a),
        as<r_int>(b),
        as<r_dbl>(b)
    );
}
',
  str_concatenate = '
[[cppally::register]]
r_str str_concatenate(r_str x, r_str y, r_str sep){
  std::string left = std::string(x.cpp_str());
  std::string right = std::string(y.cpp_str());
  std::string middle = std::string(sep.cpp_str());
  std::string combined = left + middle + right;
  return r_str(combined.c_str());
}
',
  new_list = '
using list = r_vec<r_sexp>;

[[cppally::register]]
list new_list(int n){
  return list(n);
}
',
  resize_all = '
[[cppally::register]]
r_vec<r_sexp> resize_all(r_vec<r_sexp> x, r_size_t n){
    r_size_t list_length = x.length();
    for (r_size_t i = 0; i < list_length; ++i){
        visit_vector(x.view(i), [&](auto vec) {
            x.set(i, vec.resize(n));
        });
    }
    return x;
}
',
  resize_all2 = '
[[cppally::register]]
r_vec<r_sexp> resize_all2(r_vec<r_sexp> x, r_size_t n){
    r_size_t list_length = x.length();
    for (r_size_t i = 0; i < list_length; ++i){
        visit_sexp(x.view(i), [&](auto vec) {
          using vec_t = decltype(vec); // type of object `vec`
          if constexpr (RVector<vec_t>){
            x.set(i, vec.resize(n));
          } else {
            abort("Cannot resize a non-vector");
          }
        });
    }
    return x;
}
',
  new_factor = '
[[cppally::register]]
r_factors new_factor(r_vec<r_str> x){
	return r_factors(x);
}
',
  factor_codes = '
static_assert(!RVector<r_factors>);

[[cppally::register]]
r_vec<r_int> factor_codes(r_factors x){
	return x.codes();
}
',
  list_as_df = '
[[cppally::register]]
r_vec<r_sexp> list_as_df(r_vec<r_sexp> x){

  r_size_t n = x.length();

  if (n_unique(x.lengths()) > 1){
    abort("List must have vectors of equal length to be converted to a data frame");
  }

  r_vec<r_str> names(attr::get_attr(x, cached_sym<"names">()));
  if (names.is_null()){
     abort("list must have names to be converted to a data frame");
  }

  r_vec<r_sexp> out = shallow_copy(x);

  int nrow = 0;
  r_vec<r_int> row_names;
  if (n > 0){
    nrow = out.view(0).length();
    row_names = make_vec<r_int>(na<r_int>(), -nrow);
  }

  attr::set_attr(out, cached_sym<"row.names">(), row_names);
  attr::set_attr(out, cached_sym<"class">(), make_vec<r_str>("data.frame"));
  return out;
}
'
)

# Benchmarks need debug = FALSE
benchmark_examples <- c(
  cpp_n_unique = '
template <RVector T>
[[cppally::register]]
r_int cpp_n_unique(T x){
  return as<r_int>(n_unique(x));
}
',
  primitive_sum = '
[[cppally::register]]
double primitive_sum(const r_vec<r_dbl>& x){

  // r_vec<T>::data_type always returns typename T
  using data_t = typename std::remove_cvref_t<decltype(x)>::data_type;

  using primitive_t = unwrap_t<data_t>;
  primitive_t *p_x = x.data();

  r_size_t n = x.length();
  double sum = 0;

  OMP_SIMD_REDUCTION1(+:sum)
  for (r_size_t i = 0; i < n; ++i){
    sum += p_x[i];
  }
  return sum;
}
'
)

cpp_source(code = paste(examples, collapse = "\n"), debug = TRUE)
cpp_source(code = paste(benchmark_examples, collapse = "\n"), debug = FALSE)

# Single-line expressions, pre-registered as R functions of the same name.
# Each can be invoked later as e.g. `r_true_val()` to get the evaluated result.
single_exprs <- c(
  r_true_val          = 'r_true',
  r_false_val         = 'r_false',
  r_na_val            = 'r_na',
  make_vec_dbl        = 'make_vec<r_dbl>(1, 1.5, 2, na<r_dbl>())',
  make_vec_dbl_named  = '
make_vec<r_dbl>(
    arg("first") = 1,
    arg("second") = 1.5,
    arg("third") = 2,
    arg("last") = na<r_dbl>()
  )
',
  make_vec_sexp       = 'make_vec<r_sexp>(1, 2, 3)',
  r_str_hello         = 'r_str("hello")',
  r_str_hello_c_str   = 'r_str("hello").c_str()',
  r_sym_new           = 'r_sym("new_symbol")',
  r_sym_from_str      = 'r_sym(r_str("symbol_from_string"))',
  cached_str_demo     = 'cached_str<"cached_string">()',
  cached_sym_demo     = 'cached_sym<"cached_symbol">()'
)

register_single_exprs(single_exprs, debug = TRUE)

## -----------------------------------------------------------------------------
hello_world()

## -----------------------------------------------------------------------------
cpp_eval('print("Hello World Again!")')

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(paste(
  single_exprs[["r_true_val"]],
  single_exprs[["r_false_val"]],
  single_exprs[["r_na_val"]],
  sep = "\n"
))

## ----echo=FALSE---------------------------------------------------------------
r_true_val()
r_false_val()
r_na_val()

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["lgl_ops"]])

## -----------------------------------------------------------------------------
lgl_ops()

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["bad_lgl_print"]])

## ----error=TRUE---------------------------------------------------------------
try({
bad_lgl_print(TRUE)
bad_lgl_print(FALSE)
bad_lgl_print(NA) # Can't implicitly convert NA to bool
})

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["good_lgl_print"]])

## -----------------------------------------------------------------------------
good_lgl_print(TRUE)
good_lgl_print(FALSE)
good_lgl_print(NA) # NA is handled explicitly so no issues

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["also_good_lgl_print"]])

## -----------------------------------------------------------------------------
also_good_lgl_print(TRUE)
also_good_lgl_print(FALSE)
also_good_lgl_print(NA) # Falls into 'not true' branch here as expected

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["new_integer_vector"]])

## -----------------------------------------------------------------------------
new_integer_vector(3)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["make_vec_dbl"]])

## ----echo=FALSE---------------------------------------------------------------
make_vec_dbl()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["make_vec_dbl_named"]])

## ----echo=FALSE---------------------------------------------------------------
make_vec_dbl_named()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["make_vec_sexp"]])

## ----echo=FALSE---------------------------------------------------------------
make_vec_sexp()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["all_vectors"]])

## -----------------------------------------------------------------------------
all_vectors()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["cpp_abs"]])

## -----------------------------------------------------------------------------
cpp_abs(-5)
cpp_abs(0)
cpp_abs(100)
cpp_abs(NA_real_)

## -----------------------------------------------------------------------------
cpp_abs(-3L)
cpp_abs(NA_integer_)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["scalar_default"]])

## -----------------------------------------------------------------------------
scalar_default(integer(1)) # Default is 0L
scalar_default(numeric(1)) # Default is 0.0
scalar_default(character(1)) # Default is ""

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["double_to_int"]])

## -----------------------------------------------------------------------------
double_to_int(pi)
double_to_int(NA_real_)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["to_int_vec"]])

## -----------------------------------------------------------------------------
to_int_vec(c(0, 1.5, NA))

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["coercions"]])

## -----------------------------------------------------------------------------
coercions()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_str_hello"]])

## ----echo=FALSE---------------------------------------------------------------
r_str_hello()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_str_hello_c_str"]])

## ----echo=FALSE---------------------------------------------------------------
r_str_hello_c_str()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["str_concatenate"]])

## -----------------------------------------------------------------------------
str_concatenate("hello", "how are you?", sep = ", ")

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_sym_new"]])

## ----echo=FALSE---------------------------------------------------------------
r_sym_new()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_sym_from_str"]])

## ----echo=FALSE---------------------------------------------------------------
r_sym_from_str()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["cached_str_demo"]])

## ----echo=FALSE---------------------------------------------------------------
cached_str_demo()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["cached_sym_demo"]])

## ----echo=FALSE---------------------------------------------------------------
cached_sym_demo()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["new_list"]])

## -----------------------------------------------------------------------------
new_list(0)
new_list(3)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["resize_all"]])

## -----------------------------------------------------------------------------
# Resize to size 1
resize_all(list(1:5, letters), n = 1)

## ----error=TRUE---------------------------------------------------------------
try({
resize_all(list(mean_fn = mean), 1)
})

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["resize_all2"]])

## -----------------------------------------------------------------------------
# Resize to size 1
resize_all2(list(1:5, letters), n = 1)

## ----error=TRUE---------------------------------------------------------------
try({
resize_all2(list(mean_fn = mean), n = 1)
})

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["new_factor"]])

## -----------------------------------------------------------------------------
new_factor(letters)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["factor_codes"]])

## -----------------------------------------------------------------------------
letter_fct <- new_factor(letters)

letter_fct |>
    factor_codes()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["list_as_df"]])

## -----------------------------------------------------------------------------
set.seed(42)
norm_samples <- lapply(1:5, \(x) rnorm(10, mean = x))
names(norm_samples) <- paste0("sample_", 1:5)
list_as_df(norm_samples)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(benchmark_examples[["cpp_n_unique"]])

## -----------------------------------------------------------------------------
library(bench)
x <- sample(1:100, 10^5, replace = TRUE)
mark(
  base_n_unique = length(unique(x)),
  cppally_n_unique = cpp_n_unique(x)
)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(benchmark_examples[["primitive_sum"]])

## -----------------------------------------------------------------------------
x <- rnorm(10^5)
primitive_sum(x)

