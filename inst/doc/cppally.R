## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = FALSE
)

## ----include = FALSE----------------------------------------------------------
required <- c("brio", "callr", "cli", "decor",
              "desc", "glue", "purrr", "readr", "stringr",
              "usethis", "utils", "vctrs", "withr")
if (!all(vapply(required, requireNamespace, logical(1), quietly = TRUE))) {
  knitr::knit_exit()
}

## -----------------------------------------------------------------------------
library(cppally)

## ----include=FALSE------------------------------------------------------------
not_cran <- function(){
 nzchar(Sys.getenv("CI")) || identical(Sys.getenv("NOT_CRAN"), "true") 
}
# Helpers to compile all examples in debug mode
cpp_source <- function(..., code, debug = TRUE, env = parent.frame()){
  preamble <- c("#include <cppally.hpp>", "using namespace cppally;")
  code <- paste(c(preamble, code), collapse = "\n")
  tryCatch(
    cppally::cpp_source(
      debug = debug, env = env, code = code, ...), 
    error = function(e) {
      
      if (not_cran()){
        stop(e)
      }
      knitr::knit_exit()
    }
  )
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
  compiled <- tryCatch({
    utils::getFromNamespace("source_single_exprs", "cppally")(
      exprs, env = env, ...
    )
    TRUE
  }, error = function(e) {
    if (not_cran()){
      stop(e)
    }
    knitr::knit_exit()
    FALSE
  })
  if (!compiled){
    return(invisible())
  }
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
r_vector<r_lgl> lgl_ops(){
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
cppally_equality = '
[[cppally::register]]
void cppally_equality(){

  r_int x = na<r_int>();
  r_int y = na<r_int>();
  
  r_lgl x_equal_to_y = x == y;
  bool x_identical_to_y = identical(x, y);
  
  // NA so not printed
  if ( x_equal_to_y.is_true() ){
    print("x is equal to y\\n");
  }
  
  // NA so not printed
  if ( x_equal_to_y.is_false() ){
    print("x is not equal to y\\n");
  }
  
  // NA so printed
  if (is_na(x_equal_to_y)){
    print("`x == y` produces `NA`\\n");
  }
  
  // Both na<r_int>() therefore they are identical to each other
  if (x_identical_to_y){
    print("x is identical to y\\n");
  }
}
',
cppally_identical = '
template <typename T, typename U>
[[cppally::register]]
bool cpp_identical(T x, U y){
  return identical(x, y);
}
',
  new_integer_vector = '
// Integer vector of size n
[[cppally::register]]
r_vector<r_int> new_integer_vector(int n){
  r_vector<r_int> int_vctr(n, /*fill = */ r_int(0));
  return int_vctr;
}
',
  all_vectors = '
[[cppally::register]]
r_vector<r_sexp> all_vectors(){
  return make_vec<r_sexp>(
    arg("logical") = r_vector<r_lgl>(),
    arg("integer") = r_vector<r_int>(),
    arg("integer64") = r_vector<r_int64>(), // Requires bit64
    arg("double") = r_vector<r_dbl>(),
    arg("character") = r_vector<r_str>(),
    arg("character") = r_vector<r_str_view>(),
    arg("raw") = r_vector<r_raw>(),
    arg("date") = r_vector<r_date>(),
    arg("date-time") = r_vector<r_psxct>(),
    arg("list") = r_vector<r_sexp>()
  );
}
',
cppally_math = '
[[cppally::register]]
r_vector<r_dbl> cppally_math(r_dbl x){
    return make_vec<r_dbl>(
        arg("abs")          = abs(x),
        arg("floor")        = floor(x),
        arg("ceiling")      = ceiling(x),
        arg("trunc")        = trunc(x),
        arg("round")        = round(x),
        arg("signif")       = signif(x, 3),
        arg("sign")         = sign(x),
        arg("min")          = min(0, x),
        arg("max")          = max(0, x),
        arg("sqrt")         = sqrt(x),
        arg("pow")          = pow(x, 2),
        arg("exp")          = exp(x),
        arg("log")          = log(x),
        arg("log_base")     = log(x, 2),
        arg("log10")        = log10(x)
    );
}', 
  cpp_abs = '
template <RNumber T>
[[cppally::register]]
T cpp_abs(T x){
  if (is_na(x)){
    return na<T>();
  } else if (x < 0){
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
r_vector<r_int> to_int_vec(r_vector<r_dbl> x){
  return as<r_vector<r_int>>(x);
}
',
  coercions = '
[[cppally::register]]
r_vector<r_sexp> coercions(){
    r_dbl a(4.2);
    r_vector<r_dbl> b = make_vec<r_dbl>(2.5);
    return make_vec<r_sexp>(
        as<r_vector<r_int>>(a),
        as<r_int>(a),
        as<r_int>(b),
        as<r_dbl>(b)
    );
}
',
to_from_cpp_vec = '
[[cppally::register]]
r_vector<r_int> cpp_vectors_example(r_vector<r_int> x){
  std::vector x_cpp = as<std::vector<r_int>>(x);
  x_cpp.push_back(r_int(42));
  return as<r_vector<r_int>>(x_cpp);
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
using list = r_vector<r_sexp>;

[[cppally::register]]
list new_list(int n){
  return list(n);
}
',
  resize_all = '
[[cppally::register]]
r_vector<r_sexp> resize_all(r_vector<r_sexp> x, r_size_t n){
    r_size_t list_length = x.length();
    for (r_size_t i = 0; i < list_length; ++i){
        r_sexp_visit(x.view(i), [&]<RVector T>(T vec) {
            x.set(i, vec.resize(n));
        });
    }
    return x;
}
',
  new_factor = '
[[cppally::register]]
r_factors new_factor(r_vector<r_str> x){
	return r_factors(x);
}
',
  factor_codes = '
static_assert(!RVector<r_factors>);

[[cppally::register]]
r_vector<r_int> factor_codes(r_factors x){
	return x.codes();
}
',
find_empty_string = '
[[cppally::register]]
r_vector<r_int> find_empty_string(r_vector<r_str> x){
    return x.find(r_str(""));
}
',
match_strs = '
[[cppally::register]]
r_vector<r_int> match_strs(r_vector<r_str> x, r_vector<r_str> table){
    return match(x, table);
}
',
cpp_in = '
[[cppally::register]]
r_vector<r_lgl> cpp_in(r_vector<r_str> x, r_vector<r_str> table){
    return x IS_IN table;
}
',
cpp_not_in = '
[[cppally::register]]
r_vector<r_lgl> cpp_not_in(r_vector<r_str> x, r_vector<r_str> table){
    return !(x IS_IN table);
}
',
cpp_subset = '
template <typename T, typename subscript_t>
requires (
  any<subscript_t, r_lgl, r_int, r_str> &&
  any<T, r_int, r_dbl, r_str>
)
[[cppally::register]]
r_vector<T> cpp_subset(r_vector<T> x, r_vector<subscript_t> y){
    return subset(x, y);
}
',
cpp_negative_subset = '
template <typename T, typename subscript_t>
requires (
  any<subscript_t, r_lgl, r_int, r_str> &&
  any<T, r_int, r_dbl, r_str>
)
[[cppally::register]]
r_vector<T> cpp_negative_subset(r_vector<T> x, r_vector<subscript_t> y){
    return subset(x, y, /*invert=*/ true);
}
',
  setnames = '
[[cppally::register]]
r_vector<r_sexp> set_list_names(r_vector<r_sexp> x, r_vector<r_str> names){
  x.set_names(names);
  return x;
}
',
cpp_seq_len = '
[[cppally::register]]
r_vector<r_int> cpp_seq_len(r_size_t n){
  return sequence(n, /* from = */ r_int(1), /* by = */ r_int(1));
}
',
cpp_sequences = '
template <typename T>
requires (any<T, r_int, r_int64, r_dbl>)
[[cppally::register]]
r_vector<r_sexp> cpp_sequences(r_vector<r_int> size, r_vector<T> from, r_vector<T> by){
    return pmap([](auto a, auto b, auto c){
        return as<r_sexp>(sequence(a, b, c));
    }, size, from, by);
}
',
get_time = '
[[cppally::register]]
r_date get_today(){
  return r_date::today();
}
[[cppally::register]]
r_psxct get_now(){
  return r_psxct::now();
}
',

get_time_components = '
[[cppally::register]]
r_int get_year(r_date x){
  return x.year();
}
[[cppally::register]]
r_int get_month(r_date x){
  return x.month();
}
[[cppally::register]]
r_int get_week(r_date x){
  return x.week();
}
[[cppally::register]]
r_int get_day(r_date x){
  return x.day();
}
[[cppally::register]]
r_int get_wday(r_date x, int week_start){
  return x.wday(week_start);
}
[[cppally::register]]
r_int get_yday(r_date x){
  return x.yday();
}
[[cppally::register]]
r_int get_iso_week(r_date x){
  return x.iso_week();
}
[[cppally::register]]
r_int get_iso_year(r_date x){
  return x.iso_year();
}
',
time_round = '
[[cppally::register]]
r_date floor_month(r_date x){
  return x.floor<"months">();
}
[[cppally::register]]
r_date ceiling_year(r_date x){
  return x.ceiling<"years">();
}
[[cppally::register]]
r_date round_week(r_date x, int week_start){
  return x.round<"weeks">(week_start);
}
',
create_date = '
[[cppally::register]]
r_date create_date(int year, int month, int day){
  return r_date(year, month, day);
}
[[cppally::register]]
r_date date_from_epoch_offset(double n){
  return r_date(n);
}

[[cppally::register]]
r_psxct create_datetime(int year, int month, int day, int hour, int minute, double second){
  return r_psxct(year, month, day, hour, minute, second);
}
[[cppally::register]]
r_psxct datetime_from_epoch_offset(double n){
  return r_psxct(n);
}
',
add_time = '
[[cppally::register]]
r_date add_days(r_date x, double n){
  return x.add<"days">(n);
}
[[cppally::register]]
r_date add_months(r_date x, double n){
  return x.add<"months">(n);
}
',
datetime_months = '
[[cppally::register]]
r_psxct as_dt(r_date x){
  return x.as_datetime();
}
[[cppally::register]]
r_psxct dt_add_months(r_psxct x, double n){
  return x.add<"months">(n);
}
[[cppally::register]]
r_dbl dt_diff_months(r_psxct x, r_psxct y){
  return time_diff<"months">(x, y);
}
',
time_diff = '
[[cppally::register]]
r_dbl diff_days(r_date x, r_date y){
  return time_diff<"days">(x, y);
}
[[cppally::register]]
r_dbl diff_months(r_date x, r_date y){
  return time_diff<"months">(x, y);
}
',
r_get_today = '
[[cppally::register]]
r_date r_get_today(){
  r_function sys_date("Sys.Date");
  return as<r_date>(sys_date());
}
',
base_sum_fn = '
[[cppally::register]]
r_function base_sum_fn(){
  return r_function(cached_sym<"sum">(), pkg_env<"base">());
}
',

base_sum = '
[[cppally::register]]
r_dbl base_sum(r_vector<r_dbl> x, bool na_rm){
  return as<r_dbl>(base_sum_fn()(x, arg("na.rm") = na_rm));
}
',
cpp_unique = '
template <typename T>
requires (any<T, r_int, r_dbl, r_str>)
[[cppally::register]]
r_int cpp_n_unique(r_vector<T> x){
  return as<r_int>(n_unique(x));
}

template <RVector T>
[[cppally::register]]
T cpp_unique(T x, bool sort){
  return unique(x, sort);
}

',
cpp_sort = '
template <typename T>
requires (any<T, r_int, r_dbl, r_str>)
[[cppally::register]]
r_vector<T> cpp_sort(r_vector<T> x){
  return sort(x);
}
',

make_groups = '
[[cppally::register]]
r_vector<r_sexp> group_info(r_vector<r_str> x){
  groups g = make_groups(x, /*ordered=*/ false);
  return make_vec<r_sexp>(
    arg("group_id") = g.ids,
    arg("n_groups") = g.n_groups,
    arg("ordered") = g.ordered,
    arg("sorted") = g.sorted,
    arg("starts") = g.starts(),
    arg("counts") = g.counts(),
    arg("order") = g.order(),
    arg("group_names") = group_names(x, g)
  );
}
',
group_counts = '
[[cppally::register]]
r_df group_counts(r_vector<r_str> x){
  groups g = make_groups(x, /*ordered=*/ false);
  
  return make_df(
    arg("key") = subset(x, g.starts()),
    arg("count") = g.counts()
  );
}
',
cpp_stats = '
template <RNumber T>
[[cppally::register]]
r_dbl cpp_sum(r_vector<T> x, bool na_rm){
  return as<r_dbl>(sum(x, na_rm));
}
template <RNumber T>
[[cppally::register]]
r_vector<T> cpp_range(r_vector<T> x, bool na_rm){
  return range(x, na_rm);
}
template <RNumber T>
[[cppally::register]]
r_dbl cpp_var(r_vector<T> x, bool na_rm){
  return var(x, na_rm);
}
template <RNumber T>
[[cppally::register]]
r_vector<r_dbl> cpp_quantile(r_vector<T> x, r_vector<r_dbl> probs, bool na_rm, bool names){
  return quantile(x, probs, na_rm, names); 
}
',
cpp_set_threads = '
[[cppally::register]]
void cpp_set_threads(int n){
  set_threads(n);
}

[[cppally::register]]
int cpp_get_threads(){
  return get_threads();
}
'
)

cpp_source(code = paste(examples, collapse = "\n"), debug = TRUE)

# Single-line expressions, pre-registered as R functions of the same name.
# Each can be invoked later as e.g. `r_true_val()` to get the evaluated result.
single_exprs <- c(
  r_true_val          = 'r_true',
  r_false_val         = 'r_false',
  r_na_val            = 'r_na',
  r_na_check          = 'is_na(na<r_int>())',
  r_plus              = 'r_int(0) + r_dbl(2.5)',
  r_plus_na           = 'na<r_int>() + r_dbl(2.5)',
  r_minus             = 'r_int(0) - r_int(1)',
  r_minus_na          = 'na<r_int>() - r_int(1)',
  r_mult              = 'r_int(2) * r_int(3)',
  r_mult_na           = 'na<r_int>() * r_int(3)',
  r_div               = 'r_dbl(9) / 3',
  r_div_na            = 'na<r_dbl>() / 3',
  r_lt                = 'r_int(1) < r_int(2)',
  r_lt_na             = 'na<r_int>() < r_int(2)',
  r_lte                = 'r_dbl(2) <= r_dbl(2)',
  r_lte_na             = 'na<r_dbl>() <= r_dbl(2)',
  r_gt                = 'r_int(3) > r_int(2)',
  r_gt_na             = 'na<r_int>() > r_int(2)',
  r_gte                = 'r_dbl(2) >= r_dbl(3)',
  r_gte_na             = 'na<r_dbl>() >= r_dbl(3)',
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
  cached_sym_demo     = 'cached_sym<"cached_symbol">()',
  cpp_seq             = 'seq(r_dbl(1), r_dbl(5), r_dbl(0.5))',
  cpp_sequence        = 'sequence(5, /*from = */ r_int(0), /*by = */ r_int(-1))'
)

register_single_exprs(single_exprs, debug = TRUE)

## -----------------------------------------------------------------------------
hello_world()

## -----------------------------------------------------------------------------
cpp_eval('print("Hello World Again!")')

## ----include=FALSE------------------------------------------------------------
cppally::cpp_source(code = '
#include <cppally/vector/r_vector.h>
using namespace cppally;

[[cppally::register]]
r_vector<r_int> new_integers(int n, std::optional<r_int> fill){
  if (fill.has_value()){
    return r_vector<r_int>(n, fill.value()); // Filled with `fill`
  } else {
    return r_vector<r_int>(n, r_int(0)); // Filled with zeroes
  }
}
                    ', 
  debug = TRUE)


## -----------------------------------------------------------------------------
new_integers(3)
new_integers(3, fill = NA)

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
as_cpp_chunk(single_exprs[["r_na_check"]])

## ----echo=FALSE---------------------------------------------------------------
r_na_check()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["cppally_equality"]])

## -----------------------------------------------------------------------------
cppally_equality()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["cppally_identical"]])

## -----------------------------------------------------------------------------
cpp_identical(3L, 3L)
cpp_identical(NA, NA)
cpp_identical(3L, 3) # int != double
cpp_identical(1:10, 1:10)
cpp_identical(list(1, 2, 3), list(3, 2, 1))
cpp_identical(iris, iris)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_plus"]])

## ----echo=FALSE---------------------------------------------------------------
r_plus()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_minus"]])

## ----echo=FALSE---------------------------------------------------------------
r_minus()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_mult"]])

## ----echo=FALSE---------------------------------------------------------------
r_mult()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_div"]])

## ----echo=FALSE---------------------------------------------------------------
r_div()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_plus_na"]])

## ----echo=FALSE---------------------------------------------------------------
r_plus_na()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_minus_na"]])

## ----echo=FALSE---------------------------------------------------------------
r_minus_na()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_mult_na"]])

## ----echo=FALSE---------------------------------------------------------------
r_mult_na()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_div_na"]])

## ----echo=FALSE---------------------------------------------------------------
r_div_na()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_lt"]])

## ----echo=FALSE---------------------------------------------------------------
r_lt()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_lte"]])

## ----echo=FALSE---------------------------------------------------------------
r_lte()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_gt"]])

## ----echo=FALSE---------------------------------------------------------------
r_gt()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_gte"]])

## ----echo=FALSE---------------------------------------------------------------
r_gte()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_lt_na"]])

## ----echo=FALSE---------------------------------------------------------------
r_lt_na()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_lte_na"]])

## ----echo=FALSE---------------------------------------------------------------
r_lte_na()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_gt_na"]])

## ----echo=FALSE---------------------------------------------------------------
r_gt_na()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["r_gte_na"]])

## ----echo=FALSE---------------------------------------------------------------
r_gte_na()

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
as_cpp_chunk(examples[["cppally_math"]])

## -----------------------------------------------------------------------------
cppally_math(2.5)
cppally_math(NA)

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
as_cpp_chunk(examples[["to_from_cpp_vec"]])

## -----------------------------------------------------------------------------
cpp_vectors_example(41L)

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
as_cpp_chunk(examples[["create_date"]])

## -----------------------------------------------------------------------------
create_date(1970, 1, 1); # This builds the Unix epoch date

## -----------------------------------------------------------------------------
date_from_epoch_offset(0) # Also builds the Unix epoch

## -----------------------------------------------------------------------------
create_datetime(1970, 1, 1, 0, 0, 0); # Unix epoch date-time
datetime_from_epoch_offset(0)         # Unix epoch from an offset

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["get_time"]])

## -----------------------------------------------------------------------------
get_today()
get_now()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["add_time"]])

## -----------------------------------------------------------------------------
y2k <- create_date(2000, 1, 1)

# Day after y2k
y2k |>
  add_days(1)

# Week after y2k
y2k |>
  add_days(7)

# Month after y2k
y2k |>
  add_months(1)

# Year after y2k
y2k |>
  add_months(12)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["get_time_components"]])

## -----------------------------------------------------------------------------
get_year(y2k)
get_month(y2k)
get_week(y2k)
get_day(y2k) # Day of the month
get_wday(y2k, week_start = 1) # week_start = 1 = Mon
get_yday(y2k)

## -----------------------------------------------------------------------------
get_iso_week(y2k) # Last (52nd) ISO week of the ISO year
get_iso_year(y2k) # ISO year is still 1999

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["time_round"]])

## -----------------------------------------------------------------------------
# Start of the month after advancing 100 days
y2k |> 
  add_days(100) |> 
  floor_month()

# ceiling() does not switch on boundary
y2k |> 
  ceiling_year()

y2k |> 
  add_days(1) |> 
  ceiling_year()

y2k |> 
  round_week(week_start = 1)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["time_diff"]])

## -----------------------------------------------------------------------------
x <- y2k
y <- x |> add_months(3)

diff_days(x, y)
diff_months(x, y)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["datetime_months"]])

## -----------------------------------------------------------------------------
y2k_datetime <- as_dt(y2k)

mid_month <- y2k_datetime |> 
  dt_add_months(0.5)

# Half-way through the month 
mid_month

## -----------------------------------------------------------------------------
dt_diff_months(y2k_datetime, mid_month)

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
as_cpp_chunk(examples[["r_get_today"]])

## -----------------------------------------------------------------------------
r_get_today()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["base_sum_fn"]])

## -----------------------------------------------------------------------------
base_sum_fn()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["base_sum"]])

## -----------------------------------------------------------------------------
base_sum(c(NA, 1:3, NA), na_rm = TRUE)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["find_empty_string"]])

## -----------------------------------------------------------------------------
x <- c("zero", "one", "two", "three", "four")

find_empty_string(x)

# Add empty strings
x[c(1, 3)] <- ""

find_empty_string(x)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["match_strs"]])

## -----------------------------------------------------------------------------
letters

vowels <- c("a", "e", "i", "o", "u")

# cppally::match is 0-indexed
match_strs(vowels, letters)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["cpp_in"]])

## -----------------------------------------------------------------------------
cpp_in(c("a", "A", NA), letters)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["cpp_not_in"]])

## -----------------------------------------------------------------------------
cpp_not_in(c("a", "A", NA), letters)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["cpp_subset"]])

## -----------------------------------------------------------------------------
x <- 1:10
cpp_subset(x, 0L) # index 0 is 1st value
cpp_subset(x, 9L) # index 9 is last value here
cpp_subset(x, 10L) # index 10 is out-of-bounds so NA is returned
cpp_subset(x, NA_integer_) # NA is returned with integer NA index

## -----------------------------------------------------------------------------
cpp_subset(x, x > 5)
cpp_subset(x, x > 100)

# It differs to base subsetting (via `[`)
cpp_subset(x, rep(NA, 10)) # cppally only returns values associated with TRUE
x[rep(NA, 10)] # base R returns NA values when subsetting with NA logicals

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["cpp_negative_subset"]])

## -----------------------------------------------------------------------------
cpp_negative_subset(x, 0L) # Everything but 1st
cpp_negative_subset(x, x > 5) # Everything except where x > 5
cpp_negative_subset(x, integer()) # Everything

## -----------------------------------------------------------------------------
names(x) <- LETTERS[seq_along(x)]

cpp_subset(x, c("A", "J", "Z"))
cpp_negative_subset(x, "A")

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["cpp_abs"]])

## -----------------------------------------------------------------------------
cpp_abs(-4.2)
cpp_abs(-3L)

class(cpp_abs(-4.2)) # Double preserved
class(cpp_abs(-3L)) # Integer preserved

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["scalar_default"]])

## -----------------------------------------------------------------------------
scalar_default(integer(1)) # Default is 0L
scalar_default(numeric(1)) # Default is 0.0
scalar_default(character(1)) # Default is ""

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["setnames"]])

## -----------------------------------------------------------------------------
set.seed(42)
norm_samples <- lapply(1:5, \(x) rnorm(10, mean = x))
set_list_names(norm_samples, paste0("sample_", 1:5))

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["cpp_seq"]])

## ----echo=FALSE---------------------------------------------------------------
cpp_seq()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(single_exprs[["cpp_sequence"]])

## ----echo=FALSE---------------------------------------------------------------
cpp_sequence()

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["cpp_seq_len"]])

## -----------------------------------------------------------------------------
cpp_seq_len(5)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["cpp_sequences"]])

## -----------------------------------------------------------------------------
cpp_sequences(1:3, from = 0L, by = 1L) |> 
  unlist()

# Same as base R
sequence(1:3, from = 0L, by = 1L)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["cpp_set_threads"]])

## -----------------------------------------------------------------------------

cpp_set_threads(2) # Set to 2 threads
cpp_get_threads()  # Number of threads now available

cpp_set_threads(1) # Reset back to 1
cpp_get_threads()  # Should return 1

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["cpp_stats"]])

## -----------------------------------------------------------------------------
set.seed(1)
x <- sample.int(10, 10^4, replace = TRUE)
x[sample.int(10^4, 10^3)] <- NA # Add many NAs

# Sum
cpp_sum(x, na_rm = TRUE)
cpp_sum(x, na_rm = FALSE)

# Range
cpp_range(x, na_rm = TRUE)
cpp_range(x, na_rm = FALSE)

# Variance
cpp_var(x, na_rm = TRUE)
cpp_var(x, na_rm = FALSE)

cpp_quantile(x, c(0.25, 0.75), na_rm = TRUE, names = TRUE)

# Many probabilities
cpp_quantile(x, seq(0, 1, 0.05), na_rm = TRUE, names = FALSE)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["cpp_unique"]])

## -----------------------------------------------------------------------------
x <- sample(1:10, 10^4, replace = TRUE)

cpp_n_unique(x)
length(unique(x))

cpp_unique(x, sort = FALSE)
unique(x)

cpp_unique(x, sort = TRUE)
sort(unique(x))

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["cpp_sort"]])

## -----------------------------------------------------------------------------
# Small data
cpp_sort(c(3, 2, NA, 1))

# Integer data
ints <- sample.int(10, 20, replace = TRUE)
cpp_sort(ints)

# Double-floating point precision data
dbls <- as.double(ints)
cpp_sort(dbls)

# String data
strs <- dbls |> paste0()
cpp_sort(strs)

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["make_groups"]])

## -----------------------------------------------------------------------------
set.seed(1)
x <- sample(c("A", "B", "C"), 20, replace = TRUE)

# Convert into group info

letter_groups <- group_info(x)

letter_groups$group_id # Group IDs
letter_groups$n_groups # Number of groups
letter_groups$ordered  # Groups were collected using order-of-first-appearance
letter_groups$sorted   # Are group IDs in sorted order?
letter_groups$starts   # Data start locations (0-indexed)
letter_groups$counts   # Group counts
letter_groups$order    # Order permutation (0-indexed)
letter_groups$group_names # Names of unique groups

## ----echo=FALSE, results = 'asis'---------------------------------------------
as_cpp_chunk(examples[["group_counts"]])

## -----------------------------------------------------------------------------
group_counts(x)

