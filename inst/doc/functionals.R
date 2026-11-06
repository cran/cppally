## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = FALSE----------------------------------------------------------
required <- c("bench", "brio", "callr", "cli", "cpp11",
              "decor", "desc", "ggplot2", "glue", "purrr", "readr",
              "stringr", "tidyr", "usethis", "utils", "vctrs", "withr")
if (!all(vapply(required, requireNamespace, logical(1), quietly = TRUE))) {
  knitr::knit_exit()
}

## ----setup--------------------------------------------------------------------
library(cppally)

## ----include=FALSE------------------------------------------------------------

set.seed(42)

ggplot2::theme_set(ggplot2::theme_grey())

not_cran <- function(){
 nzchar(Sys.getenv("CI")) || identical(Sys.getenv("NOT_CRAN"), "true")
}

n_threads <- if (not_cran()) 4L else 2L # CRAN allows at most 2 threads

# bench can report an extra gc row if a GC lands after the last iteration
mark <- function(..., env = parent.frame()) {
  out <- bench::mark(..., env = env)
  out$gc <- Map(function(gc, time) gc[seq_along(time), ], out$gc, out$time)
  out
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
cpp_sum3 = '
[[cppally::register]]
r_dbl cpp_sum3(r_vector<r_dbl> x){
    return sum(x);
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
    return pmap(
    
    /* fn = */ [](auto a, auto b){
    
      /* expr = */ return max(a, b);
        
    }, 
    
    /* vectors = */ x, y
  );
}
  ', 
cpp_if_else = '
template <RVector T>
[[cppally::register]]
T cpp_if_else(r_vector<r_lgl> condition, T if_true, T if_false, T if_na){
    return pmap(
      [](r_lgl condition_, auto yes, auto no, auto missing) {
        if (condition_.is_true()){
            return yes;
        } else if (condition_.is_false()){
            return no;
        } else {
            return missing;
        }
    }, 
    
    condition, if_true, if_false, if_na
  );
}
',
cpp_seq_along = '
template <RVector T>
[[cppally::register]]
r_vector<r_int> cpp_seq_along(T x){
    return pmap_with_index([](r_size_t i, auto){ // 2nd arg included so function can compile
        return as<r_int>(i) + 1; // R is 1-indexed
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
    x.apply([](auto a){ return abs(a); });
    return x;
}
', 
cpp_in_place_lag = '
[[cppally::register]]
r_vector<r_dbl> cpp_in_place_lag(r_vector<r_dbl>& x, int k){
    x.shift(k);
    return x;
}
'
)

benchmark_examples <- c(
  
cpp_set_threads = '

[[cppally::register]]
void cpp_set_threads(int n){
  set_threads(n);
}
',
 cpp_parallel_pmax = '
 
// Allows us to set threads and automatically restore them once thread_guard is destroyed (even if R aborts)
struct thread_guard {

  int curr_threads;
  
  // Set threads and store previous threads
  thread_guard(int n) {
    curr_threads = get_threads();
    set_threads(n);
  }
  
  // Restore threads on destruction
  ~thread_guard() {
    set_threads(curr_threads);
  }
  
};
template <typename F>
decltype(auto) with_threads(int n, F&& f) {
  thread_guard guard(n);
  return std::forward<F>(f)();
}
 
template <typename T>
requires (any<T, r_dbl, r_str>) // doubles or strings
[[cppally::register]]
r_vector<T> cpp_parallel_pmax(r_vector<T> x, r_vector<T> y, int n_threads){
    return with_threads(n_threads, [&]{
    
      return pmap_parallel(
      [](const auto& a, const auto& b){
          return max(a, b);
      }, 
      x, y
    );
  });
}
  ',
cpp_sqrt = '

[[cppally::register]]
r_vector<r_dbl> cpp_sqrt(r_vector<r_dbl> x){
    return pmap_parallel_simd(
      [](auto v){
        return sqrt(v);
      }, 
    x
  );
}
', 
cpp_ops = '
template <RNumber T>
[[cppally::register]]
r_vector<T> cpp_add(r_vector<T> x, r_vector<T> y, r_vector<T> z){
  return x + y + z;
}
template <RNumber T>
[[cppally::register]]
r_vector<T> cpp_multiply(r_vector<T> x, r_vector<T> y, r_vector<T> z){
  return x * y * z;
}
',
group_names = '
// Helper to add group names, which are the unique values of group_data 
// so that each result is associated with its group name
r_vector<r_dbl> add_group_names(r_vector<r_dbl>& result, r_vector<r_str> group_data, groups g){
    r_vector<r_str> nms = group_names(group_data, g);
    result.set_names(nms);
    return result;
}
',
grouped_sum = '
[[cppally::register]]
r_vector<r_dbl> grouped_sum(r_vector<r_dbl> x, r_vector<r_str> g){
  
    groups grps = make_groups(g);
  
    r_vector<r_dbl> sums = apply_by_group(
      x, /*groups = */ grps,
      [](const r_vector<r_dbl>& chunk){
          return sum(chunk, /*na_rm = */ true);
      });

      return add_group_names(sums, g, grps);
}
',
grouped_median = '
[[cppally::register]]
r_vector<r_dbl> grouped_median(r_vector<r_dbl> x, r_vector<r_str> g){
  
    groups grps = make_groups(g);
  
    r_vector<r_dbl> medians = apply_by_group(
      x, grps,
      [](const r_vector<r_dbl>& chunk){
          return quantile(chunk, 0.5, /*na_rm = */ true);
      });

      return add_group_names(medians, g, grps);
}
',
cpp_grouped_apply = '
[[cppally::register]]
r_vector<r_dbl> cpp_grouped_apply(r_vector<r_dbl> x, r_vector<r_str> g, r_function fn){
    groups grps = make_groups(g);
    auto result = apply_by_group<r_dbl>(x, grps, fn);
    return add_group_names(result, g, grps);
}
'
  
)


cpp_source(code = paste(examples, collapse = "\n"), debug = TRUE)
cpp_source(code = paste(benchmark_examples, collapse = "\n"), debug = FALSE)

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
as_cpp_chunk(examples[["cpp_sum"]])

## -----------------------------------------------------------------------------
cpp_sum(1:10)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_sum2"]])

## -----------------------------------------------------------------------------
cpp_sum2(1:10)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_cumsum"]])

## -----------------------------------------------------------------------------
cpp_cumsum(1:10)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["cpp_sum3"]])

## -----------------------------------------------------------------------------
cpp_sum3(c(10, 20, 30))

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
as_cpp_chunk(benchmark_examples[["cpp_parallel_pmax"]])

## -----------------------------------------------------------------------------
library(bench)
library(ggplot2)

x <- as.double(rpois(10^5, lambda = 10))
y <- as.double(rpois(10^5, lambda = 15))

(
  mark(
    single_threaded_pmax = cpp_parallel_pmax(x, y, n_threads = 1),
    multi_threaded_pmax = cpp_parallel_pmax(x, y, n_threads = n_threads)
  ) |> 
    autoplot(type = "violin") 
) +
  labs(title = "Numeric data")

## -----------------------------------------------------------------------------

x <- as.character(x)
y <- as.character(y)

(
  mark(
    single_threaded_pmax = cpp_parallel_pmax(x, y, n_threads = 1),
    multi_threaded_pmax = cpp_parallel_pmax(x, y, n_threads = n_threads)
  ) |> 
    autoplot(type = "violin") 
) +
  labs(title = "Character data")

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
as_cpp_chunk(benchmark_examples[["cpp_sqrt"]])

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(benchmark_examples[["cpp_set_threads"]])

## -----------------------------------------------------------------------------
cpp_set_threads(n_threads) # Set threads for the rest of the examples

## -----------------------------------------------------------------------------
x <- rnorm(10^5, mean = 50)

(
  mark(
    base_sqrt = sqrt(x),
    cpp_sqrt = cpp_sqrt(x)
  ) |> 
    autoplot(type = "violin")
) + 
  labs(title = "R vs C++ vectorised square-root")

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(benchmark_examples[["cpp_ops"]])

## -----------------------------------------------------------------------------
a <- sample(c(-1L, NA_integer_, 0L, 1L, -100L, 100L), 10^5, replace = TRUE)
b <- sample(c(NA_real_, 0, -1/3, 1/3, -10^9, 10^9), 10^5, replace = TRUE)

results <-
 mark(
   `a + a + a (base w/integers)` = a + a + a,
   `a * a * a (base w/integers)` = a * a * a,
   `b + b + b (base w/doubles)` = b + b + b,
   `b * b * b (base w/doubles)` = b * b * b,
   
   `a + a + a (cppally w/integers)` = cpp_add(a, a, a),
   `a * a * a (cppally w/integers)` = cpp_multiply(a, a, a),
   `b + b + b (cppally w/doubles)` = cpp_add(b, b, b),
   `b * b * b (cppally w/doubles)` = cpp_multiply(b, b, b),
   check = FALSE
 )

results |> 
  autoplot(type = "violin") +
  geom_violin(aes(fill = ifelse(grepl("base", expression), "base", "cppally"))) +
  labs(title = "cppally vs R arithmetic operations") + 
  scale_y_discrete(limits = rev) +
  theme(legend.title = element_blank())
  

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(benchmark_examples[["cpp_grouped_apply"]])

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(benchmark_examples[["group_names"]])

## -----------------------------------------------------------------------------
r_grouped_apply <- function(x, g, fn){
  vapply(split(x, factor(g, levels = unique(g))), fn, 0)
}

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(benchmark_examples[["grouped_sum"]])

## -----------------------------------------------------------------------------
cpp_set_threads(1) # Run these benchmarks using a single thread
n <- 10^5
x <- rnorm(n)

# 3 groups
g <- sample(c("a", "b", "c"), n, replace = TRUE)
(
  mark(
  base_grouped_sum = r_grouped_apply(x, g, sum),
  cppally_grouped_r_sum = cpp_grouped_apply(x, g, sum),
  cppally_grouped_cpp_sum = grouped_sum(x, g),
  min_iterations = 10
) |> 
    autoplot(type = "violin")
) + 
  labs(title = "R vs cppally sum by group - 3 groups") +
  bench::scale_x_bench_time(base = 2)

# 1e04 groups
g <- as.character(sample.int(10^4, n, replace = TRUE))
(
  mark(
  base_grouped_sum = r_grouped_apply(x, g, sum),
  cppally_grouped_r_sum = cpp_grouped_apply(x, g, sum),
  cppally_grouped_cpp_sum = grouped_sum(x, g),
  min_iterations = 10
) |> 
    autoplot(type = "violin")
) + 
  labs(title = "R vs cppally sum by group - 10k groups") +
  bench::scale_x_bench_time(base = 2)

# 1e05 groups
g <- as.character(sample.int(10^5, n, replace = TRUE))
(
  mark(
  base_grouped_sum = r_grouped_apply(x, g, sum),
  cppally_grouped_r_sum = cpp_grouped_apply(x, g, sum),
  cppally_grouped_cpp_sum = grouped_sum(x, g),
  min_iterations = 10
) |> 
    autoplot(type = "violin")
) + 
  labs(title = "R vs cppally sum by group - 100k groups") +
  bench::scale_x_bench_time(base = 2)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(benchmark_examples[["grouped_median"]])

## -----------------------------------------------------------------------------
# 1e04 groups
g <- as.character(sample.int(10^4, n, replace = TRUE))
(
  mark(
  base_grouped_median = r_grouped_apply(x, g, median),
  cppally_grouped_r_median = cpp_grouped_apply(x, g, median),
  cppally_grouped_cpp_median = grouped_median(x, g),
  min_iterations = 3
) |> 
    autoplot(type = "violin")
) + 
  labs(title = "R vs cppally median by group - 10k groups") +
  bench::scale_x_bench_time(base = 2)

