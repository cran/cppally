## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = FALSE
)

## ----include = FALSE----------------------------------------------------------
required <- c("bench", "brio", "callr", "cli", "decor",
              "desc", "glue", "purrr", "readr", "stringr",
              "usethis", "utils", "vctrs", "withr")
if (!all(vapply(required, requireNamespace, logical(1), quietly = TRUE))) {
  knitr::knit_exit()
}

## -----------------------------------------------------------------------------
library(cppally)

## ----include=FALSE------------------------------------------------------------
# Helpers to compile all examples in debug mode
not_cran <- function(){
 nzchar(Sys.getenv("CI")) || identical(Sys.getenv("NOT_CRAN"), "true") 
}
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
  one_unif = '
[[cppally::register]]
r_dbl one_unif(){
  return r_dbl(random_stream().uniform());
}
',
  unif_deviates = '
[[cppally::register]]
r_vector<r_dbl> unif_deviates(int n){

  random_stream r; // <-- Generated once here

  r_vector<r_dbl> out(n);

  for (r_size_t i = 0; i < n; ++i){
    out.set(i, r.uniform());
  }
  return out;
}
',
  unif_seeded = '
[[cppally::register]]
r_vector<r_dbl> unif_seeded(int n, uint64_t seed){

  random_stream r(seed);

  r_vector<r_dbl> out(n);

  for (r_size_t i = 0; i < n; ++i){
    out.set(i, r.uniform());
  }
  return out;
}
',
  normal_deviates = '
#include <random>

[[cppally::register]]
r_vector<r_dbl> normal_deviates(int n){

  random_stream r;
  r_vector<r_dbl> out(n);

  std::normal_distribution<> dist;

  for (r_size_t i = 0; i < n; ++i){
    out.set(i, dist(r));
  }
  return out;
}
',
  r_normal_deviates = '
[[cppally::register]]
r_vector<r_dbl> r_normal_deviates(int n){

  r_vector<r_dbl> out(n);

  draw_from_r([&]{
    for (r_size_t i = 0; i < n; ++i){
      double v = norm_rand();
      out.set(i, v);
    }
  });

  return out;
}
',
  random_index = '
[[cppally::register]]
r_int random_index(int a, int b){
  return as<r_int>(random_stream().index(a, b));
}
'
)

cpp_source(code = paste(examples, collapse = "\n"))

## ----include=FALSE------------------------------------------------------------
# Benchmarked separately, in an optimised build

benchmark_examples <- c(
  cpp_sample_int_with_replacement = '
[[cppally::register]]
r_vector<r_int> cpp_sample_int_with_replacement(int n, int size){

  random_stream r;
  r_vector<r_int> out(size);

  for (r_size_t i = 0; i < size; ++i){
    auto idx = r.index(0, n - 1);
    out.set(i, idx);
  }
  return out;
}
',
  boot_mean = '
[[cppally::register]]
r_vector<r_dbl> boot_mean(r_vector<r_dbl> x, int n_boot){

  r_size_t n = x.length();

  random_stream r;
  r_vector<r_dbl> out(n_boot);

  for (int b = 0; b < n_boot; ++b){
    
    r_dbl total(0);
    
    // calculate mean
    for (r_size_t i = 0; i < n; ++i){
      total += x.get(r.index(r_size_t(0), n - 1));
    }
    out.set(b, total / n);
    
  }
  return out;
}
',
resample = '
[[cppally::register]]
r_vector<r_dbl> resample(r_vector<r_dbl> x){

    r_size_t n = x.length();
    r_vector<r_dbl> out(n);
    
    random_stream r;
    
    for (r_size_t i = 0; i < n; ++i){
        out.set(i, x.get(r.index(r_size_t(0), n - 1)));
    }
    
    return out;
}
',
apply_boot = '
template <RScalar T, typename FUN>
requires RScalar<std::invoke_result_t<FUN&, const r_vector<T>&>> // Accept only fns that return scalars
auto apply_boot(const r_vector<T>& data, int n_boot, FUN fn){

  r_size_t n = data.length();

  // Function return type
  using stat_t = std::invoke_result_t<FUN&, const r_vector<T>&>;

  random_stream r;

  r_vector<stat_t> out(n_boot);
  
  // Reused across every replicate
  r_vector<T> samples(n);

  for (int b = 0; b < n_boot; ++b){

    // Sample x using a random index [0, n - 1]
    // Set the samples directly into our existing samples vector
    for (r_size_t i = 0; i < n; ++i){
        
        auto idx = r.index(r_size_t(0), n - 1);
        
        // data.view() is safe here because we are immediately protecting the result from gc
        // by placing it into samples, which is itself protected
        
        samples.set(i, data.view(idx));
    }

    // Apply function
    out.set(b, fn(samples));

  }

  return out;
}
',
boot_mean2 = '
[[cppally::register]]
r_vector<r_dbl> boot_mean2(r_vector<r_dbl> x, int n_boot){
    return apply_boot(x, n_boot, [](const auto& vec){
        return mean(vec);
    });
}
',


boot_stats = '

// Bootstrapped sum

[[cppally::register]]
r_vector<r_dbl> boot_sum(r_vector<r_dbl> x, int n_boot){
    return apply_boot(x, n_boot, [](const auto& vec){
    
      return as<r_dbl>(sum(vec));
        
    });
}

// Bootstrapped count of positive values

[[cppally::register]]
r_vector<r_int> boot_n_positive(r_vector<r_dbl> x, int n_boot){
    return apply_boot(x, n_boot, [](const auto& vec){
    
      return vec.reduce([](auto acc, auto curr){
        return as<r_int>(acc + (curr > 0));
      }, 
      /*init = */ r_int(0));
        
    });
}
'
)

cpp_source(code = paste(benchmark_examples, collapse = "\n"), debug = FALSE)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["one_unif"]])

## -----------------------------------------------------------------------------
set.seed(1)
one_unif()

set.seed(1)
one_unif() # Reproduces the same number under the same R seed

## -----------------------------------------------------------------------------
replicate(10, one_unif())

## -----------------------------------------------------------------------------
set.seed(1)
c(one_unif(), one_unif())

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["unif_deviates"]])

## -----------------------------------------------------------------------------
set.seed(1)
unif_deviates(10)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["unif_seeded"]])

## -----------------------------------------------------------------------------
set.seed(1)
unif_seeded(5, seed = 42)

set.seed(99)
unif_seeded(5, seed = 42) # Same numbers, different R seed

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["random_index"]])

## -----------------------------------------------------------------------------
set.seed(1)
random_index(0, 1000)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(benchmark_examples[["cpp_sample_int_with_replacement"]])

## -----------------------------------------------------------------------------
library(bench)
mark(
  cppally_sample_int_with_replacement = cpp_sample_int_with_replacement(10^6, 10^5),
  base_sample_int_with_replacement = sample.int(10^6, 10^5, replace = TRUE),
  check = FALSE
)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["normal_deviates"]])

## -----------------------------------------------------------------------------
set.seed(1)
normal_deviates(10)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["r_normal_deviates"]])

## -----------------------------------------------------------------------------
set.seed(1)
r_normal_deviates(5)

set.seed(1)
rnorm(5) # Identical - these are R's own draws

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(benchmark_examples[["boot_mean"]])

## -----------------------------------------------------------------------------
set.seed(42)
x <- rnorm(1000)

set.seed(1)
replicates <- boot_mean(x, 2000)

quantile(replicates, c(0.025, 0.975)) # Percentile confidence interval

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(benchmark_examples[["resample"]])

## -----------------------------------------------------------------------------
boot_mean_r <- function(x, n_boot){
  vapply(
    seq_len(n_boot),
    \(i) mean(sample(x, replace = TRUE)), 
    0
  )
}

boot_mean_cppally <- function(x, n_boot){
  vapply(
    seq_len(n_boot),
    \(i) mean(resample(x)),
    0
  )
}

mark(
  cppally = boot_mean_cppally(x, 2000),
  base_r  = boot_mean_r(x, 2000),
  check = FALSE
)

## -----------------------------------------------------------------------------
mark(
  boot_mean_cppally(x, 2000),
  boot_mean(x, 2000),
  check = FALSE
)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(benchmark_examples[["apply_boot"]])

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(benchmark_examples[["boot_mean2"]])

## -----------------------------------------------------------------------------
mark(
  boot_mean(x, 2000),
  boot_mean2(x, 2000),
  check = FALSE
)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(benchmark_examples[["boot_stats"]])

## -----------------------------------------------------------------------------
set.seed(42)
boot_sum(x, 5)
boot_n_positive(x, 5)

