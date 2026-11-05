## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = FALSE----------------------------------------------------------
required <- c("bench", "brio", "callr", "cli", "decor",
              "desc", "glue", "purrr", "readr", "stringr",
              "usethis", "vctrs")
if (!all(vapply(required, requireNamespace, logical(1), quietly = TRUE))) {
  knitr::knit_exit()
}

not_cran <- function(){
 nzchar(Sys.getenv("CI")) || identical(Sys.getenv("NOT_CRAN"), "true") 
}
cpp_source <- function(..., code, debug = FALSE, env = parent.frame()){
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
chunk_impl <- function(x, language){
  paste0("```", language, "\n", x, "\n```\n")
}
as_code_chunk <- function(x, language){
  cat(chunk_impl(x, language))
}
as_cpp_chunk <- function(x){
  as_code_chunk(x, "cpp")
}

## ----setup--------------------------------------------------------------------
library(cppally)

## ----include=FALSE------------------------------------------------------------
examples <- c(
  do_lookup = '
#include <cppally.hpp>
using namespace cppally;

[[cppally::register]]
r_vec<r_sexp> do_lookup(r_vec<r_sexp> x, r_str name, int n_iterations){
  r_vec<r_sexp> out(n_iterations);
  for (int i = 0; i < n_iterations; ++i){
    out.set(i, x.get(name));
  }
  return out;
}',
  do_first_lookup_hashed = '
#include <cppally.hpp>
using namespace cppally;

[[cppally::register]]
r_vec<r_sexp> do_first_lookup_hashed(r_vec<r_sexp> x, r_str name, int n_iterations){
  r_vec<r_sexp> out(n_iterations);

  // Initial lookup as fast linear scan to force all other lookups to be hashed
  static_cast<void>(x.get(x.names().get(0)));

  for (int i = 0; i < n_iterations; ++i){
    out.set(i, x.get(name));
  }
  return out;
}',
  do_linear_lookup = '
#include <cppally.hpp>
using namespace cppally;

[[cppally::register]]
r_vec<r_sexp> do_linear_lookup(r_vec<r_sexp> x, r_str name, int n_iterations){
  r_vec<r_sexp> out(n_iterations);
  r_vec<r_str> names = as<r_vec<r_str>>(x.names());
  for (int i = 0; i < n_iterations; ++i){
    auto first_match = names.find_first(name);
    if (!is_na(first_match)){
      out.set(i, x.view(first_match));
    }
  }
  return out;
}
'
)
cpp_source(code = paste(examples, collapse = "\n"))

## -----------------------------------------------------------------------------
# Name-value list
x <- list(
  a = 10, 
  b = 20, 
  c = 30
)

x[["c"]] # Index-by-name
x[[3]] # Index-by-location 

## -----------------------------------------------------------------------------
# If we insert an element between 2 and 3, x[[3]] changes
x <- c(
  x[1:2], 
  list(d = 40), # New element
  x[3]
)

x[[3]] # Now 40
x[["c"]] # Still the same value 

## -----------------------------------------------------------------------------
set.seed(42)
large <- as.list(sample.int(10^5))
names(large) <- paste0("name_", seq_along(large))

## -----------------------------------------------------------------------------
library(bench)
mark(large[[1]])
mark(large[[length(large)]])

## -----------------------------------------------------------------------------
mark(
    by_name = large[["name_1"]], 
    by_index = large[[1]]
)

mark(
    by_name = large[["name_100000"]], 
    by_index = large[[100000]]
)

## -----------------------------------------------------------------------------
names_hashtab <- hashtab(size = length(large))
for (i in seq_along(names(large))){
 nm <- names(large)[[i]]
 sethash(names_hashtab, nm, large[[i]])
}

# Confirm it worked
identical(gethash(names_hashtab, "name_10"), large[["name_10"]])

## -----------------------------------------------------------------------------
mark(
    by_name = large[["name_100000"]], 
    by_index = large[[100000]],
    by_hashed_name = gethash(names_hashtab, "name_100000")
)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["do_lookup"]])

## -----------------------------------------------------------------------------
r_do_lookup <- function(x, name, n_iterations){
  out <- vector("list", n_iterations)
  for (i in seq_along(out)){
    out[[i]] <- x[[name]]
  }
  out
}

## -----------------------------------------------------------------------------
nm <- names(large)[length(large)]

mark(
  cppally_one_lookup = do_lookup(large, nm, 1),
  base_one_lookup = r_do_lookup(large, nm, 1)
)

## ----fig.width=11, fig.height=7, out.width="100%", echo=FALSE-----------------
cost_per_lookup <- numeric(10^3)
measure_time <- function(expr, scale = 1){
  unclass(bench::bench_time(expr))[["real"]] * scale
}

for (i in 1:10^3){
  cost_per_lookup[i] <- measure_time(do_lookup(large, nm, i), scale = 10^6) / i
}


pt_cols <- ifelse(
    seq_along(cost_per_lookup) == 1, "orange", "black"
)
pt_cols <- ifelse(
    seq_along(cost_per_lookup) == 2, "#0072B2", pt_cols
)

plot(
    cost_per_lookup, 
    xlab = "N lookups",
    ylab = "Cost per lookup (µs)",
    main = "Time per name lookup (microseconds) as lookups increase",
    col = pt_cols
)
points(1, cost_per_lookup[1], pch = 19, col = "orange")
points(2, cost_per_lookup[2], pch = 19, col = "#0072B2")
abline(h = cost_per_lookup[1], lty = 2, col = "orange")
legend(
    "topright", 
    legend = c("1st lookup (linear scan)", "2nd lookup (hash build)"),
    col = c("orange", "#0072B2"), 
    pch = 19
)
symbols(1, cost_per_lookup[1], circles = 1, add = TRUE, inches = 0.1, fg = "orange", bg = NA)
symbols(2, cost_per_lookup[2], circles = 1, add = TRUE, inches = 0.1, fg = "#0072B2", bg = NA)

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["do_first_lookup_hashed"]])

## ----fig.width=11, fig.height=7, out.width="100%", echo=FALSE-----------------
cost_per_lookup1 <- numeric(250)
cost_per_lookup2 <- numeric(250)

for (i in 1:250){
  cost_per_lookup1[i] <- measure_time(do_lookup(large, nm, i), scale = 10^6) / i
  cost_per_lookup2[i] <- measure_time(do_first_lookup_hashed(large, nm, i), scale = 10^6) / i
}

plot(
    cost_per_lookup1, 
    xlab = "N lookups",
    ylab = "Cost per lookup (µs)",
    col = "darkblue",
    main = "Hash on 1st lookup vs hash on 2nd lookup",
    type = "l"
)
lines(cost_per_lookup2, col = "darkorange")
legend(
    "topright", 
    legend = c("Hash on 2nd lookup", "Hash on 1st lookup"),
    col = c("darkblue", "darkorange"),
    lty = 1
)

## -----------------------------------------------------------------------------
large <- as.list(sample.int(5e05))
names(large) <- paste0("name_", seq_along(large))

## ----echo=FALSE, comment="", results='asis'-----------------------------------
as_cpp_chunk(examples[["do_linear_lookup"]])

## ----fig.width=11, fig.height=7, out.width="100%", echo=FALSE-----------------

measure_ms <- function(expr){
  measure_time(expr, scale = 10^3)
}

cost_per_hash_lookup <- numeric(250)
nm <- names(large)[length(large) %/% 2]
invisible(do_lookup(large, nm, 1)) # Warm-up so one-off costs aren't timed
invisible(gc()) # Stop a pending GC landing in the first timed call
for (i in 1:length(cost_per_hash_lookup)){
  cost_per_hash_lookup[i] <- measure_ms(do_lookup(large, nm, i)) / i
}

nm <- names(large)[length(large)]
cost_per_linear_lookup_worst_case <- numeric(250)
for (i in 1:length(cost_per_linear_lookup_worst_case)){
  cost_per_linear_lookup_worst_case[i] <- measure_ms(do_linear_lookup(large, nm, i)) / i
}

nm <- names(large)[1]
cost_per_linear_lookup_best_case <- numeric(250)
for (i in 1:length(cost_per_linear_lookup_best_case)){
  cost_per_linear_lookup_best_case[i] <- measure_ms(do_linear_lookup(large, nm, i)) / i
}

nms <- sample(names(large))
cost_per_linear_lookup_mixed_case <- numeric(250)
for (i in 1:length(cost_per_linear_lookup_mixed_case)){
  cost_per_linear_lookup_mixed_case[i] <- measure_ms(do_linear_lookup(large, nms[i], i)) / i
}


# Plot from before

plot(
    cost_per_hash_lookup,
    xlab = "N lookups",
    ylab = "Cost per lookup (ms)",
    main = "Hashed lookups compared against three linear scan scenarios",
    col = "#0072B2"
)

lines(cost_per_linear_lookup_best_case, col = "purple")
lines(cost_per_linear_lookup_worst_case, col = "red")
lines(cost_per_linear_lookup_mixed_case, col = "brown")

legend(
    "topright", 
    legend = c(
        "Lazy-hash on 2nd lookup", 
        "Linear scan: best case (name always found at start)",
        "Linear scan: worst case (name always found at end)",
        "Linear scan: mixed case (name at random location)"
    ),
    col = c("#0072B2", "purple", "red", "brown"),
    pch = c(19, NA, NA, NA),
    lty = c(NA, 1, 1, 1)
)

