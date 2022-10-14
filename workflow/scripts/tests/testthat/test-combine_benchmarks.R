## This test file is designed to be run from workflow/scripts
## with the command `testthat::test_file("tests/testthat/test-combine_benchmarks.R", reporter = default_reporter())`

library(hms)

source("../../combine_benchmarks.R")

col.types <- cols(
  s = col_double(),
  `h:m:s` = col_time(format = ""),
  max_rss = col_double(),
  max_vms = col_double(),
  max_uss = col_double(),
  max_pss = col_double(),
  io_in = col_double(),
  io_out = col_double(),
  mean_load = col_double(),
  cpu_time = col_double()
)

col.names <- c("s","h:m:s","max_rss","max_vms","max_uss","max_pss","io_in","io_out","mean_load",	"cpu_time", "process", "rule")

# test input files
test.file1 <- "testthat_resources/benchmarks/21small.tsv"
test.file2 <- "testthat_resources/benchmarks/21small_1_of_3.tsv"


# test df for test.file1
vals <- c(5.6752, 5, 113.45, 2586, 107,	108, 80.51, 0.00,	72.63,	0.2, "21small", "benchmarks")
tempdf <- data.frame(matrix(nrow=1, data=vals))
tbl <- as_tibble(tempdf)
tbl$X2 = hms::hms(seconds = 5, minutes = 0)
test.file1.df <- tbl %>% mutate_at(vars(-c("X2", "X11", "X12")), as.double)
colnames(test.file1.df) <- col.names

test_that("read.benchmarks properly reads in tsv file", {
    # create test output using input file
    test.out <- read.benchmarks(test.file1)

    expect_identical(test.out, test.file1.df)
})

test_that("create.dataset combines benchmarking data correctly", {
  # append a row for exp.df with data for test.file2
  exp.df <- test.file1.df %>% add_row(
  "s" = 88.5649, "h:m:s" = hms(seconds=28, minutes = 1),	"max_rss" = 410.99,
           "max_vms" = 2588.63, "max_uss"=403.48, "max_pss"=404.22,
           "io_in"=8.14, "io_out"=2.33, "mean_load"=75.60, "cpu_time"=67.23,
           "process"="21small_1_of_3", "rule"="benchmarks")
  # vector of input files
  invec <- c(test.file1, test.file2)
  # generate expected output
  test.out <- create.dataset(invec)

  expect_identical(test.out, exp.df)
})

test_that("create.dataset throws error for providing only one file", {
  # expect error for providing only 1 file
  expect_error(create.dataset(test.file1))
})

test_that("create.dataset throws error for non-character vector input", {
  expect_error(create.dataset(1))
})
