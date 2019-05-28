context("test-read_year")

test_that("returned structure is as expected", {
  dat <- read_year(2001, cache_dir='/tmp')

  expect_setequal(names(dat),
                  c('plant_id', 'year', 'month', 'generation_mwh'))

  num_plants <- length(unique(dat$plant_id))

  expect_equal(nrow(dat),
               num_plants*12)
})
