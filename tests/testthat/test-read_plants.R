context("test-read_plants")

test_that("returned structure is as expected", {
  dat <- read_plants(cache_dir='/tmp')

  expect_setequal(names(dat),
                  c('plant_id', 'plant_name', 'address', 'city', 'state', 'zip', 'lat', 'lon', 'primary_fuel', 'capacity_mw'))
})
