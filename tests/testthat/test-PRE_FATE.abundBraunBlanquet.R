library(RFate)
context("PRE_FATE.abundBraunBlanquet() function")

test_that("PRE_FATE.abundBraunBlanquet of BB values give right results", {
  expect_equal(PRE_FATE.abundBraunBlanquet("r"), 0.5)
  expect_equal(PRE_FATE.abundBraunBlanquet("+"), 0.5)
  expect_equal(PRE_FATE.abundBraunBlanquet("1"), 3)
  expect_equal(PRE_FATE.abundBraunBlanquet(1), 3)
  expect_equal(PRE_FATE.abundBraunBlanquet("2"), 15)
  expect_equal(PRE_FATE.abundBraunBlanquet(2), 15)
  expect_equal(PRE_FATE.abundBraunBlanquet("3"), 37.5)
  expect_equal(PRE_FATE.abundBraunBlanquet(3), 37.5)
  expect_equal(PRE_FATE.abundBraunBlanquet("4"), 62.5)
  expect_equal(PRE_FATE.abundBraunBlanquet(4), 62.5)
  expect_equal(PRE_FATE.abundBraunBlanquet("5"), 87.5)
  expect_equal(PRE_FATE.abundBraunBlanquet(5), 87.5)
})

# test_that("PRE_FATE.abundBraunBlanquet of none BB values give NA", {
#   expect_equal(PRE_FATE.abundBraunBlanquet(NA), NA)
# })
# 
# test_that("PRE_FATE.abundBraunBlanquet of missing give NA", {
#   expect_equal(PRE_FATE.abundBraunBlanquet(NA), NA)
# })

