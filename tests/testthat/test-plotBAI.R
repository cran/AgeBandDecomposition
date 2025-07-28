test_that("plotBAI returns a ggplot object", {
  # Simulate inTRW as list with a data.frame/tibble
  fake_inTRW <- list(
    tibble::tibble(
      year = rep(2000:2005, 3),
      tree_code = rep(c("T1", "T2", "T3"), each = 6),
      TRW = runif(18, 1, 5)
    )
  )
  
  p <- plotBAI(fake_inTRW)
  
  expect_s3_class(p, "ggplot")
})
