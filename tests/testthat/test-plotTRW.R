test_that("plotTRW returns a ggplot object", {
  # Simula un input coerente con ciò che plotTRW si aspetta
  fake_inTRW <- list(
    tibble::tibble(
      year = rep(2000:2005, 3),
      tree_code = rep(c("T1", "T2", "T3"), each = 6),
      TRW = runif(18, 0.5, 5) # valori plausibili in mm
    )
  )
  
  p <- plotTRW(fake_inTRW)
  
  expect_s3_class(p, "ggplot")
})
