
test_that("ABD returns a tibble with expected columns", {
  # Create example input
  df_data <- tibble::tibble(
    year = rep(2000:2004, 3),
    tree_code = rep(c("T1", "T2", "T3"), each = 5),
    TRW = runif(15, 1, 5),
    age_class = rep(1:3, each = 5)
  )
  
  age_class_df <- tibble::tibble(
    tree_code = c("T1", "T2", "T3"),
    age_class = c(1, 2, 3),
    ageBands = c(5, 5, 5)
  )
  
  inTRW <- list(df_data, age_class_df)
  
  result <- ABD(inTRW, min_nTrees_year = 1, pct_stdTRW_th = .5, pct_Trees_th = 0.5)
  
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("year", "N_ageBands", "ABD") %in% colnames(result)))
})
