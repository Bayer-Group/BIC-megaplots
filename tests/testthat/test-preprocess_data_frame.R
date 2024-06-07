# test_that("preprocess_data_frame() handles all input types (.csv,.rdata(s),.rds)", {
#   # Create sample data
#   df <- tibble::tibble(x = 1, y = 2)
#   path_csv <- tempfile()
#   path_r <- tempfile()
#   
#   write.csv(df, path_csv, row.names = FALSE)
#   write.table(df, path_tsv, sep = "\t", row.names = FALSE)
# 
#     expect_equal(load_file("test.csv", path_csv), df)
#   expect_error(load_file("test.png", path_csv), "Invalid file")
# })