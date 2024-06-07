test_that("numeric_to_integer() checks", {
  #create data.frame
  df <- data.frame(
    numeric_decimal = c(1.5,2,2.5),
    numeric_integer = as.numeric(c(1,2,3)),
    integer = as.integer(c(1,2,3)),
    integer_to_big = as.numeric(c(10000000000,20000000000,30000000000)),
    character = c("one","two","three"),
    character_integer = c("1", "2", "3")
  )
  
  df2 <- data.frame(
    numeric_decimal = c(1.5,2,2.5),
    integer_to_big = as.numeric(c(10000000000,20000000000,30000000000)),
    character = c("one","two","three"),
    character_integer = c("1", "2", "3")
  )
  
  df3 <- data.frame(empty = NULL)
  
  #expect to transform structure numeric to integer if variables 
  #includes only integers, except integers which are outside the maximal
  #machine integer
  expect_false(is.integer(numeric_to_integer(df)$numeric_decimal))
  expect_true(is.integer(numeric_to_integer(df)$numeric_integer))
  expect_true(is.integer(numeric_to_integer(df)$integer))
  expect_false(is.integer(numeric_to_integer(df)$integer_to_big))
  expect_false(is.integer(numeric_to_integer(df)$character))
  expect_false(is.integer(numeric_to_integer(df)$character_integer))
  
  #if no variable includes integers expect no transformation
  expect_equal(numeric_to_integer(df2),df2)
  
  #for empty data.frames or NULL value expect return of input
  expect_null(numeric_to_integer(NULL),NULL)
  expect_equal(numeric_to_integer(df3),df3)
  
  expect_equal(numeric_to_integer("Test"),"Test")
})