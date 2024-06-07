test_that("updates_variables_selection_csvA() checks", {
  # Define a simple test dataframe A with three integer variables
  A <- data.frame(
    subjectid = c(100,101,102),
    start_time = c(1, 2, -3),
    end_time = c(53,52,70)
  )
  path_csv <- paste0(tempfile(), ".csv")
  write.csv(A, path_csv, row.names = FALSE)
    
  expect_equal(
    updates_variables_selection_csvA(
      file = data.frame(
        name = "Test.csv",
        size = 1234,        
        type = "",
        datapath = path_csv
      ),
      csvA_sep = ",",
      csvA_quote = '"',
      csvA_dec = "."
    ),
    c("subjectid","start_time","end_time")
  )
  
  # Define a simple test dataframe A2 with two integer variables and one character
  # variable. The character variable should not be returned in this case
  A2 <- data.frame(
    subjectid = c("one","two","three"),
    start_time = c(1, 2, -3),
    end_time = c(53,52,70)
  )
  write.csv(A2, path_csv, row.names=FALSE)
  expect_equal(
    updates_variables_selection_csvA(
      file = data.frame(
        name = "Test.csv",
        size = 1234,
        type = "",  
        datapath = path_csv
      ),
      csvA_sep = ",",
      csvA_quote = '"',
      csvA_dec = "."
    ),
    c("start_time","end_time")
  )
  
  # Define a simple test dataframe A3 with all character variables. 
  # The expected output is then an empty character(0) object
  A3 <- data.frame(
    subjectid = c("one","two","three"),
    start_time = c("one","two","three"),
    end_time = c("one","two","three")
  )
  write.csv(A3, path_csv, row.names=FALSE)
  expect_equal(
    updates_variables_selection_csvA(
      file = data.frame(
        name = "Test.csv",
        size = 1234,
        type = "", 
        datapath = path_csv
      ),
      csvA_sep = ",",
      csvA_quote = '"',
      csvA_dec = "."
    ),
    character(0)
  )
  
  A4 <- data.frame(
    subjectid = c("100","101","102"),
    start_time = c("1", "2", "-3"),
    end_time = c("53","52","70")
  )
  write.csv(A4, path_csv, row.names=FALSE)
  expect_equal(
    updates_variables_selection_csvB(
      file = data.frame(
        name = "Test.csv",
        size = 1234,
        type = "", 
        datapath = path_csv
      ),
      csvB_sep = ",",
      csvB_quote = '"',
      csvB_dec = "."
    ),
    c("subjectid", "start_time","end_time")
  )
  #If the file ending is not .csv, expect returning character(0) since no integer
  #variable is available
  path_csv <- paste0(tempfile(), ".abc")
  expect_null(
    updates_variables_selection_csvA(
      file = data.frame(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = path_csv
      ),
      csvA_sep = ",",
      csvA_quote = '"',
      csvA_dec = "."
    )
  )
  
  # Expect a warning, when entering list instead of a dataframe as file input 
  expect_warning(
    updates_variables_selection_csvA(
      file = list(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = path_csv
      ),
      csvA_sep = ",",
      csvA_quote = '"',
      csvA_dec = "."
    ), "parameter 'file' needs to be of class data.frame in function updates_variables_selection_csvA"
  )
  
  # Expect warning when datapath is an empty character string
  expect_warning(
    updates_variables_selection_csvA(
      file = data.frame(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = ""
      ),
      csvA_sep = ",",
      csvA_quote = '"',
      csvA_dec = "."
    ), "datapath is of length 0 in updates_variables_selection_csvA"
  )
  
  # Expect warning when datapath is non character
  expect_warning(
    updates_variables_selection_csvA(
      file = data.frame(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = 123
      ),
      csvA_sep = ",",
      csvA_quote = '"',
      csvA_dec = "."
    ), "datapath is non character in updates_variables_selection_csvA"
  )
}
)