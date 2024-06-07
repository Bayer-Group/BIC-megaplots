test_that("updates_variables_selection_csvB() checks", {
  # Define a simple test dataframe B with three integer variables
  B <- data.frame(
    subjectid = c(100,101,102),
    event_time = c(53,52,70),
    Event1 = c("MILD", "MODERATE", "SEVERE")
  )
  path_csv <- paste0(tempfile(), ".csv")
  write.csv(B, path_csv, row.names = FALSE)
    
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
    c("subjectid","event_time")
  )
  
  # Define a simple test dataframe A2 with two integer variables and one character
  # variable. The character variable should not be returned in this case
  B2 <- data.frame(
    subjectid = c("one","two","three"),
    event_time = c(53,52,70),
    Event1 = c("MILD", "MODERATE", "SEVERE")
  )
  write.csv(B2, path_csv, row.names = FALSE)
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
    c("event_time")
  )
  
  # Define a simple test dataframe A3 with all character variables. 
  # The expected output is then an empty character(0) object
  B3 <- data.frame(
    subjectid = c("one","two","three"),
    event_time = c("fifty","52","70"),
    Event1 = c("MILD", "MODERATE", "SEVERE")
  )
  write.csv(B3, path_csv, row.names=FALSE)
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
    character(0)
  )
  
  B4 <- data.frame(
    subjectid = c("100","101","102"),
    event_time = c("50","52","70"),
    Event1 = c("MILD", "MODERATE", "SEVERE")
  )
  write.csv(B4, path_csv, row.names=FALSE)
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
    c("subjectid", "event_time")
  )
  
  #If the file ending is not .csv, expect returning character(0) since no integer
  #variable is available
  path_csv <- paste0(tempfile(), ".abc")
  expect_null(
    updates_variables_selection_csvB(
      file = data.frame(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = path_csv
      ),
      csvB_sep = ",",
      csvB_quote = '"',
      csvB_dec = "."
    )
  )
  
  # Expect a warning, when entering list instead of a dataframe as file input 
  expect_warning(
    updates_variables_selection_csvB(
      file = list(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = path_csv
      ),
      csvB_sep = ",",
      csvB_quote = '"',
      csvB_dec = "."
    ), "parameter 'file' needs to be of class data.frame in function updates_variables_selection_csvB"
  )
  
  # Expect warning when datapath is an empty character string
  expect_warning(
    updates_variables_selection_csvB(
      file = data.frame(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = ""
      ),
      csvB_sep = ",",
      csvB_quote = '"',
      csvB_dec = "."
    ), "datapath is of length 0 in updates_variables_selection_csvB"
  )
  
  # Expect warning when datapath is non character
  expect_warning(
    updates_variables_selection_csvB(
      file = data.frame(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = 123
      ),
      csvB_sep = ",",
      csvB_quote = '"',
      csvB_dec = "."
    ), "datapath is non character in updates_variables_selection_csvB"
  )
}
)