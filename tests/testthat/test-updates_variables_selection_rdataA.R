test_that("updates_variables_selection_rdataA() checks", {
  # Define a simple test dataframe A with three integer variables
  A <- data.frame(
    subjectid = c(100,101,102),
    start_time = c(1, 2, -3),
    end_time = c(53,52,70)
  )
  path_rdata <- paste0(tempfile(), ".rdata")
  save(A, file = path_rdata)  
  expect_equal(
    updates_variables_selection_rdataA(
      file = data.frame(
        name = "Test.rdata",
        size = 1234,        
        type = "",
        datapath = path_rdata
      )
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
  path_rdata <- paste0(tempfile(), ".rdata")
  save(A2, file = path_rdata)  
  expect_equal(
    updates_variables_selection_rdataA(
      file = data.frame(
        name = "Test.rdata",
        size = 1234,        
        type = "",
        datapath = path_rdata
      )
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
  path_rdata <- paste0(tempfile(), ".rdata")
  save(A3, file = path_rdata)  
  expect_equal(
    updates_variables_selection_rdataA(
      file = data.frame(
        name = "Test.rdata",
        size = 1234,        
        type = "",
        datapath = path_rdata
      )
    ),
    character(0)
  )
  
  A4 <- data.frame(
    subjectid = c("100","101","102"),
    start_time = c("1", "2", "-3"),
    end_time = c("53","52","70")
  )
  path_rdata <- paste0(tempfile(), ".rdata")
  save(A4, file = path_rdata)  
  expect_equal(
    updates_variables_selection_rdataA(
      file = data.frame(
        name = "Test.rdata",
        size = 1234,        
        type = "",
        datapath = path_rdata
      )
    ),
    character(0)
  )
  #If the file ending is not .csv, expect returning character(0) since no integer
  #variable is available
  path_rdata <- paste0(tempfile(), ".abc")
  expect_null(
    updates_variables_selection_rdataA(
      file = data.frame(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = path_rdata
      )
    )
  )
  
  # Expect a warning, when entering list instead of a dataframe as file input 
  expect_warning(
    updates_variables_selection_rdataA(
      file = list(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = path_rdata
      )
    ), "parameter 'file' needs to be of class data.frame in function updates_variables_selection_rdataA"
  )
  
  # Expect warning when datapath is an empty character string
  expect_warning(
    updates_variables_selection_rdataA(
      file = data.frame(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = ""
      )
    ), "datapath is of length 0 in updates_variables_selection_rdataA"
  )
  
  # Expect warning when datapath is non character
  expect_warning(
    updates_variables_selection_rdataA(
      file = data.frame(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = 123
      )
    ), "datapath is non character in updates_variables_selection_rdataA"
  )
}
)