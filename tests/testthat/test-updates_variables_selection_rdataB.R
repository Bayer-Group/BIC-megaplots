test_that("updates_variables_selection_rdataB() checks", {
  # Define a simple test dataframe B with three integer variables
  B <- data.frame(
    subjectid = c(100,101,102),
    event_time = c(53,52,70),
    Event1 = c("MILD", "MODERATE", "SEVERE")
  )
  path_rdata <- paste0(tempfile(), ".rdata")
  save(B, file = path_rdata)
    
  expect_equal(
    updates_variables_selection_rdataB(
      file = data.frame(
        name = "Test.rdata",
        size = 1234,        
        type = "",
        datapath = path_rdata
      )
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
  save(B2, file = path_rdata)
  expect_equal(
    updates_variables_selection_rdataB(
      file = data.frame(
        name = "Test.rdata",
        size = 1234,
        type = "",  
        datapath = path_rdata
      )
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
  save(B3, file = path_rdata)
  expect_equal(
    updates_variables_selection_rdataB(
      file = data.frame(
        name = "Test.rdata",
        size = 1234,
        type = "", 
        datapath = path_rdata
      )
    ),
    character(0)
  )
  
  B4 <- data.frame(
    subjectid = c("100","101","102"),
    event_time = c("50","52","70"),
    Event1 = c("MILD", "MODERATE", "SEVERE")
  )
  save(B4, file = path_rdata)
  expect_equal(
    updates_variables_selection_rdataB(
      file = data.frame(
        name = "Test.rdata",
        size = 1234,
        type = "", 
        datapath = path_rdata
      )
    ),
    character(0)
  )
  
  #If the file ending is not .rdata, expect returning character(0) since no integer
  #variable is available
  path_rdata <- paste0(tempfile(), ".abc")
  expect_null(
    updates_variables_selection_rdataB(
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
    updates_variables_selection_rdataB(
      file = list(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = path_rdata
      )
    ), "parameter 'file' needs to be of class data.frame in function updates_variables_selection_rdataB"
  )
  
  # Expect warning when datapath is an empty character string
  expect_warning(
    updates_variables_selection_rdataB(
      file = data.frame(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = ""
      )
    ), "datapath is of length 0 in updates_variables_selection_rdataB"
  )
  
  # Expect warning when datapath is non character
  expect_warning(
    updates_variables_selection_rdataB(
      file = data.frame(
        name = "Test.abc",
        size = 1234,
        type = "", 
        datapath = 123
      )
    ), "datapath is non character in updates_variables_selection_rdataB"
  )
}
)