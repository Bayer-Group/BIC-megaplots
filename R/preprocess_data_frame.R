#' Read and Preprocess uploaded app data
#'
#' @param  selectdata select upload type (Data upload / Demo data / Saved session)
#' @param  impswitch select different data upload formats (RData / csv)
#' @param  file rdata file with datasets A and B as list
#' @param  csvA csv file with dataset A
#' @param  csvB csv file with dataset B
#' @param  rdataA rdata file with dataset A
#' @param  rdataB rdata file with dataset B
#' @param  A_subjectid_rdata Selected subjectid from dataset A (rdata)
#' @param  A_start_time_rdata Selected start time from dataset A (rdata)
#' @param  A_end_time_rdata Selected end time from dataset A (rdata)
#' @param  B_subjectid_rdata Selected subjectid from dataset B (rdata)
#' @param  B_event_time_rdata Selected event time from dataset B (rdata) 
#' @param  A_subjectid_csv Selected subjectid from dataset A (csv)
#' @param  A_start_time_csv Selected start time from dataset A (csv)
#' @param  A_end_time_csv Selected end time from dataset A (csv)
#' @param  B_subjectid_csv Selected subjectid from dataset B (csv)
#' @param  B_event_time_csv Selected event time from dataset B (csv)
#' @param  A_subjectid_rdata_files Selected subjectid from dataset A (rdata A)
#' @param  A_start_time_rdata_files Selected start time from dataset A (rdata A)
#' @param  A_end_time_rdata_files  Selected end time from dataset A (rdata A)
#' @param  B_subjectid_rdata_files Selected subjectid from dataset B (rdata B)
#' @param  B_event_time_rdata_files Selected event time from dataset B (rdata B)
#' @param csv_sep separator character in read.csv function
#' @param csv_quote quoting character in read.csv function
#' @param csv_dec character used for decimal points in read.csv function
#' @param  setting_file rds file with saved settings of megaplots session
#'
#'
#' @noRd
#' @keywords internal

preprocess_data_frame <- function(
 selectdata,
 impswitch,
 file,
 csvA,
 csvB,
 rdataA,
 rdataB,
 A_subjectid_rdata,
 A_start_time_rdata,
 A_end_time_rdata,
 B_subjectid_rdata,
 B_event_time_rdata, 
 A_subjectid_csv,
 A_start_time_csv,
 A_end_time_csv,
 B_subjectid_csv,
 B_event_time_csv,
 A_subjectid_rdata_files,
 A_start_time_rdata_files,
 A_end_time_rdata_files,
 B_subjectid_rdata_files,
 B_event_time_rdata_files,
 csv_sep,
 csv_quote,
 csv_dec,
 setting_file
) {
  #initial values
  megaplot_error_message <- NULL
  megaplot_data <- NULL
  name <- NULL
  A <- NULL
  B <- NULL
  #megaplots_demo_data <- NULL
  megaplots_selected_start_time <- megaplots_selected_end_time <- NULL
  
  if (selectdata == 'Upload data') {
    if (impswitch == '*.RData file') {
      shiny::req(file)
      if (
        !utils::tail(strsplit(file$datapath, ".", fixed = TRUE)[[1]], n = 1) %in% c("RData","rdata","Rdata")
      ) {
        megaplot_data <- NULL
        megaplot_error_message <- paste0(
          "Wrong data format. <br> You have selected a ",
          utils::tail(strsplit(
            file$datapath, ".", fixed = TRUE
          )[[1]], n = 1),
          " file. <br> Please select a .RData file <br> or choose another file format."
        )
      } else {
        load(file$datapath)
        A2 <- numeric_to_integer(A)
        B2 <- numeric_to_integer(B)
        if (
          !is.null(A_subjectid_rdata) &
          !is.null(A_start_time_rdata) &
          !is.null(A_end_time_rdata) &
          !is.null(B_subjectid_rdata) &
          !is.null(B_event_time_rdata) &
          A_subjectid_rdata != "" &
          A_start_time_rdata != "" &
          A_end_time_rdata != "" &
          B_subjectid_rdata != "" &
          B_event_time_rdata != ""
          
        ) {
        if (
          A_subjectid_rdata != A_start_time_rdata &
          A_subjectid_rdata != A_end_time_rdata &
          A_start_time_rdata != A_end_time_rdata &
          B_subjectid_rdata != B_event_time_rdata
         ) {
          if (A_subjectid_rdata %in% colnames(A) &
             A_start_time_rdata %in% colnames(A) &
             A_end_time_rdata %in% colnames(A)) {
            A <- A %>% 
              dplyr::rename(
                megaplots_selected_subjectid = A_subjectid_rdata,
                megaplots_selected_start_time = A_start_time_rdata,
                megaplots_selected_end_time = A_end_time_rdata
              )
          }
          if (B_subjectid_rdata %in% colnames(B) &
             B_event_time_rdata %in% colnames(B)) {
            B <- B %>% 
              dplyr::rename(
                megaplots_selected_subjectid = B_subjectid_rdata,
                megaplots_selected_event_time = B_event_time_rdata
              )
          } 
            #remove rows with missing ae_time value
          if (all(A$megaplots_selected_start_time <= A$megaplots_selected_end_time)) {
            B <- B[!is.na(B$megaplots_selected_event_time), ]
              name <- gsub('.RData', '', file$name)
              name <- gsub('_', ' ', name)
                    
              megaplot_error_message <- NULL
  
              A <- cbind(
                A %>% dplyr::select(c(megaplots_selected_subjectid,megaplots_selected_start_time,megaplots_selected_end_time)),
                A %>% dplyr::select(-c(megaplots_selected_subjectid,megaplots_selected_start_time,megaplots_selected_end_time))
              )
              
              B <- cbind(
                B %>% dplyr::select(c(megaplots_selected_subjectid,megaplots_selected_event_time)),
                B %>% dplyr::select(-c(megaplots_selected_subjectid,megaplots_selected_event_time))
              )
              megaplot_data <- list(
                A = A,
                B = B,
                name = name
              )
            } else {
              megaplot_error_message <-
                paste0(
                  "The start time in dataset A should be smaller than the end time for all subjects."
                )
              megaplot_data <- NULL
            }
          } else {
            megaplot_error_message <-
              paste0(
                "Please select unique columns for dataset A and B!"
              )
            megaplot_data <- NULL
          }
        } 
      }
    } else if (impswitch == '*.CSV files') {
      if (!is.null(csvA)) {
        if (utils::tail(strsplit(csvA$datapath, ".", fixed = TRUE)[[1]], n = 1) != "csv") {
          megaplot_error_message1 <- paste0(
            "Wrong data format for dataset A. <br> You have selected a ",
            utils::tail(strsplit(
              csvA$datapath, ".", fixed = TRUE
            )[[1]], n = 1),
            " file. <br> Please select a .csv file <br> or choose another file format."
          )
          megaplot_data <- NULL
          megaplot_error_message2 <- NULL
        } else {
        if (
          !is.null(A_subjectid_csv) &
          !is.null(A_start_time_csv) &
          !is.null(A_end_time_csv) &
          A_subjectid_csv != "" &
          A_start_time_csv != "" &
          A_end_time_csv != "" 
        ) {
          A <- read.csv(
            file = csvA$datapath,
            header = TRUE,
            na.strings = c('NA', '.', "",''," ", 'N/A'),
            sep = csv_sep,
            quote = csv_quote,
            dec = csv_dec,
            stringsAsFactors = FALSE
          )
          
           if (A_subjectid_csv %in% colnames(A) &
             A_start_time_csv %in% colnames(A) &
             A_end_time_csv %in% colnames(A)) {
             
              A <- A %>% 
                dplyr::rename(
                  megaplots_selected_subjectid = A_subjectid_csv,
                  megaplots_selected_start_time = A_start_time_csv,
                  megaplots_selected_end_time = A_end_time_csv
                )
            }   
            megaplot_error_message1 <- NULL
            }
          }
        }
        if (!is.null(csvB)) {
          if (utils::tail(strsplit(csvB$datapath, ".", fixed = TRUE)[[1]], n = 1) != "csv") {
            megaplot_error_message1 <- paste0(
              "Wrong data format for dataset B. <br> You have selected a ",
              utils::tail(strsplit(
                csvB$datapath, ".", fixed = TRUE
              )[[1]], n = 1),
              " file. <br> Please select a .csv file <br> or choose another file format."
            )
            megaplot_data <- NULL
            megaplot_error_message2 <- NULL
          } else {
            if (
            !is.null(B_subjectid_csv) &
            !is.null(B_event_time_csv) &
            B_subjectid_csv != "" &
            B_event_time_csv != ""
            
          ) {               
            B <- read.csv(
              file = csvB$datapath,
              header = TRUE,
              na.strings = c('NA', '.', "",''," ", 'N/A'),
              sep = csv_sep,
              quote = csv_quote,
              dec = csv_dec,
              stringsAsFactors = FALSE
            )
            if (B_subjectid_csv %in% colnames(B) &
              B_event_time_csv %in% colnames(B)) {
                B <- B %>% 
                  dplyr::rename(
                    megaplots_selected_subjectid = B_subjectid_csv,
                    megaplots_selected_event_time = B_event_time_csv
                  )       
            }
            megaplot_error_message1 <- NULL
            }
          }
        }
        if (
          !is.null(A_subjectid_csv) &
          !is.null(A_start_time_csv) &
          !is.null(A_end_time_csv) &
          !is.null(B_subjectid_csv) &
          !is.null(B_event_time_csv) &
          A_subjectid_csv != "" &
          A_start_time_csv != "" &
          A_end_time_csv != "" &
          B_subjectid_csv != "" &
          B_event_time_csv != ""
          
        ) {
          if (
            A_subjectid_csv != A_start_time_csv &
            A_subjectid_csv != A_end_time_csv &
            A_start_time_csv != A_end_time_csv &
            B_subjectid_csv != B_event_time_csv
          ) {
            if (
              all(c("megaplots_selected_subjectid","megaplots_selected_start_time","megaplots_selected_end_time") %in% colnames(A)) &
              all(c("megaplots_selected_subjectid","megaplots_selected_event_time") %in% colnames(B))
            ) {
              if (all(A$megaplots_selected_start_time <= A$megaplots_selected_end_time)) {
                B <- B[!is.na(B$megaplots_selected_event_time), ]
                name <- gsub('.csv', '', csvA$name)
                name <- gsub('_', ' ', name)
                megaplot_error_message <- NULL
  
                A <- cbind(
                  A %>% dplyr::select(c(megaplots_selected_subjectid,megaplots_selected_start_time,megaplots_selected_end_time)),
                  A %>% dplyr::select(-c(megaplots_selected_subjectid,megaplots_selected_start_time,megaplots_selected_end_time))
                )
                B <- cbind(
                  B %>% dplyr::select(c(megaplots_selected_subjectid,megaplots_selected_event_time)),
                  B %>% dplyr::select(-c(megaplots_selected_subjectid,megaplots_selected_event_time))
                )
                      
                megaplot_data <- list(
                  A = A,
                  B = B,
                  name = name
                )
                    
              } else {
                megaplot_error_message <-
                  paste0(
                    "The start time in dataset A should be smaller than the end time for all subjects."
                  )
                megaplot_data <- NULL
              }
            } else {
              megaplot_error_message <-
                  paste0(
                    "Please select unique columns for dataset A and B!"
                  )
              megaplot_data <- NULL
            }
          }
        }
      } else if (impswitch == '*.RData files (two files)') {
        if (!is.null(rdataA)) {
          if (!utils::tail(strsplit(rdataA$datapath, ".", fixed = TRUE)[[1]], n = 1) %in% c("RData","rdata","Rdata")) {
            megaplot_error_message1 <- paste0(
              "Wrong data format for dataset A. <br> You have selected a ",
              utils::tail(strsplit(
                rdataA$datapath, ".", fixed = TRUE
              )[[1]], n = 1),
              " file. <br> Please select a .RData file <br> or choose another file format."
            )
            megaplot_data <- NULL
            megaplot_error_message2 <- NULL
          } else {
            if (
              !is.null(A_subjectid_rdata_files) &
              !is.null(A_start_time_rdata_files) &
              !is.null(A_end_time_rdata_files) &
              A_subjectid_rdata_files != "" &
              A_start_time_rdata_files != "" &
              A_end_time_rdata_files != "" 
            ) {
              A <- base::get(load(
                file = rdataA$datapath
              ))
               if (A_subjectid_rdata_files %in% colnames(A) &
               A_start_time_rdata_files %in% colnames(A) &
               A_end_time_rdata_files %in% colnames(A)) {
                A <- A %>% 
                  dplyr::rename(
                    megaplots_selected_subjectid = A_subjectid_rdata_files,
                    megaplots_selected_start_time = A_start_time_rdata_files,
                    megaplots_selected_end_time = A_end_time_rdata_files
                  )
               }
                megaplot_error_message1 <- NULL
                }
              }
            }
            if (!is.null(rdataB)) {
              if (!utils::tail(strsplit(rdataB$datapath, ".", fixed = TRUE)[[1]], n = 1) %in% c("Rdata","rdata","RData")) {
                megaplot_error_message1 <- paste0(
                  "Wrong data format for dataset B. <br> You have selected a ",
                  utils::tail(strsplit(
                    rdataB$datapath, ".", fixed = TRUE
                  )[[1]], n = 1),
                  " file. <br> Please select a .RData file <br> or choose another file format."
                )
                megaplot_data <- NULL
                megaplot_error_message2 <- NULL
              } else {
                if (
                !is.null(B_subjectid_rdata_files) &
                !is.null(B_event_time_rdata_files) &
                B_subjectid_rdata_files != "" &
                B_event_time_rdata_files != ""
                
              ) {               
                B <- base::get(load(
                  file = rdataB$datapath
                ))
                if (B_subjectid_rdata_files %in% colnames(B) &
                 B_event_time_rdata_files %in% colnames(B)) {
                  B <- B %>% 
                  dplyr::rename(
                      megaplots_selected_subjectid = B_subjectid_rdata_files,
                      megaplots_selected_event_time = B_event_time_rdata_files
                  )    
                }
                megaplot_error_message1 <- NULL
                }
              }
            }
        if (
          !is.null(A_subjectid_rdata_files) &
          !is.null(A_start_time_rdata_files) &
          !is.null(A_end_time_rdata_files) &
          !is.null(B_subjectid_rdata_files) &
          !is.null(B_event_time_rdata_files) &
          A_subjectid_rdata_files != "" &
          A_start_time_rdata_files != "" &
          A_end_time_rdata_files != "" &
          B_subjectid_rdata_files != "" &
          B_event_time_rdata_files != ""
          
        ) {
          if (
            A_subjectid_rdata_files != A_start_time_rdata_files &
            A_subjectid_rdata_files != A_end_time_rdata_files &
            A_start_time_rdata_files != A_end_time_rdata_files &
            B_subjectid_rdata_files != B_event_time_rdata_files
          ) {
            if (
              all(c("megaplots_selected_subjectid","megaplots_selected_start_time","megaplots_selected_end_time") %in% colnames(A)) &
              all(c("megaplots_selected_subjectid","megaplots_selected_event_time") %in% colnames(B))
            ) {
              if (all(A$megaplots_selected_start_time <= A$megaplots_selected_end_time)) {
                B <- B[!is.na(B$megaplots_selected_event_time), ]
                name <- gsub('.RData', '', rdataA$name)
                name <- gsub('_', ' ', name)
                megaplot_error_message <- NULL
  
                A <- cbind(
                  A %>% dplyr::select(c(megaplots_selected_subjectid,megaplots_selected_start_time,megaplots_selected_end_time)),
                  A %>% dplyr::select(-c(megaplots_selected_subjectid,megaplots_selected_start_time,megaplots_selected_end_time))
                )
                B <- cbind(
                  B %>% dplyr::select(c(megaplots_selected_subjectid,megaplots_selected_event_time)),
                  B %>% dplyr::select(-c(megaplots_selected_subjectid,megaplots_selected_event_time))
                )
                      
                megaplot_data <- list(
                  A = A,
                  B = B,
                  name = name
                )
                    
              } else {
                megaplot_error_message <-
                  paste0(
                    "The start time in dataset A should be smaller than the end time for all subjects."
                  )
                megaplot_data <- NULL
              }
            } else {
              megaplot_error_message <-
                  paste0(
                    "Please select unique columns for dataset A and B!"
                  )
              megaplot_data <- NULL
            }
          }
        }
      } 
    } else if (selectdata == 'Use demo data') {
      
      A <- megaplots_demo_data$A %>% 
        dplyr::rename(
          megaplots_selected_subjectid = "subjectid",
          megaplots_selected_start_time = "start_time",
          megaplots_selected_end_time = "end_time"
        )
      B <- megaplots_demo_data$B %>% 
        dplyr::rename(
          megaplots_selected_subjectid = "subjectid",
          megaplots_selected_event_time = "ae_time"
        )
      #remove rows with missing ae_time value
      B <- B[!is.na(B$megaplots_selected_event_time),]
      name <- "MegaPlot example data 1"
      megaplot_data <- list(A = A, B = B, name = name)
    } else if (selectdata == "Upload saved data") {
      shiny::req(setting_file)
      if (!is.null(setting_file)) {
        if (utils::tail(strsplit(setting_file$datapath, ".", fixed = TRUE)[[1]], n = 1) != "rds") {
          megaplot_error_message <- paste0(
            "Wrong data format for saved dataset. <br> You have selected a ",
            utils::tail(
              strsplit(setting_file$datapath, ".", fixed = TRUE)[[1]],
              n = 1
            ),
            " file. <br> Please select a .rds file <br> or choose another upload method."
          )
          megaplot_data <- NULL
        } else {
          saved_file <- readRDS(setting_file$datapath)
      
          if (all(c("subjectid","start_time","end_time") %in% colnames(saved_file$A)) & all(!c("megaplots_selected_subjectid","megaplots_selected_start_time","megaplots_selected_end_time") %in% colnames(saved_file$A))) {
            saved_file$A <- saved_file$A %>% 
              dplyr::rename(
                megaplots_selected_subjectid = "subjectid",
                megaplots_selected_start_time = "start_time",
                megaplots_selected_end_time = "end_time"
              )
          }
        
          if (all(c("subjectid","event_time") %in% colnames(saved_file$B)) & all(!c("megaplots_selected_subjectid","megaplots_selected_event_time") %in% colnames(saved_file$B))) {
            saved_file$B <- saved_file$B %>% 
              dplyr::rename(
                megaplots_selected_subjectid = "subjectid",
                megaplots_selected_event_time = "ae_time"
              )
          }
          megaplot_data <- list(
            A = saved_file$A,
            B = saved_file$B,
            name = saved_file$name,
            saved = saved_file
          )
        }
      }
    }
  data <- list(
    megaplot_data = megaplot_data,
    megaplot_error_message = megaplot_error_message
  )
  
  return(data)
  
}