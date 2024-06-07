test_that("Check sequencing functions:", {

  #example data A:
  A_ <- data.frame(
    megaplots_selected_subjectid = 1001:1100,
    megaplots_selected_start_time = c(-6, 10, -2, 1, 0, -1, -5, -5, 3, -8, -8, 
                                      -8, 6, -8, 3, -3, -7, -7, -5, -2, -10, 2, 
                                      -7, -4, 9, 8, 2, 6, -5, -10, 0, -1, 8, 0,
                                      -3, 10, 8, -7, -8, -7, 7, 5, 3, -8, -6, 
                                      -7, 10, -7, -10, 4, 5, -4, 5, -2, 7, 5, 
                                      0, -9, 9, -3, 10, -1, -7, -8, 6, 9, -1, 
                                      -2, 6, 1, -8, 4, -6, -4, -6, 10, 8, 3, 3,
                                      -7, 4, 3, 10, 8, 7, -7, -3, 7, -4, 2, -2,
                                      -9, 1, 9, 5, 1, -8, -3, -3, -3),
    megaplots_selected_end_time = c(97, 105, 98, 95, 91, 93, 92, 94, 96, 110,
                                    108, 107, 105, 90, 107, 100, 103, 107, 99,
                                    98, 94, 95, 93, 102, 98, 110, 102, 95, 100,
                                    92, 96, 96, 98, 93, 94, 99, 92, 94, 99, 97,
                                    98, 109, 98, 104, 105, 105, 92, 98, 97, 99, 
                                    104, 93, 103, 105, 103, 105, 103, 91, 110,
                                    109, 102, 99, 103, 97, 97, 94, 101, 105, 109,
                                    102, 97, 107, 90, 100, 103, 106, 108, 99,
                                    104, 92, 107, 98, 110, 101, 108, 106, 90, 
                                    93, 100, 104, 97, 100, 106, 98, 99, 93,
                                    103, 98, 105, 95),
    treatment = c(rep("Treatment1",50),rep("Placebo",50))
  )
  
  #sample data B with exact 1 event per subject 
  B_ <- data.frame(
    megaplots_selected_subjectid = 1001:1100,
    megaplots_selected_event_time = 1:100,
    pain_intensity = rep(c("mild","moderate","severe"), length=100)
  )
    
  # example values
  da_ <- list(
    A = A_,
    B = B_,
    nume_A = c("megaplots_selected_subjectid", "megaplots_selected_start_time", "megaplots_selected_end_time")
  )
  var_ <- "pain_intensity"
  
  par_ <- list(
      "distmeasure" = "OM",
      "sm" = "CONSTANT",
      "smDHD" = "INDELS",
      "norm" = "auto",
      "norm2" = "auto",
      "indel" = "auto",
      "indel_numeric" = 1,
      "expcost" = 0.5,
      "context" = 0,
      "link" = "mean",
      "h_OMslen" = 0.5,
      "transindel" = "constant",
      "tpow" = 1,
      "otto" = 0.5,
      "previous" = FALSE,
      "add.column" = TRUE, "overlap"= FALSE,
      "step" = 1,
      "weighted" = TRUE,
      "methMissing" = "new state"
    )
  sermethod_ <- "GW_average"
  
  ## sequencing_var_app function
  
  return_value <- sequencing_var_app(
    da = da_,
    var = var_,
    par = par_,
    sermethod = sermethod_,
    group = NULL,
    multiple_distmeasures = FALSE
  )    
  
  #tests
  expect_equal(return_value$A$SEQUENCING, c(1, 100, 9, 2, 3, 8, 5, 4, 7, 6, 15, 10, 13, 14, 11, 12, 16, 21, 18, 17, 20, 19, 27, 22, 25, 26, 23, 24, 28, 33, 30, 29, 32, 31, 39, 34, 37, 38, 35, 36, 40, 45, 42, 41, 44, 43, 51, 46, 49, 50, 47, 48, 52, 57, 54, 53, 56, 55, 63, 58, 61, 62, 59, 60, 64, 69, 66, 65, 68, 67, 75, 70, 73, 74, 71, 72, 76, 81, 78, 77, 80, 79, 87, 82, 85, 86, 83, 84, 88, 93, 90, 89, 92, 91, 99, 94, 97, 98, 95, 96))
  
  
  
  ## get_B_wide function
  gBw <- get_B_wide(
    da = da_,
    var = var_,
    method_missing = par_$methMissing,
    par = par_
  )
  
  #tests
  
  ## get_alphab function
  ga <- get_alphab(
    da = da_, 
    B_wide = gBw[[1]], 
    var = var_, 
    method_missing = par_$methMissing
  )
  
  #test
  expect_equal(ga, c("mild","moderate","severe"))
  
  
  #outdated:
  sqg <- sq_grouping(
    da = da_,
    var = var_,
    par = par_,
    sermethod = sermethod_,
    group = "treatment",
    B_wide = gBw[[1]],
    alphab = ga
  )

  sqgv <- sq_grouping_var(
    da = da_,
    var = var_,
    par = par_,
    sermethod = sermethod_,
    group = "treatment",
    multiple_distmeasures = FALSE
  )
  
  expect_equal(sqg, sqgv)

  seq_ <- suppressMessages(TraMineR::seqdef(gBw[[1]], 2:ncol(gBw[[1]]), labels = ga))
  
  gp <- get_parameters(
    seq = seq_, 
    par = par_
  )
  
})