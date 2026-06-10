
#' Getting input-parameters for the seqdist-function
#'
#' @description This internal function returns a named list with all necessary input arguments for the seqdist function.
#'
#' @param seq state sequence object
#' @param par list of parameters for the distance measure
#'
#' @return named list as an input for the seqdist function
#'
#' @importFrom TraMineR alphabet
#'
#' @noRd
#' @keywords internal
get_parameters <- function(seq, par) {
  seqargs_all <- list(
    "seqdata" = seq,
    "method" = par$distmeasure,
    "with.missing" = TRUE
  )
  distmeasure <- par$distmeasure

  if (distmeasure %in% c('OM', 'OMloc', 'OMslen', 'OMspell', 'OMstran', 'HAM', 'TWED')) {
    if (par$sm == "ORDINAL") {
      if (any(seq == "*")) {
        with.missing <- TRUE
      }
      else{
        with.missing <- FALSE
      }
      l <-
        length(TraMineR::alphabet(seq, with.missing = with.missing))
      sub_matrix <- matrix(
        rep(0, l ^ 2),
        nrow = l,
        ncol = l,
        dimnames = list(
          TraMineR::alphabet(seq, with.missing = with.missing),
          TraMineR::alphabet(seq, with.missing = with.missing)
        )
      )
      for (i in 1:l) {
        for (j in 1:l) {
          if (i != j) {
            dimn <-
              c(dimnames(sub_matrix)[[1]][i], dimnames(sub_matrix)[[2]][j])
            if (any(dimn == "*")) {
              sub_matrix[i, j] <- mean(1:(l - 1))
            }
            else{
              sub_matrix[i, j] <- abs(i - j)
            }
          }
        }
      }
      seqargs_all[["sm"]] <- sub_matrix
    }
    else{
      seqargs_all["sm"] <- par$sm
    }
  }
  if (distmeasure == "DHD") {
    if (par$smDHD == "ORDINAL") {
      if (any(seq == "*")) {
        with.missing <- TRUE
      }
      else{
        with.missing <- FALSE
      }
      l <-
        length(TraMineR::alphabet(seq, with.missing = with.missing))
      sub_matrix <- matrix(
        rep(0, l ^ 2),
        nrow = l,
        ncol = l,
        dimnames = list(
          TraMineR::alphabet(seq, with.missing = with.missing),
          TraMineR::alphabet(seq, with.missing = with.missing)
        )
      )
      for (i in 1:l) {
        for (j in 1:l) {
          if (i != j) {
            dimn <-
              c(dimnames(sub_matrix)[[1]][i], dimnames(sub_matrix)[[2]][j])
            if (any(dimn == "*")) {
              sub_matrix[i, j] <- mean(1:(l - 1))
            }
            else{
              sub_matrix[i, j] <- abs(i - j)
            }
          }
        }
      }
      seqargs_all[["sm"]] <- sub_matrix
    }
    else{
      seqargs_all["sm"] <- par$smDHD
    }

  }

  if (distmeasure %in% c(
    'OM',
    'OMloc',
    'OMslen',
    'OMspell',
    'OMstran',
    'HAM',
    'DHD',
    'LCS',
    'LCS',
    'RLCP',
    'TWED'
  )) {
    seqargs_all["norm"] <- par$norm
  }
  if (distmeasure %in% c('CHI2', 'EUCLID')) {
    seqargs_all["norm"] <- par$norm2
  }
  if (distmeasure %in% c('OM', 'OMslen', 'OMspell', 'OMstran')) {
    if (par$indel == "numeric value") {
      seqargs_all["indel"] <- par$indel_numeric
    }
    else{
      seqargs_all["indel"] <- par$indel
    }

  }
  if (distmeasure %in% c('OMloc', 'OMspell')) {
    seqargs_all["expcost"] <- as.numeric(par$expcost)
  }
  if (distmeasure == "OMloc") {
    seqargs_all["context"] <- as.numeric(par$context)
  }
  if (distmeasure == "OMslen") {
    seqargs_all["link"] <- par$link
    seqargs_all["h"] <- par$h_OMslen
  }
  if (distmeasure == "OMstran") {
    seqargs_all["transindel"] <- par$transindel
    seqargs_all["otto"] <- par$otto
    seqargs_all["previous"] <- as.logical(par$previous)
    seqargs_all["add.column"] <- as.logical(par$add.column)

  }
  if (distmeasure %in% c("OMloc", "NMSMST", "SVRspell")) {
    seqargs_all["tpow"] <- par$tpow
  }
  if (distmeasure %in% c("EUCLID", "CHI2")) {
    seqargs_all["step"] <- as.numeric(par$step)
    seqargs_all["overlap"] <- as.logical(par$overlap)
  }
  if (distmeasure %in% "CHI2") {
    seqargs_all["weighted"] <- as.logical(par$weighted)
  }


  return(seqargs_all)

}

