#' Transforming class from numeric to integer when data frame column is integer but class is numeric
#'
#' @param df  data frame that needs to be checked
#'
#' @return updated data frame object
#'
#' @noRd
#' @keywords internal

numeric_to_integer <- function(df) {
  if (!is.null(df)) {
    if(is.data.frame(df)) {
      if (dim(df)[1] != 0) {
        df <- data.frame(df)
        for(i in 1:dim(df)[2]) {
          # if (!is.character(df[,i])) {
            if (all(!is.na(df[,i]))) {
              if (!is.numeric(df[,i])) {
              #check if values are smaller than machines maximal integer
              if (all(df[,i] <= .Machine$integer.max)) {
                if (all(grepl('^(?=.)([+-]?([0-9]*)(\\.([0-9]+))?)$',df[,i], perl = TRUE))){
                # if (all(stats::na.omit(df[,i] == as.numeric(df[,i])))) {
                  df[,i] <- as.numeric(df[,i])
                }
              }
            }
              # create note for cases where integer.max is reached
          }
          #
        }
      }
    }
  }
  return(df)
}
