#' get_gender
#'
#' Determine the gender of a given text string
#' @param text The text string
#' @param country The country code for the text
#' @return Gender of the text 
#' @export
get_gender <- function (text, country)
{
    rcpp_gender (text)
}
