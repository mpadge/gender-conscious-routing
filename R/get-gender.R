#' get_gender
#' 
#' Get the gender of a character vector
#' @param text The character vector to encode
#' @return Vector of name length with gender codes
#' @export
get_gender <- function (text)
{
    .Call ("R_gender", as.character (text))
}
