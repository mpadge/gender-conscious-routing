#' gender_streetnet
#'
#' Weight a street network via \pkg{dodgr} function `weight_streetnet`, adding
#' additional information on gender of street names
#'
#' @inheritParams get_gender
#' @param net Street network in \pkg{sf} or \pkg{sc} format
#' @param wt_profile Type of weighting to be used; see `dodgr::weight_streetnet`
#' for details.
#' @return A weighted street network in \pkg{dodgr} format.
#' @export
gender_streetnet <- function (net, wt_profile = "foot", country = "de") {

    netw <- dodgr::weight_streetnet (net, wt_profile = wt_profile,
                                     keep_cols = "name")

    g <- get_gender (netw$name)
    g$gender [is.na (g$text)] <- NA_character_

    if (tolower (country) == "de") {

        # These can not be personal names:
        index <- which (substring (g$text, 1, 6) == "Alter" |
                        substring (g$text, 1, 3) == "Am" |
                        substring (g$text, 1, 3) == "An " |
                        substring (g$text, 1, 4) == "Auf " |
                        substring (g$text, 1, 3) == "Im " |
                        substring (g$text, 1, 3) == "In " |
                        substring (g$text, 1, 4) == "Zum " |
                        substring (g$text, 1, 4) == "Zur " |
                        substring (g$text, 1, 3) == "Zu " |
                        !grepl ("-|\\s", g$text) |
                        grepl ("[0-9]", g$text) |
                        grepl ("Heide$", g$text))
        g$gender [index] <- "NAME_NOT_FOUND"
    }

    netw$gender <- "NAME_NOT_FOUND"
    netw$gender [which (g$gender == "IS_MALE" |
                        g$gender == "IS_MOSTLY_MALE")] <- "IS_MALE"
    netw$gender [which (g$gender == "IS_FEMALE" |
                        g$gender == "IS_MOSTLY_FEMALE")] <- "IS_FEMALE"

    netw$female <- FALSE
    netw$female [which (netw$gender == "IS_FEMALE")] <- TRUE

    return (netw)
}