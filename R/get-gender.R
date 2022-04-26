#' get_gender
#'
#' Get the gender of a character vector
#' @param text The character vector to encode
#' @param country Country for which gender is to be determined, either as
#' ISO3166 two-letter abbreviation, or full text. See \link{list_countries} for
#' list of recognized countries.
#' @return `data.frame` of text with gender attached
#' @export
get_gender <- function (text, country = NULL) {

    text <- gsub ("[[:punct:]]", " ", text)

    if (is.null (country))
        country <- 0
    else
        country <- get_country (country)

    values <- unique (text)
    res <- .Call ("R_gender", as.character (values), as.integer (country))
    res_expanded <- res [match (text, values)]

    map_gender_results (text, res_expanded)
}

map_gender_results <- function (text, res) {

    map <- data.frame (res = c (70, 102, 77, 109, 63, 67, 32, 69, 73),
                       category = c ("IS_FEMALE", "IS_MOSTLY_FEMALE",
                                     "IS_MALE", "IS_MOSTLY_MALE",
                                     "IS_UNISEX_NAME", "IS_A_COUPLE",
                                     "NAME_NOT_FOUND", "ERROR_IN_NAME",
                                     "INTERNAL_ERROR_GENDER"),
                       stringsAsFactors = FALSE)

    data.frame (text = text,
                gender = map$category [match (res, map$res)],
                stringsAsFactors = FALSE)
}



get_country <- function (country) {

    dat <- country_data ()
    i <- NULL
    if (nchar (country) == 2)
        i <- match (tolower (country), tolower (dat$iso3166))

    if (length (i) != 1) {

        i <- grep (paste0 ("^", tolower (country)), tolower (dat$country_text))
        if (length (i != 1))
            stop (paste0 ("Country not found; please enter either 2-letter ",
                          "ISO3166 abbreviation or full country name.\n",
                          "See 'list_countries' for details"))
    }
    dat$gc_country [i]
}

#' list_countries
#'
#' List all countries included in the first name gender matcher
#' @return A `data.frame` object of country names and associated ISO3166
#' abbreviations.
#' @export
list_countries <- function () {

    dat <- country_data ()
    dat [1:(nrow (dat) - 2), c ("country_text", "iso3166")]
}

country_data <- function () {

    # nolint start
    VERY_GOOD <- 1
    GOOD <- 2
    MEDIUM <- 3

    GC_BRITAIN <- 1
    GC_IRELAND <- 2
    GC_USA <- 3
    GC_SPAIN <- 4
    GC_PORTUGAL <- 5
    GC_ITALY <- 6
    GC_MALTA <- 7
    GC_FRANCE <- 8
    GC_BELGIUM <- 9
    GC_LUXEMBOURG <- 10
    GC_NETHERLANDS <- 11
    GC_GERMANY <- 12
    GC_EAST_FRISIA <- 13
    GC_AUSTRIA <- 14
    GC_SWISS <- 15
    GC_ICELAND <- 16
    GC_DENMARK <- 17
    GC_NORWAY <- 18
    GC_SWEDEN <- 19
    GC_FINLAND <- 20
    GC_ESTONIA <- 21
    GC_LATVIA <- 22
    GC_LITHUANIA <- 23
    GC_POLAND <- 24
    GC_CZECH_REP <- 25
    GC_SLOVAKIA <- 26
    GC_HUNGARY <- 27
    GC_ROMANIA <- 28
    GC_BULGARIA <- 29
    GC_BOSNIA <- 30
    GC_CROATIA <- 31
    GC_KOSOVO <- 32
    GC_MACEDONIA <- 33
    GC_MONTENEGRO <- 34
    GC_SERBIA <- 35
    GC_SLOVENIA <- 36
    GC_ALBANIA <- 37
    GC_GREECE <- 38
    GC_RUSSIA <- 39
    GC_BELARUS <- 40
    GC_MOLDOVA <- 41
    GC_UKRAINE <- 42
    GC_ARMENIA <- 43
    GC_AZERBAIJAN <- 44
    GC_GEORGIA <- 45
    GC_KAZAKH_UZBEK <- 46
    GC_TURKEY <- 47
    GC_ARABIA <- 48
    GC_ISRAEL <- 49
    GC_CHINA <- 50
    GC_INDIA <- 51
    GC_JAPAN <- 52
    GC_KOREA <- 53
    GC_VIETNAM <- 54
    # nolint end

    dat <- rbind (
        c (30, 0, VERY_GOOD,  60, GC_BRITAIN,    "UK",   "Great Britain"),
        c (31, 0, GOOD,        4, GC_IRELAND,    "IRE",  "Ireland"),
        c (32, 0, VERY_GOOD, 150, GC_USA,        "USA",  "U.S.A."),
        c (33, 0, VERY_GOOD,  60, GC_ITALY,       "I",   "Italy"),
        c (34, 0, MEDIUM,      1, GC_MALTA,       "M",   "Malta"),
        c (35, 0, GOOD,       10, GC_PORTUGAL,    "P",   "Portugal"),
        c (36, 0, VERY_GOOD,  40, GC_SPAIN,       "E",   "Spain"),
        c (37, 0, VERY_GOOD,  60, GC_FRANCE,      "F",   "France"),
        c (38, 0, VERY_GOOD,  10, GC_BELGIUM,     "B",   "Belgium"),
        c (39, 0, VERY_GOOD,   1, GC_LUXEMBOURG,  "LUX", "Luxembourg"),
        c (40, 0, VERY_GOOD,  14, GC_NETHERLANDS, "NL",  "the Netherlands"),
        c (41, 0, GOOD,        1, GC_EAST_FRISIA, "FRI", "East Frisia"),
        c (42, 0, VERY_GOOD,  80, GC_GERMANY,     "D",   "Germany"),
        c (43, 0, VERY_GOOD,   8, GC_AUSTRIA,     "A",   "Austria"),
        c (44, 0, VERY_GOOD,   7, GC_SWISS,       "CH",  "Swiss"),
        c (45, 0, VERY_GOOD,   1, GC_ICELAND,     "ICE", "Iceland"),
        c (46, 0, VERY_GOOD,   5, GC_DENMARK,     "DK",  "Denmark"),
        c (47, 0, GOOD,        4, GC_NORWAY,      "N",   "Norway"),
        c (48, 0, VERY_GOOD,   8, GC_SWEDEN,      "S",   "Sweden"),
        c (49, 0, GOOD,        5, GC_FINLAND,     "FIN", "Finland"),
        c (50, 0, GOOD,        2, GC_ESTONIA,     "EST", "Estonia"),
        c (51, 0, GOOD,        2, GC_LATVIA,      "LTV", "Latvia"),
        c (52, 0, GOOD,        3, GC_LITHUANIA,   "LIT", "Lithuania"),
        c (53, 0, GOOD,       35, GC_POLAND,      "PL",  "Poland"),
        c (54, 0, GOOD,        8, GC_CZECH_REP,   "CZ",  "Czech Republic"),
        c (55, 0, GOOD,        7, GC_SLOVAKIA,    "SK",  "Slovakia"),
        c (56, 0, GOOD,       11, GC_HUNGARY,     "H",   "Hungary"),
        c (57, 0, VERY_GOOD,  22, GC_ROMANIA,     "RO",  "Romania"),
        c (58, 0, GOOD,        9, GC_BULGARIA,    "BG",  "Bulgaria"),
        c (59, 0, MEDIUM,      4, GC_BOSNIA,      "BIH","Bosnia and Herzegovina"), # nolint
        c (60, 0, GOOD,        5, GC_CROATIA,     "CRO", "Croatia"),
        c (61, 0, MEDIUM,      1, GC_KOSOVO,      "KOS", "Kosovo"),
        c (62, 0, MEDIUM,      2, GC_MACEDONIA,   "MK",  "Macedonia"),
        c (63, 0, MEDIUM,      1, GC_MONTENEGRO,  "MON", "Montenegro"),
        c (64, 0, MEDIUM,      9, GC_SERBIA,      "SER", "Serbia"),
        c (65, 0, MEDIUM,      2, GC_SLOVENIA,    "SLO", "Slovenia"),
        c (66, 0, GOOD,        3, GC_ALBANIA,     "AL",  "Albania"),
        c (67, 0, GOOD,       10, GC_GREECE,      "GR",  "Greece"),
        c (68, 0, GOOD,      100, GC_RUSSIA,      "RUS", "Russia"),
        c (69, 0, MEDIUM,     10, GC_BELARUS,     "BY",  "Belarus"),
        c (70, 0, MEDIUM,      4, GC_MOLDOVA,     "MOL", "Moldova"),
        c (71, 0, MEDIUM,     45, GC_UKRAINE,     "UKR", "Ukraine"),
        c (72, 0, MEDIUM,      3, GC_ARMENIA,     "ARM", "Armenia"),
        c (73, 0, MEDIUM,      4, GC_AZERBAIJAN,  "AZE", "Azerbaijan"),
        c (74, 0, MEDIUM,      5, GC_GEORGIA,     "GEO", "Georgia"),
        c (75, 0, MEDIUM,     15, GC_KAZAKH_UZBEK, "KAZ", "Kazakhstan/Uzbekistan,etc."), # nolint
        c (76, 0, GOOD,       55, GC_TURKEY,      "TR",  "Turkey"),
        c (77, 0, GOOD,       80, GC_ARABIA,      "AR",  "Arabia/Persia"),
        c (78, 0, MEDIUM,      4, GC_ISRAEL,      "ISR", "Israel"),
        c (79, 0, VERY_GOOD, 300, GC_CHINA,       "CHN", "China"),
        c (80, 0, GOOD,      250, GC_INDIA,       "IND", "India/Sri Lanka"),
        c (81, 0, GOOD,       35, GC_JAPAN,       "JAP", "Japan"),
        c (82, 0, GOOD,       12, GC_KOREA,       "KOR", "Korea"),
        c (83, 0, GOOD,       17, GC_VIETNAM,     "VN",  "Vietnam"),
        c (84, 0, MEDIUM,      1,    -1L,         "XX",  "other countries"),
        c (0,  0,  0,          0,    -1L,         "",    ""))

    dat <- data.frame (dat, stringsAsFactors = FALSE)
    names (dat) <- c ("pos", "n", "quality_of_statistics", "weight",
                      "gc_country", "country_short", "country_text")
    dat$pos <- as.integer (dat$pos)
    dat$n <- as.integer (dat$n)
    dat$quality_of_statistics <- as.integer (dat$quality_of_statistics)
    dat$weight <- as.integer (dat$weight)
    dat$gc_country <- as.integer (dat$gc_country)

    # match from country_short to iso3166 codes
    iso3166 <- rbind (c ("UK", "gb"),
                      c ("IRE", "ir"),
                      c ("USA", "us"),
                      c ("I", "it"),
                      c ("M", "mt"),
                      c ("P", "pt"),
                      c ("E", "es"),
                      c ("F", "fr"),
                      c ("B", "be"),
                      c ("LUX", "lu"),
                      c ("NL", "nl"),
                      c ("FRI", "ef"), # not an ISO 3166 country
                      c ("D", "de"),
                      c ("A", "at"),
                      c ("CH", "ch"),
                      c ("ICE", "is"),
                      c ("DK", "dk"),
                      c ("N", "no"),
                      c ("S", "se"),
                      c ("FIN", "fi"),
                      c ("EST", "ee"),
                      c ("LTV", "lv"),
                      c ("LIT", "lt"),
                      c ("PL", "pl"),
                      c ("CZ", "cz"),
                      c ("SK", "sk"),
                      c ("H", "hu"),
                      c ("RO", "ro"),
                      c ("BG", "bg"),
                      c ("BIH", "ba"),
                      c ("CRO", "hr"),
                      c ("KOS", "ko"), # not an ISO 3166 country
                      c ("MK", "mk"),
                      c ("MON", "me"),
                      c ("SER", "rs"),
                      c ("SLO", "si"),
                      c ("AL", "al"),
                      c ("GR", "gr"),
                      c ("RUS", "ru"),
                      c ("BY", "by"),
                      c ("MOL", "md"),
                      c ("UKR", "ua"),
                      c ("ARM", "am"),
                      c ("AZE", "az"),
                      c ("GEO", "ge"),
                      c ("KAZ", "kz"),
                      c ("TR", "tr"),
                      c ("AR", "ar"), # not an ISO 3166 country
                      c ("ISR", "il"),
                      c ("CHN", "cn"),
                      c ("IND", "in"),
                      c ("JAP", "jp"),
                      c ("KOR", "kr"),
                      c ("VN", "vn"),
                      c ("XX", "xx"),
                      c ("", ""))
    index <- match (iso3166 [, 1], dat$country_short)
    dat$iso3166 <- iso3166 [index, 2]

    return (dat)
}
