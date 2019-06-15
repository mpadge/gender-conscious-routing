#include "gender-rcpp.h"

//' rcpp_gender
//'
//' @noRd
// [[Rcpp::export]]
std::string rcpp_gender (const Rcpp::CharacterVector text_in)
{
    int country = GENDER_DEFAULT_COUNTRY;
    int i = initialize_gender ();

    std::string str = Rcpp::as <std::string> (text_in [0]);
    //const char * ch = str.c_str ();
    char *ch = &str[0u];
    i = get_gender (ch, GENDER_COMPARE_EXPANDED_UMLAUTS, country);
    Rcpp::Rcout << "[" << i << "]" << std::endl;
    trace_info ("final result for", ch, NULL,i, NULL);

    cleanup_gender();
    std::string result = "result";

    return result;
}
