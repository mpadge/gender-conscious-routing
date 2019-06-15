#pragma once

#include <Rcpp.h>

#include "gender.h"

std::string rcpp_gender (const Rcpp::CharacterVector text_in);
