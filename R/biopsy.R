#' Find Banff Score In String
#'
#' @param fstring A string to parse
#' @param fpara A string. The Banff score ("a", "cg", "mm"...)
#'
#' @importFrom stringr str_c
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_replace_all
#'
bio_par <- function(fstring, fpara){

  fpara <- str_to_lower(fpara)

  fchars_s <- c("\\b",fpara,"\\d\\b")
  fstr_s <- str_c(fchars_s, collapse = "")

  fchars_l <- c("\\b",fpara,"\\d-\\d\\b")
  fstr_l <- str_c(fchars_l, collapse = "")

  fchars_m_s <- c("\\b",fpara,"\\s\\d\\b")
  fstr_m_s <- str_c(fchars_s, collapse = "")

  fchars_m_l <- c("\\b",fpara,"\\s\\d-\\d\\b")
  fstr_m_l <- str_c(fchars_s, collapse = "")

  return(
    suppressWarnings(
      ifelse(str_extract_all(fstring, fstr_l) != 'character(0)',
             ifelse(str_extract_all(fstring, fstr_l) == 'character(0)',
                    ifelse(str_extract_all(fstring, fstr_m_l) == 'character(0)',
                           NA,
                           str_replace_all(str_extract_all(fstring, fstr_m_l), "[a-z|A-Z]|[[:punct:]]", "")),
                    str_replace_all(str_extract_all(fstring, fstr_l), "[a-z|A-Z]", "")
             ),
             ifelse(str_extract_all(fstring, fstr_s) == 'character(0)',
                    ifelse(str_extract_all(fstring, fstr_m_s) == 'character(0)',
                           NA,
                           str_replace_all(str_extract_all(fstring, fstr_m_s), "[a-z|A-Z]|[[:punct:]]", "")),
                    str_replace_all(str_extract_all(fstring, fstr_s), "c4d|[a-z|A-Z]|[[:punct:]]", "")
             )
      )
    )
  )
}

#' Convert Range To Int
#'
#' @param fstring A string. Interval to change into a integer
#'
#' @importFrom stringr str_extract_all
#' @importFrom purrr as_vector
#' @importFrom stringr str_detect
#'
bio_par_range_to_int <-  base::Vectorize(function(fstring){
  return(
    if(!is.na(fstring)){
      if(str_detect(fstring, "^\\d-\\d$")){
        value <- stringr::str_extract_all(fstring, "\\d+")
        value <- max(as.numeric(purrr::as_vector(value)))
      } else if(str_detect(fstring, "^\\d$")){
        value <- stringr::str_extract_all(fstring, "\\d+")
        value <- as.numeric(purrr::as_vector(value))
      } else if(str_detect(fstring, "^\\d\\s\\d$")){
        value <- stringr::str_extract_all(fstring, "^\\d")
        value <- max(as.numeric(purrr::as_vector(value)))
      } else if(str_detect(fstring, "^-\\d-\\d")){
        value <- stringr::str_extract_all(fstring, "\\d+")
        value <- max(as.numeric(purrr::as_vector(value)))
      } else if(str_detect(fstring, "\"\\d")){
        value <- stringr::str_extract_all(fstring, "\\d+")
        value <- max(as.numeric(purrr::as_vector(value)))
      } else if(str_detect(fstring, "\\d\\s\\d\\s\\d")){
        value <- stringr::str_extract_all(fstring, "\\d+")
        value <- max(as.numeric(purrr::as_vector(value)))
      }
    } else {
      value <- NA
    }
  )
})


#' Parse a string into Banff Classification scores
#'
#' Parse a string (e.g.a unstructured written report) into structured form of Banff Classification scores.
#' Creates multiple new columns for each classifier.
#'
#' @param data A data frame or a data frame extension (a tibble).
#' @param col Name of the column containing strings to parse
#' @param eliminate_intervals Logical value. When `TRUE` transforms score intervals into integers. Default `TRUE`.
#' @param na_to_zeroes Logical value. When `TRUE` transforms `NA` values (not found or unable to classify) to `0`. Default `FALSE`.
#'
#' @import magrittr
#' @import dplyr
#'
#'
#' @export
biopsy_parse <- function(data, col, eliminate_intervals = TRUE, na_to_zeroes = FALSE) {

  data %<>%
    #Glomeruly:
  dplyr::mutate(bio_g = bio_par({{col}}, "g")) %>%
    dplyr::mutate(bio_cg = bio_par({{col}}, "cg")) %>%
    dplyr::mutate(bio_mm = bio_par({{col}}, "mm")) %>%
    #Intersticium:
    dplyr::mutate(bio_i = bio_par({{col}}, "i")) %>%
    dplyr::mutate(bio_ci = bio_par({{col}}, "ci")) %>%
    dplyr::mutate(bio_ti = bio_par({{col}}, "ti")) %>%
    dplyr::mutate(bio_iifta = bio_par({{col}}, "iif/ta")) %>%
    dplyr::mutate(bio_ifta = bio_par({{col}}, "if/ta")) %>%
    #Tubuly:
    dplyr::mutate(bio_t = bio_par({{col}}, "t")) %>%
    dplyr::mutate(bio_ct = bio_par({{col}}, "ct")) %>%
    #Cévy:
    dplyr::mutate(bio_v = bio_par({{col}}, "v")) %>%
    dplyr::mutate(bio_cv = bio_par({{col}}, "cv")) %>%
    dplyr::mutate(bio_ah = bio_par({{col}}, "ah")) %>%
    dplyr::mutate(bio_aah = bio_par({{col}}, "aah")) %>%
    #Peritubulární kapiláry:
    dplyr::mutate(bio_ptcs = bio_par({{col}}, "ptc-s")) %>%
    dplyr::mutate(bio_ptcq = bio_par({{col}}, "ptc-q")) %>%
    dplyr::mutate(bio_ptce = bio_par({{col}}, "ptc-e")) %>%
    dplyr::mutate(bio_c4d = bio_par({{col}}, "c4d"))

  if (eliminate_intervals) {
    data %<>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("bio_"), ~ bio_par_range_to_int(.)))
  }

  if (na_to_zeroes) {
    data %<>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("bio_"), ~ base::ifelse(is.na(.), 0, .)))
  }

  return(data)
}
