
#' Filter values by the nearest value to a date
#'
#' Match nearest sample values (e. g. laboratory results, measurements) to a date by subject IDs (e.g. patient ID).
#'
#' @param data A dataframe containing subject IDs and dates to filter by
#' @param patient_id A column of unique IDs of a patient or a subject
#' @param data_date A column of date values
#' @param lab_data A dataframe of laboratory values with Patient's ID, Sample date and Sample Value
#' @param lab_data_date A column of Sample dates
#' @param lab_data_value A column of Sample Values
#' @param matching Controls the matching dates mode. "exact" or "interval"
#' * "exact" matches dates (`date`, and `lab_date`) to be the same
#' * "interval" matches `lab_date` to be within a defined time interval of `date`
#' @param omit_na Whether to omit missing Sample Values, TRUE by default
#' @param duplicit_values Removes duplicit Samples from the same day
#' @param interval_min Used when `matching` set to "interval". Defines the lower bound of the interval in days.
#' @param interval_max Used when `matching` set to "interval". Defines the upper bound of the interval in days.
#'
#' @export
labs_near_date <- function(data, patient_id, data_date, lab_data, lab_data_date, lab_data_value, matching = "exact", omit_na = TRUE, duplicit_values = FALSE, interval_min = NA, interval_max = NA) {

  join <- base::colnames(data %>% dplyr::select({{patient_id}}))

  lab_data %<>%
    dplyr::right_join(data %>% dplyr::select(c({{patient_id}},{{data_date}})), by = join) %>%
    dplyr::mutate(interval_lab := lubridate::time_length(lubridate::interval((lubridate::ymd({{data_date}})), lubridate::ymd({{lab_data_date}})), unit="day"))

  if (omit_na) {
    lab_data %<>% dplyr::filter(!is.na({{lab_data_value}}))
  }

  if (matching == "exact") {
    lab_data %<>%
      dplyr::group_by({{patient_id}}, {{data_date}}) %>%
      dplyr::filter(interval_lab == 0) %>%
      dplyr::ungroup()
  } else if (matching == "interval") {
    lab_data %<>%
      dplyr::group_by({{patient_id}}, {{data_date}}) %>%
      dplyr::filter(interval_lab <= interval_max & interval_lab >= interval_min ) %>%
      dplyr::ungroup()
  }

  if (!duplicit_values) {
    lab_data %<>%
      dplyr::group_by({{patient_id}}, {{data_date}}) %>%
      dplyr::filter(interval_lab == base::min(interval_lab)) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup()
  }

  return(lab_data)

}
