# Global variable references for R CMD check (column names used in NSE / tidy eval)
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "in_observation",
    "subject_id",
    "pregnancy_start_date",
    "pregnancy_end_date",
    "pregnancy_id",
    "all_ids",
    "kept_pregnancy_id",
    "removed_ids",
    "removed_pregnancy_id",
    "age"
  ))
}
