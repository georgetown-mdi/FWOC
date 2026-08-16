# Author: Mark Richardson and Claude Code
# Purpose: Derivations shared between the build and the validation suite, so the two
#          cannot disagree about how a published variable is defined. Pure functions
#          (definitions only), sourced into scripts that have already attached
#          arrow/dplyr -- mirrors utils/validation_functions.R.

# Bin length of service into the BZ los_level brackets that 05 passes through.
# The bins cover every non-negative integer and length_of_service_years is int16,
# so only a missing LOS falls through -- hence no default branch.
# Takes and returns the DATASET, not the vector: nesting case_when one level inside a
# user function sends arrow's own arrow_eval() down a deprecated rlang path.
add_los_ord <- function(ds) {
  ds |>
    mutate(length_of_service_ord = case_when(length_of_service_years <  1                                  ~ "< 1",
                                             length_of_service_years >= 1  & length_of_service_years <= 2  ~ "1-2",
                                             length_of_service_years >= 3  & length_of_service_years <= 4  ~ "3-4",
                                             length_of_service_years >= 5  & length_of_service_years <= 9  ~ "5-9",
                                             length_of_service_years >= 10 & length_of_service_years <= 14 ~ "10-14",
                                             length_of_service_years >= 15 & length_of_service_years <= 19 ~ "15-19",
                                             length_of_service_years >= 20 & length_of_service_years <= 24 ~ "20-24",
                                             length_of_service_years >= 25 & length_of_service_years <= 29 ~ "25-29",
                                             length_of_service_years >= 30 & length_of_service_years <= 34 ~ "30-34",
                                             length_of_service_years >= 35                                 ~ "35+"))
}
