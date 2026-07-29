# Author: Mark Richardson and Claude Code
# Purpose: Time-varying code->label join helpers. label_codes() joins a code to
#          its label by matching the code and the date range; the opm* helpers
#          detect, clean, and month-expand the duty-station label windows for
#          setup/build_duty_station_labels.R.

# Function to label date-code pairs
#
# `extra` (optional) carries additional mapping columns through to the result.
# It is a named list; each name is the OUTPUT column name and each value is a
# list with `col` (the source column in `mapping`) and, optionally, `type` (its
# arrow type for the output schema; defaults to utf8()). When `extra` is NULL
# the behaviour is identical to the original three-column date/label join.

label_codes <- function(date_code_pairs, mapping, code_var, date_var = "date",
                        lbl_name, extra = NULL) {
  date_code_pairs <- as.data.table(date_code_pairs)
  mapping <- as.data.table(mapping)

  on_clause <- c(
    paste0("code == ", code_var),
    paste0("date_from <= ", date_var),
    paste0("date_to >= ", date_var)
  )

  # date_code_pairs: the observed set of code and date pairs; its code and date
  #   column names are supplied by the code_var / date_var arguments.
  # mapping: the label table -- column names are FIXED as code, label, date_from,
  #   date_to (hardcoded in on_clause and x.label), plus any `extra` columns.

  if (is.null(extra)) {
    result <- mapping[date_code_pairs, # i argument
      .( # j argument: return a code, date, and label
        code = get(paste0("i.", code_var)), # code is the code variable from date_code_pairs (i.)
        date = get(paste0("i.", date_var)), # date is the date variable from date_code_pairs (i.)
        label = x.label # label is the label variable from the mapping (x.)
      ),
      on = on_clause, # on argument (named)
      nomatch = NULL # nomatch argument (named): drop rows that don't match (inner join)
    ]
  } else {
    # Build the j projection dynamically so the extra mapping columns (x side)
    # are carried through under their requested output names.
    j_parts <- c(
      'code = get(paste0("i.", code_var))',
      'date = get(paste0("i.", date_var))',
      "label = x.label"
    )
    # Add variables in extra formatted for assembly and parsing below (x. columns)
    for (out_nm in names(extra)) {
      j_parts <- c(j_parts, sprintf('`%s` = get("x.%s")', out_nm, extra[[out_nm]]$col))
    }
    # Assemble a single list of output columns for the j argument in data.table syntax
    j_call <- str2lang(paste0(".(", paste(j_parts, collapse = ", "), ")"))

    result <- mapping[date_code_pairs, # i argument
      eval(j_call), # j argument
      on = on_clause, # on argument (named)
      nomatch = NULL # nomatch argument (named): drop rows that don't match (inner join)
    ]
  }

  # Set variable names
  setnames(result,
    old = c("code", "date", "label"),
    new = c(code_var, date_var, lbl_name)
  )

  # Code-date pairs must be unique or joining the result arrow table with the
  # hive will inflate rows. The label is deliberately NOT part of the key: an
  # overlap produces the same code and date with DIFFERENT labels, so including
  # it would make every overlap look unique. This guard stops the function and
  # prints a message describing the problem if any code-date pair is repeated.
  dup <- duplicated(result, by = c(code_var, date_var))
  if (any(dup)) {
    bad <- unique(result[[code_var]][dup])
    shown <- head(bad, 10)
    stop(sprintf(
      "label_codes(): %s lookup is not unique on %s/%s -- %d duplicate row(s) across %d code(s)%s. Check for overlapping windows in the mapping, or a non-distinct date_code_pairs. Code(s): %s",
      lbl_name, code_var, date_var, sum(dup), length(bad),
      if (length(bad) > length(shown)) sprintf(" (showing %d)", length(shown)) else "",
      paste(shown, collapse = ", ")
    ))
  }

  setorderv(result, c(code_var, date_var))

  # Schema: keys + label are utf8/date32; extras use their declared arrow type
  # (default utf8). Column order is forced to match the schema field order.
  sch <- c(
    stats::setNames(
      list(utf8(), date32(), utf8()),
      c(code_var, date_var, lbl_name)
    ),
    lapply(extra, function(e) if (is.null(e$type)) utf8() else e$type)
  )

  # Order rows per the schema
  setcolorder(result, names(sch))

  # Make result an arrow table for joins with the hives
  result <- as_arrow_table(result, schema = do.call(arrow::schema, sch))

  return(result)
}

#### Functions for labelling duty station ####

# Note: This function identifies types of overlap
# overlap_transition_single: TRUE when a code is mapped to two labels for one month during a transition to a new label
# overlap_transition_multi: TRUE when a code is mapped to two labels for more than one month during a transition to a new label
# subset_eq: TRUE when start year, start month, end year, and end month are identical
# subset_eq_start: TRUE when a code is mapped to two labels and the time period for both labels begins at exactly the same year and month, but one label
#                  expires while the other remains valid
# subset_eq_end: TRUE when a code is mapped to two labels and the time period for both labels expires at exactly the same year and month, but one label
#                is valid earlier than the other label
# subset_within_*: TRUE when valid date range for one label is a subset of the valid date ranges for the other label
# *_t1 & *_t2: indicates if the subset date range is the earlier label or the later label
# trans_match: TRUE if the label (i.e., trans) is different between periods; use this to identify if "new" label is actually different

# Note: This function sorts the codes by data_code then start date, then end date, then translation.

opmOverLap <- function(data) {
  data <- data[order(data_code, year_f, month_f, year_u, month_u, trans)]

  data <- data[, `:=`(
    trans_lead = shift(trans, n = 1, type = "lead"),
    year_f_lead = shift(year_f, n = 1, type = "lead"),
    month_f_lead = shift(month_f, n = 1, type = "lead"),
    year_u_lead = shift(year_u, n = 1, type = "lead"),
    month_u_lead = shift(month_u, n = 1, type = "lead")
  ), by = .(data_code)]

  data <- data[, `:=`(
    overlap_transition_single = fifelse(year_u == year_f_lead & month_u == month_f_lead, TRUE, FALSE),
    overlap_transition_multi = fifelse(((year_u == year_f_lead & month_u > month_f_lead) | (year_u > year_f_lead)) &
      ((year_u == year_u_lead & month_u < month_u_lead) | (year_u < year_u_lead)) &
      (year_f != year_f_lead & month_f != month_f_lead), TRUE, FALSE),
    subset_eq = fifelse(
      year_f == year_f_lead & month_f == month_f_lead & year_u == year_u_lead & month_u == month_u_lead,
      TRUE, FALSE
    ),
    subset_eq_start = fifelse(year_f == year_f_lead & month_f == month_f_lead &
      (year_u < year_u_lead | (year_u == year_u_lead & month_u < month_u_lead)), TRUE, FALSE),
    subset_eq_end = fifelse(((year_f == year_f_lead & month_f < month_f_lead) | (year_f < year_f_lead)) &
      year_u == year_u_lead & month_u == month_u_lead, TRUE, FALSE),
    subset_within_t1 = fifelse(((year_f == year_f_lead & month_f < month_f_lead) | (year_f < year_f_lead)) &
      ((year_u > year_u_lead) | (year_u == year_u_lead & month_u > month_u_lead)), TRUE, FALSE),
    subset_within_t2 = fifelse(((year_f == year_f_lead & month_f > month_f_lead) | year_f > year_f_lead) &
      (year_u < year_u_lead | (year_u == year_u_lead & month_u < month_u_lead)), TRUE, FALSE),
    trans_match = fifelse(trans == trans_lead, TRUE, FALSE)
  )]

  data <- data[, ov := fifelse(overlap_transition_single == TRUE |
    overlap_transition_multi == TRUE |
    subset_eq == TRUE |
    subset_eq_start == TRUE |
    subset_eq_end == TRUE |
    subset_within_t1 == TRUE |
    subset_within_t2 == TRUE, TRUE, FALSE)]


  return(data)
}

# This function takes the result of opmOverLap and corrects the single period overlaps

opmCleanSingle <- function(data_ov) {
  # Cases with a single month overlap during transition not in January - subtract one from the month to resolve the overlap
  data_ov <- data_ov[
    overlap_transition_single == TRUE & month_u != 1,
    `:=`(
      trans_u = trans_u - 1,
      month_u = month_u - 1
    )
  ]

  # Cases of overlap that need to be moved to December of preceding year
  data_ov <- data_ov[
    overlap_transition_single == TRUE & month_u == 1,
    `:=`(
      trans_u = ((year_u - 1) * 100) + 12,
      month_u = 12,
      year_u = year_u - 1
    )
  ]

  return(data_ov)
}

# This function creates data frame for which each row is a month-year-label
# This data frame can then be merged with the status data to label the codes

opmFull <- function(labels) {
  # Note: labels is the the relevant subset of the opm codes stacked

  start <- Sys.time()

  codes_out <- list()

  for (i in seq_len(nrow(labels))) {
    # Create list of years and months to search for the ith data code

    yrs <- labels[i, year_f]:labels[i, year_u] # Vector years

    ym <- list() # List of months with each item in the list containing a
    # vector of months for the jth year

    if (length(yrs) > 1) {
      for (j in seq_len(length(yrs))) {
        if (yrs[j] == min(yrs)) { # Months for the first year

          ym[[j]] <- data.table(year = yrs[j], month = labels[i, month_f]:12)
        } else if (yrs[j] == max(yrs)) {
          ym[[j]] <- data.table(year = yrs[j], month = 1:labels[i, month_u])
        } else {
          ym[[j]] <- data.table(year = yrs[j], month = 1:12)
        }
      }

      ym <- rbindlist(ym, use.names = TRUE)
    } else {
      ym <- data.table(year = yrs[1], month = labels[i, month_f]:labels[i, month_u])
    }

    codes_out[[i]] <- ym[, `:=`(
      data_code = labels[i, data_code],
      label = labels[i, trans]
    )]
  }

  out <- rbindlist(codes_out, use.names = TRUE)

  out <- out[order(data_code, year, month, label)]

  print(Sys.time() - start)

  return(out)
}
