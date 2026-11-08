# Return the school name used for a selected data year. The schools table
# holds current names; the rename table records earlier names on the same ID.
school_name_for_year <- function(school_id, year, current_name, school_renames) {
  n <- length(school_id)
  if (length(year) == 1L) year <- rep(year, n)
  if (length(year) != n || length(current_name) != n) {
    stop("School name lookup lengths differ")
  }
  result <- as.character(current_name)
  if (n == 0L || nrow(school_renames) == 0L) return(result)
  index <- match(as.character(school_id), school_renames$school_id)
  earlier <- !is.na(index) & !is.na(year) &
    year < school_renames$first_current_year[index]
  result[earlier] <- school_renames$prior_name[index[earlier]]
  result
}
