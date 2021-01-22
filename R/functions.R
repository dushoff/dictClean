#' @importFrom tibble tibble
#' @importFrom dplyr left_join filter distinct
#' @importFrom dplyr mutate pull select %>%
NULL

#' merge a data vector with a dictionary table
#' @export
#' @param dat data vector
#' @param dict dictionary table
#' @param keys dictionary keys
#' @param values dictionary values
mergeDict <- function(dat, dict=NULL, keys, values){
	if (is.null(dict)) dict = cleanDict(keys, values)
	return(left_join(
		tibble(x=dat), dict
	))
}

#' Make a dictionary tibble from a keys vector and a values vector
#' @function cleanDict
#' @export
#' @param keys dictionary keys
#' @param values dictionary values
cleanDict <- function(keys, values){
	if (
		!is.vector(keys) | !is.vector(values)
		| length(keys) != length(values)
	) stop("Need two equal-length vectors for cleanDict")
	r <- (tibble(x=keys, n=values))
	o <- dplyr::filter(r, !is.na(x) & !is.na(n))
	if (nrow(o) < nrow(r))
		warning("Omitting NAs in cleanDict")
	d <- distinct(o)
	if (nrow(d) < nrow(o))
		warning("Omitting repeated entries in cleanDict")
	c <- d %>% select(x) %>% distinct()
	if (nrow(c) < nrow(d))
		stop("Ambiguous entries found in cleanDict")
	return(d)
}

#' use mergeDict to patch errors identified in a table
#' @export
#' @param dat data vector
#' @param dict dictionary table
#' @param keys dictionary keys
#' @param values dictionary values
patchDict <- function(dat, dict=NULL, keys, values){
	return(mergeDict(dat, dict, keys, values)
		%>% mutate(n=ifelse(is.na(n), x, n))
		%>% pull("n")
	)
}

#' use mergeDict to apply a lookup table
#' @export
#' @param dat data vector
#' @param dict dictionary table
#' @param keys dictionary keys
#' @param values dictionary values
catDict <- function(dat, dict=NULL, keys, values){
	m <- (mergeDict(dat, keys, values))
	not_found <- dplyr::filter(m, 
		!is.na(x) & is.na(n)
	)
	if (nrow(not_found) > 0){
		cat("Items not found in catDict")
		warning("Items not found in catDict"
			, unique(not_found[["x"]])
		)
	}
	return(pull(m, "n"))
}
