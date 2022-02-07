#' simple utility to collapse a categorical vector
#' @param x a vector
#' @param keep numeric(1)  elements occuring less than 'keep' times are relabeled `map_rare_to`
#' @param map_rare_to character(1) elements occuring less than 'keep' times are relabeled `map_rare_to`, 
#' defaults to "other"
#' @export
combine_rare = function(x, keep=40, map_rare_to="other") {
 sx = x
 tx = table(x)
 stx = tail(sort(tx),n=keep)
 x[] = map_rare_to
 x[which(sx %in% names(stx))] = sx[which(sx %in% names(stx))]
 x
}

#' produce collapsible tree with overview of AnnotationHub species and tags
#' @param ahub AnnotationHub instance
#' @param min_date string coercible with as.Date, defaults to "2018-01-01"
#' @param n_common numeric(1) maximum number of elements to expand a leaf to, used as `keep` in `combine_rare`
#' @param map_rare_to character(1) passed to `combine_rare`
#' @examples
#' if (interactive()) make_ah_tree(AnnotationHub::AnnotationHub())
#' @export
make_ah_tree = function(ahub, min_date = "2018-01-01", n_common=30, map_rare_to="other") {
  stopifnot(inherits(ahub, "AnnotationHub"))
  mc = as.data.frame(mcols(ahub))
  dates = as.Date(mc$rdatadateadded)
  if (!is.null(min_date)) {
     earliest = try(as.Date(min_date))
     if (inherits(earliest, "try-error")) stop("can't parse min_date with as.Date")
     mc = mc[which(dates>=earliest),]
     }
  mc$Species = combine_rare(mc$species, keep=n_common, map_rare_to=map_rare_to)
  mc$Tag1 = sapply(mc$tags, function(x) strsplit(x, ",")[[1]][1])
  suppressWarnings({
  isn = which(sapply(mc$Tag1, function(x) !is.na(as.integer(x))))
  })
  if (length(isn)>0) 
    mc$Tag1[isn] = sapply(mc$tags[isn], function(x) strsplit(x, ",")[[1]][2])
  AnnHub = mc  # just rename
  collapsibleTree(AnnHub, c("Species", "Tag1"))
}
