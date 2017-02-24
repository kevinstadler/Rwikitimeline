#' Find and parse a year from a string.
#'
#' Returns the first numeric year specification within the string that follows
#' the given 'fieldname' substring.
#'
#' @param fieldname a substring more or less directly preceding the date specification
#' @param string the document string to be searched
#' @return the parsed year, as a numeric (negative for years BC)
#' @examples
#' parseyear("death_date", "| birth_place   = [['s-Hertogenbosch]], [[Duchy of Brabant]]
#' | death_date    = Buried on {{death date|1516|8|9|df=yes}}")
#' # works for BC too
#' parseyear("birth_date", "|birth_date  = c. 535 BC")
#' @export
parseyear <- function(fieldname, string) {
  m <- regexpr(paste(fieldname, "[^\\d]+(\\d+)", sep=""), string, perl=TRUE)
  start <- attr(m, "capture.start")
  end <- start + attr(m, "capture.length")
  year <- as.numeric(substr(string, start, end - 1))
  if (length(grep("BC", substr(string, end, end + 5), fixed=TRUE)))
    -year
  else
    year
}

#' Retrieve birth and death year of a person from Wikipedia.
#'
#' Retrieves the Wikipedia article with the given name and attempts to extract
#' birth and death data from its \emph{infobox} template.
#'
#' @param name the name of the Wikipedia article
#' @param wiki domain of the wiki to retrieve the article from
#' @examples
#' getwikibio("Hieronymus Bosch")
#' @export
getwikibio <- function (name, wiki="en.wikipedia.org") {
  data <- WikipediR::page_content(domain=wiki, page_name=gsub(" ", "_", name), as_wikitext=TRUE, clean_response=FALSE)
  # https://en.wikipedia.org/wiki/Template:Infobox_person#Usage
  c(birth=parseyear("birth_date", data), death=parseyear("death_date", data))
}

# avoid re-retrieval in interactive sessions
if (requireNamespace("memoise", quietly = TRUE))
  getwikibio <- memoise::memoise(getwikibio)


#' Arrange the given time periods into groups of non-overlapping time spans.
#'
#' @param starts vector of all start dates of the time periods
#' @param ends end times of the time periods (same length as the \code{starts} argument)
#' @param groups vector of pre-assigned groups for some of the time periods (optional)
#' @return a character vector of groups, with one item for every time period
#' @export
#' @importFrom stats aggregate na.omit na.pass
grouptimeperiods <- function(starts, ends, groups=NULL) {
  if (length(starts) != length(ends))
    stop("Arguments must have same number of elements")
  # put in ascending order
  ends <- ends[order(starts)]
  starts <- starts[order(starts)]

  if (is.null(groups) || all(is.na(groups))) {
    groupends <- -Inf
    groups <- rep(NA, length(starts))
  } else {
    if (length(groups) != length(starts))
      stop("Arguments must have same number of elements")
    groupends <- aggregate(ends, by=as.list(na.omit(unique(groups))), FUN=max, na.action=na.pass)
  }
  for (i in 1:length(groups)) {
    if (is.na(groups[i])) {
      group <- which(groupends < starts[i])[1]
      groups[i] <- ifelse(is.na(group), length(groupends) + 1, group)
      groupends[groups[i]] <- ends[i]
    }
  }
  groups
}

#' Retrieve and arrange biographical data of several people.
#' 
#' @param names a vector or list of names for which biographical dates should be retrieved
#' @param groups passed on to \code{\link{grouptimeperiods}}
#' @return a data frame with one row per name and four columns (name, group, birth, death)
#' @examples
#' buildtimeline(c("Hieronymus Bosch", "Michelangelo", "Pieter Bruegel the Elder"))
#' @export
buildtimeline <- function(names, groups=NA) {
  bios <- data.frame(name=names, group=groups)
  bios <- cbind(bios, t(sapply(names, getwikibio)))
  # fix groups so there's no overlap
  bios$group <- grouptimeperiods(bios$birth, bios$death, bios$group)
  bios
}

#' Display a timeline across several panels.
#'
#' @param bios a data frame passed on to \code{\link{timeline}}
#' @param title optional title to add to plot
#' @param npanels number of panels to spread the timeline between
#' @examples
#' plottimeline(buildtimeline(c("Hieronymus Bosch", "Michelangelo", "Pieter Bruegel the Elder")))
#' @seealso \code{\link{buildtimeline}}
#' @seealso \code{\link{timeline}}
#' @export
#' @importFrom ggplot2 ggplot
plottimeline <- function(bios, title="People I know about", npanels=1) {
  start <- min(bios$birth)
  spaneach <- ( max(bios$death) - start ) / npanels
  for (i in 1:npanels) {
    suppressWarnings(print(timeline::timeline(bios,
      limits=c(start+(i-1)*spaneach, start+i*spaneach)) + ggplot2::ggtitle(title)))
  }
}
