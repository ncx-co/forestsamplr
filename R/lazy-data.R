#' @title a cluster sample data object
#' @description A simple data example of a cluster sample, drawn from
#' Avery and Burkhart Forest Measurements text.
#' @docType data
#' @format an object of class dataframe
#' \itemize{
#'   \item{clusterID}{cluster}
#'   \item{volume}{sampled volume - attribute of interest}
#'   \item{isUsed}{TRUE/FALSE}
#' }
#' @name dataPlot
#' @usage dataPlot
NULL

#' @title a cluster sample data object
#' @description A more complex data example of a cluster sample
#' @docType data
#' @format an object of class dataframe
#' \itemize{
#'   \item{clusterID}{cluster}
#'   \item{bapa}{basal area per acre - attribute of interest}
#'   \item{isUsed}{TRUE/FALSE}
#' }
#' @name clusterBaData
#' @usage clusterBaData
NULL

#' @title a two-stage cluster sample data object
#' @description Dataset from Avery and Burkhart Forest Measurements text, Fifth edition
#' @docType data
#' @format an object of class dataframe
#' \itemize{
#'   \item{clusterID}{cluster}
#'   \item{volume}{plot level volume - attribute of interest}
#'   \item{isUsed}{TRUE/FALSE}
#' }
#' @name redData
#' @usage redData
NULL

#' @title a simple random sample data object
#' @description A simple random sample object, representing typical forest cruise data
#' @docType data
#' @format an object of class dataframe
#' \itemize{
#'   \item{plot}{plot ID}
#'   \item{tree}{tree ID - unique within plot}
#'   \item{BAF}{basal area factor for plot}
#'   \item{DBH}{tree diameter in inches}
#'   \item{Height}{tree height in feet}
#' }
#' @name simpleRandom
#' @usage simpleRandom
NULL
