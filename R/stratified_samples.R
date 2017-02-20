#' @title Summarize stratified sample
#' @description Summarizes strata- and population-level statistics for
#' stratified sample data
#' @param trainingData dataframe containing observations of variable of
#' interest, and stratum assignment for each plot
#' @param attribute character name of attribute to be summarized
#' @param stratumTab dataframe containing acreages for each stratum
#' @param desiredConfidence numeric desired confidence level (e.g. 0.9)
#' @param post logical true if post-stratification was used
#' @author Henry Rodman
#' @import dplyr
#' @examples
#' \dontrun{
#' trainingData <- data.frame(bapa = c(120, 140, 160, 110, 100, 90),
#'   stratum = c(1, 1, 1, 2, 2, 2))
#' stratumTab <- data.frame(stratum = c(1, 2), acres = c(200, 50))
#' attribute = 'bapa'
#' desiredConfidence = 0.9
#' }
#' @export

summarize_stratified <- function(trainingData, attribute,
		stratumTab, desiredConfidence = 0.9, post = T) {
	
	# give the variable of interest a generic name
	attrTemp <- unlist(trainingData %>% dplyr::select(one_of(attribute)))
	trainingData$attr <- attrTemp
	
	# summarize strata
	stratumSummaries <- trainingData %>%
			left_join(stratumTab) %>%
			mutate(attrExpanded = attr * acres) %>%
			group_by(stratum) %>%
			summarize(stratMeanTot = mean(attr),
					stratVarTot = var(attrExpanded) / n(),
					stratVarMean = stratVarTot / mean(acres) ^ 2,
					stratSE = sqrt(stratVarMean),
					stratPlots = n())
	
	# summarize population (non-post-stratification)
	if(!post) {
		totalSummary <- stratumSummaries %>%
				left_join(stratumTab) %>%
				summarize(popMean = weighted.mean(stratMeanTot, w = acres),
						popVar = sum(stratVarTot) / (sum(acres) ^ 2),
						popSE = sqrt(popVar),
						popCIhalf = popSE * qt(1 - (1 - desiredConfidence) / 2,
								df = sum(stratPlots - 1))) %>%
				select(popMean, popSE, popCIhalf)
	} else { # summarize (post-stratification, in progress)
#		totalSummary <- stratumSummaries %>%
#				left_join(stratumTab) %>%
#				summarize(popMean = weighted.mean(stratMeanTot, w = acres),
#						popMeanVar = sqrt(sum((acres / sum(acres)) ^ 2 * (1 / stratPlots) * stratSE ^ 2)),
#						popSE = sqrt(popVar),
#						popCIhalf = popSE * qt(1 - (1 - desiredConfidence) / 2,
#								df = sum(stratPlots - 1))) %>%
#				select(popMean, popSE, popCIhalf)
	}
	
	# return list of 
	outList <- list(stratumSummaries = stratumSummaries,
			totalSummary = totalSummary)
	
	return(outList)
	
}