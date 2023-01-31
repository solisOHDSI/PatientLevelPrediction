internalRecalibrationInTheLarge <- function(prediction, columnType = 'evaluationType'){
  
  if(attr(prediction, "metaData")$modelType == 'binary'){
    misCal <- prediction %>% dplyr::filter(evaluationType == "Train") %>% calibrationInLarge()
    
    # obsOdds <- misCal$observedRisk/ (1-misCal$observedRisk)
    outcomeRate <- prediction %>% 
      dplyr::filter(evaluationType == "Test") %>%
      dplyr::summarize(outcomeRate = sum(outcomeCount)/dplyr::n())
    obsOdds <- outcomeRate$outcomeRate / (1- outcomeRate$outcomeRate)
    
    predOdds <- misCal$meanPredictionRisk/ (1 -  misCal$meanPredictionRisk)
    correctionFactor <- log(obsOdds / predOdds)
    
    recalibrated <- prediction %>% dplyr::filter(evaluationType == "Test")
    recalibrated$value = logFunct(inverseLog(recalibrated$value) + correctionFactor)
    
    recalibrated[,columnType] <- 'recalibrationInTheLarge'
    prediction <- rbind(prediction, recalibrated)
    attr(prediction, 'metaData')$recalibrationInTheLarge = list(correctionFactor = correctionFactor)
    
    return(prediction)
  }
  
  if(attr(prediction, "metaData")$modelType == 'survival'){
    
    ParallelLogger::logError('Survival recal in the large not currently available')
  }
  
  
}