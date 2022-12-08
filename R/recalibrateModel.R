#'  recalibrateModel - Re-calibrate model probabilities.
#' 
#' @description
#' Recalibrate model
#' 
#' @details 
#' This function takes as input a plpResult as generated from PatientLevelPrediction::loadPlpResult() and returns again a model
#' with re-calibrated probabilities.
#' 
#' @export
recalibrateModel <- function(
    plpResult,
    recalibrationMethod,
    logSettings = createLogSettings(
      verbosity = "DEBUG",
      timeStamp = T,
      logName = 'plpLogRecalibration'),
    saveDirectory){
  
  # Global variables
  analysisId = plpResult$analysisRef$analysisId
  analysisName = plpResult$analysisRef$analysisName
  ExecutionDateTime <- Sys.time()
  
  # start log
  analysisPath <- file.path(saveDirectory, analysisId)
  logSettings$saveDirectory <- analysisPath
  logSettings$logFileName <- 'plpLogRecalibration'
  logger <- do.call(createLog,logSettings)
  ParallelLogger::registerLogger(logger)
  on.exit(closeLog(logger))
  # if(!is.null(predictionTest)){
  #   prediction <- rbind(predictionTest, prediction[, colnames(prediction)!='index'])
  # }
  prediction <- plpResult$prediction
  recalibrationPrediction <- PatientLevelPrediction::recalibratePlp(prediction = prediction,
                                                                    analysisId = analysisId,
                                                                    typeColumn = "evaluationType",
                                                                    method = recalibrationMethod)
  # evaluate model
  performanceRecalibration <- tryCatch(
    {
      PatientLevelPrediction::evaluatePlp(recalibrationPrediction, typeColumn = 'evaluationType')
    },
    error = function(e) { ParallelLogger::logError(e); return(NULL)}
  )
  # Removing the following as not necessary
  # # covariateSummary
  # covariateSummaryResult <- NULL
  # if(executeSettings$runCovariateSummary){
  #   
  #   if(!is.null(data$Test)){
  #     strata <- data.frame(
  #       rowId = c(
  #         data$Train$labels$rowId, 
  #         data$Test$labels$rowId 
  #       ),
  #       strataName = c(
  #         rep('Train', nrow(data$Train$labels)), 
  #         rep('Test', nrow(data$Test$labels))
  #       )
  #     )
  #   } else{
  #     strata <- data.frame(
  #       rowId = c( data$Train$labels$rowId ),
  #       strataName = c( rep('Train', nrow(data$Train$labels)) )
  #     )
  #   }
  #   
  #   variableImportance <- plpData$covariateData$covariateRef %>% 
  #     dplyr::mutate(covariateValue = 0) %>% 
  #     dplyr::select(.data$covariateId, .data$covariateValue) %>% 
  #     dplyr::collect()
  #   if(!is.null(model)){
  #     if(!is.null(model$covariateImportance)){
  #       variableImportance <- model$covariateImportance %>% dplyr::select(.data$covariateId, .data$covariateValue)
  #     }
  #   }
  #   
  #   covariateSummaryResult <- do.call(covariateSummary,   
  #                                     list(
  #                                       covariateData = plpData$covariateData,
  #                                       cohort = population %>% dplyr::select(.data$rowId),
  #                                       labels = population %>% dplyr::select(.data$rowId, .data$outcomeCount), 
  #                                       strata = strata,
  #                                       variableImportance = variableImportance,
  #                                       featureEngineering = NULL
  #                                     )
  #   )
  #   
  # }
  
  #  ExecutionSummary details:
  # log the end time:
  endTime <- Sys.time()
  TotalExecutionElapsedTime <- difftime(endTime, ExecutionDateTime, units='mins')
  
  executionSummary <- list(
    PackageVersion = list(
      rVersion= R.Version()$version.string,
      packageVersion = utils::packageVersion("PatientLevelPrediction")
    ),
    PlatformDetails= list(
      platform = R.Version()$platform,
      cores = Sys.getenv('NUMBER_OF_PROCESSORS'),
      RAM = memuse::Sys.meminfo()[1]
    ),
    TotalExecutionElapsedTime = TotalExecutionElapsedTime,
    ExecutionDateTime = ExecutionDateTime,
    Log = logSettings$logFileName # location for now
    #Not available at the moment: CDM_SOURCE -  meta-data containing CDM version, release date, vocabulary version
  )
  
  # if model is NULL convert it to list for saving 
  if(is.null(plpResult$model)){
    model <- list(noModel = T)
    attr(model, "predictionFunction") <- 'noModel'
    attr(model, "saveType") <- 'RtoJson'
    class(model) <- 'plpModel'
  }
  
  results <- list(
    #inputSetting = inputSetting, 
    executionSummary = plpResult$executionSummary, 
    model = plpResult$model,
    prediction = recalibrationPrediction,
    performanceEvaluation = performanceRecalibration,
    covariateSummary = plpResult$covariateSummary,
    analysisRef = list(
      analysisId = analysisId,
      analysisName = analysisName
    )
  )
  class(results) <- c('runPlp')
  
  ParallelLogger::logInfo("Run finished successfully.")
  
  # save the results
  ParallelLogger::logInfo(paste0('Saving PlpResult'))
  tryCatch(savePlpResult(results, file.path(analysisPath,'plpResult')),
           finally= ParallelLogger::logTrace('Done.'))
  ParallelLogger::logInfo(paste0('plpResult saved to ..\\', analysisPath ,'\\plpResult'))
  
  return(results)
  
}
