# Create analysis plans
createAnalysesDetails <- function(workFolder){
  makeCovariateIdsToInclude <- function(includeIndexYear = FALSE) {
    ageGroupIds <- unique(
      floor(c(18:110) / 5) * 1000 + 3
    )

    # Index month
    monthIds <-c(1:12) * 1000 + 7

    # Gender
    genderIds <- c(8507, 8532) * 1000 + 1

    # Index year
    if (includeIndexYear) {
      yearIds <- c(2016:2019) * 1000 + 6
    } else {
      yearIds <- c()
    }
    
    return(c(ageGroupIds,monthIds,yearIds,genderIds))
  }

  firstExposureOnly <- FALSE # TODO Reconfirm
  studyStartDate <- "" # "20200101" # TODO Reconfirm
  fixedPsVariance <- 1 # TODO confirm
  fixedOutcomeVariance <- 4
  riskWindowEnd <- 30

  covarSettingsWithHtnMeds <- FeatureExtraction::createDefaultCovariateSettings()
  covarSettingsWithHtnMeds$mediumTermStartDays <- -90
  covarSettingsWithHtnMeds$longTermStartDays <- -180
  covarSettingsWithHtnMeds$longTermStartDays <- -180
  covarSettingsWithHtnMeds$endDays  <- -1
  covarSettingsWithHtnMeds$DemographicsIndexMonth

  covarSettingsWithoutHtnMeds <- FeatureExtraction::createDefaultCovariateSettings(
    excludedCovariateConceptIds = htnIngredientConceptIds,
    addDescendantsToExclude = TRUE
  )
  covarSettingsWithoutHtnMeds$mediumTermStartDays <- -90
  covarSettingsWithoutHtnMeds$longTermStartDays <- -180
  covarSettingsWithoutHtnMeds$endDays  <- -1

  getDbCmDataArgsWithHtnMeds <- CohortMethod::createGetDbCohortMethodDataArgs(
    firstExposureOnly = firstExposureOnly,
    studyStartDate = studyStartDate,
    removeDuplicateSubjects = "remove all",
    excludeDrugsFromCovariates = FALSE,
    covariateSettings = covarSettingsWithHtnMeds
  )

  getDbCmDataArgsWithoutHtnMeds <- CohortMethod::createGetDbCohortMethodDataArgs(
    firstExposureOnly = firstExposureOnly,
    studyStartDate = studyStartDate,
    removeDuplicateSubjects = "remove all",
    excludeDrugsFromCovariates = FALSE,
    covariateSettings = covarSettingsWithoutHtnMeds
  )

  createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
    removeSubjectsWithPriorOutcome = TRUE,
    minDaysAtRisk = 0,
    riskWindowStart = 0,
    startAnchor = "cohort start",
    riskWindowEnd = riskWindowEnd,
    endAnchor = "cohort start")

  createMinPsArgs <- CohortMethod::createCreatePsArgs(
    stopOnError = FALSE,
    includeCovariateIds = makeCovariateIdsToInclude(),
    prior = Cyclops::createPrior(priorType = "normal",
                                 variance = fixedPsVariance,
                                 useCrossValidation = FALSE))

  createLargeScalePsArgs <- CohortMethod::createCreatePsArgs(
    stopOnError = FALSE,
    prior = Cyclops::createPrior(priorType = "laplace",
                                 useCrossValidation = TRUE))

  createLargeScalePsArgsNoCv <- CohortMethod::createCreatePsArgs(
    stopOnError = FALSE,
    prior = Cyclops::createPrior(priorType = "laplace",
                                 variance = fixedPsVariance,
                                 useCrossValidation = FALSE))

  fitUnadjustedOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
    modelType = "logistic",
    useCovariates = FALSE,
    stratified = FALSE)

  fitAdjustedOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
    modelType = "logistic",
    useCovariates = TRUE,
    includeCovariateIds = makeCovariateIdsToInclude(),
    stratified = FALSE,
    prior = Cyclops::createPrior(priorType = "normal",
                                 variance = fixedOutcomeVariance,
                                 useCrossValidation = FALSE))

  fitPsOutcomeModelArgsConditioned<- CohortMethod::createFitOutcomeModelArgs(
    modelType = "logistic",
    useCovariates = FALSE,
    stratified = TRUE)
  fitPsOutcomeModelArgsUnConditioned<- CohortMethod::createFitOutcomeModelArgs(
    modelType = "logistic",
    useCovariates = FALSE,
    stratified = FALSE)

  stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(numberOfStrata = 5)

  matchByPsArgs <- CohortMethod::createMatchOnPsArgs(
    maxRatio = 1 # TODO Allow for multiple matches
  )

  matchByPsArgsVariable <- CohortMethod::createMatchOnPsArgs(
    maxRatio = 100 # TODO Allow for multiple matches
  )

  # Analysis 1 -- crude/adjusted

  cmAnalysis1 <- CohortMethod::createCmAnalysis(analysisId = 1,
                                                description = "Crude/unadjusted",
                                                getDbCohortMethodDataArgs = getDbCmDataArgsWithHtnMeds,
                                                createStudyPopArgs = createStudyPopArgs,
                                                createPs = FALSE,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = fitUnadjustedOutcomeModelArgs)

  # Analysis 2 -- adjusted outcome

  cmAnalysis2 <- CohortMethod::createCmAnalysis(analysisId = 2,
                                                description = "Adjusted outcome",
                                                getDbCohortMethodDataArgs = getDbCmDataArgsWithHtnMeds,
                                                createStudyPopArgs = createStudyPopArgs,
                                                createPs = FALSE,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = fitAdjustedOutcomeModelArgs)

  # Analysis 3 -- minimal PS stratification

  cmAnalysis3 <- CohortMethod::createCmAnalysis(analysisId = 3,
                                                description = "Min PS stratified",
                                                getDbCohortMethodDataArgs = getDbCmDataArgsWithoutHtnMeds,
                                                createStudyPopArgs = createStudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createMinPsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratifyByPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = fitPsOutcomeModelArgsConditioned)

  # Analysis 4 -- minimal 1:1 PS matching

  cmAnalysis4 <- CohortMethod::createCmAnalysis(analysisId = 4,
                                                description = "Min 1:1 PS matched",
                                                getDbCohortMethodDataArgs = getDbCmDataArgsWithoutHtnMeds,
                                                createStudyPopArgs = createStudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createMinPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = matchByPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = fitPsOutcomeModelArgsUnConditioned)

  # Analysis 5 -- minimal Variable-ratio PS matching

  cmAnalysis5 <- CohortMethod::createCmAnalysis(analysisId = 5,
                                                description = "Min Variable-ratio PS matched",
                                                getDbCohortMethodDataArgs = getDbCmDataArgsWithoutHtnMeds,
                                                createStudyPopArgs = createStudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createMinPsArgs,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = matchByPsArgsVariable,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = fitPsOutcomeModelArgsConditioned)

  # Analysis 7 -- Large-scale PS stratification

  cmAnalysis7 <- CohortMethod::createCmAnalysis(analysisId = 7,
                                                description = "Full PS stratified",
                                                getDbCohortMethodDataArgs = getDbCmDataArgsWithoutHtnMeds,
                                                createStudyPopArgs = createStudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createLargeScalePsArgs,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratifyByPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = fitPsOutcomeModelArgsConditioned)

  # Analysis 8 -- Large-scale PS stratification, no cross-validation

  cmAnalysis8 <- CohortMethod::createCmAnalysis(analysisId = 8,
                                                description = "Full PS stratified, no CV",
                                                getDbCohortMethodDataArgs = getDbCmDataArgsWithoutHtnMeds,
                                                createStudyPopArgs = createStudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createLargeScalePsArgsNoCv,
                                                stratifyByPs = TRUE,
                                                stratifyByPsArgs = stratifyByPsArgs,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = fitPsOutcomeModelArgsConditioned)

  cmAnalysisList <- list(cmAnalysis1,cmAnalysis2,cmAnalysis3,cmAnalysis4, cmAnalysis5)#,cmAnalysis7,cmAnalysis8)

  CohortMethod::saveCmAnalysisList(cmAnalysisList, file.path(workFolder, "cmAnalysisList.json"))
}
