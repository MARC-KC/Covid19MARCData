
outputFolder = '//KCJazz/GIS/DataDevelopment/HumanServices/COVID-19/Outputs/PipelineDataOutputs/PublishData'
baseData <- getBaseCovidData()

dfListHub <- createBiDatasets_Hub(baseDataList = baseData, lagDaysCDT = 10, lagDaysHosp = 2)
list2CSV(dfListHub, outputFolder)


dfListWDS <- createBiDatasets_WDS(baseDataList = baseData, cutoffDay = 'Tuesday', lagDaysCDT = 10, lagDaysHosp = 2)


list2CSV(c(dfListHub, dfListWDS), outputFolder)

