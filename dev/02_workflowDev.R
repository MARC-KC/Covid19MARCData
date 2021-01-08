



dfList = createBiDatasets_Hub(baseDataList = getBaseCovidData(), lagDays = 10, lagDaysHosp = 2)
outputFolder = '//KCJazz/GIS/DataDevelopment/HumanServices/COVID-19/Outputs/PipelineDataOutputs/PublishData'
list2CSV(dfList, outputFolder)


