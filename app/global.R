#setwd("D:/libraries/EconCurves/EconCurves/app")

library(EconCurves)
initEconModels()
EM = getEM()
load.collection("makro.yaml")

app = shinyStoriesApp(EM = EM)
app$verbose = FALSE
app$is.running = TRUE

#shinyApp(ui = app$ui, server = app$server)
#runEventsApp(app,launch.browser = rstudio::viewer)
