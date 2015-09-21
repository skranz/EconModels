..EM = new.env()

initEconModels = function(
    models.path = paste0(getwd(),"/models"),
    types.path = paste0(path.package("EconModels", quiet = TRUE),"/yamltypes"),
    allow.edit=TRUE
) {
  restore.point("initEconModels")
  options(stringsAsFactors = FALSE)
  EM = ..EM
  EM$types.path = types.path
  EM$models.path = models.path
  EM$types = load.yaml.types(types.path=types.path)
  EM$allow.edit = allow.edit
  EM
}

getEM = function() {
  app = try(getApp(),silent=TRUE)
  if (!is(app,"try-error")) {
    if (!is.null(app[["..EM"]]))
      return(app[["..EM"]])
  }
  ..EM
}



# check the package for bugs
check.econ.curves = function() {
  txt = NULL
  codetools::checkUsagePackage("EconCurves",
    report = function(str)  txt <<- c(txt,str),
    suppressLocalUnused = TRUE
  )
  txt
  rows = str.starts.with(txt,"examples.")
  rows = rows | has.substr(txt,"no visible global function definition")
  txt = txt[!rows]
  cat(paste0(1:NROW(txt),": ", txt, collapse="\n"))
  invisible(txt)
}

examples.load.model = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  initEconModels()
  res = load.model("ThreeEq")
  tt = res$tt
  em = res$em
}

load.model = function(modelId, file=paste0(modelId,".yaml"), dir=getEM()$models.path, EM = getEM()) {
  restore.point("load.model")

  tt = load.struct(name="model",file = paste0(dir,"/",file),typeName = "model", types=EM$types)
  list(em = as.environment(tt.object(tt,1)), tt=tt)

}
