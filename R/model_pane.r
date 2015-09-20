
init.model.panes = function(em) {
  restore.point("init.model.panes")

  em$panes = lapply(em$panes, function(pane) {
    restore.point("inner.init.model.pae")

    init.model.pane(em=em, pane=pane)
   })
  invisible(em$panes)
}



init.model.pane = function(em, pane,...) {
  restore.point("init.model.pane")

  if (!is.null(pane$curve_names)) {
    curve_names = setdiff(pane$curve_names, names(pane$curves))
    pane$curves = c(pane$curves, em$curves[pane$curve_names])
  }
  init.pane(pane,...)
}


# plot.pane
plot.model.pane = function(em,pane.ind=1,pane=em$panes[[pane.ind]], geom.names=names(pane$geom), params = as.list(sim[sim.row,]), sim=em$sim, sim.row = 1, scen=em$scen,...) {
  restore.point("plot.pane")

  #pane = init.model.pane(em,pane)

  axis = scen$axis
  pane$xrange = as.numeric(axis[[pane$xvar]])
  pane$yrange = as.numeric(axis[[pane$yvar]])

  pane$geoms = compute.pane.geoms(pane, params=params)

  plot.pane(pane = pane,...)

  invisible(pane)
}




# compute.pane.lines
compute.model.pane.lines = function(em, pane, t, sim=em$sim, val=as.list(sim[t,,drop=FALSE]), level=1, symbols=".all", remove.na.lines = TRUE) {
  restore.point("compute.pane.lines")
  axis = em$scen$axis
  xrange = as.numeric(axis[[pane$xvar]])
  yrange = as.numeric(axis[[pane$yvar]])

  if (!identical(symbols,".all")) {
    marker.names = intersect(names(pane$markers), symbols)
    curve.names = intersect(pane$curves,symbols)
  } else {
    marker.names = names(pane$markers)
    curve.names = pane$curves
  }
  pane.name = pane$name

  # marker
  ma.li = lapply(pane$markers[marker.names], function(marker) {
    compute.marker.line(marker=marker, val = val,xrange = xrange,yrange=yrange, t=t, pane.name=pane.name,level=level)
  })

  cu = em$curves[[1]]
  cu.li = lapply(em$curves[curve.names], function(cu) {
    curve.to.line(cu=cu, xrange = xrange,yrange=yrange,val = val,level=level,t=t, pane.name=pane.name)
  })

  li = c(ma.li, cu.li)
  names(li) = c(marker.names,curve.names)

  if (remove.na.lines) {
    is.na.line = sapply(li, function(line) all(is.na(line$x)) | all(is.na(line$y)))
    if (any(is.na.line)) {
      names = lapply(li[is.na.line], function(line) line$name)
      cat("\nLines ", paste0(names, collapse=", ")," can not yet be computed.\n")
      li = li[!is.na.line]
    }
  }
  #bases = sapply(li, function(line) line$base)
  #names(li) = bases
  li
}





# plot.lines
plot.model.lines = function(em, lines, pane.names=names(em$panes),...) {
  restore.point("plot.lines")
  line.panes = sapply(lines, function(line) line$pane)
  for (pane in em$panes[pane.names]) {
    plines = lines[line.panes %in% pane$name]
    plot.pane(em=em,pane=pane,lines=plines,...)
  }
}
