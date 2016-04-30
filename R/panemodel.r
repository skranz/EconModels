example.pane.model = function() {
yaml = "
IS:
  xy: [I_,r_]
  curves:
    invest:
      label: I{{id}}(r)
      eq: I_ == A - b * r_
      color: red
    saving:
      label: S{{id}}
      eq: I_ == S
      color: blue
  vars:
    I_n:
      xcut: [invest, saving]
    r_n:
      ycut: [invest, saving]
    I_cb:
      xcurve: invest
      y: r_CB
  ymarkers: [r_n, r_cb]
  xmarkers: [I_n, I_cb]
  xrange: [0,100]
  yrange: [0,100]
"
  pane = init.yaml.pane(yaml=yaml)
  em = make.pane.model(pane)
  cdf = em$cdf
  em$sim.fun
}

make.pane.model = function(pane) {
  restore.point("make.pane.model")
  if (is.null(pane$vars)) {
    pane$has.model = FALSE
    pane$em = NULL
    return(invisible(NULL))
  }
  em = as.environment(as.list(pane)[c("vars","curves")])
  init.static.model(em, init.curves=FALSE, init.panes=FALSE)
  pane$em = em
  pane$has.model = TRUE
  invisible(em)
}

init.static.model = function(em, init.curves=TRUE, init.panes=TRUE) {
  restore.point("init.static.model")

  em = as.environment(em)

  init.model.params(em)
  init.model.funs(em)
  if (init.curves)
    init.model.curves(em)

  if (init.panes)
    init.model.panes(em)
  init.model.vars(em)

  em$is.static.model = TRUE
  em$initialized = TRUE

  cdf = em$cdf
  make.static.sim.funs(em=em)
  invisible(em)

}

make.static.sim.funs = function(em,cdf=em$cdf, all.implicit=FALSE) {
  restore.point("create.static.sim.fun")

  var.names = em$var.names

  inner = make.inner.compute.code(cdf=cdf, em=em, all.implicit=all.implicit)

  # rename arguments
  value.inner = substitute.call(inner, list(var.mat=as.name("values"), par.mat=as.name("values"),ti=1))

  fun.body = substitute(
    {
      restore.point("static.value.sim.fun")
      if (is.list(values) & !is.data.frame(values))
        values = as.data.frame(values)
      inner
      values
    },
    list(inner=inner)
  )
  fun = function(values) {}
  body(fun) <- fun.body
  em$value.sim.fun = fun



  if (all(em$cdt$solved)) {
    # vectorized
    fun.body = substitute(
      {
        restore.point("static.matrix.sim.fun")

        if (!is.matrix(par.mat))
          par.mat = as.matrix(par.mat[,par.names])
        var.mat = matrix(NA_real_, NROW(par.mat),nvar)
        colnames(var.mat) = var.names
        if (NROW(par.mat)==0) return(var.mat)

        ti = 1:NROW(par.mat)
        inner

        var.mat
      },
      list(inner=inner, var.names = var.names,nvar=length(var.names), par.names=em$par.names)
    )
  } else {
    fun.body = substitute(
      {
        restore.point("static.matrix.sim.fun")

        if (!is.matrix(par.mat))
          par.mat = as.matrix(par.mat[,par.names])
        var.mat = matrix(NA_real_, NROW(par.mat),nvar)
        colnames(var.mat) = var.names
        if (NROW(par.mat)==0) return(var.mat)

        for (ti in 1:NROW(par.mat)) {
          inner
        }

        var.mat
      },
      list(inner=inner, var.names = var.names,nvar=length(var.names), par.names=em$par.names)
    )

  }
  fun = function(par.mat) {}
  body(fun) <- fun.body
  em$sim.fun = fun
  return(NULL)
}


