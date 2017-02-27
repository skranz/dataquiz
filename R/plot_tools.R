dyplot = function (data, xcol = colnames(data)[1], ycol = setdiff(colnames(data),
    xcol), interval = NULL)
{
    restore.point("dyplot")
    library(dygraphs)
    ts = to.xts(data[, ycol, drop = FALSE], time = data[[xcol]],
        interval = interval)
    dygraph(ts)
}

xplotOutput = function(id,..., plot.lib=c("ggplot2","base","dygraphs")[1]) {
  if (plot.lib=="dygraphs") return(dygraphs::dygraphOutput(id,...))
  if (plot.lib=="highcharter") return(highcharter::highchartOutput(id,...))
  if (plot.lib=="plotly") return(plotly::plotlyOutput(id,...))


  shiny::plotOutput(id,...)
}

xsetPlot = function (id, expr, app = getApp(), update.env = parent.frame(),
    quoted = FALSE, plot.lib=c("ggplot2","base","highcharter", "dygraphs")[1], ...)
{
  if (plot.lib == "dygraphs") {
    render.fun = renderDygraph
  } else if (plot.lib == "highcharter") {
    render.fun = renderHighchart
  } else {
    render.fun = renderPlot
  }
  if (app$verbose)
      cat("\n updatePlot: ", id)
  if (!quoted) {
      expr.object = substitute(expr)
  }
  else {
      expr.object = expr
  }
  app$output[[id]] <- render.fun(env = update.env, quoted = TRUE,
      expr = expr.object)
}
