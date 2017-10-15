example.plotquiz = function() {
  setwd("D:/libraries/dataquiz")
  restore.point.options(display.restore.point = TRUE)

  library(tidyr)
  library(readr)
  library(dplyr)
  dat = read_csv("stromprod.csv")
  d = gather(dat,key = "year",value="TWh",-type, convert=TRUE)
  names(d)
  d$GW = d$TWh * 1000 / 8760
  d = select(d,-TWh)
  sum = group_by(d, year) %>%
    summarize(type="Total",GW=sum(GW, na.rm=TRUE))
  d = rbind(d,sum)


  app = dataquizApp(quiz.fun = make.plotquiz, game.fun=pq.solo.game,quiz.fun.args = nlist(keyvar="type",valuevar="GW",dat=d, question="Electricity Production in Germany average GWh per hour. Which time series is shown?"))
  viewApp(app)

  dq = make.plotquiz(d, keyvar="type",valuevar="TWh")
  app=dataquizApp(dq=dq)
  viewApp(app)

  app = eventsApp()
  app$ui = make.pq.ui(dq=dq)
  viewApp(app)

  app = rankquizApp(dq)
  viewApp(app)
}

make.plotquiz = function(dat, keyvar, valuevar,timevar="year", facetvar=NULL, donevar=NULL, question="Which time series is shown?", choice.n = NULL, keys = NULL, facet.scales = "fixed", gentype=gen$gentype, gen=NULL, plot.lib = c("ggplot2","plotly", "highcharter")[1]) {
  restore.point("make.plotquiz")

  if (is.null(keys)) {
    keys = unique(dat[[keyvar]])
  }

  if (is.null(choice.n)) choice.n = length(keys)

  dat = dat[dat[[keyvar]] %in% keys, ]

  make.help = !is.null(dat[["help.link"]])
  if (make.help) {
    help.df = dat %>%
      group_by_(keyvar) %>%
      summarize(help.link = first(help.link))
    help.links = help.df[["help.link"]]
    names(help.links) = help.df[[keyvar]]
    help.links = help.links[keys]

  } else {
    help.links = NULL
  }

  key = sample(keys, 1)

  key.dat = dat[dat[[keyvar]] %in% key,]

  doneval = NULL
  if (!is.null(donevar)) doneval = key.dat[[donevar]][1]

  format.fun = function(x) {
    format(x,big.mark = " ")
  }


  if (plot.lib == "ggplot2" || plot.lib=="plotly") {
    if (length(facetvar)>0) {
      aes = aes_string(x=timevar,y=valuevar, color=facetvar[1])
    } else {
      aes = aes_string(x=timevar,y=valuevar, color=keyvar)

    }
    plot = ggplot(data=key.dat, aes) + geom_line(size=1.2) + guides(color=FALSE) + scale_y_continuous(labels=format.fun) + theme_bw() +  theme(plot.background = element_rect(fill='#ddddff'))

    if (!is.null(facetvar)) {
      plot = plot + facet_wrap(facetvar, scales = first.non.null(facet.scales, "fixed"))
    }

    if (plot.lib == "plotly")
      plot = plotly::ggplotly(plot)

  } else if (plot.lib == "highcharter") {
    restore.point("pq.quiz.make.highcharter")

    library(highcharter)
    plot = hchart(key.dat,"line",hcaes(x=year,y=value, color=cntry, group=cntry))
  } else {
    stop(paste0("plot.lib ", plot.lib, " not yet implemented,"))
  }

  #plot

  # draw random choices
  choices = sample(c(key,sample(setdiff(keys,key),size = choice.n-1)))

  help.links = help.links[choices]

  dq = nlist(quiztype="pq",gentype=gentype, dat=dat, plot=plot, choices,question, choice.n, key=key,keys=keys,keyvar, valuevar, timevar, doneval=doneval, help.links=help.links, plot.lib=plot.lib)

  dq$dqhash = digest(dq)
  if (is.null(gen)) {
    dq$genhash = ""
  } else {
    dq$genhash = digest(gen)
  }
  dq$gen = gen
  try(save.dataquiz.app.dq(dq))

  dq
}

make.pq.ui = function(dq=game$dq,game=app$game,finished=first.non.null(game$finished, FALSE), chosen.keys = game$chosen.keys, choice.handler=game$choice.handler, msg=first.non.null(game$msg,""), app=getApp()) {
  restore.point("make.pq.ui")
  dat = dq$dat

  rem.choices = setdiff(dq$choices,chosen.keys)

  buttons = lapply(seq_along(rem.choices), function(i) {
    choice = rem.choices[[i]]
    style="white-space: normal; margin-bottom: 0.5em; background-color: #eeeeff;"
    if (is.null(dq$help.links))
      style = paste0(style," width: 100%;")
    btn = simpleButton(id=paste0("choiceBtn_",i),label = choice, class.add = "quizChoiceBtn",style=style)
    if (is.null(dq$help.links)) return(btn)

    tags$table(tags$tr(style="width: 100%",
      tags$td(
        btn
      ),
      tags$td(
        tags$a(href=dq$help.links[choice], target="_blank", "(?)")
      )
    ))
  })

  classEventHandler("quizChoiceBtn", event="click", fun=function(id,...) {
    restore.point("plotQuizChoiceClick")
    index = as.integer(str.right.of(id,"_"))
    if (!is.null(choice.handler))
      choice.handler(choice = rem.choices[[index]], dq=dq)
  })
  plot.ui = xplotOutput("quizPlot", height="12em", plot.lib = dq$plot.lib)


  if (!finished) {
    ui = tagList(
      p(dq$question),
      plot.ui,
      p(msg),
      buttons,
      if (!is.null(game$quiz.fun)) simpleButton("newGameBtn","New Question",style = "width=100%;")

    )
  } else {
    ui = tagList(
    p(dq$question),
    plot.ui,
    p(msg),
    if (!is.null(game$quiz.fun)) simpleButton("newGameBtn","New Question",style = "width=100%;")
    )
  }
  xsetPlot(id="quizPlot",expr= dq$plot)
  buttonHandler("newGameBtn",function(...) {
    app$game = new.game.instance(game)
    refresh.game.ui(game=app$game)
  })

  ui
}

pq.solo.game = function(dq=NULL, correct.factor= first.non.null(game$correct.factor,5), quiz.fun=NULL, quiz.fun.args=list(...),..., game=NULL) {
  restore.point("pq.solo.game")

  if (is.null(dq)) {
    dq = do.call(quiz.fun, c(list(done=game$done),quiz.fun.args))
  }

  game = as.environment(nlist(game.type="pq.game", quiztype="pq",
    dq, choice.handler = pq.choice.handler, ui.fun = make.pq.ui, solved = FALSE, finished=FALSE, quiz.fun=quiz.fun, quiz.fun.args=quiz.fun.args, done=game$done
  ))
  game
}

pq.choice.handler = function(choice, dq, game=getGame(), ...) {
  restore.point("pq.solo.choice.handler")
  cat("\nChoice pressed...")

  game$chosen.keys = c(game$chosen.keys, choice)
  if (choice == game$dq$key) {
    game$finished=TRUE
    game$solved = TRUE
    game$done = c(game$done, game$dq$doneval)
    game$msg = paste0("Right, that is the time series for ", choice)
  } else {
    #comp.choice = pq.solo.computer.move(game)
    game$msg = paste0("No, that is not the time series for ", choice, ". Try again.")
    game$finished=FALSE
  }
  writeDataQuizLog("pq_answer",c(dq$dqhash, game$solved, paste0('"',choice,'"')))

  ui =  game$ui.fun(game=game)
  setUI("mainUI",ui)
}

pq.solo.computer.move = function(game) {
  restore.point("pq.solo.computer.move")
  rem.choices = setdiff(game$dq$keys, game$chosen.keys)
  probs = rep(1, length(rem.choices))
  probs[game$qg$key] = game$correct.factor
  choice = sample(rem.choices, 1,prob = probs)
  return(choice)
}
