example.rankquiz = function() {
  setwd("D:/libraries/rankquiz")
  restore.point.options(display.restore.point = TRUE)

  library(tidyr)
  library(readr)
  library(dplyr)
  dat = read_csv("stromprod.csv")
  d = gather(dat,key = "year",value="MWh",-type, convert=TRUE)
  max.year = max(d$year)
  d = d %>% filter(year==max.year)

  rq = make.rankquiz(d,key="type",value="MWh", question="Which power plant types produced most electricity in Germany in 2014?")

  app = rankquizApp(rq)
  viewApp(app)

  library(readr)
  dat = read_csv("sbs_de.csv",col_types = cols(Value="n"))
  rq = make.eurostat.sbs.sector.rank.quiz(dat=dat)
  game = rq.solo.game(rq, comp.prob= 0.35)
  app = rankquizApp(game=game)
  viewApp(app)
}

make.rankquiz = function(dat, key, value, question="Find the top ranked items", decreasing=TRUE, top.n=10, choice.n = round(top.n*1.5), ts.dat=NULL) {
  restore.point("make.rankquiz")

  if (is.null(dat$info)) dat$info = ""
  info = if ("info" %in% colnames(dat)) "info" else NULL

  dat = na.omit(dat[,c(key, value,info)])

  ord = order(dat[[value]],decreasing = decreasing)

  # sort data
  dat = dat[ord,]
  dat$rank = 1:NROW(dat)
  dat = dat[,c("rank",key, value,info)]

  top.n = min(top.n, NROW(dat))
  choice.n = min(choice.n, NROW(dat))
  keys = dat[[key]][1:top.n]

  dat$points = pmax(0,top.n - dat$rank +1)

  # draw random choices
  choices = sample(dat[[key]][1:choice.n],size = choice.n)

  if (!is.null(ts.dat)) {
    ts.dat = ts.dat[ts.dat[[key]] %in% keys,]
    ts.dat$label = paste0(format.zero.left(match(ts.dat[[key]],keys),2),".\n",ts.dat[[key]])

    ts.dat$key = ts.dat[[key]]
    ts.dat$value = ts.dat[[value]]

    ts.dat = ts.dat %>%
      group_by_(key) %>%
      mutate(index=100* (value / first(value)))
  }

  nlist(dat, choices,question, top.n, choice.n, key=key,value=value, info="info", keys=keys, ts.dat)

}

make.rq.ui = function(rq=game$rq, solved = first.non.null(game$solved,rep(0,rq$top.n)), chosen.keys=game$chosen.keys, choice.handler = game$choice.handler, points=game$points, prev.choices = game$prev.choices, finished=game$finished, game=app$game, app=getApp()) {
  restore.point("make.rq.ui")
  dat = rq$dat

  info.col = NULL
  if ("info" %in% names(dat)) {
    if (any(nchar(dat$info)>0))
      info.col = "info"
  }
  rows = which(solved>0)
  show.dat = dat[rows,c("rank",rq$key,rq$value,info.col, "points")]
  show.dat[[rq$value]] = format(show.dat[[rq$value]],big.mark=" ")

  #show.dat[[rq$key]][solved == 0] = ""
  #show.dat[[rq$value]][solved == 0] = ""
  #show.dat[[rq$info]][solved == 0] = ""

  bg.color = c("white","#ccccff","#ffcccc")[solved[rows]+1]
  names(bg.color)=names(solved[rows])

  if (!is.null(prev.choices)) {
    try({
      if (prev.choices[1] %in% names(bg.color))
        bg.color[prev.choices[1]] = "#9999ff"
      if (prev.choices[2] %in% names(bg.color))
        bg.color[prev.choices[2]] = "#ff8888"
    })
  }

  tab = html.table(show.dat,bg.color = bg.color)

  rem.choices = setdiff(rq$choices,chosen.keys)
  buttons = lapply(seq_along(rem.choices), function(i) {
    choice = rem.choices[[i]]
    smallButton(id=paste0("choiceBtn_",i),label = choice, class.add = "rankQuizChoiceBtn")
  })

  plot.ui = NULL
  if (finished) {
    buttons=NULL

    if (!is.null(rq$ts.dat)) {
      library(ggplot2)
      library(plotly)

      #plot.abs = ggplot(data=rq$ts.dat,aes_string(x="year",y=rq$value,color="label")) + geom_line(size=1.2) + facet_wrap(~label) + guides(color=FALSE)
      plot.index = ggplot(data=rq$ts.dat,aes_string(x="year",y="index",color="label")) + geom_line(size=1.2) + facet_wrap(~label) + guides(color=FALSE)

    }
  }
  classEventHandler("rankQuizChoiceBtn", event="click", fun=function(id,...) {
    restore.point("rankQuizChoiceClick")

    index = as.integer(str.right.of(id,"_"))
    if (!is.null(choice.handler))
      choice.handler(choice = rem.choices[[index]], rq=rq)
  })

  if (!finished) {
    ui = tagList(
      p(rq$question),
      p("Results:"),
      HTML(tab),
      p(paste0("Your points: ",points[1],", computer: ", points[2])),
      if (!is.null(game$rq.fun)) smallButton("newGameBtn","New Quiz"),
      p("Your choice:"),
      buttons
    )
  } else {
    ui = tagList(
    p(rq$question),
    p("Results:"),
    HTML(tab),
    p(paste0("Your points: ",points[1],", computer: ", points[2])),
    if (!is.null(game$rq.fun)) smallButton("newGameBtn","New Quiz"),
    plotOutput("indexPlot"),
    plotOutput("absPlot")
    )
    try(setPlot(id="indexPlot",expr= plot.index))
    #try(setPlot(id="absPlot",expr= plot.abs))
  }
  buttonHandler("newGameBtn",function(...) {
    app$game = new.game.instance(game)
    refresh.game.ui(game=app$game)
  })

  ui
}

rankquizApp = function(rq=NULL, game=NULL, game.fun = rq.solo.game,rq.fun=NULL,rq.fun.args=list(...),...) {
  restore.point("rankquizApp")
  app = eventsApp()

  if (is.null(game)) {
    game = game.fun(rq=rq,rq.fun=rq.fun,rq.fun.args=rq.fun.args)
  }
  app$game.fun = game.fun
  app$game = game
  app$ui = bootstrapPage(
    uiOutput("mainUI")
  )
  refresh.game.ui(app$game)
  app
}

refresh.game.ui = function(game=app$game, app=getApp()) {
  ui =  game$ui.fun(game=game)
  setUI("mainUI",ui)

}

new.game.instance = function(game, game.fun=app$game.fun, app=getApp()) {
  game.fun(rq.fun=game$rq.fun,comp.prob=game$comp.prob, rq.fun.args=game$rq.fun.args)
}

rq.solo.game = function(rq=NULL, comp.prob= 0.5, rq.fun=NULL, rq.fun.args=list(...),...) {
  restore.point("rq.solo.game")

  if (is.null(rq)) {
    rq = do.call(rq.fun, rq.fun.args)
  }

  game = as.environment(nlist(
    rq,comp.prob, solved = rep(0,rq$top.n), chosen.keys=NULL, choice.handler = solo.choice.handler, ui.fun = make.rq.ui, points = c(0,0), prev.choices = NULL, finished=FALSE, rq.fun=rq.fun, rq.fun.args=rq.fun.args
  ))
  game$keys = rq$dat[[rq$key]][1:rq$choice.n]
  names(game$solved) = game$keys[1:rq$top.n]

  game

}

solo.choice.handler = function(choice, rq, game=getGame(), ...) {
  restore.point("solo.choice.handler")
  cat("\nChoice pressed...")
  game$solved[[choice]] = 1
  game$chosen.keys = c(game$chosen.keys, choice)

  comp.choice = solo.computer.move(game)

  game$finished = (game$rq$choice.n -length(game$chosen.keys)<=1) | length(setdiff(game$rq$key,game$chosen.keys))==0

  game$prev.choices = c(choice, comp.choice)

  game$points = c(
    sum(game$rq$dat$points[which(game$solved==1)]),
    sum(game$rq$dat$points[which(game$solved==2)])
  )
  ui =  game$ui.fun(game=game)
  setUI("mainUI",ui)
}

solo.computer.move = function(game) {
  restore.point("solo.computer.move")


  # the index of the choosen alternative
  # the computer goes the answers down from top to bottom
  # and picks the current answer with probability comp.prob
  # This can be simulated with the following draw from
  # a negative binomial distribution
  index = rnbinom(n=1, size=1, prob=game$comp.prob)+1
  rem.keys = setdiff(game$keys, game$chosen.keys)
  choice = rem.keys[((index-1) %% length(rem.keys))+1]

  game$solved[[choice]] = 2
  game$chosen.keys = c(game$chosen.keys, choice)
  return(choice)
}

getGame = function(app=getApp()) {
  app$game
}
