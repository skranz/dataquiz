example.dataquiz = function() {
  setwd("D:/libraries/dataquiz")
  restore.point.options(display.restore.point = TRUE)

  library(tidyr)
  library(readr)
  library(dplyr)
  dat = read_csv("stromprod.csv")
  d = gather(dat,key = "year",value="MWh",-type, convert=TRUE)
  max.year = max(d$year)
  d = d %>% filter(year==max.year)

  dq = make.rankquiz(d,key="type",value="MWh", question="Which power plant types produced most electricity in Germany in 2014?")

  app = dataquizQuickApp(dq)
  viewApp(app)
}

dataquizQuickApp = function(dq=NULL, game=NULL, game.fun = NULL,quiz.fun=NULL,quiz.fun.args=list(...),...) {
  restore.point("dataquizApp")
  app = eventsApp()

  if (is.null(game) & is.null(game.fun)) {
    if (is.null(dq)) {
      dq = do.call(quiz.fun, quiz.fun.args)
    }
    game.fun = get.default.game.fun(dq=dq)
  }

  if (is.null(game)) {
    game = game.fun(dq=dq,quiz.fun=quiz.fun,quiz.fun.args=quiz.fun.args)
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
  restore.point("new.game.instance")
  game.fun(quiz.fun=game$quiz.fun, quiz.fun.args=game$quiz.fun.args, game=game)
}

get.default.game.fun = function(dq, quiztype=dq$quiztype) {
  if (quiztype=="pq") return(pq.solo.game)
  if (quiztype=="rq") return(rq.solo.game)
  stop(paste0("unknown quiztype ", quiztype))

}

getGame = function(app=getApp()) {
  app$game
}

set.dataquiz.options = function(quiz.dir) {
  opts = list(
    quiz.dir = quiz.dir,
    db.dir = file.path(quiz.dir, "db"),
    dq.dir = file.path(quiz.dir, "dq"),
    gen.dir = file.path(quiz.dir, "gen"),
    data.dir = file.path(quiz.dir, "data")
  )
  options(dataquiz = opts)
}

dataquiz.data.dir = function() {
  dir = getOption("dataquiz")$data.dir
  if (is.null(dir)) stop("No data.dir set. Call set.dataquiz.options.")
  dir
}
