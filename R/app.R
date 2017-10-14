examples.dataquizApp = function() {
  restore.point.options(display.restore.point=TRUE)
  quiz.dir = "D:/libraries/dataquiz/quizdir"
  app = dataquizApp(quiz.dir)
  viewApp(app)
}


dataquizApp = function(quiz.dir) {
  restore.point("dataquizApp")
  app = eventsApp()

  app$quiz.dir = quiz.dir
  app$db.dir = file.path(quiz.dir, "db")
  app$dq.dir = file.path(quiz.dir, "dq")

  set.dataquiz.options(quiz.dir)


  # database
  schema.file = file.path(quiz.dir, "schema", "quizdb.yaml")
  app$schemas = load.and.init.schemas(schema.file)

  db = dbConnect(SQLite(), dbname=file.path(app$db.dir,"quizdb.sqlite"))
  app$db = db = set.db.schemas(db, app$schemas)

  lop = loginModule(container.id = "mainUI", need.userid =TRUE, need.password = FALSE, use.signup = FALSE, init.userid= random.nickname(), login.fun = dataquiz.login)


  app$ui = fluidPage(
    uiOutput("mainUI")
  )
  appInitHandler(function(...,session=app$session,app=getApp()) {
    initLoginDispatch(lop)
  })
  app
}

dataquiz.login = function(userid,...) {
  restore.point("dataquiz.login")
  ui = make.quiz.ameco.gen.ui()
  setUI("mainUI",ui)
}


startDataQuiz = function(dq=NULL, game=NULL, game.fun = NULL,quiz.fun=NULL,quiz.fun.args=list(...),app=getApp(),...) {
  restore.point("startDataQuiz")

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
  refresh.game.ui(app$game)
  app
}


