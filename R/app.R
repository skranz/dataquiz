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

  lop = loginModule(container.id = "mainUI", need.userid =FALSE, need.password = FALSE, use.signup = FALSE, init.userid="Guest", login.fun = dataquiz.login)


  shiny::addResourcePath("dataquiz", system.file("www", package="dataquiz"))
  app$ui = fluidPage(
    #tags$head(tags$script(src="dataquiz/jquery.swipe.min.js")),
    #tags$head(tags$script(src="dataquiz/dataquiz.js")),
    HTML("<style> body {background-color:#ddddff}</style>"),
    uiOutput("mainUI")
  )
  appInitHandler(function(...,session=app$session,app=getApp()) {
    initLoginDispatch(lop)
  })
  app
}

dataquiz.login = function(userid,...) {
  restore.point("dataquiz.login")
  ui = tagList(
    h3("Macroeconomic Data Quiz"),
    p("Teaching and research in economics focuses on theoretical and econometric models. But why not from time to time just take a dive into the raw data? Perhaps you gets some insights by solving some quizzes..."),
    make.quiz.ameco.gen.ui(),
    hr(),
    make.quiz.eurostat.gen.ui(),
    hr(),
    HTML("This website was created by <a href='https://www.uni-ulm.de/mawi/mawi-wiwi/institut/mitarbeiter/skranz/' target='_blank'>Sebastian Kranz</a> from <a href='http://www.uni-ulm.de/en/' target='_blank'>Ulm University</a>.

The quizzes are based on the EU commission's <a href='https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/macro-economic-database-ameco_en' target='_blank'>AMECO database</a>, as well as, sector level data from <a href='http://ec.europa.eu/eurostat/data/database' target='_blank'>EUROSTAT</a>.")
  )
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


