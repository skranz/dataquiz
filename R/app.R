examples.dataquizApp = function() {
  restore.point.options(display.restore.point=TRUE)
  quiz.dir = "D:/libraries/dataquiz/quizdir"
  app = dataquizApp(quiz.dir)
  viewApp(app)
}


dataquizApp = function(quiz.dir, do.log=TRUE, show.oecd=TRUE, show.eurostat.na=TRUE, show.ameco=FALSE) {
  restore.point("dataquizApp")
  app = eventsApp()

  app$do.log = TRUE
  app$quiz.dir = quiz.dir
  app$db.dir = file.path(quiz.dir, "db")
  app$dq.dir = file.path(quiz.dir, "dq")
  app$show.oecd = show.oecd
  app$show.eurostat.na = show.eurostat.na
  app$show.ameco = show.ameco





  set.dataquiz.options(quiz.dir)


  # database
  schema.file = file.path(quiz.dir, "schema", "quizdb.yaml")
  app$schemas = load.and.init.schemas(schema.file)

  db = dbConnect(SQLite(), dbname=file.path(app$db.dir,"quizdb.sqlite"))
  app$db = db = set.db.schemas(db, app$schemas)

  lop = loginModule(container.id = "mainUI", need.userid =FALSE, need.password = FALSE, use.signup = FALSE, init.userid=random.string(), login.fun = dataquiz.login, cookie.name = "dataQuizUserCookie")


  shiny::addResourcePath("dataquiz", system.file("www", package="dataquiz"))
  app$ui = fluidPage(
    #tags$head(tags$script(src="dataquiz/jquery.swipe.min.js")),
    #tags$head(tags$script(src="dataquiz/dataquiz.js")),
    cookiesHeader("dataquizUserCookie", eventId="dataquizLoadCookies"),
    HTML("<style> body {background-color:#ddddff}</style>"),
    uiOutput("mainUI")
  )
  loadPageCookiesHandler(dataquiz.cookies.loaded, "dataquizLoadCookies")

  appInitHandler(function(...,session=app$session,app=getApp()) {
    initLoginDispatch(lop)
  })
  app
}

dataquiz.login = function(userid=NULL,..., app=getApp()) {
  restore.point("dataquiz.login")

  app$userid = userid
  writeDataQuizLog("login")

  ui = tagList(
    h3("Macroeconomic Data Quiz"),
    p("Teaching and research in economics focuses on theoretical and econometric models. But why not from time to time just take a dive into the raw data? Perhaps you gets some insights by solving some quizzes..."),
    if (app$show.oecd) {
      tagList(
        make.quiz.oecd.gen.ui(),
        hr()
      )
    },
    if (app$show.eurostat.na) {
      tagList(
        make.quiz.eurostat.gen.ui(),
        hr()
      )
    },
    if (app$show.ameco) {
      tagList(
        make.quiz.ameco.gen.ui(),
        hr()
      )
    },
    HTML("This website was created by <a href='https://www.uni-ulm.de/mawi/mawi-wiwi/institut/mitarbeiter/skranz/' target='_blank'>Sebastian Kranz</a> from <a href='http://www.uni-ulm.de/en/' target='_blank'>Ulm University</a>.")
  )
  setUI("mainUI",ui)
}

dataquiz.cookies.loaded = function(cookies, ..., app=getApp()) {
  restore.point("dataquiz.cookies.loaded")
  cookie = cookies[["dataquizUserCookie"]]
  if (is.null(cookie)) {
    cookie = list(cookieid = random.string())
    setCookie("dataquizUserCookie", cookie, expires=360)
  }
  app$cookie = cookie
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

save.dataquiz.app.dq = function(dq, app=getApp(), smaller=TRUE) {
  restore.point("save.dataquiz.app.dq")

  if (is.null(app$quiz.dir)) return()
  dq.dir = file.path(app$quiz.dir,"dq")
  if (!dir.exists(dq.dir)) return()

  dq$plot = NULL
  dq$dat = NULL

  sdq = dq[c("quiztype","gentype","dqhash","question","keys")]
  sdq$sol.ind = match(dq$key, dq$keys)
  sdq$gen = dq$gen

  json = toJSON(sdq)
  write(json, file.path(dq.dir,"dq.json"),append = TRUE)
}

writeDataQuizLog = function(log.type="login",values=NULL, do.log = isTRUE(app$do.log), log.dir = file.path(app$quiz.dir,"log"),  log.file = paste0(log.type,".txt"), cookieid = first.non.null(app$cookie$cookieid, "-"), app=getApp()) {
  restore.point("writeDataQuizLog")
  if (!do.log) return()
  if (!dir.exists(log.dir)) return()

  values=c(as.character(Sys.time()),app$userid,cookieid, values)
  txt = paste0(values, collapse=", ")
  try(write(txt, file.path(log.dir, log.file), append=TRUE))
}
