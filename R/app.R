examples.dataquizApp = function() {
  quiz.dir = "D:/libraries/dataquiz/quizdir"
  app = dataquiApp(quiz.dir)
  viewApp(app)
}


dataquizApp = function(quiz.dir) {
  restore.point("dataquizApp")
  app = eventsApp()

  app$quiz.dir = quiz.dir
  app$db.dir = file.path(quiz.dir, "db")
  app$dq.dir = file.path(quiz.dir, "dq")

  # database
  schema.file = file.path(quiz.dir, "schema", "quizdb.yaml")
  app$schemas = load.and.init.schemas(schema.file)

  db = dbConnect(SQLite(), dbname=file.path(app$db.dir,"quizdb.sqlite"))
  app$db = db = set.db.schemas(db, app$schemas)

  lop = loginModule(need.userid=TRUE,container.id = "mainUI", need.userid =TRUE, need.password = FALSE, use.signup = FALSE, init.userid= "")


  app$ui = bootstrapPage(
    uiOutput("mainUI")
  )
  appInitHandler(function(...,session=app$session,app=getApp()) {
    initLoginDispatch(lop)
  })
  app
}

dataquiz.login = function(userid,...) {

}


