examples.db = function() {
  quiz.dir = "D:/libraries/dataquiz/quizdir"
  db.dir = file.path(quiz.dir, "db")
  dq.dir = file.path(quiz.dir, "dq")
  schema.file = file.path(quiz.dir, "schema", "quizdb.yaml")

  data = readRDS(file.path(quiz.dir,"data","name_pq.rds"))
  gen = quiz.default.gen.eurostat.na(countries=c("DE","FR"), start.year=1990)
  dq = make.quiz.eurostat.na(dat=data, gen=gen)


  schemas = load.and.init.schemas(schema.file)
  db = dbConnect(SQLite(), dbname=file.path(db.dir,"quizdb.sqlite"))
  db = set.db.schemas(db, schemas)
  get.db.schemas(db)

  insert.dq(db=db, dq=dq, dq.dir=dq.dir)
  delete.dq(db=db, dq=dq, dq.dir=dq.dir)
  dbCreateSchemaTables(db)



  dbDisconnect(db)
}

insert.dq = function(db=app$db,dq, dq.dir = file.path(app$quiz.dir,"dq"), app=getApp()) {
  restore.point("insert.dq")

  values = nlist(
    quiztype=dq$quiztype,
    gentype=dq$gen$gentype,
    dqhash = dq$dqhash,
    genhash = dq$genhash,
    solution = first.non.null(dq[["key"]],""),
    created = Sys.time()
  )
  dbInsert(db,"dq",values)
  saveRDS(dq, file=file.path(dq.dir, dq$dqhash))
}

read.dq = function(dqhash, dq.dir = file.path(app$quiz.dir,"dq"), app=getApp()) {
  restore.point("read.dq")
  readRDS(file=file.path(dq.dir, dqhash))
}

delete.dq = function(db,dqhash=dq$dqhash, dq.dir = file.path(app$quiz.dir,"dq"),dq=NULL) {
  dbDelete(db,"dq",list(dqhash=dqhash))
  file.remove(file.path(dq.dir,dqhash))
}
