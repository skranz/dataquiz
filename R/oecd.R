example.ameco.pq = function() {
  quiz.dir = "D:/libraries/dataquiz/quizdir"
  set.dataquiz.options(quiz.dir)
  data.dir = file.path(quiz.dir,"data")
  #combine.oecd.quiz.data(data.dir)

  options(scipen=999)


  gen = quiz.gen.oecd.pq(countries=c("DEU","GBR"), start.year=1990, do.scale = TRUE)
  data = load.gen.data.oecd.pq(gen=gen)

  dq = make.quiz.oecd.pq(data, gen=gen)

  app = dataquizQuickApp(quiz.fun=make.quiz.oecd.pq, dat=data, gen=gen)
  viewApp(app)
}


examples.oecd = function() {
  library(readr)
  library(dplyr)


}


quiz.gen.oecd.pq = function(country="DE", compare.country = NULL, num.items=4, start.year=NULL, end.year = NULL, do.scale=FALSE, countries = c(country, compare.country), ignore.unit.types = if(length(countries)>1) "large") {
  restore.point("quiz.gen.oecd.pq")

  list(
    gentype = "oecd.pq",
    quiztype = "pq",
    countries = countries,
    do.scale = do.scale,
    start.year = start.year,
    end.year = end.year,
    item = NULL,
    num.items= num.items,
    ignore.unit.types = ignore.unit.types
  )
}

make.quiz.oecd.pq = function(dat=load.gen.data.ameco.pq(gen=gen), gen=quiz.gen.oecd.pq(),...) {
  restore.point("quiz.make.oecd.pq")

  dat = na.omit(dat)
  if (!is.null(gen$start.year))
    dat = filter(dat, year >= gen$start.year)

  if (!is.null(gen$end.year))
    dat = filter(dat, year <= gen$end.year)

  d = filter(dat,cntry %in% gen$countries)

  if (!is.null(gen$ignore.unit.types))
    d = filter(d, !unit.type %in% gen$ignore.unit.types)

  # Maybe adapt to get long country names
  countries = country.code.to.name(gen$countries,code = "iso3c")

  d = mutate(d,measure = paste0(indicator_label, ", ", subject_label, ", ",unit_label,"."))
    #cat(paste0('"',unique(dat$title),'"', collapse=",\n"))


  items = first.non.null(gen[["items"]],unique(d$measure))
  num.items = min(gen$num.items, length(items))
  if (is.null(gen[["item"]])) {
    item = sample(items, 1)
  } else {
    item = gen[["item"]]
  }

  # pick items of same unit.type
  row = min(which(d$measure==item))
  .unit.type = unit.type = d$unit.type[row]

  d = filter(d, unit.type==.unit.type)
  items = unique(d$measure)
  num.items = min(num.items, length(items))

  choice.items = sample(c(item, sample(setdiff(items,item), num.items-1)))

  d = filter(d, measure %in% choice.items)
  key = item

  facetvar = NULL
  if (length(gen$countries)>1) {
    facetvar = "cntry"
    d$cntry = factor(d$cntry, levels = gen$countries)
  }

  restore.point("3u73u74rzhzdi")

  question = paste0("Which time series for ", paste0(countries, collapse=" and ")," does the plot show?")

  d$help.link = NULL

  dq = make.plotquiz(dat=d,keyvar = "measure", valuevar="value", timevar="year", facetvar=facetvar, question=question,gen=gen)

  dq$plot = dq$plot + ylab("") + xlab("")

  writeDataQuizLog("make_quiz",c("oecd_pq", dq$dqhash))
  dq
}


make.quiz.oecd.gen.ui = function(country = "DEU", compare.country = "FRA", countries=c("ARG","AUS","AUT","BEL","BGR","BRA","BRN","CAN","CHE","CHL","CHN","COL","CRI","CYP","CZE","DEU","DEW","DNK","EA","EA18","EA19","ESP","EST","EU","EU28","FIN","FRA","G20","GBR","GRC","HKG","HRV","HUN","IDN","IND","IRL","ISL","ISR","ITA","JPN","KHM","KOR","LTU","LUX","LVA","MAR","MEX","MLT","MYS","NLD","NOR","NZL","OECD","OECDE","PER","PHL","POL","PRT","ROU","RUS","SAU","SGP","SVK","SVN","SWE","THA","TUN","TUR","TWN","USA","VNM","WLD","ZAF")) {

  restore.point("make.quiz.oecd.gen.ui")
  ns = NS("oecd")
  #cat(paste0('"', sort(unique(df$cntry)), '"', collapse=","))

  country_labels = country.code.to.name(countries)
  names(countries) = country_labels
  ord = order(country_labels)
  countries = countries[ord]

  form.ids =c(ns("countrySelect"),ns("compareCountrySelect"),ns("scaleCheckbox"))
  ui = tagList(
    h3("Annual Data from the OECD"),
    selectInput(ns("countrySelect"),"Country", choices=countries, selected = country),
    selectInput(ns("compareCountrySelect"),"Comparison Country", choices=countries, selected = compare.country),
    #checkboxInput(ns("scaleCheckbox"),"Scale comparison country",value = FALSE),
    simpleButton(ns("startBtn"),"Start Quiz", form.ids = form.ids)
  )

  buttonHandler(ns("startBtn"), function(formValues,...) {
    restore.point("oecdStartQuiz")

    gen = quiz.gen.oecd.pq(countries=c(formValues[[ns("countrySelect")]], formValues[[ns("compareCountrySelect")]]), start.year=1990)
    data = load.gen.data.oecd.pq(gen=gen)
    dq = make.quiz.oecd.pq(data, gen=gen)

    startDataQuiz(quiz.fun=make.quiz.oecd.pq, dat=data, gen=gen)

  })

  ui
}


load.gen.data.oecd.pq = function(gen, data.dir=dataquiz.data.dir()) {
  restore.point("load.gen.data.oecd.pq")

  setwd(data.dir)
  # save data for specific countries
  files = paste0("oecd_pq_",gen$countries,".rds")
  exists = file.exists(files)

  if (all(exists)) {
    li = lapply(files, readRDS)
    dat = bind_rows(li)
  } else {
    dat = readRDS("oecd.rds")
    dat = dat[dat$cntry %in% gen$countries,]

    # save country data
    for (i in which(!exists)) {
      file = files[i]
      country = gen$countries[i]
      d = dat[dat$cntry %in% country,]
      saveRDS(d, file)
    }
  }
  dat
}


combine.oecd.quiz.data = function(data.dir, oecd.dir = file.path(data.dir, "oecd")) {
  restore.point("combine.oecd.quiz.data")
  files = list.files(oecd.dir, glob2rx("*.csv"),full.names = TRUE)
  li = lapply(files, function(file) {
    readr::read_csv(file)
  })
  df_raw = bind_rows(li)
  saveRDS(df_raw, file.path(data.dir, "oecd_raw.rds"))

  df = adapt.oecd.quiz.data(df_raw)
  saveRDS(df, file.path(data.dir, "oecd.rds"))

}

adapt.oecd.quiz.data = function(df = readRDS("oecd_raw.rds")) {
  restore.point("adapt.oecd.quiz.data")
  #cat(paste0('"',colnames(df),'"', collapse=","))
  df=df[,c("cntry","TIME","INDICATOR","SUBJECT","MEASURE","Value","indicator_label","subject_label","measure_label")]

  colnames(df) = tolower(colnames(df))
  colnames(df)[c(2,5,9)] = c("year","unit","unit_label")
  unique(df$unit)
  df = add.oecd.unit.type(df)
  df
}

add.oecd.unit.type = function(df) {
  df$unit.type = "other"
  rows = str.starts.with(df$unit,"PC")
  df$unit.type[rows] = "percentage"

  rows = df$unit %in% c("MLN_USD","USD_CAP")
  df$unit.type[rows] = df$unit[rows]
  df

}

