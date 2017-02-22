example.eurostat_sbs = function() {
  #install.packages("eurostat")

  options(eurostat_cache_dir="D:/libraries/rankquiz/cache/eurostat", euro_stat_update=FALSE)
  id = "sbs_na_sca_r2"

  dat = get.eurostat.data(id)

  rq = make.eurostat.sbs.sector.rank.quiz(dat=dat, country="DE", sector.digits=1)

  app = rankquizApp(rq)
  viewApp(app)

}

get.eurostat.data = function(id, stringsAsFactors=FALSE) {
  restore.point("get.eurostat.data")

  library(eurostat)
  dat = get_eurostat(id, stringsAsFactors=stringsAsFactors)
  labels = suppressWarnings(label_eurostat(dat))
  labels = labels[, setdiff(names(labels),c("time","values"))]
  colnames(labels)=paste0(colnames(labels),"_label")

  dat = cbind(dat,labels)
  dat$year = year(dat$time)
  dat

}

make.eurostat.sbs.sector.rank.quiz = function(dat, var=NULL, top.n=10, choice.n=top.n,  decreasing = TRUE, year=NULL, vars=NULL, base.year=NULL, country="DE", mode=sample(c("value","change","perc_change"),1), sector.digits = c(3,1)[1]) {

  restore.point("make.eurostat.sbs.sector.rank.quiz")

  dat = filter(dat, geo==country)

  if (sector.digits == 3) {
    dat = suppressWarnings(mutate(dat, is_nace = nchar(nace_r2)==3 & !is.na(as.numeric(substring(nace_r2,2,3)))))
    dat = filter(dat, is_nace)
  } else if (sector.digits == 1) {
    dat = mutate(dat, is_nace = nchar(nace_r2)==1)
    dat = filter(dat, is_nace)
  }


  years = unique(dat$year)
  if (is.null(year))
    year = max(years)
  if (is.null(base.year))
    base.year = min(years)
  .year = year

  if (mode=="value")
    base.year = NULL

  #cat(paste0('"', unique(dat$INDIC_SB),'"', collapse=",\n"))

  if (is.null(vars) & mode=="value") {
    vars = c(
    "Number of enterprises",
    "Turnover or gross premiums written",
    "Production value",
    "Gross margin on goods for resale",
    "Value added at factor cost",
#    "Gross operating surplus",
#    "Total purchases of goods and services",
    "Gross investment in tangible goods",
"Number of persons employed",
"Number of employees",
"Number of employees in full time equivalent units",
"Turnover per person employed",
"Apparent labour productivity (Gross value added per person employed)",
"Wage adjusted labour productivity (Apparent labour productivity by average personnel costs) (%)",
"Gross value added per employee",
"Share of personnel costs in production (%)",
"Average personnel costs (personnel costs per employee) (thousand euro)",
#"Growth rate of employment (%)",
"Number of persons employed per enterprise",
"Gross operating surplus/turnover (gross operating rate) (%)",
"Value added at factor cost in production value",
"Investment per person employed",
"Investment rate (investment/value added at factors cost)"
    )
  } else if (is.null(vars)) {
    vars = c(
    "Number of enterprises",
    "Turnover or gross premiums written",
    "Production value",
    "Gross margin on goods for resale",
    "Value added at factor cost",
#    "Gross operating surplus",
#    "Total purchases of goods and services",
    "Gross investment in tangible goods",
"Number of persons employed",
"Number of employees",
"Number of employees in full time equivalent units",
"Turnover per person employed",
"Apparent labour productivity (Gross value added per person employed)",
"Wage adjusted labour productivity (Apparent labour productivity by average personnel costs) (%)",
"Gross value added per employee",
"Average personnel costs (personnel costs per employee) (thousand euro)",
#"Growth rate of employment (%)",
"Gross operating surplus/turnover (gross operating rate) (%)",
"Value added at factor cost in production value",
"Investment per person employed",
"Investment rate (investment/value added at factors cost)"
    )

  }

  vars = intersect(vars,unique(dat$indic_sb_label))
  #vars = unique(dat$INDIC_SB)


  if (is.null(var))
    var = sample(vars,1)

  d = filter(dat, indic_sb_label==var) %>%
    mutate(sector = paste0(nace_r2, " ",nace_r2_label))


  # time series dat for additional plots
  ts.dat = rename(d, sector=sector, value=values) %>%
    arrange(sector,year) %>%
    select(year,sector, value)


  d = filter(d, year %in% c(base.year,.year))

  d = d %>%
    rename(indicator=indic_sb_label, value=values) %>%
    select(year, sector, indicator, value)

  var.label = var
  if (mode =="change") {
    d = arrange(d, sector, year) %>%
      group_by(sector, indicator) %>%
      summarize(year=max(year),info=paste0("to ",format(value[2], big.mark=" ")),value=value[2]-value[1]) %>%
      select(year, sector, indicator, value, info)
    var.label = paste0(var," change from ", base.year, " to ",.year)

  } else if (mode =="perc_change") {
    d = arrange(d, sector, year) %>%
      group_by(sector, indicator) %>%
      summarize(year=max(year),info=paste0("to ",format(value[2], big.mark=" ")),value=round( ((value[2]-value[1])/value[1])*100,1)) %>%
      select(year, sector, indicator, value, info)

    var.label = paste0(var," % change from ", base.year, " to ",.year)

  }


  rq = make.rankquiz(d,ts.dat = ts.dat,key="sector",value="value", question=paste0("Find sectors with ", if (decreasing) "highest" else "lowest"," '", var.label,"'."), top.n=top.n, choice.n=choice.n, decreasing=decreasing)

  rq
}
