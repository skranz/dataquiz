example.eurostat_sbs = function() {
  #install.packages("eurostat")

  options(eurostat_cache_dir="D:/libraries/dataquiz/cache/eurostat", euro_stat_update=FALSE)
  setwd("D:/libraries/dataquiz")

  dat = make.eurostat.name.industry.data(level="basic")
  d = filter(dat, geo=="DE")
  saveRDS(d, "name_10_DE.rds")

  dat = make.eurostat.name.industry.data(level="detailed")
  d = filter(dat, geo=="DE")
  saveRDS(d, "name_64_DE.rds")


  d = readRDS("name_10_DE.rds")
  d = readRDS("name_64_DE.rds")

  app = dataquizApp(quiz.fun=make.eurostat.na.industry.rank.quiz, dat=d, year=2012, base.year=2002, sector.digits=1)
  viewApp(app)

}

make.eurostat.na.industry.rank.quiz = function(dat, var=NULL, top.n=10, choice.n=top.n,  decreasing = TRUE, year=NULL, vars=NULL, base.year=NULL, country="DE", mode=c("value","change","perc_change")[1], sector.digits = NULL, unit=NULL) {

  restore.point("make.eurostat.na.industry.rank.quiz")

  if (!is.null(country))
    dat = filter(dat, geo==country)

  if (isTRUE(sector.digits == 3)) {
    dat = suppressWarnings(mutate(dat, is_nace = nchar(nace_r2)==3 & !is.na(as.numeric(substring(nace_r2,2,3)))))
    dat = filter(dat, is_nace)
  } else if (isTRUE(sector.digits == 1)) {
    dat = mutate(dat, is_nace = nchar(nace_r2)==1)
    dat = filter(dat, is_nace)
  }


  years = unique(dat$year)
  if (is.null(year))
    year = max(years)
  if (is.null(base.year))
    base.year = max(year-10,min(years))
  .year = year

  if (mode=="value")
    base.year = NULL

  #cat(paste0('"', unique(dat$INDIC_SB),'"', collapse=",\n"))

  dat = rename(dat,
    indicator = na_item_label,
    value = values
  ) %>%
    mutate(sector = paste0(nace_r2, " ",nace_r2_label)) %>%
    select(year, sector, indicator, value, unit_label)

  dat = filter(dat, !str.starts.with(sector,"TOTAL"))

  vars = unique(dat$indicator)
  if (is.null(var))
    var = sample(vars,1)

  d = filter(dat, indicator==var)

  #cat(paste0('"',unique(dat$unit_label),'"', collapse=",\n"))

  if (is.null(unit)) {
    # pick a unit
    ranked_units = c(
      "Current prices, million euro",
      "Thousand persons",
      "Thousand hours worked",
      "Price index (implicit deflator), 2010=100, euro"
    )
    units = unique(d$unit_label)
    rank = match(units, ranked_units)
    rank[is.na(rank)] = Inf
    unit = units[which.min(rank)]

  }

  d = filter(d, unit_label==unit)

  # time series dat for additional plots
  ts.dat = arrange(d, sector,year)

  d = filter(d, year %in% c(base.year,.year))

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

  } else {
    var.label = paste0(var, " in year ", year)
  }


  dq = make.rankquiz(d,ts.dat = ts.dat,key="sector",value="value", question=paste0("Eurostat National Accounts: Find sectors with ", if (decreasing) "highest" else "lowest"," '", var.label,"' (Unit=",unit,")."), top.n=top.n, choice.n=choice.n, decreasing=decreasing)

  dq
}



make.eurostat.name.industry.data = function(level=c("basic","detailed")[1]) {
  restore.point("make.eurostat.name.industry.data")
  if (level == "basic") {
    ids = c("nama_10_a10","nama_10_a10_e")
  } else {
    ids = c("nama_10_a64","nama_10_a64_e")
  }
  dat1 = get.eurostat.data(ids[1])
  dat2 = get.eurostat.data(ids[2])

  dat = rbind(dat1,dat2)

  #cat(paste0('"',unique(dat$unit),'"',collapse=",\n"))
  #cat(paste0('"',unique(dat$unit_label),'"',collapse=",\n"))

  used.units = c(
"Current prices, million euro",
"Percentage of gross domestic product (GDP)",
"Percentage of total",
#"Price index (implicit deflator), 2010=100, euro",
"Contribution to GDP growth, percentage point change on previous period",
"Current prices, million purchasing power standards",
#"Percentage of EU28 total (based on million euro), current prices",
#"Percentage change on previous period (based on hours worked)",
#"Percentage change on previous period (based on jobs)",
#"Percentage change on previous period (based on persons)",
"Percentage of total (based on hours worked)",
"Percentage of total (based on jobs)",
"Percentage of total (based on persons)",
"Thousand hours worked",
"Thousand jobs",
"Thousand persons"
  )

  dat = filter(dat, unit_label %in% used.units)

  dat = na.omit(dat)
  dat
}

