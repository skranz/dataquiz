example.oecd = function() {
  setwd("D:/libraries/rankquiz")
  library(readr)
  ad = read_csv("oecd_stan.csv")
  d = annotate_stan_data(ad)
  saveRDS(d,"oecd_stan.rds")

  setwd("D:/libraries/rankquiz")
  d = readRDS("oecd_stan.rds")
  rq = make.oecd.stan.sector.rank.quiz(d, country="DEU",mode="value", value="RelValue")

  app = rankquizApp(rq)
  viewApp(app)


  dat = sample_n(ad,size = 100)
}

annotate_stan_data = function(d) {
  restore.point("annotate_stan_data")
  cat(paste0('"', unique(d$Variable),'"',collapse=",\n"))

  colnames(d)[1] ="cou"

  quiz.vars = c(
    "VALU Value added, current prices",
#    "VALK Value added, volumes",
#    "VALP Value Added, deflators",
    "LABR Labour costs (compensation of employees)",
#    "WAGE Wages and salaries",
    "EMPN Number of persons engaged (total employment)",
    "GFCF Gross fixed capital formation, current prices",
#    "GFCK Gross fixed capital formation, volumes",
#    "GFCP Gross Fixed Capital Formation, deflators",
    "CPNK Net capital stock, volumes",
#    "VAFC Value added at factor costs, current prices",
#    "OTXS Other taxes less subsidies on production",
#    "GOPS Gross operating surplus and mixed income",
    "EXPO Exports of goods at current prices",
    "IMPO Imports of goods at current prices",
#    "PROD Production (gross output), current prices",
#    "PRDK Production (gross output), volumes",
#    "PRDP Production (Gross Output), deflators",
#    "INTI Intermediate inputs, current prices",
#    "INTK Intermediate inputs, volumes",
#    "INTP Intermediate inputs, deflators",
    "EMPE Number of employees",
    "SELF Self-employed",
#    "FTEN Full-time equivalents - total engaged",
    "FTEE Full-time equivalents - employees",
    "HRSN Hours worked - total engaged",
    "HRSE Hours worked - employees",
#    "CPGK Gross capital stock, volumes",
#    "NOPS Net operating surplus and mixed income",
    "CFCC Consumption of fixed capital"
  )

  library(dplyr)
  d$is.quiz.var = d$Variable %in% quiz.vars
  d = filter(d, is.quiz.var)

  cat(paste0('"', unique(d$Industry),'"',collapse=",\n"))

  sector.total = "CTOTAL TOTAL"
  sector.large = c(
    "C01T05 AGRICULTURE, HUNTING, FORESTRY AND FISHING",
    "C10T14 MINING AND QUARRYING",
    "C10T41 INDUSTRY INCLUDING ENERGY",
    "C50T99 TOTAL SERVICES"
  )

  sector.medium = c(
"C01T05 AGRICULTURE, HUNTING, FORESTRY AND FISHING",
"C10T14 MINING AND QUARRYING",
"C15T16 Food products, beverages and tobacco",
"C17T19 Textiles, textile products, leather and footwear",
"C21T22 Pulp, paper, paper products, printing and publishing",
"C23T25 Chemical, rubber, plastics and fuel products",
"C2423 Pharmaceuticals",
"C27T28 Basic metals and fabricated metal products",
"C29 Machinery and equipment, n.e.c.",
"C30T33 Electrical and optical equipment",
"C34 Motor vehicles, trailers and semi-trailers",
"C35 Other transport equipment",
"C36T37 Manufacturing n.e.c. and recycling",
"C40T41 ELECTRICITY GAS AND, WATER SUPPLY",
"C45 CONSTRUCTION",
"C55 Hotels and restaurants",
"C60T63 Transport and storage",
"C64 Post and telecommunications",
"C65T67 Financial intermediation",
"C70 Real estate activities",
"C71T74 Renting of mach. and equip. - other business activities",
"C75 Public admin. and defence - compulsory social security",
"C80 Education",
"C85 Health and social work",
"C50 Sale, maintenance and repair of motor vehicles and motorcycles - retail sale of automotive fuel",
"C51 Wholesale, trade and commission excl. motor vehicles",
"C52 Retail trade excl. motor vehicles - repair of household goods",
"C72 Computer and related activities",
"C73 Research and development",
"C95 Private households with employed persons"
)
  sector.special = c(
"HITECH High-technology manufactures",
"MHTECH Medium-high technology manufactures",
"MLTECH Medium-low technology manufactures",
"LOTECH Low technology manufactures",
"ENERGYP Energy producing activities"
  )

  d$is.sector.total = d$Industry %in% sector.total
  d$is.sector.large = d$Industry %in% sector.large
  d$is.sector.medium = d$Industry %in% sector.medium
  d$is.sector.special = d$Industry %in% sector.special

  d$year = d$TIME
  d = select(d, -TIME, -`PowerCode Code`,-PowerCode,-`Reference Period`,`Reference Period Code`,-`Flags`,-`Flag Codes`)

  d = group_by(d,VAR,cou,year,Unit) %>%
    mutate(
      has.total = any(Industry == "CTOTAL TOTAL"),
      TotalValue = ifelse(has.total,Value[Industry == "CTOTAL TOTAL"],NA),
      RelValue=Value /TotalValue
    ) %>%
    select(-has.total) %>%
    ungroup()

  d
}


make.oecd.stan.sector.rank.quiz = function(dat, var=NULL, value=c("Value","RelValue")[1], top.n=10, choice.n=top.n,  decreasing = TRUE, year=NULL, base.year=NULL, country="GER", mode=sample(c("value","change","perc_change"),1), sector.type = c("medium","large","special")[1]) {

  restore.point("make.oecd.stan.sector.rank.quiz")

  dat = filter(dat, cou==country)

  # sector type
  dat = dat[dat[[paste0("is.sector.",sector.type)]],]
  years = unique(dat$year)
  if (is.null(year))
    year = max(years)
  if (is.null(base.year))
    base.year = min(years)
  .year = year

  if (mode=="value")
    base.year = NULL

  vars = unique(dat$Variable)
  #vars = unique(dat$INDIC_SB)


  if (is.null(var))
    var = sample(vars,1)

  d = filter(dat, Variable==var) %>%
    mutate(sector = Industry)


  # time series dat for additional plots
  ts.dat = rename_(d, .dots=list(sector="sector", value= value)) %>%
    arrange(sector,year) %>%
    select(year,sector, value)


  d = filter(d, year %in% c(base.year,.year))

  d = d %>%
    rename_(d, .dots=list(indicator="Variable", value= value)) %>%
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

