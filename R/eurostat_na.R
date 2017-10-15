example.eurostat_na = function() {
  #install.packages("eurostat")
  quiz.dir = "D:/libraries/dataquiz/quizdir"
  set.dataquiz.options(quiz.dir)
  options(scipen=999)

  co = unique(rbind(eu_countries, ea_countries, efta_countries, eu_candidate_countries) %>% arrange(code))

  txt = paste0('"',co[,2],'" = "',co[,1], '"', collapse=", ")
  writeClipboard(txt)


  gen = quiz.gen.eurostat.na(countries=c("DE","UK"), start.year=1990, do.scale = TRUE)
  data = load.gen.data.eurostat.na(gen=gen)
  unique(data$geo)

  dq = make.quiz.eurostat.na(data, gen=gen)

  app = dataquizQuickApp(quiz.fun=make.quiz.eurostat.na, dat=data, gen=gen)
  viewApp(app)

}


quiz.gen.info.eurostat.na = function() {
  info = list(
    label = "Guess the Sector (Eurostat time series)",
    fields =   list(
      country = list(
        default = "EU28",
        set = unique(dat$cntry)
      ),
      compare.country = list(
        default = "",
        set = c("",unique(dat$cntry))
      ),
      num.sectors = list(
        default = 4,
        class = "integer"
      ),
      start.year = list(
        default = NULL,
        class = "integer"
      ),
      end.year = list(
        default = NULL,
        class = "integer"
      )
    )
  )

}

quiz.gen.eurostat.na = function(country="DE", compare.country = NULL, num.sectors=4, start.year=NULL, end.year = NULL, do.scale=TRUE, countries = c(country, compare.country)) {
  list(
    gentype = "eurostat.na.pq",
    quiztype = "pq",
    #country = country,
    #compare.country = compare.country,
    countries = countries,
    do.scale = do.scale,
    start.year = start.year,
    end.year = end.year,
    sectors = NULL,
    item = NULL,
    num.sectors= num.sectors
  )
}


make.quiz.eurostat.gen.ui = function(country = "DE", compare.country = "FR", countries=c("Albania" = "AL", "Austria" = "AT", "Belgium" = "BE", "Bulgaria" = "BG", "Switzerland" = "CH", "Cyprus" = "CY", "Czech Republic" = "CZ", "Germany" = "DE", "Denmark" = "DK", "Estonia" = "EE", "Greece" = "EL", "Spain" = "ES", "Finland" = "FI", "France" = "FR", "Croatia" = "HR", "Hungary" = "HU", "Ireland" = "IE", "Iceland" = "IS", "Italy" = "IT", "Liechtenstein" = "LI", "Lithuania" = "LT", "Luxembourg" = "LU", "Latvia" = "LV", "Montenegro" = "ME", "The former Yugoslav Republic of Macedonia" = "MK", "Malta" = "MT", "Netherlands" = "NL", "Norway" = "NO", "Poland" = "PL", "Portugal" = "PT", "Romania" = "RO", "Serbia" = "RS", "Sweden" = "SE", "Slovenia" = "SI", "Slovakia" = "SK", "Turkey" = "TR", "United Kingdom" = "UK"
)) {

  restore.point("make.quiz.eurostat.gen.ui")
  ns = NS("eurostat")
  form.ids =c(ns("countrySelect"),ns("compareCountrySelect"),ns("scaleCheckbox"))
  ui = tagList(
    h3("Sector Statistics Eurostat"),
    selectInput(ns("countrySelect"),"Country", choices=countries, selected = country),
    selectInput(ns("compareCountrySelect"),"Comparison Country", choices=countries, selected = compare.country),
    checkboxInput(ns("scaleCheckbox"),"Scale comparison country",value = FALSE),
    simpleButton(ns("startBtn"),"Start Quiz", form.ids = form.ids)
  )

  buttonHandler(ns("startBtn"), function(formValues,...) {
    restore.point("eurostatStartQuiz")

    gen = quiz.gen.eurostat.na(countries=c(formValues[[ns("countrySelect")]], formValues[[ns("compareCountrySelect")]]), start.year=1990, do.scale = formValues[[ns("scaleCheckbox")]])
    data = load.gen.data.eurostat.na(gen=gen)
    dq = make.quiz.eurostat.na(data, gen=gen)

    startDataQuiz(quiz.fun=make.quiz.eurostat.na, dat=data, gen=gen)
  })

  ui
}


make.quiz.eurostat.na = function(dat=load.gen.data.eurostat.na(gen=gen), gen, done=NULL, weight.done = first.non.null(gen$weight.done,0.05)) {

  restore.point("make.quiz.eurostat.na")

  if (!is.null(gen$countries))
    dat = filter(dat, geo %in% gen$countries)

  dat = na.omit(dat)




  if (!is.null(gen$start.year))
    dat = filter(dat, year >= gen$start.year)

  if (!is.null(gen$end.year))
    dat = filter(dat, year <= gen$end.year)

  dat = mutate(dat,
    measure = paste0(na_item_label, ". Unit: ", unit_label),
    value = values
    ) %>%
    mutate(
      sector = paste0(nace_r2, " ",nace_r2_label),
      donekey = paste0(sector,"\t",measure)
) %>%
    select(year, sector, measure, value, geo, pop, donekey)

  # item is drawn irrespective of done weights
  items = unique(dat$measure[dat$geo == gen$countries[1] ])
  item = sample(items,1)


  d = filter(dat, measure==item)

  all.sectors = na.omit(unique(d$sector[dat$geo == gen$countries[1] ]))

  is.done = paste0(all.sectors,"\t",item) %in% done
  prob = ifelse(is.done, weight.done, 1)

  sectors = sample(all.sectors, gen$num.sectors,prob = prob)

  d = filter(d, sector %in% sectors)

  restore.point("make.eurostat.na.2")


  country.lab = gen$countries
  if (length(country.lab)==2 & gen$do.scale) {
    country.lab[2] = paste0(country.lab[2]," (scaled to population of ", country.lab[1],")")
  }

  question = paste0("The plot shows the time series of ", item," in ", paste0(country.lab, collapse=" and "), " for which sector?")

  facetvar = NULL
  if (length(gen$countries)==2) {
    if (gen$do.scale) {
      pop1 = d %>%
        filter(geo == gen$countries[1]) %>%
        group_by(year) %>%
        summarize(pop1 = first(pop))

      d = left_join(d, pop1, by="year")

      d = mutate(d,value = value * pop1 / pop)
    }

    facetvar = "geo"
    d$geo = factor(d$geo, levels = gen$countries)
  }

  dq = make.plotquiz(dat=d,keyvar = "sector", valuevar="value", timevar="year", donevar="donekey", facetvar=facetvar, question=question, choice.n=length(sectors), keys=sectors, gen=gen)

  dq
}

get.eurostat.pop.data = function() {
  pop = get.eurostat.data("demo_pjanbroad")
  pop = filter(pop, age=="TOTAL", sex=="T") %>%
    select(geo, year, values) %>%
    rename(pop=values)
  pop
}

make.eurostat.industry.pq.data = function() {
  restore.point("make.eurostat.name.industry.data")
  dat1 = get.eurostat.data("nama_10_a64")
  dat2 = get.eurostat.data("nama_10_a64_e")

  pop = get.eurostat.pop.data()


  dat = rbind(dat1,dat2)

  #dat = left_join(dat, pop, by=c("year","geo"))
  #cat(paste0('"',unique(dat$na_item_label),'"',collapse=",\n"))

  dat$sector = paste0(dat$nace_r2," ",dat$nace_r2_label)

  #cat(paste0('"',unique(dat$sector),'"',collapse=",\n"))


  items = c(
    "Value added, gross",
    "Compensation of employees",
    "Total employment domestic concept"
  )
  dat = filter(dat, na_item_label %in% items)
  #cat(paste0('"',unique(dat$unit_label),'"',collapse=",\n"))

  used.units = c(
"Chain linked volumes (2005), million euro",
"Chain linked volumes (2010), million euro",
#"Current prices, million euro",
#"Thousand hours worked",
"Thousand persons",
"Thousand jobs")

  dat = filter(dat, unit_label %in% used.units)

  sectors = c(
    "A Agriculture, forestry and fishing",
"A01 Crop and animal production, hunting and related service activities",
"A02 Forestry and logging",
"A03 Fishing and aquaculture",
"B Mining and quarrying",
"B-E Industry (except construction)",
"C Manufacturing",
"C10-C12 Manufacture of food products; beverages and tobacco products",
"C13-C15 Manufacture of textiles, wearing apparel, leather and related products",
#"C16 Manufacture of wood and of products of wood and cork, except furniture; manufacture of articles of straw and plaiting materials",
#"C16-C18 Manufacture of wood, paper, printing and reproduction",
"C17 Manufacture of paper and paper products",
"C18 Printing and reproduction of recorded media",
"C19 Manufacture of coke and refined petroleum products",
"C20 Manufacture of chemicals and chemical products",
"C21 Manufacture of basic pharmaceutical products and pharmaceutical preparations",
#"C22 Manufacture of rubber and plastic products",
"C22_C23 Manufacture of rubber and plastic products and other non-metallic mineral products",
#"C23 Manufacture of other non-metallic mineral products",
#"C24 Manufacture of basic metals",
"C24_C25 Manufacture of basic metals and fabricated metal products, except machinery and equipment",
#"C25 Manufacture of fabricated metal products, except machinery and equipment",
"C26 Manufacture of computer, electronic and optical products",
"C27 Manufacture of electrical equipment",
"C28 Manufacture of machinery and equipment n.e.c.",
"C29 Manufacture of motor vehicles, trailers and semi-trailers",
#"C29_C30 Manufacture of motor vehicles, trailers, semi-trailers and of other transport equipment",
"C30 Manufacture of other transport equipment",
#"C31-C33 Manufacture of furniture; jewellery, musical instruments, toys; repair and installation of machinery and equipment",
"C31_C32 Manufacture of furniture; other manufacturing",
"C33 Repair and installation of machinery and equipment",
"D Electricity, gas, steam and air conditioning supply",
"E Water supply; sewerage, waste management and remediation activities",
#"E36 Water collection, treatment and supply",
#"E37-E39 Sewerage, waste management, remediation activities",
"F Construction",
"G Wholesale and retail trade; repair of motor vehicles and motorcycles",
#"G-I Wholesale and retail trade, transport, accomodation and food service activities",
"G45 Wholesale and retail trade and repair of motor vehicles and motorcycles",
"G46 Wholesale trade, except of motor vehicles and motorcycles",
"G47 Retail trade, except of motor vehicles and motorcycles",
"H Transportation and storage",
"H49 Land transport and transport via pipelines",
"H50 Water transport",
"H51 Air transport",
"H52 Warehousing and support activities for transportation",
"H53 Postal and courier activities",
"I Accommodation and food service activities",
"J Information and communication",
"J58 Publishing activities",
#"J58-J60 Publishing, motion picture, video, television programme production; sound recording, programming and broadcasting activities",
"J59_J60 Motion picture, video, television programme production; programming and broadcasting activities",
"J61 Telecommunications",
"J62_J63 Computer programming, consultancy, and information service activities",
"K Financial and insurance activities",
"K64 Financial service activities, except insurance and pension funding",
"K65 Insurance, reinsurance and pension funding, except compulsory social security",
"K66 Activities auxiliary to financial services and insurance activities",
"L Real estate activities",
"L68A Imputed rents of owner-occupied dwellings",
"M Professional, scientific and technical activities",
#"M69-M71 Legal and accounting activities; activities of head offices; management consultancy activities; architectural and engineering activities; technical testing and analysis",
"M69_M70 Legal and accounting activities; activities of head offices; management consultancy activities",
"M71 Architectural and engineering activities; technical testing and analysis",
"M72 Scientific research and development",
"M73 Advertising and market research",
#"M73-M75 Advertising and market research; other professional, scientific and technical activities; veterinary activities",
#"M74_M75 Other professional, scientific and technical activities; veterinary activities",
#"M_N Professional, scientific and technical activities; administrative and support service activities",
"N Administrative and support service activities",
"N77 Rental and leasing activities",
"N78 Employment activities",
"N79 Travel agency, tour operator reservation service and related activities",
"N80-N82 Security and investigation, service and landscape, office administrative and support activities",
"O Public administration and defence; compulsory social security",
#"O-Q Public administration, defence, education, human health and social work activities",
"P Education",
"Q Human health and social work activities",
"Q86 Human health activities",
"Q87_Q88 Residential care activities and social work activities without accommodation",
"R Arts, entertainment and recreation",
#"R-U Arts, entertainment and recreation; other service activities; activities of household and extra-territorial organizations and bodies",
"R90-R92 Creative, arts and entertainment activities; libraries, archives, museums and other cultural activities; gambling and betting activities",
"R93 Sports activities and amusement and recreation activities",
"S Other service activities",
#"S94 Activities of membership organisations",
"S95 Repair of computers and personal and household goods",
"S96 Other personal service activities",
"T Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use",
"TOTAL Total - all NACE activities",
"U Activities of extraterritorial organisations and bodies"
  )
  dat = filter(dat, sector %in% sectors)

  dat = na.omit(dat)
  dat = left_join(dat, pop, by=c("year","geo"))
  dat
}
load.main.data.eurostat.na = function(data.dir=dataquiz.data.dir(), update=FALSE) {
  restore.point("load.main.data.eurostat.na")

  options(eurostat_cache_dir=file.path(data.dir,"cache"), euro_stat_update=update)

  file = file.path(data.dir, "eurostat_name_pq.rds")
  if (file.exists(file) & !update) {
    dat = readRDS(file)
    return(dat)
  }

  dat = make.eurostat.industry.pq.data()
  saveRDS(dat, file)
  dat
}

load.gen.data.eurostat.na = function(gen, data.dir=dataquiz.data.dir()) {
  restore.point("load.gen.data.eurostat.na")

  setwd(data.dir)
  # save data for specific countries
  files = paste0("eurostat_name_pq_",gen$countries,".rds")
  exists = file.exists(files)

  if (all(exists)) {
    li = lapply(files, readRDS)
    dat = bind_rows(li)
  } else {
    dat = load.main.data.eurostat.na()
    dat = dat[dat$geo %in% gen$countries,]

    # save country data
    for (i in which(!exists)) {
      file = files[i]
      country = gen$countries[i]
      d = dat[dat$geo %in% country,]
      saveRDS(d, file)
    }
  }
  dat
}

