example.ameco.pq = function() {
  quiz.dir = "D:/libraries/dataquiz/quizdir"
  set.dataquiz.options(quiz.dir)
  options(scipen=999)


  gen = quiz.gen.ameco.pq(countries=c("DEU","GBR"), start.year=1990, do.scale = TRUE)
  data = load.gen.data.ameco.pq(gen=gen)

  dq = make.quiz.ameco.pq(data, gen=gen)

  app = dataquizQuickApp(quiz.fun=make.quiz.ameco.pq, dat=data, gen=gen)
  viewApp(app)
}


quiz.gen.ameco.pq = function(country="DE", compare.country = NULL, num.items=4, start.year=NULL, end.year = NULL, do.scale=TRUE, countries = c(country, compare.country), ignore.unit.types = if(length(countries)>1) "large") {
  restore.point("quiz.gen.ameco.pq")

  list(
    gentype = "ameco.pq",
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

make.quiz.ameco.gen.ui = function(country = "DEU", compare.country = "FRA", countries=c("EU28","EU15","EA19","EA12","DU15","DA12","BEL","BGR","CZE","DNK","DEU","D","EST","IRL","GRC","ESP","FRA","HRV","ITA","CYP","LVA","LTU","LUX","HUN","MLT","NLD","AUT","POL","PRT","ROM","SVN","SVK","FIN","SWE","GBR","MKD","ISL","TUR","MNE","SRB","ALB","NOR","CHE","USA","JPN","CAN","MEX","KOR","AUS","NZL","CU15","CA12","FU12","FA17","FA18","FA19","FU27","FU28","FA12","FU15")) {

  restore.point("make.quiz.ameco.gen.ui")
  ns = NS("ameco")
  form.ids =c(ns("countrySelect"),ns("compareCountrySelect"),ns("scaleCheckbox"))
  ui = tagList(
    h3("Macroeconomic Time Series from the AMECO Database"),
    selectInput(ns("countrySelect"),"Country", choices=countries, selected = country),
    selectInput(ns("compareCountrySelect"),"Comparison Country", choices=countries, selected = compare.country),
    checkboxInput(ns("scaleCheckbox"),"Scale comparison country",value = FALSE),
    simpleButton(ns("startBtn"),"Start Quiz", form.ids = form.ids)
  )

  buttonHandler(ns("startBtn"), function(formValues,...) {
    restore.point("amecoStartQuiz")

    gen = quiz.gen.ameco.pq(countries=c(formValues[[ns("countrySelect")]], formValues[[ns("compareCountrySelect")]]), start.year=1990, do.scale = formValues[[ns("scaleCheckbox")]])
    data = load.gen.data.ameco.pq(gen=gen)
    dq = make.quiz.ameco.pq(data, gen=gen)

    startDataQuiz(quiz.fun=make.quiz.ameco.pq, dat=data, gen=gen)

  })

  ui
}

make.quiz.ameco.pq = function(dat=load.gen.data.ameco.pq(gen=gen), gen=quiz.gen.ameco.pq(),...) {
  restore.point("quiz.make.ameco.pq")


  dat = na.omit(dat)
  if (!is.null(gen$start.year))
    dat = filter(dat, year >= gen$start.year)

  if (!is.null(gen$end.year))
    dat = filter(dat, year <= gen$end.year)


  d = filter(dat,cntry %in% gen$countries)

  if (!is.null(gen$ignore.unit.types))
    d = filter(d, !unit.type %in% gen$ignore.unit.types)


  if (length(gen$countries)>1)
    d = filter(d, !local.unit)


  temp = group_by(d, cntry) %>%
    summarize(country=first(country))
  countries = temp$country
  names(countries) = temp$cntry
  countries = countries[gen$countries]

  d = mutate(d,measure = paste0(title, ". Unit: ",unit,"."))
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

  d$help.link = paste0("http://ec.europa.eu/economy_finance/ameco/HelpHtml/", tolower(d$item_code),".html")

  dq = make.plotquiz(dat=d,keyvar = "measure", valuevar="value", timevar="year", facetvar=facetvar, question=question)

  dq$plot = dq$plot + ylab("") + xlab("")

  writeDataQuizLog("make_quiz",c("ameco_pq", dq$dqhash))


  dq
}


get.ameco.items = function(...) {
  items = c(
"Total population",
"Population: 65 years and over",
"Total labour force (Labour force statistics)",
"Civilian employment, persons (domestic)",
"Unemployment rate: total :- Member States: definition EUROSTAT",
"NAWRU",
"Employment, persons: total economy (National accounts)",
"Employment, full-time equivalents: total economy (National accounts)",
"Number of self-employed: total economy (National accounts)",
"Net exports of goods and services at current prices (National accounts)",
"Net exports of goods at current prices (National accounts)",
"Net exports of services at current prices (National accounts)",
"Net primary income from the rest of the world (National accounts)",
"Net current transfers from the rest of the world (National accounts)",
"Balance on current transactions with the rest of the world (National accounts)",
"Net capital transactions with the rest of the world (National accounts)",
"Net lending (+) or net borrowing (-): total economy",
"Total exports of goods :- Foreign trade statistics",
"Total imports of goods :- Foreign trade statistics",
"Share of exports of goods in world exports including intra EU exports :- Foreign trade statistics",
"Share of imports of goods in world imports including intra EU imports :- Foreign trade statistics",
"Average share of imports and exports of goods in world trade including intra EU trade :- Foreign trade statistics",
"Employment, persons: agriculture, forestry and fishery products (National accounts)",
"Employment, persons: industry excluding building and construction (National accounts)",
"Employment, persons: building and construction (National accounts)",
"Employment, persons: services (National accounts)",
"Employment, persons: manufacturing industry (National accounts)",
"Employment, full-time equivalents: agriculture, forestry and fishery products (National accounts)",
"Employment, full-time equivalents: industry excluding building and construction (National accounts)",
"Employment, full-time equivalents: building and construction (National accounts)",
"Employment, full-time equivalents: services (National accounts)",
"Employment, full-time equivalents: manufacturing industry (National accounts)",
"Employees, persons: agriculture, forestry and fishery products (National accounts)",
"Employees, persons: industry excluding building and construction (National accounts)",
"Employees, persons: building and construction (National accounts)",
"Employees, persons: services (National accounts)",
"Employees, persons: manufacturing industry (National accounts)",
"Employees, full-time equivalents: agriculture, forestry and fishery products (National accounts)",
"Employees, full-time equivalents: industry excluding building and construction (National accounts)",
"Employees, full-time equivalents: building and construction (National accounts)",
"Employees, full-time equivalents: services (National accounts)",
"Employees, full-time equivalents: manufacturing industry (National accounts)",
"Gross Value Added at current prices: total of branches",
"Gross Value Added at current prices: agriculture, forestry and fishery products",
"Gross value added at current prices: industry excluding building and construction",
"Gross value added at current prices: building and construction",
"Gross value added at current prices: services",
"Gross value added at current prices: manufacturing industry",
"Gross value added at current prices per person employed: agriculture, forestry and fishery products",
"Gross value added at current prices per person employed: industry excluding building and construction",
"Gross value added at current prices per person employed: building and construction",
"Gross value added at current prices per person employed: services",
"Gross value added at current prices per person employed: manufacturing industry",
"Gross value added at current prices per employee: manufacturing industry",
"Gross value added at 2010 prices: total of branches",
"Gross value added at 2010 prices: agriculture, forestry and fishery products",
"Gross value added at 2010 prices: industry excluding building and construction",
"Gross value added at 2010 prices: building and construction",
"Gross value added at 2010 prices: services",
"Gross value added at 2010 prices: manufacturing industry",
"Gross value added at 2010 prices per person employed: agriculture, forestry and fishery products",
"Gross value added at 2010 prices per person employed: industry excluding building and construction",
"Gross value added at 2010 prices per person employed: building and construction",
"Gross value added at 2010 prices per person employed: services",
"Gross value added at 2010 prices per person employed: manufacturing industry",
"Gross value added at 2010 prices per employee: manufacturing industry",
"Nominal compensation per employee: agriculture, forestry and fishery products",
"Nominal compensation per employee: industry excluding building and construction",
"Nominal compensation per employee: building and construction",
"Nominal compensation per employee: services",
"Nominal compensation per employee: manufacturing industry",
"Adjusted wage share: manufacturing industry (Compensation per employee as percentage of nominal gross value added per person employed.)",
"Nominal unit wage costs: agriculture, forestry and fishery products",
"Nominal unit wage costs: industry excluding building and construction",
"Nominal unit wage costs: building and construction",
"Nominal unit wage costs: manufacturing industry",
"Nominal unit wage costs: services",
"Nominal unit labour costs: manufacturing industry",
"Real unit labour costs: manufacturing industry",
"Industrial production: construction excluded",
"Real effective exchange rates, based on unit labour costs (total economy) :- Performance relative to the rest of the former EU-15: double export weights",
"Real effective exchange rates, based on unit labour costs (total economy) :- Performance relative to the rest of 24 industrial countries: double export weights : EU-15, TR CH NR US CA JP AU MX and NZ",
"GDP purchasing power parities :- Units of national currency per PPS (purchasing power standard)",
"Nominal short-term interest rates",
"Real short-term interest rates, deflator GDP",
"Nominal long-term interest rates",
"Real long-term interest rates, deflator GDP",
"Net property income: corporations",
#"Net current transfers received: corporations",
#"Other taxes on production: corporations",
"Compensation of employees: corporations",
#"Current taxes on income and wealth: corporations",
#"Adjustment for the change in net equity of households on pension funds: corporations",
"Gross capital formation: corporations",
#"Other capital expenditure, net: corporations",
#"Gross value added at basic prices: corporations",
"Gross operating surplus: corporations",
#"Gross balance of primary income, corporations",
#"Net balance of primary income, corporations",
#"Gross disposable income, corporations",
#"Net disposable income, corporations",
"Gross saving: corporations",
"Net saving: corporations",
"Net lending (+) or net borrowing (-): corporations",
"Compensation of employees: households and NPISH",
"Gross wages and salaries: households and NPISH",
"Net property income: households and NPISH",
#"Current transfers received: households and NPISH",
"Current taxes on income and wealth: households and NPISH",
#"Current transfers paid: households and NPISH",
"Final consumption expenditure: households and NPISH",
"Gross capital formation: households and NPISH",
#"Other capital expenditure, net: households and NPISH",
#"Gross operating surplus and mixed income: households and NPISH",
"Non-labour income: households and NPISH",
"Gross disposable income: households and NPISH",
"Real gross disposable income, deflator private consumption: households and NPISH",
"Net disposable income: households and NPISH",
"Gross saving: households and NPISH",
"Saving rate, gross: households and NPISH (Gross saving as percentage of gross disposable income)",
"Net saving: households",
"Saving rate, net: households and NPISH (Net saving as percentage of net disposable income)",
"Net lending (+) or net borrowing (-): households and NPISH",
"Taxes linked to imports and production (indirect taxes): general government :- ESA 2010",
"Current taxes on income and wealth (direct taxes): general government :- ESA 2010",
"Net social contributions received: general government :- ESA 2010",
"Actual social contributions received: general government :- ESA 2010",
#"Imputed social contributions: general government :- ESA 2010",
"Gross disposable income, general government :- ESA 2010",
"Net disposable income: general government :- ESA 2010",
"Current tax burden: total economy :- ESA 2010",
"Total tax burden excluding imputed social security contributions: total economy :- ESA 2010",
"Total tax burden including imputed social security contributions: total economy :- ESA 2010",
#"Other current revenue: general government :- ESA 2010",
#"Other current revenue including sales: general government :- ESA 2010",
"Total current revenue: general government :- ESA 2010",
"Gross saving: general government :- ESA 2010",
"Net saving: general government :- ESA 2010",
#"Capital transfers received: general government :- ESA 2010",
"Capital taxes: general government :- ESA 2010",
"Total revenue: general government :- ESA 2010",
"Subsidies: general government :- ESA 2010",
"Social benefits other than social transfers in kind: general government :- ESA 2010",
"Social transfers in kind supplied to households via market producers: general government :- ESA 2010",
"Interest: general government :- ESA 2010",
"Implicit interest rate: general government :- Interest as percent of gross public debt of preceding year Excessive deficit procedure (based on ESA 2010)",
"Final consumption expenditure of general government :- ESA 2010",
"Collective consumption expenditure :- ESA 2010",
"Social transfers in kind :- ESA 2010",
"Compensation of employees: general government :- ESA 2010",
"Intermediate consumption: general government :- ESA 2010",
#"Other current expenditure: general government :- ESA 2010",
"Total current expenditure: general government :- ESA 2010",
"Gross fixed capital formation: general government :- ESA 2010",
#"Other capital expenditure, including capital transfers: general government :- ESA 2010",
#"Capital transfers paid: general government :- ESA 2010",
"Total expenditure: general government :- ESA 2010",
"Real total expenditure of general government, deflator GDP :- ESA 2010",
"Total current expenditure excluding interest: general government :- ESA 2010",
"Total expenditure excluding interest: general government :- ESA 2010",
"Real total expenditure excluding interest of general government, deflator GDP :- ESA 2010",
"Net lending (+) or net borrowing (-): general government :- ESA 2010",
"Net lending (+) or net borrowing (-) excluding interest: general government :- ESA 2010",
"Net lending (+) or net borrowing (-) excluding gross fixed capital formation: general government :- ESA 2010",
"Interest: general government :- Excessive deficit procedure",
"Gross saving: general government :- Excessive deficit procedure",
"Total current expenditure: general government :- Excessive deficit procedure",
"Total expenditure: general government :- Excessive deficit procedure",
"Net lending (+) or net borrowing (-): general government :- Excessive deficit procedure",
"Net lending (+) or net borrowing (-) excluding interest: general government :- Excessive deficit procedure",
"Cyclically adjusted net lending (+) or net borrowing (-) of general government :- Adjustment based on potential GDP Excessive deficit procedure",
"Structural balance of general government :- Adjustment based on potential GDP Excessive deficit procedure",
"Net lending (+) or net borrowing (-) excluding interest of general government adjusted for the cyclical component :- Adjustment based on potential GDP Excessive deficit procedure",
"Structural balance of general government excluding interest :- Adjustment based on potential GDP Excessive deficit procedure",
"Cyclically adjusted total revenue of general government :- Adjustment based on potential GDP ESA 2010",
"Cyclically adjusted total expenditure of general government :- Adjustment based on potential GDP Excessive deficit procedure",
"Total expenditure excluding interest of general government adjusted for the cyclical component :- Adjustment based on potential GDP Excessive deficit procedure",
"Cyclical component of net lending (+) or net borrowing (-) of general government :- Based on potential GDP ESA 2010",
"Cyclical component of revenue of general government :- Based on potential GDP ESA 2010",
"Cyclical component of expenditure of general government :- Based on potential GDP ESA 2010",
#"One-off and other temporary measures: general government",
#"One-off and other temporary measures - revenue: general government",
#"One-off and other temporary measures - expenditure: general government",
#"Discretionary measures current expenditure: general government",
#"Discretionary measures current revenue: general government",
#"Discretionary measures capital expenditure: general government",
#"Discretionary measures capital transfers received: general government",
"Cyclically adjusted net lending (+) or net borrowing (-) of general government :- Adjustment based on trend GDP Excessive deficit procedure",
"Net lending (+) or net borrowing (-) excluding interest of general government adjusted for the cyclical component :- Adjustment based on trend GDP Excessive deficit procedure",
"Cyclically adjusted total revenue of general government :- Adjustment based on trend GDP ESA 2010",
"Cyclically adjusted total expenditure of general government :- Adjustment based on trend GDP Excessive deficit procedure",
"Total expenditure excluding interest of general government adjusted for the cyclical component :- Adjustment based on trend GDP Excessive deficit procedure",
"Cyclical component of net lending (+) or net borrowing (-) of general government :- Based on trend GDP ESA 2010",
"Cyclical component of revenue of general government :- Based on trend GDP ESA 2010",
"Cyclical component of expenditure of general government :- Based on trend GDP ESA 2010",
"General government consolidated gross debt :- Excessive deficit procedure (based on ESA 2010)",
"Impact of the nominal increase of GDP on general government consolidated gross debt :- Excessive deficit procedure (based on ESA 2010)",
"Snow ball effect on general government consolidated gross debt :- Excessive deficit procedure (based on ESA 2010)",
"Stock-flow adjustment on general government consolidated gross debt :- Excessive deficit procedure (based on ESA 2010)",
"General government consolidated gross debt :- Excessive deficit procedure (based on ESA 2010) and former definitions (linked series)",
"General government consolidated gross debt :- Excessive deficit procedure (based on ESA 2010) and former definition (linked series)",
"Private final consumption expenditure at current prices",
"Private final consumption expenditure at 2010 prices",
"Price deflator private final consumption expenditure",
"Private final consumption expenditure at current prices per head of population",
"Actual individual final consumption of households at current prices",
"Actual individual final consumption of households at 2010 prices",
#"Price deflator actual individual final consumption of households",
"Harmonised consumer price index (All-items)",
"National consumer price index (All-items)",
"Final consumption expenditure of general government at current prices",
"Final consumption expenditure of general government at 2010 prices",
#"Price deflator total final consumption expenditure of general government",
"Collective consumption of general government at current prices",
"Collective consumption of general government at 2010 prices",
#"Price deflator collective consumption of general government",
"Individual consumption of general government at current prices",
"Individual consumption of general government at 2010 prices",
#"Price deflator individual consumption of general government",
"Total consumption at current prices",
"Total consumption at 2010 prices",
"Price deflator total consumption",
"Gross fixed capital formation at current prices: total economy",
"Gross fixed capital formation at 2010 prices: total economy",
"Price deflator gross fixed capital formation: total economy",
"Gross fixed capital formation at current prices: general government",
"Gross fixed capital formation at current prices: private sector",
"Net fixed capital formation at current prices: total economy",
"Net fixed capital formation at 2010 prices: total economy",
"Net fixed capital formation at current prices: general government",
"Net fixed capital formation at current prices: private sector",
"Consumption of fixed capital at current prices: total economy",
"Capital consumption at 2010 prices: total economy",
"Consumption of fixed capital at current prices: general government",
"Gross fixed capital formation at current prices: construction",
"Gross fixed capital formation at current prices: dwellings",
"Gross fixed capital formation at current prices: non-residential construction and civil engineering",
"Gross fixed capital formation at current prices: equipment",
"Gross fixed capital formation at current prices: genl products and machinery",
"Gross fixed capital formation at current prices: transport equipment",
"Gross fixed capital formation at current prices: other investment",
"Gross fixed capital formation at 2010 prices: construction",
"Gross fixed capital formation at 2010 prices: dwellings",
"Gross fixed capital formation at 2010 prices: non-residential construction and civil engineering",
"Gross fixed capital formation at 2010 prices: equipment",
"Gross fixed capital formation at 2010 prices: genl products and machinery",
"Gross fixed capital formation at 2010 prices: transport equipment",
"Gross fixed capital formation at 2010 prices: other investment",
"Changes in inventories and acquisitions less disposals of valuables at current prices: total economy",
"Changes in inventories and acquisitions less disposals of valuables at 2010 prices: total economy",
"Gross capital formation at current prices: total economy",
"Gross capital formation at 2010 prices: total economy",
"Gross national saving",
"Gross saving: private sector :- ESA 2010",
"Net national saving",
"Net saving: private sector :- ESA 2010",
"Domestic demand excluding stocks at current prices",
"Domestic demand excluding stocks at 2010 prices",
"Domestic demand including stocks at 2010 prices :- Performance relative to the rest of 24 industrial countries: double export weights : EU-15, TR CH NR US CA JP AU MX and NZ",
"Final demand at current prices",
"Final demand at 2010 prices",
"Gross national income at current prices",
"Gross national income at 2010 reference levels, deflator GDP",
"Gross national income at current prices per head of population",
"Gross domestic product at current prices",
"Gross domestic product at 2010 reference levels",
"Gross domestic product at 2010 reference levels :- Performance relative to the rest of the former EU-15: double export weights",
"Gross domestic product at 2010 reference levels :- Performance relative to the rest of 24 industrial countries: double export weights : EU-15, TR CH NR US CA JP AU MX and NZ",
"Price deflator gross domestic product",
"Price deflator gross domestic product :- Performance relative to the rest of the former EU-15: double export weights",
"Price deflator gross domestic product :- Performance relative to the rest of 24 industrial countries: double export weights : EU-15, TR CH NR US CA JP AU MX and NZ",
"Gross domestic product at current prices per head of population",
"Gross domestic product at 2010 reference levels per head of population",
"Gross domestic product at current prices per hour worked",
"Average annual hours worked per person employed",
"Total annual hours worked: total economy",
"Potential gross domestic product at 2010 reference levels",
"Gap between actual and potential gross domestic product at 2010 reference levels",
"Contribution to the increase of GDP at constant prices of private consumption",
"Contribution to the increase of GDP at constant prices of public consumption",
"Contribution to the increase of GDP at constant prices of gross fixed capital formation",
"Contribution to the increase of GDP at constant prices of domestic demand excluding stocks",
"Contribution to the increase of GDP at constant prices of changes in inventories and acquisitions less disposals of valuables",
"Contribution to the increase of GDP at constant prices of domestic demand including stocks",
"Contribution to the increase of GDP at constant prices of exports of goods and services :- including intra-EU trade",
"Contribution to the increase of GDP at constant prices of final demand :- including intra-EU trade",
"Contribution to the increase of GDP at constant prices of imports of goods and services :- including intra-EU trade",
"Contribution to the increase of GDP at constant prices of the balance of goods and services",
"Contribution to the increase of GDP at constant prices of total consumption",
"Compensation of employees: total economy",
"Nominal compensation per employee: total economy",
"Nominal compensation per employee: total economy :- Performance relative to the rest of the former EU-15: double export weights",
"Nominal compensation per employee: total economy :- Performance relative to the rest of 24 industrial countries: double export weights : EU-15, TR CH NR US CA JP AU MX and NZ",
"Real compensation per employee, deflator GDP: total economy",
"Real compensation per employee, deflator GDP: total economy :- Performance relative to the rest of the former EU-15: double export weights",
"Real compensation per employee, deflator GDP: total economy :- Performance relative to the rest of 24 industrial countries: double export weights : EU-15, TR CH NR US CA JP AU MX and NZ",
"Adjusted wage share: total economy: as percentage of GDP at current prices (Compensation per employee as percentage of GDP at market prices per person employed.)",
"Nominal unit labour costs: total economy (Ratio of compensation per employee to real GDP per person employed.)",
"Nominal unit labour costs: total economy :- Performance relative to the rest of the former EU-15: double export weights (Ratio of compensation per employee to real GDP per person employed.)",
"Nominal unit labour costs: total economy :- Performance relative to the rest of 24 industrial countries: double export weights (Ratio of compensation per employee to real GDP per person employed.)",
"Real unit labour costs: total economy (Ratio of compensation per employee to nominal GDP per person employed.)",
"Real unit labour costs: total economy :- Performance relative to the rest of the former EU-15: double export weights (Ratio of compensation per employee to nominal GDP per person employed.)",
"Real unit labour costs: total economy :- Performance relative to the rest of 24 industrial countries: double export weights (Ratio of compensation per employee to nominal GDP per person employed.)",
"Net capital stock at 2010 prices: total economy",
"Net capital stock at 2010 prices per person employed: total economy :- Capital intensity",
"Net capital stock per unit of gross domestic product at constant prices :- Capital output ratio: total economy",
"Gross domestic product at 2010 reference levels per unit of net capital stock :- Capital productivity",
"Net returns on net capital stock: total economy",
"Total factor productivity: total economy",
"Labour share in total factor productivity: total economy",
"Capital share in total factor productivity: total economy",
#"Labour-capital substitution: total economy",
#"Capital-labour substitution: total economy",
"Marginal efficiency of capital: total economy (Change in GDP at constant market prices of year T per unit of gross fixed capital formation at constant prices of year T-.5.)",
"Exports of goods and services at current prices (National accounts)",
"Exports of goods and services at 2010 prices",
"Exports of goods and services at 2010 prices :- Performance relative to the rest of the former EU-15: double export weights",
"Exports of goods and services at 2010 prices :- Performance relative to the rest of 24 industrial countries: double export weights : EU-15, TR CH NR US CA JP AU MX and NZ",
"Market performance of exports of goods and services on export weighted imports of goods and services :- 36 industrial markets : EU-27, TR CH NR US CA JP AU MX NZ)",
"Export markets: Export weighted imports :- Goods and services at constant prices: 36 industrial markets : EU-27, TR CH NR US CA JP AU MX NZ)",
"Imports of goods and services at current prices (National accounts)",
"Imports of goods and services at 2010 prices",
"Price deflator imports of goods and services",
"Exports of goods at current prices (National accounts)",
"Exports of goods at 2010 prices",
"Price deflator exports of goods",
"Exports of services at current prices (National accounts)",
"Exports of services at 2010 prices",
"Price deflator exports of services",
"Imports of goods at current prices (National accounts)",
"Imports of goods at 2010 prices",
"Price deflator imports of goods",
"Imports of services at current prices (National accounts)",
"Imports of services at 2010 prices",
"Price deflator imports of services",
"Terms of trade goods and services (National accounts)",
"Terms of trade goods (National accounts)",
"Terms of trade services (National accounts)",
"Impact of terms of trade goods and services on real income"
)
  items
}

add.ameco.unit.type = function(dat = ameco) {
  restore.point("add.ameco.unit.type")

  units = unique(ameco$unit)
  units = units[nchar(units)<200]
  #cat(paste0('"',unique(units),'"', collapse=",\n"))


    thousand.euro = c(
"1000 ECU/EUR, Weighted mean of t/t-1 national growth rates (weights: t-1 current prices in ECU/EUR)",
"1000 EURO-BEF",
"1000 EURO-DEM",
"1000 EURO-EEK",
"1000 EURO-IEP",
"1000 EURO-GRD",
"1000 EURO-ESP",
"1000 EURO-FRF",
"1000 EURO-ITL",
"1000 EURO-CYP",
"1000 EURO-LVL",
"1000 EURO-LTL",
"1000 EURO-LUF",
"1000 EURO-MTL",
"1000 EURO-NLG",
"1000 EURO-ATS",
"1000 EURO-PTE",
"1000 EURO-SIT",
"1000 EURO-SKK",
"1000 EURO-FIM",
"1000 ECU/EUR, Standard aggregation",
"1000 EUR",
"(1000 EUR)",
"1000 ECU/EUR, Weighted mean of t/t-1 national growth rates (weights: t-1 current prices in PPS)"
  )
  dat$unit[dat$unit %in% thousand.euro] = "1000 EUR"

  mrd.euro=c(
"Mrd ECU/EUR, Standard aggregation",
"Mrd EURO-BEF",
"Mrd EURO-DEM",
"Mrd EURO-EEK",
"Mrd EURO-IEP",
"Mrd EURO-GRD",
"Mrd EURO-ESP",
"Mrd EURO-FRF",
"Mrd EURO-ITL",
"Mrd EURO-CYP",
"Mrd EURO-LVL",
"Mrd EURO-LTL",
"Mrd EURO-LUF",
"Mrd EURO-MTL",
"Mrd EURO-NLG",
"Mrd EURO-ATS",
"Mrd EURO-PTE",
"Mrd EURO-SIT",
"Mrd EURO-SKK",
"Mrd EURO-FIM",
"Mrd ECU/EUR",
"(Mrd EUR)",
"Mrd ECU/EUR, Weighted mean of t/t-1 national growth rates (weights: t-1 current prices in ECU/EUR)",
"Mrd ECU/EUR, Weighted mean of t/t-1 national growth rates (weights: t-1 current prices in PPS)"
  )
  dat$unit[dat$unit %in% mrd.euro] = "Mrd EUR"

  local.units = c(
"1000 BGN",
"1000 CZK",
"1000 DKK",
"1000 HRK",
"1000 HUF",
"1000 PLN",
"1000 RON",
"1000 SEK",
"1000 GBP",
"1000 MKD",
"1000 ISK",
"1000 TRY",
"1000 ALL",
"1000 NOK",
"1000 CHF",
"1000 USD",
"1000 JPY",
"1000 CAD",
"1000 MXN",
"1000 WON",
"1000 AUD",
"1000 NZD",
"1000 RSD",
"Mrd BGN",
"Mrd CZK",
"Mrd DKK",
"Mrd HRK",
"Mrd HUF",
"Mrd PLN",
"Mrd RON",
"Mrd SEK",
"Mrd GBP",
"Mrd MKD",
"Mrd ISK",
"Mrd TRY",
"Mrd RSD",
"Mrd ALL",
"Mrd NOK",
"Mrd CHF",
"Mrd USD",
"Mrd JPY",
"Mrd CAD",
"Mrd MXN",
"Mrd WON",
"Mrd AUD",
"Mrd NZD"

  )
  dat$local.unit = dat$unit %in% local.units

  index.units = c(
"(EUR: EU-15 = 100)",
"(EUR: EU-28 = 100)",
"(PPS: EU-15 = 100)",
"(PPS: EU-28 = 100)"
  )

  time.index.units = c(
"(2010 = 100), Weighted mean of t/t-1 national growth rates (weights: current prices in ECU/EUR)",
"(National currency: 2010 = 100)",
"(ECU/EUR: 2010 = 100), Weighted mean of t/t-1 national growth rates (weights: current prices in ECU/EUR)",
"ECU/EUR: 2010 = 100",
"(2010 = 100)",
"(2010 = 100), Weighted mean of t/t-1 national growth rates (weights: current prices in PPS)",
"(2005 = 100)",
"(2010 = 100), Weighted mean of t/t-1 national growth rates (weights: t-1 current prices in ECU/EUR)",
"(2010 = 100), Weighted mean of t/t-1 national growth rates (weights: t-1 current prices in PPS)",
"(USD: 2010 = 100)",
"(EUR: 2010 = 100)"
  )


  percentage.units = c(
"(Percentage of gross domestic product at current prices)",
"(Percentage of GDP at current prices (excessive deficit procedure))",
"(Percentage of potential GDP at current prices)",
"(Percentage of trend GDP at current prices)",
"(Percentage of gross domestic product at current prices (excessive deficit procedure))",
"(Percentage of potential gross domestic product at constant prices)",
"(Percentage of trend gross domestic product at constant prices)",
"(Percentage of GDP of preceding year)",
"(Percentage of gross domestic product at constant prices of preceding year)",
"(%)"

  )




  thousand.units = c(
"1000 persons",
"1000 ECU/EUR, Weighted mean of t/t-1 national growth rates (weights: t-1 current prices in ECU/EUR)",
"1000 EURO-BEF",
"1000 BGN",
"1000 CZK",
"1000 DKK",
"1000 EURO-DEM",
"1000 EURO-EEK",
"1000 EURO-IEP",
"1000 EURO-GRD",
"1000 EURO-ESP",
"1000 EURO-FRF",
"1000 HRK",
"1000 EURO-ITL",
"1000 EURO-CYP",
"1000 EURO-LVL",
"1000 EURO-LTL",
"1000 EURO-LUF",
"1000 HUF",
"1000 EURO-MTL",
"1000 EURO-NLG",
"1000 EURO-ATS",
"1000 PLN",
"1000 EURO-PTE",
"1000 RON",
"1000 EURO-SIT",
"1000 EURO-SKK",
"1000 EURO-FIM",
"1000 SEK",
"1000 GBP",
"1000 MKD",
"1000 ISK",
"1000 TRY",
"1000 ALL",
"1000 NOK",
"1000 CHF",
"1000 USD",
"1000 JPY",
"1000 CAD",
"1000 MXN",
"1000 WON",
"1000 AUD",
"1000 NZD",
"1000 ECU/EUR, Standard aggregation",
"1000 EUR",
"(1000 EUR)",
"1000 ECU/EUR, Weighted mean of t/t-1 national growth rates (weights: t-1 current prices in PPS)",
"1000 RSD",
"1000 PPS, Standard aggregation",
"(1000 PPS)"

  )
  large.units=c(
"Mrd ECU/EUR, Standard aggregation",
"Mrd EURO-BEF",
"Mrd BGN",
"Mrd CZK",
"Mrd DKK",
"Mrd EURO-DEM",
"Mrd EURO-EEK",
"Mrd EURO-IEP",
"Mrd EURO-GRD",
"Mrd EURO-ESP",
"Mrd EURO-FRF",
"Mrd HRK",
"Mrd EURO-ITL",
"Mrd EURO-CYP",
"Mrd EURO-LVL",
"Mrd EURO-LTL",
"Mrd EURO-LUF",
"Mrd HUF",
"Mrd EURO-MTL",
"Mrd EURO-NLG",
"Mrd EURO-ATS",
"Mrd PLN",
"Mrd EURO-PTE",
"Mrd RON",
"Mrd EURO-SIT",
"Mrd EURO-SKK",
"Mrd EURO-FIM",
"Mrd SEK",
"Mrd GBP",
"Mrd MKD",
"Mrd ISK",
"Mrd TRY",
"Mrd EUR",
"Mrd RSD",
"Mrd ALL",
"Mrd NOK",
"Mrd CHF",
"Mrd USD",
"Mrd JPY",
"Mrd CAD",
"Mrd MXN",
"Mrd WON",
"Mrd AUD",
"Mrd NZD",
"Mrd ECU/EUR",
"(Mrd EUR)",
"Mrd ECU/EUR, Weighted mean of t/t-1 national growth rates (weights: t-1 current prices in ECU/EUR)",
"Mrd ECU/EUR, Weighted mean of t/t-1 national growth rates (weights: t-1 current prices in PPS)",
"Mrd PPS, Standard aggregation",
"(Mrd PPS)"
  )

  mio.units = c(
    "(millions)"
  )

  ignore.units = c(
"(PPS: EU-15 = 100)",
"(PPS: EU-28 = 100)",
"Mrd PPS, Standard aggregation",
"(Mrd PPS)"
  )
  rows = which(!dat$unit %in% ignore.units)
  dat = dat[rows,]



  unit.type = ifelse(nchar(dat$unit)>200,"weird","no")
  unit.type[dat$unit %in% index.units] = "index"
  unit.type[dat$unit %in% time.index.units] = "time.index"
  unit.type[dat$unit %in% percentage.units] = "percentage"
  unit.type[dat$unit %in% thousand.units] = "1000"
  unit.type[dat$unit %in% large.units] = "large"
  unit.type[dat$unit %in% mio.units] = "mio"

  dat$unit.type = unit.type

  # add per capita units
  pc = filter(dat, unit.type == "large")

  repl.fun = function(x){
    x = gsub("Mrd ","",x,fixed = TRUE)
    x
  }
  pc = mutate(pc,
    value = 1000*value / pop_mio,
    unit = repl.fun(unit),
    title = paste0(title, " per capita"),
    unit.type = "1000pc"
  )
  dat = rbind(dat,pc)

  dat
}

load.main.data.ameco.pq = function(data.dir=dataquiz.data.dir(), update=FALSE) {
  restore.point("load.main.data.ameco.pq")

  file = file.path(data.dir, "ameco_pq.rds")
  if (file.exists(file) & !update) {
    dat = readRDS(file)
    return(dat)
  }

  library(ameco)
  dat = na.omit(ameco)

  items = get.ameco.items()
  dat = filter(dat, title %in% items)

  my.fun = function(x) {
    if (length(x)==0) return(NA)
    x
  }
  dat = group_by(dat, year, cntry) %>%
    mutate(
      pop_mio = my.fun(value[title=="Total population"]) / 1000)

  # item code:
  # extract the value after the last .
  regex = "[:alnum:]+$"
  dat$item_code = str_match(dat$code, regex)


  dat = add.ameco.unit.type(dat)
  #cat(paste0('"',sort(unique(dat$title)),'"', collapse=",\n"))




  saveRDS(dat, file)
  dat
}


load.gen.data.ameco.pq = function(gen, data.dir=dataquiz.data.dir()) {
  restore.point("load.gen.data.ameco.pq")

  setwd(data.dir)
  # save data for specific countries
  files = paste0("ameco_pq_",gen$countries,".rds")
  exists = file.exists(files)

  if (all(exists)) {
    li = lapply(files, readRDS)
    dat = bind_rows(li)
  } else {
    dat = load.main.data.ameco.pq()
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

