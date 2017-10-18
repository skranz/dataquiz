country.names.examples = function() {
  cntry = unique(df$cntry)
  country.code.to.name(cntry)
}

country.code.to.name = function(cntry, code=c("iso3c","iso2c")[1]) {
  library(countrycode)
  countries = countrycode(cntry,origin="iso3c",destination = "country.name",warn=FALSE)
  rows = is.na(countries)
  countries[rows] = cntry[rows]
  countries[countries=="United Kingdom of Great Britain and Northern Ireland"] = "United Kingdom"
  countries[countries=="Taiwan, Province of China"] = "Taiwan"
  countries
}
