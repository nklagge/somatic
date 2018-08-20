library(dplyr)
library(httr)
library(jsonlite)
library(purrr)

query_markets_data <- function(stub) {
  arglist <- list()
  url <- "http://markets.newyorkfed.org/api/"
  url %>%
    paste0(stub, ".json") %>%
    GET() %>%
    content(as = "text") %>%
    fromJSON(simplifyVector = FALSE)
}

#marked in API documentation as non-public, but is accessible
#seems to be the only source of PD timeseries definitions
#add parsing
k <- query_markets_data("pd/get/menu")

k <- query_markets_data("pd/list/asof")
j <- k %>%
  pluck("pd", "asofdates") %>%
  bind_rows()



#parsing error
#k <- query_markets_data("pd/list/seriesbreaks")

k <- query_markets_data("pd/list/timeseries")
j <- k %>%
  pluck("pd", "timeseries") %>%
  bind_rows()

# summary holdings
k <- query_markets_data("soma/summary")
j <- k %>%
  pluck("soma", "summary") %>%
  bind_rows() %>%
  mutate_at(-1, as.numeric) %>%
  mutate_at(1, as.Date)

#mbs releases
k <- query_markets_data("soma/mbs/list/release")
j <- k %>%
  pluck("soma", "mbs") %>%
  unlist(use.names = FALSE)
t <- j[420]
stub <- paste0("soma/mbs/get/release/", t)
k <- query_markets_data(stub)
j <- k %>%
  pluck("soma", "mbs") %>%
  bind_rows() %>%
  mutate_at("asOfDate", as.Date) %>%
  mutate_at("currentFaceValue", as.numeric)

#pretty duplicative with list/release
k <- query_markets_data("soma/mbs/list/asof")
j <- k %>%
  pluck("soma", "mbs") %>%
  unlist(use.names = FALSE)
#for instance
t <- j[420]
stub <- paste0("soma/mbs/get/asof/", t)
k <- query_markets_data(stub)
j <- k %>%
  pluck("soma", "mbs") %>%
  bind_rows() %>%
  mutate_at("asOfDate", as.Date) %>%
  mutate_at("currentFaceValue", as.numeric)
x <- j$cusip[400]
stub <- paste0("soma/mbs/get/cusip/", x)
k <- query_markets_data(stub)
j <- k %>%
  pluck("soma", "mbs") %>%
  bind_rows() %>%
  mutate_at("asOfDate", as.Date) %>%
  mutate_at("currentFaceValue", as.numeric)

s#doesn't seem super important
k <- query_markets_data("soma/mbs/get/release_log")
j <- k %>%
  pluck("soma", "mbs")

#does not work - xml works
#k <- query_markets_data("soma/mbs/get/quarterly")
#does not work - xml works
#k <- query_markets_data("soma/non-mbs/get/monthly")

k <- query_markets_data("soma/non-mbs/list/release")
j <- k %>%
  pluck("soma", "nonmbs") %>%
  unlist(use.names = FALSE)
#for instance
t <- j[420]
#ALL, BILLS, NOTESBONDS, FRN, TIPS, AGENCIES
stub <- paste0("soma/non-mbs/get/ALL/release/", t)
k <- query_markets_data(stub)
j <- k %>%
  pluck("soma", "nonmbs") %>%
  bind_rows() %>%
  mutate_at(c("asOfDate", "maturityDate"), as.Date) %>%
  mutate_at(c("spread", "coupon", "parValue", "inflationCompensation",
              "percentOutstanding", "changeFromPriorWeek",
              "changeFromPriorYear"), as.numeric)

k <- query_markets_data("soma/non-mbs/list/asof")
j <- k %>%
  pluck("soma", "nonmbs") %>%
  unlist(use.names = FALSE)
#for instance
t <- j[420]
#ALL, BILLS, NOTESBONDS, FRN, TIPS, AGENCIES
stub <- paste0("soma/non-mbs/get/ALL/asof/", t)
k <- query_markets_data(stub)
j <- k %>%
  pluck("soma", "nonmbs") %>%
  bind_rows() %>%
  mutate_at(c("asOfDate", "maturityDate"), as.Date) %>%
  mutate_at(c("spread", "coupon", "parValue", "inflationCompensation",
              "percentOutstanding", "changeFromPriorWeek",
              "changeFromPriorYear"), as.numeric)
#for instance
x <- j$cusip[110]
stub <- paste0("soma/non-mbs/get/cusip/", x)
#throws an error about undocumented exceptionOnEmpty argument
k <- query_markets_data(stub)
