install.packages("restatapi")
library(restatapi)
install.packages("eurostat")
library(eurostat)

toc<-get_eurostat_toc()

mySearches<-function(searchstr) {
  searchstr="consumption"
  tmpdf<-toc %>% filter(str_detect(title,searchstr)) %>%
    filter(type=="dataset") %>%
    select(title,code)
  return(tmpdf)
}
list <- mySearches("expenditure")

dfconsum <- get_eurostat_data("namq_10_fcs",
                              filters = list(
                              geo=c("DK","BE","NL","SE","AT","DE","FR","IT","ES"),
                              na_item="P31_S14",
                              s_adj="NSA",
                              unit="CLV20_MEUR"),
date_filter=">1999",
label=TRUE,
name=FALSE)

pivotland <- pivot_wider(
  data = dfconsum,
  names_from = geo,
  values_from = values
)
landyear <- seq.Date(from = as.Date("2000-01-01"),
                   to = as.Date("2025-06-30"),
                   by = "quarter")

landpfv <- as.data.frame(landyear)

names(landpfv)[names(landpfv) == "lmyear"] <- "year"
landpfv$PfvAT <- c(0, diff(log(pivotland$Austria),lag=4)*100)[-1]
landpfv$PfvBE <- c(0, diff(log(pivotland$Belgium),lag=4)*100)[-1]
landpfv$PfvGE <- c(0, diff(log(pivotland$Germany),lag=4)*100)[-1]
landpfv$PfvDK <- c(0, diff(log(pivotland$Denmark),lag=4)*100)[-1]
landpfv$PfvES <- c(0, diff(log(pivotland$Spain),lag=4)*100)[-1]
landpfv$PfvFR <- c(0, diff(log(pivotland$France),lag=4)*100)[-1]
landpfv$PfvIT <- c(0, diff(log(pivotland$Italy),lag=4)*100)[-1]
landpfv$PfvNL <- c(0, diff(log(pivotland$Netherlands),lag=4)*100)[-1]
landpfv$PfvSE <- c(0, diff(log(pivotland$Sweden),lag=4)*100)[-1]

mean(landpfv$PfvAT)
mean(landpfv$PfvBE)
mean(landpfv$PfvGE)
mean(landpfv$PfvDK)
mean(landpfv$PfvES)
mean(landpfv$PfvFR)
mean(landpfv$PfvIT)
mean(landpfv$PfvNL)
mean(landpfv$PfvSE)

#81-89

landpfvu <-  as.data.frame(landpfv[-c(81:89),])

mean(landpfvu$PfvAT)
mean(landpfvu$PfvBE)
mean(landpfvu$PfvGE)
mean(landpfvu$PfvDK)
mean(landpfvu$PfvES)
mean(landpfvu$PfvFR)
mean(landpfvu$PfvIT)
mean(landpfvu$PfvNL)
mean(landpfvu$PfvSE)

landpfvm <-  as.data.frame(landpfv[c(81:89),])

mean(landpfvm$PfvAT)
mean(landpfvm$PfvBE)
mean(landpfvm$PfvGE)
mean(landpfvm$PfvDK)
mean(landpfvm$PfvES)
mean(landpfvm$PfvFR)
mean(landpfvm$PfvIT)
mean(landpfvm$PfvNL)
mean(landpfvm$PfvSE)
