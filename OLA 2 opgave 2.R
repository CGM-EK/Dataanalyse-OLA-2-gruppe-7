#loader pakker til brug i opgaven
library(tidyverse)
library(ggplot2)
library(readxl)

#vi henter data fra Danmarks statistik
forbrugerforv <- dst_meta(table = "FORV1", lang = "da")

#vi udvælger variabler vi vil kigge på og opretter et dataset
forbrugerforv_meta_filters <- list(
  INDIKATOR = "*",
  Tid = "*"
)
f.tillid1 <- dst_get_data(table = "FORV1", query = forbrugerforv_meta_filters, lang = "da")
f.tillid1 <- f.tillid1 %>% filter(TID >="2000-01-01")

f.tillid <- pivot_wider(
  data = f.tillid1,
  names_from = INDIKATOR,
  values_from = value)

#vi henter data fra Danmarks statistik
p.forbrugss <- dst_meta(table = "NKN1", lang = "da")

#vi udvælger variabler vi vil kigge på og opretter et dataset
pforbrug_meta_filters <- list(
  TRANSAKT = "P.31 Privatforbrug",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)
p.forbrug1 <- dst_get_data(table = "NKN1", query = pforbrug_meta_filters, lang = "da")
p.forbrug <- p.forbrug1 %>% filter(TID >="1999-01-01")

f.tillid <- pivot_wider(
  data = f.tillid1,
  names_from = INDIKATOR,
  values_from = value)

#kvartalersekvenser opsættes
kvartalseq1 <- seq(1,304, 3)
kvartalseq2 <- seq(2,305, 3)
kvartalseq3 <- seq(3,306, 3)

#variabler defineres for DI og DST's forbrugertillidsindikatorer i f.tillid
f.tillid$sammenlagtDI <- c((f.tillid$`F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
                              f.tillid$`F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
                              f.tillid$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`+
                              f.tillid$`F10 Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`)/4)

f.tillid$sammenlagtDST <- c((f.tillid$`F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
                               f.tillid$`F3 Familiens økonomiske  situation om et år, sammenlignet med i dag`+
                               f.tillid$`F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
                               f.tillid$`F5 Danmarks økonomiske situation om et år, sammenlignet med i dag`+
                               f.tillid$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)/5)

#kvartalsekvenser anvendes på forbrugertillidsindikatorerne og der oprettes dataframes
kvartalerDIft1 <- f.tillid$sammenlagtDI[kvartalseq1]
kvartalerDIft2 <- f.tillid$sammenlagtDI[kvartalseq2]
kvartalerDIft3 <- f.tillid$sammenlagtDI[kvartalseq3]
forbrugertillidDI <- as.data.frame(c((kvartalerDIft1+kvartalerDIft2+kvartalerDIft3)/3))

kvartalerDSTft1 <- f.tillid$sammenlagtDST[kvartalseq1]
kvartalerDSTft2 <- f.tillid$sammenlagtDST[kvartalseq2]
kvartalerDSTft3 <- f.tillid$sammenlagtDST[kvartalseq3]
forbrugertillidDST <- as.data.frame((kvartalerDSTft1+kvartalerDSTft2+kvartalerDSTft3)/3)

#der oprettes en vektorer for kvartalerne fra k1 2000 til k2 2025
year <- seq.Date(from = as.Date("2000-01-01"),
                      to = as.Date("2025-06-30"),
                      by = "quarter")
f.tillidsammen <- as.data.frame(year)

#Der oprettes en vektor for den årlige kvartalvise realvækst for privatforbruget, som indsættes i dataframes
P.forbrugvaekst <- c(0, diff(log(p.forbrug$Privatforbrug),lag=4)*100)
f.tillidsammen$pfv <- P.forbrugvaekst[-1]
f.tillidsammen$f.tillidDI <- forbrugertillidDI$`c((kvartalerDIft1 + kvartalerDIft2 + kvartalerDIft3)/3)`
f.tillidsammen$f.tillidDST <- forbrugertillidDST$`(kvartalerDSTft1 + kvartalerDSTft2 + kvartalerDSTft3)/3`

#lineære modeller for dst og di's forbrugertillidsindikatorer med fitted values og korrelation
lm.test.di <- lm(f.tillidsammen$pfv~f.tillidsammen$f.tillidDI)
summary(lm.test.di)
fitted.lm.test.di <- lm.test.di$fitted.values
cor(fitted.lm.test.di,f.tillidsammen$pfv)

lm.test.dst <- lm(f.tillidsammen$pfv~f.tillidsammen$f.tillidDST)
summary(lm.test.dst)
fitted.lm.test.dst <- lm.test.dst$fitted.values
cor(fitted.lm.test.dst,f.tillidsammen$pfv)

#plots for at tjekke data
plot(f.tillidsammen$pfv, ylim = c(-8,8), type = "l", xaxt = "n", xlab = "year", ylab ="Årlig realvækst pr. kvartal i privat forbruget")
#indsætter årstal på x-aksen
axis(side = 1, at = seq(1, 106, by = 4), labels = paste("", 0:26))

#plotter talene for 2016 for at sammenligne med grafen for DI
dfsammen2016 <- as.data.frame(f.tillidsammen[1:66,])
plot(dfsammen2016$pfv, ylim = c(-8,8), type = "l", xaxt = "n", xlab = "year", ylab ="Årlig realvækst pr. kvartal i privat forbruget")
#indsætter årstal på x-aksen
axis(side = 1, at = seq(1, 66, by = 4), labels = paste("", 0:16))


plot(fitted.lm.test.di, type = "l")
plot(fitted.lm.test.di, ylim = c(-8,8), type = "l", xaxt = "n", xlab = "year", ylab ="DI's forbrugertillidsindikator")
axis(side = 1, at = seq(1, 106, by = 4), labels = paste("", 0:26))

#for at få grafen til at se flottere ud vælger vi at gøre gennemsnittet af værdierne
#i forbrugertillidsindikatoren til 0
indikatorplusDI <- sum(f.tillid$sammenlagtDI)/nrow(f.tillid)
vplus <- c(rep(indikatorplusDI,102))
f.tillidsammen$modeltalDI <- f.tillidsammen$f.tillidDI+(vplus*-1)

indikatorplusDST <- sum(f.tillid$sammenlagtDST)/nrow(f.tillid)
vplusDST <- c(rep(indikatorplusDST,102))
f.tillidsammen$modeltalDST <- f.tillidsammen$f.tillidDST+(vplusDST*-1)

#plot for DI's ftillid og pfv 2000 til 2025 2.k
ggplot(data = f.tillidsammen, aes(x=year))+
  geom_bar(aes(y=pfv*3.125, color = "Privatforbrugets kvartalvise realvækst"), fill = "steelblue", stat = "identity", width = 75)+
  geom_line(aes(y=modeltalDI, color = "DI's forbrugertillidsindikator"), size = 1.2)+
  geom_line(aes(y=modeltalDST, color = "DST's forbrugertillidsindikator"), size = 1.2)+
  scale_y_continuous(
    name = "Nettotal Forbrugertillidsindikator",
    limits = c(-40,30),
    sec.axis = sec_axis(~./3.125, name ="Realvækst Privatforbrug")
  ) +scale_color_manual(
    name = "",  # Overskrift for linjerne
    values = c("DI's forbrugertillidsindikator" = "orange", "DST's forbrugertillidsindikator" = "brown","Privatforbrugets kvartalvise realvækst" = "steelblue")
  )+
  theme_minimal()+ labs(title = "DI's forbrugertillidsindikator følger i højere grad privatforbruget", caption = "Kilde:https://www.statistikbanken.dk/NKN1\nKilde:https://www.statistikbanken.dk/FORV1")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")+
  scale_x_date(name = "Year", breaks = f.tillidsammen$year[seq(1, length(f.tillidsammen$year), by = 4)],
               labels =format(f.tillidsammen$year[seq(1, length(f.tillidsammen$year), by = 4)], "%Y"))+
  geom_hline(yintercept = 0, linetype = "solid", linewidth = 0.1, color = "black")

#2016 ftillid DI for at kontrollere at det stemmer overens med DI's graf
indikatorplus2016DI <- sum(dfsammen2016$f.tillidDI)/nrow(dfsammen2016)
vplus2016DI <- c(rep(indikatorplus2016DI,66))
dfsammen2016$modeltal <- (dfsammen2016$f.tillidDI)+(vplus2016DI*-1)
ggplot(data = dfsammen2016, aes(x=year))+
  geom_col(aes(y=pfv*3.125), fill = "steelblue")+
  geom_line(aes(y=modeltal), size = 0.7)+
  scale_y_continuous(
    name = "Nettotal",
    limits = c(-25,25),
    sec.axis = sec_axis(~./3.125, name ="PCT")
  )+ theme_minimal()+ labs(title = "DI's forbrugertillidsindikator følger i højere grad privatforbruget")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_date(breaks = dfsammen2016$year[seq(1, length(dfsammen2016$year), by = 4)],
               labels = format(dfsammen2016$year[seq(1, length(dfsammen2016$year), by = 4)], "%Y"))

#forudsigelser for privatforbrug k3 fra DI og DST's Forbrugertillidsindikator
forbrugertillid_3_kvartal <- read_excel("R/R projekter/forbrugertillid 3.kvartal.xlsx")
DI3k <- c((f.tillid$`F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden`[307:309]+
             f.tillid$`F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[307:309]+
             f.tillid$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[307:309]+
             f.tillid$`F10 Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[307:309])/4)
DI3ksam <- sum(DI3k)/3

DST3k <- c((f.tillid$`F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden`[307:309]+
              f.tillid$`F3 Familiens økonomiske  situation om et år, sammenlignet med i dag`[307:309]+
              f.tillid$`F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[307:309]+
              f.tillid$`F5 Danmarks økonomiske situation om et år, sammenlignet med i dag`[307:309]+
              f.tillid$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[307:309])/5)
DST3ksam <- sum(DST3k)/3

DI3kpfv <- 2.09628+0.20139*DI3ksam

DST3kpfv <- 1.19684+0.18502*DST3ksam

################
#plot for privatforbruget alene
pfvplotdf <- f.tillidsammen[73:102,]

ggplot(data = pfvplotdf, aes(x=year))+
  geom_bar(aes(y=pfv), fill = "steelblue", stat = "identity")+
  geom_line(aes(y=p.forbrug/p.forbrug[1]*100-100), size = 1.2)+
  theme_minimal()+ labs(title = "Privatforbruget har haft fremgang de seneste kvartaler")+
  theme(axis.text.x = element_text(size = 15,angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_date(name = "Årstal", breaks = f.tillidsammen$year[seq(1, length(f.tillidsammen$year), by = 4)],
               labels = format(f.tillidsammen$year[seq(1, length(f.tillidsammen$year), by = 4)], "%Y"))+
  scale_y_continuous(name = "Årlige kvartalvise realvækst",limits = c(-10,12), breaks = seq(from = -10, to = 10, by = 2),
                     sec.axis = sec_axis(~(.+100)/100*pfvplotdf$p.forbrug[1], name ="Privatforbruget i hele milliader kr."))+
  geom_hline(yintercept = 0, linetype = "solid", linewidth = 0.1, color = "black")

total.pforbrug2024 <- sum(p.forbrug$value[101:104])
total.pforbrug2025DST <- sum(p.forbrug$value[105:106])+p.forbrug$value[103]*((100-DST3kpfv)/100)
total.pforbrug2025DI <- sum(p.forbrug$value[105:106])+p.forbrug$value[103]*((100-DI3kpfv)/100)
manglendepforbrug2025DST <- total.pforbrug2024*1.02-total.pforbrug2025DST
manglendepforbrug2025DI <- total.pforbrug2024*1.02-total.pforbrug2025DI
sofarpforbrug2025 <- sum(p.forbrug$value[105:106])

forvDI <- p.forbrug$value[103]*((100+DI3kpfv)/100)
forvDST <- p.forbrug$value[103]*((100+DST3kpfv)/100)

ggplot(data = f.tillidsammen, aes(x=year))+
  geom_line(aes(y=modeltalDST, color = "DST's forbrugertillidsindikator"), size = 1.2)+
  theme_minimal()+
  scale_color_manual(
    name = "",  # Overskrift for linjerne
    values = c("DST's forbrugertillidsindikator" = "orange"))+
  labs(title = "Det er tydeligt at forbrugertilliden falder i krisetider", ylab = "forbrugertillidsindikator")+
  scale_y_continuous(
    name = "Forbrugertillidsindikatoren",
    limits = c(-35,20))+
  scale_x_date(name = "Årstal", breaks = f.tillidsammen$year[seq(1, length(f.tillidsammen$year), by = 4)],
               labels = format(f.tillidsammen$year[seq(1, length(f.tillidsammen$year), by = 4)], "%Y"))+
  theme(axis.text.x = element_text(size = 10,angle = 90, vjust = 0.5, hjust = 1))+
  geom_hline(yintercept = 0, linetype = "solid", linewidth = 0.1, color = "black")
  

