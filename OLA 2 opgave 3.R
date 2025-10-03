#loader pakker til brug i opgaven
library(readxl)

f.tillid <- read_excel("R/R projekter/Forbrugertillidsindikator2000_2025 OLA2.xlsx")

p.forbrug <- read_excel("R/R projekter/D-dsheli-main/uge 37 forbrugertillid/Privatforbrug 1999-2025.xlsx", 
                        sheet = "Ark1")

#oprettelse af faktorvariabel der viser om det kvartalvise årlige vækst er steget eller faldet
year <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2025-06-30"),
                 by = "quarter")
Dfopg3 <- as.data.frame(year)

P.forbrugvaekst <- c(0, diff(log(p.forbrug$Privatforbrug),lag=4)*100)
Dfopg3$pfv <- P.forbrugvaekst[-1]
Dfopg3$YN <- as.factor(ifelse(Dfopg3$pfv >=0, "1", "0"))
table(Dfopg3$YN)
# no yes 
# 24 78 

############################################
#kvartalersekvenser opsættes
kvartalseq1 <- seq(1,304, 3)
kvartalseq2 <- seq(2,305, 3)
kvartalseq3 <- seq(3,306, 3)

#variabler defineres for DI og DST's forbrugertillidsindikatorer i f.tillid
f.tillid$sammenlagtDI <- c((f.tillid$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
                              f.tillid$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
                              f.tillid$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`+
                              f.tillid$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`)/4)

f.tillid$sammenlagtDST <- c((f.tillid$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
                               f.tillid$`Familiens økonomiske  situation om et år, sammenlignet med i dag`+
                               f.tillid$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
                               f.tillid$`Danmarks økonomiske situation om et år, sammenlignet med i dag`+
                               f.tillid$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)/5)

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

##
#indsætter de fittede værdier fra forbrugertillidsindikatorerne fra DST og DI i dataframen
Dfopg3$DI.fitted <- fitted.lm.test.di
Dfopg3$DST.fitted <- fitted.lm.test.dst


Dfopg3$YNDI <- as.factor(ifelse(Dfopg3$DI.fitted >=0, "1", "0"))
table(Dfopg3$YNDI)
#no  Yes 
#20  82 

Dfopg3$YNDST <- as.factor(ifelse(Dfopg3$DST.fitted >=0, "1", "0"))
table(Dfopg3$YNDST)
#no  Yes
#18  84 

glm.DIPFV <- glm(formula = Dfopg3$YN~Dfopg3$YNDI, family = "binomial")
summary(glm.DIPFV)

pred_probsDI <- predict(glm.DIPFV, type = "response")
threshold <- 0.5
pred_classDI <- ifelse(pred_probsDI > threshold,1, 0)
table(predicted =pred_classDI, actual = Dfopg3$YN)

glm.DSTPFV <- glm(formula = Dfopg3$YN~Dfopg3$YNDST, family = "binomial")
summary(glm.DSTPFV)

pred_probsDST <- predict(glm.DSTPFV, type = "response")
pred_classDST <- ifelse(pred_probsDST > threshold,1, 0)
table(predicted =pred_classDST, actual = Dfopg3$YN)

#########################

forbrugertillid_meta_filters <- list(
  INDIKATOR = "*",
  Tid = "*"
)
forbrugertillidspgs <- dst_get_data(table = "FORV1", query = forbrugertillid_meta_filters, lang = "da")
befolkningsdatacl <- befolkningsdata
table(forbrugertillidspgs$INDIKATOR)

vurderingsdf <- as.data.frame(year)
vurderingsdf$fam.sit.bag <- fam.sit.ift.bag
vurderingsdf$fam.sit.frem <- fam.sit.frem
vurderingsdf$dk.sit.bag <- dk.sit.bag
vurderingsdf$dk.sit.frem <- dk.sit.frem
vurderingsdf$an.str.fbg.fd <- an.str.fbg.fd
vurderingsdf$an.str.fbg.n12 <- an.str.fbg.n12

vurderingsdf$fam.sit.bagYN <- as.factor(ifelse(vurderingsdf$fam.sit.bag >=0, "1", "0"))
vurderingsdf$fam.sit.fremYN <- as.factor(ifelse(vurderingsdf$fam.sit.frem >=0, "1", "0"))
vurderingsdf$dk.sit.bagYN <- as.factor(ifelse(vurderingsdf$dk.sit.bag >=0, "1", "0"))
vurderingsdf$dk.sit.fremYN <- as.factor(ifelse(vurderingsdf$dk.sit.frem >=0, "1", "0"))
vurderingsdf$an.str.fbg.fdYN <- as.factor(ifelse(vurderingsdf$an.str.fbg.fd >=0, "1", "0"))
vurderingsdf$an.str.fbg.n12YN <- as.factor(ifelse(vurderingsdf$an.str.fbg.n12 >=0, "1", "0"))

nydfopned <- forbrugertillidspgs %>% filter(TID>="2000-01-01")
nydfopned$valueopned <- as.factor(ifelse(nydfopned$value >=0, "yes", "no"))

ggplot(data = nydfopned, aes(x=INDIKATOR, y=n, fill = INDIKATOR))+
  geom_bar(stat = "identity",position = "dodge")

df_counts <- nydfopned %>%
  group_by(INDIKATOR, valueopned) %>%
  summarise(n = n())

df_count_yes <- df_counts %>% filter(valueopned=="yes")
df_count_no <- df_counts %>% filter(valueopned=="no")
ggplot(data = df_count_yes, aes(x=INDIKATOR, y=n, fill = INDIKATOR))+
  geom_bar(stat = "identity", position = "dodge")
ggplot(data = df_count_no, aes(x=INDIKATOR, y=n, fill = INDIKATOR))+
  geom_bar(stat = "identity", position = "dodge")
