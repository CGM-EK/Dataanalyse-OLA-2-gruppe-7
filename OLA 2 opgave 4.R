#loader pakker til brug i opgaven
library(readxl)
library(danstat)
library(dkstat)
library(mapDK)
library(tidyverse)
#loader data for forbrugertillid og privatforbrug til brug i opgaven
f.tillid <- read_excel("R/R projekter/Forbrugertillidsindikator2000_2025 OLA2.xlsx", 
                       sheet = "Ark1")
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

#laver et gennemsnit på vektoren
mean(f.tillid$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)
#-10.53072



year <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2025-06-30"),
                 by = "quarter")
plotdata <- as.data.frame(year)

#kvartalersekvenser opsættes
kvartalseq1 <- seq(1,304, 3)
kvartalseq2 <- seq(2,305, 3)
kvartalseq3 <- seq(3,306, 3)

#kvartalsekvenser anvendes på forbrugertillidsindikatorerne og der oprettes dataframes
kvartalerplot1 <- f.tillid$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[kvartalseq1]
kvartalerplot2 <- f.tillid$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[kvartalseq2]
kvartalerplot3 <- f.tillid$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[kvartalseq3]
plotdata$ansk <- c((kvartalerplot1+kvartalerplot2+kvartalerplot3)/3)

ggplot(data = plotdata, aes(x=year, y = ansk, color = "Anskaffelse af større forbrugsgoder"))+
  geom_line(size = 1.5)+
  geom_point(size = 2)+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_date(name = "Year", breaks = plotdata$year[seq(1, length(plotdata$year), by = 4)],
               labels = format(plotdata$year[seq(1, length(plotdata$year), by = 4)], "%Y"))+
  theme_minimal()+
  scale_y_continuous(name = "Anskaffelse af større forbrugsgoder",
                     limits = c(-50,5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")+
  scale_color_manual(
    name = "",  # Overskrift for linjerne
    values = c("Anskaffelse af større forbrugsgoder" = "orange"))+
  labs(title = "Forbrugerne svarer generelt negativt omkring anskaffelse af større forbrugsgoder", caption = "Kilde:https://www.statistikbanken.dk/FORV1")
gnsansk <- mean(plotdata$ansk)
########################3

forbruglist <- dst_meta(table = "NAHC021", lang = "da")

dkforbrug_meta_filters <- list(
  FORMAAAL = c("Fødevarer mv.",
                "Drikkevarer og tobak mv.",
                "Beklædning og fodtøj",
                "Boligbenyttelse",
                "Elektricitet, fjernvarme og andet brændsel",
                "Boligudstyr, husholdningstjenester mv.",
                "Medicin, lægeudgifter o.l.",
                "Køb af køretøjer",
                "Drift af køretøjer og transporttjenester",
                "Information og kommunikation",
                "Fritid, sport og kultur",
                "Undervisning",
                "Restauranter og hoteller",
                "Forsikring og finansielle tjenester",
                "Andre varer og tjenester"),
  PRISENHED = "2020-priser, kædede værdier",
  TID = "*"
)

forbrugsdata <- dst_get_data(table = "NAHC021", query = dkforbrug_meta_filters, lang = "da")

forbrugsdata1 <- forbrugsdata %>% filter(TID >="2000-01-01")

#laver en ny vektor med tallene i tusinder
forbrugsdata1$modeltal <- forbrugsdata1$value/1000

#filtre til opdeling i forbrugsgrupper

madogforbrug <- forbrugsdata1 %>% filter(FORMAAAL == "CPA Fødevarer mv."|
                                           FORMAAAL == "CPB Drikkevarer og tobak mv."|
                                           FORMAAAL == "CPM Restauranter og hoteller")

boligoghusholdning <- forbrugsdata1 %>% filter(FORMAAAL == "CPD Boligbenyttelse"|
                                           FORMAAAL == "CPE Elektricitet, fjernvarme og andet brændsel"|
                                           FORMAAAL == "CPF Boligudstyr, husholdningstjenester mv.")

sundhedogpersonligomsorg <- forbrugsdata1 %>% filter(FORMAAAL == "CPG Medicin, lægeudgifter o.l.")

transport <- forbrugsdata1 %>% filter(FORMAAAL == "CPH Køb af køretøjer"|
                                                 FORMAAAL == "CPI Drift af køretøjer og transporttjenester")

fritiduddannelseogøk<- forbrugsdata1 %>% filter(FORMAAAL == "CPK Fritid, sport og kultur"|
                                                  FORMAAAL == "CPL Undervisning"|
                                                  FORMAAAL == "CPN forsikring og finansielle tjenester"|
                                                  FORMAAAL == "CPJ Information og kommunikation")


ggplot(data = madogforbrug, aes(x=TID, y=modeltal, group = FORMAAAL, color = FORMAAAL))+
  geom_line()

ggplot(data = forbrugsdata2, aes(x=TID, y=modeltal, group = FORMAAAL, color = FORMAAAL))+
  geom_line()

dfopg33 <- forbrugsdata1[c(1,3,4)]

dfopg333 <- dfopg33 %>% filter(TID == "2000-01-01"|TID =="2024-01-01")

dfopg3333 <- dfopg333 %>% 
  group_by(FORMAAAL) %>% 
  summarise(value=diff(value),.groups = "drop")
dfopg3333$pctforskel <- dfopg333 %>% 
  group_by(FORMAAAL) %>% 
  summarise(value=diff(value),.groups = "drop")

#vi laver plottet igen men med indekstal for at gøre udviklingen visuelt sammenlignelig
forbrugsgoder_2020_2025 <- read_excel("R/forbrugsgoder 2020-2025.xlsx", 
                                      sheet = "Ark1")
dfindeks <- forbrugsgoder_2020_2025 %>% mutate(indeks = row_number())

forbrugsgoder_2020_2025 <- forbrugsgoder_2020_2025 %>%
  mutate(across(where(is.numeric),
                ~ .x / first(.x) * 100,
                .names = "{.col}_idx"))   



#setup til lineære regressioner
lmyear <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2024-10-01"),
                 by = "year")

lmtestdf <- as.data.frame(lmyear)

names(lmtestdf)[names(lmtestdf) == "lmyear"] <- "year"

forbrugertilliddilm <- as.data.frame(f.tillidsammen$year[1:100])
forbrugertilliddilm$f.tillidDI <- f.tillidsammen$f.tillidDI[1:100]
forbrugertilliddilm$f.tillidDST <- f.tillidsammen$f.tillidDST[1:100]

#kvartalersekvenser opsættes
kvartalseq1lm<- seq(1,97, 4)
kvartalseq2lm <- seq(2,98, 4)
kvartalseq3lm <- seq(3,99, 4)
kvartalseq4lm <- seq(4,100,4)
#kvartalsekvenser anvendes på forbrugertillidsindikatorerne og der oprettes dataframes
kvartalerlm1DI <- forbrugertilliddilm$f.tillidDI[kvartalseq1lm]
kvartalerlm2DI <- forbrugertilliddilm$f.tillidDI[kvartalseq2lm]
kvartalerlm3DI <- forbrugertilliddilm$f.tillidDI[kvartalseq3lm]
kvartalerlm4DI <- forbrugertilliddilm$f.tillidDI[kvartalseq4lm]
lmtestdf$DIft <- c((kvartalerlm1DI+kvartalerlm2DI+kvartalerlm3DI+kvartalerlm4DI)/4)

kvartalerlm1DST <- forbrugertilliddilm$f.tillidDST[kvartalseq1lm]
kvartalerlm2DST <- forbrugertilliddilm$f.tillidDST[kvartalseq2lm]
kvartalerlm3DST <- forbrugertilliddilm$f.tillidDST[kvartalseq3lm]
kvartalerlm4DST <- forbrugertilliddilm$f.tillidDST[kvartalseq4lm]
lmtestdf$DSTft <- c((kvartalerlm1DST+kvartalerlm2DST+kvartalerlm3DST+kvartalerlm4DST)/4)

pivottabel <- pivot_wider(
  data = forbrugsdata1,
  names_from = FORMAAAL,
  values_from = value
)
names(pivottabel)[names(pivottabel) == "TID"] <- "year"
names(lmtestdf)[names(lmtestdf) == "lmyear"] <- "year"
lmtestdfjoined <- left_join(lmtestdf, pivottabel, "year")

#lineære regressioner
lm.test.kat1DI <- lm(lmtestdfjoined$`CPA Fødevarer mv.`~lmtestdfjoined$DIft)
summary(lm.test.kat1DI)
lm.test.kat1DST <- lm(lmtestdfjoined$`CPA Fødevarer mv.`~lmtestdfjoined$DSTft)
summary(lm.test.kat1DST)

lm.test.kat2DI <- lm(lmtestdfjoined$`CPB Drikkevarer og tobak mv.`~lmtestdfjoined$DIft)
summary(lm.test.kat2DI)
lm.test.kat2DST <- lm(lmtestdfjoined$`CPB Drikkevarer og tobak mv.`~lmtestdfjoined$DSTft)
summary(lm.test.kat2DST)

lm.test.kat3DI <- lm(lmtestdfjoined$`CPC Beklædning og fodtøj`~lmtestdfjoined$DIft)
summary(lm.test.kat3DI)
lm.test.kat3DST <- lm(lmtestdfjoined$`CPC Beklædning og fodtøj`~lmtestdfjoined$DSTft)
summary(lm.test.kat3DST)

lm.test.kat4DI <- lm(lmtestdfjoined$`CPD Boligbenyttelse`~lmtestdfjoined$DIft)
summary(lm.test.kat4DI)
lm.test.kat4DST <- lm(lmtestdfjoined$`CPD Boligbenyttelse`~lmtestdfjoined$DSTft)
summary(lm.test.kat4DST)

lm.test.kat5DI <- lm(lmtestdfjoined$`CPE Elektricitet, fjernvarme og andet brændsel`~lmtestdfjoined$DIft)
summary(lm.test.kat5DI)
lm.test.kat5DST <- lm(lmtestdfjoined$`CPE Elektricitet, fjernvarme og andet brændsel`~lmtestdfjoined$DSTft)
summary(lm.test.kat5DST)

lm.test.kat6DI <- lm(lmtestdfjoined$`CPF Boligudstyr, husholdningstjenester mv.`~lmtestdfjoined$DIft)
summary(lm.test.kat6DI)
lm.test.kat6DST <- lm(lmtestdfjoined$`CPF Boligudstyr, husholdningstjenester mv.`~lmtestdfjoined$DSTft)
summary(lm.test.kat6DST)

lm.test.kat7DI <- lm(lmtestdfjoined$`CPG Medicin, lægeudgifter o.l.`~lmtestdfjoined$DIft)
summary(lm.test.kat7DI)
lm.test.kat7DST <- lm(lmtestdfjoined$`CPG Medicin, lægeudgifter o.l.`~lmtestdfjoined$DSTft)
summary(lm.test.kat7DST)

lm.test.kat8DI <- lm(lmtestdfjoined$`CPH Køb af køretøjer`~lmtestdfjoined$DIft)
summary(lm.test.kat8DI)
lm.test.kat8DST <- lm(lmtestdfjoined$`CPH Køb af køretøjer`~lmtestdfjoined$DSTft)
summary(lm.test.kat8DST)

lm.test.kat9DI <- lm(lmtestdfjoined$`CPI Drift af køretøjer og transporttjenester`~lmtestdfjoined$DIft)
summary(lm.test.kat9DI)
lm.test.kat9DST <- lm(lmtestdfjoined$`CPI Drift af køretøjer og transporttjenester`~lmtestdfjoined$DSTft)
summary(lm.test.kat9DST)

lm.test.kat10DI <- lm(lmtestdfjoined$`CPJ Information og kommunikation`~lmtestdfjoined$DIft)
summary(lm.test.kat10DI)
lm.test.kat10DST <- lm(lmtestdfjoined$`CPJ Information og kommunikation`~lmtestdfjoined$DSTft)
summary(lm.test.kat10DST)

lm.test.kat11DI <- lm(lmtestdfjoined$`CPK Fritid, sport og kultur`~lmtestdfjoined$DIft)
summary(lm.test.kat11DI)
lm.test.kat11DST <- lm(lmtestdfjoined$`CPK Fritid, sport og kultur`~lmtestdfjoined$DSTft)
summary(lm.test.kat11DST)

lm.test.kat12DI <- lm(lmtestdfjoined$`CPL Undervisning`~lmtestdfjoined$DIft)
summary(lm.test.kat12DI)
lm.test.kat12DST <- lm(lmtestdfjoined$`CPL Undervisning`~lmtestdfjoined$DSTft)
summary(lm.test.kat12DST)

lm.test.kat13DI <- lm(lmtestdfjoined$`CPM Restauranter og hoteller`~lmtestdfjoined$DIft)
summary(lm.test.kat13DI)
lm.test.kat13DST <- lm(lmtestdfjoined$`CPM Restauranter og hoteller`~lmtestdfjoined$DSTft)
summary(lm.test.kat13DST)

lm.test.kat14DI <- lm(lmtestdfjoined$`CPN Forsikring og finansielle tjenester`~lmtestdfjoined$DIft)
summary(lm.test.kat14DI)
lm.test.kat14DST <- lm(lmtestdfjoined$`CPN Forsikring og finansielle tjenester`~lmtestdfjoined$DSTft)
summary(lm.test.kat14DST)

lm.test.kat15DI <- lm(lmtestdfjoined$`CPO Andre varer og tjenester`~lmtestdfjoined$DIft)
summary(lm.test.kat15DI)
lm.test.kat15DST <- lm(lmtestdfjoined$`CPO Andre varer og tjenester`~lmtestdfjoined$DSTft)
summary(lm.test.kat15DST)

