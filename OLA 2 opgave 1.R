#pakker loades til brug i opgaven
library(danstat)
library(dkstat)
library(mapDK)
library(tidyverse)
#testtest
#vi henter data fra Danmarks statistik
dkbefolkningpost <- dst_meta(table = "POSTNR2", lang = "da")

#vi udvælger variabler vi vil kigge på og opretter et dataset
dkbefolk_meta_filters <- list(
  PNR20 = "*",
  Tid = "2025"
)
befolkningsdata <- dst_get_data(table = "POSTNR2", query = dkbefolk_meta_filters, lang = "da")
befolkningsdatacl <- befolkningsdata


#opretter ny kolonne med postnummer ud fra datasettet
befolkningsdatacl$mzip <- sub(".* - ", "", befolkningsdata$PNR20) #fjerner alt før "-"
befolkningsdatacl$mzip <- as.numeric(gsub("[^0-9]","", befolkningsdatacl$mzip)) #fjerner alt på nær tal

#opretter ny kolonne med bynavn ud fra datasettet
befolkningsdatacl$by <- sub(".* - ", "", befolkningsdata$PNR20) #fjerner alt før "-"
befolkningsdatacl$by <- gsub("[-^0-9]","", befolkningsdatacl$by) #fjerner alle tal
befolkningsdatacl$by <- gsub(" ","", befolkningsdatacl$by) #fjerner mellemrum før udtryk

#data fra "Hele landet" fra dataframen
befolkningsdatacl <- befolkningsdatacl %>% filter(by!="Helelandet")

#vi fjerner alle observationer hvor der ikke er nogle beboere
befolkningsdatacl <- as.data.frame(subset(befolkningsdatacl, befolkningsdatacl$value != 0))

############################################
#vi indlæser datafil for boliger
boligcl2 <- readRDS("~/R/R projekter/boligcl2.rds")

#laver et nyt dataset til at arbejde i
boligcl22 <- boligcl2

#opretter ny prisvariabel i kr. med kun tal
boligcl22$prisny <- as.numeric(gsub("[^0-9]","", boligcl22$pris))

#opretter en ny variabel med kvm/pris
boligcl22$kvmpris <- boligcl22$prisny/boligcl22$kvm2

#oprettter et dataframe med totale befolkningstal pr by
combinedby <- befolkningsdatacl %>% 
  group_by(by) %>% 
  summarise(total=sum(value),.groups = "drop")

#vi merger for at få befolkningstal for byerne ind i dataframet
mergedbolig2 <- left_join(befolkningsdatacl, combinedby, by = "by")

#vi merger vores to dataset på postnummer
mergedbolig <- inner_join(mergedbolig2, boligcl22, by = "mzip")

#vi laver en ny dataframe uden dublikater ud fra boligID
mergedclean <- mergedbolig %>% distinct(mergedbolig$bolig_id, .keep_all = TRUE)

#vi undersøger befolkningstalet fra de forskellige byer
summary(mergedclean$total)

#vi definerer vores breaks og labels til den nye bykategorivariabel
mybreaks=c(160000,40000,10000, 2500, 1000,0)
mylabs=c("Landsby (<1.000)","Lille by (1.001-2.500)", "Almindelig by (2.501-10.000)", "Større by (10.001-40.000)", "Storby (40.001+)")

#den nye kategorivariabel defineres
mergedclean$Bykategori <- cut(mergedclean$total, labels = mylabs, breaks = mybreaks)

#vi laver så den endelige dataframe hvor vi har vores by, pris, kvmpris, og bykategori
boliger1.4 <- mergedclean[,c(5,8,24,27)]

#vi laver en ny dataframe der indeholder den gennemsnitlige kvmpris for bykategorien
combinedkvm <- boliger1.4 %>% 
  group_by(Bykategori) %>% 
  summarise(kvmpris=mean(kvmpris),.groups = "drop")

#vi ploter den gennemsnitlige kvmpris for bykategorierne
ggplot(data = combinedkvm, aes(x=Bykategori, y=kvmpris, fill = Bykategori))+
  geom_bar(stat = "identity")+
  theme_minimal()+ theme(legend.position = "none") +
  labs(title = "Kvadratmeterpriser stiger med indbyggertal", caption = "Kilde:https://www.statistikbanken.dk/POSTNR2\nKilde:https://www.boligsiden.dk/")+ 
                        ylab("Gennemsnitlig kvadratmeterpris i Kr.")+ xlab("Bykategori baseret på indbyggertal")
  

#############################
co <- boligcl22 %>% 
  group_by(type) %>% 
  summarise(kvmpris=mean(kvmpris),.groups = "drop")

ggplot(data = boligcl22, aes(x=type, y=kvmpris), color = type, group = type)+
  geom_bar(stat="identity")

