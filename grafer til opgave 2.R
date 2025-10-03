f.tillid <- Forbrugertillidsindikator2000_2025_OLA2
p.forbrug <- Privatforbrug_1999_2025

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

pfvplotdf$p.forbrug <- Privatforbrug_1999_2025$Privatforbrug[77:106]

ggplot(data = pfvplotdf, aes(x=year))+
  geom_line(aes(y=p.forbrug/p.forbrug[1]*100-100))+
  theme_minimal()+ labs(title = "Privatforbruget har haft fremgang de seneste kvartaler")
2

ggplot(data = pfvplotdf, aes(x=year))+
  geom_bar(aes(y=pfv),fill="steelblue", stat="identity")+
  labs(colorp.forbruglabs(color ="Linjer", fill = "søjler"))

summary(pfvplotdf$p.forbrug)
280.1/269.6*100-100
(3.884659+100)/100*269.6
p.forbrug/p.forbrug[1]*100-100

ggplot(data = pfvplotdf, aes(x=year))+
  geom_line(aes(y=f.tillidDST), size = 1.2)+
  theme_minimal()+ labs(title = "DST's forbrugertillidsindikator har været negativ siden 2022 men måske med udsigt til stigning")+
  scale_x_date(name = "Årstal", breaks = f.tillidsammen$year[seq(1, length(f.tillidsammen$year), by = 4)],
               labels = format(f.tillidsammen$year[seq(1, length(f.tillidsammen$year), by = 4)], "%Y"))+
  scale_y_continuous(name = "DST's forbrugertillidsindikator",limits = c(-35,10), breaks = seq(from = -40, to = 10, by = 5))+
  geom_hline(yintercept = 0, linetype = "solid", linewidth = 0.1, color = "black")
