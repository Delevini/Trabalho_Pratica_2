
rmv.cha <- function(x) iconv(x, to = "ASCII//TRANSLIT")

### Mapas dos Indicadores

library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggpubr)
library(GGally)
library(readxl)
library(tidyverse)
library(geobr)
library(patchwork)
library(tmap)
library(plotly)
library(DT)
library(sf)

dadoscams2023<-read_csv2("daily_pm25_all_regions_2023.csv")
dadoscams2022<-read_csv2("daily_pm25_all_regions_2022.csv")

dadosdon2023 <- read_xlsx("Donkelar_dados_completos_consolidado_2023.xlsx")  
dadosdon2022 <- read_xlsx("Donkelar_dados_completos_consolidado_2022.xlsx")  

dadosdon2022 = dadosdon2022 %>% filter(SIGLA_UF == "ES")
dadosdon2023 = dadosdon2023 %>% filter(AREA_KM2 == "ES")
################################################################################################
### Ano 2022
### PM2.5 diario
long22=melt(dadoscams2022, id.vars="Date")
colnames(long22)=c("Date","CodRes","pm2.5")
long22$Cod=substring(long22$CodRes, 4,10)
long22$Cod2=substring(long22$Cod, 1,6)
long22$UF=substring(long22$Cod, 1,2)
long22$Date2=ymd(long22$Date)

long22$ano=year(long22$Date2)
long22$month=month(long22$Date2)

str(long22)
summary(long22$pm2.5)
table(long22$ano)

#PM2.5 anual
pm2.5anual22<-long22%>%group_by(Cod)%>%summarise(pm2.5=mean(pm2.5,na.rm=T))

summary(pm2.5anual22$pm2.5)

pm2.5anual22$Miss<-is.na(pm2.5anual22$pm2.5)
pm2.5anual22$cod6=as.integer(substring(pm2.5anual22$Cod, 1,6))

pm2.5anual22 = pm2.5anual22 |> rename("CD_MUN" = Cod)

Anual22 = left_join(dadosdon2022,pm2.5anual22, by = "CD_MUN") |> select(1,2,13) |> unique()

########Ano 2023

### PM2.5 diario
long=melt(dadoscams2023, id.vars="Date")
colnames(long)=c("Date","CodRes","pm2.5")
long$Cod=substring(long$CodRes, 4,10)
long$Cod2=substring(long$Cod, 1,6)
long$UF=substring(long$Cod, 1,2)
long$Date2=dmy(long$Date)

long$ano=year(long$Date2)
long$month=month(long$Date2)

str(long)
summary(long$pm2.5)
table(long$ano)

#PM2.5 anual
pm2.5anual<-long%>%group_by(Cod)%>%summarise(pm2.5=mean(pm2.5,na.rm=T))
summary(pm2.5anual$pm2.5)

pm2.5anual$Miss<-is.na(pm2.5anual$pm2.5)
pm2.5anual$cod6=as.integer(substring(pm2.5anual$Cod, 1,6))
dadosdon2023$SIGLA_UF = dadosdon2022$SIGLA_UF  
dadosdon2023$AREA_KM2 = dadosdon2022$AREA_KM2

pm2.5anual = pm2.5anual |> rename("CD_MUN" = Cod)

dadosdon2023$CD_MUN = as.character(dadosdon2023$CD_MUN)

Anual = left_join(dadosdon2023,pm2.5anual, by = "CD_MUN") |> select(1,2,13) |> unique()

long = long |> filter(UF == "32")
long22 = long22 |> filter(UF == "32")
###########################################################################################
#long(23/22) refere-se aos dados diários do modelo CAMS por dia
#anual(23/22) é o resumo média de cada municipio seguindo o modelo CAMS
#dadosdon(23/22) é o método donkelar


cams23 = left_join(long |> select(-c(1:2,Cod2,ano)) |> rename("CD_MUN" = Cod) ,
                   dadosdon2022 |> select(1:4),
                   by = "CD_MUN") |> unique()

cams22 = left_join(long22 |> select(-c(1:2,Cod2,ano)) |> rename("CD_MUN" = Cod) ,
                   dadosdon2022 |> select(1:4),
                   by = "CD_MUN") |> unique()

#cams(22/23) agora se referindo ao modelo CAMS por dia

#BOXPLOT CAMS  por mês

boxcams23 = cams23 |> 
  ggplot(mapping = aes(y = pm2.5,
                       x = factor(month))) +
  geom_boxplot() +
  labs(x = "Mês",
       y = "PM2.5") +
  stat_summary(fun = mean, 
               geom = "point", 
               size = 2, 
               color = "green")+
  stat_summary(fun = median, 
               geom = "point", 
               size = 2, 
               color = "red")+
  theme_minimal()

boxcams23

boxcams22 = cams22 |> 
  ggplot(mapping = aes(y = pm2.5,
                       x = factor(month))) +
  geom_boxplot() +
  labs(x = "Mês",
       y = "PM2.5") +
  stat_summary(fun = mean, 
               geom = "point", 
               size = 2, 
               color = "green")+
  stat_summary(fun = median, 
               geom = "point", 
               size = 2, 
               color = "red")+
  theme_minimal()

boxcams22

boxcams22 / boxcams23

#Box modelo Donkelar por ano
boxdonk22 = dadosdon2022 |> 
  ggplot(mapping = aes(y = Media_PM25,
                       x = factor(Mes))) +
  geom_boxplot() +
  labs(x = "Mês",
       y = "PM2.5") +
  stat_summary(fun = mean, 
               geom = "point", 
               size = 2, 
               color = "green")+
  stat_summary(fun = median, 
               geom = "point", 
               size = 2, 
               color = "red")+
  theme_minimal()

boxdonk23 = dadosdon2023 |> 
  ggplot(mapping = aes(y = Media_PM25,
                       x = factor(Mes))) +
  geom_boxplot() +
  labs(x = "Mês",
       y = "PM2.5") +
  stat_summary(fun = mean, 
               geom = "point", 
               size = 2, 
               color = "green")+
  stat_summary(fun = median, 
               geom = "point", 
               size = 2, 
               color = "red")+
  theme_minimal()

boxdonk22 / boxdonk23 


donk22  = dadosdon2022 |> select("pm2.5" = Media_PM25, CD_MUN, "month" = Mes) |> mutate(fonte = "Donkelar")
cams22 = cams22 |> group_by(month,CD_MUN) |> summarise(pm2.5 = mean(pm2.5)) |> mutate(fonte = "cams")

compara22 = rbind(donk22,cams22)

box22 = compara22 |> 
  ggplot(mapping = aes(y = pm2.5,
                       x = factor(month),
                       fill = fonte)) +
  geom_boxplot() +
  stat_summary(fun = mean, 
               geom = "point", 
               size = 2, 
               color = "green",
               position = position_dodge(width = 0.75))+
  stat_summary(fun = median, 
               geom = "point", 
               size = 2, 
               color = "red",
               position = position_dodge(width = 0.75))+
  theme_minimal()+
  labs(x = "Mês",
       y = "PM2.5",
       title = "Ano de 2022",
       caption = "ponto verde: Média | Ponto vermelho: Mediana")

box22
#

donk23  = dadosdon2023 |> select("pm2.5" = Media_PM25, CD_MUN, "month" = Mes) |> mutate(fonte = "Donkelar")
cams23 = cams23 |> group_by(month,CD_MUN) |> summarise(pm2.5 = mean(pm2.5)) |> mutate(fonte = "cams")

compara23 = rbind(donk23,cams23)

box23 = compara23 |> 
  ggplot(mapping = aes(y = pm2.5,
                       x = factor(month),
                       fill = fonte)) +
  geom_boxplot() +
  stat_summary(fun = mean, 
               geom = "point", 
               size = 2, 
               color = "green",
               position = position_dodge(width = 0.75))+
  stat_summary(fun = median, 
               geom = "point", 
               size = 2, 
               color = "red",
               position = position_dodge(width = 0.75))+
  theme_minimal()+
  labs(x = "Mês",
       y = "PM2.5",
       title = "Ano de 2023",
       caption = "ponto verde: Média | Ponto vermelho: Mediana")

box23

box22 / box23
######################################################################################

mapa <- read_municipality(code_muni = "ES", year = 2024)

ggplot() +
  geom_sf(data = mapa, fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Mapa do Estado do Espirito Santo") +
  coord_sf(
    xlim = c(-42, -39.5),  
    ylim = c(-21.5, -17.5) 
  ) +
  geom_sf_text(data = mapa, aes(label = name_muni), size = 2.5, color = "black")


ES23 = left_join(mapa,Anual, by = c("name_muni" = "NM_MUN")) |> mutate(Ano = "2023")
ES22 = left_join(mapa,Anual22, by = c("name_muni" = "NM_MUN")) |> mutate(Ano = "2022")
EScams = rbind(ES22,ES23)

anualdonk22 = donk22 |> group_by(CD_MUN) |> mutate(pm_2.5 = mean(pm2.5)) |> select(2,5) |> unique() |> rename("pm2.5" = pm_2.5)
anualdonk23 = donk23 |> group_by(CD_MUN) |> mutate(pm_2.5 = mean(pm2.5)) |> select(2,5) |> unique() |> rename("pm2.5" = pm_2.5)

anualdonk22 = left_join(anualdonk22, Anual22, by = "CD_MUN") |> select(1:3)
anualdonk23 = left_join(anualdonk23, Anual22, by = "CD_MUN") |> select(1:3)

ES23donk = left_join(mapa,anualdonk23, by = c("name_muni" = "NM_MUN")) |> mutate(Ano = "2023")
ES22donk = left_join(mapa,anualdonk22, by = c("name_muni" = "NM_MUN")) |> mutate(Ano = "2022")

ESdonk = rbind(ES22donk,ES23donk) |> rename("pm2.5" = pm2.5.x)

diff = abs(EScams$pm2.5 - ESdonk$pm2.5)
ESdiff = ESdonk |> select(-pm2.5) |> mutate(pm2.5 = diff)

#ESdonk, EScams e ESdiff referem-se ao valor anual tanto no ano de de 22 quanto no ano de 23

breaks <- c(0, 5, 10, 15, 20,25,30,35, Inf )
legendas <- c(
  "0-5: Ideal",
  "5-10: Bom",
  "10-15: Aceitável",
  "15-20: Moderado",
  "20-25: Ruim",
  "25-30: Muito Ruim",
  "30-35: Perigoso",
  "35+: Crítico"
)

EScams <- EScams %>%
  mutate(
    pm25_categoria = cut(
      pm2.5,
      breaks = breaks,
      labels = legendas,
      right = FALSE, 
      include.lowest = TRUE
    )
  )
  
ESdonk <- ESdonk %>%
  mutate(
    pm25_categoria = cut(
      pm2.5,
      breaks = breaks,
      labels = legendas,
      right = FALSE, # Inclui o valor da esquerda no intervalo [0, 5), [5, 10), etc.
      include.lowest = TRUE
    )
  )  
  
ESdiff <- ESdiff %>%
  mutate(
    pm25_categoria = cut(
      pm2.5,
      breaks = breaks,
      labels = legendas,
      right = FALSE, # Inclui o valor da esquerda no intervalo [0, 5), [5, 10), etc.
      include.lowest = TRUE
    )
  )  

cores_personalizadas <- c(
  "0-5: Ideal" = "#4CAF50",       
  "5-10: Bom" = "#8BC34A",       
  "10-15: Aceitável" = "#CDDC39", 
  "15-20: Moderado" = "#FFEB3B",  
  "20-25: Ruim" = "#FFC107",       
  "25-30: Muito Ruim" = "#FF9800", 
  "30-35: Perigoso" = "#F44336",   
  "35+: Crítico" = "#B71C1C"      
)

map_cams = ggplot(data = EScams) +
  geom_sf(mapping = aes(fill = pm25_categoria)) +
  facet_wrap(~Ano) +
  scale_fill_manual(
    name = "Níveis de PM 2.5", 
    values = cores_personalizadas,
  ) +
  theme_void() +
  theme(legend.title = element_text(size = 16,
                                    colour = "Red",
                                    face = "bold"),
        legend.text = element_text(size = 10,
                                   colour = "Red")) +
  coord_sf(
    xlim = c(-42, -39.5),  
    ylim = c(-21.5, -17.5) 
  ) +
  labs(title = "CAMS") #+
  #geom_sf_text(data = mapa, aes(label = name_muni), size = 2.5, color = "black")


map_donk = ggplot(data = ESdonk) +
  geom_sf(mapping = aes(fill = pm25_categoria)) +
  facet_wrap(~Ano) +
  scale_fill_manual(
    name = "Níveis de PM 2.5", 
    values = cores_personalizadas
  ) +
  theme_void() +
  theme(legend.title = element_text(size = 16,
                                    colour = "Red",
                                    face = "bold"),
        legend.text = element_text(size = 10,
                                   colour = "Red")) +
  coord_sf(
    xlim = c(-42, -39.5),  
    ylim = c(-21.5, -17.5) 
  ) +
  labs(title = "DONKELAR") #+
  #geom_sf_text(data = mapa, aes(label = name_muni), size = 2.5, color = "black")

map_diff = ggplot(data = ESdiff) +
  geom_sf(mapping = aes(fill = pm25_categoria)) +
  facet_wrap(~Ano) +
  scale_fill_manual(
    name = "Níveis de PM 2.5", 
    values = cores_personalizadas,
  ) +
  theme_void() +
  theme(legend.title = element_text(size = 16,
                                    colour = "Red",
                                    face = "bold"),
        legend.text = element_text(size = 10,
                                   colour = "Red")) +
  coord_sf(
    xlim = c(-42, -39.5),  
    ylim = c(-21.5, -17.5) 
  ) +
  labs(title = "diferença")

map_diff

plot_mapa = map_donk / map_cams / map_diff
plot_mapa

media_cams = EScams |> group_by(name_muni) |> summarise(pm2.5 = mean(pm2.5)) 
media_donk = ESdonk |> group_by(name_muni) |> summarise(pm2.5 = mean(pm2.5)) 
media_diff = ESdiff |> group_by(name_muni) |> summarise(pm2.5 = mean(pm2.5)) 

ggplot(data = media_diff) +
  geom_sf(mapping = aes(fill = pm2.5)) +
  scale_fill_gradient(name = "PM 2.5", 
                      low = "White", 
                      high = "Red") +
  theme_void() +
  theme(legend.title = element_text(size = 16,
                                    colour = "Red",
                                    face = "bold"),
        legend.text = element_text(size = 10,
                                   colour = "Red")) +
  coord_sf(
    xlim = c(-42, -39.5),  
    ylim = c(-21.5, -17.5) 
  ) +
  labs(title = "media das diferença")

ggplot(data = media_cams) +
  geom_sf(mapping = aes(fill = pm2.5)) +
  scale_fill_gradient(name = "PM 2.5", 
                      low = "White", 
                      high = "Red") +
  theme_void() +
  theme(legend.title = element_text(size = 16,
                                    colour = "Red",
                                    face = "bold"),
        legend.text = element_text(size = 10,
                                   colour = "Red")) +
  coord_sf(
    xlim = c(-42, -39.5),  
    ylim = c(-21.5, -17.5) 
  ) +
  labs(title = "media do cams")

ggplot(data = media_donk) +
  geom_sf(mapping = aes(fill = pm2.5)) +
  scale_fill_gradient(name = "PM 2.5", 
                      low = "White", 
                      high = "Red") +
  theme_void() +
  theme(legend.title = element_text(size = 16,
                                    colour = "Red",
                                    face = "bold"),
        legend.text = element_text(size = 10,
                                   colour = "Red")) +
  coord_sf(
    xlim = c(-42, -39.5),  
    ylim = c(-21.5, -17.5) 
  ) +
  labs(title = "media do Donkelar")

#### Descritiva ####
Descri = ESdonk |> select(Ano,"Municipio" = name_muni) |> mutate(pm2.5_donk = ESdonk$pm2.5,
                                            pm2.5_cams = EScams$pm2.5,
                                            pm2.5_diff = ESdiff$pm2.5)

tabela_resumo <- Descri %>%
  group_by(Ano) %>%
  summarise(
    media_donk = mean(pm2.5_donk, na.rm = TRUE),
    desvio_padrao_donk = sd(pm2.5_donk, na.rm = TRUE),
    max_donk = max(pm2.5_donk, na.rm = TRUE),
    municipio_max_donk = Municipio[which.max(pm2.5_donk)],
    min_donk = min(pm2.5_donk, na.rm = TRUE),
    municipio_min_donk = Municipio[which.min(pm2.5_donk)],
    media_cams = mean(pm2.5_cams, na.rm = TRUE),
    desvio_padrao_cams = sd(pm2.5_cams, na.rm = TRUE),
    max_cams = max(pm2.5_cams, na.rm = TRUE),
    municipio_max_cams = Municipio[which.max(pm2.5_cams)],
    min_cams = min(pm2.5_cams, na.rm = TRUE),
    municipio_min_cams = Municipio[which.min(pm2.5_cams)],
    media_diff = mean(pm2.5_diff, na.rm = TRUE),
    desvio_padrao_diff = sd(pm2.5_diff, na.rm = TRUE),
    max_diff = max(pm2.5_diff, na.rm = TRUE),
    municipio_max_diff = Municipio[which.max(pm2.5_diff)],
    min_diff = min(pm2.5_diff, na.rm = TRUE),
    municipio_min_diff = Municipio[which.min(pm2.5_diff)],
    .groups = 'drop' 
  )

tabela_resumo$geom <- NULL

tabela_resumo = t(tabela_resumo)
colnames(tabela_resumo) = tabela_resumo[1,]
tabela_resumo = tabela_resumo[-1,]



########################################### FIM








#### parte 2 ####

dispersao22 = compara22 |> pivot_wider(names_from = fonte,
                                       values_from = pm2.5)

ggplot(dispersao22, aes(x = Donkelar, y = cams)) +
  geom_point(size = 3, alpha = 0.7, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    title = "Comparação de Métodos de Medição de PM2.5",
    subtitle = "Cada ponto representa um mês",
    x = "PM2.5 Donkelar",
    y = "PM2.5 Cams"
  ) +
  theme_bw()

dispersao23 = compara23 |> pivot_wider(names_from = fonte,
                                       values_from = pm2.5)

ggplot(dispersao23, aes(x = Donkelar, y = cams)) +
  geom_point(size = 3, alpha = 0.7, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    title = "Comparação de Métodos de Medição de PM2.5",
    subtitle = "Cada ponto representa um mês",
    x = "PM2.5 Donkelar",
    y = "PM2.5 Cams"
  ) +
  theme_bw()


#correlação de Pearson e de Spearman
cor(dispersao22$Donkelar,dispersao22$cams)
cor.test(dispersao22$Donkelar,dispersao22$cams)
cor.test(dispersao22$Donkelar,dispersao22$cams, method = "spearman")

cor(dispersao23$Donkelar,dispersao23$cams)
cor.test(dispersao23$Donkelar,dispersao23$cams)
cor.test(dispersao23$Donkelar,dispersao23$cams, method = "spearman")

bland22 <- dispersao22 %>%
  mutate(
    media = (Donkelar + cams) / 2,
    diferenca = Donkelar - cams
  )

bland23 <- dispersao23 %>%
  mutate(
    media = (Donkelar + cams) / 2,
    diferenca = Donkelar - cams
  )

media_diff22 <- mean(bland22$diferenca, na.rm = TRUE)
sd_diff22 <- sd(bland22$diferenca, na.rm = TRUE)
limite_superior22 <- media_diff22 + (1.96 * sd_diff22)
limite_inferior22 <- media_diff22 - (1.96 * sd_diff22)

media_diff23 <- mean(bland22$diferenca, na.rm = TRUE)
sd_diff23 <- sd(bland22$diferenca, na.rm = TRUE)
limite_superior23 <- media_diff23 + (1.96 * sd_diff23)
limite_inferior23 <- media_diff23 - (1.96 * sd_diff23)

ggplot(bland22, aes(x = media, y = diferenca)) +
  geom_point(alpha = 0.6) +
  geom_hline(
    yintercept = media_diff22, 
    color = "blue", 
    linetype = "dashed", 
    linewidth = 1
  ) +
  geom_hline(
    yintercept = limite_superior22, 
    color = "red", 
    linetype = "dotted", 
    linewidth = 1
  ) +
  geom_hline(
    yintercept = limite_inferior22, 
    color = "red", 
    linetype = "dotted", 
    linewidth = 1
  ) +
  labs(
    title = "Gráfico de Bland-Altman: Comparação Donkelar vs. cams",
    x = "Média das Medições ((Donkelar + cams) / 2)",
    y = "Diferença das Medições (Donkelar - cams)"
  ) +
  theme_bw()

ggplot(bland23, aes(x = media, y = diferenca)) +
  geom_point(alpha = 0.6) +
  geom_hline(
    yintercept = media_diff23, 
    color = "blue", 
    linetype = "dashed", 
    linewidth = 1
  ) +
  geom_hline(
    yintercept = limite_superior23, 
    color = "red", 
    linetype = "dotted", 
    linewidth = 1
  ) +
  geom_hline(
    yintercept = limite_inferior23, 
    color = "red", 
    linetype = "dotted", 
    linewidth = 1
  ) +
  labs(
    title = "Gráfico de Bland-Altman: Comparação Donkelar vs. cams",
    x = "Média das Medições ((Donkelar + cams) / 2)",
    y = "Diferença das Medições (Donkelar - cams)"
  ) +
  theme_bw()

library(irr)

icc(dispersao22 |> select(Donkelar,cams),
    model = "twoway",
    unit = "single")

icc(dispersao23 |> select(Donkelar,cams),
    model = "twoway",
    #type = "agreement",
    unit = "single")
