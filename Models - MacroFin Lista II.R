#### Instalar os Pacotes #### 

install.packages('tidyverse')
install.packages('dplyr')
install.packages('purrr')
install.packages('broom')
install.packages("writexl")
install.packages("ggplot2")
install.packages("ggpubr")


library(tidyverse)
library(dplyr)
library(purrr)
library(broom)
library(writexl)
library(ggplot2)
library(ggpubr)


#### Baixar os Dados ####

tsirbr1 <- read.table("C:/Users/fredm/OneDrive/Área de Trabalho/MacroFin/DadosTxJuros.txt", header = TRUE)
View(tsirbr1)

#### Definir os parametros Labmda estimados em De Rezende, R. B., & Ferreira, M. S. (2013) ####

lambda = 0.097
lambdaBL1 = 0.048
lambdaBL2 = 0.114
lambdaSV1 = 0.084
lambdaSV2 = 0.222
lambdaFF1 = 0.042
lambdaFF2 = 0.320

#### Manipulando os Dados ####

#Adicionar uma coluna que enumera os dias comerciais (bizdays) e reestruturar o dataframe 

tsirbr1$bizdays <- 1:nrow(tsirbr1)

tsirbr <- tsirbr1 %>%
pivot_longer(!bizdays, names_to = "maturitytype", values_to = "spot")
tsirbr$maturity <- (strtoi(substring(tsirbr$maturitytype, 2,5)))/30

View(tsirbr)

#### Estatísticas Descritivas ####

sumtsir <- tsirbr %>%
  group_by(maturity) %>%
  summarize(media = mean(spot), 
            mediana = median(spot), 
            quant10 = quantile(spot, probs = 0.1),
            quant90 = quantile(spot, probs = 0.9),
            min = min(spot),
            max = max(spot),
            sd = sd(spot)
  )

write_xlsx(sumtsir,"C:\\Users\\fredm\\OneDrive\\Área de Trabalho\\sumtsir.xlsx")

tsirbrmod <- tsirbr1

tsirbrmod$nivel <- (tsirbrmod$j30 + tsirbrmod$j360 + tsirbrmod$j1800)/3
tsirbrmod$declividade <- tsirbrmod$j1800 - tsirbrmod$j30
tsirbrmod$curvatura <- 2*tsirbrmod$j360 - tsirbrmod$j30 - tsirbrmod$j1800

sumtsirbrmod <- tsirbrmod %>%
  summarize(across(c(nivel, declividade, curvatura), list(mean, sd)
  ))
  

#### Adicionar os Termos do Modelo ####

tsirbr$t1 <- (1 - exp(-lambda*tsirbr$maturity))/(lambda*tsirbr$maturity)
tsirbr$t2 <- ((1 - exp(-lambda*tsirbr$maturity))/(lambda*tsirbr$maturity) - exp(-lambda*tsirbr$maturity))

tsirbr$t1BL <- ((1 - exp(-lambdaBL1*tsirbr$maturity))/(lambdaBL1*tsirbr$maturity))
tsirbr$t2BL <- (((1 - exp(-lambdaBL2*tsirbr$maturity))/(lambdaBL2*tsirbr$maturity)) - exp(-lambdaBL2*tsirbr$maturity)) 
                
tsirbr$t1SV <- ((1 - exp(-lambdaSV1*tsirbr$maturity))/(lambdaSV1*tsirbr$maturity))
tsirbr$t2SV <- ((1 - exp(-lambdaSV1*tsirbr$maturity))/(lambdaSV1*tsirbr$maturity) - exp(-lambdaSV1*tsirbr$maturity))
tsirbr$t3SV <- ((1 - exp(-lambdaSV2*tsirbr$maturity))/(lambdaSV2*tsirbr$maturity) - exp(-lambdaSV2*tsirbr$maturity))

tsirbr$t1FF <- ((1 - exp(-lambdaFF1*tsirbr$maturity))/(lambdaFF1*tsirbr$maturity))
tsirbr$t2FF <- ((1 - exp(-lambdaFF2*tsirbr$maturity))/(lambdaFF2*tsirbr$maturity))
tsirbr$t3FF <- ((1 - exp(-lambdaFF1*tsirbr$maturity))/(lambdaFF1*tsirbr$maturity) - exp(-lambdaFF1*tsirbr$maturity))
tsirbr$t4FF <- ((1 - exp(-lambdaFF2*tsirbr$maturity))/(lambdaFF2*tsirbr$maturity) - exp(-lambdaFF2*tsirbr$maturity))
                                
#### Rodar o modelo NS para cada business day ####

NSmodel <- NULL
for (i in tsirbr$bizdays) {
  out <- coef(lm(spot ~ t1 + t2, tsirbr[tsirbr$bizdays == i,]))
  NSmodel <- rbind(NSmodel, out)
}

NSmodel <- unique(NSmodel)

colnames(NSmodel) <- c("Level", "Slope", "Curvat")
rownames(NSmodel) <- 1:nrow(NSmodel)

View(NSmodel)

#### Rodar o modelo BL para cada business day ####

BLmodel <- NULL
for (i in tsirbr$bizdays) {
  out <- coef(lm(spot ~ t1BL + t2BL, tsirbr[tsirbr$bizdays == i,]))
  BLmodel <- rbind(BLmodel, out)
}

BLmodel <- unique(BLmodel)

colnames(BLmodel) <- c("Level", "Slope", "Curvat")
rownames(BLmodel) <- 1:nrow(BLmodel)

#### Rodar o modelo SV para cada business day ####

SVmodel <- NULL
for (i in tsirbr$bizdays) {
  out <- coef(lm(spot ~ t1SV + t2SV + t3SV, tsirbr[tsirbr$bizdays == i,]))
  SVmodel <- rbind(SVmodel, out)
}

SVmodel <- unique(SVmodel)

colnames(SVmodel) <- c("Level", "Slope", "Curvat1", "Curvat2")
rownames(SVmodel) <- 1:nrow(SVmodel)

#### Rodar o modelo FF para cada business day ####

FFmodel <- NULL
for (i in tsirbr$bizdays) {
  out <- coef(lm(spot ~ t1FF + t2FF + t3FF + t4FF, tsirbr[tsirbr$bizdays == i,]))
  FFmodel <- rbind(FFmodel, out)
}

FFmodel <- unique(FFmodel)

colnames(FFmodel) <- c("Level", "Slope1", "Slope2", "Curvat1F", "Curvat2F")
rownames(FFmodel) <- 1:nrow(FFmodel)

#### Modificações Adicionais aos outputs dos modelos ####

NSmodel <- as.data.frame(NSmodel)
BLmodel <- as.data.frame(BLmodel)
SVmodel <- as.data.frame(SVmodel)
FFmodel <- as.data.frame(FFmodel)

NSmodel$bizdays <- 1:nrow(NSmodel)
BLmodel$bizdays <- 1:nrow(BLmodel)
SVmodel$bizdays <- 1:nrow(SVmodel)
FFmodel$bizdays <- 1:nrow(FFmodel)

NSmodel$modeltype <- "NS"
BLmodel$modeltype <- "BL"
SVmodel$modeltype <- "SV"
FFmodel$modeltype <- "FF"

tsirest <- tsirbrmod %>%
  select(bizdays ,nivel, declividade, curvatura)

colnames(tsirest) <- c("bizdays","Level", "Slope", "Curvat")

tsirest$modeltype <- "Observado"

Models <- dplyr::bind_rows(NSmodel, BLmodel, SVmodel, FFmodel)

mean(NSmodel$Level)
mean(BLmodel$Level)
mean(SVmodel$Level)
mean(FFmodel$Level)

mean(NSmodel$Slope)
mean(BLmodel$Slope)
mean(SVmodel$Slope)
mean(FFmodel$Slope1 + FFmodel$Slope2)

mean(NSmodel$Curvat)
mean(BLmodel$Curvat)
mean(SVmodel$Curvat1 + SVmodel$Curvat2)
mean(FFmodel$Curvat1 + FFmodel$Curvat2)

#### Graficos ####

#### Plots Descritivos ####

plotdescritivo <- sumtsir %>%
  ggplot(aes(x = maturity)) +
  geom_point(aes(y = media), color ="#F8766D") +
  geom_point(aes(y = mediana), color ="#C49A00") +
  geom_point(aes(y = quant10), color ="#53B400") +
  geom_point(aes(y = quant90), color ="#A58AFF") +
  labs(title = "Estrutura a termo da taxa de \n juros Brasileira 2000-2007", 
       y = "Taxas", x = "Maturidade em Meses", ) +
  theme_bw()

plotdescritivo2 <- sumtsir %>% 
  ggplot(aes(x = maturity)) +
  geom_line(aes(y = media), color ="#F8766D", linewidth = 1.2) +
  geom_line(aes(y = mediana), color ="#C49A00", linewidth = 1.2) +
  geom_line(aes(y = quant10), color ="#53B400", linewidth = 1.2) +
  geom_line(aes(y = quant90), color ="#A58AFF", linewidth = 1.2) +
  labs(title = "Estrutura a termo da taxa de \n juros Brasileira 2000-2007", y = "Taxas", x = "Maturidade em meses") +
  theme_bw()

#### Plots Modelos ####

plotlevel <- Models %>%
  ggplot(aes(x = bizdays, y = Level, color = modeltype)) +
  geom_line() +
  theme_bw() +
  labs(title = "Nível da Curva de Juros estimadas pelos 4 modelos",
       y = "Nível", x ="") +
  scale_y_continuous(breaks = c(-10, 0, 10, 20, 30, 40, 50, 60, 70)) +
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 1750)) +
  theme(legend.title= element_blank()) +
  ylim(-30,70) 


plotslope <- Models %>%
  ggplot(aes(x = bizdays, y = -Slope, color = modeltype)) +
  geom_line() +
  geom_line(aes(y = -I(Slope1 + Slope2))) +
  theme_bw() +
  labs(title = "Declividade da Curva de Juros estimadas pelos 4 modelos",
       y = "Declividade", x ="") +
  scale_y_continuous(breaks = c(-10 , 0, 10, 20, 30, 40, 50, 60, 70)) +
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 1750)) +
  theme(legend.title= element_blank()) +
  ylim(-30,70) 


plotcurvat <- Models[Models$modeltype != "FF",] %>%
  ggplot(aes(x = bizdays, y = Curvat, color = modeltype)) +
  geom_line() +
  geom_line(aes(y = I(Curvat1 + Curvat2))) +
  theme_bw() +
  labs(title = "Curvatura da Curva de Juros estimadas pelos modelos NS, BL e SV",
       y = "Curvatura", x ="") +
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 1750)) +
  scale_y_continuous(breaks = c(-20, -10 , 0, 10, 20, 30, 40, 50, 60, 70)) +
  theme(legend.title= element_blank()) + 
  ylim(-30,70) 

plotcurvatFF <- Models[Models$modeltype == "FF",] %>%
  ggplot(aes(x = bizdays)) +
  geom_line(aes(y = Curvat1F, color = "C1")) +
  geom_line(aes(y = Curvat2F, color = "C2")) +
  theme_bw() +
  labs(title = "Curvaturas da Curva de Juros estimadas pelo modelo FF",
       y = "Curvatura",
       x ="",
       ) +
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 1750)) +
  scale_y_continuous(breaks = c( -75,-50, -25, 0, 25, 50)) +
  theme(legend.title= element_blank()) +
  ylim(-30,70)
 
plotmodels <- ggarrange(plotlevel, plotslope, plotcurvat, plotcurvatFF,
                        ncol = 1, nrow = 4)

plotsum <- ggarrange(plotdescritivo, plotdescritivo2,
                        ncol = 2, nrow = 1)

#### Gráfico de 4 Momentos Específicos ####

NSfn <- function(m, l, s, c){
  l + s*(1 - exp(-lambda*m))/(lambda*m) +
    c*((1 - exp(-lambda*m))/(lambda*m) - exp(-lambda*m))
}

BLfn <- function(m, l, s, c){
  l + s*(1 - exp(-lambdaBL1*m))/(lambdaBL1*m) +
    c*((1 - exp(-lambdaBL2*m))/(lambdaBL2*m) - exp(-lambdaBL2*m))
}  

SVfn <- function(m, l, s, c1, c2){
  l + s*(1 - exp(-lambdaSV1*m))/(lambdaSV1*m) + 
    c1*((1 - exp(-lambdaSV1*m))/(lambdaSV1*m) - exp(-lambdaSV1*m)) +
    c2*((1 - exp(-lambdaSV2*m))/(lambdaSV2*m) - exp(-lambdaSV2*m))
}

FFfn <- function(m, l, s1, s2, c1, c2){
  l + s1*(1 - exp(-lambdaFF1*m))/(lambdaFF1*m) +
    s2*(1 - exp(-lambdaFF2*m))/(lambdaFF2*m) +
    c1*((1 - exp(-lambdaFF1*m))/(lambdaFF1*m) - exp(-lambdaFF1*m)) +
    c2*((1 - exp(-lambdaFF2*m))/(lambdaFF2*m) - exp(-lambdaFF2*m))
}

# bizadays escolhidos aleatoriamente 350, 636, 1038, 1699

tsirday1 <- tsirbr[tsirbr$bizdays == 1,] %>%
  ggplot(aes(y = spot, x = maturity)) +
  geom_point() +
  theme_bw() + 
  geom_function(fun = ~NSfn(.x,
                            l = 23.37880,
                            s = -4.4646279,
                            c = -5.84963520),
                aes(color ="#F8766D")) +
  geom_function(fun = ~BLfn(.x,
                            l = 23.64783,
                            s = -4.7512605,
                            c = -3.14235053),
                aes(color ="#C49A00")) +
  geom_function(fun = ~SVfn(.x,
                            l = 24.21365,
                            s = -5.5815822,
                            c1 = -7.76725604,
                            c2 = 1.67286282),
                aes(color ="#53B400")) +
  geom_function(fun = ~FFfn(.x,
                            l = 28.14862,
                            s1 = -7.74898914,
                            s2 = -1.54874703,
                            c1 = -11.5143080,
                            c2 = -1.68646027),
                aes(color ="#A58AFF")) +
  scale_color_identity(name = "Model",
                       breaks = c("#F8766D", "#C49A00", "#53B400", "#A58AFF"),
                       labels = c("NS", "BL", "SV", "FF"),
                       guide = "legend") +
  scale_x_continuous(breaks = c(1, 6, 12, 18, 24, 36, 48, 60)) +
  labs(title = "Estrutura a termo da taxa de juros do dia 1", x = "Maturidade (em meses)", y = "Taxa")

tsirday350 <- tsirbr[tsirbr$bizdays == 350,] %>%
  ggplot(aes(y = spot, x = maturity)) +
  geom_point() +
  theme_bw() + 
  geom_function(fun = ~NSfn(.x,
                            l = 34.05328,
                            s = -13.574813,
                            c = -3.55651230),
                aes(color ="#F8766D")) +
  geom_function(fun = ~BLfn(.x,
                            l = 35.86873,
                            s = -15.612809,
                            c = 3.712514406),
                aes(color ="#C49A00")) +
  geom_function(fun = ~SVfn(.x,
                            l = 35.80022,
                            s = -16.166331,
                            c1 = -8.52127634,
                            c2 = 5.9320651),
                aes(color ="#53B400")) +
  geom_function(fun = ~FFfn(.x,
                            l = 28.78091,
                            s1 = -18.16835422,
                            s2 = 8.127545941,
                            c1 = 27.7408918,
                            c2 = 13.54987285),
                aes(color ="#A58AFF")) +
  scale_color_identity(name = "Model",
                       breaks = c("#F8766D", "#C49A00", "#53B400", "#A58AFF"),
                       labels = c("NS", "BL", "SV", "FF"),
                       guide = "legend") +
  scale_x_continuous(breaks = c(1, 6, 12, 18, 24, 36, 48, 60)) +
  labs(title = "TSIR do dia 350", x = "Maturidade (em meses)", y = "Taxa")

tsirday1038 <- tsirbr[tsirbr$bizdays == 1038,] %>%
  ggplot(aes(y = spot, x = maturity)) +
  geom_point() +
  theme_bw() + 
  geom_function(fun = ~NSfn(.x,
                            l = 24.65734,
                            s = -9.2998841,
                            c = -4.82139524),
                aes(color ="#F8766D")) +
  geom_function(fun = ~BLfn(.x,
                            l = 25.74352,
                            s = -10.5024936,
                            c = 0.2794503),
                aes(color ="#C49A00")) +
  geom_function(fun = ~SVfn(.x,
                            l = 25.46496,
                            s = -10.406513,
                            c1 = -6.15590753,
                            c2 = 1.907739435),
                aes(color ="#53B400")) +
  geom_function(fun = ~FFfn(.x,
                            l = 30.26087,
                            s1 = -9.6244340,
                            s2 = -5.155765843,
                            c1 = -12.9857983,
                            c2 = -4.67395777),
                aes(color ="#A58AFF")) +
  scale_color_identity(name = "Model",
                       breaks = c("#F8766D", "#C49A00", "#53B400", "#A58AFF"),
                       labels = c("NS", "BL", "SV", "FF"),
                       guide = "legend") +
  scale_x_continuous(breaks = c(1, 6, 12, 18, 24, 36, 48, 60)) +
  labs(title = "TSIR do dia 1038", x = "Maturidade (em meses)", y = "Taxa") 

tsirday1699 <- tsirbr[tsirbr$bizdays == 1699,] %>%
  ggplot(aes(y = spot, x = maturity)) +
  geom_point() +
  theme_bw() + 
  geom_function(fun = ~NSfn(.x,
                            l = 12.75810,
                            s = 0.171850166,
                            c = -1.5948649),
                aes(color ="#F8766D")) +
  geom_function(fun = ~BLfn(.x,
                            l = 12.62684,
                            s = 0.33325735,
                            c = -1.6131986),
                aes(color ="#C49A00")) +
  geom_function(fun = ~SVfn(.x,
                            l = 12.63818,
                            s = 0.39128470,
                            c1 = -0.8470406,
                            c2 = -0.851193820),
                aes(color ="#53B400")) +
  geom_function(fun = ~FFfn(.x,
                            l = 12.528712,
                            s1 = -0.667793689,
                            s2 = 1.15839241,
                            c1 = 0.46466414,
                            c2 = 0.14938962),
                aes(color ="#A58AFF")) +
  scale_color_identity(name = "Model",
                       breaks = c("#F8766D", "#C49A00", "#53B400", "#A58AFF"),
                       labels = c("NS", "BL", "SV", "FF"),
                       guide = "legend") +
  scale_x_continuous(breaks = c(1, 6, 12, 18, 24, 36, 48, 60)) +
  labs(title = "TSIR do dia 1699", x = "Maturidade (em meses)", y = "Taxa") 

tsirday636 <- tsirbr[tsirbr$bizdays == 636,] %>%
  ggplot(aes(y = spot, x = maturity)) +
  geom_point() +
  theme_bw() + 
  geom_function(fun = ~NSfn(.x,
                            l = 53.70703,
                            s = -36.008451,
                            c = -15.03329065),
                aes(color ="#F8766D")) +
  geom_function(fun = ~BLfn(.x,
                            l = 58.20779,
                            s = -40.96484,
                            c = 4.27941425),
                aes(color ="#C49A00")) +
  geom_function(fun = ~SVfn(.x,
                            l = 55.21498,
                            s = -37.94102,
                            c1 = -14.02294534,
                            c2 = 2.286842822),
                aes(color ="#53B400")) +
  geom_function(fun = ~FFfn(.x,
                            l = 69.75766,
                            s1 = -32.449754,
                            s2 = -18.59894192,
                            c1 = -35.79586899,
                            c2 = -18.703046791),
                aes(color ="#A58AFF")) +
  scale_color_identity(name = "Model",
                       breaks = c("#F8766D", "#C49A00", "#53B400", "#A58AFF"),
                       labels = c("NS", "BL", "SV", "FF"),
                       guide = "legend") +
  scale_x_continuous(breaks = c(1, 6, 12, 18, 24, 36, 48, 60)) +
  labs(title = "TSIR do dia 636", x = "Maturidade (em meses)", y = "Taxa") 

plottsirdays <- ggarrange(tsirday350, tsirday636, tsirday1038, tsirday1699,
                                        ncol = 2, nrow = 2)

