##### Trabalho Final de Macrofinancas #####

install.packages("anytime")
install.packages("moments")
install.packages("sandwich")
install.packages("RcmdrMisc")

library(readxl)
library(tidyverse)
library(dplyr)
library(anytime)
library(lubridate)
library(moments)
library(broom)
library(sandwich)
library(RcmdrMisc)
library(sjPlot)
library(plm)

##### Dados #####

# ZCB Brasil

tsir_anbima <- read_excel("C:/Users/fredm/OneDrive/Área de Trabalho/Trab Final MF/dados - curva anbima (1).xlsx", 
                                    col_types = c("date", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric"))
View(tsir_anbima)

tsir_anbima$date <- as.Date(tsir_anbima$date)
tsir_anbima$Curve_Pre <- (tsir_anbima$PRE_B3 + tsir_anbima$PRE_B4)
tsir_anbima$Curve_IPCA <- (tsir_anbima$IPCA_B3 + tsir_anbima$IPCA_B4)

tsir_anbima <- tsir_anbima %>%
  rename(Level_IPCA = IPCA_B1,
         Slope_IPCA = IPCA_B2,
         Curve1_IPCA = IPCA_B3,
         Curve2_IPCA = IPCA_B4,
         Level_Pre = PRE_B1,
         Slope_Pre = PRE_B2,
         Curve1_Pre = PRE_B3,
         Curve2_Pre = PRE_B4)
         
# Selic

selic <- read.csv("C:/Users/fredm/OneDrive/Área de Trabalho/selicipea.csv")

selic$date <- as.character(selic$date)
selic$day <- substr(selic$date, start = 1, stop = 2)
selic$month <- substr(selic$date, start = 4, stop = 5)
selic$year <- substr(selic$date, start = 7, stop = 10)

selic$date <- paste(selic$month,selic$day,selic$year, sep = "-")
selic$date <- anytime(selic$date)
selic$date <- as.Date(selic$date)

selic <- selic %>%
  group_by(date = floor_date(date, "month")) %>%
             summarize(selic = mean(selic, na.rm = T))
           
View(selic)

# IPCA

IPCA <- read_excel("C:/Users/fredm/Downloads/ipeadata[23-12-2022-12-34].xls")

IPCA$year <- substr(IPCA$date, start = 1, stop = 4)
IPCA$month <- substr(IPCA$date, start = 6, stop = 7)
IPCA$day <- 1

IPCA$date <- paste(IPCA$month, IPCA$day, IPCA$year, sep = "-")
IPCA$date <- anytime(IPCA$date)
IPCA$date <- as.Date(IPCA$date)

IPCA$IPCA <- (100*IPCA$IPCA)/IPCA$IPCA[IPCA$date == "2009-09-01"]

IPCA <- IPCA %>%
  mutate(Tri_IPCA = 100*((IPCA/lag(IPCA, n = 2)) - 1))

View(IPCA)

# IBC-BR

ibcbr <- read_excel("C:/Users/fredm/OneDrive/Área de Trabalho/IBCBR.xlsx")

ibcbr$year <- substr(ibcbr$date, start = 1, stop = 4)
ibcbr$month <- substr(ibcbr$date, start = 6, stop = 7)
ibcbr$day <- 1

ibcbr$date <- paste(ibcbr$month, ibcbr$day, ibcbr$year, sep = "-")
ibcbr$date <- anytime(ibcbr$date)
ibcbr$date <- as.Date(ibcbr$date)

ibcbr$IBC_br <- (100*ibcbr$IBC_br)/ibcbr$IBC_br[ibcbr$date == "2009-09-01"]

ibcbr <- ibcbr %>%
  mutate(Tri_IBC_br = IBC_br - lag(IBC_br, n = 2))
                   
View(ibcbr)

# Cambio 

TX_Cambio <- read_excel("C:/Users/fredm/Downloads/ipeadata[23-12-2022-12-37].xls")

TX_Cambio$year <- substr(TX_Cambio$date, start = 1, stop = 4)
TX_Cambio$month <- substr(TX_Cambio$date, start = 6, stop = 7)
TX_Cambio$day <- 1

TX_Cambio$date <- paste(TX_Cambio$month, TX_Cambio$day, TX_Cambio$year, sep = "-")
TX_Cambio$date <- anytime(TX_Cambio$date)
TX_Cambio$date <- as.Date(TX_Cambio$date)

View(TX_Cambio)

# EMBI Brazil 

embibr <- read_excel("C:/Users/fredm/OneDrive/Área de Trabalho/embi.xlsx")

embibr$year <- substr(embibr$date, start = 7, stop = 10)
embibr$month <- substr(embibr$date, start = 4, stop = 5)
embibr$day <- substr(embibr$date, start = 1, stop = 2)

embibr$date <- paste(embibr$month, embibr$day, embibr$year, sep = "-")
embibr$date <- anytime(embibr$date)
embibr$date <- as.Date(embibr$date)

embibr <- embibr %>%
  group_by(date = floor_date(date, "month")) %>%
  summarise(embi = mean(embi, na.rm = TRUE))
  
View(embibr)

# VIX

vix <- read.csv("C:/Users/fredm/Downloads/VIX_History.csv")

vix$year <- substr(vix$date, start = 7, stop = 10)
vix$month <- substr(vix$date, start = 1, stop = 2)
vix$day <- substr(vix$date, start = 4, stop = 5)

vix$date <- paste(vix$month, vix$day, vix$year, sep = "-")
vix$date <- mdy(vix$date)
vix$date <- as.Date(vix$date)

vix <- vix %>%
  group_by(date = floor_date(date, "month")) %>%
  summarise(vix = mean(CLOSE, na.rm = TRUE))

View(vix)

# Colar as séries

start_date = "2008-01-01"

selic <- selic[selic$date >= start_date,]
IPCA <- IPCA[IPCA$date >= start_date,]
ibcbr <- ibcbr[ibcbr$date >= start_date,]
TX_Cambio <- TX_Cambio[TX_Cambio$date >= start_date,]
embibr <- embibr[embibr$date >= start_date,]
vix <- vix[vix$date >= start_date,]

var_macro <- selic %>%
  left_join(IPCA, by = "date") %>%
  left_join(ibcbr, by = "date") %>%
  left_join(TX_Cambio, by = "date") %>%
  left_join(embibr, by = "date") %>%
  left_join(vix, by = "date") %>%
  select(!c(year.x, month.x, day.x,
            year.y, month.y, day.y,
            year, month, day))

##### Estatísticas Descritivas Var Macro #####

a <- "IPCA"

summacro <- dados_macro_tsir %>%
  summarize(media = mean(vix), 
            mediana = median(vix), 
            sd = sd(vix),
            skewness = skewness(vix),
            kurtosis = kurtosis(vix),
            quant10 = quantile(vix, probs = 0.1),
            quant90 = quantile(vix, probs = 0.9)
)
  

##### Plots das séries temporais dos Betas #####

betasSV <- tsir_anbima %>%
  select(date, 
         Level_IPCA, Slope_IPCA, Curve1_IPCA, Curve2_IPCA, Curve_IPCA,
         Level_Pre, Slope_Pre, Curve1_Pre, Curve2_Pre, Curve_Pre) %>%
  pivot_longer(c(Level_IPCA, Level_Pre), names_to = "Level", values_to = "VLevel") %>%
  pivot_longer(c(Slope_IPCA, Slope_Pre), names_to = "Slope", values_to = "VSlope") %>%
  pivot_longer(c(Curve1_IPCA, Curve2_IPCA, Curve_IPCA,
                 Curve1_Pre, Curve2_Pre, Curve_Pre), 
               names_to = "Curve", values_to = "VCurve") 

plotlevel <- betasSV %>%
  ggplot(aes(x = date, y = 100*VLevel, color = Level)) +
  geom_line() +
  theme_bw() +
  labs(title = "Nível das Curvas de Juros Real e Nominal em % - Brasil Set/2009 - Maio/2018",
       y = "Nível",
       x ="") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_color_manual(values=c("#CC6666", "#9999CC"))
  
plotslopereal <- betasSV[betasSV$Slope == "Slope_IPCA",] %>%
  ggplot(aes(x = date, y = -100*VSlope, color = Slope)) +
  geom_line() +
  theme_bw() +
  labs(title = "Declividade da Curva de Juros Real em % - Brasil Set/2009 - Maio/2018",
       y = "Declividade",
       x ="") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_color_manual(values=c("#9999CC"))

plotslopenominal <- betasSV[betasSV$Slope == "Slope_Pre",] %>%
  ggplot(aes(x = date, y = -100*VSlope, color = Slope)) +
  geom_line() +
  theme_bw() +
  labs(title = "Declividade da Curva Nominal em % - Brasil Set/2009 - Maio/2018",
       y = "Declividade",
       x ="") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_color_manual(values=c("#CC6666"))

plotcurve <- betasSV[betasSV$Curve %in% c("Curve_IPCA", "Curve_Pre"),] %>%
  ggplot(aes(x = date, y = 100*VCurve, color = Curve)) +
  geom_line() +
  theme_bw() +
  labs(title = "Curvatura (Beta 3 + Beta 4) das Curvas de Juros Real e Nominal em % - Brasil Set/2009 - Maio/2018",
       y = "Curvatura",
       x ="") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_color_manual(values=c("#CC6666", "#9999CC"))

plotcurveIPCA <- betasSV[betasSV$Curve %in% c("Curve1_IPCA", "Curve2_IPCA"),] %>%
  ggplot(aes(x = date, y = 100*VCurve, color = Curve)) +
  geom_line() +
  theme_bw() +
  labs(title = "Betas referentes à curvatura da curva de Juros Real em % - Brasil Set/2009 - Maio/2018",
       y = "Curvatura",
       x ="") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") 

plotcurvePre <- betasSV[betasSV$Curve %in% c("Curve1_Pre", "Curve2_Pre"),] %>%
  ggplot(aes(x = date, y = 100*VCurve, color = Curve)) +
  geom_line() +
  theme_bw() +
  labs(title = "Betas referentes à curvatura da curva de Juros Nominal em % - Brasil Set/2009 - Maio/2018",
       y = "Curvatura",
       x ="") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")

ggarrange(plotlevel,
          plotslopenominal, 
          plotslopereal,
          nrow = 3,
          ncol = 1)

ggarrange(plotcurve,
          plotcurveIPCA, 
          plotcurvePre,
          nrow = 3,
          ncol = 1)
  
##### Estatisticas Descritivas Betas #####

sumlevel <- betasSV %>%
  group_by(Level) %>%
  summarize(media = mean(100*VLevel), 
            mediana = median(100*VLevel), 
            sd = sd(100*VLevel),
            skewness = skewness(100*VLevel),
            kurtosis = kurtosis(100*VLevel),
            quant10 = quantile(100*VLevel, probs = 0.1),
            quant90 = quantile(100*VLevel, probs = 0.9),
            min = min(100*VLevel),
            max = max(100*VLevel)
  )

sumslope <- betasSV %>%
  group_by(Slope) %>%
  summarize(media = mean(-100*VSlope), 
            mediana = median(-100*VSlope), 
            sd = sd(-100*VSlope),
            skewness = skewness(-100*VSlope),
            kurtosis = kurtosis(-100*VSlope),
            quant10 = quantile(-100*VSlope, probs = 0.1),
            quant90 = quantile(-100*VSlope, probs = 0.9),
            min = min(-100*VSlope),
            max = max(-100*VSlope)
  )  

sumcurve <- betasSV %>%
  group_by(Curve) %>%
  summarize(media = mean(100*VCurve), 
            mediana = median(100*VCurve), 
            sd = sd(100*VCurve),
            skewness = skewness(100*VCurve),
            kurtosis = kurtosis(100*VCurve),
            quant10 = quantile(100*VCurve, probs = 0.1),
            quant90 = quantile(100*VCurve, probs = 0.9),
            min = min(100*VCurve),
            max = max(100*VCurve)
  )  

##### Autocorrelação temporal #####

# FACs Betas IPCA

FAC_Level_IPCA <- acf(tsir_anbima$Level_IPCA, main = "FAC - Level IPCA")
PFAC_Level_IPCA <- pacf(tsir_anbima$Level_IPCA, main = "PFAC - Level IPCA", ylim = c(-0.2, 1))

FAC_Slope_IPCA <- acf(tsir_anbima$Slope_IPCA, main = "FAC - Slope IPCA")
PFAC_Slope_IPCA <- pacf(tsir_anbima$Slope_IPCA, main = "PFAC - Slope IPCA", ylim = c(-0.2, 1))

FAC_Curve_IPCA <- acf(tsir_anbima$Curve_IPCA,  main = "FAC - Curve IPCA")
PFAC_Curve_IPCA <- pacf(tsir_anbima$Curve_IPCA, main = "PFAC - Curve IPCA", ylim = c(-0.2, 1))

FAC_Curve1_IPCA <- acf(tsir_anbima$Curve1_IPCA, main = "FAC - Beta 3 IPCA")
PFAC_Curve1_IPCA <- pacf(tsir_anbima$Curve1_IPCA, main = "PFAC - Beta 3 IPCA", ylim = c(-0.2, 1))

FAC_Curve2_IPCA <- acf(tsir_anbima$Curve2_IPCA, main = "FAC - Beta 4 IPCA")
PFAC_Curve2_IPCA <- pacf(tsir_anbima$Curve2_IPCA, main = "PFAC - Beta 4 IPCA", ylim = c(-0.2, 1))

# FACs Betas Pre

FAC_Level_Pre <- acf(tsir_anbima$Level_Pre, main = "FAC - Level Pre")
PFAC_Level_Pre <- pacf(tsir_anbima$Level_Pre, main = "PFAC - Level Pre", ylim = c(-0.2, 1))

FAC_Slope_Pre <- acf(tsir_anbima$Slope_Pre, main = "FAC - Slope Pre")
PFAC_Slope_Pre <- pacf(tsir_anbima$Slope_Pre, main = "PFAC - Slope Pre", ylim = c(-0.2, 1))

FAC_Curve_Pre <- acf(tsir_anbima$Curve_Pre, main = "FAC - Curve Pre")
PFAC_Curve_Pre <- pacf(tsir_anbima$Curve_Pre,  main = "PFAC - Curve Pre", ylim = c(-0.2, 1))

FAC_Curve1_Pre <- acf(tsir_anbima$Curve1_Pre, main = "FAC - Beta 3 Pre")
PFAC_Curve1_Pre <- pacf(tsir_anbima$Curve1_Pre, main = "PFAC - Beta 3 Pre", ylim = c(-0.2, 1))

FAC_Curve2_Pre <- acf(tsir_anbima$Curve2_Pre, main = "FAC - Beta 4 Pre")
PFAC_Curve2_Pre <- pacf(tsir_anbima$Curve2_Pre, main = "PFAC - Beta 4 Pre", ylim = c(-0.2, 1))

# Extraindo os coefs

FAC_Level_IPCA$acf[2:7]
FAC_Slope_IPCA$acf[2:7]
FAC_Curve_IPCA$acf[2:7]
FAC_Curve1_IPCA$acf[2:7]
FAC_Curve2_IPCA$acf[2:7]

FAC_Level_Pre$acf[2:7]
FAC_Slope_Pre$acf[2:7]
FAC_Curve_Pre$acf[2:7]
FAC_Curve1_Pre$acf[2:7]
FAC_Curve2_Pre$acf[2:7]

# Autocorrelação Cruzada

par(mfrow = c(2, 2))

CCF_Levels <- ccf(tsir_anbima$Level_Pre, tsir_anbima$Level_IPCA, ylim = c(-0.2, 1),
                  main = "Correlação Cruzada Nível \n Nominal e Real")
CCF_Slopes <- ccf(tsir_anbima$Slope_Pre, tsir_anbima$Slope_IPCA, ylim = c(-0.2, 1),
                  main = "Correlação Cruzada Inclinação \n Nominal e Real")
CCF_Curves <- ccf(tsir_anbima$Curve_Pre, tsir_anbima$Curve_IPCA, ylim = c(-0.2, 1),
                  main = "Correlação Cruzada Curvatura \n Nominal e Real")
CCF_Curves1 <- ccf(tsir_anbima$Curve1_Pre, tsir_anbima$Curve1_IPCA, ylim = c(-0.2, 1),
                   main = "Correlação Cruzada Beta 3 \n Nominal e Real")
CCF_Curves2 <- ccf(tsir_anbima$Curve2_Pre, tsir_anbima$Curve2_IPCA, ylim = c(-0.2, 1),
                   main = "Correlação Cruzada Beta 4 \n Nominal e Real")

CCF_Levels$acf[18:24] 
CCF_Slopes$acf[18:24]  
CCF_Curves$acf[18:24]   
CCF_Curves1$acf[18:24]   
CCF_Curves2$acf[18:24]  

ccfsIPCA_Pre

CCF_LevelSlopeIPCA <- ccf(tsir_anbima$Level_IPCA, tsir_anbima$Slope_IPCA, ylim = c(-0.5, 1),
                          main = "Correlação Cruzada Nível \n e Inclinação Reais")
CCF_LevelCurveIPCA <- ccf(tsir_anbima$Level_IPCA, tsir_anbima$Curve_IPCA, ylim = c(-0.5, 1))
CCF_LevelCurve1IPCA <- ccf(tsir_anbima$Level_IPCA, tsir_anbima$Curve1_IPCA, ylim = c(-0.5, 1),
                           main = "Correlação Cruzada Nível \n e Beta 3 Reais")
CCF_LevelCurve2IPCA <- ccf(tsir_anbima$Level_IPCA, tsir_anbima$Curve2_IPCA, ylim = c(-0.5, 1),
                           main = "Correlação Cruzada Nível \n e Beta 4 Reais")

CCF_LevelSlopeIPCA$acf[18:24]
CCF_LevelCurve1IPCA$acf[18:24]
CCF_LevelCurve2IPCA$acf[18:24]

CCF_LevelSlopePRE <- ccf(tsir_anbima$Level_Pre, tsir_anbima$Slope_Pre, ylim = c(-0.5, 1),
                         main = "Correlação Cruzada Nível \n e Inclinação Nominais")
CCF_LevelCurvePRE <- ccf(tsir_anbima$Level_Pre, tsir_anbima$Curve_Pre, ylim = c(-0.5, 1))
CCF_LevelCurve1PRE <- ccf(tsir_anbima$Level_Pre, tsir_anbima$Curve1_Pre, ylim = c(-0.5, 1),
                          main = "Correlação Cruzada Nível \n e Beta 3 Nominais")
CCF_LevelCurve2PRE <- ccf(tsir_anbima$Level_Pre, tsir_anbima$Curve2_Pre, ylim = c(-0.5, 1),
                          main = "Correlação Cruzada Nível \n e Beta 3 Nominais")

CCF_LevelSlopePRE$acf[18:24]
CCF_LevelCurve1PRE$acf[18:24]
CCF_LevelCurve2PRE$acf[18:24]

CCF_SlopeCurveIPCA <- ccf(tsir_anbima$Slope_IPCA, tsir_anbima$Curve_IPCA, ylim = c(-1, 1))
CCF_SlopeCurve1IPCA <- ccf(tsir_anbima$Slope_IPCA, tsir_anbima$Curve1_IPCA, ylim = c(-1, 1),
                           main = "Correlação Cruzada Inclinação \n e Beta 3 Reais")
CCF_SlopeCurve2IPCA <- ccf(tsir_anbima$Slope_IPCA, tsir_anbima$Curve2_IPCA, ylim = c(-1, 1),
                           main = "Correlação Cruzada Inclinação \n e Beta 4 Reais")

CCF_SlopeCurve1IPCA$acf[18:24]
CCF_SlopeCurve2IPCA$acf[18:24]

CCF_SlopeCurve1Pre <- ccf(tsir_anbima$Slope_Pre, tsir_anbima$Curve1_Pre, ylim = c(-1, 1),
                          main = "Correlação Cruzada Inclinação \n e Beta 3 Nominais")
CCF_SlopeCurve2Pre <- ccf(tsir_anbima$Slope_Pre, tsir_anbima$Curve2_Pre, ylim = c(-1, 1),
                          main = "Correlação Cruzada Inclinação \n e Beta 4 Nominais")

CCF_SlopeCurve1Pre$acf[18:24]
CCF_SlopeCurve2Pre$acf[18:24]

##### Base de Dados Final #####

var_macro <- var_macro %>%
  mutate(Tri_IPCA_6mnths = dplyr::lead(Tri_IPCA, n = 6),
         Tri_IPCA_12mnths = dplyr::lead(Tri_IPCA, n = 12),
         Tri_IBC_br6months = dplyr::lead(Tri_IBC_br, n = 6),
         Tri_IBC_br12months = dplyr::lead(Tri_IBC_br, n = 12),
         LnVix = log(vix),
         LnEmbi = log(embi),
         DeltaLnVix = LnVix - dplyr::lag(LnVix),
         DeltaEmbi = LnEmbi - dplyr::lag(LnEmbi),
         LnFX = log(BRL_per_USD)
  )

dados_macro_tsir <- var_macro %>%
  left_join(tsir_anbima, by = "date")

dados_macro_tsir <- dados_macro_tsir %>%
  drop_na(Level_IPCA,
          Level_Pre)

View(dados_macro_tsir)

##### Modelos #####

# Modelo Inflação 6 meses e nível Pre

reg_inf_levelPre_6months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Level_Pre + selic + Tri_IPCA)
reg_inf_levelPre_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Level_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1))
reg_inf_levelPre_6months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Level_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2))
reg_inf_levelPre_6months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Level_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3))
reg_inf_levelPre_6months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Level_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3) +
       lag(selic, n = 4) + lag(Tri_IPCA, n = 4))
       
glance(reg_inf_levelPre_6months_0)
glance(reg_inf_levelPre_6months_1)
glance(reg_inf_levelPre_6months_2)
glance(reg_inf_levelPre_6months_3)
glance(reg_inf_levelPre_6months_4)


# Modelo Inflação 6 meses e nível IPCA

reg_inf_levelIPCA_6months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Level_IPCA + selic + Tri_IPCA)
reg_inf_levelIPCA_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Level_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1))
reg_inf_levelIPCA_6months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Level_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2))
reg_inf_levelIPCA_6months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Level_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3))
reg_inf_levelIPCA_6months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Level_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3) +
       lag(selic, n = 4) + lag(Tri_IPCA, n = 4))

glance(reg_inf_levelIPCA_6months_0)
glance(reg_inf_levelIPCA_6months_1)
glance(reg_inf_levelIPCA_6months_2)
glance(reg_inf_levelIPCA_6months_3)
glance(reg_inf_levelIPCA_6months_4)

# Modelo Inflação 6 meses e Inclinação Pre

reg_inf_SlopePre_6months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Slope_Pre + selic + Tri_IPCA)
reg_inf_SlopePre_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Slope_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1))
reg_inf_SlopePre_6months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Slope_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2))
reg_inf_SlopePre_6months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Slope_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3))
reg_inf_SlopePre_6months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Slope_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3) +
       lag(selic, n = 4) + lag(Tri_IPCA, n = 4))

glance(reg_inf_SlopePre_6months_0)
glance(reg_inf_SlopePre_6months_1)
glance(reg_inf_SlopePre_6months_2)
glance(reg_inf_SlopePre_6months_3)
glance(reg_inf_SlopePre_6months_4)

# Modelo Inflação 6 meses e Inclinação IPCA

reg_inf_SlopeIPCA_6months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Slope_IPCA + selic + Tri_IPCA)
reg_inf_SlopeIPCA_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Slope_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1))
reg_inf_SlopeIPCA_6months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Slope_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2))
reg_inf_SlopeIPCA_6months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Slope_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3))
reg_inf_SlopeIPCA_6months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Slope_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3) +
       lag(selic, n = 4) + lag(Tri_IPCA, n = 4))

glance(reg_inf_SlopeIPCA_6months_0)
glance(reg_inf_SlopeIPCA_6months_1)
glance(reg_inf_SlopeIPCA_6months_2)
glance(reg_inf_SlopeIPCA_6months_3)
glance(reg_inf_SlopeIPCA_6months_4)

# Modelo Inflação 6 meses e Curva Pre

reg_inf_CurvePre_6months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Curve_Pre + selic + Tri_IPCA)
reg_inf_CurvePre_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Curve_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1))
reg_inf_CurvePre_6months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Curve_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2))
reg_inf_CurvePre_6months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Curve_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3))
reg_inf_CurvePre_6months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Curve_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3) +
       lag(selic, n = 4) + lag(Tri_IPCA, n = 4))

glance(reg_inf_CurvePre_6months_0)
glance(reg_inf_CurvePre_6months_1)
glance(reg_inf_CurvePre_6months_2)
glance(reg_inf_CurvePre_6months_3)
glance(reg_inf_CurvePre_6months_4)

# Modelo Inflação 6 meses e Curva IPCA

reg_inf_CurveIPCA_6months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Curve_IPCA + selic + Tri_IPCA)
reg_inf_CurveIPCA_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Curve_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1))
reg_inf_CurveIPCA_6months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Curve_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2))
reg_inf_CurveIPCA_6months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Curve_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3))
reg_inf_CurveIPCA_6months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Curve_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3) +
       lag(selic, n = 4) + lag(Tri_IPCA, n = 4))

glance(reg_inf_CurveIPCA_6months_0)
glance(reg_inf_CurveIPCA_6months_1)
glance(reg_inf_CurveIPCA_6months_2)
glance(reg_inf_CurveIPCA_6months_3)
glance(reg_inf_CurveIPCA_6months_4)

reg_inf_Curve1Pre_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Curve1_Pre + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )
reg_inf_Curve2Pre_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Curve2_Pre + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )
reg_inf_Curve1IPCA_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Curve1_IPCA + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )
reg_inf_Curve2IPCA_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_6mnths ~ Curve2_IPCA + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )


##### Sumário dos Modelos de Inflação 6 meses com erros padrão robustos HAC #####

summarySandwich(reg_inf_levelPre_6months_1, type = "hac")
summarySandwich(reg_inf_levelIPCA_6months_1, type = "hac")                
summarySandwich(reg_inf_SlopePre_6months_1, type = "hac")
summarySandwich(reg_inf_SlopeIPCA_6months_1, type = "hac")
summarySandwich(reg_inf_CurvePre_6months_1, type = "hac")               
summarySandwich(reg_inf_CurveIPCA_6months_1, type = "hac")

summarySandwich(reg_inf_Curve1Pre_6months_1, type = "hac")
summarySandwich(reg_inf_Curve2Pre_6months_1, type = "hac")
summarySandwich(reg_inf_Curve1IPCA_6months_1, type = "hac")
summarySandwich(reg_inf_Curve2IPCA_6months_1, type = "hac")

tab_model(reg_inf_levelPre_6months_1,
          reg_inf_levelIPCA_6months_1,
          reg_inf_SlopePre_6months_1,
          reg_inf_SlopeIPCA_6months_1,
          reg_inf_CurvePre_6months_1,
          reg_inf_CurveIPCA_6months_1,
          show.ci = FALSE,
          show.se = TRUE,
          vcov.fun = "hac")
  
##### Modelos de Inflação 12 meses #####
        
# Modelo Inflação 12 meses e nível Pre

reg_inf_levelPre_12months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Level_Pre + selic + Tri_IPCA)
reg_inf_levelPre_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Level_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1))
reg_inf_levelPre_12months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Level_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2))
reg_inf_levelPre_12months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Level_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3))
reg_inf_levelPre_12months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Level_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3) +
       lag(selic, n = 4) + lag(Tri_IPCA, n = 4))

glance(reg_inf_levelPre_12months_0)
glance(reg_inf_levelPre_12months_1)
glance(reg_inf_levelPre_12months_2)
glance(reg_inf_levelPre_12months_3)
glance(reg_inf_levelPre_12months_4)


# Modelo Inflação 12 meses e nível IPCA

reg_inf_levelIPCA_12months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Level_IPCA + selic + Tri_IPCA)
reg_inf_levelIPCA_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Level_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1))
reg_inf_levelIPCA_12months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Level_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2))
reg_inf_levelIPCA_12months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Level_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3))
reg_inf_levelIPCA_12months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Level_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3) +
       lag(selic, n = 4) + lag(Tri_IPCA, n = 4))

glance(reg_inf_levelIPCA_12months_0)
glance(reg_inf_levelIPCA_12months_1)
glance(reg_inf_levelIPCA_12months_2)
glance(reg_inf_levelIPCA_12months_3)
glance(reg_inf_levelIPCA_12months_4)

# Modelo Inflação 12 meses e Inclinação Pre

reg_inf_SlopePre_12months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Slope_Pre + selic + Tri_IPCA)
reg_inf_SlopePre_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Slope_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1))
reg_inf_SlopePre_12months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Slope_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2))
reg_inf_SlopePre_12months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Slope_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3))
reg_inf_SlopePre_12months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Slope_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3) +
       lag(selic, n = 4) + lag(Tri_IPCA, n = 4))

glance(reg_inf_SlopePre_12months_0)
glance(reg_inf_SlopePre_12months_1)
glance(reg_inf_SlopePre_12months_2)
glance(reg_inf_SlopePre_12months_3)
glance(reg_inf_SlopePre_12months_4)

# Modelo Inflação 12 meses e Inclinação IPCA

reg_inf_SlopeIPCA_12months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Slope_IPCA + selic + Tri_IPCA)
reg_inf_SlopeIPCA_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Slope_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1))
reg_inf_SlopeIPCA_12months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Slope_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2))
reg_inf_SlopeIPCA_12months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Slope_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3))
reg_inf_SlopeIPCA_12months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Slope_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3) +
       lag(selic, n = 4) + lag(Tri_IPCA, n = 4))

glance(reg_inf_SlopeIPCA_12months_0)
glance(reg_inf_SlopeIPCA_12months_1)
glance(reg_inf_SlopeIPCA_12months_2)
glance(reg_inf_SlopeIPCA_12months_3)
glance(reg_inf_SlopeIPCA_12months_4)

# Modelo Inflação 12 meses e Curva Pre

reg_inf_CurvePre_12months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Curve_Pre + selic + Tri_IPCA)
reg_inf_CurvePre_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Curve_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1))
reg_inf_CurvePre_12months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Curve_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2))
reg_inf_CurvePre_12months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Curve_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3))
reg_inf_CurvePre_12months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Curve_Pre + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3) +
       lag(selic, n = 4) + lag(Tri_IPCA, n = 4))

glance(reg_inf_CurvePre_12months_0)
glance(reg_inf_CurvePre_12months_1)
glance(reg_inf_CurvePre_12months_2)
glance(reg_inf_CurvePre_12months_3)
glance(reg_inf_CurvePre_12months_4)

# Modelo Inflação 12 meses e Curva IPCA

reg_inf_CurveIPCA_12months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Curve_IPCA + selic + Tri_IPCA)
reg_inf_CurveIPCA_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Curve_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1))
reg_inf_CurveIPCA_12months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Curve_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2))
reg_inf_CurveIPCA_12months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Curve_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3))
reg_inf_CurvePre_12months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Curve_IPCA + selic + Tri_IPCA +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IPCA, n = 3) +
       lag(selic, n = 4) + lag(Tri_IPCA, n = 4))

glance(reg_inf_CurveIPCA_12months_0)
glance(reg_inf_CurveIPCA_12months_1)
glance(reg_inf_CurveIPCA_12months_2)
glance(reg_inf_CurveIPCA_12months_3)
glance(reg_inf_CurveIPCA_12months_4)

reg_inf_Curve1Pre_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Curve1_Pre + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )
reg_inf_Curve2Pre_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Curve2_Pre + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )
reg_inf_Curve1IPCA_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Curve1_IPCA + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )
reg_inf_Curve2IPCA_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IPCA_12mnths ~ Curve2_IPCA + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )

##### Sumário dos Modelos de Inflação 12 meses com erros padrão robustos HAC #####

summarySandwich(reg_inf_levelPre_12months_0, type = "hac")
summarySandwich(reg_inf_levelIPCA_12months_0, type = "hac")
summarySandwich(reg_inf_SlopePre_12months_0, type = "hac")
summarySandwich(reg_inf_SlopeIPCA_12months_0, type = "hac")
summarySandwich(reg_inf_CurvePre_12months_1, type = "hac")
summarySandwich(reg_inf_CurveIPCA_12months_1, type = "hac")

summarySandwich(reg_inf_Curve1Pre_12months_1, type = "hac")
summarySandwich(reg_inf_Curve2Pre_12months_1, type = "hac")
summarySandwich(reg_inf_Curve1IPCA_12months_1, type = "hac")
summarySandwich(reg_inf_Curve2IPCA_12months_1, type = "hac")

tab_model(reg_inf_levelPre_12months_0,
          reg_inf_levelIPCA_12months_0,
          reg_inf_SlopePre_12months_0,
          reg_inf_SlopeIPCA_12months_0,
          reg_inf_CurvePre_12months_1,
          reg_inf_CurveIPCA_12months_1,
          show.ci = FALSE,
          show.se = TRUE,
          vcov.fun = "hac")

##### Modelos de Atividade #####

# Modelo Atividade 6 meses e nível Pre

reg_atv_levelPre_6months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Level_Pre + selic + Tri_IBC_br)
reg_atv_levelPre_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Level_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1))
reg_atv_levelPre_6months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Level_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2))
reg_atv_levelPre_6months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Level_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3))
reg_atv_levelPre_6months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Level_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3) +
       lag(selic, n = 4) + lag(Tri_IBC_br, n = 4))

glance(reg_atv_levelPre_6months_0)
glance(reg_atv_levelPre_6months_1)
glance(reg_atv_levelPre_6months_2)
glance(reg_atv_levelPre_6months_3)
glance(reg_atv_levelPre_6months_4)


# Modelo Atividade 6 meses e nível IPCA

reg_atv_levelIPCA_6months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Level_IPCA + selic + Tri_IBC_br)
reg_atv_levelIPCA_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Level_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1))
reg_atv_levelIPCA_6months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Level_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2))
reg_atv_levelIPCA_6months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Level_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3))
reg_atv_levelIPCA_6months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Level_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3) +
       lag(selic, n = 4) + lag(Tri_IBC_br, n = 4))

glance(reg_atv_levelIPCA_6months_0)
glance(reg_atv_levelIPCA_6months_1)
glance(reg_atv_levelIPCA_6months_2)
glance(reg_atv_levelIPCA_6months_3)
glance(reg_atv_levelIPCA_6months_4)

# Modelo Atividade 6 meses e Inclinação Pre

reg_atv_SlopePre_6months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Slope_Pre + selic + Tri_IBC_br)
reg_atv_SlopePre_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Slope_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1))
reg_atv_SlopePre_6months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Slope_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2))
reg_atv_SlopePre_6months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Slope_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3))
reg_atv_SlopePre_6months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Slope_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3) +
       lag(selic, n = 4) + lag(Tri_IBC_br, n = 4))

glance(reg_atv_SlopePre_6months_0)
glance(reg_atv_SlopePre_6months_1)
glance(reg_atv_SlopePre_6months_2)
glance(reg_atv_SlopePre_6months_3)
glance(reg_atv_SlopePre_6months_4)

# Modelo Atividade 6 meses e Inclinação IPCA

reg_atv_SlopeIPCA_6months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Slope_IPCA + selic + Tri_IBC_br)
reg_atv_SlopeIPCA_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Slope_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1))
reg_atv_SlopeIPCA_6months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Slope_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2))
reg_atv_SlopeIPCA_6months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Slope_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3))
reg_atv_SlopeIPCA_6months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Slope_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3) +
       lag(selic, n = 4) + lag(Tri_IBC_br, n = 4))

glance(reg_inf_SlopeIPCA_6months_0)
glance(reg_inf_SlopeIPCA_6months_1)
glance(reg_inf_SlopeIPCA_6months_2)
glance(reg_inf_SlopeIPCA_6months_3)
glance(reg_inf_SlopeIPCA_6months_4)

# Modelo Atividade 6 meses e Curva Pre

reg_atv_CurvePre_6months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Curve_Pre + selic + Tri_IBC_br)
reg_atv_CurvePre_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Curve_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1))
reg_atv_CurvePre_6months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Curve_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2))
reg_atv_CurvePre_6months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Curve_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3))
reg_atv_CurvePre_6months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Curve_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3) +
       lag(selic, n = 4) + lag(Tri_IBC_br, n = 4))

glance(reg_inf_CurvePre_6months_0)
glance(reg_inf_CurvePre_6months_1)
glance(reg_inf_CurvePre_6months_2)
glance(reg_inf_CurvePre_6months_3)
glance(reg_inf_CurvePre_6months_4)

# Modelo Atividade 6 meses e Curva IPCA

reg_atv_CurveIPCA_6months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Curve_IPCA + selic + Tri_IBC_br)
reg_atv_CurveIPCA_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Curve_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1))
reg_atv_CurveIPCA_6months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Curve_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2))
reg_atv_CurveIPCA_6months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Curve_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3))
reg_atv_CurveIPCA_6months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Curve_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3) +
       lag(selic, n = 4) + lag(Tri_IBC_br, n = 4))

glance(reg_atv_CurveIPCA_6months_0)
glance(reg_atv_CurveIPCA_6months_1)
glance(reg_atv_CurveIPCA_6months_2)
glance(reg_atv_CurveIPCA_6months_3)
glance(reg_atv_CurveIPCA_6months_4)

reg_atv_Curve1Pre_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Curve1_Pre + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )
reg_atv_Curve2Pre_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Curve2_Pre + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )
reg_atv_Curve1IPCA_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Curve1_IPCA + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )
reg_atv_Curve2IPCA_6months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br6months ~ Curve2_IPCA + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )

##### Sumário dos Modelos de atividade 6 meses com erros padrão robustos HAC #####

summarySandwich(reg_atv_levelPre_6months_0, type = "hac")
summarySandwich(reg_atv_levelIPCA_6months_0, type = "hac")
summarySandwich(reg_atv_SlopePre_6months_0, type = "hac")
summarySandwich(reg_atv_SlopeIPCA_6months_1, type = "hac")
summarySandwich(reg_atv_CurvePre_6months_1, type = "hac")
summarySandwich(reg_atv_CurveIPCA_6months_0, type = "hac")

summarySandwich(reg_atv_Curve1Pre_6months_1, type = "hac")
summarySandwich(reg_atv_Curve2Pre_6months_1, type = "hac")
summarySandwich(reg_atv_Curve1IPCA_6months_1, type = "hac")
summarySandwich(reg_atv_Curve2IPCA_6months_1, type = "hac")

tab_model(reg_atv_levelPre_6months_0,
          reg_atv_levelIPCA_6months_0,
          reg_atv_SlopePre_6months_0,
          reg_atv_SlopeIPCA_6months_1,
          reg_atv_CurvePre_6months_1,
          reg_atv_CurveIPCA_6months_0,
          show.ci = FALSE,
          show.se = TRUE
          )

##### Modelos de Atividade 12 meses #####

# Modelo Atividade 12 meses e nível Pre

reg_atv_levelPre_12months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Level_Pre + selic + Tri_IBC_br)
reg_atv_levelPre_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Level_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1))
reg_atv_levelPre_12months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Level_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2))
reg_atv_levelPre_12months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Level_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3))
reg_atv_levelPre_12months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Level_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3) +
       lag(selic, n = 4) + lag(Tri_IBC_br, n = 4))

glance(reg_atv_levelPre_12months_0)
glance(reg_atv_levelPre_12months_1)
glance(reg_atv_levelPre_12months_2)
glance(reg_atv_levelPre_12months_3)
glance(reg_atv_levelPre_12months_4)


# Modelo Atividade 12 meses e nível IPCA

reg_atv_levelIPCA_12months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Level_IPCA + selic + Tri_IBC_br)
reg_atv_levelIPCA_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Level_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1))
reg_atv_levelIPCA_12months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Level_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2))
reg_atv_levelIPCA_12months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Level_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3))
reg_atv_levelIPCA_12months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Level_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3) +
       lag(selic, n = 4) + lag(Tri_IBC_br, n = 4))

glance(reg_atv_levelIPCA_12months_0)
glance(reg_atv_levelIPCA_12months_1)
glance(reg_atv_levelIPCA_12months_2)
glance(reg_atv_levelIPCA_12months_3)
glance(reg_atv_levelIPCA_12months_4)

# Modelo Atividade 12 meses e Inclinação Pre

reg_atv_SlopePre_12months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Slope_Pre + selic + Tri_IBC_br)
reg_atv_SlopePre_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Slope_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1))
reg_atv_SlopePre_12months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Slope_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2))
reg_atv_SlopePre_12months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Slope_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3))
reg_atv_SlopePre_12months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Slope_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3) +
       lag(selic, n = 4) + lag(Tri_IBC_br, n = 4))

glance(reg_atv_SlopePre_12months_0)
glance(reg_atv_SlopePre_12months_1)
glance(reg_atv_SlopePre_12months_2)
glance(reg_atv_SlopePre_12months_3)
glance(reg_atv_SlopePre_12months_4)

# Modelo Atividade 12 meses e Inclinação IPCA

reg_atv_SlopeIPCA_12months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Slope_IPCA + selic + Tri_IBC_br)
reg_atv_SlopeIPCA_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Slope_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1))
reg_atv_SlopeIPCA_12months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Slope_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IPCA, n = 1) +
       lag(selic, n = 2) + lag(Tri_IPCA, n = 2))
reg_atv_SlopeIPCA_12months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Slope_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3))
reg_atv_SlopeIPCA_12months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Slope_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3) +
       lag(selic, n = 4) + lag(Tri_IBC_br, n = 4))

glance(reg_inf_SlopeIPCA_12months_0)
glance(reg_inf_SlopeIPCA_12months_1)
glance(reg_inf_SlopeIPCA_12months_2)
glance(reg_inf_SlopeIPCA_12months_3)
glance(reg_inf_SlopeIPCA_12months_4)

# Modelo Atividade 12 meses e Curva Pre

reg_atv_CurvePre_12months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Curve_Pre + selic + Tri_IBC_br)
reg_atv_CurvePre_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Curve_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1))
reg_atv_CurvePre_12months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Curve_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2))
reg_atv_CurvePre_12months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Curve_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3))
reg_atv_CurvePre_12months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Curve_Pre + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3) +
       lag(selic, n = 4) + lag(Tri_IBC_br, n = 4))

glance(reg_inf_CurvePre_12months_0)
glance(reg_inf_CurvePre_12months_1)
glance(reg_inf_CurvePre_12months_2)
glance(reg_inf_CurvePre_12months_3)
glance(reg_inf_CurvePre_12months_4)

# Modelo Aticidade 12 meses e Curva IPCA

reg_atv_CurveIPCA_12months_0 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Curve_IPCA + selic + Tri_IBC_br)
reg_atv_CurveIPCA_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Curve_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1))
reg_atv_CurveIPCA_12months_2 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Curve_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2))
reg_atv_CurveIPCA_12months_3 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Curve_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3))
reg_atv_CurveIPCA_12months_4 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Curve_IPCA + selic + Tri_IBC_br +
       lag(selic, n = 1) + lag(Tri_IBC_br, n = 1) +
       lag(selic, n = 2) + lag(Tri_IBC_br, n = 2) + 
       lag(selic, n = 3) + lag(Tri_IBC_br, n = 3) +
       lag(selic, n = 4) + lag(Tri_IBC_br, n = 4))

glance(reg_atv_CurveIPCA_12months_0)
glance(reg_atv_CurveIPCA_12months_1)
glance(reg_atv_CurveIPCA_12months_2)
glance(reg_atv_CurveIPCA_12months_3)
glance(reg_atv_CurveIPCA_12months_4)

reg_atv_Curve1Pre_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Curve1_Pre + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )
reg_atv_Curve2Pre_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Curve2_Pre + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )
reg_atv_Curve1IPCA_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Curve1_IPCA + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )
reg_atv_Curve2IPCA_12months_1 <- dados_macro_tsir %>%
  lm(formula = Tri_IBC_br12months ~ Curve2_IPCA + selic + Tri_IPCA +
       dplyr::lag(selic, n = 1) +  dplyr::lag(Tri_IPCA, n = 1)
  )

##### Sumário dos Modelos de atividade 12 meses com erros padrão robustos HAC #####

summarySandwich(reg_atv_levelPre_12months_0, type = "hac")
summarySandwich(reg_atv_levelIPCA_12months_0, type = "hac")
summarySandwich(reg_atv_SlopePre_12months_0, type = "hac")
summarySandwich(reg_atv_SlopeIPCA_12months_0, type = "hac")
summarySandwich(reg_atv_CurvePre_12months_1, type = "hac")
summarySandwich(reg_atv_CurveIPCA_12months_0, type = "hac")

summarySandwich(reg_atv_Curve1Pre_12months_1, type = "hac")
summarySandwich(reg_atv_Curve2Pre_12months_1, type = "hac")
summarySandwich(reg_atv_Curve1IPCA_12months_1, type = "hac")
summarySandwich(reg_atv_Curve2IPCA_12months_1, type = "hac")

tab_model(reg_atv_levelPre_12months_0,
          reg_atv_levelIPCA_12months_0,
          reg_atv_SlopePre_12months_0,
          reg_atv_SlopeIPCA_12months_0,
          reg_atv_CurvePre_12months_1,
          reg_atv_CurveIPCA_12months_0,
          show.ci = FALSE,
          show.se = TRUE)

##### Modelos fatores nominais e risco #####

# Modelos com 0 defasagens

fd_LevelPre_Riscos_0 <- dados_macro_tsir %>%
  lm(formula = I(Level_Pre - dplyr::lag(Level_Pre)) ~ 
       DeltaLnVix + DeltaEmbi)
fd_SlopePre_Riscos_0 <- dados_macro_tsir %>%
  lm(formula = I(Slope_Pre - dplyr::lag(Slope_Pre)) ~ 
       DeltaLnVix + DeltaEmbi)
fd_CurvePre_Riscos_0 <- dados_macro_tsir %>%
  lm(formula = I(Curve_Pre - dplyr::lag(Curve_Pre)) ~ 
       DeltaLnVix + DeltaEmbi)
fd_Curve1Pre_Riscos_0 <- dados_macro_tsir %>%
  lm(formula = I(Curve1_Pre - dplyr::lag(Curve1_Pre)) ~ 
       DeltaLnVix + DeltaEmbi)
fd_Curve2Pre_Riscos_0 <- dados_macro_tsir %>%
  lm(formula = I(Curve2_Pre - dplyr::lag(Curve2_Pre)) ~ 
       DeltaLnVix + DeltaEmbi)

tab_model(fd_LevelPre_Riscos_0,
          fd_SlopePre_Riscos_0,
          fd_CurvePre_Riscos_0,
          fd_Curve1Pre_Riscos_0,
          fd_Curve2Pre_Riscos_0,
          show.ci = FALSE,
          show.se = TRUE,
          vcov.fun = "HAC", 
          digits = 4,
          show.aic = TRUE,
          terms = c("(Intercept)", "DeltaLnVix", "DeltaEmbi")
)

# Modelos com 1 defasagem
          
fd_LevelPre_Riscos_1 <- dados_macro_tsir %>%
  lm(formula = I(Level_Pre - dplyr::lag(Level_Pre)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Level_Pre) - dplyr::lag(Level_Pre, n = 2)))
fd_SlopePre_Riscos_1 <- dados_macro_tsir %>%
  lm(formula = I(Slope_Pre - dplyr::lag(Slope_Pre)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Slope_Pre) - dplyr::lag(Slope_Pre, n = 2)))
fd_CurvePre_Riscos_1 <- dados_macro_tsir %>%
  lm(formula = I(Curve_Pre - dplyr::lag(Curve_Pre)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Curve_Pre) - dplyr::lag(Curve_Pre, n = 2)))
fd_Curve1Pre_Riscos_1 <- dados_macro_tsir %>%
  lm(formula = I(Curve1_Pre - dplyr::lag(Curve1_Pre)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Curve1_Pre) - dplyr::lag(Curve1_Pre, n = 2)))
fd_Curve2Pre_Riscos_1 <- dados_macro_tsir %>%
  lm(formula = I(Curve2_Pre - dplyr::lag(Curve2_Pre)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Curve2_Pre) - dplyr::lag(Curve2_Pre, n = 2)))

tab_model(fd_LevelPre_Riscos_1,
          fd_SlopePre_Riscos_1,
          fd_CurvePre_Riscos_1,
          fd_Curve1Pre_Riscos_1,
          fd_Curve2Pre_Riscos_1,
          show.ci = FALSE,
          show.se = TRUE,
          vcov.fun = "HAC", 
          digits = 3, 
          show.aic = TRUE,
          terms = c("(Intercept)", "DeltaLnVix", "DeltaEmbi")
)

summarySandwich(fd_LevelPre_Riscos_1, "hac")
summarySandwich(fd_SlopePre_Riscos_1, "hac")
summarySandwich(fd_Curve1Pre_Riscos_1, "hac")
summarySandwich(fd_Curve2Pre_Riscos_1, "hac")


# Modelos com 2 defasagens

fd_LevelPre_Riscos_2 <- dados_macro_tsir %>%
  lm(formula = I(Level_Pre - dplyr::lag(Level_Pre)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Level_Pre) - dplyr::lag(Level_Pre, n = 2)) +
       I(dplyr::lag(Level_Pre, n = 2) - dplyr::lag(Level_Pre, n = 3))
  )
fd_SlopePre_Riscos_2 <- dados_macro_tsir %>%
  lm(formula = I(Slope_Pre - dplyr::lag(Slope_Pre)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Slope_Pre) - dplyr::lag(Slope_Pre, n = 2)) +
       I(dplyr::lag(Slope_Pre, n = 2) - dplyr::lag(Slope_Pre, n = 3))
  )
fd_CurvePre_Riscos_2 <- dados_macro_tsir %>%
  lm(formula = I(Curve_Pre - dplyr::lag(Curve_Pre)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Curve_Pre) - dplyr::lag(Curve_Pre, n = 2)) +
       I(dplyr::lag(Curve_Pre, n = 2) - dplyr::lag(Curve_Pre, n = 3))
  )
fd_Curve1Pre_Riscos_2 <- dados_macro_tsir %>%
  lm(formula = I(Curve1_Pre - dplyr::lag(Curve1_Pre)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Curve1_Pre) - dplyr::lag(Curve1_Pre, n = 2)) +
       I(dplyr::lag(Curve1_Pre, n = 2) - dplyr::lag(Curve1_Pre, n = 3))
  )
fd_Curve2Pre_Riscos_2 <- dados_macro_tsir %>%
  lm(formula = I(Curve2_Pre - dplyr::lag(Curve2_Pre)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Curve2_Pre) - dplyr::lag(Curve2_Pre, n = 2)) +
       I(dplyr::lag(Curve2_Pre, n = 2) - dplyr::lag(Curve2_Pre, n = 3))
  )

tab_model(fd_LevelPre_Riscos_2,
          fd_SlopePre_Riscos_2,
          fd_CurvePre_Riscos_2,
          fd_Curve1Pre_Riscos_2,
          fd_Curve2Pre_Riscos_2,
          show.ci = FALSE,
          show.se = TRUE,
          vcov.fun = "HAC", 
          digits = 4, 
          show.aic = TRUE,
          terms = c("(Intercept)", "DeltaLnVix", "DeltaEmbi")
)

summarySandwich(fd_LevelPre_Riscos_2, "hac")
summarySandwich(fd_SlopePre_Riscos_2, "hac")
summarySandwich(fd_Curve1Pre_Riscos_2, "hac")
summarySandwich(fd_Curve2Pre_Riscos_2, "hac")

glance(fd_LevelPre_Riscos_2)
glance(fd_LevelPre_Riscos_1)

##### Modelos fatores reais e risco #####

# Modelos com 0 defasagens

fd_LevelIPCA_Riscos_0 <- dados_macro_tsir %>%
  lm(formula = I(Level_IPCA - dplyr::lag(Level_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi)
fd_SlopeIPCA_Riscos_0 <- dados_macro_tsir %>%
  lm(formula = I(Slope_IPCA - dplyr::lag(Slope_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi)
fd_CurveIPCA_Riscos_0 <- dados_macro_tsir %>%
  lm(formula = I(Curve_IPCA - dplyr::lag(Curve_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi)
fd_Curve1IPCA_Riscos_0 <- dados_macro_tsir %>%
  lm(formula = I(Curve1_IPCA - dplyr::lag(Curve1_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi)
fd_Curve2IPCA_Riscos_0 <- dados_macro_tsir %>%
  lm(formula = I(Curve2_IPCA - dplyr::lag(Curve2_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi)

tab_model(fd_LevelIPCA_Riscos_0,
          fd_SlopeIPCA_Riscos_0,
          fd_CurveIPCA_Riscos_0,
          fd_Curve1IPCA_Riscos_0,
          fd_Curve2IPCA_Riscos_0,
          show.ci = FALSE,
          show.se = TRUE,
          vcov.fun = "HAC", 
          digits = 4,
          show.aic = TRUE,
          terms = c("(Intercept)", "DeltaLnVix", "DeltaEmbi")
)

# Modelos com 1 defasagem

fd_LevelIPCA_Riscos_1 <- dados_macro_tsir %>%
  lm(formula = I(Level_IPCA - dplyr::lag(Level_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Level_IPCA) - dplyr::lag(Level_IPCA, n = 2)))
fd_SlopeIPCA_Riscos_1 <- dados_macro_tsir %>%
  lm(formula = I(Slope_IPCA - dplyr::lag(Slope_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Slope_IPCA) - dplyr::lag(Slope_IPCA, n = 2)))
fd_CurveIPCA_Riscos_1 <- dados_macro_tsir %>%
  lm(formula = I(Curve_IPCA - dplyr::lag(Curve_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Curve_IPCA) - dplyr::lag(Curve_IPCA, n = 2)))
fd_Curve1IPCA_Riscos_1 <- dados_macro_tsir %>%
  lm(formula = I(Curve1_IPCA - dplyr::lag(Curve1_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Curve1_IPCA) - dplyr::lag(Curve1_IPCA, n = 2)))
fd_Curve2IPCA_Riscos_1 <- dados_macro_tsir %>%
  lm(formula = I(Curve2_IPCA - dplyr::lag(Curve2_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Curve2_IPCA) - dplyr::lag(Curve2_IPCA, n = 2)))

tab_model(fd_LevelIPCA_Riscos_1,
          fd_SlopeIPCA_Riscos_1,
          fd_CurveIPCA_Riscos_1,
          fd_Curve1IPCA_Riscos_1,
          fd_Curve2IPCA_Riscos_1,
          show.ci = FALSE,
          show.se = TRUE,
          vcov.fun = "HAC", 
          digits = 4, 
          show.aic = TRUE,
          terms = c("(Intercept)", "DeltaLnVix", "DeltaEmbi")
)

# Modelos com 2 defasagens

fd_LevelIPCA_Riscos_2 <- dados_macro_tsir %>%
  lm(formula = I(Level_IPCA - dplyr::lag(Level_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Level_IPCA) - dplyr::lag(Level_IPCA, n = 2)) +
       I(dplyr::lag(Level_IPCA, n = 2) - dplyr::lag(Level_IPCA, n = 3))
  )
fd_SlopeIPCA_Riscos_2 <- dados_macro_tsir %>%
  lm(formula = I(Slope_IPCA - dplyr::lag(Slope_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Slope_IPCA) - dplyr::lag(Slope_IPCA, n = 2)) +
       I(dplyr::lag(Slope_IPCA, n = 2) - dplyr::lag(Slope_IPCA, n = 3))
  )
fd_CurveIPCA_Riscos_2 <- dados_macro_tsir %>%
  lm(formula = I(Curve_IPCA - dplyr::lag(Curve_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Curve_IPCA) - dplyr::lag(Curve_IPCA, n = 2)) +
       I(dplyr::lag(Curve_IPCA, n = 2) - dplyr::lag(Curve_IPCA, n = 3))
  )
fd_Curve1IPCA_Riscos_2 <- dados_macro_tsir %>%
  lm(formula = I(Curve1_IPCA - dplyr::lag(Curve1_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Curve1_IPCA) - dplyr::lag(Curve1_IPCA, n = 2)) +
       I(dplyr::lag(Curve1_IPCA, n = 2) - dplyr::lag(Curve1_IPCA, n = 3))
  )
fd_Curve2IPCA_Riscos_2 <- dados_macro_tsir %>%
  lm(formula = I(Curve2_Pre - dplyr::lag(Curve2_IPCA)) ~ 
       DeltaLnVix + DeltaEmbi +
       I(dplyr::lag(Curve2_IPCA) - dplyr::lag(Curve2_IPCA, n = 2)) +
       I(dplyr::lag(Curve2_IPCA, n = 2) - dplyr::lag(Curve2_IPCA, n = 3))
  )

tab_model(fd_LevelIPCA_Riscos_2,
          fd_SlopeIPCA_Riscos_2,
          fd_CurveIPCA_Riscos_2,
          fd_Curve1IPCA_Riscos_2,
          fd_Curve2IPCA_Riscos_2,
          show.ci = FALSE,
          show.se = TRUE,
          vcov.fun = "HAC", 
          digits = 4, 
          show.aic = TRUE,
          terms = c("(Intercept)", "DeltaLnVix", "DeltaEmbi")
)

##### Modelos de Câmbio #####

fd_FX_Risco <- dados_macro_tsir %>%
  lm(formula = I(LnFX - dplyr::lag(LnFX)) ~
       DeltaLnVix + DeltaEmbi
  )
fd_FX_Risco_LevelPre <- dados_macro_tsir %>%
  lm(formula = I(LnFX - dplyr::lag(LnFX)) ~
       DeltaLnVix + DeltaEmbi +
       I(Level_Pre - dplyr::lag(Level_Pre))
  )  
fd_FX_Risco_LevelIPCA <- dados_macro_tsir %>%
  lm(formula = I(LnFX - dplyr::lag(LnFX)) ~
       DeltaLnVix + DeltaEmbi +
       I(Level_IPCA - dplyr::lag(Level_IPCA))
  )  
fd_FX_Risco_SlopePre <- dados_macro_tsir %>%
  lm(formula = I(LnFX - dplyr::lag(LnFX)) ~
       DeltaLnVix + DeltaEmbi +
       I(Slope_Pre - dplyr::lag(Slope_Pre))
  )  
fd_FX_Risco_SlopeIPCA <- dados_macro_tsir %>%
  lm(formula = I(LnFX - dplyr::lag(LnFX)) ~
       DeltaLnVix + DeltaEmbi +
       I(Slope_IPCA - dplyr::lag(Slope_IPCA))
  )  
fd_FX_Risco_Curve1Pre <- dados_macro_tsir %>%
  lm(formula = I(LnFX - dplyr::lag(LnFX)) ~
       DeltaLnVix + DeltaEmbi +
       I(Curve1_Pre - dplyr::lag(Curve1_Pre))
  ) 
fd_FX_Risco_Curve1IPCA <- dados_macro_tsir %>%
  lm(formula = I(LnFX - dplyr::lag(LnFX)) ~
       DeltaLnVix + DeltaEmbi +
       I(Curve1_IPCA - dplyr::lag(Curve1_IPCA))
  ) 
fd_FX_Risco_Curve2Pre <- dados_macro_tsir %>%
  lm(formula = I(LnFX - dplyr::lag(LnFX)) ~
       DeltaLnVix + DeltaEmbi +
       I(Curve2_Pre - dplyr::lag(Curve2_Pre))
  ) 
fd_FX_Risco_Curve2IPCA <- dados_macro_tsir %>%
  lm(formula = I(LnFX - dplyr::lag(LnFX)) ~
       DeltaLnVix + DeltaEmbi +
       I(Curve2_IPCA - dplyr::lag(Curve2_IPCA))
  ) 

tab_model(fd_FX_Risco_LevelPre,
          fd_FX_Risco_LevelIPCA,
          fd_FX_Risco_SlopePre,
          fd_FX_Risco_SlopeIPCA,
          show.ci = FALSE,
          show.se = TRUE,
          vcov.fun = "HAC", 
          digits = 3, 
          show.aic = TRUE
)

summarySandwich(fd_FX_Risco_LevelPre, "hac")
summarySandwich(fd_FX_Risco_LevelIPCA, "hac")
summarySandwich(fd_FX_Risco_SlopePre , "hac")
summarySandwich(fd_FX_Risco_SlopeIPCA, "hac")

tab_model(fd_FX_Risco_Curve1Pre,
          fd_FX_Risco_Curve1IPCA,
          fd_FX_Risco_Curve2Pre,
          fd_FX_Risco_Curve2IPCA,
          show.ci = FALSE,
          show.se = TRUE,
          vcov.fun = "HAC", 
          digits = 3, 
          show.aic = TRUE
)

summarySandwich(fd_FX_Risco_Curve1Pre, "hac")
summarySandwich(fd_FX_Risco_Curve1IPCA, "hac")
summarySandwich(fd_FX_Risco_Curve2Pre, "hac")
summarySandwich(fd_FX_Risco_Curve2IPCA, "hac")

##### Modelos FX com todos os Fatores #####

fd_FX_Risco_FatoresPre <- dados_macro_tsir %>%
  lm(formula = I(LnFX - dplyr::lag(LnFX)) ~
       DeltaLnVix + DeltaEmbi +
       I(Level_Pre - dplyr::lag(Level_Pre)) +
       I(Slope_Pre - dplyr::lag(Slope_Pre)) +
       I(Curve1_Pre - dplyr::lag(Curve1_Pre)) +
       I(Curve2_Pre - dplyr::lag(Curve2_Pre))
  )

fd_FX_Risco_FatoresIPCA <- dados_macro_tsir %>%
  lm(formula = I(LnFX - dplyr::lag(LnFX)) ~
       DeltaLnVix + DeltaEmbi +
       I(Level_IPCA - dplyr::lag(Level_IPCA)) +
       I(Slope_IPCA - dplyr::lag(Slope_IPCA)) +
       I(Curve1_IPCA - dplyr::lag(Curve1_IPCA)) +
       I(Curve2_IPCA - dplyr::lag(Curve2_IPCA))
  )

tab_model(fd_FX_Risco_FatoresPre,
          fd_FX_Risco_FatoresIPCA,
          show.ci = FALSE,
          show.se = TRUE,
          vcov.fun = "HAC", 
          digits = 3
          )

summarySandwich(fd_FX_Risco_FatoresPre, "hac")
summarySandwich(fd_FX_Risco_FatoresIPCA, "hac")
