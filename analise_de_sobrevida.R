#### análise de sobrevida dos estudantes Economia/UFF

### PACOTES UTILIZADOS ####

packages <- c('tidyverse', 'readx1', 'survival', 'devtools', 'usethis',
              'survminer', 'lubridate','ggplot','survminer','tidymodels')

for(package in packages){
  if(!(package %in% rownames(installed.packages()))){
    install.packages(package)
    library(package)
  } else{ library(package)}
}

install_github("adibender/ldatools", force = TRUE)
library(ldatools)

#### CHAMANDO O BANCO ####

setwd('C:\\Users\\Isabella Correa\\Downloads')

dados <- read_dta('estudantes_total2.dta')

#### DADOS ####

# Estimador de Kaplan-Meier

#### Analise de Permanencia ####
permanencia <- Surv(dados$numero_semestres, dados$estudantes_total)

# Estimando a curva de sobrevivencia pelo Kaplan-Meier


#### raça survival ----

raca_km <- survfit(permanencia ~ raca, data = dados)

# plot raça -

ggsurvplot(
  fit = raca_km, 
  legend.labs = c(
    "Pretos, pardos ou indígenas",
    "Brancos ou amarelos"),
  xlab = "Semestres", 
  ylab = "Probabilidade de sobrevivência",
  ggtheme = theme_minimal(), conf.int = TRUE, palette = c("#E7B800", "#2E9FDF"), linetypepval=TRUE, pval=TRUE) 

# sexo survival ----

sexo_km <- survfit(permanencia ~ sexo, data = dados)
summary(sexo_km)

# plot sexo

ggsurvplot(
  fit = sexo_km, 
  legend.labs = c(
    "Mulheres",
    "Homens"),
  xlab = "Semestres", 
  ylab = "Probabilidade de sobrevivência",
  ggtheme = theme_minimal(), conf.int = TRUE, palette = c("#E7B800", "#2E9FDF"), pval=TRUE) 

# EM survival ----

em_km <- survfit(permanencia ~ ensino_medio, data = dados)

# plot ensino medio

ggsurvplot(
  fit = em_km, 
  legend.labs = c(
    "Ensino médio privado",
    "Ensino médio público"),
  xlab = "Semestres", 
  ylab = "Probabilidade de sobrevivência",
  ggtheme = theme_minimal(), conf.int = TRUE, palette = c("#E7B800", "#2E9FDF"), pval=TRUE) 

# cota survival ----

cota_km <- survfit(permanencia ~ modalidade_ingresso, data = dados)

# plot cota

ggsurvplot(
  fit = cota_km, 
  legend.labs = c(
    "Ação afirmativa",
    "Ampla concorrência"),
  xlab = "Semestres", 
  ylab = "Probabilidade de sobrevivência",
  ggtheme = theme_minimal(), conf.int = TRUE, palette = c("#E7B800", "#2E9FDF"), pval=TRUE) 

# DESEMPENHO



# cr survival ####

cr_km <- survfit(permanencia ~ cr, data = dados)

# plot cota

ggsurvplot(
  fit = cr_km, 
  legend.labs = c(
    "0.00 - 4.99",
    "5.00 - 6.99",
    "7.00 - 10.00"),
  xlab = "Semestres", 
  ylab = "Probabilidade de sobrevivência",
  ggtheme = theme_minimal(), conf.int = TRUE, pval=TRUE) 

###########################################

#### Estimador COX ####

#### COX para permanencia ####
cox <- survival::coxph(permanencia ~ ensino_medio +
                         modalidade_ingresso +
                         sexo +
                         raca +
                         cr,
                       data = dados)
summary(cox)

ggcoxdiagnostics(cox, type = "deviance",
                 linear.predictions = FALSE,
                 ggtheme = theme_minimal())


#### paramétrico ####
fit <- survreg(Surv(numero_semestres, estudantes_total) ~ 
                 ensino_medio + modalidade_ingresso +
                 sexo + raca + cr , data = dados, 
               dist = "exponential")
summary(fit)
plot(fit)
