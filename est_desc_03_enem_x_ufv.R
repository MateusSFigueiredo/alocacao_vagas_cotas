# ==============================================================================
# Arquivo: est_desc_03_enem_x_ufv.R
#
# Quantos inscritos tem na UFV e ENEM?
# Qual a correlação n. inscritos na UFV e n. inscritos no ENEM por ano?
#
# Modificado em: 2023-05-23.
# Autor: Mateus Silva Figueiredo
# ==============================================================================
# Preparação
library(ggplot2)# gráficos
# install.packages("ggrepel")
library(ggrepel) # gráficos

# ==============================================================================
# Carregar dados_ufv

setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")

# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- F   # deseja separar candidatos por curso? obrigatório
por_ano   <- F   # deseja separar candidatos por ano? opcional
# source("data_04_carregar_dados_UFV.R") # cria ~10 objetos

# limpeza após source
if (!por_curso){
  rm(cursos_cca,cursos_ccb,cursos_cce,cursos_cch,cursos_crp,cursos_caf)
  rm(lista_cursos,lista_cursos_18_22,lista_cursos_estavel,lista_cursos_mudou)
}

# ==============================================================================
# objetos necessários

sisu_anos<-paste0("SISU",c(2013:2022))

# ==============================================================================
# Analisando por ano
# Criar dados_ano
# dados_ufv[which (Processo_Seletivo==sisu_anos[i])] -> dados_ano

# criar data.frame n_inscritos
n_inscritos <- data.frame(sisu_anos=sisu_anos,
                          inscritos_UFV=NA,
                          inscritos_ENEM_n_1=c(5791332,  #ENEM 2012 = SISU 2013
                                               7173574,  #ENEM 2013 = SISU 2014
                                               8722290,  #ENEM 2014 = SISU 2015
                                               7792025,  #ENEM 2015 = SISU 2016
                                               8627371,  #ENEM 2016 = SISU 2017
                                               6731136,  #ENEM 2017 = SISU 2018
                                               5513662,  #ENEM 2018 = SISU 2019
                                               5095308,  #ENEM 2019 = SISU 2020
                                               5783357,  #ENEM 2020 = SISU 2021
                                               4004764), #ENEM 2021 = SISU 2022
                          taxa_ENEM=c(35, # ENEM 2011 = SISU 2012
                                      35,35,63,68,82,
                                      82,85,85,
                                      85)) # ENEM 2021 = SISU 2022

# Fonte: Wikipédia - "Exame Nacional do Ensino Médio". Consultado em 2022-12-18.
# Dados de inscritos do ENEM de um ano anterior ao SISU da UFV.
# Ou seja, dados do ENEM 2011 comparados dom dados do SISU 2012.

# preencher n de inscritos na UFV por ano
for (i in 1:10){
  dados_ano <- dados_ufv[which (Processo_Seletivo==sisu_anos[i])] # separa por ano
  n_inscritos[i,2] <- dados_ano %>% nrow()} # quantos inscritos tem por ano?
rm(dados_ano) # usado no loop, não precisa mais

n_inscritos$inscritos_UFV %>% sum()
n_inscritos$inscritos_ENEM %>% sum()

# ==============================================================================

# calcula lm

# summary com R quadrado e significância
lm(n_inscritos$inscritos_UFV~n_inscritos$inscritos_ENEM_n_1) %>% summary()
(lm(n_inscritos$inscritos_UFV~n_inscritos$inscritos_ENEM_n_1) %>% summary())$r.squared
(lm(n_inscritos$inscritos_UFV~n_inscritos$inscritos_ENEM_n_1) %>% summary())$coefficients[,4][2]  



# ENEM x UFV Usando ggplot

n_inscritos$inscritos_ENEM/1e+6 -> inscritos_ENEM
n_inscritos$inscritos_UFV -> inscritos_UFV

# fazer texto do p valor
(lm(n_inscritos$inscritos_UFV~n_inscritos$inscritos_ENEM_n_1) %>% summary())$coefficients[,4][2] %>%
  as.numeric() %>% round(5) -> p.valor
paste("p valor =",p.valor) -> p.valor

# fazer texto da formula
(lm(n_inscritos$inscritos_UFV~n_inscritos$inscritos_ENEM_n_1) %>% summary())$coefficients[,1][1] %>%
  as.numeric() %>% round(5) -> intercepto
(lm(n_inscritos$inscritos_UFV~n_inscritos$inscritos_ENEM_n_1) %>% summary())$coefficients[,1][2] %>%
  as.numeric() %>% round(5) -> slope

formula <- paste0("y = ", intercepto," + (",slope," * x)")
formula

# ------------------------------------------------------------------------------

# ==============================================================================

# Plot gráfico Inscritos no ENEM x Inscritos na UFV

set.seed(10) # usado por geom_text_repel

ggplot(mapping = aes(n_inscritos$inscritos_ENEM_n_1/1e+6,n_inscritos$inscritos_UFV)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  annotate("text", x = 8, y = 10700, label = "Curva: LM", colour = "blue") +
  annotate("text", x = 8, y = 8500, label = p.valor, colour = "blue") +
  annotate("text", x = 8, y = 9600, label = formula, colour = "blue") +
  
  xlim(4.0,8.8) +
  #  ylim(5000,25000) +
  scale_y_continuous(labels = scales::label_comma(big.mark=".",decimal.mark=",")) +
  
  # geom_text_repel para colocar ano em cada ponto
  geom_text_repel(aes(x = n_inscritos$inscritos_ENEM_n_1/1e+6 + 0.0,
                                      y = inscritos_UFV - 000,
                      label=n_inscritos$sisu_anos)) +
  
  # xlab e ylab
  xlab("Inscritos no ENEM anterior (milhões)") +
  ylab("Inscritos na UFV") 

