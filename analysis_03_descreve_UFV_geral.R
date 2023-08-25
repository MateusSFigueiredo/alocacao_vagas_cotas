# ==============================================================================
# Arquivo: analysis_03_descreve_UFV_geral.R
#
# Quantos cursos tem? Quantos são estáveis?
# Quantos inscrtiso tem?
# Qual a correlação n. inscritos na UFV e n. inscritos no ENEM por ano?
#
# Modificado em: 2022-12-10.
# Autor: Mateus Silva Figueiredo
# ==============================================================================
#
# Usar após data_04_carregar_dados_UFV.R ter carregado dados por curso
#
# Dicionário
#
# Inputs:
# dados_ufv com todos os cursos de todos os anos 2013-2022
# dados_2013 até dados_2022 com todos os cursos em cada
# dados_2013 em diante tem colunas id, nota, mod_ins, mod_con,
# Processo_Seletivo e Curso
# dados_curso
lista_cursos
lista_cursos_estavel
#
# Outputs:
# Gráficos
# Tabelas
#
# ==============================================================================
# Preparação
library(ggplot2)# gráficos
# install.packages("ggrepel")
library(ggrepel) # gráficos
# install.packages("scales")
library(scales)


belch2 <- function(x, y) { eval(parse(text=(paste0(x, y, sep=""))))}
belch3 <- function(x, y, z) {eval(parse(text=(paste0(x, y, z,sep=""))))}

mod <- c("A0","L01","L02","L05","L06", "L09", "L10", "L13", "L14")
mod_cotas <- mod[mod!= "A0"] # cria mod_cotas sem A0

anos<-c(2013:2022)
sisu_anos<-paste0("SISU",c(2013:2022))

# ==============================================================================
# Analisando por ano
# Criar dados_ano
# i <- 1
# dados_ufv[which (Processo_Seletivo==sisu_anos[i])] -> dados_ano

# criar data.frame n_inscritos
n_inscritos <- data.frame(
  sisu_anos=sisu_anos,
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
                                      85),
                                      anos=2013:2022) # ENEM 2021 = SISU 2022

# Fonte: Wikipédia - "Exame Nacional do Ensino Médio". Consultado em 2022-12-18.
# Dados de inscritos do ENEM de um ano anterior ao SISU da UFV.
# Ou seja, dados do ENEM 2011 comparados dom dados do SISU 2012.

# preencher
for (i in 1:10){
  dados_ufv[which (Processo_Seletivo==sisu_anos[i])] -> dados_ano
  dados_ano %>% nrow() -> n_inscritos[i,2]}
# ==============================================================================
# Qual modelo estatístico usar?

# inscritos_UFV por ano
plot(inscritos_UFV ~ anos, data = n_inscritos)

# inscritos_ENEM por ano
plot(inscritos_ENEM ~ anos, data = n_inscritos)

# inscritos_UFV x inscritos_ENEM_n_1
lm(inscritos_UFV ~ inscritos_ENEM_n_1, data = n_inscritos)
lm(n_inscritos$inscritos_UFV~n_inscritos$inscritos_ENEM_n_1) %>% summary()
plot(inscritos_UFV ~ inscritos_ENEM_n_1, data = n_inscritos)
lm(n_inscritos$inscritos_UFV~n_inscritos$inscritos_ENEM_n_1) %>% 
  abline(col="red",lty=2)


# ==============================================================================
# Gráficos

#-------------------------------------------------------------------------------
# Inscritos na UFV pelo tempo
plot(n_inscritos$inscritos_UFV~c(2013:2022))

ggplot(mapping = aes(anos,n_inscritos$inscritos_UFV)) +
  geom_point() +
  ylab("Inscritos na UFV") +
  xlab("Edições do SISU") +
  geom_smooth(method = "gam", se=T) + annotate("text", x = 2013.5, y = 10700, label = "Curva: GAM", colour = "blue") +
  #  geom_smooth(method = "lm", se=F) +
  scale_x_continuous(breaks=seq(2013,2022,1)) +
  scale_y_continuous(breaks=seq(10000,25000,2500),
                     labels = scales::label_comma(big.mark=".",decimal.mark=","))

#-----------
for(i in 1:10){
  (n_inscritos$inscritos_UFV[i]/n_inscritos$inscritos_UFV[i-1]) %>% print()
}
# a cada ano que passa, a UFV tem cerca de 90% dos inscritos do ano anterior

#-------------------------------------------------------------------------------
#Inscritos na UFV x inscritos no ENEM
plot(n_inscritos$inscritos_UFV~n_inscritos$inscritos_ENEM)

{
  plot(y=n_inscritos$inscritos_UFV,
       x=n_inscritos$inscritos_ENEM_n_1,
       xlim=range((2.8*10^6):(10*10^6)),
       ylim=range((8000):(23000)),
       xlab="Inscritos no ENEM anterior",
       ylab="Inscritos na UFV",
       col="black"
  )
  
  text(y=n_inscritos$inscritos_UFV,
       x=n_inscritos$inscritos_ENEM_n_1,
       pos=4, # 4 = direita.
       labels=n_inscritos$anos)
}
lm(n_inscritos$inscritos_UFV~n_inscritos$inscritos_ENEM_n_1) %>% 
  abline(col="red",lty=2)

# summary com R quadrado e significância
lm(n_inscritos$inscritos_UFV~n_inscritos$inscritos_ENEM) %>% summary()
(lm(n_inscritos$inscritos_UFV~n_inscritos$inscritos_ENEM) %>% summary())$r.squared
(lm(n_inscritos$inscritos_UFV~n_inscritos$inscritos_ENEM) %>% summary())$coefficients[,4][2]  

#-------------------------------------------------------------------------------
# ENEM x UFV Usando ggplot

n_inscritos$inscritos_ENEM/1e+6 -> inscritos_ENEM
n_inscritos$inscritos_UFV -> inscritos_UFV

(lm(n_inscritos$inscritos_UFV~n_inscritos$inscritos_ENEM) %>% summary())$coefficients[,4][2] %>%
  as.numeric() %>% round(5) -> p.valor
p.valor 
paste("p valor =",p.valor) -> p.valor

# ------------------------------------------------------------------------------

# Plot melhor até agora - usando geom_text_repel
ggplot(mapping = aes(n_inscritos$inscritos_ENEM/1e+6,n_inscritos$inscritos_UFV)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  annotate("text", x = 8, y = 10700, label = "Curva: LM", colour = "blue") +
  annotate("text", x = 8, y = 9600, label = p.valor, colour = "blue") +
  # geom_smooth(method = "gam", se=T) + annotate("text", x = 8, y = 10700, label = "Curva: GAM", colour = "blue") +
  xlim(4.0,8.9) +
  #  ylim(5000,25000) +
  scale_y_continuous(labels = scales::label_comma(big.mark=".",decimal.mark=",")) +

    # Add geom_text = ano de cada ponto
  # ajuste simples, meio ruim:
#  geom_text(label = n_inscritos$anos, vjust=1.2) +

    # O mesmo bom ajuste para todos, com geom_text_repel:
  geom_text_repel(label = n_inscritos$anos) + 
  
  # xlab e ylab
  xlab("Inscritos no ENEM anterior (milhões)") +
  ylab("Inscritos na UFV") 

# ------------------------------------------------------------------------------

# Plot melhor até agora - testando posição do geom_text
ggplot(mapping = aes(n_inscritos$inscritos_ENEM/1e+6,n_inscritos$inscritos_UFV)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  annotate("text", x = 8, y = 10700, label = "Curva: LM", colour = "blue") +
  annotate("text", x = 8, y = 9600, label = p.valor, colour = "blue") +
  # geom_smooth(method = "gam", se=T) + annotate("text", x = 8, y = 10700, label = "Curva: GAM", colour = "blue") +
  xlim(4.0,8.9) +
  #  ylim(5000,25000) +
  scale_y_continuous(labels = scales::label_comma(big.mark=".",decimal.mark=",")) +
  
  # Add geom_text = ano de cada ponto
  geom_text(label = n_inscritos$anos=="SISU2013", vjust=1.2) +

  # # Adjust specific text labels
  # geom_text(data = n_inscritos[which(n_inscritos$anos == "SISU2020"), ],
  #           aes(x = n_inscritos$inscritos_ENEM / 1e+6, y = n_inscritos$inscritos_UFV),
  #           label = n_inscritos$anos, vjust = 1.5, hjust = 0, color = "black") +
  
  geom_text(data = n_inscritos[which(n_inscritos$anos == "SISU2020"), ],
            vjust = 1.5, hjust = -0.2, color = "black")
  
  # xlab e ylab
  xlab("Inscritos no ENEM anterior (milhões)") +
  ylab("Inscritos na UFV") 

# ------------------------------------------------------------------------------

# Plot com escala de 4.000.000 até 8.000.000. xlim ignorado por scale_x
ggplot(mapping = aes((n_inscritos$inscritos_ENEM),n_inscritos$inscritos_UFV)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlim(3.3*10^6,9*10^6) +
  geom_text(label = n_inscritos$anos, vjust=1.5) +
  #  ggtitle("Título no canto") +
  scale_y_continuous(labels = scales::label_comma(big.mark=".",decimal.mark=",")) +
  scale_x_continuous(labels = scales::label_comma(big.mark=".",decimal.mark=",")) +
  xlab("Inscritos no ENEM (milhões)") +
  ylab("Inscritos na UFV")

# Plot com escala de 4 até 8 milhões
ggplot(mapping = aes((n_inscritos$inscritos_ENEM),n_inscritos$inscritos_UFV)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlim(3.3*10^6,9.6*10^6) +
  geom_text(label = n_inscritos$anos, vjust=1.5) +
  xlab("Inscritos no ENEM (milhões)") +
  ylab("Inscritos na UFV") +
  #  ggtitle("Título no canto") +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e0, big.mark=".",decimal.mark=",")) +
  scale_x_continuous(labels = unit_format(unit = "", scale = 1e-6, big.mark=".",decimal.mark=","))

#-------------------------------------------------------------------------------
# Inscritos ENEM x Taxa ENEM
{
  plot(n_inscritos$inscritos_ENEM~n_inscritos$taxa_ENEM,
       xlim=range(20:100),
       ylim=range((3*10^6):(10*10^6)),
       xlab = "Taxa do ENEM",
       ylab = "Inscritos no ENEM"
  )
  
  text(y=n_inscritos$inscritos_ENEM,
       x=n_inscritos$taxa_ENEM,
       pos=2, # 3 = direita.
       labels=n_inscritos$anos)
  
  lm(n_inscritos$inscritos_ENEM~n_inscritos$taxa_ENEM) %>% abline(col="red",lty=2)
}

#-------------------------------------------------------------------------------
# Anos no eixo x
# Inscritos na UFV no eixo y 1
# Inscritos no ENEM no eixo y 2

plot(n_inscritos$inscritos_UFV~c(2013:2022),
     col="blue",pch=20,cex=1,
     xlab="não recomendado")
points(n_inscritos$inscritos_ENEM_n_1/400~c(2013:2022),
       col="red",pch=15)

# n_inscritos$inscritos_UFV %>% max()
# n_inscritos$inscritos_UFV %>% min()
# n_inscritos$inscritos_ENEM_n_1 %>% max()
# n_inscritos$inscritos_ENEM_n_1 %>% min()

legend(2020, 20000, legend=c("UFV", "ENEM"),
       col=c("blue", "red"), pch=c(20,15))

# -------------------------------------------------
# ENEM no eixo y da direita
# UFV no eixo y da esquerda


library(ggplot2)

n_inscritos$anos<-2013:2022

# Create the plot
plot <- ggplot(n_inscritos, aes(x = anos)) +
  geom_line(aes(y = inscritos_UFV, color = "UFV"), size = 1.5) +
  geom_line(aes(y = inscritos_ENEM_n_1/400, color = "ENEM -1"), size = 1.5) +
  scale_x_continuous(
    breaks = seq(2, max(n_inscritos$anos), by = 2),
    labels = seq(2, max(n_inscritos$anos), by = 2)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / 1000000*400, name = "Inscritos no ENEM (milhões)"),
    name = "Inscritos na UFV"
  ) +
  labs(x = "Years") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Display the plot
plot



#===============================================================================
# Análise em texto:
paste(
  dados_ano %>% nrow()
  ,"= número total de inscritos em",sisu_anos[i])
paste(
  dados_ano[which (mod_con == "NA")] %>% nrow()
  ,"= número total de inscritos não convocados em",sisu_anos[i])

# ==============================================================================
# Analisando informações sobre cursos
# Obtendo informações com print de textos

# Quantos cursos de cada campus?
# Em todos os dados
paste(length(lista_cursos),"cursos ao todo") # 70

paste(sum(endsWith(lista_cursos,"_RP")), "cursos em Rio Paranaíba") # 12

paste(sum(endsWith(lista_cursos,"_FL")), "cursos em Florestal") # 10

paste(length(lista_cursos)
      -sum(endsWith(lista_cursos,"_RP"))
      -sum(endsWith(lista_cursos,"_FL")),
      "cursos em Viçosa") # 48

# Apenas cursos que aparecem nos 10 anos de processo seletivo
paste(length(lista_cursos_estavel),"cursos estáveis em todos os 10 anos") #64

print(paste(sum(endsWith(lista_cursos_estavel,"_RP")), 
            "cursos de Rio Paranaíba estáveis em todos os 10 anos.")) #12

print(paste(sum(endsWith(lista_cursos_estavel,"_FL")), 
            "cursos de Florestal estáveis em todos os 10 anos.")) #10

print(paste(length(lista_cursos_estavel)
            -sum(endsWith(lista_cursos_estavel,"_RP"))
            -sum(endsWith(lista_cursos_estavel,"_FL")),
            "cursos estáveis de Viçosa em todos os 10 anos.")) #42

# ==============================================================================
# Investigando Numero_Chamada_Convocacao == NA ou == 0

# Inscritos com Numero_Chamada_Convocacao == 0
dados_ufv[which (Numero_Chamada_Convocacao == 0)] %>% nrow() # 44349 nesta situação estranha
dados_ufv[which (Numero_Chamada_Convocacao == 0)] %>% count(Processo_Seletivo)
# Há estudantes com Numero_Chamada_Convocacao == 0 apenas em 2015 até 2020

# Inscritos com Numero_Chamada_Convocacao == NA
dados_ufv[which (!Numero_Chamada_Convocacao %in% c(0:20))] %>% nrow() # 51219 nesta situação normal
dados_ufv[which (!Numero_Chamada_Convocacao %in% c(0:20))] %>% count(Processo_Seletivo)
# Há estudantes com Numero_Chamada_Convocacao == 0 em todos os anos


# ==============================================================================
# Cursos em que existem suplentes

ano<-2022 # edição do sisu

# Quais são os códigos de Numero_Chamada_Convocacao? 0 até 20 e NA
dados_ufv$Numero_Chamada_Convocacao %>% unique() # %>% sort()
count(dados_ufv, Numero_Chamada_Convocacao)[c(1,22)]


# Inscritos com Numero_Chamada_Convocacao == 0
dados_ufv[which (Numero_Chamada_Convocacao == 0)]$mod_con %>% unique()

# Inscritos com Numero_Chamada_Convocacao == NA
dados_ufv[which (!Numero_Chamada_Convocacao %in% c(0:20))] %>% View()
dados_ufv[which (!Numero_Chamada_Convocacao %in% c(0:20))]$mod_con %>% unique()
# tem pessoas com Numero_Chamada_Convocacao == NA e mod_con diferente de NA 

dados_ufv[which (!Numero_Chamada_Convocacao %in% c(0:20))][which (mod_con != "NA")] %>% View()

dados_ufv[which (Processo_Seletivo == "SISU2014")]

dados_ufv[which (Processo_Seletivo == "SISU2013")][which (Identificacao == 54657)]

dados_ufv[which (Processo_Seletivo == "SISU2022")][which (!Numero_Chamada_Convocacao %in% c(0:20))]
dados_ufv[which (Processo_Seletivo == "SISU2022")][which (Numero_Chamada_Convocacao %in% c(0))]

dados_ufv %>% nrow() # 170199

dados_ufv[which (Processo_Seletivo == sisu_anos[ano-2012])][which (mod_con=="NA")]$Curso %>% unique()

# ==============================================================================
# Numero_Chamada_Convocacao



# ==============================================================================
# Cursos em que todos os inscritos foram convocados

# ==============================================================================
# Cursos em que há mais vagas do que inscritos em alguma modalidade


# ==============================================================================
# Organizar df_cursos_mudou e exportar

# Definir ordem das linhas
ordem<-c(
  "LICENCIATURA_EM_FISICA",
  "ECONOMIA_DOMESTICA",
  "SERVIÇO_SOCIAL",
  "EDUCACAO_FISICA",
  "EDUCACAO_FISICA_BACHARELADO",
  "EDUCACAO_FISICA_LICENCIATURA")

# Conferir
df_cursos_mudou

# Reorganizar linhas seguindo ordem
df_cursos_mudou %>%
  slice(match(ordem, curso)) -> df_cursos_mudou_export

df_cursos_mudou_export

# Exportar df_cursos_mudou
getwd()
# write.csv(df_cursos_mudou_export,
#           "C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas/privado/df_cursos_mudou.csv",
#           row.names=FALSE)

