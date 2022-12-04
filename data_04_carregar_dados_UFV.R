# ==============================================================================
# Arquivo: data_04_carregar_dados_UFV.R

#
# Modificado em: 2022-12-04.
# Autor: Mateus Silva Figueiredo
#
# Nota: Vale a pena separar este arquivo em dois.
# Um para carregar os dados, outro para analisar.
# ==============================================================================
#
# Dicionário
#
# Inputs:
# Arquivo "Wania SISU 2012 a 2022 - 2022-11-17 - original.csv"
#
# Outputs:
# dados_ufv com todos os cursos de todos os anos 2013-2022
# dados_2013 até dados_2022 com todos os cursos em cada
# dados_2013 em diante tem colunas id, nota, mod_ins, mod_con,
# Processo_Seletivo e Curso
#
# ==============================================================================
# Preparação
library(tidyverse)
library(data.table)

# Disable scientific notation
# Para manter o Número de Inscrição inalterado
options(scipen = 999)

# ==============================================================================
# Carregar dados

setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas/privado")
# list.files()

colunas<-c("Identificacao","Processo_Seletivo",
#         "Sexo", "Idade_Ingresso","UF","Cidade",                     
          "ENEM",
          "Modalidade_Inscrita","Numero_Chamada_Convocacao","Modalidade_Convocada",
         "Codigo_Modalidade_Inscrita","Codigo_Modalidade_Convocada",
"Curso") # apenas colunas de interesse

dados_ufv<-fread("Wania SISU 2012 a 2022 - 2022-11-17 - original.csv",
#                   nrow=10, # caso queira carregar poucos dados
select = colunas,
                   dec=",")

# View(dados_ufv) # 170199 obs.
# dados_ufv%>% summary()

# ==============================================================================
# Limpar dados_ufv

# Sinonimizar cursos que mudaram de nome.

# Mudanças de de 2013 para 2014: Letras, Pedagogia, Comunicação Social.
dados_ufv[dados_ufv == "LETRAS - LICENCIATURA"] <- "LETRAS" 
dados_ufv[dados_ufv == "PEDAGOGIA - LICENCIATURA"] <- "PEDAGOGIA"
dados_ufv[dados_ufv == "COMUNICACAO SOCIAL - JORNALISMO"] <- "COMUNICACAO SOCIAL"
dados_ufv[dados_ufv == "SUPERIOR TEC. EM GESTAO AMBIENTAL - FL"] <- "GESTAO AMBIENTAL - FL"
# Mudança de 2013 para 2020:
dados_ufv[dados_ufv == "LIC. EM CIENCIAS BIOLOGICAS - FL"] <- "LICENCIATURA EM CIENCIAS BIOLOGICAS - FL"
# Mudança de 2017 para 2018:
dados_ufv[dados_ufv == "CIENCIAS DE ALIMENTOS - RP"] <- "CIENCIA E TECNOLOGIA DE ALIMENTOS - RP"

# excluir espaços dos cursos
gsub(" ", "_", dados_ufv$Curso) -> dados_ufv$Curso
gsub("_-_", "_", dados_ufv$Curso) -> dados_ufv$Curso

# conferir nomes dos cursos
# unique(dados_ufv$Curso)

# ==============================================================================
# Tornar dados compatíveis com código já escrito

# ------------------------------------------------------------------------------
# Criar colunas mod_ins e mod_con.
# Nomes das categorias UFV vai ser UFV1 até UFV9, para depois substituir.
# Criar coluna mod_ins, para não confundir os números
dados_ufv$mod_ins <- paste0("UFV",dados_ufv$Modalidade_Inscrita)

# Criar coluna mod_con, para não confundir os números
dados_ufv$mod_con <- paste0("UFV",dados_ufv$Modalidade_Convocada)


 dados_ufv %>% count(Modalidade_Inscrita) == dados_ufv %>% count(mod_ins) 
 # coluna n deve dar tudo TRUE

 # dados_ufv %>% count(Modalidade_Convocada) == dados_ufv %>% count(mod_con)
 # coluna n deve dar tudo TRUE

# Substitui modalidade UFV por modalidade INEP.

i<-1 
 
# Regra para SISU 2013 a 2017. 
subset(dados_ufv,Processo_Seletivo %in% c("SISU2013","SISU2014",
                                          "SISU2015","SISU2016",
                                          "SISU2017")) -> regra1
{
  regra1[regra1 == "UFV1"] <- "L2" # UFV1 == L2. pub + bxa + ppi
  regra1[regra1 == "UFV2"] <- "L1" # UFV2 == L1. pub + bxa
  regra1[regra1 == "UFV3"] <- "L6" # UFV3 == L6. pub + ppi
  regra1[regra1 == "UFV4"] <- "L5" # UFV4 == L5. pub 
  regra1[regra1 == "UFV5"] <- "A0" # UFV5 == A0. 
  regra1[regra1 == "UFVNA"] <- "NA" #NA 
} # fim regra 2013 a 2016

# Regra para SISU 2018 a 2022
subset (dados_ufv,Processo_Seletivo %in% c("SISU2018",
                                          "SISU2019","SISU2020",
                                          "SISU2021","SISU2022")) -> regra2
{
  regra2[regra2 == "UFV1"] <- "L2" # UFV1 == L2. pub + bxa + ppi
  regra2[regra2 == "UFV3"] <- "L1" # UFV2 == L1. pub + bxa
  regra2[regra2 == "UFV5"] <- "L6" # UFV3 == L6. pub + ppi
  regra2[regra2 == "UFV7"] <- "L5" # UFV4 == L5. pub 
  regra2[regra2 == "UFV9"] <- "A0" # UFV5 == A0. 
  
  regra2[regra2 == "UFV2"] <- "L9"  # UFV2 == L2. pub + bxa + ppi + pcd
  regra2[regra2 == "UFV4"] <- "L10" # UFV4 == L1. pub + bxa + pcd
  regra2[regra2 == "UFV6"] <- "L13" # UFV6 == L6. pub + ppi + pcd
  regra2[regra2 == "UFV8"] <- "L14" # UFV8 == L5. pub + pcd
  
  regra2[regra2 == "UFVNA"] <- "NA" #NA 
} # fim regra 2013 a 2016

# Junta regra1 com regra2
rbind (regra1, regra2) -> dados_ufv; rm(regra1, regra2)

# # Conferência
# unique (dados_ufv$Modalidade_Inscrita)
# count (dados_ufv,mod_ins)
# 
# subset (dados_ufv,mod_ins=="A0")[1]
# subset (dados_ufv,mod_ins=="L1")[1]
# subset (dados_ufv,mod_ins=="L2")[1]
# subset (dados_ufv,mod_ins=="L5")[1]
# subset (dados_ufv,mod_ins=="L6")[1]
# subset (dados_ufv,mod_ins=="L9")[1]
# subset (dados_ufv,mod_ins=="L10")[1]
# subset (dados_ufv,mod_ins=="L13")[1]
# subset (dados_ufv,mod_ins=="L14")[1]

   
   
# ==============================================================================
# Criar dados por ano
# Criar dados_2013 até dados_2022

anos<-c(2013:2022) # definir anos
i<-1 # útil para anos[i], para pegar um ano de cada vez
# ------------------------------------------------------------------------------
 for (j in 1:length(anos)){ # rodar loop 10 vezes para criar dados_2013 a 2022
    { # abre loop
  
# Criar dados_ano a partir de anos 2013:2022, um de cada vez

eval(parse(text=(paste(
'dados_ano <- subset (dados_ufv,dados_ufv$Processo_Seletivo=="SISU',
                     anos[i],
                     '")'
,
sep="")))) # cria dados_ano com inscritos de um ano específico
dados_ano$Processo_Seletivo[1] # qual processo seletivo

# ------------------------------------------------------------------------------

# Criar colunas id, nota, mod_ins, mod_con, e ao final limpar dados_ano
# {
# Criar coluna id
dados_ano$id <- (anos[i]*10000000+1:nrow(dados_ano))
# summary(dados_ano$id) # Criar id único para cada inscrição, diferente do Identificacao

# Criar coluna nota
dados_ano$nota <- dados_ano$ENEM
# summary(dados_ano$nota)

# ------------------------------------------------------------------------------
# Limpa dados_ano. Mantém apenas colunas de interesse

subset(dados_ano,Identificacao>1,
       select=c(Processo_Seletivo,Curso,id,nota,mod_ins,mod_con)
       ) -> dados_ano

# } # finaliza procedimentos com dados_ano
# ------------------------------------------------------------------------------

# salva dados_ano para variável dados_2013 até dados_2022
    eval(parse(text=(paste(
      'dados_ano ->> dados_',
      anos[i]
      ,
      sep=""))))

print(anos[i])
i<-i+1
  
} # fim do for loop
}

rm(dados_ano)
# Limpeza dos dados
# rm(dados_2013,dados_2014,dados_2015,dados_2016,dados_2017,dados_2018,
#    dados_2019,dados_2020,dados_2021,dados_2022)

# ==============================================================================
# Trabalhando com cursos
# Cursos já devem ter sido sinonimizados lá em cima na linha 54

# Identificar lista de cursos.
unique(dados_ufv$Curso) %>% sort() -> lista_cursos
 lista_cursos # 75 cursos

# ==============================================================================
# Cursos que abriram, fecharam ou mudaram de nome
 
# # Alguns cursos abriram, fecharam, ou mudaram de nome. Ver um exemplo:
# # "ECONOMIA DOMESTICA" aparece nos anos 2013 até 2015.
# unique(subset(dados_ufv,Curso=="ECONOMIA DOMESTICA")$Processo_Seletivo)
 
# Cursos que mudaram de nome
 # Cria lista_cursos_mudou para cursos que aparecem em menos de 10 anos
lista_cursos_mudou <- character()

# Preencher lista_cursos_mudou
for (i in 1:length(lista_cursos)) {

  if (
    unique(subset(dados_ufv,Curso==lista_cursos[i])$Processo_Seletivo) %>% length() != 10
  ) append(lista_cursos_mudou,lista_cursos[i]) ->> lista_cursos_mudou
}

lista_cursos_mudou

# Gerar data.frame df_cursos_mudou com cursos que mudaram e quais anos eles existiram
N=1:length(lista_cursos_mudou)

rbindlist(
lapply(N, function(i) {
paste(unique(subset(
dados_ufv,Curso==lista_cursos_mudou[i])$Processo_Seletivo),
collapse=", ") -> anos_por_curso
data.table(curso = lista_cursos_mudou[i], anos = anos_por_curso)
}))  ->> df_cursos_mudou

# Limpar df_cursos_mudou
# Remover palavra "SISU"
df_cursos_mudou$anos<-gsub("SISU","",df_cursos_mudou$anos)


# ------------------------------------------------------------------------------
# Resumir anos. Não otimizado, mas funciona para estes dados.
# Ordem das linhas não importa.
{
df_cursos_mudou[df_cursos_mudou == "2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022"] <- "2014:2022"
df_cursos_mudou[df_cursos_mudou == "2017, 2018, 2019, 2020, 2021, 2022"] <- "2017:2022"
df_cursos_mudou[df_cursos_mudou == "2018, 2019, 2020, 2021, 2022"] <- "2018:2022"
# df_cursos_mudou[df_cursos_mudou == "2020, 2021, 2022"] <- "2020:2022" # não funciona????
# [df_cursos_mudou == "2021, 2022"] <- "2021:2022" # não funciona??? 

df_cursos_mudou[df_cursos_mudou == "2013, 2014, 2015, 2016, 2017, 2018"] <- "2013:2018"
df_cursos_mudou[df_cursos_mudou == "2013, 2014, 2015, 2016, 2017"] <- "2013:2017"
df_cursos_mudou[df_cursos_mudou == "2013, 2014, 2015, 2016"] <- "2013:2016"
df_cursos_mudou[df_cursos_mudou == "2013, 2014, 2015"] <- "2013:2015"

df_cursos_mudou[df_cursos_mudou == "2014, 2015, 2016, 2017, 2018, 2019, 2020"] <- "2014:2020"
df_cursos_mudou[df_cursos_mudou == "2014, 2015, 2016, 2017, 2018, 2019"] <- "2014:2019"

df_cursos_mudou[df_cursos_mudou == "2013, 2020, 2021, 2022"] <- "2013, 2020:2022"
df_cursos_mudou[df_cursos_mudou == "2013, 2021, 2022"] <- "2013, 2021:2022"
}
df_cursos_mudou$anos

# ------------------------------------------------------------------------------
# Cursos que NÃO mudaram de nome
# Cria lista_cursos_mudou para cursos que aparecem em exatos 10 anos
lista_cursos_estavel <- character()

# Preencher lista_cursos_estavel
for (i in 1:length(lista_cursos)) {
  
  if (
    unique(subset(dados_ufv,Curso==lista_cursos[i])$Processo_Seletivo) %>% length() == 10
  ) append(lista_cursos_estavel,lista_cursos[i]) ->> lista_cursos_estavel
}

summary(lista_cursos_estavel) # length == 59
# Devem aparecer apenas uma vez:
# PEDAGOGIA, LETRAS, COMUNICAÇÃO SOCIAL
# GESTAO AMBIENTAL - FL, LICENCIATURA EM CIENCIAS BIOLOGICAS - FL
# CIENCIA E TECNOLOGIA DE ALIMENTOS - RP



# ==============================================================================
# Criando dados por curso # escrevendo
length(lista_cursos_estavel)
lista_cursos_estavel

# ------------------------------------------------------------------------------

# Começar a criar objetos dados_curso, e dados_ADMINISTRACAO até dados_ZOOTECNIA

i<-1 # útil para lista_cursos_estavel[i], para pegar um curso de cada vez

# loop para criar todos os objetos dados_curso
for (i in 1:length(lista_cursos_estavel)){
lista_cursos_estavel[i]
dados_curso <- subset(dados_ufv,dados_ufv$Curso==lista_cursos_estavel[i])
eval(parse(text=(paste0("dados_",
                        lista_cursos_estavel[i],
                        "<<-dados_curso"))))
} # fim do loop

# Limpeza
rm(dados_curso)

# Para apagar todos os objetos dados_ADMINISTRACAO até dados_ZOOTECNIA:
# for (i in 1:length(lista_cursos_estavel)) {
# eval(parse(text=(paste0("rm(dados_",
#                         lista_cursos_estavel[i],
#                         ")"))))}


# ==============================================================================
# Referências

# rbindlist(lapply(N, function(i) {...}))
# https://stackoverflow.com/questions/43147235/for-loop-in-r-return-error-object-not-found

# ==============================================================================
# Fim do código

