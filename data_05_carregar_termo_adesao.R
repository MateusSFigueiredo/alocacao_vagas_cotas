# ==============================================================================
# Arquivo: data_05_carregar_termo_adesao.R
#
# Carrega dados do termo de adesão 2018 e 2022.
#
# Modificado em: 2024-05-14.
# Autor: Mateus Silva Figueiredo
#
# diff: documentação.

# ==============================================================================
#
# Dicionário
#
# Inputs:
# Arquivo "Wania SISU 2012 a 2022 - 2022-11-17 - original.csv"
#
# Outputs:
# nvagas_ADMINISTRACAO até nvagas_ZOOTECNIA
#
# ==============================================================================
# Preparação
library(tidyverse)
library(data.table)

belch2 <- function(x, y) { eval(parse(text=(paste0(x, y, sep=""))))}
belch3 <- function(x, y, z) { eval(parse(text=(paste0(x, y, z, sep=""))))}

setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")

# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- F   # deseja separar candidatos por curso? 
por_ano   <- F  # deseja separar candidatos por ano? 
source("data_04_carregar_dados_UFV.R") # F,F => cria ~10 objetos

# deseja carregar dados de 2018? se não existir, padrão é sim (T)
if (!exists ("carrega_2018")) {carrega_2018 <- T}

# ==============================================================================

# Carregar termo de adesão em formato csv
termo_adesao_2022<-fread("vagas_ufv_termo_de_adesao_2022.csv",
#                   nrow=10, # caso queira carregar poucos dados
                   encoding = 'Latin-1', # Latin-1 # UTF-8
                   dec=".")

termo_adesao_2022$Curso

# excluir espaços dos cursos
gsub(" ", "_", termo_adesao_2022$Curso) -> termo_adesao_2022$Curso
gsub("_-_", "_", termo_adesao_2022$Curso) -> termo_adesao_2022$Curso

# remover acentos
iconv(termo_adesao_2022$Curso, to="ASCII//TRANSLIT") -> termo_adesao_2022$Curso

# ver
termo_adesao_2022$Curso

# cursos que estão no termo_adesao_2022 e não estão em lista_cursos
# deve ter apenas "UNIVERSIDADE_FEDERAL_DE_VICOSA"
subset(termo_adesao_2022,!Curso %in% lista_cursos)$Curso

# ==============================================================================
# conferir se nomes de cursos são iguais com lista_cursos obtida de dados_ufv
all (termo_adesao_2022$Curso %in% c(lista_cursos,"UNIVERSIDADE_FEDERAL_DE_VICOSA"))
# deve ser TRUE

# ==============================================================================
# Criar nvagas_curso para todos os cursos

for (i in 1:nrow(termo_adesao_2022)){ # abre loop de criar nvagas_CURSO
c(termo_adesao_2022$A0[i],
  termo_adesao_2022$L01[i], termo_adesao_2022$L02[i],
  termo_adesao_2022$L05[i], termo_adesao_2022$L06[i],
  termo_adesao_2022$L09[i], termo_adesao_2022$L10[i],
  termo_adesao_2022$L13[i],termo_adesao_2022$L14[i]) -> nvagas_curso

belch2(
"nvagas_curso ->> nvagas_",
termo_adesao_2022$Curso[i]
)
} # fecha loop de criar nvagas_CURSO

# sum(nvagas_ADMINISTRACAO)
# sum(nvagas_UNIVERSIDADE_FEDERAL_DE_VICOSA)

print("nvagas 2022 estão carregadas")

# ==============================================================================
# ======================== 2018 =======================================
# ==============================================================================

if(carrega_2018){ # deseja carregar dados de 2018? carrega_2018 definido lá em cima

# Carregar termo de adesão 2018 em formato csv
termo_adesao_2018<-fread("vagas_ufv_termo_de_adesao_2018.csv",
                         #                   nrow=10, # caso queira carregar poucos dados
                         encoding = 'Latin-1', # Latin-1 # UTF-8
                         dec=".")

termo_adesao_2018$Curso

# excluir espaços dos cursos
gsub(" ", "_", termo_adesao_2018$Curso) -> termo_adesao_2018$Curso
gsub("_-_", "_", termo_adesao_2018$Curso) -> termo_adesao_2018$Curso

# remover acentos
iconv(termo_adesao_2018$Curso, to="ASCII//TRANSLIT") -> termo_adesao_2018$Curso

# ver
termo_adesao_2018$Curso

# cursos que estão no termo_adesao_2018 e não estão em lista_cursos
# deve ter apenas "UNIVERSIDADE_FEDERAL_DE_VICOSA"
subset(termo_adesao_2018,!Curso %in% lista_cursos)$Curso

# ==============================================================================
# conferir se nomes de cursos são iguais com lista_cursos obtida de dados_ufv
all(termo_adesao_2018$Curso %in% c(lista_cursos,"UNIVERSIDADE_FEDERAL_DE_VICOSA"))
# deve ser TRUE

# ==============================================================================
# Criar nvagas_curso para todos os cursos

for (i in 1:nrow(termo_adesao_2018)){ # abre loop de criar nvagas_CURSO
  c(termo_adesao_2018$A0[i],
    termo_adesao_2018$L01[i], termo_adesao_2018$L02[i],
    termo_adesao_2018$L05[i], termo_adesao_2018$L06[i],
    termo_adesao_2018$L09[i], termo_adesao_2018$L10[i],
    termo_adesao_2018$L13[i],termo_adesao_2018$L14[i]) -> nvagas_curso
  
  belch3(
    "nvagas_curso ->> nvagas_",
    termo_adesao_2018$Curso[i],
    "_2018"
  )
} # fecha loop de criar nvagas_CURSO


print("nvagas 2018 estão carregadas")
} # fim de carregar dados de 2018

# ==============================================================================
# Limpeza 2022

# # apagar todos os nvagas_CURSO
# for (i in 1:nrow(termo_adesao_2022)){ # abre loop de limpeza
#   eval(parse(text=(paste0("rm(nvagas_",
#                           termo_adesao_2022$Curso[i],
#                           ")"))))}
# fecha loop de limpeza
