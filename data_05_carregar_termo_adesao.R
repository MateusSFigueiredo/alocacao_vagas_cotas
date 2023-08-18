# ==============================================================================
# Arquivo: data_05_carregar_termo_adesao.R
#
# Modificado em: 2022-12-11.
# Autor: Mateus Silva Figueiredo
#
# Carrega dados do termo de adesão.
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

# ==============================================================================
# Carregar dados

setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")
# list.files()


termo_adesao_2022<-fread("vagas_ufv_termo_de_adesao_2022.csv",
#                   nrow=10, # caso queira carregar poucos dados
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
# deve ser vazio
subset(termo_adesao_2022,!Curso %in% lista_cursos)$Curso

# ==============================================================================
# Criar nvagas_curso para todos os cursos

for (i in 1:nrow(termo_adesao_2022)){ # abre loop de criar nvagas_CURSO
c(termo_adesao_2022$A0[i],
  termo_adesao_2022$L1[i], termo_adesao_2022$L2[i],
  termo_adesao_2022$L5[i], termo_adesao_2022$L6[i],
  termo_adesao_2022$L9[i], termo_adesao_2022$L10[i],
  termo_adesao_2022$L13[i],termo_adesao_2022$L14[i]) -> nvagas_curso

belch2(
"nvagas_curso ->> nvagas_",
termo_adesao_2022$Curso[i]
)
} # fecha loop de criar nvagas_CURSO

sum(nvagas_ADMINISTRACAO)

# ==============================================================================
# Limpeza

# # apagar todos os nvagas_CURSO
# for (i in 1:nrow(termo_adesao_2022)){ # abre loop de limpeza
#   eval(parse(text=(paste0("rm(nvagas_",
#                           termo_adesao_2022$Curso[i],
#                           ")"))))}
# fecha loop de limpeza
