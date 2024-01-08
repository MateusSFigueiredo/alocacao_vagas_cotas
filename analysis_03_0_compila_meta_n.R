# # ==============================================================================
# Arquivo: analysis_03_0_compila_meta_n.R

# Compila dados dos cinco modelos de concorrencia:
# 1 = concorrencia separada (aka listas multiplas)
# 2 = concorrencia concomitante, AC primeiro
# 3 = concorrencia concomitante, cotas primeiro
# 4 = concorrencia segundo Bó e Senkevics, 2023
# 5 = concorrencia adaptado de Bó e Senkevics, 2023

# Faz:
# dfs meta_n_ CURSO _ SISU ANO
# Atribuir NA para linhas com Nan, -Inf ou Inf

# Falta:
# Exportar como Word (talvez fazer em outro script)

# Objetivo final:
# Gerar Word compila_meta_n

# Modificado em 2024-01-04.
# Autor: Mateus Silva Figueiredo

# diff: organização.

# ==============================================================================
# Carregar dados
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")

# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- T   # deseja separar candidatos por curso? obrigatório
por_ano   <- F   # deseja separar candidatos por ano? opcional
if (!exists("dados_ZOOTECNIA")) source("data_04_carregar_dados_UFV.R") # cria ~80 objetos

# Criar vetores n_vagas para cada curso, com base no termo de adesão de 2022
if (!exists("vagas_MEDICINA")) source("data_05_carregar_termo_adesao.R") # cria ~70 objetos

# Carregar df_so_concorridos
if (!exists ("df_so_concorridos")) source("analysis_001_compara_ins_vagas.R")

# criar vetor mod, com a ordem das modalidades
mod <- c("A0","L01","L02","L05","L06", "L09", "L10", "L13", "L14")
# ------------------------------------------------------------------------------
# Preparação
# options(digits = 8) # talvez melhor não
library(tidyverse)

# ==============================================================================
# Iniciar alocação
# definir inicio e fim, para depois rodar for loop

# para um conjunto
#i<-1
# ------------------------------------------------------------------------------

# apenas alguns conjuntos
# inicio<-29; fim<-31

# ------------------------------------------------------------------------------
# ================ ABRE FOR LOOP ===============================================

# para todos os conjuntos:
inicio<-1; fim<-nrow(df_so_concorridos)

for (i in inicio:fim){ # abre loop i, um para cada conjunto
  
  # gerar conjunto candidatos a partir de número 1 a 63
  candidatos <- dados_ufv[Curso==df_so_concorridos[i,1]][Processo_Seletivo==df_so_concorridos[i,2]]
  
  if(exists("nvagas")) rm("nvagas")
  # Atribuir nvagas correto # criar nvagas a partir do curso em candidatos
  nvagas <- get(paste0("nvagas_",candidatos$Curso[1]))
  nvagas
  
  # para regularizar, fingir que nenhum candidato foi convocado ainda
  candidatos$mod_con <- 0
  candidatos %>% head
  
  # remover objetos analise_n e analise_v
  rm(list = ls(pattern = "^analise_n"))
  rm(list = ls(pattern = "^analise_v"))
  
  setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas") # definir pasta
  if (!exists ("analise_v_c5")){ 
    # Correr concorrências
    source("analysis_01_1_conc_separada.R") # parece ok
    source("analysis_01_2_conc_c_ac.R")     # parece ok 
    source("analysis_01_3_conc_c_cotas.R")  # parece ok           
    source("analysis_01_4_conc_bo.R")       # parece ok   
    source("analysis_01_5_conc_ob.R")       # testando           
    
  }
  # ==============================================================================
  # Criar df meta_n com todas as notas dos tres modelos
  meta_n<-rbind(analise_n_c1,analise_n_c2,analise_n_c3,analise_n_c4,analise_n_c5)
  
  # ==============================================================================
  
  
  # ------------------------------------------------------------------------------
  # verificar resultados
  print(meta_n)
  print(paste(candidatos$Curso[1],candidatos$Processo_Seletivo[1]))
  print(paste(sum(nvagas),"== sum(nvagas)"))

  # ------------------------------------------------------------------------------
  # salvar como meta_n_ curso _ sisu, usa como fonte candidatos
  eval(parse(text=(paste0(
    "meta_n_",candidatos$Curso[1],"_",candidatos$Processo_Seletivo[1]," <- meta_n"
  ))))
  # 
} # fim do loop i, de cada conjunto

# ==============================================================================

# Se a linha tiver NaN, Inf ou -Inf, faça a linha inteira virar NA

# ------------- ABRE FOR LOOP --------------------------------------------------

# para todos os conjuntos:
inicio<-1; fim<-nrow(df_so_concorridos)

for (i in inicio:fim){ # abre loop
  
# df <- df_meta_n_ CURSO ANO 
eval(parse(text=(paste0(
  "df <- meta_n_",df_so_concorridos[i,1],"_",df_so_concorridos[i,2]
))))

# colocar NA em df
df[apply(df, 1, function(row) any(!is.finite(row))), ] <- NA

# df -> df_meta_n CURSO ANO
eval(parse(text=(paste0(
  "df -> meta_n_",df_so_concorridos[i,1],"_",df_so_concorridos[i,2]
))))
} # fecha loop



# ==============================================================================
# Limpeza
if(T){
  # remover objetos analise_n e analise_v
  rm(list = ls(pattern = "^comp_vagas"))
  rm(list = ls(pattern = "^convocados"))
  rm("candidatos")
}
