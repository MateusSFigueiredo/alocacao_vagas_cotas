# # ==============================================================================
# Arquivo: analysis_03_0_compila_meta_n.R


# Modificado em 2024-01-18
# Autor: Mateus Silva Figueiredo

# diff: apaguei loop que apagava linhas com Inf, NaN e -Inf
# dessa forma, consertei DANCA 2018, LIC C BIO FL 2018, NUT RP 2018
# não atrapalha conjuntos que deveriam ter NA na c5

# --------

# Compila dados dos cinco modelos de concorrencia: c1 até c5

# Faz:
# dataframes meta_n_ CURSO _ SISU ANO
# ou seja, meta-análise resumida das notas por curso e por ano

# Atribuir NA para linhas com Nan, -Inf ou Inf
# ou seja, apaga as linhas em que deu erro (parece que só em c5 dá erro)

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
#i<-54
# ------------------------------------------------------------------------------

# apenas alguns conjuntos
# inicio<-29; fim<-31

# ------------------------------------------------------------------------------
# ================ ABRE FOR LOOP ===============================================

# para todos os conjuntos:
inicio<-1; fim<-nrow(df_so_concorridos)

for (i in inicio:fim){ # abre loop i, um para cada conjunto
  
  # gerar conjunto candidatos a partir de número 1 a 67
  candidatos <- dados_ufv[Curso==df_so_concorridos[i,1]][Processo_Seletivo==df_so_concorridos[i,2]]
  
  if(exists("nvagas")) rm("nvagas")
  
  # Atribuir nvagas correto, do termo 2018 ou 2022
  # se for SISU2018, pegar nvagas_CURSO_2018
  # se não, pegar nvagas_CURSO
  # criar nvagas a partir do nome do curso i
  if(candidatos$Processo_Seletivo[1]=="SISU2018"){nvagas <- get(paste0("nvagas_",df_so_concorridos[i,1],"_2018"))} else {
    nvagas <- get(paste0("nvagas_",df_so_concorridos[i,1])) }
  
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

# Parece que aqui, onde deveria ter NA já tem NA
meta_n_CIENCIA_DA_COMPUTACAO_SISU2019[15,]

# cursos com 0 vagas em alguma modalidade ficam com Inf, NaN e -Inf.
# feio, mas não é um problema.
meta_n_DANCA_SISU2018[,7]

# ------------------------------------------------------------------------------
print ("dataframes meta_n CURSO ANO estão criados")

# ==============================================================================
# Limpeza
if(T){
  # remover objetos analise_n e analise_v
  rm(list = ls(pattern = "^comp_vagas"))
  rm(list = ls(pattern = "^convocados"))
  rm("candidatos")
}
