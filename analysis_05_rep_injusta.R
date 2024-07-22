# ==============================================================================
# Arquivo: analysis_05_rep_injusta.R

# Procura reprovações injustas
# Comparar apenas c1, c2 e c3
#
# Modificado em 2024-01-12 ESCREVENDO
# Autor: Mateus Silva Figueiredo

# diff: 

# -------

# Roda scripts de analysis_01 de formas de concorrência

# ==============================================================================
#
# Preparação
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")

# ==============================================================================
# Carregar dados

# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- T   # deseja separar candidatos por curso? obrigatório
por_ano   <- F   # deseja separar candidatos por ano? opcional
if (!exists("dados_ZOOTECNIA")) source("data_04_carregar_dados_UFV.R") # cria ~80 objetos

# Criar vetores n_vagas para cada curso, com base no termo de adesão de 2022
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")
carrega_2018 <- T # deseja carregar nvagas do termo de 2018?
if (!exists("vagas_MEDICINA")) source("data_05_carregar_termo_adesao.R") # cria ~70 objetos

# Carregar df_so_concorridos
if (!exists ("df_so_concorridos")) source("analysis_001_compara_ins_vagas.R")

# criar vetor mod, com a ordem das modalidades de nvagas
mod <- c("A0","L01","L02","L05","L06", "L09", "L10", "L13", "L14")

# ==============================================================================
# Começar loop e alocação
# ==============================================================================

# Iniciar alocação para apenas um conjunto
i<-66 # de 1 até 67, para os 67 conjuntos concorridos encontrados

# Iniciar alocação para todos os conjuntos
for (i in 1:nrow(df_so_concorridos)){

# nome e ano do conjunto i
paste(df_so_concorridos[i,1],df_so_concorridos[i,2]) 

# gerar conjunto candidatos a partir de número 1 a 67
candidatos <- dados_ufv[Curso==df_so_concorridos[i,1]][Processo_Seletivo==df_so_concorridos[i,2]]

# Atribuir nvagas correto, do termo 2018 ou 2022
# se for SISU2018, pegar nvagas_CURSO_2018
# se não, pegar nvagas_CURSO
# criar nvagas a partir do nome do curso i
if(candidatos$Processo_Seletivo[1]=="SISU2018"){nvagas <- get(paste0("nvagas_",df_so_concorridos[i,1],"_2018"))} else {
  nvagas <- get(paste0("nvagas_",df_so_concorridos[i,1])) }

# para regularizar, fingir que nenhum candidato foi convocado ainda
candidatos$mod_con <- 0

# ==============================================================================

setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas") # definir pasta
{ 
  # Correr concorrências
  source("analysis_01_1_conc_separada.R") # parece ok
  source("analysis_01_2_conc_c_ac.R")     # parece ok 
  source("analysis_01_3_conc_c_cotas.R")     # parece ok 
} # apenas c1 e c2
  # ==============================================================================

# Pega nota mínima dos convocados por c1 por A0
convocados_c1[mod_con=="A0"]$nota %>% min() -> min_A0
print(paste(min_A0,"é a nota mínima dos convocados_c1 por A0 em",df_so_concorridos[i,1],df_so_concorridos[i,2]))

# Pega candidatos com nota maior que isso
# pega apenas os que não são inscritos por A0
# cria df talvez_rep_inju de talvez reprovados injustamente
candidatos[nota>min_A0][mod_ins!="A0"] -> talvez_rep_inju
print(paste(nrow(talvez_rep_inju),"candidatos em talvez_rep_inju"))

# compara id dos talvez_rep_inju com id dos convocados_c1
all(talvez_rep_inju$id %in% convocados_c1$id)

# print texto comparando as ids
# if(all(talvez_rep_inju$id %in% convocados_c1$id)) {print("todos os talvez_rep_inju estão em convocados_c1")}

# ----

# roda loop para checar, para cada conjuntos, se tem reprovação injusta
# print uma frase falando CURSO, ANO, e se tem ou não reprovação injusta
if(all(talvez_rep_inju$id %in% convocados_c1$id)){
  print(paste(df_so_concorridos[i,1],df_so_concorridos[i,2],"não tem reprovação injusta"))
} else {
  print(paste(df_so_concorridos[i,1],df_so_concorridos[i,2],"TEM REPROVAÇÃO INJUSTA"))
}

}

# resultado: apenas SERVICO_SOCIAL SISU2021 TEM REPROVAÇÃO INJUSTA
# os demais conjuntos não tem