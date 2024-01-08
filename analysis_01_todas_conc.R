# ==============================================================================
# Arquivo: analysis_01_todas_conc.R
# Roda scripts de analysis_01 de formas de concorrência
#
#
# Modificado em 2024-01-08
# Autor: Mateus Silva Figueiredo

# diff: documentação.
# diff: consegue ler nvagas de 2018
# diff: renomeador de analysis_01_todas_conc_teste.R para analysis_01_todas_conc.R
# ==============================================================================
# Dicionário
# Inputs:
#
# Outputs:
# analise_n1 = nota máxima, média e mínima de cada categoria com modelo 1
# analise_v1 = número de candidatos convocados por modalidade com modelo 1
#
# analise_n2 = nota máxima, média e mínima de cada categoria com modelo 2
# analise_v2 = número de candidatos convocados por modalidade com modelo 2
#
# analise_n3 = nota máxima, média e mínima de cada categoria com modelo 3
# analise_v3 = número de candidatos convocados por modalidade com modelo 3
#
# analise_n4 = nota máxima, média e mínima de cada categoria com modelo 4
# analise_v4 = número de candidatos convocados por modalidade com modelo 4
#
# analise_n5 = nota máxima, média e mínima de cada categoria com modelo 5
# analise_v5 = número de candidatos convocados por modalidade com modelo 5
#
# ==============================================================================
#
# Preparação
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")
getwd()
# list.files()
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

# criar vetor mod, com a ordem das modalidades de nvagas
mod <- c("A0","L01","L02","L05","L06", "L09", "L10", "L13", "L14")

# ==============================================================================
# Escolher um curso e ano arbitrários
# cu <- "LICENCIATURA_EM_QUIMICA_FL" # pelo nome
# cu <- lista_cursos_18_22[10]; # pelo número na lista

# Escolher uma edição do SISU
# edicao <- "SISU2018" # pelo nome

# Definir n_vagas, a partir do nvagas_CURSO gerado por data_05
# nvagas <- get(paste0("nvagas_",cu))

# ------------------------------------------------------------------------------
# Escolher um curso e ano a partir de df_so_concorridos
# Gerar conjunto candidatos com analysis_001_compara_ins_vagas.R
# Carregar df_so_concorridos, apenas se ainda não existir df_so_concorridos
if (!exists ("df_so_concorridos")) source("analysis_001_compara_ins_vagas.R")
df_so_concorridos %>% nrow()

# limpeza após analysis_001
if (exists ("concorrido")) rm(cu,concorrido,i,j) # não tem problema se der Warning messages

# ==============================================================================
# Iniciar alocação
i<-1 # de 1 até 67, para os 67 conjuntos concorridos encontrados
df_so_concorridos[i,1] # nome do curso curso i

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
source("analysis_01_3_conc_c_cotas.R")  # parece ok           
source("analysis_01_4_conc_bo.R")       # parece ok             
source("analysis_01_5_conc_ob.R")       # precisa ser checado

# ==============================================================================
#
# Meta análise
# Criar dfs com resultado compilado dos tres modelos
#
# Criar df meta_n com todas as notas dos tres modelos
meta_n<-rbind(analise_n_c1,analise_n_c2,analise_n_c3,analise_n_c4,analise_n_c5)

# Criar df meta_v com todas as alocações de vagas dos tres modelos
meta_v<-rbind(analise_v_c1,analise_v_c2,analise_v_c3,analise_v_c4,analise_v_c5)

# ------------------------------------------------------------------------------
# Conferir resultados
df_so_concorridos[i,1] # nome do curso i, usado no conjunto
df_so_concorridos[i,2] # edição do sisu, usado no conjunto
nvagas
meta_v
}
# ==============================================================================

print("Fim do arquivo")
# Fim do arquivo
