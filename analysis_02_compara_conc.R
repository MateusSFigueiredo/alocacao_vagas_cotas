# # ==============================================================================
# Arquivo: analysis_02_compara_conc.R
# Compara cinco modelos de concorrencia:
# 1 = concorrencia separada (aka listas multiplas)
# 2 = concorrencia concomitante, AC primeiro
# 3 = concorrencia concomitante, cotas primeiro
# 4 = concorrencia segundo Bó e Senkevics, 2023
# 5 = concorrencia adaptado de Bó e Senkevics, 2023

# Gera df comparando_vagas

# Modificado em 2023-08-28.
# Autor: Mateus Silva Figueiredo

# quase funcionando para todos os conjuntos

# ==============================================================================
# Carregar dados

# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- T   # deseja separar candidatos por curso? obrigatório
por_ano   <- F   # deseja separar candidatos por ano? opcional
if (!exists("dados_ufv")) source("data_04_carregar_dados_UFV.R") # cria ~80 objetos

# Criar vetores n_vagas para cada curso, com base no termo de adesão de 2022
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")
if (!exists("vagas_MEDICINA")) source("data_05_carregar_termo_adesao.R") # cria ~70 objetos

# Carregar df_so_concorridos
if (!exists ("df_so_concorridos")) source("analysis_001_compara_ins_vagas.R")

# criar vetor mod, com a ordem das modalidades
mod <- c("A0","L01","L02","L05","L06", "L09", "L10", "L13", "L14")
# ------------------------------------------------------------------------------
# Iniciar alocação

# apenas um conjunto
# i<-4 # de 1 até 63, para os 63 conjuntos concorridos encontrados

{ # rodar tudo
# todos os conjuntos
 for (i in 1:nrow(df_so_concorridos)){ # abre loop i, um para cada conjunto

df_so_concorridos[i,1] # curso do conjunto i
df_so_concorridos[i,2] # sisu do conjunto i

# gerar conjunto candidatos a partir de número 1 a 63
candidatos <- dados_ufv[Curso==df_so_concorridos[i,1]][Processo_Seletivo==df_so_concorridos[i,2]]

# Atribuir nvagas correto # criar nvagas a partir do nome do curso i
nvagas <- get(paste0("nvagas_",df_so_concorridos[i,1]))

# para regularizar, fingir que nenhum candidato foi convocado ainda
candidatos$mod_con <- 0

# Rodar após analysis_01_1, analysis_01_2, analysis_01_3, 
# analysis_01_4, analysis_01_5

setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas") # definir pasta
if (!exists ("analise_v_c5")){ 
  # Correr concorrências
  source("analysis_01_1_conc_separada.R") # parece ok
  source("analysis_01_2_conc_c_ac.R")     # parece ok 
  source("analysis_01_3_conc_c_cotas.R")  # parece ok           
  source("analysis_01_4_conc_bo.R")       # parece ok             
#  source("analysis_01_5_conc_ob.R")       # precisa ser checado
}
# ==============================================================================

# Cria data.frame comparando vagas

# cria diferença entre modelo1 para modelos 2, 3, 4, 5
# sprintf("%+.f" serve para colocar um sinal de + antes dos números
dif_modelo_c2 <- sprintf("%+.f", analise_v_c2[1,] - analise_v_c1[1,])
dif_modelo_c3 <- sprintf("%+.f", analise_v_c3[1,] - analise_v_c1[1,])
dif_modelo_c4 <- sprintf("%+.f", analise_v_c4[1,] - analise_v_c1[1,])
#dif_modelo_c5 <- sprintf("%+.f", analise_v_c5[1,] - analise_v_c1[1,])
dif_modelo_c5 <- NA

comparando_vagas <- rbind(analise_v_c1[1,],dif_modelo_c2,dif_modelo_c3,
                          dif_modelo_c4,dif_modelo_c5)

colnames(comparando_vagas) <- colnames(analise_v_c1)
rownames(comparando_vagas) <- c("Modelo c1",
                                "dif Modelo c2",
                                "dif Modelo c3",
                                "dif Modelo c4",
                                "dif Modelo c5")

# ------------------------------------------------------------------------------
# verificar resultados
print(comparando_vagas)
candidatos$Processo_Seletivo[1]
candidatos$Curso[1]
} #fecha rodar tudo
# ------------------------------------------------------------------------------
# salvar como comp_vagas_ curso _ sisu
# eval(parse(text=(paste0(
#   "comp_vagas_",candidatos$Curso[1],"_",candidatos$Processo_Seletivo[1]," <- comparando_vagas"
#   ))))
# 
} # fim do loop i, de cada conjunto
