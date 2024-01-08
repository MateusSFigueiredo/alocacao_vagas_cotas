# # ==============================================================================
# Arquivo: analysis_02_0_compara_conc.R
# Compara cinco modelos de concorrencia:
# 1 = concorrencia separada (aka listas multiplas)
# 2 = concorrencia concomitante, AC primeiro
# 3 = concorrencia concomitante, cotas primeiro
# 4 = concorrencia segundo Bó e Senkevics, 2023
# 5 = concorrencia adaptado de Bó e Senkevics, 2023

# Gera df comparando_vagas

# Erro: mesmo com apenas concs 1 a 4, ainda dá erro returning Inf e -Inf
# mas será que tem problema? Erro só ocorre em cursos pouco concorridos.

# Modificado em 2024-01-08.
# Autor: Mateus Silva Figueiredo

# diff: consegue trabalhar com termo 2018. TESTANDO.

# ==============================================================================
# Carregar dados

# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- T   # deseja separar candidatos por curso? obrigatório
por_ano   <- F   # deseja separar candidatos por ano? opcional
if (!exists("dados_ZOOTECNIA")) source("data_04_carregar_dados_UFV.R") # cria ~80 objetos

# Criar vetores n_vagas para cada curso, com base no termo de adesão de 2022
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")
if (!exists("vagas_MEDICINA")) source("data_05_carregar_termo_adesao.R") # cria ~70 objetos

# Carregar df_so_concorridos
if (!exists ("df_so_concorridos")) source("analysis_001_compara_ins_vagas.R")

# criar vetor mod, com a ordem das modalidades
mod <- c("A0","L01","L02","L05","L06", "L09", "L10", "L13", "L14")
# ------------------------------------------------------------------------------

# ==============================================================================
# Iniciar alocação
# definir inicio e fim, para depois rodar for loop

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

# Cria data.frame comparando vagas

# cria diferença entre modelo1 para modelos 2, 3, 4
# sprintf("%+.f" serve para colocar um sinal de + antes dos números
dif_modelo_c2 <- sprintf("%+.f", analise_v_c2[1,] - analise_v_c1[1,])
dif_modelo_c3 <- sprintf("%+.f", analise_v_c3[1,] - analise_v_c1[1,])
dif_modelo_c4 <- sprintf("%+.f", analise_v_c4[1,] - analise_v_c1[1,])
dif_modelo_c5 <- sprintf("%+.f", analise_v_c5[1,] - analise_v_c1[1,])

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
print(paste(candidatos$Curso[1],candidatos$Processo_Seletivo[1]))
print(paste(sum(nvagas),"== sum(nvagas)"))
# obter nota máxima para garantir não haver duplicata dos conjuntos
print(paste((max(candidatos$nota)),"é a nota máxima de candidatos"))
print(paste((min(convocados_c5$nota)),"é a nota minima de convocados_c5"))
# alguns cursos: total não bate com sum nvagas. Resolvido com analysis_02_2
# ------------------------------------------------------------------------------
# salvar como comp_vagas_ curso _ sisu, usa como fonte candidatos
eval(parse(text=(paste0(
  "comp_vagas_",candidatos$Curso[1],"_",candidatos$Processo_Seletivo[1]," <- comparando_vagas"
  ))))
# 
} # fim do loop i, de cada conjunto

# ==============================================================================
# Limpeza
if(T){
# remover objetos analise_n e analise_v
# rm(list = ls(pattern = "^comp_vagas"))
rm(list = ls(pattern = "^convocados"))
rm("candidatos")
}
