# ==============================================================================
# Arquivo: est_desc_02.R

#
# Modificado em: 2023-05-12
# Autor: Mateus Silva Figueiredo
#
# Utiliza dados_ufv carregado por data_04_carregar_dados_UFV

### OBJETIVO A FAZER:
# Ver quais cursos tem mais de 100 candidatos por ano
# NÃO FUNCIONA AINDA

# ==============================================================================
# Carregar dados_ufv
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")

# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- F   # deseja separar candidatos por curso? 
por_ano   <- F   # deseja separar candidatos por ano? opcional
source("data_04_carregar_dados_UFV.R") # cria ~80 objetos

# ------------------------------------------------------------------------------

# Escolher um curso
# cu <- "LICENCIATURA_EM_QUIMICA_FL" # pelo nome
cu <- lista_cursos_18_22[1]; # pelo número na lista

# Escolher uma edição do SISU
edicao <- "SISU2019" # pelo nome

# ==============================================================================

dados_ufv %>% head()

dados_ufv[Processo_Seletivo == edicao][Curso == cu] %>% nrow()
