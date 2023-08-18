# ==============================================================================
# Arquivo: analysis_001_compara_ins_vagas.R
# Há mais inscritos ou mais vagas para cada curso, em cada modalidade?
#
#
# Modificado em 2023-05-17
# Autor: Mateus Silva Figueiredo
# ==============================================================================

# TESTANDO

# ==============================================================================

setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")
getwd()
# list.files()

# ------------------------------------------------------------------------------
# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- T   # deseja separar candidatos por curso? obrigatório
por_ano   <- F   # deseja separar candidatos por ano? opcional
source("data_04_carregar_dados_UFV.R") # cria ~80 objetos
# data_04 faz setwd da pasta /privado

# Criar vetores n_vagas para cada curso, com base no termo de adesão de 2022
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")
source("data_05_carregar_termo_adesao.R") # cria ~70 objetos

# criar vetor mod, com a ordem das modalidades
mod <- c("A0","L01","L02","L05","L06", "L09", "L10", "L13", "L14")

# ------------------------------------------------------------------------------
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas") # definir pasta
getwd()                                                 # conferir pasta


# ------------------------------------------------------------------------------
# Escolher um curso
# cu <- "LICENCIATURA_EM_QUIMICA_FL" # pelo nome
 cu <- lista_cursos_18_22[i]; # pelo número na lista

# Escolher uma edição do SISU
edicao <- "SISU2018" # pelo nome

# Definir n_vagas, a partir do nvagas_CURSO gerado por data_05
nvagas <- get(paste0("nvagas_",cu))

# ------------------------------------------------------------------------------

# começa aqui?

# cria df_concorridos para ter cursos com mais candidatos do que vagas
df_concorridos<-data.frame("curso"=lista_cursos_18_22,
                                  "SISU2018"=NA,
                                  "SISU2019"=NA,
                                  "SISU2020"=NA,
                                  "SISU2021"=NA,
                                  "SISU2022"=NA)

# Carregar lista_todos # todos os candidatos para determinado curso e ano
# lista_todos <- 

# i<-1 # from 1:length(lista_cursos_18_22)
# j<-4 # from 2:5

for (i in 1:length(lista_cursos_18_22)){
for (j in 2:6){

lista_todos <- get(paste0("dados_",df_concorridos[i,1])) %>% subset(Processo_Seletivo==colnames(df_concorridos)[j])

ninscritos <- lista_todos$mod_ins %>% table
ninscritos; nvagas

length(ninscritos) == length(nvagas) # há 1 ou mais inscrito para cada modalidade
#prod(ninscritos >= nvagas) # 1 se todos TRUE. 0 se ao menos 1 FALSE (não concorrido)

if (length(ninscritos) == length(nvagas)){ # se houver 1+ inscrito para cada
  concorrido <<- prod(ninscritos >= nvagas)
} else {concorrido <<- FALSE}

df_concorridos[i,j]<-concorrido

} # end j loop
} # end i loop

# read me df_concorrido
# se 1, então há mais candidatos do que vagas naquele ano naquele curso
# se 0, há alguma modalidade com mais vagas do que candidatos 
# apenas MEDICINA e MEDICINA_VETERINÁRIA tem 1 em todos os anos 

# ==============================================================================

print("Fim do arquivo")
# Fim do arquivo
