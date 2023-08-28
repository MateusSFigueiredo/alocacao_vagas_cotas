# ==============================================================================
# Arquivo: analysis_001_compara_ins_vagas.R
# Há mais inscritos ou mais vagas para cada curso, em cada modalidade?
#
# Escrito em 2023-05-17
# Modificado em 2023-08-28, para usar lista_cursos_estavel_18_22 em vez de lista_cursos_18_22
# Autor: Mateus Silva Figueiredo

# Atualização 2023-0-26: gera df_so_concorridos para gerar candidatos

# Aviso: se mudar de nome, mudar também em analysis_01_todas_conc.R, que usa source este
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
# Escolher apenas um curso
# i<-1
# # cu <- "LICENCIATURA_EM_QUIMICA_FL" # pelo nome
#  cu <- lista_cursos_estavel_18_22[i]; # pelo número na lista
# 
# # Escolher uma edição do SISU
# edicao <- "SISU2018" # pelo nome
# 
# # Definir n_vagas, a partir do nvagas_CURSO gerado por data_05
# nvagas <- get(paste0("nvagas_",cu))

# ------------------------------------------------------------------------------

# Com todos os cursos

# cria df_concorridos para ter cursos com mais candidatos do que vagas
df_concorridos<-data.frame("curso"=lista_cursos_estavel_18_22,
                                  "SISU2018"=NA,
                                  "SISU2019"=NA,
                                  "SISU2020"=NA,
                                  "SISU2021"=NA,
                                  "SISU2022"=NA)

# Carregar inscritos_curso # todos os candidatos para determinado curso e ano
# inscritos_curso_ano <- 

# cu<-1 # from 1:length(lista_estavel_cursos_18_22)
# j<-2 # from 2:6 # colunas de df_concorridos

for (cu in 1:length(lista_cursos_estavel_18_22)){
for (j in 2:6){

# cria inscritos_curso_ano
inscritos_curso_ano <- get(paste0("dados_",df_concorridos[cu,1])) %>% subset(Processo_Seletivo==colnames(df_concorridos)[j])

# ----------------------
# cria vetor ninscritos e preenche
ninscritos<-numeric()

# preenche
for (m in 1:9){
inscritos_curso_ano[mod_ins==mod[m]] %>% nrow() -> ninscritos[m]
}
ninscritos; nvagas
# ----------------------

length(ninscritos) == length(nvagas) # há 1 ou mais inscrito para cada modalidade
#prod(ninscritos >= nvagas) # 1 se todos TRUE. 0 se ao menos 1 FALSE (não concorrido)

if (length(ninscritos) == length(nvagas)){ # se houver 1+ inscrito para cada
  concorrido <<- prod(ninscritos >= nvagas)
} else {concorrido <<- FALSE}

df_concorridos[cu,j]<-concorrido

} # end j loop # colunas de df_concorridos
} # end cu loop # cursos

View(df_concorridos)
nrow(df_concorridos) %>% paste("cursos analisados, estáveis entre 2018:2022")

# ---------------------------------------
# Totais
sum(df_concorridos[,2:6]) %>% paste("células prenchidas") # total de células preenchidas
(df_concorridos %>% length * df_concorridos %>% nrow) %>% paste("células ao todo") # total de células

# cria coluna df_concorridos$SISU_todos
# df_concorridos$SISU_todos <- rowSums( df_concorridos[,2:6] )

# readme df_concorrido
# se 1, então há mais candidatos do que vagas naquele ano naquele curso
# se 0, há alguma modalidade com mais vagas do que candidatos 
# apenas MEDICINA e MEDICINA_VETERINÁRIA tem 1 em todos os anos 

# idealmente, apenas usar células preenchidas para correr análises ingênuas
# análise ingênua = não sabe lidar com mais vagas do que candidatos

# ==============================================================================
# criar dataframe "df só concorridos" apenas com conjuntos concorridos
# objetivo: usar apenas conjuntos de estudantes de cursos concorridos

# Convert the dataframe to long format # obrigado ChatGPT
df_so_concorridos <- df_concorridos %>%
  pivot_longer(cols = starts_with("SISU"),
               names_to = "year",
               values_to = "availability") %>%
  filter(availability == 1) %>%
  select(curso, year)

# Print the resulting dataframe
print(df_so_concorridos)
df_so_concorridos %>% nrow()

# ----
# um conjunto concorrido = inscritos para um curso, em um ano,
# que tenha mais candidatos do que vagas em todas as modalidades:
# i<-2
dados_ufv[Curso==df_so_concorridos[i,1]][Processo_Seletivo==df_so_concorridos[i,2]]

# gera conjunto candidatos
candidatos <- dados_ufv[Curso==df_so_concorridos[i,1]][Processo_Seletivo==df_so_concorridos[i,2]]
# ==============================================================================

print("Fim do arquivo")
# Fim do arquivo
