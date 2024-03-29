# ==============================================================================
# Arquivo: est_desc_06_ins_por_mod_curso_ano.R
#
# Quantos inscritos tem por ano, por modalidade de inscrição,
# para cada curso?
# usar mod_ins


# Faz:
# Lista número de inscritos por modalidade, por ano, por curso
# Soma quantos casos teve 0 inscritos
# Exporta df_ins_mca.csv

# Cria df_compara_termo_ins

# Modificado em: 2024-07-01.
# diff: compara Termo de Adesão 2022 com inscritos.
# Autor: Mateus Silva Figueiredo
# ==============================================================================


# ==============================================================================
# Carregar dados_ufv

setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")

# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- T   # deseja separar candidatos por curso? 
por_ano   <- F   # deseja separar candidatos por ano? opcional
source("data_04_carregar_dados_UFV.R") # T,F cria ~80 objetos

# ==============================================================================
# objetos necessários

# edições do sisu
sisu_anos<-paste0("SISU",c(2013:2022))

# criar vetor mod, com a ordem das modalidades
mod <- c("A0","L01","L02","L05","L06", "L09", "L10", "L13", "L14")

# ==============================================================================


# ==============================================================================

# nomes das colunas
colunas <- c("Curso","Ano","tot",mod)

# cria data.frame df_ins_mca mca = modalidade curso ano
df_ins_mca <- as.data.frame(
  matrix(nrow=length(lista_cursos)*10,
                   ncol=length(colunas))
)

# nomeia colunas
colnames(df_ins_mca) <- colunas

# ==============================================================================
# preenche coluna Ano
df_ins_mca$Ano <- sisu_anos

# preenche coluna cursos.
# 10 vezes cada curso, mesmo se não foi oferecido naquele ano
for (n in 0:(length(lista_cursos)-1)){
df_ins_mca$Curso[(1+n*10):(10+n*10)]<- lista_cursos[n+1]
}
# depois, se preciso, excluir os cursos com zero inscritos
# ==============================================================================
# determina conjunto curso-ano

# i<-1 # para um conjunto arbitrario
df_ins_mca[i,1] # curso da linha i
df_ins_mca[i,2] # ano da linha i

# para todos os conjuntos
for (i in 1:nrow(df_ins_mca)){
  
# define candidatos com base em i
candidatos <- dados_ufv[Curso==df_ins_mca[i,1]][Processo_Seletivo==df_ins_mca[i,2]]

# preenche tot, A0 até L14
{ #preencher colunas tot, A0 até L06 para todos
df_ins_mca$tot[i] <- candidatos %>% nrow()
df_ins_mca$A0[i]  <- candidatos[mod_ins=="A0"]  %>% nrow()
df_ins_mca$L01[i] <- candidatos[mod_ins=="L01"] %>% nrow()
df_ins_mca$L02[i] <- candidatos[mod_ins=="L02"] %>% nrow()
df_ins_mca$L05[i] <- candidatos[mod_ins=="L05"] %>% nrow()
df_ins_mca$L06[i] <- candidatos[mod_ins=="L06"] %>% nrow()

# preencher L09 até L14 apenas se não for 2013 até 2015
if(!df_ins_mca[i,2] %in% c("SISU2013","SISU2014","SISU2015",
                         "SISU2016","SISU2017")){
df_ins_mca$L09[i] <- candidatos[mod_ins=="L09"] %>% nrow()
df_ins_mca$L10[i] <- candidatos[mod_ins=="L10"] %>% nrow()
df_ins_mca$L13[i] <- candidatos[mod_ins=="L13"] %>% nrow()
df_ins_mca$L14[i] <- candidatos[mod_ins=="L14"] %>% nrow()}
} # fecha preencher tot, A0 até L14
} # fecha for loop de nrow df_ins_mca

df_ins_mca %>% nrow() # 70 cursos * 10 anos = 700 conjuntos hipotéticos

# ------------------------------------------------------------------------------
# mantém apenas linhas em que tot != 0, ou seja, cursos com 1+ inscrito
mydata <- df_ins_mca[df_ins_mca$tot != 0, ]

sum(mydata$A0==0) # 0
paste("L01",sum(is.na(mydata$L01)),"NAs") # 0 antes, 8 depois da transformação "batata"
paste("L02",sum(is.na(mydata$L02)),"NAs") # 0
paste("L05",sum(is.na(mydata$L05)),"NAs") # 0 antes, 11 depois da transformação "batata"
paste("L06",sum(is.na(mydata$L06)),"NAs") # 0
paste("L09",sum(is.na(mydata$L09)),"NAs") # 335
paste("L10",sum(is.na(mydata$L10)),"NAs") # 335
paste("L13",sum(is.na(mydata$L13)),"NAs") # 335
paste("L14",sum(is.na(mydata$L14)),"NAs") # 335

mydata %>% nrow() # 671 conjuntos após remover cursos sem oferta

# ==============================================================================
# colocar NA nos cursos que tiveram 0 vagas oferecidas
# Fonte: Termos de Adesão

# seleciona L01 até L06 com 0 inscritos
selected_rows <- mydata[mydata$L01 == 0 | mydata$L02 == 0 | mydata$L05 == 0 | mydata$L06 == 0, ]

# encontra numero das linhas a serem consertadas:
zero_vagas <- which(mydata$L05 == 0) # c(31, 131, 221, 222, 470, 496, 516, 536, 586, 616, 626)

# linhas a serem consertadas. nestas linhas, 0 deve virar NA.
mydata[zero_vagas,]

# transformação "batata": 0 de L01 e L05 em NA, apenas nas linhas de indice zero_vagas
mydata[zero_vagas, "L01"][mydata[zero_vagas, "L01"] == 0] <- NA
mydata[zero_vagas, "L05"][mydata[zero_vagas, "L05"] == 0] <- NA

# confere se virou NA
mydata[zero_vagas,]

mydata %>% nrow()
df_ins_mca <- mydata; rm(mydata) # atribui à variavel com nome descritivo
df_ins_mca %>% nrow()
# mydata está pronto

# ==============================================================================
# conta quantas modalidades tiveram 0 inscritos
sum(df_ins_mca$L01 == 0 & !is.na(df_ins_mca$L01)) # 0
sum(df_ins_mca$L02 == 0 & !is.na(df_ins_mca$L02)) # 0
sum(df_ins_mca$L05 == 0 & !is.na(df_ins_mca$L05)) # 0
sum(df_ins_mca$L06 == 0 & !is.na(df_ins_mca$L06)) # 0
sum(df_ins_mca$L09 == 0 & !is.na(df_ins_mca$L09)) # 172
sum(df_ins_mca$L10 == 0 & !is.na(df_ins_mca$L10)) # 175
sum(df_ins_mca$L13 == 0 & !is.na(df_ins_mca$L13)) # 207
sum(df_ins_mca$L14 == 0 & !is.na(df_ins_mca$L14)) # 157

# ---

df_ins_mca[df_ins_mca$Ano=="SISU2018",]
sum(df_ins_mca[df_ins_mca$Ano=="SISU2018",] == 0) # 82
sum(df_ins_mca[df_ins_mca$Ano=="SISU2019",] == 0) # 162
sum(df_ins_mca[df_ins_mca$Ano=="SISU2020",] == 0) # 165
sum(df_ins_mca[df_ins_mca$Ano=="SISU2021",] == 0) # 205
sum(df_ins_mca[df_ins_mca$Ano=="SISU2022",] == 0) # 237

# ----


# ---
zero_inscritos <- df_ins_mca[df_ins_mca$L09 == 0 | df_ins_mca$L10 == 0 | df_ins_mca$L13 == 0 | df_ins_mca$L14 == 0, ]
zero_inscritos <- na.omit(zero_inscritos)
zero_inscritos
# 268 conjuntos tem alguma modalidade com 0 inscritos

# ==============================================================================
# exportar df_ins_mca
getwd()

# obtém tempo atual, sem segundos, para nome do arquivo
tempo_atual<-format(Sys.time(), "%Y-%m-%d-%H-%M")

# Nome do documento
save_path <- paste0("df_ins_mca_", tempo_atual, ".csv")

write.csv(df_ins_mca,save_path)

# write.csv(DataFrame Name, "Path to export the DataFrame\\File Name.csv", row.names=FALSE)

# ==============================================================================
# Escrevendo em 2024-01-07

# Objetivo: comparar número de inscritos com número de vagas do Termo de Adesão

# Carregar Termo de Adesão 2022
source("data_05_carregar_termo_adesao.R")
termo_adesao_2022
termo_adesao_2018

# ------------------------------------------------------------------------------

df_ins_mca %>% nrow() # 671

# Interesse apenas entre 2018 e 2022
df_ins_vagas <- subset(df_ins_mca,
                       df_ins_mca$Ano %in% c("SISU2018","SISU2019",
                                        "SISU2020","SISU2021",
                                        "SISU2022"))

df_ins_vagas %>% nrow() # 336

# Apenas cursos estáveis entre 2018 e 2022
df_ins_vagas <- subset(df_ins_vagas,
                       df_ins_vagas$Curso %in% lista_cursos_estavel_18_22)

df_ins_vagas %>% filter(Curso %in% lista_cursos_estavel_18_22) %>% nrow()
df_ins_vagas %>% filter(!Curso %in% lista_cursos_estavel_18_22) # deve ser vazio


df_ins_vagas %>% nrow() # 330

# ------------------------

termo_adesao_2022 %>% colnames()
df_ins_vagas %>% colnames()
colunas <- intersect(termo_adesao_2022 %>% colnames(),df_ins_vagas %>% colnames())
colunas

df_ins_vagas_resumido <- df_ins_vagas %>% select(all_of(colunas))
termo_resumido_22 <- termo_adesao_2022 %>% select(all_of(colunas))
termo_resumido_18 <- termo_adesao_2018 %>% select(all_of(colunas))

i<-1
lista_cursos_estavel_18_22[i]

sum (!termo_resumido_22$Curso %in% lista_cursos_estavel_18_22)
# 2 false = Ciencia de Alimentos RP, e UFV

sum (!termo_resumido_18$Curso %in% lista_cursos_estavel_18_22)
# 3 false = Lic Física, Ciencia de Alimentos RP, e UFV
# ---

# df_termo indica origem dos dados do termo de adesao
# df_ins indica origem dos dados dos inscritos
df_termo_18 <- data.frame(Origem="T2018") # T de termo
df_termo <- data.frame(Origem="T2022") # T de termo
df_ins <- data.frame(Origem=c(2018:2022))

# ------------------------------------------------------------------------------
# Para um curso, apenas T2022:

# Subsetting rows in termo_resumido_22 with Curso equal to 'i'
# Add coluna df_termo antes
subset_termo <- cbind (df_termo,subset(termo_resumido_22, Curso == lista_cursos_estavel_18_22[i]))

# Subsetting rows in df_ins_vagas_resumido with Curso equal to 'i'
subset_df_ins <- cbind (df_ins,subset(df_ins_vagas_resumido, Curso == lista_cursos_estavel_18_22[i]))

# Combining the subsets using rbind
df_compara_termo_ins <- rbind(subset_termo, subset_df_ins)

# ------------------------------------------------------------------------------
# Para todos os cursos

# Creating an empty data frame to store the combined results
df_compara_termo_ins <- data.frame()

for (i in 1:length(lista_cursos_estavel_18_22)) {
  
  # Subsetting rows in termo_resumido_22 with Curso equal to 'i'
  termo_curso_22 <- subset(termo_resumido_22, Curso == lista_cursos_estavel_18_22[i])
  
  # Subsetting rows in termo_resumido_18 with Curso equal to 'i'
  termo_curso_18 <- subset(termo_resumido_18, Curso == lista_cursos_estavel_18_22[i])
  
  # Adiciona coluna Origem T2022 e T2018
  termo_curso_22 <- cbind(df_termo, termo_curso_22)
  termo_curso_18 <- cbind (df_termo_18, termo_curso_18)
  
  # Subsetting rows in df_ins_vagas_resumido with Curso equal to 'i'
  subset_inscritos <- subset(df_ins_vagas_resumido, Curso == lista_cursos_estavel_18_22[i])
  
  # Adiciona coluna Origem 2018:2022
  subset_inscritos <- cbind(df_ins, subset_inscritos)
  
  # Combining
  df_compara_curso <- rbind(termo_curso_18,termo_curso_22,subset_inscritos)
  
  # Cresce df_compara_termo_ins
  df_compara_termo_ins <- rbind(df_compara_termo_ins,df_compara_curso)
}

df_compara_termo_ins

# ==============================================================================
# exportar df_compara_termo_ins
getwd()

# obtém tempo atual, sem segundos, para nome do arquivo
tempo_atual<-format(Sys.time(), "%Y-%m-%d-%H-%M")

# Nome do documento
save_path <- paste0("df_compila_termo_ins_", tempo_atual, ".csv")

write.csv(df_compara_termo_ins,save_path)

# ==============================================================================
