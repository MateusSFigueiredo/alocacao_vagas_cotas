# ==============================================================================
# Arquivo: data_04_carregar_dados_UFV.R

#
# Modificado em: 2023-08-23
# Autor: Mateus Silva Figueiredo
#
# Carrega dados da UFV
# Separa em data.frames por ano
# Separa em data.frames por curso

# diff:
# sempre carrega lista_cursos (linha 250), e não apaga (linha 383)

# ==============================================================================
#
# Dicionário
#
# Inputs:
# Arquivo "Wania SISU 2012 a 2022 - 2022-11-17 - original.csv"
#
# Outputs:
# dados_ufv com todos os cursos de todos os anos 2013-2022
# dados_2013 até dados_2022 com todos os cursos em cada
# dados_2013 em diante tem colunas id, nota, mod_ins, mod_con,
# Processo_Seletivo e Curso
# lista_cursos, lista_cursos_estavel, lista_cursos_mudou, lista_cursos_18_22
#
# ==============================================================================
# Preparação
library(tidyverse)
library(data.table)

# Disable scientific notation
# Para manter o Número de Inscrição inalterado
options(scipen = 999)

# ==============================================================================
# Carregar dados

setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas/privado")
# list.files()

colunas<-c("Identificacao","Processo_Seletivo",
           #         "Sexo", "Idade_Ingresso","UF","Cidade",                     
           "ENEM",
           "Modalidade_Inscrita","Numero_Chamada_Convocacao","Modalidade_Convocada",
           "Codigo_Modalidade_Inscrita","Codigo_Modalidade_Convocada",
           "Curso") # apenas colunas de interesse

dados_ufv<-fread("Wania SISU 2012 a 2022 - 2022-11-17 - original.csv",
                 #                   nrow=10, # caso queira carregar poucos dados
                 select = colunas,
                 dec=",")

# View(dados_ufv) # 170199 obs.
# dados_ufv%>% summary()

# ==============================================================================
# Limpar dados_ufv

# Conferir lista de cursos em ordem alfabética
# unique(dados_ufv$Curso) %>% sort()

# Sinonimizar cursos que mudaram de nome.
{
  # Mudanças de de 2013 para 2014: Letras, Pedagogia, Comunicação Social.
  dados_ufv[dados_ufv == "LETRAS - LICENCIATURA"] <- "LETRAS" 
  dados_ufv[dados_ufv == "PEDAGOGIA - LICENCIATURA"] <- "PEDAGOGIA"
  dados_ufv[dados_ufv == "COMUNICACAO SOCIAL - JORNALISMO"] <- "COMUNICACAO SOCIAL"
  dados_ufv[dados_ufv == "SUPERIOR TEC. EM GESTAO AMBIENTAL - FL"] <- "GESTAO AMBIENTAL - FL"
  # Mudança de 2017 para 2018:
  dados_ufv[dados_ufv == "CIENCIAS DE ALIMENTOS - RP"] <- "CIENCIA E TECNOLOGIA DE ALIMENTOS - RP"
  
  # Licenciaturas de Florestal:
  dados_ufv[dados_ufv == "LIC. EM CIENCIAS BIOLOGICAS - FL"] <- "LICENCIATURA EM CIENCIAS BIOLOGICAS - FL"
  dados_ufv[dados_ufv == "CIENCIAS BIOLOGICAS - FL"] <- "LICENCIATURA EM CIENCIAS BIOLOGICAS - FL"
  dados_ufv[dados_ufv == "QUIMICA - FL"] <- "LICENCIATURA EM QUIMICA - FL"
  dados_ufv[dados_ufv == "FISICA - FL"] <- "LICENCIATURA EM FISICA - FL"
  dados_ufv[dados_ufv == "MATEMATICA - FL"] <- "LICENCIATURA EM MATEMATICA - FL"
  dados_ufv[dados_ufv == "EDUCACAO FISICA - FL"] <- "LICENCIATURA EM EDUCACAO FISICA - FL"
}
# excluir espaços dos cursos
gsub(" ", "_", dados_ufv$Curso) -> dados_ufv$Curso
gsub("_-_", "_", dados_ufv$Curso) -> dados_ufv$Curso

# remover acentos
iconv(dados_ufv$Curso, to="ASCII//TRANSLIT") -> dados_ufv$Curso

# Conferir lista de cursos em ordem alfabética
unique(dados_ufv$Curso) %>% sort()

# ==============================================================================
# Tornar dados compatíveis com código já escrito

# coluna ENEM vira coluna nota
colnames(dados_ufv)[colnames(dados_ufv) == "ENEM"] <- "nota"

# coluna Identificacao vira coluna id
colnames(dados_ufv)[colnames(dados_ufv) == "Identificacao"] <- "id"

# ------------------------------------------------------------------------------
# Criar id único composto por ano, 0, id original
dados_ufv$id <-
  paste0(
    gsub("SISU","",dados_ufv$Processo_Seletivo), # remover escrito "SISU"
    sprintf("%06d", dados_ufv$id) # print number with leading zeroes
  )

# ------------------------------------------------------------------------------
# Criar colunas mod_ins e mod_con.
# Nomes das categorias UFV vai ser UFV1 até UFV9, para depois substituir.
# Criar coluna mod_ins, para não confundir os números
dados_ufv$mod_ins <- paste0("UFV",dados_ufv$Modalidade_Inscrita)

# Criar coluna mod_con, para não confundir os números
dados_ufv$mod_con <- paste0("UFV",dados_ufv$Modalidade_Convocada)


dados_ufv %>% count(Modalidade_Inscrita) == dados_ufv %>% count(mod_ins) 
# coluna n deve dar tudo TRUE

# dados_ufv %>% count(Modalidade_Convocada) == dados_ufv %>% count(mod_con)
# coluna n deve dar tudo TRUE

# Substitui modalidade UFV por modalidade INEP.

# Regra para SISU 2013 a 2017. 
subset(dados_ufv,Processo_Seletivo %in% c("SISU2013","SISU2014",
                                          "SISU2015","SISU2016",
                                          "SISU2017")) -> regra1
{
  regra1[regra1 == "UFV1"] <- "L02" # UFV1 == L2. pub + bxa + ppi
  regra1[regra1 == "UFV2"] <- "L01" # UFV2 == L1. pub + bxa
  regra1[regra1 == "UFV3"] <- "L06" # UFV3 == L6. pub + ppi
  regra1[regra1 == "UFV4"] <- "L05" # UFV4 == L5. pub 
  regra1[regra1 == "UFV5"] <- "A0" # UFV5 == A0. 
  regra1[regra1 == "UFVNA"] <- "NA" #NA 
} # fim regra 2013 a 2016

# Regra para SISU 2018 a 2022
subset (dados_ufv,Processo_Seletivo %in% c("SISU2018",
                                           "SISU2019","SISU2020",
                                           "SISU2021","SISU2022")) -> regra2
{
  regra2[regra2 == "UFV1"] <- "L02" # UFV1 == L2. pub + bxa + ppi
  regra2[regra2 == "UFV3"] <- "L01" # UFV2 == L1. pub + bxa
  regra2[regra2 == "UFV5"] <- "L06" # UFV3 == L6. pub + ppi
  regra2[regra2 == "UFV7"] <- "L05" # UFV4 == L5. pub 
  regra2[regra2 == "UFV9"] <- "A0" # UFV5 == A0. 
  
  regra2[regra2 == "UFV2"] <- "L09"  # UFV2 == L2. pub + bxa + ppi + pcd
  regra2[regra2 == "UFV4"] <- "L10" # UFV4 == L1. pub + bxa + pcd
  regra2[regra2 == "UFV6"] <- "L13" # UFV6 == L6. pub + ppi + pcd
  regra2[regra2 == "UFV8"] <- "L14" # UFV8 == L5. pub + pcd
  
  regra2[regra2 == "UFVNA"] <- "NA" #NA 
} # fim regra 2013 a 2016

# Junta regra1 com regra2
rbind (regra1, regra2) -> dados_ufv; rm(regra1, regra2)

# # Conferência
# unique (dados_ufv$Modalidade_Inscrita)
# count (dados_ufv,mod_ins)
# 
# subset (dados_ufv,mod_ins=="A0")[1]
# subset (dados_ufv,mod_ins=="L1")[1]
# subset (dados_ufv,mod_ins=="L2")[1]
# subset (dados_ufv,mod_ins=="L5")[1]
# subset (dados_ufv,mod_ins=="L6")[1]
# subset (dados_ufv,mod_ins=="L9")[1]
# subset (dados_ufv,mod_ins=="L10")[1]
# subset (dados_ufv,mod_ins=="L13")[1]
# subset (dados_ufv,mod_ins=="L14")[1]

paste("dados_ufv tem",ncol(dados_ufv),"colunas")   

# ==============================================================================
# acrescentar colunas pub, bxa, ppi, pcd
# assume que nenhum candidato omitiu privilégio

# criar coluna pub para egressos de escola publica
dados_ufv$mod_ins %in% c("L01","L02","L06","L05","L09","L14","L13","L10") -> dados_ufv$pub

# criar coluna bxa para baixa renda
dados_ufv$mod_ins %in% c("L01","L02","L09","L10") -> dados_ufv$bxa

# criar coluna ppi para estudantes pretos pardos indigenas
dados_ufv$mod_ins %in% c("L02","L06","L10","L14") -> dados_ufv$ppi

# criar coluna pcd para estudantes pessoas com deficiência
dados_ufv$mod_ins %in% c("L09","L10","L13","L14") -> dados_ufv$pcd

paste("dados_ufv tem",ncol(dados_ufv),"colunas")

# ==============================================================================
# Criar coluna Centro (para campus e centro de ciências)
dados_ufv$Centro <- 0

# Criar listas de cursos cursos_cca até cursos_crp e cursos_caf
{
  cursos_cca <- c("AGRONEGOCIO","AGRONOMIA","COOPERATIVISMO","ENGENHARIA_AGRICOLA_E_AMBIENTAL","ENGENHARIA_FLORESTAL", "ZOOTECNIA")
  cursos_ccb <- c("BIOQUIMICA","CIENCIAS_BIOLOGICAS","EDUCACAO_FISICA","EDUCACAO_FISICA_BACHARELADO", "EDUCACAO_FISICA_LICENCIATURA","ENFERMAGEM", "LICENCIATURA_EM_CIENCIAS_BIOLOGICAS","MEDICINA", "MEDICINA_VETERINARIA","NUTRICAO")
  cursos_cce <- c("ARQUITETURA_E_URBANISMO","CIENCIA_DA_COMPUTACAO","CIENCIA_E_TECNOLOGIA_DE_LATICINIOS","ENGENHARIA_AMBIENTAL","ENGENHARIA_CIVIL","ENG._DE_AGRIMENSURA_E_CARTOGRAFICA","ENGENHARIA_DE_ALIMENTOS","ENGENHARIA_DE_PRODUCAO", "ENGENHARIA_ELETRICA","ENGENHARIA_MECANICA","ENGENHARIA_QUIMICA","FISICA","LICENCIATURA_EM_FISICA", "LICENCIATURA_EM_MATEMATICA", "LICENCIATURA_EM_QUIMICA","MATEMATICA","QUIMICA")
  cursos_cch <- c("ADMINISTRACAO","CIENCIAS_CONTABEIS", "CIENCIAS_ECONOMICAS","CIENCIAS_SOCIAIS","COMUNICACAO_SOCIAL", "DANCA","DIREITO","ECONOMIA_DOMESTICA","EDUCACAO_INFANTIL","GEOGRAFIA", "HISTORIA", "LETRAS", "PEDAGOGIA", "SECRETARIADO_EXECUTIVO_TRILINGUE","SERVICO_SOCIAL")
  # EDUCACAO_DO_CAMPO está ausente dos dados pois não utiliza SISU
  cursos_crp <- c("ADMINISTRACAO_DIURNO_RP","ADMINISTRACAO_NOTURNO_RP","AGRONOMIA_RP", "CIENCIA_E_TECNOLOGIA_DE_ALIMENTOS_RP","CIENCIAS_BIOLOGICAS_RP","CIENCIAS_CONTABEIS_RP","ENGENHARIA_CIVIL_RP", "ENGENHARIA_DE_PRODUCAO_RP", "NUTRICAO_RP","QUIMICA_RP", "SISTEMAS_DE_INFORMACAO_DIURNO_RP","SISTEMAS_DE_INFORMACAO_NOTURNO_RP")
  cursos_caf <- c("ADMINISTRACAO_FL","AGRONOMIA_FL","CIENCIA_DA_COMPUTACAO_FL","ENGENHARIA_DE_ALIMENTOS_FL", "GESTAO_AMBIENTAL_FL","LICENCIATURA_EM_CIENCIAS_BIOLOGICAS_FL","LICENCIATURA_EM_EDUCACAO_FISICA_FL", "LICENCIATURA_EM_FISICA_FL","LICENCIATURA_EM_MATEMATICA_FL", "LICENCIATURA_EM_QUIMICA_FL")
  # length(cursos_cca)+length(cursos_ccb)+length(cursos_cce)+length(cursos_cch)+length(cursos_caf)+length(cursos_crp) == length(lista_cursos)
}

# preencher coluna Centro
{
  dados_ufv$Centro[dados_ufv$Curso %in% cursos_cca] <- "CCA"
  dados_ufv$Centro[dados_ufv$Curso %in% cursos_ccb] <- "CCB"
  dados_ufv$Centro[dados_ufv$Curso %in% cursos_cce] <- "CCE"
  dados_ufv$Centro[dados_ufv$Curso %in% cursos_cch] <- "CCH"
  dados_ufv$Centro[dados_ufv$Curso %in% cursos_crp] <- "CRP"
  dados_ufv$Centro[dados_ufv$Curso %in% cursos_caf] <- "CAF"
}

paste("dados_ufv tem",ncol(dados_ufv),"colunas")

# ==============================================================================
# ==============================================================================
# ==============================================================================

# # Deseja carregar dados por ano? 
# por_ano <- F # T = sim, F = não

if (por_ano){
  
  for (ano in 2013:2022) {
    processo_seletivo <- paste0("SISU", ano)
    filtro <- dados_ufv[Processo_Seletivo == processo_seletivo]
    assign(paste0("dados_", ano), filtro)
  }
  
  rm(processo_seletivo,filtro) # remove desnecessários
  
  print("Dados por ano estão carregados")
} # fim do if (por_ano) # obrigado Chat GPT 

# ==============================================================================
# Trabalhando por curso
# Cursos já devem ter sido sinonimizados lá em cima na linha 54

# Identificar lista de cursos.
lista_cursos <- unique(dados_ufv$Curso) %>% sort()
# lista_cursos # 70 cursos

##  Deseja carregar dados por curso? 
# por_curso <- F # T = sim, F = não

if (por_curso){
  
  for (cu in lista_cursos) {
    filtro <- dados_ufv[Curso == cu]
    assign(paste0("dados_", cu), filtro)
  }
  
  rm(cu,filtro) # remove desnecessários
  
  print("Dados por curso estão carregados")
} # fim do if (por_curso) # obrigado Chat GPT 

# ==============================================================================
# Cursos que abriram, fecharam ou mudaram de nome

# # Alguns cursos abriram, fecharam, ou mudaram de nome. Ver um exemplo:
# # "ECONOMIA DOMESTICA" aparece nos anos 2013 até 2015.
# unique(subset(dados_ufv,Curso=="ECONOMIA DOMESTICA")$Processo_Seletivo)

# Criar df_cursos_mudou para cursos que mudaram de nome
# Cria lista_cursos_mudou para cursos que aparecem em menos de 10 anos
lista_cursos_mudou <- character()

# Preencher lista_cursos_mudou
for (i in 1:length(lista_cursos)) {
  
  if (
    unique(subset(dados_ufv,Curso==lista_cursos[i])$Processo_Seletivo) %>% length() != 10
  ) append(lista_cursos_mudou,lista_cursos[i]) ->> lista_cursos_mudou
}

lista_cursos_mudou

# ------------------------------------------------------------------------------

# Gerar data.frame df_cursos_mudou com cursos que mudaram e quais anos eles existiram
N=1:length(lista_cursos_mudou)

rbindlist(
  lapply(N, function(i) {
    paste(unique(subset(
      dados_ufv,Curso==lista_cursos_mudou[i])$Processo_Seletivo),
      collapse=", ") -> anos_por_curso
    data.table(curso = lista_cursos_mudou[i], anos = anos_por_curso)
  }))  ->> df_cursos_mudou

# Limpar df_cursos_mudou
# Remover palavra "SISU"
df_cursos_mudou$anos<-gsub("SISU","",df_cursos_mudou$anos)

# ------------------------------------------------------------------------------
# Resumir anos. Não otimizado, mas funciona para estes dados.
# Ordem das linhas não importa.
{
  df_cursos_mudou[df_cursos_mudou == "2017, 2018, 2019, 2020, 2021, 2022"] <- "2017 até 2022"
  df_cursos_mudou[df_cursos_mudou == "2013, 2014, 2015, 2016, 2017, 2018"] <- "2013 até 2018"
  df_cursos_mudou[df_cursos_mudou == "2013, 2014, 2015, 2016"] <- "2013 até 2016"
  df_cursos_mudou[df_cursos_mudou == "2013, 2014, 2015"] <- "2013 até 2015"
}

# Conferir
# df_cursos_mudou # está criado

# ------------------------------------------------------------------------------
# Cursos que NÃO mudaram de nome
# Cria lista_cursos_mudou para cursos que aparecem em exatos 10 anos
lista_cursos_estavel <- character()

# Preencher lista_cursos_estavel
for (i in 1:length(lista_cursos)) {
  
  if (
    unique(subset(dados_ufv,Curso==lista_cursos[i])$Processo_Seletivo) %>% length() == 10
  ) append(lista_cursos_estavel,lista_cursos[i]) ->> lista_cursos_estavel
}

summary(lista_cursos_estavel) # length == 64
# print(lista_cursos_estavel)
# Devem aparecer apenas uma vez:
# PEDAGOGIA, LETRAS, COMUNICAÇÃO SOCIAL
# GESTAO AMBIENTAL - FL, LICENCIATURA EM CIENCIAS BIOLOGICAS - FL
# CIENCIA E TECNOLOGIA DE ALIMENTOS - RP
# Licenciaturas de Florestal

# ------------------------------------------------------------------------------
# Cursos que existem em 2018 e em 2022, para diminuir escopo da pesquisa
dados_ufv[Processo_Seletivo=="SISU2018"]$Curso %>% unique() -> lista_cursos_18
dados_ufv[Processo_Seletivo=="SISU2022"]$Curso %>% unique() -> lista_cursos_22

intersect(lista_cursos_18,lista_cursos_22) %>% sort() -> lista_cursos_18_22
rm(lista_cursos_18,lista_cursos_22)

# ------------------------------------------------------------------------------

# como descobrir numero a partir do curso
# which(lista_cursos=="MEDICINA")
# which(lista_cursos=="PEDAGOGIA")
# which(lista_cursos_estavel=="MEDICINA")

# como descobrir curso a partir do número
# lista_cursos[59]
# lista_cursos_estavel[54]

# ==============================================================================
# exportar dados de poucos cursos

# dados_MEDICINA %>% subset(Processo_Seletivo=="SISU2019") -> dados_MEDICINA_2019
# dados_PEDAGOGIA %>% subset(Processo_Seletivo=="SISU2019") -> dados_PEDAGOGIA_2019
# 
# write.csv(dados_MEDICINA_2019, "dados_medicina_2019.csv", row.names=FALSE)
# write.csv(dados_PEDAGOGIA_2019, "dados_pedagogia_2019.csv", row.names=FALSE)

# ==============================================================================
# Limpeza
# rm(dados_curso)

# Para apagar todos os objetos dados_ADMINISTRACAO até dados_ZOOTECNIA:
# for (i in 1:length(lista_cursos_estavel)) {
# eval(parse(text=(paste0("rm(dados_",
#                         lista_cursos_estavel[i],
#                         ")"))))}

#rm(por_curso)

# limpeza caso não for trabalhar com cursos
if (!por_curso){
  rm(cursos_cca,cursos_ccb,cursos_cce,cursos_cch,cursos_crp,cursos_caf)
#  rm(lista_cursos,lista_cursos_18_22,lista_cursos_estavel,lista_cursos_mudou)
}

# setwd para facilitar uso de outros scripts
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")
# ==============================================================================
# Referências

# rbindlist(lapply(N, function(i) {...}))
# https://stackoverflow.com/questions/43147235/for-loop-in-r-return-error-object-not-found

# ==============================================================================
# Fim do código