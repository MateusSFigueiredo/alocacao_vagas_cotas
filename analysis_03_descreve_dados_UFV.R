# ==============================================================================
# Arquivo: analysis_03_descreve_dados_UFV.R
#
# incompleto
#
# Modificado em: 2022-12-04
# Autor: Mateus Silva Figueiredo
# ==============================================================================
#
# Usar após data_04_carregar_dados_UFV.R
#
# Dicionário
#
# Inputs:
# dados_ufv com todos os cursos de todos os anos 2013-2022
# dados_2013 até dados_2022 com todos os cursos em cada
# dados_2013 em diante tem colunas id, nota, mod_ins, mod_con,
# Processo_Seletivo e Curso
#
#
# Outputs:
#
# ==============================================================================

# Testando como analisar dados por ano
dados_ano <- dados_2013
ncurso <- 1

dados_ano$Curso %>% unique() -> cursos

subset(dados_ano,Curso==cursos[ncurso]) -> dados_curso

cursos[ncurso]
dados_curso %>% count(mod_ins) -> teste; print(teste)
dados_curso %>% count(mod_con) -> teste; print(teste)



# ==============================================================================
# Analisando informações sobre cursos
# Obtendo informações com print de textos

# Quantos cursos de cada campus?
# Em todos os dados
paste(length(lista_cursos),"cursos ao todo") # 75

paste(sum(endsWith(lista_cursos,"_RP")), "cursos em Rio Paranaíba") # 12

paste(sum(endsWith(lista_cursos,"_FL")), "cursos em Florestal") # 15

paste(length(lista_cursos)
      -sum(endsWith(lista_cursos,"_RP"))
      -sum(endsWith(lista_cursos,"_FL")),
      "cursos em Viçosa") # 48

# Apenas cursos que aparecem nos 10 anos de processo seletivo
paste(length(lista_cursos_estavel),"cursos em todos os 10 anos") #59

print(paste(sum(endsWith(lista_cursos_estavel,"_RP")), 
            "cursos de Rio Paranaíba em todos os 10 anos.")) #12

print(paste(sum(endsWith(lista_cursos_estavel,"_FL")), 
            "cursos de Florestal em todos os 10 anos.")) #5

print(paste(length(lista_cursos_estavel)
            -sum(endsWith(lista_cursos_estavel,"_RP"))
            -sum(endsWith(lista_cursos_estavel,"_FL")),
            "cursos de Viçosa em todos os 10 anos.")) #42


# ==============================================================================
# Organizar df_cursos_mudou e exportar

# Definir ordem das linhas
ordem<-c(
  "LICENCIATURA EM CIENCIAS BIOLOGICAS - FL",
  "CIENCIAS BIOLOGICAS - FL",
  "LICENCIATURA EM FISICA - FL",
  "FISICA - FL",
  "LICENCIATURA EM MATEMATICA - FL",
  "MATEMATICA - FL",
  "LICENCIATURA EM EDUCACAO FISICA - FL",
  "EDUCACAO FISICA - FL",
  "LICENCIATURA EM QUIMICA - FL",
  "QUIMICA - FL",
  "LICENCIATURA EM FISICA",
  "ECONOMIA DOMESTICA",
  "SERVIÇO SOCIAL",
  "EDUCACAO FISICA",
  "EDUCACAO FISICA - BACHARELADO",
  "EDUCACAO FISICA - LICENCIATURA")

# Reorganizar linhas seguindo ordem
df_cursos_mudou %>%
  slice(match(ordem, curso)) -> df_cursos_mudou

# Exportar df_cursos_mudou
getwd()
# write.csv(df_cursos_mudou,
#           "C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas/privado/df_cursos_mudou.csv",
#           row.names=FALSE)

