# ==============================================================================
# Arquivo: analysis_04_descreve_UFV_mobilidade.R
#
# Quais os efeitos da mobilidade de modalidade?
#
# Modificado em: 2022-12-26.
# Autor: Mateus Silva Figueiredo
# ==============================================================================
#
# Usar após data_04_carregar_dados_UFV.R
#
# Dicionário
#
# Inputs:
# dados_ufv com todos os cursos de todos os anos 2013-2022
# Processo_Seletivo e Curso
# dados_curso
lista_cursos
lista_cursos_estavel
#
# Outputs:
# Funções:
# seleciona_mudaram %>% class()
# seleciona_status %>% class()
# seleciona_todos %>% class()
# Tabelas
#
# ==============================================================================
# Preparação

belch2 <- function(x, y) { eval(parse(text=(paste0(x, y, sep=""))))}
belch3 <- function(x, y, z) {eval(parse(text=(paste0(x, y, z,sep=""))))}

mod <- c("A0","L01","L02","L05","L06", "L09", "L10", "L13", "L14")
mod_cotas <- mod[mod!= "A0"] # cria mod_cotas sem A0

anos<-c(2013:2022)
sisu_anos<-paste0("SISU",anos)

# ==============================================================================

# Descrevendo mobilidade de modalidade. Cria df_mob_cota_A0 e df_mob_A0_cota
# Quantos estudantes foram convocados por mod_con diferente de mod_ins?

# Criar data.frame df_mob_cota_A0, para estudantes que foram de cota para A0
df_mob_cota_A0 <- data.frame(matrix(NA,
                                    nrow = length(lista_cursos_estavel),
                                    ncol = length(sisu_anos)+1))

# Criar data.frame df_mob_A0_cota, para estudantes que foram de A0 para cota
df_mob_A0_cota <- data.frame(matrix(NA,
                                    nrow = length(lista_cursos_estavel),
                                    ncol = length(sisu_anos)+1))

# Coloca nome nas colunas dos df
colnames(df_mob_A0_cota) <- c(sisu_anos,"lista_cursos_estavel")
colnames(df_mob_cota_A0) <- c(sisu_anos,"lista_cursos_estavel")

# Preenche coluna lista_cursos_estavel
df_mob_A0_cota$lista_cursos_estavel <- lista_cursos_estavel
df_mob_cota_A0$lista_cursos_estavel <- lista_cursos_estavel


# Prepara loop
for (i in 1:length(lista_cursos_estavel)){
  for (j in 1:length(sisu_anos)){
    
    # Criar dados_curso_ano
    dados_ufv[which 
              (Curso==lista_cursos_estavel[i])][which
                                                (Processo_Seletivo==sisu_anos[j])] -> dados_curso_ano
    
    # Quantos estudantes convocados tem mod_ins != mod_con?
    # na.omit(dados_curso_ano)[which (mod_ins != mod_con)] %>% nrow()
    
    # Quantos estudantes convocados foram de A0 para cota
    na.omit(dados_curso_ano)[
      which (mod_ins == "A0")][which (mod_con != "A0")] %>% nrow() -> df_mob_A0_cota[i,j]
    
    
    # Quantos estudantes convocados foram de cota para A0
    na.omit(dados_curso_ano)[
      which (mod_ins != "A0")][which (mod_con == "A0")] %>% nrow() -> df_mob_cota_A0[i,j]
    
  } # fim do loop j
} # fim do loop i
# estão criados e preenchidos df_mob_cota_A0 e df_mob_A0_cota, sem linha de total


#-------------------------------------------------------------------------------
tail(df_mob_A0_cota,2) # Confere ultimas linhas

# df_mob_A0_cota: cria linha para total
df_mob_A0_cota[nrow(df_mob_A0_cota)+1,]<-NA
df_mob_A0_cota$lista_cursos_estavel[nrow(df_mob_A0_cota)] <- "Total"

# df_mob_A0_cota: preenche linha Total
for (i in 1:(ncol(df_mob_A0_cota)-1)){
  df_mob_A0_cota[1:nrow(df_mob_A0_cota)-1,i] %>% sum() -> 
    df_mob_A0_cota[nrow(df_mob_A0_cota),  i]
}
tail(df_mob_A0_cota,3) # Confere ultimas linhas

#-------------------------------------------------------------------------------
tail(df_mob_cota_A0,2) # Confere ultimas linhas

# df_mob_cota_A0: cria linha para total em 
df_mob_cota_A0[nrow(df_mob_cota_A0)+1,]<-NA
df_mob_cota_A0$lista_cursos_estavel[nrow(df_mob_cota_A0)] <- "Total"

# df_mob_cota_A0: preenche linha Total
for (i in 1:(ncol(df_mob_cota_A0)-1)){
  df_mob_cota_A0[1:nrow(df_mob_cota_A0)-1,i] %>% sum() -> 
    df_mob_cota_A0[nrow(df_mob_cota_A0),  i]
}
tail(df_mob_cota_A0,3) # Confere ultimas linhas

# Compara
tail(df_mob_cota_A0,1)
tail(df_mob_A0_cota,1)

print("Estão preenchidas df_mob_A0_cota e df_mob_cota_A0 com linha de total")

#===============================================================================
# Cria funções

# Cria função seleciona_todos

# Seleciona todos os inscritos para um ano e curso
seleciona_todos <- function (ano,n_curso){return(
  (dados_ufv)[which
              (Processo_Seletivo==sisu_anos[ano-2012])][which
                                                        (Curso==lista_cursos[n_curso])])}

seleciona_todos(2022,54) # %>% View() # todos do ano 2022 do curso lista_cursos[54]

#-------------------------------------------------------------------------------
# Cria função seleciona_status

# Seleciona todos os inscritos para um ano e curso baseado em status de convocação
# Se convocado=TRUE, seleciona apenas convocados
# Se convocado=FALSE, seleciona apenas não convocados

seleciona_status <- function (ano,n_curso,convocado){
  (dados_ufv)[which
              (Processo_Seletivo==sisu_anos[ano-2012])][which
              (Curso==lista_cursos[n_curso])] -> dados_curso_ano
  
  if (convocado){
    return(dados_curso_ano[which (mod_con!="NA")])
  } # caso convocado == TRUE
  
  if (!convocado){
    return(dados_curso_ano[which (mod_con=="NA")])
  } # caso convocado == FALSE
}

which (lista_cursos == "MEDICINA") # descobrir numero sabendo o nome do curso
lista_cursos[59] # confere se numero corresponde ao curso

seleciona_status(2014,59,convocado=T) # apenas convocados
seleciona_status(2013,59,convocado=F) # apenas não convocados

seleciona_status(2022,59,convocado=F)[which (mod_ins == "L05")] # apenas convocados ins na L05
seleciona_status(2022,59,convocado=T)[which (mod_ins == "L05")] # apenas NÃO convocados ins na L05

seleciona_status(2022,54,T)$ENEM %>% max() # dentre os convocados, qual maior nota
seleciona_status(2022,54,T)$ENEM %>% min() # dentre os convocados, qual menor nota

seleciona_status(2022,54,F)$Modalidade_Convocada # deve ser tudo NA, pois F = não convocado


#-------------------------------------------------------------------------------
# Cria função seleciona_mudaram

# Seleciona todos os inscritos que mudaram de modalidade, para um ano e curso
# Se cota_para_A0=TRUE, seleciona estudantes que foram da cota para A0
# Se cota_para_A0=FALSE, seleciona estudantes que foram da A0 para cota

seleciona_mudaram <- function (ano,n_curso,cota_para_A0=TRUE){
  na.omit(dados_ufv)[which
                     (Processo_Seletivo==sisu_anos[ano-2012])][which
                                                               (Curso==lista_cursos[n_curso])] -> dados_curso_ano
  if (cota_para_A0){return(
    dados_curso_ano[which
                    (mod_ins!="A0")][which
                                     (mod_con=="A0")])}
  if (!cota_para_A0){return(
    dados_curso_ano[which
                    (mod_ins=="A0")][which
                                     (mod_con!="A0")])}
}

seleciona_mudaram(ano=2022,n_curso=1,cota_para_A0 = T)
seleciona_mudaram(ano=2022,n_curso=2,cota_para_A0 = F)

#===============================================================================
# Calcular diferença entre menor A0 convocado e maior cotista suplente
# Se o número for negativo, isso indica que tem cotista bom ficando de fora

# Problema: alguns valores Inf ou -Inf

# Cria df_diferenca_nota 
# para calcular diferença entre menor A0 convocado e maior cotista suplente
df_diferenca_nota <- data.frame(
  matrix(NA,nrow = length(lista_cursos_estavel), ncol = length(anos)))

# Preenche nomes de colunas e linhas
colnames(df_diferenca_nota) <- sisu_anos
rownames(df_diferenca_nota) <- lista_cursos_estavel

#-------------------------------------------------------------------------------
# Prepara loop para df_diferenca_nota
n_curso=1
for (j in 1:length(lista_cursos_estavel)){
ano=2013
for (i in 1:length(sisu_anos)){
  
# Ano 2022, curso 1, convocado, mod_ins A0, menor nota no ENEM
dados_ufv[which (Processo_Seletivo == sisu_anos[i])][which
                (Curso == lista_cursos_estavel[j])][which
                (mod_ins=="A0")][which (mod_con !="NA")]$ENEM %>% min() ->  min_A0_con

# Ano 2022, curso 1, suplente, mod_ins cota, maior nota no ENEM
  dados_ufv[which (Processo_Seletivo == sisu_anos[i])][which
                  (Curso == lista_cursos_estavel[j])][which
                  (mod_ins!="A0")][which 
                  (mod_con =="NA")]$ENEM %>% max() -> max_cota_sup
  
(min_A0_con - max_cota_sup) -> df_diferenca_nota[j,i]

paste(lista_cursos_estavel[j],"no ano",anos[i],"teve max_cota_sup =", max_cota_sup,
      "e min_A0_con =",min_A0_con) %>% print()
    
} # fecha for loop i
} # fecha for loop j

print("df_diferenca_nota está preenchido")

#------------------------------------------------------------------------------
# Mitiga problema dos Inf e -Inf

# Armazena df original
# teste <- df_diferenca_nota

# Transforma Inf e -Inf em 0
# df_diferenca_nota[df_diferenca_nota == Inf] <- 0; df_diferenca_nota[df_diferenca_nota == -Inf] <- 0

# Transforma Inf e -Inf em 0
df_diferenca_nota[df_diferenca_nota == Inf] <- NA; df_diferenca_nota[df_diferenca_nota == -Inf] <- NA

# Descobrir n do curso
which(lista_cursos=="MATEMATICA") # 58
which(lista_cursos=="FISICA") # 44
which(lista_cursos=="MEDICINA") # 59

df_diferenca_nota %>% max() # 142.54. Física 2013
df_diferenca_nota %>% min() # -209.82. Matemática 2017.


seleciona_todos(ano=2013,n_curso=44) %>% View()

seleciona_todos(ano=2013,n_curso=44)[which (mod_ins=="A0")]$ENEM %>% max()

# Física 2013. 
# min_A0_con - max_cota_sup = 142.54
#   579.28   -    436.74    = 142.54
# Cotista que ficou de fora tem nota baixa. Tá ok.
seleciona_status(ano=2013,n_curso=44,convocado=T)[which (mod_ins=="A0")]$ENEM %>% min()# -
seleciona_status(ano=2013,n_curso=44,convocado=F)[which (mod_ins!="A0")]$ENEM %>% max()

# Matemática 2017.
# min_A0_con - max_cota_sup = -209.82
#    433.2   -    643.02    = -209.82
# cotista que ficou de fora tem nota alta. Ruim.
lista_cursos[58]
seleciona_todos(ano=2017,n_curso=58)
seleciona_status(ano=2017,n_curso=58,convocado=T)[which (mod_ins=="A0")]$ENEM %>% min()# -
seleciona_status(ano=2017,n_curso=58,convocado=F) %>% View() #[which (mod_ins!="A0")]$ENEM %>% max()

seleciona_todos(ano=2017,n_curso=58)[which (ENEM==643.02)] # Número de Chamada = 0
seleciona_todos(ano=2017,n_curso=58)[which (ENEM==433.2)] # Número de Chamada = 3

# Medicina 2017
lista_cursos[59]
seleciona_status(ano=2017,n_curso=59,convocado=T)[which (mod_ins=="A0")]$ENEM %>% min()# -
seleciona_status(ano=2017,n_curso=59,convocado=F)[which (mod_ins!="A0")]$ENEM %>% max()
seleciona_todos(ano=2017,n_curso=59)[which (ENEM == 788.84)] # Numero de Chamada = 8
seleciona_todos(ano=2017,n_curso=59)[which (ENEM == 776.34)] # Numero de Chamada = 0

#-------------------------------------------------------------------------------
# Estudando Numero de Chamada = 0 ou NA

dados_ufv[which (Numero_Chamada_Convocacao == 0)][which (mod_con=="NA")]
# 44k linhas. Chamada 0 => não convocado.

dados_ufv[which (Numero_Chamada_Convocacao == 0)][which (!mod_con=="NA")]
# 0 linhas. Nenhum Chamada 0 foi convocado.

dados_ufv[which (is.na(Numero_Chamada_Convocacao))]
# 51k linhas. Muitos tem Chamada NA.

dados_ufv[which (is.na(Numero_Chamada_Convocacao))][which (mod_con=="NA")]
# 50k linhas. MUito tem Chamada NA e são não convocados.
# Até aí tudo bem

# Problema:
dados_ufv[which
          (is.na(Numero_Chamada_Convocacao))][which 
          (!mod_con=="NA")] # 547 linhas com Chamada NA foram convocados.

dados_ufv[which 
          (is.na(Numero_Chamada_Convocacao))][which 
          (!mod_con=="NA")] # %>% View() # ver

dados_ufv[which
          (is.na(Numero_Chamada_Convocacao))][which 
          (!mod_con=="NA")][which 
          (Processo_Seletivo=="SISU2021")][1] # exemplos individuais

dados_ufv[which
          (is.na(Numero_Chamada_Convocacao))][which 
           (!mod_con=="NA")][which 
            (Identificacao %in% c(54657,66835,5737,4145))] # um exemplo de cada ano

# 
df_diferenca_nota[1] %>% class()
df_diferenca_nota[1] %>% max()
df_diferenca_nota[1] %>% mean() # dá erro
(df_diferenca_nota[1] %>% sum()) / (df_diferenca_nota[1] %>% nrow())

# Análise em texto
for (i in 1:10){
paste(
  df_diferenca_nota[i] %>% sum() / (df_diferenca_nota[i] %>% nrow()),
  "é a média das diferenças no",
sisu_anos[i]) %>% print()}
# Há números positivos em 2013:2015, 2019, 2021:2022 = bom
# Há número negativos em 2016:2018 e 2020 = ruim

#===============================================================================
# Analisando caso individual

# Ciências Sociais em 2019
which (lista_cursos == "CIENCIAS_SOCIAIS") # descobrir numero sabendo o nome do curso

# Ano 2019, curso 20, convocado, mod_ins A0, menor nota no ENEM
seleciona_status(ano=2019,n_curso=20,convocado=T)[which (mod_ins == "A0")]$ENEM %>% min()
# > 606.26

# Ano 2019, curso 20, não convocado, mod_ins cota, maior nota no ENEM
seleciona_status(ano=2019,n_curso=20,convocado=F)[which (mod_ins != "A0")]$ENEM %>% max()
# > 606.34

# Algo está errado. O cotista com nota 606.34 deveria ter sido convocado pela A0.

# Quem tirou 606.34?
seleciona_todos(ano=2019,n_curso=20)[which (ENEM == 606.34)]
# > mod.ins == L05, mod_con == NA

# Quem tirou 606.34?
seleciona_todos(ano=2019,n_curso=20)[which (ENEM == 606.26)]
# > mod.ins == A0, mod_con == A0


seleciona_mudaram(ano=2019,n_curso=20,cota_para_A0=TRUE) # dois casos.
# > ENEM == 637.84 e 645.70
seleciona_mudaram(ano=2019,n_curso=20,cota_para_A0=FALSE) # vazio


seleciona_todos(ano=2019,n_curso=20) -> a

subset(a, "Identificacao">0,
       select=c("Identificacao","Processo_Seletivo","ENEM",
                                     "Modalidade_Inscrita",
                                     "Numero_Chamada_Convocacao",   
                                     "mod_ins","mod_con","Curso")) -> a
a

?subset

rbind(
dados_ufv[which (Numero_Chamada_Convocacao==0)],
dados_ufv[which (is.na(Numero_Chamada_Convocacao))]   )

# plot(dados_ufv$Numero_Chamada_Convocacao)


#===============================================================================
# Exportar
# 
# write.csv(a,
#           "C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas/privado/inscritos_2019_20_todos.csv",
#           row.names=FALSE)

lista_cursos
