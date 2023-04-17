# ==============================================================================
# Arquivo: analysis_04_numero_inscritos_por_ano.R

#
# Modificado em: 2022-12-10
# Autor: Mateus Silva Figueiredo
# ==============================================================================
#
# Usar após data_04_carregar_dados_UFV.R
# Calcula numero de inscritos por ano
#
# Dicionário
#
# Inputs:
# dados_ufv com todos os cursos de todos os anos 2013-2022
# lista_cursos_estavel 
#
# Outputs:
# n_inscritos_total, com o numero de inscritos por curso por ano por modalidade
# ==============================================================================
# Preparação
belch2 <- function(x, y) { eval(parse(text=(paste0(x, y, sep=""))))}
# belch3 <- function(x, y, z) {eval(parse(text=(paste0(x, y, z,sep=""))))}

mod <- c("A0","L01","L02","L05","L06", "L09", "L10", "L13", "L14")
mod_cotas <- mod[mod!= "A0"] # cria mod_cotas sem A0


# ==============================================================================
# Número de inscritos por curso a cada ano

dados_ufv$Processo_Seletivo %>% unique() -> sisu_anos; sisu_anos # SISU2013 até SISU2022
lista_cursos_estavel # todos os cursos presentes nos 10 anos de dados. length = 64
mod # modalidades de A0 a L01 até L14
insc_mod  <- paste0("insc_",mod)   # para usar para preencher df
mod_cotas # modalidades de L01 até L14

# define e == qual ano, m == qual modalidade
e<-2013-2012; sisu_anos[e] # edição do sisu. alterar primeiro ano desta linha
m<-1; mod[m] # qual modalidade

# cria colunas para inscritos_por_curso
{colunas<-c(
  "curso",
  insc_mod
)} # "ano" vai ser criado depois, com rownames_to_column()
# inscritos_cotas" vai ser criado depois, com mutate

# ------------------------------------------------------------------------------
# prepara para o for loop cu
# define i == qual curso
i<-1; lista_cursos_estavel[i] # qual curso da lista
cu<-1 # permite que o for loop cu rode

# abre for loop cu, para cada curso
for (i in 1:length(lista_cursos_estavel)){
  # cria dados_curso
  dados_curso <- dados_ufv[which(Curso == lista_cursos_estavel[i])]
  
  
  # ------------------------------------------------------------------------------
  # cria data.frame inscritos_por_curso cheio de NA
  {
    inscritos_por_curso <- data.frame(matrix(ncol = length(colunas), nrow = length(sisu_anos)))
    colnames(inscritos_por_curso) <- colunas
    rownames(inscritos_por_curso) <- sisu_anos
    inscritos_por_curso # confere. deve ter 22 colunas com nome, e 10 linhas com NA
  } # criou e viu inscritos_por_curso cheio de NA
  
  # ------------------------------------------------------------------------------
  # preenche curso
  inscritos_por_curso$curso<-lista_cursos_estavel[i]
  
  # cria coluna ano
  rownames(inscritos_por_curso) -> inscritos_por_curso$ano
  
  # ------------------------------------------------------------------------------
  # Usa dois for loop para preencher inscritos_por_curso com inscritos por ano
  e <- 1
  for (b in 1:length(sisu_anos)){
    m <- 1
    for (a in 1:length(insc_mod)){
      (
        dados_curso[which
                    (Processo_Seletivo == sisu_anos[e])][which # subset ano
                                                         (mod_ins == mod[m])] # subset modalidade
        # não subset por numero de chamada, para pegar todos os inscritos
      ) %>% nrow() -> inscritos_por_curso[sisu_anos[e],insc_mod[m]]
      m <- m+1
    } # fecha loop a que preenche vagas de mods modalidades = colunas
    e <- e+1
  } # fecha loop b que pula pra próxima linha = próximo ano
  # fim dos dois for loop
  
  # confere
  inscritos_por_curso
  
  # ------------------------------------------------------------------------------
  # preenche inscritos_cotas somando insc_L01 até insc_L14
  
  e<-1
  for (a in 1:10){
    (inscritos_por_curso[sisu_anos[e],insc_mod[2:9]]) %>%
      sum() -> inscritos_por_curso$insc_cotas[e]
    # inscritos_por_curso$insc_cotas # conferência
    e <- e+1} # fim do loop a para preencher vagas_cotas
  
  
  # confere
  inscritos_por_curso
  
  # ------------------------------------------------------------------------------
  # gera data.frame inscritos_CURSO
  belch2("inscritos_por_curso ->> inscritos_", inscritos_por_curso$curso[1])
  
  cu<-cu+1
}
# fecha for loop cu
# agora, existem objetos inscritos_ADMINISTRACAO até inscritos_ZOOTECNIA

# cria lista de todos os data.frames que foram criados
lista_objetos <- paste0("inscritos_",lista_cursos_estavel)

# une todos os data.frames em um só, usando um for loop
n_inscritos_total <- eval(parse(text=lista_objetos[1]))
for (i in 2:length(lista_cursos_estavel)){
  rbind(n_inscritos_total,eval(parse(text=lista_objetos[i])))->n_inscritos_total
}

# remover rownames, pois são desnecessarios agora
rownames(n_inscritos_total) <- NULL

# ==============================================================================

# Conferência com dados_ufv - Apenas cursos estáveis nos 10 anos
sum(n_inscritos_total$insc_A0) ==
  dados_ufv[which(mod_ins=="A0")][which(Curso %in% lista_cursos_estavel)] %>% nrow()
# deve ser TRUE

sum(n_inscritos_total$insc_cotas) ==
  dados_ufv[which(mod_ins!="A0")][which(Curso %in% lista_cursos_estavel)] %>% nrow()
# deve ser TRUE

# ==============================================================================
# dados de SISU2018, primeiro ano com cota para PCD

dados_ufv[which(Processo_Seletivo=="SISU2018")]
subset(n_inscritos_total,ano=="SISU2018") -> n_inscritos_2018

# dados do SISU2018 em diante
subset(n_inscritos_total,ano %in% c("SISU2018","SISU2019","SISU2020",
                                    "SISU2021","SISU2022")) -> n_inscritos_2018_2022

paste("Houve",n_inscritos_2018$insc_L09[54],"inscritos para",
       n_inscritos_2018$curso[54],"na modalidade L09 no SISU2018") #125

# ==============================================================================
# Análise
# Cria data.frame número de inscritos por ano

inscritos_por_ano <- data.frame(ano=sisu_anos,
                                n_inscritos=0)

for (e in 1:nrow(inscritos_por_ano)){  
dados_ufv[which(Processo_Seletivo==inscritos_por_ano$ano[e])] %>% nrow() ->
  inscritos_por_ano$n_inscritos[e]
}

# numero de inscritos A0
for (e in 1:nrow(inscritos_por_ano)){  
  dados_ufv[which
            (mod_ins=="A0")][which
            (Processo_Seletivo==inscritos_por_ano$ano[e])] %>% nrow() ->
    inscritos_por_ano$n_inscritos_A0[e]
}

# numero de inscritos cotas
for (e in 1:nrow(inscritos_por_ano)){  
  dados_ufv[which
            (mod_ins!="A0")][which
                             (Processo_Seletivo==inscritos_por_ano$ano[e])] %>% nrow() ->
    inscritos_por_ano$n_inscritos_cotas[e]
}

# confere. deve ser tudo TRUE
inscritos_por_ano$n_inscritos==
  inscritos_por_ano$n_inscritos_A0+inscritos_por_ano$n_inscritos_cotas

# ==============================================================================
# Limpeza
rm(inscritos_por_curso)

# Para apagar todos os objetos inscritos_ADMINISTRACAO até inscritos_ZOOTECNIA:
# for (i in 1:length(lista_cursos_estavel)) {
# eval(parse(text=(paste0("rm(inscritos_",
#                         lista_cursos_estavel[i],
#                         ")"))))}
