# ==============================================================================
# Arquivo: analysis_01_3_conc_c_cotas.R # escrevendo
# Roda modelo c3, de concorrência concomitante, cotas primeiro
#
# Todos os candidatos podem preencher vagas da AC
# cotistas só podem preencher vaga da cota que eles se inscreveram
# Gera análise de notas e de preenchimento de vagas ao final
#
# Modificado em 2024-01-03
# Autor: Mateus Silva Figueiredo
#
# diff 2024-01-03: cria preenche_A0 por ultimo. Não deve fazer diferença.
# Mudança em documentação.
# Mudança grande: funções preenche_ agora têm arrange(desc(nota))

# ==============================================================================
# Preparação
# Deve ser feito pelo analysis_01_todas_conc, usando source()

# Escolher um curso
# cu <- "MEDICINA" # pelo nome
# cu <- lista_cursos[59]; # pelo número na lista

# Escolher uma edição do SISU
# edicao <- "SISU2022" # pelo nome

# ==============================================================================
#
# Carregar lista_todos

# lista_todos # lista de todos os candidatos para determinado curso e ano

# opção 1: dados simulados
# lista_todos <- candidatos # gerado por data_02_cria_candidatos_por_mod.R

# # opção 2: dados observados
# # carregado a partir de candidatos gerado por analysis_01
lista_todos <- candidatos

# ------------------------------------------------------------------------------

# quantos candidatos tem? print texto
paste(lista_todos %>% nrow(), "candidatos em",
      lista_todos$Curso[1], "no", lista_todos$Processo_Seletivo[1])

# ==============================================================================
# é preciso existir nvagas

# ordem: A0, L01, L02, L05, L06, L09, L10, L13, L14

# opção 1: usar função gera_nvagas. input = ppi, pcd, tot. tem ppi e pcd default.
# gera_nvagas (0.5366,0.0843,tot) # mg = (0.5366,0.0843,tot). output = nvagas

# opção 2: vagas arbitrárias. Preferencialmente copiando de termo de adesão
# nvagas <- c(25,5,6,4,6,1,1,1,1) # Medicina UFV
# nvagas <- c(30,5,8,5,8,1,1,1,1) # Pedagogia UFV

nvagas %>% length == 9

# é preciso existir nvagas_A0 até nvagas_L14
{nvagas[1]->nvagas_A0
  nvagas[2]->nvagas_L01
  nvagas[3]->nvagas_L02
  nvagas[4]->nvagas_L05
  nvagas[5]->nvagas_L06
  nvagas[6]->nvagas_L09
  nvagas[7]->nvagas_L10
  nvagas[8]->nvagas_L13
  nvagas[9]->nvagas_L14}

# ==============================================================================

# Criar funções preenche_ mod _ c3 (c3 = concorrencia conc, cotas primeiro, modelo c3)

# ------------------------------------------------------------------------------
{ # começa a criar todas as funcion preenche mod c3
  
  preenche_L01_c3<-function(){
    
    # Cria aprovados_L01
    aprovados_L01 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L01))
    colnames(aprovados_L01) <<- colnames(lista_todos)
    
    # Reorder lista_todos based on nota column
    lista_todos <<- lista_todos %>% arrange(desc(nota))
    
    # Preenche aprovados_L01
    lista_todos %>% subset(mod_ins=="L01") %>%slice_head(n=nvagas_L01) ->> aprovados_L01
    aprovados_L01$mod_con <<- "L01"
    
    # Atualiza lista_todos removendo presentes em aprovados_L01
    subset(lista_todos, !id %in% aprovados_L01$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L02_c3<-function(){
    
    # Cria aprovados_L02
    aprovados_L02 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L02))
    colnames(aprovados_L02) <<- colnames(lista_todos)
    
    # Reorder lista_todos based on nota column
    lista_todos <<- lista_todos %>% arrange(desc(nota))
    
    # Preenche aprovados_L02
    lista_todos %>% subset(mod_ins=="L02") %>% slice_head(n=nvagas_L02) ->> aprovados_L02
    aprovados_L02$mod_con <<- "L02"
    
    # Atualiza lista_todos removendo presentes em aprovados_L02
    subset(lista_todos, !id %in% aprovados_L02$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L05_c3<-function(){
    
    # Cria aprovados_L05
    aprovados_L05 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L05))
    colnames(aprovados_L05) <<- colnames(lista_todos)
    
    # Reorder lista_todos based on nota column
    lista_todos <<- lista_todos %>% arrange(desc(nota))
    
    # Preenche aprovados_L05
    lista_todos %>% subset(mod_ins=="L05") %>%slice_head(n=nvagas_L05) ->> aprovados_L05
    aprovados_L05$mod_con <<- "L05"
    
    # Atualiza lista_todos removendo presentes em aprovados_L05
    subset(lista_todos, !id %in% aprovados_L05$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L06_c3<-function(){
    
    # Cria aprovados_L06
    aprovados_L06 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L06))
    colnames(aprovados_L06) <<- colnames(lista_todos)
    
    # Reorder lista_todos based on nota column
    lista_todos <<- lista_todos %>% arrange(desc(nota))
    
    # Preenche aprovados_L06
    lista_todos %>% subset(mod_ins=="L06") %>%slice_head(n=nvagas_L06) ->> aprovados_L06
    aprovados_L06$mod_con <<- "L06"
    
    # Atualiza lista_todos removendo presentes em aprovados_L06
    subset(lista_todos, !id %in% aprovados_L06$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L09_c3<-function(){
    
    # Cria aprovados_L09
    aprovados_L09 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L09))
    colnames(aprovados_L09) <<- colnames(lista_todos)
    
    # Reorder lista_todos based on nota column
    lista_todos <<- lista_todos %>% arrange(desc(nota))
    
    # Preenche aprovados_L09
    lista_todos %>% subset(mod_ins=="L09") %>%slice_head(n=nvagas_L09) ->> aprovados_L09
    aprovados_L09$mod_con <<- "L09"
    
    # Atualiza lista_todos removendo presentes em aprovados_L09
    subset(lista_todos, !id %in% aprovados_L09$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L10_c3<-function(){
    
    # Cria aprovados_L10
    aprovados_L10 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L10))
    colnames(aprovados_L10) <<- colnames(lista_todos)
    
    # Reorder lista_todos based on nota column
    lista_todos <<- lista_todos %>% arrange(desc(nota))
    
    # Preenche aprovados_L10
    lista_todos %>% subset(mod_ins=="L10") %>%slice_head(n=nvagas_L10) ->> aprovados_L10
    aprovados_L10$mod_con <<- "L10"
    
    # Atualiza lista_todos removendo presentes em aprovados_L10
    subset(lista_todos, !id %in% aprovados_L10$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L13_c3<-function(){
    
    # Cria aprovados_L13
    aprovados_L13 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L13))
    colnames(aprovados_L13) <<- colnames(lista_todos)  
    
    # Reorder lista_todos based on nota column
    lista_todos <<- lista_todos %>% arrange(desc(nota))
    
    # Preenche aprovados_L13
    lista_todos %>% subset(mod_ins=="L13") %>%slice_head(n=nvagas_L13) ->> aprovados_L13
    aprovados_L13$mod_con <<- "L13"
    
    # Atualiza lista_todos removendo presentes em aprovados_L13
    subset(lista_todos, !id %in% aprovados_L13$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L14_c3<-function(){
    
    # Cria aprovados_L14
    aprovados_L14 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L14))
    colnames(aprovados_L14) <<- colnames(lista_todos)
    
    # Reorder lista_todos based on nota column
    lista_todos <<- lista_todos %>% arrange(desc(nota))
    
    # Preenche aprovados_L14
    lista_todos %>% subset(mod_ins=="L14") %>%slice_head(n=nvagas_L14) ->> aprovados_L14
    aprovados_L14$mod_con <<- "L14"
    
    # Atualiza lista_todos removendo presentes em aprovados_L14
    subset(lista_todos, !id %in% aprovados_L14$id) ->> lista_todos
    
    
  }# fim da function
  # ----------------------------------------------------------------------------
  preenche_A0_c3<-function(){
    
    # Cria aprovados_A0
    aprovados_A0 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_A0))
    colnames(aprovados_A0) <<- colnames(lista_todos)
    
    # Reorder lista_todos based on nota column
    lista_todos <<- lista_todos %>% arrange(desc(nota))
    
    # Preenche aprovados_A0
    lista_todos %>% slice_head(n=nvagas_A0) ->> aprovados_A0
    aprovados_A0$mod_con <<- "A0"
    
    # Atualiza lista_todos removendo presentes em aprovados_A0
    subset(lista_todos, !id %in% aprovados_A0$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
} # fim de criar todas as function preenche_ mod _c3

# ==============================================================================
# remove objetos aprovados_, caso já existam
rm(list=ls(pattern="^aprovados_"))

# Rodar funções preenche_ mod _ c3 (c3 = concorrencia conc cotas primeiro, modelo c3)

if(isFALSE(exists("aprovados_L01"))) {preenche_L01_c3()} # cotas primeiro
if(isFALSE(exists("aprovados_L02"))) {preenche_L02_c3()}
if(isFALSE(exists("aprovados_L05"))) {preenche_L05_c3()}
if(isFALSE(exists("aprovados_L06"))) {preenche_L06_c3()}
if(isFALSE(exists("aprovados_L09"))) {preenche_L09_c3()}
if(isFALSE(exists("aprovados_L10"))) {preenche_L10_c3()}
if(isFALSE(exists("aprovados_L13"))) {preenche_L13_c3()}
if(isFALSE(exists("aprovados_L14"))) {preenche_L14_c3()}

if(isFALSE(exists("aprovados_A0")))  {preenche_A0_c3()} #AC depois

# cria objeto com todos os aprovados
aprovados <- do.call("rbind", list(aprovados_A0,
                                   aprovados_L01,aprovados_L02,
                                   aprovados_L05,aprovados_L06,
                                   aprovados_L09,aprovados_L10,
                                   aprovados_L13,aprovados_L14))

# ==============================================================================
# Análises

# ------------------------------------------------------------------------------

# Analise - inscritos aprovados
aprovados_c3 <- aprovados # concorrencia separada = sufixo 2

# Cria data.frame analise_v1 para concorrencia 1 (separada) para as vagas
analise_v_c3<-data.frame(matrix(ncol = length(mod), nrow = 1),
                         row.names=c("n_c3"))
colnames(analise_v_c3) <- c(mod)

# preenche as 9 colunas com numero de convocados
{eval(parse(text=(paste(
  'analise_v_c3[,"',mod,'"]<-aprovados %>% filter (mod_ins=="',mod,'") %>% nrow()',                   
  sep=""))))}

# total de vagas preenchidas por grupo social
analise_v_c3$tot <- sum(analise_v_c3[1:9]) # soma de todas as mod
analise_v_c3$pub <- sum(analise_v_c3[2:9]) # soma das mod de cotas
analise_v_c3$bxa <- sum(analise_v_c3$L01,analise_v_c3$L02, # soma de baixa renda
                        analise_v_c3$L09,analise_v_c3$L10)
analise_v_c3$ppi <- sum(analise_v_c3$L02,analise_v_c3$L06, # soma das cotas para ppi
                        analise_v_c3$L10,analise_v_c3$L14)
analise_v_c3$pcd <- sum(analise_v_c3$L09,analise_v_c3$L10, # soma das cotas para pcd
                        analise_v_c3$L13,analise_v_c3$L14)

# ------------------------------------------------------------------------------

# Análise - notas

# Cria data.frame analise_n_c3 para concorrencia 3 (conc conc, cotas primeiro) para as notas
analise_n_c3<-data.frame(matrix(ncol = length(mod)+1, nrow = 3),
                         row.names=c("max","mean","min"))
colnames(analise_n_c3) <- c("geral",mod)

# preenche as 9 colunas com nota maxima, media e minima
{eval(parse(text=(paste(
  'analise_n_c3[,"',mod,'"]<-c(max(aprovados_',mod,'$nota),
                        mean(aprovados_',mod,'$nota),
                        min(aprovados_',mod,'$nota))',                   
  sep=""))))}

analise_n_c3$geral<-c(max(aprovados$nota),
                      mean(aprovados$nota),
                      min(aprovados$nota))
rownames(analise_n_c3)<-c('max_c3','mean_c3','min_c3')

analise_n_c3$cotas<-c(
  subset(aprovados_c3,aprovados_c3$mod_con!="A0")$nota%>%max(),
  subset(aprovados_c3,aprovados_c3$mod_con!="A0")$nota%>%mean(),
  subset(aprovados_c3,aprovados_c3$mod_con!="A0")$nota%>%min()
) # cria coluna de todos os cotistas (convocados em modalidade de cotas)

# } #fim do loop (?)

# ==============================================================================
# Guarda aprovados_c3
convocados_c3 <- aprovados_c3

# ==============================================================================

# Limpeza

rm(list=ls(pattern="^aprovados")) # objetos que começam com "aprovados"
rm(list=ls(pattern="^preenche")) # objetos que começam com "preenche"
# rm(lista_todos)

# manter analise e candidatos. manter aprovados_c3
