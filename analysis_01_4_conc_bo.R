# ==============================================================================
# Arquivo: analysis_014_conc_bo.R
# Roda modelo c4, de Aygun e Bó (2021) e Bó e Senkevics (2023)

# Todos os candidatos podem preencher vagas da AC
# cotistas podem preencher quaisquer modalidade que eles tenham direito
# Gera análise de notas e de preenchimento de vagas ao final
# AC ao final

# Modificado em 2023-02-11.
# Autor: Mateus Silva Figueiredo

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
# # carregado a partir de data_04_carregar_dados_UFV.R
lista_todos <- get(paste0("dados_",cu)) %>% subset(Processo_Seletivo==edicao)

lista_todos %>% head()

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
nvagas <- c(30,5,8,5,8,1,1,1,1) # Pedagogia UFV

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

# Cria funções preenche_ A0 até L13

# Cada função preenche_ faz:
# cria aprovados_ A0 até L13
# preenche aprovados_ A0 até L13
# Atualiza lista_todos para remover os aprovados

# ------------------------------------------------------------------------------

# Criar funções preenche_ mod _ c4 (c4 = concorrencia segundo bo e senkevics)

{ # criar todas as funções preenche_ A0 até L13
print("criando funções preenche_ A0 até L13")

# ------------------------------------------------------------------------------

preenche_A0_c4<-function(){
  
# Cria aprovados_A0
aprovados_A0 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_A0))
colnames(aprovados_A0) <<- colnames(lista_todos)

# Preenche aprovados_A0
lista_todos %>% slice_head(n=nvagas_A0) ->> aprovados_A0 # ignora mod_ins
aprovados_A0$mod_con <<- "A0"

# Atualiza lista_todos removendo presentes em aprovados_A0
subset(lista_todos, !id %in% aprovados_A0$id) ->> lista_todos
}# fim da function

# ------------------------------------------------------------------------------

preenche_L01_c4<-function(){
  
# Cria aprovados_L01
aprovados_L01 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L01))
colnames(aprovados_L01) <<- colnames(lista_todos)

# Preenche aprovados_L01 # apenas pub e bxa
lista_todos %>% subset(pub) %>% subset(bxa) %>% slice_head(n=nvagas_L01) ->> aprovados_L01
aprovados_L01$mod_con <<- "L01"

# Atualiza lista_todos removendo presentes em aprovados_L01
subset(lista_todos, !id %in% aprovados_L01$id) ->> lista_todos
}# fim da function

# ------------------------------------------------------------------------------

preenche_L02_c4<-function(){
  
# Cria aprovados_L02
aprovados_L02 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L02))
colnames(aprovados_L02) <<- colnames(lista_todos)

# Preenche aprovados_L02# apenas pub bxa ppi
lista_todos %>% subset(pub) %>% subset(bxa) %>% subset(ppi) %>%
  slice_head(n=nvagas_L02) ->> aprovados_L02
aprovados_L02$mod_con <<- "L02"

# Atualiza lista_todos removendo presentes em aprovados_L02
subset(lista_todos, !id %in% aprovados_L02$id) ->> lista_todos
}# fim da function

# ------------------------------------------------------------------------------

preenche_L05_c4<-function(){
  
# Cria aprovados_L05
aprovados_L05 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L05))
colnames(aprovados_L05) <<- colnames(lista_todos)

# Preenche aprovados_L05# apenas pub
lista_todos %>% subset(pub) %>% 
  slice_head(n=nvagas_L05) ->> aprovados_L05
aprovados_L05$mod_con <<- "L05"

# Atualiza lista_todos removendo presentes em aprovados_L02
subset(lista_todos, !id %in% aprovados_L05$id) ->> lista_todos
}# fim da function

# ------------------------------------------------------------------------------

preenche_L06_c4<-function(){
  
# Cria aprovados_L06
aprovados_L06 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L06))
colnames(aprovados_L06) <<- colnames(lista_todos)

# Preenche aprovados_L06# apenas pub ppi
lista_todos %>% subset(pub) %>% subset(ppi) %>%
  slice_head(n=nvagas_L06) ->> aprovados_L06
aprovados_L06$mod_con <<- "L06"

# Atualiza lista_todos removendo presentes em aprovados_L06
subset(lista_todos, !id %in% aprovados_L06$id) ->> lista_todos
}# fim da function

# ------------------------------------------------------------------------------

preenche_L09_c4<-function(){
  
# Cria aprovados_L09
aprovados_L09 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L09))
colnames(aprovados_L09) <<- colnames(lista_todos)

# Preenche aprovados_L09 # apenas pub bxa pcd
lista_todos %>% subset(pub) %>% subset(bxa) %>% subset(pcd) %>%
  slice_head(n=nvagas_L09) ->> aprovados_L09
aprovados_L09$mod_con <<- "L09"

# Atualiza lista_todos removendo presentes em aprovados_L09
subset(lista_todos, !id %in% aprovados_L09$id) ->> lista_todos
}# fim da function

# ------------------------------------------------------------------------------

preenche_L10_c4<-function(){
  
# Cria aprovados_L10
aprovados_L10 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L10))
colnames(aprovados_L10) <<- colnames(lista_todos)

# Preenche aprovados_L10 # apenas pub bxa ppi pcd
lista_todos %>% subset(pub) %>% subset(bxa) %>% subset(ppi) %>% subset(pcd) %>%
  slice_head(n=nvagas_L10) ->> aprovados_L10
aprovados_L10$mod_con <<- "L10"

# Atualiza lista_todos removendo presentes em aprovados_L10
subset(lista_todos, !id %in% aprovados_L10$id) ->> lista_todos
}# fim da function

# ------------------------------------------------------------------------------

preenche_L13_c4<-function(){
  
# Cria aprovados_L13
aprovados_L13 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L13))
colnames(aprovados_L13) <<- colnames(lista_todos)

# Preenche aprovados_L13 # apenas pub  pcd
lista_todos %>% subset(pub) %>%  subset(pcd) %>%
  slice_head(n=nvagas_L13) ->> aprovados_L13
aprovados_L13$mod_con <<- "L13"

# Atualiza lista_todos removendo presentes em aprovados_L10
subset(lista_todos, !id %in% aprovados_L13$id) ->> lista_todos
}# fim da function

# ------------------------------------------------------------------------------

preenche_L14_c4<-function(){
  
# Cria aprovados_L14
aprovados_L14 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L14))
colnames(aprovados_L14) <<- colnames(lista_todos)

# Preenche aprovados_L14 # apenas pub ppi pcd
lista_todos %>% subset(pub) %>% subset(ppi) %>% subset(pcd) %>%
  slice_head(n=nvagas_L14) ->> aprovados_L14
aprovados_L14$mod_con <<- "L14"

# Atualiza lista_todos removendo presentes em aprovados_L10
subset(lista_todos, !id %in% aprovados_L14$id) ->> lista_todos
}# fim da function

# ------------------------------------------------------------------------------

} # fim da criação de todas as funções preenche_ A0 até L13

# ==============================================================================
# Criar aprovados

# # remove objetos aprovados_, caso já existam
 rm(list=ls(pattern="^aprovados_"))

# Cria lista_todos
# candidatos -> lista_todos
# trabalhar apenas com lista_todos. Manter candidatos inalterado.
# pode ir removendo de lista_todos aqueles que forem aprovados.

# Rodar funções preenche_
# IMPORTANTE! lista_todos deve estar inteira antes de rodar
# if(nrow(lista_todos)==n) confere se lista_todos está inteira

# IMPORTANTE! Rodar apenas uma vez cada uma.
# if(isFALSE(exists("aprovados_"))) = só roda se não existir objeto aprovados_

# Existem 9! ordens possíveis para fazer os preenchimentos (mais de 300 mil).

# ordem proposta por bo e senkevics, 2023 (L10, L02, L09, L1, L14, L06, L13, L05, A0)
{
  if(isFALSE(exists("aprovados_L10"))) {preenche_L10_c4()}
  if(isFALSE(exists("aprovados_L02"))) {preenche_L02_c4()}
  if(isFALSE(exists("aprovados_L09"))) {preenche_L09_c4()}
  if(isFALSE(exists("aprovados_L01"))) {preenche_L01_c4()}
  if(isFALSE(exists("aprovados_L14"))) {preenche_L14_c4()}
  if(isFALSE(exists("aprovados_L06"))) {preenche_L06_c4()}
  if(isFALSE(exists("aprovados_L13"))) {preenche_L13_c4()}
  if(isFALSE(exists("aprovados_L05"))) {preenche_L05_c4()}
  if(isFALSE(exists("aprovados_A0")))  {preenche_A0_c4()} # AC por último
  
} # todas as funções preenche foram rodadas

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
aprovados_c4 <- aprovados # concorrencia c4 de bo = sufixo 4

# Cria data.frame analise_v_c4 para concorrencia c4 (bo) para as vagas
analise_v_c4<-data.frame(matrix(ncol = length(mod), nrow = 1),
                       row.names=c("n_c4"))
colnames(analise_v_c4) <- c(mod)

# preenche as 9 colunas com numero de convocados
{eval(parse(text=(paste(
  'analise_v_c4[,"',mod,'"]<-aprovados %>% filter (mod_ins=="',mod,'") %>% nrow()',                   
  sep=""))))}

# total de vagas preenchidas por grupo social
analise_v_c4$tot <- sum(analise_v_c4[1:9]) # soma de todas as mod
analise_v_c4$pub <- sum(analise_v_c4[2:9]) # soma das mod de cotas
analise_v_c4$bxa <- sum(analise_v_c4$L01, analise_v_c4$L02, # soma de baixa renda
                        analise_v_c4$L09, analise_v_c4$L10)
analise_v_c4$ppi <- sum(analise_v_c4$L02, analise_v_c4$L06, # soma das cotas para ppi
                        analise_v_c4$L10, analise_v_c4$L14)
analise_v_c4$pcd <- sum(analise_v_c4$L09, analise_v_c4$L10, # soma das cotas para pcd
                        analise_v_c4$L13, analise_v_c4$L14)

# ------------------------------------------------------------------------------

# Análise - notas

# Cria data.frame analise_n_c4 para concorrencia c4 (bo e senkevics) para as notas
analise_n_c4<-data.frame(matrix(ncol = length(mod)+1, nrow = 3),
                       row.names=c("max","mean","min"))
colnames(analise_n_c4) <- c("geral",mod)

# preenche as 9 colunas com nota maxima, media e minima
{eval(parse(text=(paste(
  'analise_n_c4[,"',mod,'"]<-c(max(aprovados_',mod,'$nota),
                        mean(aprovados_',mod,'$nota),
                        min(aprovados_',mod,'$nota))',                   
  sep=""))))}

analise_n_c4$geral<-c(max(aprovados$nota),
                    mean(aprovados$nota),
                    min(aprovados$nota))
rownames(analise_n_c4)<-c('max_c4','mean_c4','min_c4')

analise_n_c4$cotas<-c(
  subset(aprovados_c4,aprovados_c4$mod_con!="A0")$nota%>%max(),
  subset(aprovados_c4,aprovados_c4$mod_con!="A0")$nota%>%mean(),
  subset(aprovados_c4,aprovados_c4$mod_con!="A0")$nota%>%min()
) # cria coluna de todos os cotistas (convocados em modalidade de cotas)

# } #fim do loop (?)

# ==============================================================================
# Guarda aprovados_c4
convocados_c4 <- aprovados_c4

# ==============================================================================

# Limpeza

rm(list=ls(pattern="^aprovados")) # objetos que começam com "aprovados"
rm(list=ls(pattern="^preenche")) # objetos que começam com "preenche"
# rm(lista_todos)

# manter analise e candidatos. manter aprovados_c4
