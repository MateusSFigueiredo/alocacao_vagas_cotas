# ==============================================================================
# Arquivo: analysis_02_textos.R
# Analisa as três modalidades de concorrência. Gera textos curtos.
#
# Função compara_vagas:
# analisa alocação de vagas do mesmo grupo social em modelos diferentes
#
# Função compara_notas_est:
# compara estatísticas diferentes no mesmo modelo e mesma categoria

# Modificado em 2022-11-13.
# Autor: Mateus Silva Figueiredo

# ==============================================================================
#
# Dicionário
#
# meta_n = compilado das notas de ingresso pelas três modalidades de concorrencia
# meta_v = compilado das vagas alocadas pelas três modalidades de concorrencia
#
#
# sufixo 1 = Separa candidatos seguindo concorrência separada
# sufixo 2 = Separa candidatos seguindo concorrência concomitante, AC primeiro
# sufixo 3 = Separa candidatos seguindo concorrência concomitante, cotas primeiro
#
# Inputs:
#
# Outputs:
#
#
# ==============================================================================
# Carregar função e biblioteca necessária
library(tidyverse)

belch4<-function(x, y, z, a) 
{eval(parse(text=(paste(x, y, z, a,sep=""))))}

belch6<-function(x, y, z, a, b, d) 
{eval(parse(text=(paste(x, y, z, a, b, d, sep=""))))}

# ==============================================================================
#
# Tabela para converter código em palavra e gerar textos bonitos
nomes <- as.data.frame(tribble(
  ~codigo,~nome,
  "min","mínima",
  "max","máxima",
  "mean","média",
  "bxa","de baixa renda",
  "ppi","pretos, pardos ou indígenas",
  "pcd","pessoas com deficiência",
  "tot","no total",
  "pub","egressos de escola pública",
  "geral","em geral",
  "A0", "na modalidade A0",
  "L1", "na modalidade L1",
  "L2", "na modalidade L2",
  "L5", "na modalidade L5",
  "L6", "na modalidade L6",
  "L9", "na modalidade L9",
  "L10","na modalidade L10",
  "L11","na modalidade L11",
  "L13","na modalidade L13",
  "L14","na modalidade L14",
  "cotas","nas modalidades de cotas"
)); nomes <- column_to_rownames(nomes,var="codigo")

# ==============================================================================

# Gerar funções
#
# ------------------------------------------------------------------------------
# Função compara_vagas:
# analisar alocação de vagas do mesmo grupo social em modelos diferentes
# Função chama data.frame nomes para escrever a frase bonitinha.

compara_vagas<-function(mod.a,mod.b,dimensao)
{
  valor.a<-meta_v[mod.a,dimensao];
  valor.b<-meta_v[mod.b,dimensao];
  paste(
    "Com o modelo",
    mod.a,
    "entraram",
    valor.a,
    "estudantes",
    nomes[dimensao,],
    ". Este número é",
    ((valor.a/valor.b)*100-100)%>%abs()%>%round(),
    ifelse(valor.a<=valor.b,"% menor","% maior"),
    "do que os",
    valor.b,
    "estudantes",
    nomes[dimensao,],
    "aprovados pelo modelo",
    mod.b)
}
compara_vagas(1,2,"bxa") # teste
# Fim da função compara_vagas

# Testa função compara_vagas
# Preencher com (um modelo, outro modelo, grupo)
# Modelos  1, 2, 3
# Grupos = tot, ppi, pcd, pub, bxa
compara_vagas(1,2,"ppi")
compara_vagas(2,3,"pcd")
compara_vagas(1,3,"tot")
compara_vagas(2,3,"bxa")
compara_vagas(2,3,"pub")
compara_vagas(2,3,"A0")
compara_vagas(2,3,"L1")
compara_vagas(2,3,"pub")


# ------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------
# Função compara_notas_est: 
# compara estatísticas diferentes no mesmo modelo 

compara_notas_est<-function(mod.a,cate="geral",est.a="mean",est.b="min"){
  
  belch6('valor.a<<-meta_n["',
         est.a,
         mod.a,
         '","',
         cate,
         '"]%>%round(2)')
  
  belch6('valor.b<<-meta_n["',
         est.b,
         mod.a,
         '","',
         cate,
         '"]%>%round(2)')
  
  paste(
    "Com o modelo",
    mod.a,
    ", a nota",
    nomes[est.a,],
    "foi de",
    valor.a,
    "e a nota",
    nomes[est.b,],
    "foi de",
    valor.b,
    nomes[cate,])
} # fim da função compara_notas_est

# Testando função compara_notas_est
# Preencher com (modelo, categoria, uma estatística, outra estatística)
# Modelos  1, 2, 3
# Estatísticas = max, mean, min
# Categorias = A0, L1, L2, L5, L6, L9, L10, L13, L14
compara_notas_est(1,'geral', 'mean','min')
compara_notas_est(2,'geral')
compara_notas_est(3,'L2',est.a="min",est.b="mean")
compara_notas_est(1,cate="cotas")
compara_notas_est(1,cate="A0")

compara_notas_est(1);compara_notas_est(2);compara_notas_est(3)

for (i in 1:3){print(compara_notas_est(i,"A0"))} # parece funcionar ok
for (i in 1:3){print(compara_notas_est(i,"L2"))} # parece funcionar ok
for (i in 1:3){print(compara_notas_est(i,"cotas"))} # parece funcionar ok


# Cuidado ao mandar o programa fazer a função várias vezes com vetor.
# DÁ ERRO: # i <- 1:3; compara_notas_est(i,"A0")

# ------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------
# Função compara_notas_mod: 
# compara a mesma estatistica em modelos diferentes

compara_notas_mod<-function(mod.a,mod.b,cate="geral",est="mean"){
  
  belch6('valor.a<<-meta_n["',
         est,
         mod.a,
         '","',
         cate,
         '"]%>%round(2)')
  
  belch6('valor.b<<-meta_n["',
         est,
         mod.b,
         '","',
         cate,
         '"]%>%round(2)')
  
  paste(
    "A nota",
    nomes[est,],
    nomes[cate,],
    "foi de",
    valor.a,
    "no modelo",
    mod.a,
    "e de",
    valor.b,
    "no modelo",
    mod.b)
  
} # fim da função compara_notas_mod

compara_notas_mod(1,2,cate="geral",est="mean")

# Testando função compara_notas_est
# Preencher com (mod.a,mod.b,cate="geral",est="mean")
# Modelos  1, 2, 3
# Estatísticas = max, mean, min
# Categorias = A0, L1, L2, L5, L6, L9, L10, L13, L14

compara_notas_mod(1,2,cate="cotas","min")
compara_notas_mod(2,3,cate="L1","max")

# ==============================================================================

# É possível fazer função compara_notas_cat,
# que usa o mesmo modelo, a mesma estatística,
# e compara categorias diferentes. Por exemplo, 
# a função geraria o seguinte texto:
# [1] "No modelo 1, a nota mínima da categoria L1 foi de 677.560
# e a nota mínima da categoria L2 foi de 662.9400."
#
# No entanto, esta função me parece inútil neste momento.

############### RASCUNHO ABAIXO ######################
############### RASCUNHO ABAIXO ######################
############### RASCUNHO ABAIXO ######################



# ==============================================================================


# ==============================================================================

# # Pegar valores individuais de notas
# 
# meta_n["min3","L1"]
# meta_n["max3","L10"]
# meta_n["mean3","L9"]
# 
# # Escrever um em cada linha
# print("Com modelo 2, a média das notas da A0 foi de");meta_n["mean2","A0"];
# 
# # Escrever tudo numa linha só
# paste(
#   "Com modelo",
#   2,
#   ", a média das notas de",
#   "A0",
#   "foi de",
#   meta_n["mean2","A0"],
#   "."
# )
# 
# print("Com modelo 2, a nota média foi de");meta_n["mean2","geral"];
# print("e a nota mínima foi de");meta_n["min2","A0"]
# 
# paste(
#   "Com modelo",
#   2,
#   "a nota",
#   "média",
#   "foi de",
#   meta_n["mean2","geral"],
#   "e a nota",
#   "mínima",
#   "foi de",
#   meta_n["min2","geral"]
# )
# 
# # A nota mínima foi de x no modelo 1, x no modelo 2 e x no modelo 3.
# 
# 
# compara_nota_mods<-function(n){
#   belch6('nota.1<<-meta_n["','mean',n,'","','geral','"]')
#   paste(nota.1)
# }
# compara_nota_mods(1)
# 
# compara_nota_mods<-function(est){
#   
#   belch6('nota.1<<-meta_n["',min,1,'","',geral,'"]')
#   
#   nota.1<<-meta_n["min1","geral"]
#   nota.2<<-meta_n["min2","geral"]
#   nota.3<<-meta_n["min3","geral"]
#   
#   nota1<-meta_n
# paste(
#   "A nota",
# est,
# "foi de",
# min1,
# "no modelo 1",
# min2,
# "no modelo 2, e",
# min3,
# "no modelo 3."
# )}
# 
# 
# #
# 
# 
# 
# # ------------------------------------------------------------------------------
# # Função compara_notas_mod: 
# # compara a mesma estatistica em modelos diferentes
# 
# compara_notas_mod<-function(mod.a,mod.b,cate="geral",est="mean"){
#   
#   belch6('valor.a<<-meta_n["',
#          est,
#          mod.a,
#          '","',
#          cate,
#          '"]%>%round(2)')
#   
#   belch6('valor.b<<-meta_n["',
#          est,
#          mod.b,
#          '","',
#          cate,
#          '"]%>%round(2)')
#   
#   paste(
#     "A nota",
#     nomes[est,],
#     nomes[cate,],
#     "foi de",
#     valor.a,
#     "no modelo",
#     mod.a,
#     "e de",
#     valor.b,
#     "no modelo",
#     mod.b)
#   
# } # fim da função compara_notas_mod
# 
# compara_notas_mod(1,2,cate="geral",est="mean")
# 
# # Testando função compara_notas_est
# # Preencher com (mod.a,mod.b,cate="geral",est="mean")
# # Modelos  1, 2, 3
# # Estatísticas = max, mean, min
# # Categorias = A0, L1, L2, L5, L6, L9, L10, L13, L14
# 
# compara_notas_mod(1,2,cate="cotas","min")
# compara_notas_mod(2,3,cate="L1","max")
