# ==============================================================================
# Arquivo: analysis_02_meta.R
# Analisa as três modalidades de concorrência
#
#

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
# Carregar função necessária
belch4<-function(x, y, z, a) 
{eval(parse(text=(paste(x, y, z, a,sep=""))))}

belch6<-function(x, y, z, a, b, d) 
{eval(parse(text=(paste(x, y, z, a, b, d, sep=""))))}

# ==============================================================================
#
# Gerar funções
#
# ------------------------------------------------------------------------------
# Função compara_vagas:
# analisar alocação de vagas do mesmo grupo social em modelos diferentes

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
    dimensao,
    ". Este número é",
    ((valor.a/valor.b)*100-100)%>%abs()%>%round(),
    ifelse(valor.a<=valor.b,"% menor","% maior"),
    "do que os",
    valor.b,
    "estudantes",
    dimensao,
    "aprovados pelo modelo",
    mod.b)
}
# Fim da função compara_vagas

# Testa função compara_vagas
# Preencher com (um modelo, outro modelo, grupo)
# Modelos  1, 2, 3
# Grupos = tot, ppi, pcd, pub, bxa
compara_vagas(1,2,"ppi")
compara_vagas(2,3,"pcd")

# ------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------
# Função compara_notas_est: 
# compara estatísticas diferentes no mesmo modelo 

compara_notas_est<-function(mod.a,est.1,est.2){
  belch4('valor.a<<-meta_n["',est.1,mod.a,'","geral"]%>%round(2)');
  belch4('valor.b<<-meta_n["',est.2,mod.a,'","geral"]%>%round(2)');
  paste(
    "Com o modelo",
    mod.a,
    ", a nota",
    est.1,
    "foi de",
    valor.a,
    "e a nota",
    est.2,
    "foi de",
    valor.b)
} # fim da função compara_notas_est

# Testando função compara_notas_est
# Preencher com (modelo, uma estatística, outra estatística)
# Modelos  1, 2, 3
# Estatísticas = max, mean, min
compara_notas_est(2,'mean','min')
compara_notas_est(3,'max' ,'min')



# ==============================================================================

############### RASCUNHO ABAIXO ######################
############### RASCUNHO ABAIXO ######################
############### RASCUNHO ABAIXO ######################

# ==============================================================================



#
# Pegar valores individuais
meta_v["n1","tot"]

# Escrever um em cada linha
print("Com modelo 2, entraram");meta_v["n2","ppi"];"estudantes ppi"

# Escrever tudo numa linha só
paste(
  "Com modelo",
  2,
  ", entraram",
  meta_v["n2","ppi"],
  "estudantes ppi, um número",
  round((meta_v["n2","ppi"]/meta_v["n1","ppi"])*100-100),
  ifelse(meta_v["n2","ppi"]>=meta_v["n1","ppi"], "% maior","% menor"),
  "do que os",
  meta_v["n1","ppi"],
  "estudantes ppi que entraram pelo modelo",
  1,
  "."
)


# ==============================================================================



# Algumas análises
compara_vagas(1,2,"ppi")
compara_vagas(2,3,"bxa")
compara_vagas(1,3,"tot")
compara_vagas(3,1,"pcd")
compara_vagas(2,3,"pub")
compara_vagas(3,2,"pub")

# ==============================================================================

# Pegar valores individuais de notas

meta_n["min3","L1"]
meta_n["max3","L10"]
meta_n["mean3","L9"]

# Escrever um em cada linha
print("Com modelo 2, a média das notas da A0 foi de");meta_n["mean2","A0"];

# Escrever tudo numa linha só
paste(
  "Com modelo",
  2,
  ", a média das notas de",
  "A0",
  "foi de",
  meta_n["mean2","A0"],
  "."
)

print("Com modelo 2, a nota média foi de");meta_n["mean2","geral"];
print("e a nota mínima foi de");meta_n["min2","A0"]

paste(
  "Com modelo",
  2,
  "a nota",
  "média",
  "foi de",
  meta_n["mean2","geral"],
  "e a nota",
  "mínima",
  "foi de",
  meta_n["min2","geral"]
)

# ------------------------------------------------------------------------------
# Função funcionando:
compara_notas_est<-function(mod.a,est.1,est.2){
  belch4('valor.a<<-meta_n["',est.1,mod.a,'","geral"]%>%round(2)');
  belch4('valor.b<<-meta_n["',est.2,mod.a,'","geral"]%>%round(2)');
  paste(
    "Com o modelo",
    mod.a,
    ", a nota",
    est.1,
    "foi de",
    valor.a,
    "e a nota",
    est.2,
    "foi de",
    valor.b)
  } # fim da função

compara_vagas_est(2,'mean','min')
# fim da função
# ------------------------------------------------------------------------------


# A nota mínima foi de x no modelo 1, x no modelo 2 e x no modelo 3.


compara_nota_mods<-function(n){
  belch6('nota.1<<-meta_n["','mean',n,'","','geral','"]')
  paste(nota.1)
}
compara_nota_mods(1)

compara_nota_mods<-function(est){
  
  belch6('nota.1<<-meta_n["',min,1,'","',geral,'"]')
  
  nota.1<<-meta_n["min1","geral"]
  nota.2<<-meta_n["min2","geral"]
  nota.3<<-meta_n["min3","geral"]
  
  nota1<-meta_n
paste(
  "A nota",
est,
"foi de",
min1,
"no modelo 1",
min2,
"no modelo 2, e",
min3,
"no modelo 3."
)}


#



# ==============================================================================
#
# Parte boa
#
# ==============================================================================

# Comparar alocação de vagas para diferentes grupos sociais com diferentes modelos
compara_vagas(1,2,"ppi")
compara_vagas(2,3,"pcd")

# Preencher com (um modelo, outro modelo, grupo)
# Modelos  1, 2, 3
# Grupos = tot, ppi, pcd, pub, bxa

# ------------------------------------------------------------------------------

# Comparar estatísticas diferentes do mesmo modelo de alocação de vagas
compara_notas_est(2,'mean','min')
compara_notas_est(3,'max' ,'min')

# Preencher com (modelo, uma estatística, outra estatística)
# Modelos  1, 2, 3
# Estatísticas = max, mean, min
