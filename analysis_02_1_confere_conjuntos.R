# # ==============================================================================
# Arquivo: analysis_02_1_confere_conjuntos.R

# Rodar após analysis_02_compara_conc.R
# Amplpia df_so_concorridos com colunas c2ok e c5ok

# ------------------------------------------------------------------------------

# df_so_concorridos gera lista de conjuntos concorridos
# no entanto, em alguns deles a concorrencia c2 e c5 deu problema
# o total de convocados foi menor do que o número de vagas
# provavelmente devido a cotista ir pra AC e não ter cotista pra preencher vaga

# foram 17 conjuntos com esse problema

# solução: limitar ainda mais a lista de conjuntos
# problema: isso cria um viés
# decisão: simplificar análise para poder terminar logo o mestrado
# decisão: solução vale a pena, só explicitar o viés depois

# Modificado em 2023-08-31.
# Autor: Mateus Silva Figueiredo

# ==============================================================================
# cria coluna c2ok que vai me falar se a concorência c2 está ok
df_so_concorridos$c2ok <- NA

# cria coluna c5ok que vai me falar se a concorência c5 está ok
df_so_concorridos$c5ok <- NA

# ==============================================================================

# # Para um curso arbitrário
# comp_vagas_ADMINISTRACAO_FL_SISU2018$tot[2] == "+0"

# ------------------------------------------------------------------------------
# Para um curso chamado por i
# i<-1
# 
# df_so_concorridos[i,1] # curso do conjunto i
# df_so_concorridos[i,2] # sisu do conjunto i
# 
# # analisa total de c2
# comp_vagas_ZOOTECNIA_SISU2018$tot[2]           # valor de tot c2
# comp_vagas_ZOOTECNIA_SISU2018$tot[2] == '+0'   # compara com '+0'
#tot[2] é o total da concorrência 2. se for '+0' então está ok.

# ------------------------------------------------------------------------------
# para todos os cursos # c2
# --- eu + chat gpt

for (i in 1:nrow(df_so_concorridos)) {
if (
eval(parse(text =
  paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "$tot[2] == '+0'")))) {
    df_so_concorridos$c2ok[i] <- "ok"
  } else df_so_concorridos$c2ok[i] <- "bad"
}

sum(df_so_concorridos$c2ok == "ok") # 45 cursos c2 ok

# ==============================================================================
# testando c5

# # analisa total de c5 de um curso chamado por i
# comp_vagas_ZOOTECNIA_SISU2018$tot[5]           # valor de tot c5
# comp_vagas_ZOOTECNIA_SISU2018$tot[5] == '+0'   # compara com '+0'

# roda para todos os cursos

for (i in 1:nrow(df_so_concorridos)) {
  if (
    eval(parse(text =
               paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "$tot[5] == '+0'")))) {
    df_so_concorridos$c5ok[i] <- "ok"
  } else df_so_concorridos$c5ok[i] <- "bad"
}

sum(df_so_concorridos$c5ok == "ok") # 32 cursos c5 ok

# ==============================================================================
# c3 e c4 provavelmente desnecessário

# cria coluna c3ok que vai me falar se a concorência c3 está ok
df_so_concorridos$c3ok <- NA

# cria coluna c4ok que vai me falar se a concorência c4 está ok
df_so_concorridos$c4ok <- NA

# roda para todos os cursos

# c3

for (i in 1:nrow(df_so_concorridos)) {
  if (
    eval(parse(text =
               paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "$tot[3] == '+0'")))) {
    df_so_concorridos$c3ok[i] <- "ok"
  } else df_so_concorridos$c3ok[i] <- "bad"
}

sum(df_so_concorridos$c3ok == "ok") # 63 cursos c3 ok

# c4

for (i in 1:nrow(df_so_concorridos)) {
  if (
    eval(parse(text =
               paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "$tot[4] == '+0'")))) {
    df_so_concorridos$c4ok[i] <- "ok"
  } else df_so_concorridos$c4ok[i] <- "bad"
}

sum(df_so_concorridos$c4ok == "ok") # 63 cursos c4 ok

# ==============================================================================
# gerar df_muito_concorridos apenas com c2ok e c5ok
df_muito_concorridos <- subset(df_so_concorridos,df_so_concorridos$c2ok=="ok")
df_muito_concorridos <- subset(df_muito_concorridos,df_muito_concorridos$c5ok=="ok")
