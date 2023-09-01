# # ==============================================================================
# Arquivo: analysis_02_2_altera_comp_vagas

# Rodar após analysis_02_compara_conc.R
# Altera dfs comp_vagas_ CURSO _ ANO, apagando linhas de concorrencias com problema

# ------------------------------------------------------------------------------

# df_so_concorridos gera lista de conjuntos concorridos
# no entanto, em alguns deles a concorrencia c2 e c5 deu problema
# o total de convocados foi menor do que o número de vagas
# provavelmente devido a cotista ir pra AC e não ter cotista pra preencher vaga

# foram 17 conjuntos com esse problema em c2

# solução: apagar linhas de concorrencias que dê tot errado
# problema: isso cria um viés
# decisão: simplificar análise para poder terminar logo o mestrado
# decisão: solução vale a pena, só explicitar o viés depois

# Modificado em 2023-08-31.
# Autor: Mateus Silva Figueiredo

# ==============================================================================

# # Para um curso arbitrário
# comp_vagas_ADMINISTRACAO_FL_SISU2018$tot[2] == "+0"

# ------------------------------------------------------------------------------
# Para um curso chamado por i
# i<-1
# 
df_so_concorridos[i,1] # curso do conjunto i
df_so_concorridos[i,2] # sisu do conjunto i

# analisa total de c2
comp_vagas_ZOOTECNIA_SISU2018$tot[2]           # valor de tot c2
comp_vagas_ZOOTECNIA_SISU2018$tot[2] == '+0'   # compara com '+0'
#tot[2] é o total da concorrência 2. se for '+0' então está ok.

# ------------------------------------------------------------------------------
# para todos os cursos # c2

# --- eu
# apaga linha 2 caso tot =! '+0', preenche com NA
# para todos os conjuntos

eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "$tot[2] == '+0'")))

# ==== rodar for loop c2 ====
for (i in 1:nrow(df_so_concorridos)){

if(eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "$tot[2] == '+0'")))){
  "true"
} else {"false"
  eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "[2,] <- NA")))
} # fecha if
  }; print("fim loop c2") # fecha for loop
# se já tiver $tot[2]==NA, vai dar erro, mas tudo bem

# ------------------------------------------------------------------------------
# para todos os cursos # c5

# --- eu
# apaga linha 5 caso tot =! '+0', preenche com NA
# para todos os conjuntos

eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "$tot[5] == '+0'")))

# ==== rodar for loop c5 ====
for (i in 1:nrow(df_so_concorridos)){
  
if(eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "$tot[5] == '+0'")))){
  "true"
} else {"false"
  eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "[5,] <- NA")))
} # fecha if
}; print("fim for loop c5") # fecha for loop
# se já tiver $tot[2]==NA, vai dar erro, mas tudo bem

# ==============================================================================
# Conferir

(eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2]," %>% print"))))

# print todos os 63 conjuntos 
for (i in 1:nrow(df_so_concorridos)){
  # print curso e ano
  print(paste(df_so_concorridos[i,1],df_so_concorridos[i,2]))
  # print comp_vagas CURSO _ ANO
  (eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2]," %>% print"))))
}
