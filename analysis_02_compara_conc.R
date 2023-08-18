# # ==============================================================================
# Arquivo: analysis_02_compara_conc.R
# Compara quatro modelos de concorrencia:
# 1 = concorrencia separada (aka listas multiplas)
# 2 = concorrencia concomitante, AC primeiro
# 3 = concorrencia concomitante, cotas primeiro
# 4 = concorrencia segundo Bó e Senkevics, 2023
# 5 = concorrencia adaptado de Bó e Senkevics, 2023

# Modificado em 2023-05-11.
# Autor: Mateus Silva Figueiredo

# ==============================================================================
# Rodar após analysis_01_1, analysis_01_2, analysis_01_3, 
# analysis_01_4, analysis_01_5

# ==============================================================================

# Cria data.frame comparando vagas

# cria diferença entre modelo1 para modelos 2, 3, 4, 5
# sprintf("%+.f" serve para colocar um sinal de + antes dos números
diff_modelo_c2 <- sprintf("%+.f", analise_v_c2[1,] - analise_v_c1[1,])
diff_modelo_c3 <- sprintf("%+.f", analise_v_c3[1,] - analise_v_c1[1,])
diff_modelo_c4 <- sprintf("%+.f", analise_v_c4[1,] - analise_v_c1[1,])
diff_modelo_c5 <- sprintf("%+.f", analise_v_c5[1,] - analise_v_c1[1,])

comparando_vagas <- rbind(analise_v_c1[1,],diff_modelo_c2,diff_modelo_c3,
                          diff_modelo_c4,diff_modelo_c5)

colnames(comparando_vagas) <- colnames(analise_v_c1)
rownames(comparando_vagas) <- c("Modelo c1",
                                "diff Modelo c2",
                                "diff Modelo c3",
                                "diff Modelo c4",
                                "diff Modelo c5")

