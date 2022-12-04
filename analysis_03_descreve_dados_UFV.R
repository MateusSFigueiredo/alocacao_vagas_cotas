# ==============================================================================
# Arquivo: analysis_03_descreve_dados_UFV.R
#
# incompleto
#
# Modificado em: 2022-11-28
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

dados_ano <- dados_2013
ncurso <- 1

dados_ano$Curso %>% unique() -> cursos

subset(dados_ano,Curso==cursos[ncurso]) -> dados_curso

cursos[ncurso]
dados_curso %>% count(mod_ins) -> teste; print(teste)
dados_curso %>% count(mod_con) -> teste; print(teste)


