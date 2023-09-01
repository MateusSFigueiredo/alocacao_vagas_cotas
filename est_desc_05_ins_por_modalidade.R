# ==============================================================================
# Arquivo: est_desc_05_ins_por_modalidade.R
#
# Quantos inscritos tem por ano, por modalidade de inscrição?
# usar mod_ins

# Não distingue cursos

# Faz:
# Data.frame ins_mod com n. inscritos por mod, por ano
# Gráfico de barras empilhado com n. inscritos por mod, por ano
# Gráfico de linhas, mesmos dados

# Data.frame ins_mod2 com n. inscritos por população (bxa, ppi, pcd)

# Falta:
# Definir cores para gráfico
# Exportar gráfico (não sei se precisa mesmo)
#
# Modificado em: 2023-09-01.
# Autor: Mateus Silva Figueiredo
# ==============================================================================
# Preparação
library(ggplot2)# gráficos
library(pals) # cores

# ==============================================================================
# Carregar dados_ufv

setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")

# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- T   # deseja separar candidatos por curso? obrigatório
por_ano   <- T   # deseja separar candidatos por ano? opcional
source("data_04_carregar_dados_UFV.R") # cria ~10 objetos

# ==============================================================================
# objetos necessários

sisu_anos<-paste0("SISU",c(2013:2022))

# ==============================================================================

# Cria ins_mod = data.frame com n de inscritos por modalidade por ano
ins_mod <- data.frame(sisu_anos=sisu_anos,
                          anos=2013:2022,
                         ins_L01=NA,
                         ins_L02=NA,
                         ins_L05=NA,
                         ins_L06=NA,
                         ins_L09=NA,
                         ins_L10=NA,
                         ins_L13=NA,
                         ins_L14=NA
                         )

# Coloca nome nas linhas
row.names(ins_mod) <- c(2013:2022)

# ------------------------------------------------------------------------------

# # Preenche para uma modalidade, um ano
# i<-2013
# dados_ufv[mod_ins == "L01"][Processo_Seletivo == paste0("SISU",i)] %>%
#   nrow() -> ins_mod[paste(i),"ins_L01"]
# 
# # Preenche para uma modalidade, todos os anos
# for (i in 2013:2022){
#   dados_ufv[mod_ins == "L01"][Processo_Seletivo == paste0("SISU",i)] %>%
#     nrow() -> ins_mod[paste(i),"ins_L01"]
# }


# Preenche para todas as modalidades, todos os anos

for (modalidade in c("A0","L01","L02","L05","L06","L09","L10","L13","L14")){
  for (i in 2013:2022) {
  
    dados_ufv[mod_ins %in% modalidade][Processo_Seletivo == paste0("SISU",i)] %>%
      nrow() -> ins_mod[paste(i), paste("ins_", modalidade, sep = "")]
    
  } # fecha i in 2013:2022
} # fecha modalidade in c("A0"...)

# ------------------------------------------------------------------------------

# Preenche para UFV inteira, todos os anos. Bom para conferir soma.
for (i in 2013:2022){
  dados_ufv[Processo_Seletivo == paste0("SISU",i)] %>%
    nrow() -> ins_mod[paste(i),"ins_ufv"]
}

# ------------------------------------------------------------------------------
# Confere somatória. Deve ser tudo TRUE.
ins_mod$ins_ufv == (
  ins_mod$ins_A0 +
    ins_mod$ins_L01 +
    ins_mod$ins_L02 +
    ins_mod$ins_L05 +
    ins_mod$ins_L06 +
    ins_mod$ins_L09 +
    ins_mod$ins_L10 +
    ins_mod$ins_L13 +
    ins_mod$ins_L14)

# ------------------------------------------------------------------------------
# Cria colunas de totais por tipo de população
# Data.frame ins_mod2 para guardar dados
# Data.frame ins_mod vai ser usado para gráficos

if (F){ # deseja criar ins_mod2? T = sim. F = não.
ins_mod2<-ins_mod

# Total de inscrições por cotas em ins_cotas
ins_mod2$ins_cotas <- (
  ins_mod$ins_L01 +
    ins_mod$ins_L02 +
    ins_mod$ins_L05 +
    ins_mod$ins_L06 +
    ins_mod$ins_L09 +
    ins_mod$ins_L10 +
    ins_mod$ins_L13 +
    ins_mod$ins_L14)

# Total de inscrições por cotas ppi em ins_cotas
ins_mod2$ins_bxa <- (
  ins_mod$ins_L01 +
    ins_mod$ins_L02 +
    ins_mod$ins_L09 +
    ins_mod$ins_L10)

# Total de inscrições por cotas ppi em ins_cotas
ins_mod2$ins_pcd <- (
  ins_mod$ins_L02 +
    ins_mod$ins_L06 +
    ins_mod$ins_L10 +
    ins_mod$ins_L14)

# Total de inscrições por cotas pcd em ins_cotas
ins_mod2$ins_pcd <- (
    ins_mod$ins_L09 +
    ins_mod$ins_L10 +
    ins_mod$ins_L13 +
    ins_mod$ins_L14)
} # fim de criar ins_mod2

# ==============================================================================

### Preparar para gráfico

# Deletar colunas desnecessárias
ins_mod$ins_ufv <- NULL
ins_mod$sisu_anos <- NULL

# renomeia colunas para facilitar gráfico
colnames(ins_mod) <- gsub("ins_","",colnames(ins_mod)) 

# ----------
# Preparar para gráfico. Obrigado ChatGPT.

# Reshape the data from wide to long format
data_long <- reshape2::melt(ins_mod, id.vars = "anos", variable.name = "Mod", value.name = "Count")

#---
# cor. Precisa de 9 cores.

# usando pacote pals: stepped3 roxo, stepped2 verde, alphabet laranja
# colors <- c(unname(stepped3())[13:16], # tons de roxo
#             unname(stepped2())[5:8], # tons de verde
#             unname(alphabet())[15])  # laranja feio

#usando hexadecimal para obter mesmas cores:
colors <- c("#756BB1","#9E9AC8","#BCBDDC","#DADAEB",
            "#637939","#8CA252","#B5CF6B","#CEDB9C",
            "#ff9a22") # outro laranja

# ------------------------------------------------------------------------------
# Create the stacked bar graph
grafico <- 
ggplot(data_long, aes(x = as.factor(anos), y = Count, fill = 
  # A0 em baixo
    factor(Mod, levels = c("L01","L02","L05","L06","L09","L10","L13","L14","A0")))) +
  # # A0 em cima                  
  # factor(Mod, levels = c("A0","L01","L02","L05","L06","L09","L10","L13","L14")))) +
    geom_bar(stat = "identity") +
  labs(x = "Edição do SISU", y = "Inscritos por Modalidade", fill = "Mod") +
  scale_fill_manual(values = colors) +
  theme_bw() +
  ggtitle("Inscritos por modalidade de cotas na UFV") +
  theme(legend.position = "right")


print(grafico)



# ==============================================================================
# Fazer gráfico de linhas

# modifica ins_mod para 0 virar NA
mydata[zero_vagas, "L01"][mydata[zero_vagas, "L01"] == 0] <- NA
ins_mod[ins_mod==0] <- NA
ins_mod

# Gráfico de linhas
grafico <- ggplot(data = ins_mod, aes(x = anos)) +
  geom_line(aes(y = L01, color = "L01")) +
  geom_line(aes(y = L02, color = "L02")) +
  geom_line(aes(y = L05, color = "L05")) +
  geom_line(aes(y = L06, color = "L06")) +
  geom_line(aes(y = L09, color = "L09")) +
  geom_line(aes(y = L10, color = "L10")) +
  geom_line(aes(y = L13, color = "L13")) +
  geom_line(aes(y = L14, color = "L14")) +
  geom_line(aes(y = A0, color = "A0")) +
  labs(x = "Edição do SISU", y = "Número de inscritos", color = "Modalidade") +
  scale_color_manual(values = c(
    L01 = "blue", L02 = "red", L05 = "green", L06 = "purple", 
    L09 = "orange", L10 = "darkgray", L13 = "brown", L14 = "cyan", 
    A0 = "black"
  )) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(ins_mod$anos), max(ins_mod$anos), by = 1)) +
#  scale_y_log10() + # y em log
  ggtitle("Inscritos por modalidade por ano")

print(grafico)
rm(grafico)

# ------------------------------------------------------------------------------
# Gráfico de linhas apenas de PCD

ins_mod_pcd <- ins_mod[6:10,]
ins_mod_pcd[,2:5] <- NULL
ins_mod_pcd

grafico <- ggplot(data = ins_mod_pcd, aes(x = anos)) +

  geom_line(aes(y = L09, color = "L09")) +
  geom_line(aes(y = L10, color = "L10")) +
  geom_line(aes(y = L13, color = "L13")) +
  geom_line(aes(y = L14, color = "L14")) +
  labs(x = "Edição do SISU", y = "Número de inscritos", color = "Modalidade") +
  scale_color_manual(values = c(
    L09 = "orange", L10 = "darkgray", L13 = "brown", L14 = "cyan"
  )) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(2017), max(ins_mod$anos), by = 1)) +
  #  scale_y_log10() + # y em log
  ggtitle("Inscritos por modalidade PCD por ano")

print(grafico)

# Fim do código em 2023-09-01
# ==============================================================================
# Referências
# R Color Brewer's palettes https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
# Pals' palettes https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html 

# ==============================================================================

# Possibilidades de cores
# library(pals)

# sequências que me interessaram:
colors <- c(unname(stepped3())[13:16], # tons de roxo
            unname(stepped2())[5:8], # tons de verde
            unname(kelly())[5])  # laranja

colors <- c(unname(stepped2())[5:8], # tons de verde
            unname(stepped3())[5:8], # tons de laranja
            unname(stepped3())[2])  # azul

colors <- c(unname(stepped2())[5:8], # tons de verde
            unname(stepped3())[5:8], # tons de laranja
            "#201A01")

# sequências stepped possíveis:
unname(stepped()) == c(
"#990F26", "#B33E52", "#CC7A88", "#E6B8BF", # unname(stepped())[1:4]
"#99600F", "#B3823E", "#CCAA7A", "#E6D2B8", 
"#54990F", "#78B33E", "#A3CC7A", "#CFE6B8", 
"#0F8299", "#3E9FB3", "#7ABECC", "#B8DEE6",
"#3D0F99", "#653EB3", "#967ACC", "#C7B8E6",
"#333333", "#666666", "#999999", "#CCCCCC")

unname(stepped2()) == c(
"#393B79", "#5254A3", "#6B6ECF", "#9C9EDE", 
"#637939", "#8CA252", "#B5CF6B", "#CEDB9C", # unname(stepped2())[5:8]
"#8C6D31", "#BD9E39", "#E7BA52", "#E7CB94", 
"#843C39", "#AD494A", "#D6616B", "#E7969C",
"#7B4173", "#A55194", "#CE6DBD", "#DE9ED6")

unname(stepped3()) == c(
"#3182BD", "#6BAED6", "#9ECAE1", "#C6DBEF", 
"#E6550D", "#FD8D3C", "#FDAE6B", "#FDD0A2", 
"#31A354", "#74C476", "#A1D99B", "#C7E9C0",
"#756BB1", "#9E9AC8", "#BCBDDC", "#DADAEB", # unname(stepped3())[13:16]
"#636363", "#969696", "#BDBDBD", "#D9D9D9")

# R Color Brewer's 
# colors <- brewer.pal(7, "Set1")  # Change the number to match the number of Centro categories
# colors <- brewer.pal(7, "Accent")  # Change the number to match the number of Centro categories
