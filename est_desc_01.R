# ==============================================================================
# Arquivo: est_desc_01.R

#
# Modificado em: 2023-04-12.
# Autor: Mateus Silva Figueiredo
#
# Utiliza dados_ufv carregado por data_04_carregar_dados_UFV

### OBJETIVO A FAZER:
# Gera estatística descritiva de cada curso, ano e modalidade

# ==============================================================================
# Pacotes
# install.packages("esquisse")
library(esquisse)

# ==============================================================================

dados_ufv %>% head(1)
dados_ufv %>% colnames() -> colunas
colunas

# ------------------------------------------------------------------------------

# Identificar lista de cursos_ufv.
unique(dados_ufv$Curso) %>% sort() -> cursos_ufv
cursos_ufv # 70 cursos
cursos_ufv[5]

# ==============================================================================
# Apenas convocados em 1a chamada = dados_convocados

dados_ufv[Numero_Chamada_Convocacao == 1] -> dados_convocados

dados_convocados[Curso == cursos_ufv[1]]$nota %>% min()
dados_convocados[Curso == cursos_ufv[1]]$nota %>% max()
dados_convocados[Curso == cursos_ufv[1]]$nota %>% mean()
dados_convocados[Curso == cursos_ufv[1]]$nota %>% sd()

# Selecionar colunas
colunas<-c("id","Processo_Seletivo","nota",
"Modalidade_Inscrita","Numero_Chamada_Convocacao" ,"Modalidade_Convocada",
"Curso","Centro",
"mod_ins","mod_con",
"pub","bxa","ppi","pcd")

dados_convocados <- dados_convocados %>% select(all_of(colunas))

# group = ano + escola pública ou não
dados_convocados$group <- paste0(dados_convocados$Processo_Seletivo,dados_convocados$pub)

dados_convocados$ano <- paste0(dados_convocados$Processo_Seletivo,dados_convocados$pub)


# transforma group em apenas ano + grupo (cota ou AC) 
dados_convocados$group<- gsub("FALSE"," AC",dados_convocados$group)
dados_convocados$group<- gsub("TRUE"," cota",dados_convocados$group)
dados_convocados$group<- gsub("SISU","",dados_convocados$group)

# transforma coluna pub em algo bonito pro gráfico plotar
dados_convocados$pub<- gsub("TRUE","cota",dados_convocados$pub)
dados_convocados$pub<- gsub("FALSE","AC",dados_convocados$pub)


# ==============================================================================
# ==============================================================================
# ==============================================================================
# Gráficos

# ------------------------------------------------------------------------------
# Necessário para ambas ridgelines
# Create a vector with the year labels
year_labels <- c("2022","","2021", "", "2020", "", "2019", "", "2018", "",
                 "2017", "", "2016", "", "2015", "", "2014", "",
                 "2013", "")

# ------------------------------------------------------------------------------
# Ridgeline automática. Linha horizontal até o fim dos eixos

# Seleciona curso
dados_convocados[Curso == cursos_ufv[6]] -> dados_curso
# cursos_ufv[6] # confere curso
# dados_curso[Processo_Seletivo == "SISU2020"] -> dados_curso # cf um ano

# Fazer gráfico ridgeline com máximo e mínimo (|) e média (o) usando stat_summary

# Plot ridgeline
ggplot(dados_curso, aes(x = nota, y = fct_rev(group), fill = pub)) +
  geom_density_ridges() +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3,
               color = "white", # ponto indica média
               position = position_nudge(y = 0.1), # ponto um pouco pra cima 
               show.legend = F
               ) +
  stat_summary(fun = "min", geom = "point", shape = "|", size = 2,
               color = "black", # ponto indica média
               position = position_nudge(y = 0.2), # ponto um pouco pra cima 
               show.legend = F
               ) +
  stat_summary(fun = "max", geom = "point", shape = "|", size = 2,
               color = "black", # ponto indica média
               position = position_nudge(y = 0.2), # ponto um pouco pra cima
               show.legend = F
  ) +
  scale_y_discrete(limits = rev(levels(dados_curso$group))
                   , labels = year_labels # adiciona year_labels
  )  +
  coord_cartesian(xlim = c(400, 820)) + # decide nota max e min no eixo x 
    scale_fill_cyclical(values = c("#F8766D", "#00BA38"), guide = "legend",
                        name = "Modalidade") +
  #   theme_ridges(grid = T) # + 
  labs(title = gsub("_"," ",dados_curso$Curso[1]), # título = curso sem _
       x = "Nota",
       y = "Ano",
       fill = "") +  # new legend label
  theme_minimal()

# ------------------------------------------------------------------------------
# Ridgeline com cálculo de densidade, sem linha horizontal até o fim dos eixos

i <- 25 # caso não for usar o loop
# for loop para gerar todos os gráficos de uma vez
# for (i in 1:length(cursos_ufv))
{
# Seleciona curso
dados_curso <- dados_convocados[Curso == cursos_ufv[i]] # seleciona por indice

# dados_curso <- dados_convocados[Curso == cursos_ufv[which(cursos_ufv=="CIENCIAS_BIOLOGICAS")]] # seleciona por nome
  
# cursos_ufv[i] # confere curso
# dados_convocados[Curso == cursos_cca[i]] -> dados_curso # por centro
# dados_curso[Processo_Seletivo == "SISU2020"] -> dados_curso # cf um ano

# calcular densidade
dados_densidades <- dados_curso %>%
  group_by(group) %>%
  group_modify(~ ggplot2:::compute_density(.x$nota, NULL)) %>%
  rename(nota = x)

# cria coluna pub com base em group
dados_densidades$pub <- gsub('[[:digit:]]+ ', '', dados_densidades$group)

# Plot ridgelines

grafico <- ggplot(dados_densidades, aes(x = nota, y = fct_rev(group), height = density, fill = pub)) + 
  geom_density_ridges(stat = "identity") +
  scale_fill_manual(values = c("#F8766D", "#00BA38"), name = "teste") +
  theme_minimal() +
  labs(
    title = gsub("_"," ",dados_curso$Curso[1]), # remove _ do curso
    x = "Nota", y = "Ano") +
  coord_cartesian(xlim = c(400, 820)) +
  scale_y_discrete(limits = rev(levels(dados_curso$group))
#                   , labels = year_labels # adiciona year_labels, 
                   # year_labels dá mentira quando curso mudou nesses 10 anos
#                  , labels = unique(dados_densidades$group) # testando labels, 
  ); print(grafico)

# fazendo ridgeline com base em densidade calculada, não adicionar max mean min


}


# ==============================================================================
# Rascunhos abaixo

# ==============================================================================
# Histogramas

# ------------------------------------------------------------------------------
# Colocar 10 histogramas na mesma página
par(mfrow=c(2,5)) # 2 linhas, 5 colunas de gráficos

anos <- c("SISU2013","SISU2014",
          "SISU2015","SISU2016",
          "SISU2017","SISU2018",
          "SISU2019","SISU2020",
          "SISU2021","SISU2022")

i <- 2 # caso não use loop
for (i in 1:10){
dados_convocados[dados_convocados$Processo_Seletivo==anos[i]] -> dados
hist(dados$nota,ylim=c(0,350),xlim=c(300,900),
       breaks = seq(from=0, to=900, by=10),
       main=dados$Processo_Seletivo[1])
} # caso use loop

par(mfrow=c(1,1)) # retornar para apenas 1 gráfico por imagem
# ------------------------------------------------------------------------------
# ==============================================================================
# Boxplot

# Selecionar curso
dados_convocados[Curso == "MEDICINA"] -> dados_curso


# ------------------------------------------------------------------------------

# Boxplot no base R
if(FALSE){ #if(FALSE) serve para ignorar o código
boxplot(nota ~ group, dados_curso,
        at = c(1+00,2+00,
               1+04,2+04,
               1+08,2+08,
               1+12,2+12,
               1+16,2+16,
               1+20,2+20,
               1+24,2+24,
               1+28,2+28,
               1+32,2+32,
               1+36,2+36
               ),
        main = "Medicina",
        xlab = "Escola pública?",
        ylab = "Nota",
        col = c("red","green"))} #if(FALSE) serve para ignorar o código

# ------------------------------------------------------------------------------
# Boxplot e violin no ggplot
library(ggplot2)

# boxplot vertical
ggplot(dados_curso, aes(x = group, y = nota, fill = pub)) +
  geom_boxplot() + # escolher ou boxplot
#  geom_violin() +  # ou violino
  scale_fill_manual(values = c("red", "blue")) +
  labs(
    title = gsub("_"," ",dados_curso$Curso[1]), # título = curso sem _
       x = "Escola pública?", y = "Nota") +
  stat_summary(fun="mean", color="white", shape=16,size=0.1)+
  theme_classic() +
  coord_cartesian(ylim = range(dados_curso$nota))

# violin vertical
ggplot(dados_curso, aes(x = group, y = nota, fill = pub)) +
#  geom_boxplot() + # escolher ou boxplot
  geom_violin() +  # ou violino
  scale_fill_manual(values = c("red", "blue")) +
#  stat_summary(fun.y="mean")+ # média
  labs(
    title = gsub("_"," ",dados_curso$Curso[1]), # título = curso sem _
    x = "Escola pública?", y = "Nota") +
  scale_x_discrete(labels = c("A"),
                   breaks = c(1)) +
  theme_classic() +
  coord_cartesian(ylim = range(dados_curso$nota))

# ------------------------------------------------------------------------------
# Selecionar curso e candidatos
dados_convocados[Curso == cursos_ufv[6]] -> dados_curso
# dados_curso[Processo_Seletivo == "SISU2022"] -> dados_curso # cf um ano
# dados_curso[pcd == TRUE] -> dados_curso # cf por grupo

# boxplot horizontal funcionando ok
ggplot(dados_curso, aes(y = fct_rev(group), x = nota, fill = pub)) +
  geom_boxplot() + # escolher ou boxplot
#  geom_violin() +  # ou violino
  scale_fill_manual(values = c("red", "blue")) +
  labs(
#    title = dados_curso$Curso[1], # título = curso
    title = gsub("_"," ",dados_curso$Curso[1]), # remove _ do curso
       x = "Nota", y = "Escola pública?") +
#scale_y_discrete(labels = year_labels, position = position_nudge(y = 0.2)) +
  stat_summary(fun="mean", color="white", shape=16,size=0.1)+
#  theme_classic() +
  coord_flip() +
  coord_cartesian(xlim = range(dados_curso$nota))




# ------------------------------------------------------------------------------
# Ridgelines
library(ggridges)

# Create a vector with the year labels
year_labels <- c("2022","","2021", "", "2020", "", "2019", "", "2018", "",
                 "2017", "", "2016", "", "2015", "", "2014", "",
                 "2013", "")

# Selecionar curso e candidatos
dados_convocados[Curso == cursos_ufv[6]] -> dados_curso

dados_convocados$Processo_Seletivo %>% unique()
  


# Fazer gráfico ridgeline com máximo e mínimo (|) e média (o)
# Ridgelines muito boas:

# Ridgeline
ggplot(dados_curso, aes(x = nota, y = fct_rev(group), fill = pub)) +
  geom_density_ridges() +
    stat_summary(fun = "mean", geom = "point", shape = 21, size = 3,
               color = "white", # ponto indica média
               position = position_nudge(y = 0.1) # ponto um pouco pra cima 
               ) +
  stat_summary(fun = "min", geom = "point", shape = "|", size = 2,
               color = "black", # ponto indica média
               position = position_nudge(y = 0.2) # ponto um pouco pra cima 
  ) +
  stat_summary(fun = "max", geom = "point", shape = "|", size = 2,
               color = "black", # ponto indica média
               position = position_nudge(y = 0.2) # ponto um pouco pra cima 
  ) +
  scale_y_discrete(limits = rev(levels(dados_curso$group))
                   , labels = year_labels # adiciona year_labels
                   )  +
  coord_cartesian(xlim = c(400, 820)) + # decide nota max e min no eixo x 
#  scale_fill_cyclical(values = c("red","blue"), guide = "legend")
#   theme_ridges(grid = T) # + 
  labs(title = gsub("_"," ",dados_curso$Curso[1]), # título = curso sem _
       x = "Nota",
       y = "Ano",
       fill = "") +  # new legend label
  theme_minimal()
#   theme(legend.position = "none")

###
# ==============================================================================
# ------------------------------------------------------------------------------


ggplot(dados_curso, aes(x = nota, y = fct_rev(group), fill = pub)) +
  geom_density_ridges() +
  stat_summary(
    fun = "mean", 
    geom = "point", 
    shape = 21, 
    size = 3, 
    color = "white",
    position = position_nudge(y = 0.2)
  ) +
  
  # add linha vertical | para mínimo
  stat_summary(fun = "min", geom = "point", shape = "|", size = 2,
               color = "black", # ponto indica média
               position = position_nudge(y = 0.2) # ponto um pouco pra cima 
  ) +
  # add linha vertical | para máximo
  stat_summary(fun = "max", geom = "point", shape = "|", size = 2,
               color = "black", # ponto indica média
               position = position_nudge(y = 0.2) # ponto um pouco pra cima 
  ) +
  
  scale_y_discrete(limits = rev(levels(dados_curso$group)), labels = year_labels) +
  theme_ridges() + 
  theme(legend.position = "none",
        axis.text.y = element_text(vjust = -0.5))

# ==============================================================================

# ------------------------------------------------------------------------------

ggplot(dados_convocados) +
  aes(x = Processo_Seletivo, y = nota) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()

# ==============================================================================
# Rascunho
set.seed(8642)                                               # Create random data
x <- rnorm(1000)
boxplot(x,col="red")

which(cursos_ufv=="ECONOMIA_DOMESTICA")

# ==============================================================================
# Referências

# Boxplot in Base R # Boxplot no base R
# https://statisticsglobe.com/boxplot-in-r

# Introduction to ggridges # Ridgelines
# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html