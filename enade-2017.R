#vetor_pacotes=c("readr","ggplot2","plotly","e1071",
#                "dplyr","Hmisc","esquisse","gridExtra")
#install.packages(vetor_pacotes)
library(readr)
library(ggplot2)
library(plotly)
library(e1071)
require(dplyr)
require(Hmisc)
require(esquisse)
#require(devtools)
require(gridExtra)

# Leitura do dataset
getwd()
setwd("F:\\Comunidade_estatistica\\ENADE\\projeto")
df = read_csv2("MICRODADOS_ENADE_2017.txt")

#filtrar as variaveis
df1 = df %>% dplyr::select(CO_GRUPO, CO_REGIAO_CURSO,NU_IDADE,TP_SEXO,CO_TURNO_GRADUACAO,
                           NT_GER,QE_I01,QE_I02,QE_I08,QE_I21,QE_I23, NT_OBJ_FG,NT_OBJ_CE
                         )
names(df1_ti)

#Filtrando os dados só para os profissionais de análise e desenvolvimento de sistemas (t.i)
df1_ti = df1 %>% filter(CO_GRUPO==72) 
table(df1_ti$CO_GRUPO)



# recodificando as variaveis(Estado civil, regiao do curso,sexo , horas de estudos semanais)
df1_ti = df1_ti %>% mutate(estado_civil = case_when(QE_I01=="A"~"Solteiro(a)",
                                                     QE_I01=="B"~"Casado(a)",
                                                     QE_I01=="C"~"Separado(a)",
                                                     QE_I01=="D"~"Víuvo(a)",
                                                     QE_I01=="E"~"Outro"))


df1_ti = df1_ti %>% mutate(regiao = case_when(CO_REGIAO_CURSO == 1 ~ "Norte",
                                           CO_REGIAO_CURSO == 2 ~ "Nordeste",
                                           CO_REGIAO_CURSO == 3 ~ "Sudeste",
                                           CO_REGIAO_CURSO == 4 ~ "Sul",
                                           CO_REGIAO_CURSO == 5 ~ "Centro-Oeste"))

df1_ti = df1_ti %>% mutate(sexo = case_when(TP_SEXO == "M" ~ "Masculino",
                                            TP_SEXO == "F" ~ "Feminino"))



df1_ti = df1_ti %>% mutate(hestudos = case_when(QE_I23 =="A" ~ "Nenhuma, apenas assisto às aulas",
                                                QE_I23 =="B" ~ "De uma a três",
                                                QE_I23 =="C" ~ "De quatro a sete",
                                                QE_I23 =="D" ~ "De oito a doze",
                                                QE_I23 =="E" ~ "Mais de doze"))



#removendo as variáveis 
df1_ti = subset(df1_ti, select = -c(QE_I01, CO_REGIAO_CURSO, TP_SEXO, QE_I23))

# ANALISE DESCRITIVA DAS VARIAVEIS
s=summary(df1_ti)
d=describe(df1_ti)

# frequencias
t = table(df1_ti$estado_civil)
p = prop.table(t)

# resumo estado civil
describe(df1_ti$estado_civil)
unique(df1_ti$estado_civil)

# total de cada esdadl civil agrupado
df1_ti %>% select(estado_civil) %>% 
  group_by(estado_civil) %>% 
  summarise(total = n())

# média das notas agrupada por estado civil
df1_ti %>% 
  select(estado_civil, NT_OBJ_FG) %>% 
  group_by(estado_civil) %>% 
  summarise(media = mean(NT_OBJ_FG, na.rm=T))
  
# remover os NA's
df_ti_na = df1_ti %>% na.omit()
resumo_na = df_ti_na %>% 
  select(everything()) %>% 
  summarise_all(list(~sum(is.na(.))))
View(resumo_na)

# total de linhas do dataset original
dim(df1_ti)[1]

#total de linhas do dataset sem os na's
dim(df_ti_na)[1]

# total de linhas removidas
linhas_removidas = dim(df1_ti)[1] - dim(df_ti_na)[1]


# ESTATISTICA DESCRITIVA DA VARIAVEL NOTA
# tamanho do vetor de notas
qtd_notas = length(df_ti_na$NT_OBJ_CE)

# média das notas
media=mean(df_ti_na$NT_OBJ_CE)

# mediana das notas
mediana = median(df_ti_na$NT_OBJ_CE)

# Moda
moda = df_ti_na      %>% select(NT_OBJ_CE) %>% 
       table()     %>%
       which.max() %>% 
       names()     %>% 
       as.numeric()

# concatenando media, mediana e moda
concatenados = data.frame("quantidade de notas" = qtd_notas,
                          "Media" = media,
                          "Mediana"=mediana,
                          "Moda"=moda)



# Temos que a média(42)>mediana(40)=moda(40), logo não podemos afirmar
# que a distribuição é assimétrica, contudo apresentado uma leve simetria, que
# só poderemos afirmar pelo calculo do coeficiente de assimetria de  pearson

#Assimetria
assimetria = skewness(df_ti_na$NT_OBJ_CE)

#Coeficiente de assimetria de pearson=0.1863963 > 0, logo a distribuição terá assimetria
#positiva e concetração a esquerda dos maiores valores.

#Curtose
curtose = kurtosis(df_ti_na$NT_OBJ_CE)
#pelo R, temos que se 
#k>0, leptocúrtica
#k=0, Mesocúrtica
#k<0, Platicúrtica
# nossa curtose deu valor -0.2791433, logo dizendo que é platicúrtica

#concatenar tudo junto
concatenados = cbind(concatenados, assimetria, curtose)

# Gráficos para conferir o que constatamos

# histograma da nota dos alunos com a frequencia relativa das notas
g_hist=ggplot(df_ti_na,aes(x= NT_OBJ_CE))+
  geom_histogram(color="black",fill="lightblue", bins=50, aes(y=(..count..)/sum(..count..)))+
  ggtitle("Histograma da nota dos alunos de TI")+
  xlab("Nota")+
  ylab("Frequencia relativa")
g_hist
  
g_densidade=ggplot(df_ti_na,aes(x= NT_OBJ_CE)) +
  geom_density(col=2,size=1, aes(y=(..count..)/sum(..count..)))+
  ggtitle("Curva de densidade da nota dos alunos de TI")+
  xlab("Nota")+
  ylab("Frequência relativa")
g_densidade

g_hist_densidade = ggplot(df_ti_na,aes(x=NT_OBJ_CE)) + 
  geom_histogram(color = "black",fill="lightblue",bins =50,aes(y=(..count..)/sum(..count..)))+
  geom_density(col=2,size = 1, aes(y = 27 * (..count..)/sum(..count..))) +
  ggtitle("Histograma e curva de densidade da nota dos alunos de análise de sistemas")+
  xlab("nota") +
  ylab("Frequência relativa")
g_hist_densidade
ggplotly(g_hist_densidade)  


grid.arrange( g_hist,
              g_densidade,
              g_hist_densidade,
              nrow=3,ncol=1)



















