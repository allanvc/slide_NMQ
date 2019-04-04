


# reg log credit risk FIES
# baseado em: http://rpubs.com/vikash_datascientist/182183


library(data.table)

loc <- file.choose()

df <- fread(loc)

# df <- as.data.frame(df, stringsAsFactors = FALSE)

df <- as.data.frame(df, stringsAsFactors = FALSE)

str(df)


# var. dependente

library(dplyr)

df <- df %>%
  select(5:6, 8, 10:11, 12:17, 19, 29, 31, 33:36)

# acrescentar uma variavel se ele ja deveu em algum momento antes da amortizacao
# --- isso vai melhorar muito o modelo

df <- df %>%
  filter(DS_FASE_DIGEF == "Amortizacao")

str(df)

# help("case_when")

df <- df %>%
  mutate(loan_status = case_when((NU_DIAS_ATRASO > 0) ~ "1", TRUE ~ "0"))
# nao faz sentido usar fase pois eh um aspecto temporal -- queremos saber a probabilidade com 
#... as variaveis que temos desde o inicio

# df <- df %>%
#   mutate(loan_status = case_when((NU_DIAS_ATRASO > 0 & DS_FASE_DIGEF == "Amortizacao") ~ "0", TRUE ~ "1"))
# melhor acerto tanto para modelo log quanto para naive bayes

# df <- df %>%
#   mutate(loan_status = case_when((NU_DIAS_ATRASO > 0) ~ "0", TRUE ~ "1"))

# melhor ajuste para achar os devedores

# ajustando idade

library(lubridate)

str(df)

# help("difftime")

df <- df %>%
  mutate(idade = floor(decimal_date(now()) - decimal_date(ymd_hms(DT_NASCIMENTO))))

# now() instead of today, pq today() nao eh POSIXct


# limpando

df <- df %>%
  select(-VL_LIBERADO,-DT_NASCIMENTO, -DS_FASE_DIGEF) # eliminar fase por ser 
#...aspecto temporal


# colocando log da renda familiar e log da rend per capita

# df <- df %>%
#   mutate(log_renda_familiar = log(VL_RENDA_FAMILIAR_BRUTA_MENSAL),
#          log_renda_percapita = log(VL_RENDA_PERCAPTA))

#limpando 2
# df <- df %>%
#   select(-c(VL_RENDA_FAMILIAR_BRUTA_MENSAL, VL_RENDA_PESSOA_BRUTA_MENSAL, 
#             VL_RENDA_PERCAPTA, NU_CEP, NU_CPF, CO_INSCRICAO, 
#             NU_DIAS_ATRASO, NO_ESTUDANTE))

df <- df %>%
  select(-c(NU_DIAS_ATRASO))

df <- df %>%
  select(-c(NU_ANO_CONCLUSAO_ENSINO_MEDIO))

# df <- df %>%
#   select(-c(VL_RENDA_PESSOA_BRUTA_MENSAL,
#             NU_DIAS_ATRASO))

str(df)

# df <- df %>%
#   select(-c(SG_UF,
#             ST_DEFICIENCIA))

df <- df %>%
  na.omit()

str(df)

# df <- df %>%
#   mutate(NU_ANO_CONCLUSAO_ENSINO_MEDIO = as.factor(NU_ANO_CONCLUSAO_ENSINO_MEDIO))


df <- df %>%
  mutate_if(is.character, as.factor)


# transformando por grandes regiões - deixar estados mesmo pela questão de dificuldade dos estados

str(df)

df <- df %>%
  mutate(loan_status = as.numeric(as.character(loan_status)))

df <- df %>%
  select(-VL_INADIMPLENTE)

table(df$SG_SEXO)

table(df$ST_DEFICIENCIA)

table(df$DS_RACA_COR)

table(df$DS_ESTADO_CIVIL)

table(df$ST_BOLSISTA_PROUNI)

table(df$ST_ENSINO_MEDIO_ESCOLA_PUBLICA, df$ST_BOLSISTA_PROUNI)

# df <- df %>%
#   mutate(SG_SEXO = case_when(SG_SEXO == "F" ~ 1, TRUE ~ 2),
#          ST_DEFICIENCIA = case_when(ST_DEFICIENCIA == "N" ~ 1, TRUE ~ 2),
#          DS_RACA_COR = case_when(DS_RACA_COR == "AMARELO" ~ 1,
#                                  DS_RACA_COR == "BRANCO" ~ 2,
#                                  DS_RACA_COR == "INDIO" ~ 3,
#                                  DS_RACA_COR == "NEGRO" ~ 4,
#                                  TRUE ~ 5),
#          DS_ESTADO_CIVIL = case_when(DS_ESTADO_CIVIL == "CASADO" ~ 1,
#                                      DS_ESTADO_CIVIL == "DIVORCIADO" ~ 2,
#                                      DS_ESTADO_CIVIL == "SEPARADO" ~ 3,
#                                      DS_ESTADO_CIVIL == "SOLTEIRO" ~ 4,
#                                      DS_ESTADO_CIVIL == "UNIAO ESTAVEL" ~ 5,
#                                      TRUE ~ 6),
#          
#          ST_BOLSISTA_PROUNI = case_when(ST_BOLSISTA_PROUNI == "N" ~ 1, TRUE ~ 2)
#   )
# 
# str(df)           
# 
# df <- df %>%
#   mutate(SG_UF = case_when(SG_UF == "AC" ~ 1,
#                            SG_UF == "AL"~2,
#                            SG_UF == "AP"~3,
#                            SG_UF == "AM"~3,
#                            SG_UF == "BA"~4,
#                            SG_UF == "CE"~5,
#                            SG_UF == "DF"~6,
#                            SG_UF == "ES"~7,
#                            SG_UF == "GO"~8,
#                            SG_UF == "MA"~9,
#                            SG_UF == "MT"~10,
#                            SG_UF == "MS"~11,
#                            SG_UF == "MG"~12,
#                            SG_UF == "PA"~13,
#                            SG_UF == "PB"~14,
#                            SG_UF == "PR"~15,
#                            SG_UF == "PE"~16,
#                            SG_UF == "PI"~17,
#                            SG_UF == "RJ"~18,
#                            SG_UF == "RN"~19,
#                            SG_UF == "RS"~20,
#                            SG_UF == "RO"~21,
#                            SG_UF == "RR"~22,
#                            SG_UF == "SP"~23,
#                            SG_UF == "SC"~24,
#                            SG_UF == "SE"~25,
#                            SG_UF == "TO"~26))


# df <- df %>%
#   mutate(ST_ENSINO_MEDIO_ESCOLA_PUBLICA = case_when(
#     ST_ENSINO_MEDIO_ESCOLA_PUBLICA == "N" ~ 1,
#     ST_ENSINO_MEDIO_ESCOLA_PUBLICA == "P" ~ 2, 
#     TRUE ~ 3))


str(df)                                 


# df <- df %>%
#   select(-NU_ANO_CONCLUSAO_ENSINO_MEDIO)
# transformando outras variaveis de factors em nros




### preparing the dataset for prediction

library(caTools)

set.seed(1984)
spl = sample.split(df$loan_status, 0.7)
train = subset(df, spl == TRUE)
test = subset(df, spl == FALSE)


modLog = glm(loan_status ~ . , data=train, family=binomial(link = "logit"))
summary(modLog)
# retirar ano conclusao ens medio e ST_DEFICIENCIA

# saveRDS(modLog, "./dados/modLog.rds")

# accuracy

test$predicted.risk = predict(modLog, newdata=test, type="response")

testTable = table(test$loan_status, as.numeric(test$predicted.risk >= 0.5))

# saveRDS(testTable, "./dados/confusion_matrix.rds")

overall_accuracy = (testTable[1,1] + testTable[2,2])/nrow(test)
sensitivity = testTable[2,2]/sum(testTable[2,]) # linha eh a referencia
specificity = testTable[1,1]/sum(testTable[1,]) # soh com amortizacao fica bom


# plot

# confusion_matrix <- as.data.frame(readRDS("./dados/confusion_matrix.rds"))

# confusion_matrix <- as.data.frame(table(predicted_class, actual_class))
library(ggplot2)
plotly_pallette <- c('#1F77B4', '#FF7F0E', '#2CA02C', '#D62728')
library(dplyr)
# p <- ggplot(data = confusion_matrix,
#             mapping = aes(x = Var2,
#                           y = Var1)) +
#   geom_tile(aes(fill = Freq)) +
#   geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
#   scale_fill_gradient(low = '#2CA02C',
#                       high = '#FF7F0E',
#                       trans = "log")+ # if your results aren't quite as clear as the above exampl
#   # scale_x_discrete(limits = rev(confusion_matrix$Var2))+
#   
#   theme_bw()+
#   # theme(panel.border = element_blank())+ # para ficar igual o plotly
#   theme(legend.position = "none")+
#   labs(x = "predicted", y = "original")

## ordem correta:

p <- ggplot(data = confusion_matrix,
            mapping = aes(x = reorder(Var2, dplyr::desc(Var2)),
                          y = Var1)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = '#2CA02C',
                      high = '#FF7F0E',
                      trans = "log")+ # if your results aren't quite as clear as the above exampl
  # scale_x_discrete(limits = rev(confusion_matrix$Var2))+
  
  theme_bw()+
  # theme(panel.border = element_blank())+ # para ficar igual o plotly
  theme(legend.position = "none")+
  labs(x = "predicted class", y = "actual class")

saveRDS(p, "./dados/confusion_matrix_plot.rds")

p <- ggplotly(p)
library(htmlwidgets)
library(plotly)
saveWidget(as_widget(ggplotly(p)), file = "/home/allan/Documents/r-projects/apres_NMQ1/assets/widgets/ggplotly_confusion_matrix.html")
# plotly::ggplotly(p)

#=============================================================================



# analise exploratoria - correlacao variaveis

res <- cor(df) # precisamos antes transformar em numeric
round(res, 2)

# cor entre renda per capita e renda familiar eh alto - deixar familiar bruta por conta de fiador
# mas eh melhor renda per capita pegaria nro pessoas na familia


#### ===== novo modelo retirando-se variaveis nao significativas e com alta correlacao

# df <- df %>%
#   select(-c(VL_RENDA_FAMILIAR_BRUTA_MENSAL, NU_ANO_CONCLUSAO_ENSINO_MEDIO, ST_DEFICIENCIA,
#             CO_FASE_BANCO))

# transformacoes para tentar melhorar o modelo

# df <- df %>%
#   rename(VL_RENDA_PERCAPITA = VL_RENDA_PERCAPTA) %>%
#   mutate(log_per_capita = log(VL_RENDA_PERCAPITA),
#          log_divida = log(VL_DIVIDA),
#          log_renda_familiar_bruta_mensal = log(VL_RENDA_FAMILIAR_BRUTA_MENSAL)) %>%
#   select(-c(VL_DIVIDA, VL_RENDA_PERCAPITA, VL_RENDA_FAMILIAR_BRUTA_MENSAL))
# 
# 
# df <- df %>%
#   select(-c(VL_RENDA_FAMILIAR_BRUTA_MENSAL, NU_ANO_CONCLUSAO_ENSINO_MEDIO, ST_DEFICIENCIA,
#             CO_FASE_BANCO))
# 
# df <- df %>%
#   select(-CO_FASE_BANCO)

# ==== plot
result_df <- test


set.seed(2019)
inspl = sample.split(result_df$loan_status, 0.05)
spl = subset(result_df, inspl == TRUE)

spl <- spl %>%
  rename(VL_RENDA_PERCAPITA = VL_RENDA_PERCAPTA) %>%
  mutate(pred_label = as.numeric(spl$predicted.risk >= 0.5))

# verificando representatividade
splTable = table(spl$loan_status, as.numeric(spl$predicted.risk >= 0.5))

overall_accuracy = (splTable[1,1] + splTable[2,2])/nrow(spl)
sensitivity = splTable[2,2]/sum(splTable[2,]) 
specificity = splTable[1,1]/sum(splTable[1,]) 
# ok

str(spl)


# colocando log da renda familiar e log da rend per capita

spl <- spl %>%
  mutate(log_renda_familiar = log(VL_RENDA_FAMILIAR_BRUTA_MENSAL),
         log_renda_percapita = log(VL_RENDA_PERCAPITA),
         log_divida = log(VL_DIVIDA))



library(plotly)
#plot_ly(color = I("blue"), showlegend = T) %>%
spl_plot1 <- plot_ly(showlegend = T) %>%
  add_markers(data = spl, x = ~idade, y = ~log_divida, z = ~log_renda_percapita, 
              marker=list(opacity=0.5), 
              split = ~factor(loan_status),
              size = I(3.5))

library(htmlwidgets)
library(plotly)
saveWidget(as_widget(spl_plot1), file = "/home/allan/Documents/r-projects/slide_NMQ/assets/widgets/plotly_scatter1.html")

spl_plot2 <- plot_ly(showlegend = T) %>%
  add_markers(data = spl, x = ~NU_SEMESTRE_FINANCIADO, y = ~log_divida, z = ~log_renda_percapita, 
              marker=list(opacity=0.5), 
              split = ~factor(loan_status),
              size = I(3.5))

library(htmlwidgets)
library(plotly)
saveWidget(as_widget(spl_plot2), file = "/home/allan/Documents/r-projects/slide_NMQ/assets/widgets/plotly_scatter2.html")

spl_plot <- plot_ly(showlegend = T) %>%
  add_bars(data = spl, x = ~NU_SEMESTRE_FINANCIADO, y = ~log_renda_percapita, 
              marker=list(opacity=0.5), 
              split = ~factor(loan_status),
              size = I(3.5))

# diagnostico
# problema com financiamentos de menor prazo
spl_plot <- plot_ly(showlegend = T) %>%
  add_boxplot(data = df, x = ~NU_SEMESTRE_FINANCIADO,
           marker=list(opacity=0.5), 
           split = ~factor(loan_status),
           size = I(3.5))

spl_plot <- plot_ly(showlegend = T) %>%
  add_histogram(data = df, x = ~idade, 
                marker=list(opacity=0.5), 
                split = ~factor(loan_status),
                size = I(3.5))

p <- plot_ly(data = spl, x = ~log_divida, y = ~log_renda_percapita,
             marker=list(opacity=0.5),
             split = ~factor(loan_status)
             )




spl_plot <- plot_ly(showlegend = T) %>%
  add_trace(data = df, x = ~SG_UF, 
                marker=list(opacity=0.5), 
                split = ~factor(loan_status),
                size = I(3.5))

  # add_paths(data = novo_d, x = ~V1, y = ~V2, z = ~V3, color = I("red"), visible="legendonly", name="MST_dados") %>%
  # add_paths(data = novo_d2, x = ~V1, y = ~V2, z = ~V3, color = I("red"), visible="legendonly", name="clust_dados") %>%
  # add_trace(x = simd[,1], y = simd[,2], z = simd[,3], marker=list(opacity=0.5), size = I(3.5),  
  #           visible="legendonly", name="hipercubo", color=I(plotly_palette[2])) %>%
  # add_paths(data = novo_d3, x = ~V1, y = ~V2, z = ~V3, color = I(plotly_palette[5]), visible="legendonly", name="MST_cubo") %>%
  # add_paths(data = novo_d4, x = ~V1, y = ~V2, z = ~V3, color = I(plotly_palette[5]), visible="legendonly", name="clust_cubo")




test2 = subset(df, spl2 == FALSE)


library(caret)

# partition
set.seed(3033)
intrain <- createDataPartition(y = df$loan_status, p= 0.7, list = FALSE)
training <- df[intrain,]
testing <- df[-intrain,]



library(caTools)

set.seed(1984)
spl2 = sample.split(df$loan_status, 0.7)
train2 = subset(df, spl2 == TRUE)
test2 = subset(df, spl2 == FALSE)


modLog2 = glm(loan_status ~ . , data=train2, family=binomial(link = "logit"))
summary(modLog2)


# accuracy

test2$predicted.risk = predict(modLog2, newdata=test2, type="response")

table(test2$loan_status, as.numeric(test2$predicted.risk >= 0.5))




###======================= com SVM
install.packages("e1071")
library(e1071)

x <- subset(df, select=-df$loan_status)
y <- df$loan_status

svm_model <- svm(loan_status ~ ., data=df)
summary(svm_model)


## SVM 2
install.packages("caret")

library(caret)

# partition
set.seed(3033)
intrain <- createDataPartition(y = df$loan_status, p= 0.7, list = FALSE)
training <- df[intrain,]
testing <- df[-intrain,]

dim(training); dim(testing);

# target variable to factor
training$loan_status = factor(training$loan_status)


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

str(training)

set.seed(3233)
svm_Linear <- train(loan_status ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)


svm_Linear

test_pred <- predict(svm_Linear, newdata = testing)

confusionMatrix(test_pred, testing$V14 )


#======================= NAIVE BAYES

library(caret)



set.seed(7267166)
set.seed(3033)
intrain <- createDataPartition(y = df$loan_status, p= 0.7, list = FALSE)
train <- df[intrain,]
test <- df[-intrain,]

dim(train); dim(test);

str(train)

# target variable to factor
train$loan_status = factor(train$loan_status)
test$loan_status = factor(test$loan_status)
library(e1071)

NBclassfier=naiveBayes(loan_status~., data=train)
print(NBclassfier)

testPred=predict(NBclassfier, newdata = test, type = "class")
testTable=table(test$loan_status, testPred); testTable




overall_accuracy = (testTable[1,1] + testTable[2,2])/nrow(test)
sum(trainTable)
sensitivity = testTable[1,1]/sum(testTable[1,]) 
specificity = testTable[2,2]/sum(testTable[2,]) 


test$predicted.risk = predict(NBclassfier, newdata=test, type="response")

table(test2$loan_status, as.numeric(test2$predicted.risk >= 0.5))