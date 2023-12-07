
# Instalar SQLite
library(RSQLite)
library(dplyr)
library(tidyr)
library(maps)
library(shiny)
library(shinythemes)
library(tidyverse)


# Remover o banco SQLite, caso exista - Não é obrigatório
system("del exemplo.db") # --> no Windows
# system("rm exemplo.db")    # --> no Mac e Linux

# Criando driver e conexão ao banco de dados
drv = dbDriver("SQLite")
con = dbConnect(drv, dbname = "mental_health.sqlite")

dbListTables(con)

answer <- dbReadTable(con, "Answer")

question <- dbReadTable(con, "Question")

survey <- dbReadTable(con, "Survey")

query <- "SELECT answer.*, question.questiontext  FROM
answer JOIN question ON
answer.QuestionID = question.questionid
"

tab_join <- dbGetQuery(conn = con, statement = query)



df <- tab_join %>%
  mutate(
    AnswerText = ifelse(AnswerText %in% c("-1", "0", "1"),
                        ifelse(AnswerText == "-1", "Null",
                               ifelse(AnswerText == "0", "No", "Yes")),
                        AnswerText)
  )



df <- df[df$QuestionID %in% c(1:3, 5:30), ]

df_resultado <- data.frame() 
for (i in unique(df$SurveyID)){
  df_temp <- df %>% 
    filter(SurveyID == i)
    sampled_ids <- sample(df_temp$UserID, 300, replace = FALSE)
    df_resultado <- bind_rows(df_resultado, df_temp %>% filter(UserID %in% sampled_ids))
}

df <- df_resultado

df <- df %>%
  mutate(AnswerText = ifelse(AnswerText == "Male", "male", ifelse(AnswerText == "Female", "female", AnswerText)))

df <- df %>%
  mutate(AnswerText = case_when(
    grepl("United States", AnswerText, ignore.case = TRUE) ~ "USA",
    TRUE ~ AnswerText
  ))



df %>% 
  filter(QuestionID == 3) %>% 
  select(AnswerText) %>% 
  unique()

perguntas <- c(
  "Qual é a sua idade?",
  "Qual é o seu gênero?",
  "Em qual país você mora?",
  "abacate",
  "Você é autônomo?",
  "Você tem histórico familiar de doenças mentais?",
  "Você já procurou tratamento para um transtorno de saúde mental com um profissional de saúde mental?",
  "Quantos funcionários sua empresa ou organização possui?",
  "Seu empregador é principalmente uma empresa/organização de tecnologia?",
  "Seu empregador oferece benefícios de saúde mental como parte da cobertura de saúde?",
  "Sua anonimidade é protegida se você optar por aproveitar os recursos de tratamento de saúde mental ou abuso de substâncias fornecidos pelo seu empregador?",
  "Você mencionaria um problema de saúde mental com um potencial empregador em uma entrevista?",
  "Você estaria disposto a discutir um problema de saúde mental com seus supervisores diretos?",
  "Você mencionaria um problema de saúde física com um potencial empregador em uma entrevista?",
  "Você ouviu falar ou observou consequências negativas para colegas com condições de saúde mental em seu local de trabalho?",
  "Quaisquer notas ou comentários adicionais",
  "Seu papel principal em sua empresa está relacionado à tecnologia/TI?",
  "Você conhece as opções de cuidados com a saúde mental disponíveis sob a cobertura de saúde fornecida pelo seu empregador?",
  "Seu empregador já discutiu formalmente a saúde mental (por exemplo, como parte de uma campanha de bem-estar ou outra comunicação oficial)?",
  "Seu empregador oferece recursos para aprender mais sobre transtornos de saúde mental e opções de busca de ajuda?",
  "Se um problema de saúde mental o levasse a solicitar uma licença médica do trabalho, quão fácil ou difícil seria pedir essa licença?",
  "Você se sentiria confortável discutindo um problema de saúde mental com seus colegas?",
  "Você se sentiria confortável discutindo um problema de saúde mental com seus supervisores diretos?",
  "Você possui cobertura médica (seguro privado ou fornecido pelo estado) que inclui tratamento para transtornos de saúde mental?",
  "Você conhece recursos locais ou online para procurar ajuda para um problema de saúde mental?",
  "Você teve empregadores anteriores?",
  "Seus empregadores anteriores ofereceram benefícios de saúde mental?",
  "Você estava ciente das opções de cuidados com a saúde mental fornecidas por seus empregadores anteriores?",
  "Seus empregadores anteriores discutiram formalmente a saúde mental (como parte de uma campanha de bem-estar ou outra comunicação oficial)?",
  "Seus empregadores anteriores forneceram recursos para aprender mais sobre transtornos de saúde mental e como buscar ajuda?",
  "Sua anonimidade foi protegida se você optou por aproveitar os recursos de tratamento de saúde mental ou abuso de substâncias com empregadores anteriores?",
  "Você teria estado disposto a discutir sua saúde mental com seus supervisores diretos anteriores?",
  "Você estaria disposto a mencionar um problema de saúde física a um potencial empregador em uma entrevista?",
  "Quão disposto você estaria a compartilhar com amigos e familiares que você tem uma doença mental?",
  "Você acha que discutir um transtorno de saúde mental com seu empregador teria consequências negativas?",
  "Você ouviu falar ou observou consequências negativas para colegas que foram abertos sobre questões de saúde mental em seu local de trabalho?"
)

for (i in unique(df$QuestionID)) {
  df$questiontext[df$QuestionID == i] <- perguntas[i]
}

for (i in unique(df$QuestionID)) {
  df$questiontext[df$QuestionID == i] <- perguntas[i]
}


# Função para identificar e remover outliers usando o método IQR
remove_outliers <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  lower <- q[1] - 1.5 * iqr
  upper <- q[2] + 1.5 * iqr
  return(between(x, lower, upper))
}



world_map <- map_data("world")




# Definir UI
ui <- fluidPage(
  titlePanel("Respostas das pesquisas sobre saúde mental"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pergunta", "Escolha uma pergunta:", choices = unique(df$QuestionID)[order(unique(df$QuestionID))]),
      selectInput("ano", "Escolha o ano:", choices = df$SurveyID)
    ),
    
    mainPanel(
      textOutput("texto_dinamico"),
      plotOutput("plot")
    )
  )
)

# Definir Server
server <- function(input, output) {
  
  output$texto_dinamico <- renderText({
    dados_filtrados <- subset(df, QuestionID == input$pergunta & SurveyID == input$ano)
    paste(unique(dados_filtrados$questiontext))
  })
  
  output$plot <- renderPlot({
    # Filtrar dados para a pergunta selecionada
    dados_filtrados <- subset(df, QuestionID == input$pergunta & SurveyID == input$ano)
    
    # Verificar se a resposta é numérica ou categórica
    if (input$pergunta == "1") {
      # Histograma para respostas numéricas
      ggplot(dados_filtrados, aes(x = as.numeric(AnswerText))) +
        geom_histogram(fill = "skyblue", color = "black", bins = 30) +
        labs(x = "Idade", y = "Frequência", title = "Idade dos entrevistados") +
        theme(legend.title=element_text(color="black",
                                        face="bold"),
              plot.title= element_text(size=14,
                                       color="black",
                                       face="bold"),
              axis.title.x = element_text(face="bold"),
              axis.title.y = element_text(face="bold"))
    } else if (input$pergunta == "3") {
      # Histograma para respostas numéricas
      frequencia_paises <- dados_filtrados %>% 
        filter(QuestionID == 3) %>% 
        count(AnswerText)
      names(frequencia_paises)[1] <- "region"
      world_map <- left_join(world_map, frequencia_paises, by = "region")
      ggplot(world_map , aes(x = long, y = lat, group=group, text = paste('Pais:', region, '<br>Pontos totais: ', n))) +
        geom_polygon(aes(fill = n), color = "black") +
        scale_fill_gradient(name = "Valor da media", low = "#0099E6", high =  "#0000FF", na.value = "grey50") +
        labs(x = "Longitude", y = "Latitude", title = "Media do resultado final de cada pais em mapa") +
        theme(legend.title=element_text(color="black",
                                        face="bold"),
              plot.title= element_text(size=14,
                                       color="black",
                                       face="bold"),
              axis.title.x = element_text(face="bold"),
              axis.title.y = element_text(face="bold"))
    }
    
    else {
      # Gráfico de barras para respostas categóricas
      ggplot(dados_filtrados, aes(x = AnswerText)) +
        geom_bar(fill = "skyblue", color = "black") +
        labs(x = "Resposta", y = "Frequência", title = "Frequência de respostas") +
        theme(legend.title=element_text(color="black",
                                        face="bold"),
              plot.title= element_text(size=14,
                                       color="black",
                                       face="bold"),
              axis.title.x = element_text(face="bold"),
              axis.title.y = element_text(face="bold"))
    }
  }
  )
}

# Rodar aplicativo Shiny
shinyApp(ui = ui, server = server)