library(shiny)
require(jiebaR)
library(RColorBrewer)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(stringr)
library(ggplot2)
library(wordcloud)
library(showtext)
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Line Message Analysis"),
    span('This idea is from https://line-message-analyzer.netlify.app/ and my friend, Joy.'),
    # Navigation Bar
    navbarPage("Menu",
               tabPanel("Summary",
                        sidebarLayout(
                            sidebarPanel(
                                fileInput('File', 'Line Messages txt File'),
                                br(),
                                h5("The First Day of Conversion : "),
                                textOutput('First_day'),
                                br(),
                                h5("It has been"),
                                textOutput('Days_ago'),
                                br(),
                                h5("Total Days of Chat:"),
                                textOutput('Total_chat_days'),
                                br(),
                            ),
                            mainPanel(
                                plotOutput('Text_cloud')
                            )
                        )
               ),
               
               tabPanel("Call",
                        sidebarLayout(
                            sidebarPanel(
                                h5("Total Numbers of Calls:"),
                                textOutput('Total_calls'),
                                br(),
                                h5("Total Time of Calls"),
                                textOutput('Total_time_calls'),
                                br(),
                                h5("Avg.Time Per Call:"),
                                textOutput('Avg_call_time'),
                            ),
                            mainPanel(
                                h4("Who initiated calls"),
                                plotOutput('Caller')
                            )
                        )               
               ),
               tabPanel("Text",
                        sidebarLayout(
                            sidebarPanel(
                                h5("Total Numbers of Text Messages:"),
                                textOutput('Total_texts'),
                                br(),
                                h5("Avg. Numbers of Text Messages Per Day:"),
                                textOutput('Avg_texts_day'),
                                br(),
                                h5("Total Words of Messages:"),
                                textOutput('Total_words'),
                                br(),
                                h5("Avg.Words Per Message:"),
                                textOutput('Avg_words_text'),
                                br(),
                                radioButtons('Time_type', 'Time Format', c('Weekday' = 'weekday', 'Hour' = 'hour')),
                                selectInput('Word_metrics', "Compare Senders' Texts :", 
                                            c('Total nums of messages' = 'Total_num_of_messages', 'Avg. nums of messages per day' = 'Avg_num_of_messages_per_day',
                                              'Total nums of words'='Total_num_of_words', 'Avg. nums of words per message'='Avg_num_of_words_per_message'))
                            ),
                            mainPanel(
                                fluidRow(
                                    splitLayout(cellWidths = c('50%', '50%'),
                                                plotOutput("Day_messages"),
                                                plotOutput('Text_messages_sender'))
                                )
                            )
                        )               
               ))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data <- reactive({
        if(is.null(input$File))return(NULL)
        data <- read.table(input$File$datapath, sep='\n', skip=2)
        Date_pos <- grep('[0-9]{4}[/|][0-9]{2}/[0-9]{2}', data[,])
        Date_index <- as.matrix(data[grep('[0-9]{4}/[0-9]{2}/[0-9]{2}', data[,]),])
        Date_last_index <- rep(Date_index[length(Date_index),], (nrow(data)-Date_pos[length(Date_pos)]))
        Date <- as.matrix(mapply(rep, Date_index[-length(Date_index),], ((Date_pos-lag(Date_pos))[-1]-1)))
        Date <- as.matrix(unlist(Date))
        Date <- rbind(Date, Date_last_index)
        Chat_index <- as.matrix(data[-grep('[0-9]{4}/[0-9]{2}/[0-9]{2}', data[,]),])
        Chat_index<-do.call(rbind, strsplit(Chat_index, '\t' ))
        Chat<-cbind(Date, Chat_index)
        colnames(Chat) <- c('Date', 'Time', 'Sender', 'Message')
        Chat <- Chat[grepl('[0-9]{2}:[0-9]{2}', Chat[, 'Time']), ]
        Chat
    })
    
    #第一次開始聊天 （a. 日期 b. 距現在多久 c. 共聊天幾天）
    output$First_day <- renderText({
        Chat <- data()
        Chat[1,'Date']})
    output$Days_ago <- renderText({
        Chat <- data()
        if(is.null(Chat[1, 'Date']))return(NULL)
        paste(floor(Sys.time() - as.POSIXct(Chat[1, 'Date'], format = "%Y/%m/%d", "days  ago")), 'days ago')
    })
    output$Total_chat_days <- renderText({
        Chat <- data()
        length(unique(Chat[,'Date']))})
    
    output$Text_cloud <- renderPlot({
        Chat <- data()
        if(is.null(Chat))return(NULL)
        options(encoding = 'UTF-8')
        content <- as.character(Chat[, 'Message'])
        content <- str_remove_all(content, '[0-9]+?')
        stop_words <- c("在","的","下","個","來","與","或", "到", "做", "有", "但", "也", "你", "了","嗎","我","跟","呢","喔")
        writeLines(stop_words, 'stop_words.txt')
        cutter <- worker(stop_word = 'stop_words.txt', bylines = FALSE)
        words <- cutter[content]
        word_freq <- freq(words)
        par(family=("STFangsong"))
        par(mar = rep(0, 4))
        showtext_auto()
        wordcloud(word_freq$char, word_freq$freq, min.freq = 2, colors = rainbow(nrow(word_freq)), scale=c(4,4))
        
    })
    
    #通話 - (a. 總次數 b. 總長 c. 平均每次通話時間）
    output$Total_calls <- renderText({
        Chat <- data()
        length(grep('Call time', Chat[,'Message']))
    })
    
    output$Total_time_calls <- renderText({
        Chat <- data()
        if(is.null(Chat))return(NULL)
        Call <- Chat[grep('Call time', Chat[,'Message']), 'Message']
        Call_second <- sum(as.period(ms(lapply(strsplit(Call, split=' '), '[[', 4)), unit='second'))
        paste(Call_second %/% (60*60), 'h', Call_second %/% 60, 'min',  Call_second %% 60, 'sec')
    })
    
    output$Avg_call_time <- renderText({
        Chat <- data()
        if(is.null(Chat))return(NULL)
        Call <- Chat[grep('Call time', Chat[,'Message']), 'Message']
        Call_second <- sum(as.period(ms(lapply(strsplit(Call, split=' '), '[[', 4)), unit='second'))
        Avg_call_second <- Call_second / length(grep('Call time', Chat[,'Message']))
        paste(Avg_call_second %/% (60*60), 'h', Avg_call_second %/% 60, 'min',  Avg_call_second %% 60, 'sec')
        
    })
    
    #主動發起call
    output$Caller <- renderPlot({
        Chat <- data()
        if(is.null(Chat))return(NULL)
        Call_sender <- Chat[grep('Call time', Chat[,'Message']),'Sender']
        Call_sender <- as.data.frame(Call_sender)
        colnames(Call_sender) <- 'Sender'
        Call_sender %>% group_by(Sender) %>%
            ggplot(., aes(x='', fill=Sender)) + geom_bar(stat= 'count', position = 'stack') + coord_polar("y", start=0)
        
    })
    
    #文字訊息 - (a. 總則數  b. 平均每日則數c. 總字數 d. 平均每則字數)
    text_message <- reactive({
        Chat <- data()
        Text <- Chat[-grep('☎|\\[File]|\\[Sticker]|\\[Notes]|\\[Photo]', Chat[,'Message']), ]
        Text
    })
    
    output$Total_texts <- renderText({
        Text <- text_message()
        nrow(Text)
    })
    
    output$Avg_texts_day <- renderText({
        Chat <- data()
        Text <- text_message()
        nrow(Text)/length(unique(Chat[,'Date']))
    })
    
    output$Total_words <- renderText({
        Text <- text_message()
        sum(nchar(as.character(Text[, 'Message'])))
    })
    
    output$Avg_words_text <- renderText({
        Text <- text_message()
        sum(nchar(as.character(Text[, 'Message']))) / nrow(Text)
    })
    
    #都在幾點聊天 #都在星期幾聊天
    output$Day_messages <- renderPlot({
        Chat <- data()
        if(is.null(Chat[, 'Date']))return(NULL)
        if(input$Time_type == 'weekday'){
            Chat <- cbind(Chat, unlist(lapply(strsplit(as.character(Chat[, 'Date']), split=','), '[[', 2)))
            colnames(Chat)[5] <- 'Weekday'
            Chat <- as.data.frame(Chat)
            Chat %>% group_by(Sender, Weekday) %>%
                ggplot(., aes(x=Weekday, fill=Sender)) + geom_bar(stat='count', position = 'stack')
        }else{
            Sys.setlocale("LC_TIME","en_GB.UTF-8")
            Chat[,'Time'] <- format(strptime(Chat[,'Time'], format='%I:%M %p'), format='%H:%M')
            Chat <- cbind(Chat, unlist(lapply(strsplit(Chat[, 'Time'], split=':'), '[[', 1)))
            colnames(Chat)[5] <- 'Hour'
            Chat <- as.data.frame(Chat)
            Chat %>% group_by(Sender, Hour) %>%
                ggplot(., aes(x=Hour, fill=Sender)) + geom_bar(stat='count', position = 'stack')
        }
    })
    
    #兩人傳送文字訊息 - (a. 總則數  b. 平均每日則數c. 總字數 d. 平均每則字數)
    output$Text_messages_sender <- renderPlot({
        Text <- text_message()
        if(is.null(Text))return(NULL)
        Text <- as.data.frame(Text)
        if(input$Word_metrics == 'Total_num_of_messages'){
            Text %>% group_by(Sender) %>%
                ggplot(., aes(x=Sender, fill=Sender))+geom_bar(stat='count', position = 'dodge')
            
        }else if (input$Word_metrics == 'Avg_num_of_messages_per_day'){
            Text %>% group_by(Sender) %>% summarise(
                Messages_per_day = n() / n_distinct(Date)
            )%>%
                ggplot(., aes(x=Sender, y=Messages_per_day, fill=Sender))+geom_bar(stat='identity', position='dodge')
        }else if(input$Word_metrics == 'Total_num_of_words'){
            Text %>% group_by(Sender) %>% summarise(Total_char = sum(nchar(as.character(Message))))%>% 
                ggplot(., aes(x= Sender, y= Total_char, fill=Sender))+ geom_bar(stat='identity')
        }else if(input$Word_metrics == 'Avg_num_of_words_per_message'){
            Text %>% group_by(Sender) %>% summarise(Mean_char = mean(nchar(as.character(Message)))) %>% 
                ggplot(., aes(x= Sender, y= Mean_char, fill=Sender))+ geom_bar(stat='identity')
        }
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
