#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
library(ggcorrplot)
library(quantmod)
library(DT)





# Define UI for application that draws a histogram
ui <- dashboardPage(skin = 'black', 
    dashboardHeader(title = "test"),
    dashboardSidebar(
        
        sidebarMenu(
            menuItem("Top Insider Trades", tabName = "InsTrades", icon = icon("dashboard")),
            menuItem("Whale SS", tabName = "Whale", icon = icon("dashboard")),
            menuItem("Correlations", tabName = "Correlations", icon = icon("dashboard"))
            
            
            
    )),
    dashboardBody(
        tabItems(
           
            tabItem("InsTrades",fluidPage(
                h1("Top Insider Trades"),
                dataTableOutput("InsTrade"),
                box(plotOutput("ratioplot"), width = 6),
                box(plotOutput("ratioplot2"), width = 6),
                box(plotOutput("ratioplot3"), width = 6),
                box(tableOutput("tableone"), width = 6)
                
            )
            ),
            tabItem("Whale",fluidPage(
                h1("Whale Data"),
                dataTableOutput("WhaleData")
            )),
            tabItem("Correlations",fluidPage(
                h1("Stock Correlations"),
                sidebarPanel(textInput("stocklist", label = "stocklist",value = ""),
                             actionButton("Append", "Append", width = 80),
                             actionButton("Remove", "Remove",width = 80),
                             actionButton("Clear","Clear", width = 80),
                             textOutput("Stocklist")),
                
                box(plotOutput("Corplot"))))
        
    )
    
    
))

    



# Define server logic required to draw a histogram
server <- function(input, output) {
    data = read.csv("Ins_Trades.csv")
    data2 = read.csv("whale.csv")
  
    output$InsTrade = renderDataTable({data})
    output$ratioplot = renderPlot({
        ggplot(data, aes(x = data$Transaction))+geom_bar(color="blue", fill ="orange")
    })
    output$ratioplot2 = renderPlot({
        ggplot(data, aes(x = Transaction, y = log(NumShares ),))+ geom_boxplot()+facet_wrap(~Transaction, scales = 'free')+
            scale_fill_brewer(palette = "Dark2")
                                                                                   
    })
    output$ratioplot3 = renderPlot({
        ggplot(data, aes(x = Transaction, y = log(ValueDollar) ),)+ geom_boxplot()+
            scale_fill_brewer(palette = "Dark2")

    })
    output$tableone = renderTable({
        unsorted_table = table(data$Ticker )
        sorted_table = sort(unsorted_table, decreasing = TRUE)
        sorted_table[1:10]
        
        
    })
    
    output$WhaleData = renderDataTable({data2})
    output$Corplot = renderPlot({
        tickerstest = c("NNDM", "PSFE", "THCB", "ZNGA", "MARA", 'SLB', 'HBAN', 'VALE', 'KDP','HEXO')
        ClosePrices = function(tickers, date){
            portfolioPrices  = NULL
            for (ticker in tickers){
                price = getSymbols.yahoo(ticker, from = date, auto.assign = FALSE)[,4]
                portfolioPrices=cbind(portfolioPrices,price)
            }
            colnames(portfolioPrices) = tickers
            return(na.omit(portfolioPrices))
        }
        work = ClosePrices(tickerstest, '2016-01-01') 
        cor_work = cor(work)
        
        ggcorrplot(cor_work,ggtheme = ggplot2::theme_void, title = "Portfolio Correlation Chart",
                   colors = c("tomato2", "white", "springgreen3"), type = 'lower',hc.order = TRUE,
                   lab = TRUE, lab_size = 3, insig = 'blank')
        
        
    })
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)

