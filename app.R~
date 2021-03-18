library(rsconnect)
library(forecast)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(knitr)
library(rmarkdown)
library(tidyverse)
library(shiny)
library(htmlwidgets)
library(formattable)
library(digest)
library(bit)
library(plotly)
library(flexdashboard)
library(shiny)
library(shinydashboard)
library(datasets)
library(quantmod)
library(XML)
library(xml2)
library(XML2R)
library(curl)
library(Hmisc)
library(foreign)
library(zip)
library(rvest)
library(tidyverse)
library(RCurl)
library(jsonlite)
library(lubridate)
library(ggparliament)
library(httpuv)
library(tm)
library(wordcloud2)
library(wordcloud)
library(siebanxicor)
library(highcharter)
library(DT)
library(readxl)
library(XML)
library(xml2)
library(XML2R)
library(dplyr)
library(curl)
library(Hmisc)
library(foreign)
library(zip)
library(rvest)
library(tidyverse)
library(RCurl)
library(jsonlite)
library(lubridate)
library(ggparliament)
library(rtweet)
library(twitteR)
library(httpuv)
library(tm)
library(siebanxicor)
library(RColorBrewer)



resumen_semanas2 <- 
    read_excel("resumen_semanas.xlsx", 
                               col_types = c("text", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric"))
ganancias_mensuales <-
    read_excel("ganancias_mensuales.xlsx", 
               col_types = c("text", "numeric", "numeric",
                             "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
ganancias_mensuales2<-unite(ganancias_mensuales,variables,c('mes','año'),sep=' ',remove = T)

ventas_dia <- 
    read_excel("ventas_dia.xlsx", 
                         col_types = c("date", "text", "text", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric","numeric","numeric"))

resumen_ventas_dia1<-ventas_dia[,c('normal_19',	'alcalino_19',	'normal_11',	'alcalina_11',	'botella_medio',	'botella_litro')]

colores <- c('rgb(0,96,100)','rgb(0,131,143)','rgb(77,208,225)','rgb(128,222,234)','rgb(255,224,130)','rgb(255,213,79)')

ganancias_mensuales_m <-
    read_excel("ganancias_mensuales_m.xlsx", 
               col_types = c("text", "numeric", "numeric","numeric"))
ganancias_mensuales_m2<-ganancias_mensuales_m[-1]
ganancias_mensuales_m3<-ganancias_mensuales_m2
mes<-10
ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title='Santa Elena'),
    dashboardSidebar(
        sidebarMenu(
            menuItem('General',tabName = 'General',icon=icon('fas fa-chart-line')),
            menuItem('Mensual',tabName = 'Mensual',icon=icon('far fa-calendar-check')),
            menuItem('Semanal',tabName = 'Semanal',icon=icon('fas fa-calendar-week')),
            menuItem('Productos',tabName = 'productos',icon=icon('fas fa-tint'))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem('General',
                    fluidPage(
                        box(
                            selectInput('desde','Analizar desde:',
                                        c('2018','2019','2020'),selected = '2018'),width='100%'
                        ),
                        fluidRow(
                            infoBoxOutput("prom_anual"),
                            infoBoxOutput("prom_mensual"),
                            infoBoxOutput("tasa_año_pasado")
                        ),
                        box(plotlyOutput("historico_ventas"),width='100%'),
                        fluidRow(
                            box(plotlyOutput('grafica_pie')),
                            box(plotlyOutput('grafica_barras_año'))
                        ),
                        box(plotlyOutput('grafico_meses_año'),width='100%')
                        
                )             
            ),
           
            tabItem('Mensual',
                    fluidPage(
                        
                    box(
                        selectInput('desde_mensual','Últimos:',
                                    c('4 meses','8 meses', '12 meses'),selected='4 meses'), width='100%'
                    ),
                    fluidRow(
                        infoBoxOutput("KPI1"),
                        infoBoxOutput("prom_mensual_2"),
                        infoBoxOutput("incremento_mensual")
                    ),
                    box(plotlyOutput('historico_mes'),width='100%'
                        ),
                    box(dataTableOutput('resumen_mes'),width='100%'
                        )
                    )),
            tabItem('Semanal',
                    fluidPage(
                        box(
                            selectInput('desde_semanal','Últimas:',
                                        c('4 semanas','8 semanas', '12 semanas'),selected='4 semanas'), width='100%'
                        ),
                        fluidRow(
                            infoBoxOutput("KPI2"),
                            infoBoxOutput("prom_semanal"),
                            infoBoxOutput("incremento_ventas")
                        ),
                        
                        box(plotlyOutput('historico_semanal'),width='100%'),
                        box(plotlyOutput('productos_semanal'),width='100%'),
                        box(dataTableOutput('datatable_productos'),width='100%')
                    )),
            tabItem('productos',
                    fluidPage(
                        box(
                            selectInput('select_product','Seleccione un producto:',
                                        c('Garrafón 19 L','Garrafón alcalino 19 L','Garrafón 11L','Garrafón alcalino 11L','Botella medio litro','Botella 1L'), selected = 'Garrafón 19L'),width='100%'
                        ),
                        box(
                            selectInput('dia_product','Seleccione un día: ',
                                        c('Lunes','Martes','Miércoles','Jueves','Viernes','Sábado','Domingo'), selected = 'Lunes'),width='100%'
                        ),
                        box(plotlyOutput('g_comportamiento'),width='100%'),
                        box(plotlyOutput('grafico_cajas'),width = '100%'),
                        box(dataTableOutput('tabla_predicciones'),width='100%')
                    )
            )
            
        )
    )
)

server <- function(input, output) {
    output$prom_anual<-renderInfoBox({
        if(input$desde == '2018'){
            
        }else if (input$desde == '2019'){
            ganancias_mensuales_m3<-ganancias_mensuales_m2[,c('2019','2020')]
        }else if(input$desde == '2020'){
            ganancias_mensuales_m3<-ganancias_mensuales_m2[,c('2020')]
        }
        ganancias_anuales<-colSums(ganancias_mensuales_m3,na.rm=T)
        p1<-round(mean(ganancias_anuales,na.rm = T),2)
        infoBox("Promedio ventas anual",paste('$',round(p1,2)), icon=icon('fas fa-dollar-sign'), color = "black")
        
        
    })
    
    output$prom_mensual<-renderInfoBox({
        if(input$desde == '2018'){
            
        }else if (input$desde == '2019'){
            ganancias_mensuales2<-ganancias_mensuales[10:nrow(ganancias_mensuales),]  
        }else if(input$desde == '2020'){
            ganancias_mensuales2<-ganancias_mensuales[22:nrow(ganancias_mensuales),]
        }
        prom_mensual2<-round(mean(ganancias_mensuales2$ganancia),2)
        infoBox("Promedio ventas mensual",value=paste('$',prom_mensual2) , icon=icon('fas fa-money-check-alt'), color = "black")
    })
    
    output$tasa_año_pasado<-renderInfoBox({
        tabla_año_actual<-ganancias_mensuales_m[1:mes,c('2019','2020')]
        tasa_año_p<-colSums(tabla_año_actual)
        dif<-tasa_año_p[2]-tasa_año_p[1]
        if(dif<0){
            infoBox("Ventas al año pasado",paste('-$',-dif), icon=icon('fas fa-arrow-down'),color='red')
        }else{
            infoBox("Ventas al año pasado",paste('+$',dif), icon=icon('fas fa-arrow-up'),color='green')
        }
        
    })
    
    output$historico_ventas<-renderPlotly({
        if(input$desde == '2018'){
            resumen_semanas2<-resumen_semanas2
        }else if(input$desde == '2019'){
            resumen_semanas2<-resumen_semanas2[39:nrow(resumen_semanas2),]
        }else if(input$desde == '2020'){
            resumen_semanas2<-resumen_semanas2[91:nrow(resumen_semanas2),]
        }
        g_resumen_semanal<-plot_ly(resumen_semanas2,x=~semana,y=~total,type='scatter',mode='lines') %>% 
            layout(title='Ventas Totales',
                   xaxis=list(title='Semanas',
                              categoryorder='array',
                              categoryarray=resumen_semanas2$semana),
                   yaxis=list(title='Ventas'))
        g_resumen_semanal

    })
    
    output$grafica_pie<-renderPlotly({
        if(input$desde == '2018'){
            resumen_ventas_dia<-data.frame(colSums(resumen_ventas_dia1))
            
        }else if(input$desde == '2019'){
            resumen_ventas_dia2<-resumen_ventas_dia1[268:nrow(resumen_ventas_dia1),]
            resumen_ventas_dia<-data.frame(colSums(resumen_ventas_dia2))

        }else if(input$desde == '2020'){
            resumen_ventas_dia2<-resumen_ventas_dia1[633:nrow(resumen_ventas_dia1),]
            resumen_ventas_dia<-data.frame(colSums(resumen_ventas_dia2))
        }
        colnames(resumen_ventas_dia)<-c('total')
        rownames(resumen_ventas_dia)<-c('Garrafón 19L','Garrafón alcalino 19L','Garrafón 11L','Garrafón alcalino 11L','Botella medio litro','Botella litro')
        grafica_pie_productos<-plot_ly(resumen_ventas_dia,type='pie',labels=rownames(resumen_ventas_dia),values=resumen_ventas_dia$total,textposition='inside',marker=list(colors=colores)) %>% 
            layout(title='Productos',uniformtext=list(minsize=15,mode='hide'))
        grafica_pie_productos
    })
    output$grafica_barras_año<-renderPlotly({
        if(input$desde == '2018'){
            ganancias_mensuales_m3<-ganancias_mensuales_m2
        }else if(input$desde == '2019'){
            ganancias_mensuales_m3<-ganancias_mensuales_m2[,c('2019','2020')]
        }else if(input$desde == '2020'){
            ganancias_mensuales_m3<-ganancias_mensuales_m2[,c('2020')]
        }
        p1<-as.data.frame(colMeans(ganancias_mensuales_m3,na.rm = T))
        colnames(p1)<-c('promedio_ventas')
        graph_años<-plot_ly(p1,type='bar',x=rownames(p1),y=p1$promedio_ventas,text=p1$promedio_ventas, textposition = 'auto') %>% 
            layout(title='Ventas por año')
        graph_años
    })
    output$grafico_meses_año<-renderPlotly({
        if(input$desde == '2018'){
            ventas_mensuales<-plot_ly(ganancias_mensuales_m,type='bar',y=~mes,x=ganancias_mensuales_m$'2020',name='2020') %>% 
                add_trace(x=ganancias_mensuales_m$'2019',name='2019') %>% 
                add_trace(x=ganancias_mensuales_m$'2018',name='2018') %>% 
                layout(title='Ventas mensuales',
                       xaxis=list(title='Ventas'),
                       yaxis=list(title='Meses',
                                  categoryorder='array',
                                  categoryarray=c('Diciembre','Noviembre','Octubre','Septiembre','Agosto','Julio','Junio','Mayo','Abril','Marzo','Febrero','Enero')))
        }else if(input$desde == '2019'){
            ventas_mensuales<-plot_ly(ganancias_mensuales_m,type='bar',y=~mes,x=ganancias_mensuales_m$'2020',name='2020') %>% 
                add_trace(x=ganancias_mensuales_m$'2019',name='2019') %>% 
                layout(title='Ventas mensuales',
                       xaxis=list(title='Ventas'),
                       yaxis=list(title='Meses',
                                  categoryorder='array',
                                  categoryarray=c('Diciembre','Noviembre','Octubre','Septiembre','Agosto','Julio','Junio','Mayo','Abril','Marzo','Febrero','Enero')))
        }else if(input$desde == '2020'){
            ventas_mensuales<-plot_ly(ganancias_mensuales_m,type='bar',y=~mes,x=ganancias_mensuales_m$'2020',name='2020') %>% 
                layout(title='Ventas mensuales',
                       xaxis=list(title='Ventas'),
                       yaxis=list(title='Meses',
                                  categoryorder='array',
                                  categoryarray=c('Diciembre','Noviembre','Octubre','Septiembre','Agosto','Julio','Junio','Mayo','Abril','Marzo','Febrero','Enero')))
        }
        ventas_mensuales
    })
    output$KPI1<-renderInfoBox({
        if(input$desde_mensual == '4 meses'){
            kpi1<-1
        }else if (input$desde_mensual == '8 meses'){
            kpi1<-2
        }else if(input$desde_mensual == '12 meses'){
            kpi1<-3
        }  
        infoBox('KPI',kpi1,icon=icon('fas fa-check'),color='black')
    })
    output$prom_mensual_2<-renderInfoBox({
        if(input$desde_mensual == '4 meses'){
            inicio_filas_m<-nrow(ganancias_mensuales)-3
        }else if (input$desde_mensual == '8 meses'){
            inicio_filas_m<-nrow(ganancias_mensuales)-7
        }else if(input$desde_mensual == '12 meses'){
            inicio_filas_m<-nrow(ganancias_mensuales)-11
        }
        fin_filas_m<-nrow(ganancias_mensuales)
        datos_mensuales_m<-ganancias_mensuales[inicio_filas_m:fin_filas_m,]
        prom_mensual_m<-round(mean(datos_mensuales_m$ganancia),2)
        infoBox("Promedio ventas mensual",value=paste('$',prom_mensual_m) , icon=icon('fas fa-money-check-alt'), color = "black")
        
    })
    output$incremento_mensual<-renderInfoBox({
        if(input$desde_mensual == '4 meses'){
            inicio_filas_m<-nrow(ganancias_mensuales)-3
        }else if (input$desde_mensual == '8 meses'){
            inicio_filas_m<-nrow(ganancias_mensuales)-7
        }else if(input$desde_mensual == '12 meses'){
            inicio_filas_m<-nrow(ganancias_mensuales)-11
        }
        fin_filas_m<-nrow(ganancias_mensuales)-1
        datos_mensuales_m<-ganancias_mensuales[inicio_filas_m:fin_filas_m,]
        prom_mensual_m<-round(mean(datos_mensuales_m$ganancia),2)
        incremento_mensual_m<-ganancias_mensuales[nrow(ganancias_mensuales),c('ganancia')]-prom_mensual_m
        if(incremento_mensual_m<0){
            infoBox("Descenso Mensual",value=paste('-$',-incremento_mensual_m) , icon=icon('fas fa-arrow-down'), color = "red")
        }else{
            infoBox("Aumento Mensual",value=paste('+$',incremento_mensual_m) , icon=icon('fas fa-arrow-up'), color = "green")
        }
        
    })
    output$historico_mes<-renderPlotly({
        if(input$desde_mensual == '4 meses'){
            inicio_filas<-nrow(ganancias_mensuales2)-3
        }else if(input$desde_mensual == '8 meses'){
            inicio_filas<-nrow(ganancias_mensuales2)-7
        }else if(input$desde_mensual == '12 meses'){
            inicio_filas<-nrow(ganancias_mensuales2)-11
        }
        fin_filas<-nrow(ganancias_mensuales2)
        ganancias_mensuales3<-ganancias_mensuales2[inicio_filas:fin_filas,]
        graph_analisis_mensual<-plot_ly(ganancias_mensuales3,x=ganancias_mensuales3$variables,y=ganancias_mensuales3$ganancia,text = ganancias_mensuales3$ganancia, textposition = 'auto') %>% 
            layout(title='Ventas Mensuales',
                   xaxis=list(title='Meses',
                              categoryorder='array',
                              categoryarray=ganancias_mensuales3$variables),
                   yaxis=list(title='Ventas'))
        graph_analisis_mensual
    })
    output$resumen_mes<-renderDataTable({
        if(input$desde_mensual == '4 meses'){
            inicio_f<-nrow(ganancias_mensuales)-3
        }else if(input$desde_mensual == '8 meses'){
            inicio_f<-nrow(ganancias_mensuales)-7
        }else if(input$desde_mensual == '12 meses'){
            inicio_f<-nrow(ganancias_mensuales)-11
        }
        fin_f<-nrow(ganancias_mensuales)
        ganancias_mensuales_d<-ganancias_mensuales2[inicio_f:fin_f,c('variables','normal_19', 'alcalino_19', 'normal_11', 'alcalina_11', 'botella_medio','botella_litro')]
        ganancias_mensuales_d<-data.frame(ganancias_mensuales_d)
        rownames(ganancias_mensuales_d)<-ganancias_mensuales_d$variables
        ganancias_mensuales_d<-ganancias_mensuales_d[-1]
        colnames(ganancias_mensuales_d)<-c('Garrafón 19L','Garrafón alcalino 19L','Garrafón 11L','Garrafón alcalino 11L','Botella medio litro','Botella litro')
        formattable(ganancias_mensuales_d)
    })
    output$KPI2<-renderInfoBox({
        if(input$desde_mensual == '4 meses'){
            kpi2<-1
        }else if (input$desde_mensual == '8 meses'){
            kpi2<-2
        }else if(input$desde_mensual == '12 meses'){
            kpi2<-3
        }  
        infoBox('KPI',kpi2,icon=icon('fas fa-check'),color='black')
    })
    output$prom_semanal<-renderInfoBox({
        if(input$desde_semanal == '4 semanas'){
            inicio_filas5<-nrow(resumen_semanas2)-3
        }else if(input$desde_semanal == '8 semanas'){
            inicio_filas5<-nrow(resumen_semanas2)-7
        }else if(input$desde_semanal == '12 semanas'){
            inicio_filas5<-nrow(resumen_semanas2)-11
        }
        fin_filas5<-nrow(resumen_semanas2)
        resumen_semanas5<-resumen_semanas2[inicio_filas5:fin_filas5,]
        promedio_semanal_p2<-mean(resumen_semanas5$total)
        infoBox('Promedio ganancia semanal',paste('$',round(promedio_semanal_p2,2)), icon=icon('fas fa-dollar-sign'), color = "black")
        
    })
    output$incremento_ventas<-renderInfoBox({
        if(input$desde_semanal == '4 semanas'){
            inicio_filas4<-nrow(resumen_semanas2)-3
            fin_filas4<-nrow(resumen_semanas2)-1
        }else if(input$desde_semanal == '8 semanas'){
            inicio_filas4<-nrow(resumen_semanas2)-7
            fin_filas4<-nrow(resumen_semanas2)-1
        }else if(input$desde_semanal == '12 semanas'){
            inicio_filas4<-nrow(resumen_semanas2)-11
            fin_filas4<-nrow(resumen_semanas2)-1
        }
        
        resumen_semanas4<-resumen_semanas2[inicio_filas4:fin_filas4,]
        promedio_semanal_p<-round(mean(resumen_semanas4$total),2)
        ultima_ganancia<-resumen_semanas2[nrow(resumen_semanas2),'total']
        incremento<-ultima_ganancia-promedio_semanal_p
        if(incremento < 0){
            infoBox('Descenso semenal',paste('-$',-round(incremento,2)), icon=icon('fas fa-arrow-down'), color = "red")
        }else{
            infoBox('Aumento semanal',paste('+$',round(incremento,2)), icon=icon('fas fa-arrow-up'), color = "olive")
        }
    })
    output$historico_semanal<-renderPlotly({
        if(input$desde_semanal == '4 semanas'){
            inicio_filas2<-nrow(resumen_semanas2)-3
        }else if(input$desde_semanal == '8 semanas'){
            inicio_filas2<-nrow(resumen_semanas2)-7
        }else if(input$desde_semanal == '12 semanas'){
            inicio_filas2<-nrow(resumen_semanas2)-11
        }
        fin_filas2<-nrow(resumen_semanas2)
        resumen_semanas3<-resumen_semanas2[inicio_filas2:fin_filas2,]
        graph_semanas<-plot_ly(resumen_semanas3,type='bar',x=resumen_semanas3$semana,y=resumen_semanas3$total,text = resumen_semanas3$total, textposition = 'auto') %>% 
            layout(title='Ventas semanales',
                   xaxis=list(title='Semanas',
                              categoryorder='array',
                              categoryarray=resumen_semanas2$semana),
                   yaxis=list(title='Ventas'))
        graph_semanas
    })
    output$productos_semanal<-renderPlotly({
        if(input$desde_semanal == '4 semanas'){
            inicio_filas2<-nrow(resumen_semanas2)-3
        }else if(input$desde_semanal == '8 semanas'){
            inicio_filas2<-nrow(resumen_semanas2)-7
        }else if(input$desde_semanal == '12 semanas'){
            inicio_filas2<-nrow(resumen_semanas2)-11
        }
        fin_filas2<-nrow(resumen_semanas2)
        resumen_semanas3<-resumen_semanas2[inicio_filas2:fin_filas2,]
        productos_semanas<-plot_ly(resumen_semanas3,x=resumen_semanas3$semana,type='scatter',mode='lines+markers',y=resumen_semanas3$normal_19L,name='Garrafón 19L') %>% 
            add_trace(y=resumen_semanas3$alcalina_19L,mode='lines+markers',name='Garrafón alcalino 19L') %>% 
            add_trace(y=resumen_semanas3$normal_11L,mode='lines+markers',name='Garrafón 11 L') %>% 
            add_trace(y=resumen_semanas3$alcalina_11L,mode='lines+markers',name='Garrafón alcalino 11 L') %>%
            add_trace(y=resumen_semanas3$botella_medio,mode='lines+markers',name='Botella 1/2 L') %>%
            layout(title='Ventas por producto',
                   xaxis=list(title='Semanas',
                              categoryorder='array',
                              categoryarray=resumen_semanas3$semana),
                   yaxis=list(title='Unidades vendidas'))
        productos_semanas
    })
    output$datatable_productos<-renderDataTable({
        if(input$desde_semanal == '4 semanas'){
            inicio_filas2<-nrow(resumen_semanas2)-3

        }else if(input$desde_semanal == '8 semanas'){
            inicio_filas2<-nrow(resumen_semanas2)-7

        }else if(input$desde_semanal == '12 semanas'){
            inicio_filas2<-nrow(resumen_semanas2)-11
        }
        fin_filas2<-nrow(resumen_semanas2)
        resumen_semanas3<-resumen_semanas2[inicio_filas2:fin_filas2,]
        resumen_semanas4<-data.frame(resumen_semanas3[,c('normal_19L', 'alcalina_19L', 'normal_11L', 'alcalina_11L', 'botella_medio')])
        rownames(resumen_semanas4)<-resumen_semanas3$semana
        colnames(resumen_semanas4)<-c('Garrafón 19L','Garrafón alcalino 19L','Garrafón 11L','Garrafón alcalino 11L','Botella medio litro')
        formattable(resumen_semanas4)
    })
   
    output$g_comportamiento<-renderPlotly({
        inicio1<-nrow(ventas_dia)-140
        fin1<-nrow(ventas_dia)
        if(input$select_product == 'Garrafón 19 L'){
            ventas_dia_lm<-ventas_dia[inicio1:fin1,c('fecha','dia','normal_19')]
        }else if(input$select_product == 'Garrafón alcalino 19 L'){
            ventas_dia_lm<-ventas_dia[inicio1:fin1,c('fecha','dia','alcalino_19')]
        }else if(input$select_product == 'Garrafón 11L'){
            ventas_dia_lm<-ventas_dia[inicio1:fin1,c('fecha','dia','normal_11')]
        }else if(input$select_product == 'Garrafón alcalino 11L'){
            ventas_dia_lm<-ventas_dia[inicio1:fin1,c('fecha','dia','alcalina_11')]
        }else if(input$select_product == 'Botella medio litro'){
            ventas_dia_lm<-ventas_dia[inicio1:fin1,c('fecha','dia','botella_medio')]
        }else if(input$select_product == 'Botella 1L'){
            ventas_dia_lm<-ventas_dia[inicio1:fin1,c('fecha','dia','botella_litro')]      
        }
        
        
        ventas_dia_lm2<-ventas_dia_lm %>% 
            mutate_if(is.character,as.factor)
        
        if(input$dia_product == 'Lunes'){
            p_dia<-ventas_dia_lm2 %>% 
                filter(dia == 'Lunes')
        }else if(input$dia_product == 'Martes'){
            p_dia<-ventas_dia_lm2 %>% 
                filter(dia == 'Martes')
        }else if(input$dia_product == 'Miércoles'){
            p_dia<-ventas_dia_lm2 %>% 
                filter(dia == 'Miércoles')
        }else if(input$dia_product == 'Jueves'){
            p_dia<-ventas_dia_lm2 %>% 
                filter(dia == 'Jueves')
        }else if(input$dia_product == 'Viernes'){
            p_dia<-ventas_dia_lm2 %>% 
                filter(dia == 'Viernes')
        }else if(input$dia_product == 'Sábado'){
            p_dia<-ventas_dia_lm2 %>% 
                filter(dia == 'Sábado')
        }else if(input$dia_product == 'Domingo'){
            p_dia<-ventas_dia_lm2 %>% 
                filter(dia == 'Domingo')
        }
        filas<-nrow(p_dia)
        num_dia<-c(1:nrow(p_dia))
        cbind_p_dia<-cbind(num_dia,p_dia)
        colnames(cbind_p_dia)<-c('num_dia','fecha','dia','producto')
        filas_f<-filas+20
        data1<-c(1:filas_f)
        ts<-ts(cbind_p_dia$num_dia,start = 0,frequency=1)
        ts2<-ts(data1,start = 0, end=filas_f,frequency=1)
        
        cbind_p_dia2<-cbind(ts,cbind_p_dia)
        colnames(cbind_p_dia2)<-c('ts','num_dia','fecha','dia','producto')
        
        fit.cons<-tslm(producto~ts,data = cbind_p_dia2)
        fcast<-forecast(fit.cons,newdata = ts2)
        
        fcast_pasado<-data.frame(fcast)
        rownames(fcast_pasado)<-c(1:nrow(fcast_pasado))
        fin_f<-filas+4
        fcast_pasado2<-fcast_pasado[1:fin_f,]
        rownames(fcast_pasado2)<-c(1:fin_f)
        
        cbind_p_dia3<-cbind_p_dia[,c('fecha','producto')]
        cbind_p_dia3$fecha<-as.Date(cbind_p_dia3$fecha,origin='1970-01-01')
        f0<-cbind_p_dia3[nrow(cbind_p_dia),'fecha']
        f1<-f0+7
        f2<-f1+7
        f3<-f2+7
        f4<-f3+7
        fechas_f<-rbind(f1,f2,f3,f4)
        productos_f<-fcast_pasado[21:24,'Point.Forecast']
        fechas_p<-data.frame(cbind(fechas_f,productos_f))
        colnames(fechas_p)<-c('fecha','producto')
        fechas_p$fecha<-as.Date(fechas_p$fecha,origin='1970-01-01')
        
        datos_graph_producto2<-rbind(cbind_p_dia3,fechas_p)
        
        datos_graph_producto<-cbind(datos_graph_producto2,fcast_pasado2)
        
        p<-ggplot(datos_graph_producto,aes(y=producto,x=fecha))+
            geom_line()+
            geom_point()+
            geom_smooth(method='lm')+
            geom_line(aes(y=Lo.80),color='red',linetype='dashed')+
            geom_line(aes(y=Hi.80),color='red',linetype='dashed')+
            xlab('Fecha')+
            ylab('Producto')+
            ggtitle('Comportamiento del producto')
        p2<-ggplotly(p)
        p2
    })
    output$grafico_cajas<-renderPlotly({
        inicio2<-nrow(ventas_dia)-140
        fin2<-nrow(ventas_dia)
        datos_graph_caja1<-ventas_dia[inicio2:fin2,c('dia','normal_19',	'alcalino_19',	'normal_11',	'alcalina_11',	'botella_medio',	'botella_litro')]
        if(input$select_product == 'Garrafón 19 L'){
            datos_graph_caja2<-datos_graph_caja1[,c('dia','normal_19')]
            
        }else if(input$select_product == 'Garrafón alcalino 19 L'){
            datos_graph_caja2<-datos_graph_caja1[,c('dia','alcalino_19')]
            
        }else if(input$select_product == 'Garrafón 11L'){
            datos_graph_caja2<-datos_graph_caja1[,c('dia','normal_11')]
            
        }else if(input$select_product == 'Garrafón alcalino 11L'){
            datos_graph_caja2<-datos_graph_caja1[,c('dia','alcalina_11')]
            
        }else if(input$select_product == 'Botella medio litro'){
            datos_graph_caja2<-datos_graph_caja1[,c('dia','botella_medio')]
            
        }else if(input$select_product == 'Botella 1L'){
            datos_graph_caja2<-datos_graph_caja1[,c('dia','botella_litro')]
        }
        lunes<-data.frame(filter(datos_graph_caja2,dia=="Lunes"))
        martes<-data.frame(filter(datos_graph_caja2,dia=="Martes"))
        miercoles<-data.frame(filter(datos_graph_caja2,dia=="Miércoles"))
        jueves<-data.frame(filter(datos_graph_caja2,dia=="Jueves"))
        viernes<-data.frame(filter(datos_graph_caja2,dia=="Viernes"))
        sabado<-data.frame(filter(datos_graph_caja2,dia=="Sábado"))
        domingo<-data.frame(filter(datos_graph_caja2,dia=="Domingo"))
        lunes2<-lunes[,-1]
        martes2<-martes[,-1]
        miercoles2<-miercoles[,-1]
        jueves2<-jueves[,-1]
        viernes2<-viernes[,-1]
        sabado2<-sabado[,-1]
        domingo2<-domingo[,-1]
        box_semana<-plot_ly(y=lunes2,type = 'box',quartilemethod='inclusive',name='Lunes') %>% 
            add_trace(y=martes2,type='box',quartilemethod='inclusive',name='Martes') %>% 
            add_trace(y=miercoles2,type='box',quartilemethod='inclusive',name='Miércoles') %>% 
            add_trace(y=jueves2,type='box',quartilemethod='inclusive',name='Jueves') %>% 
            add_trace(y=viernes2,type='box',quartilemethod='inclusive',name='Viernes') %>% 
            add_trace(y=sabado2,type='box',quartilemethod='inclusive',name='Sábado') %>% 
            add_trace(y=domingo2,type='box',quartilemethod='inclusive',name='Domingo')
        box_semana
    })
    output$tabla_predicciones<-renderDataTable({
        inicio1<-nrow(ventas_dia)-140
        fin1<-nrow(ventas_dia)
        if(input$select_product == 'Garrafón 19 L'){
            ventas_dia_lm<-ventas_dia[inicio1:fin1,c('fecha','dia','normal_19')]
        }else if(input$select_product == 'Garrafón alcalino 19 L'){
            ventas_dia_lm<-ventas_dia[inicio1:fin1,c('fecha','dia','alcalino_19')]
        }else if(input$select_product == 'Garrafón 11L'){
            ventas_dia_lm<-ventas_dia[inicio1:fin1,c('fecha','dia','normal_11')]
        }else if(input$select_product == 'Garrafón alcalino 11L'){
            ventas_dia_lm<-ventas_dia[inicio1:fin1,c('fecha','dia','alcalina_11')]
        }else if(input$select_product == 'Botella medio litro'){
            ventas_dia_lm<-ventas_dia[inicio1:fin1,c('fecha','dia','botella_medio')]
        }else if(input$select_product == 'Botella 1L'){
            ventas_dia_lm<-ventas_dia[inicio1:fin1,c('fecha','dia','botella_litro')]      
        }
        
        
        ventas_dia_lm2<-ventas_dia_lm %>% 
            mutate_if(is.character,as.factor)
        
        if(input$dia_product == 'Lunes'){
            p_dia<-ventas_dia_lm2 %>% 
                filter(dia == 'Lunes')
        }else if(input$dia_product == 'Martes'){
            p_dia<-ventas_dia_lm2 %>% 
                filter(dia == 'Martes')
        }else if(input$dia_product == 'Miércoles'){
            p_dia<-ventas_dia_lm2 %>% 
                filter(dia == 'Miércoles')
        }else if(input$dia_product == 'Jueves'){
            p_dia<-ventas_dia_lm2 %>% 
                filter(dia == 'Jueves')
        }else if(input$dia_product == 'Viernes'){
            p_dia<-ventas_dia_lm2 %>% 
                filter(dia == 'Viernes')
        }else if(input$dia_product == 'Sábado'){
            p_dia<-ventas_dia_lm2 %>% 
                filter(dia == 'Sábado')
        }else if(input$dia_product == 'Domingo'){
            p_dia<-ventas_dia_lm2 %>% 
                filter(dia == 'Domingo')
        }
        filas<-nrow(p_dia)
        num_dia<-c(1:nrow(p_dia))
        cbind_p_dia<-cbind(num_dia,p_dia)
        colnames(cbind_p_dia)<-c('num_dia','fecha','dia','producto')
        filas_f<-filas+20
        data1<-c(1:filas_f)
        ts<-ts(cbind_p_dia$num_dia,start = 0,frequency=1)
        ts2<-ts(data1,start = 0, end=filas_f,frequency=1)
        
        cbind_p_dia2<-cbind(ts,cbind_p_dia)
        colnames(cbind_p_dia2)<-c('ts','num_dia','fecha','dia','producto')
        
        fit.cons<-tslm(producto~ts,data = cbind_p_dia2)
        fcast<-forecast(fit.cons,newdata = ts2)
        
        fcast_pasado<-data.frame(fcast)
        fcast_pasado2<-fcast_pasado[1:filas,]
        rownames(fcast_pasado2)<-c(1:filas)
        f1<-cbind_p_dia2[nrow(cbind_p_dia2),'fecha']
        f2<-as.Date(as.POSIXct(f1,origin = "1970-01-01"))
        f3<-f2+7
        f4<-f3+7
        f5<-f4+7
        f6<-f5+7
        f3_1<-format(f3,'%d de %B del %Y')
        f4_1<-format(f4,'%d de %B del %Y')
        f5_1<-format(f5,'%d de %B del %Y')
        f6_1<-format(f6,'%d de %B del %Y')
        filas2<-filas+1
        filas3<-filas2+3
        fcast_futuro<-data.frame(fcast)
        fcast_futuro2<-fcast_pasado[filas2:filas3,]
        fechas_f<-rbind(f3_1,f4_1,f5_1,f6_1)
        rownames(fcast_futuro2)<-fechas_f
        tabla<-cbind(fechas_f,fcast_futuro2)
        
        fcast_futuro3<-fcast_futuro2[,c('Lo.80','Point.Forecast','Hi.80')]
        colnames(fcast_futuro3)<-c('Predicción mínima','Predicción','Predicción máxima')
        fcast_futuro4<-round(fcast_futuro3,0)
        formattable(fcast_futuro4)
    })
}

shinyApp(ui = ui, server = server)
