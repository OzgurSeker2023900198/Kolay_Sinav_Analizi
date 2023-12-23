library(shiny)
library(shinyjs)
library(readxl)
library(ggplot2)
library(plotly)
library(DT)
library(shinythemes)
library(bslib)
library(circlize)
library(viridis)
library(writexl)
library(hrbrthemes)
library(tidyverse)
library(DescTools)
library(dplyr)
library(shinydashboard)
library(fontawesome)

ui <- fluidPage(
    titlePanel( div(tags$img(src = "MyImage1.jpg",height="35px",weight="40px"),"Kolay Sınav Analizi")),
              
  
  theme = bs_theme(version = 4, bootswatch = "cerulean"), 
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "dosya", 
                label = "Lütfen Excel dosyasını Göz at tuşuna tıklayarak seçiniz.",
                accept = c(".xlsx",".xls",".csv"), buttonLabel = "Göz at"),
      selectInput(inputId = "eleman", label = "Sınıftan bir öğrenci seçiniz.", choices = NULL),
      textInput(inputId = "sinif", label = "Sınıfı Giriniz", value = ""),
      div(
        id = "languageDropdown",
        selectInput("languageSelect", label = "Dil-Language", choices = c("Türkçe", "English"))
      ),
      actionButton("geri_bildirim_button", 
                   HTML('<i class="fas fa-comments"></i> Geri Bildirim')),
      textOutput("feedback_text"),
      
      
      style = "width:250px;"
    ),
    mainPanel(
      div(
        tabsetPanel(
          
          tabPanel("Sınıfın Sütun Grafikleri", 
                   fluidRow(
                     column(9, plotOutput("sutun_grafigi")),
                     column(4, textOutput("sutun_not_text"), align = "center"),
                     column(12, actionButton("sutun_not_button1", "Bilgilendirme"), align = "center"),
                     column(12, downloadButton("kayit_button", "Grafiği Kaydet"), align = "center",style = "margin-top: 10px;"),
                     
                     
                     
                     column(4, textOutput("meanOutput_sinav_1"), align = "left",style = "margin-top: 20px;"),
                     column(4, textOutput("medianOutput_sinav_1"), align = "left",style = "margin-top: 20px;"),
                     column(4, textOutput("modeOutput_sinav_1"), align = "left",style = "margin-top: 20px;"),
                     
                     
                     column(9, plotOutput("sutun_grafigi_second"),style = "margin-top: 20px;"),
                     column(4, textOutput("sutun_second_not_text"), align = "center"),
                     column(12, actionButton("sutun_second_not_button", "Bilgilendirme"), align = "center"),
                     
                     column(4, textOutput("meanOutput_sinav_2"), align = "left",style = "margin-top: 20px;"),
                     column(4, textOutput("medianOutput_sinav_2"), align = "left",style = "margin-top: 20px;"),
                     column(4, textOutput("modeOutput_sinav_2"), align = "left",style = "margin-top: 20px;"),
                     
                     
                     column(9, plotOutput("ortalama_sutun_grafigi"),style = "margin-top: 20px;"),
                     column(4, textOutput("ortalama_sutun_not_text"), align = "center"),
                     column(12, actionButton("ortalama_sutun_not_button", "Bilgilendirme"), align = "center"),
                     column(4, textOutput("ortalamaOutput"), align = "left",style = "margin-top: 20px;"),
                     column(4, textOutput("meanOutput_sinav_toplam"), align = "left",style = "margin-top: 20px;"),
                     column(8, textOutput("medianOutput_sinav_toplam"), align = "right",style = "margin-top: 20px;"),
                     column(12, textOutput("modeOutput_sinav_toplam"), align = "center",style = "margin-top: 20px;"),
                     
                     
                   )
          ),
          
          tabPanel("Sınıfın Nokta Grafikleri", 
                   fluidRow(
                     column(8, plotOutput("scatter_plot")),
                     column(4, textOutput("scatter_not_text"), align = "center"),
                     column(12, actionButton("scatter_not_button", 
                                             "Bilgi Kutusu"), 
                            align = "center",style = "margin-top: 20px;"),
                     column(8, plotOutput("scatter_plot_2"),style = "margin-top: 20px;"),
                     column(4, textOutput("scatter_plot_2_not_text"), align = "center"),
                     column(12, actionButton("scatter_plot_2_button", 
                                             "Bilgi Kutusu"), 
                            align = "center",style = "margin-top: 20px;"),
                     column(8, plotOutput("scatter_plot_ortalama"),style = "margin-top: 20px;"),
                     column(4, textOutput("scatter_plot_ortalama_not_text"), align = "center"),
                     column(12, actionButton("scatter_plot_ortalama_button", 
                                             "Bilgi Kutusu"), 
                            align = "center",style = "margin-top: 20px;"),
                   )
          ),
          
          tabPanel("Öğrenci Analizi",
                   fluidRow(
                     column(4, plotOutput("ogr11")),  
                     column(4, plotOutput("ogr12")),  
                     column(4, plotOutput("secilen_ogr_ortalama_sutun_grafigi")), 
                     column(4, textOutput("secilen_sutun_not_text"), align = "center"),
                     column(12, actionButton("secilen_sutun_not_button", "Bilgi Kutusu"), align = "center")
                   ),
                   textOutput("secilen_eleman_text")
          ),
          
          
          
          ## datatable fonksiyonu tab panel kısmı
          
          tabPanel("Sınıfın Not Fişi", DTOutput("veri_tablosu")),
          
          
          
          tabPanel("Meraklısına İstatistikler", 
                   fluidRow(
                     column(3, actionButton("buton_1", "Lolipop Grafiği")),
                     column(3, actionButton("buton_2", "Kümeleme Analizi")),
                     column(3, actionButton("buton_3", "Pasta Grafiği")),
                     column(3, actionButton("buton_4", "Isı Haritası")),
                     column(3, actionButton("buton_5", "Balon Grafiği"), 
                            style = "margin-top: 10px;"),
                     column(3, actionButton("buton_6", "Renkli Sütunlar"),
                            style = "margin-top: 10px;"),
                     column(3, actionButton("buton_7", "Yığılmış Sütunlar"),
                            style = "margin-top: 10px;"),
                     column(3, actionButton("buton_8", "Donut Grafiği "),
                            style = "margin-top: 10px;"),
                   )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  veri <- reactive({
    req(input$dosya)
    readxl::read_excel(input$dosya$datapath)
  })
  
  # 1. sınavın not ortalaması
  ikinci_sutun_ortalama <- reactive({
    data <- veri()
    
    if (!is.null(data)) {
      return(mean(data[[2]]))
    }
  })
  
  # Ekrana yazdır
  output$meanOutput_sinav_1 <- renderPrint({
    ilk_sinav_ortalamalari <- ikinci_sutun_ortalama()
    cat("1. sınav notlarının ortalaması: ", ilk_sinav_ortalamalari, "\n")
  })
  
  # 1. sınav notlarının medyanı
  
  ikinci_sutun_medyan <- reactive({
    data <- veri()
    
    if (!is.null(data)) {
      return(median(data[[2]]))
    }
  })
  output$medianOutput_sinav_1 <- renderPrint({
    cat("1. sınav notlarının medyanı: ", ikinci_sutun_medyan(), "\n")
  })
  
 
  # 1. sınav notlarının modu
  ikinci_sutun_mod <- reactive({
    data <- veri()
    
    if (!is.null(data)) {
      table_result <- table(data[[2]])
      mod_values <- as.numeric(names(table_result)[table_result == max(table_result)])
      
      if (length(mod_values) > 0) {
        return(mod_values)
      } else {
        return("Mod bulunamadı.")
      }
    }
  })
  
  
  output$modeOutput_sinav_1 <- renderPrint({
    cat("1. sınavın not modu: ", ifelse(ikinci_sutun_mod() == "Mod bulunamadı", ikinci_sutun_mod(), sprintf("%.1f", ikinci_sutun_mod())), "\n")
  })
  
  

  
  
  
  # 2.sınavın not ortalaması text
  
  ucuncu_sutun_ortalama <- reactive({
    data <- veri()
    
    if (!is.null(data)) {
      return(mean(data[[3]]))
    }
  })
  
  # Ekrana yazdır
  output$meanOutput_sinav_2 <- renderPrint({
    ilk_sinav_ortalamalari <- ucuncu_sutun_ortalama()
    cat("2. sınav notlarının ortalaması: ", ilk_sinav_ortalamalari, "\n")
  })
  
  
  # 2. sınav notlarının medyanı
  
  ucuncu_sutun_medyan <- reactive({
    data <- veri()
    
    if (!is.null(data)) {
      return(median(data[[3]]))
    }
  })
  output$medianOutput_sinav_2 <- renderPrint({
    cat("2. sınav notlarının medyanı: ", ucuncu_sutun_medyan(), "\n")
  })
  
  # 2. sınav notlarının medyanı
  ucuncu_sutun_mod <- reactive({
    data <- veri()
    
    if (!is.null(data)) {
      table_result <- table(data[[3]])
      mod_values <- as.numeric(names(table_result)[table_result == max(table_result)])
      
      if (length(mod_values) > 0) {
        return(mod_values)
      } else {
        return("Mod bulunamadı.")
      }
    }
  })
  
  
  output$modeOutput_sinav_2 <- renderPrint({
    cat("2. sınavın not modu: ", ifelse(ucuncu_sutun_mod() == "Mod bulunamadı", ucuncu_sutun_mod(), sprintf("%.1f", ucuncu_sutun_mod())), "\n")
  })
  
  
  
  observeEvent(input$geri_bildirim_button, {
    showModal(
      modalDialog(
        title = "Geri Bildirim",
        
        tags$p("Geri bildirimleriniz için lütfen postaozgurseker@gmail.com adresine mesaj atınız."),
        tags$p("Özgür Şeker", tags$br()),
        tags$p("9 Eylül Üniversitesi İstatistik Anabilim Dalı Veri Bilimi Yüksek Lisans"),
        easyClose = TRUE,
        footer = modalButton("Kapat")
      )
    )
  })
  
  observe({
    req(input$dosya)
    dosya <- readxl::read_excel(input$dosya$datapath)
    updateSelectInput(session, "eleman", choices = unique(dosya[[1]]))
  })
  
  
  ## datatable fonksiyonu output kısmı
  
    output$veri_tablosu <- renderDT({
    datatable(veri())
  })
  
  
  # ortalama sütun grafiğinin mean,mod ve medyanı
  
  # iki sınavın not ortalaması
  toplam_sutun_ortalama <- reactive({
    data_sutun <- veri()
    y_degerleri_yeni <- rowMeans(data_sutun[, c(names(data_sutun)[2], names(data_sutun)[3])], na.rm = TRUE)
    
    if (!is.null(data_sutun)) {
      ortalama <- mean(y_degerleri_yeni, na.rm = TRUE)
      return(ortalama)
    }
  })
  
  # Ekrana yazdır
  output$meanOutput_sinav_toplam <- renderPrint({
    toplam_sinav_ortalama <- toplam_sutun_ortalama()
    cat("Sınıf notlarının ortalaması: ", toplam_sinav_ortalama, "\n")
  })
  
  
  
  # ortalama sütun grafiğinin medyanı
  
  medyan_sinav_ortalama <- reactive({
    data_sutun <- veri()
    y_degerleri_yeni <- rowMeans(data_sutun[, c(names(data_sutun)[2], names(data_sutun)[3])], na.rm = TRUE)
    
    if (!is.null(data_sutun)) {
      medyan <- median(y_degerleri_yeni, na.rm = TRUE)
      return(medyan)
    }
  })
  
  # Ekrana yazdır
  output$medianOutput_sinav_toplam <- renderPrint({
    medyan_sinav_ortalama <- medyan_sinav_ortalama()
    cat("Sınıf notlarının ortalamalarının medyanı: ", medyan_sinav_ortalama, "\n")
  })
  
  # ortalama sütun grafiğinin modu
  
  # Mod hesaplama fonksiyonu
  calculate_mode <- function(x) {
    table_x <- table(x)
    mode_values <- as.numeric(names(table_x[table_x == max(table_x)]))
    
    if (length(mode_values) == length(x)) {
      return("Mod bulunamadı")
    } else {
      return(mode_values)
    }
  }
  
  # Sınıf notlarının modunu bulan kod
  mod_sinav_ortalama <- reactive({
    data_sutun <- veri()
    y_degerleri_yeni <- rowMeans(data_sutun[, c(names(data_sutun)[2], names(data_sutun)[3])], na.rm = TRUE)
    
    if (!is.null(data_sutun)) {
      mod <- calculate_mode(y_degerleri_yeni)
      return(mod)
    }
  })
  
  # Ekrana yazdır
  output$modeOutput_sinav_toplam <- renderPrint({
    mod_sinav_ortalama <- mod_sinav_ortalama()
    cat("Sınıf notlarının ortalamalarının modu: ", mod_sinav_ortalama, "\n")
  })
  
  
  
  #.data ifadesi dplyr paketinde kullanılan bir veri okuma işlevidir.
  
  output$sutun_grafigi <- renderPlot({
    if (!is.null(input$dosya)) {
      ggplot(veri(), aes(x = veri()[[1]], y = veri()[[2]])) +
        geom_col(aes(fill = veri()[[2]] < 50), color = "black",width = 0.4) +
        ylim(0,110)+
        geom_text(aes(label = veri()[[2]]), vjust = -0.5, size = 4, color = "black")+
        scale_fill_manual(values = c("TRUE" = "lightpink", "FALSE" = "lightblue"),guide=F) +
        labs(
          x = "Öğrenci İsimleri", 
          y = "1. Sınav Notları", 
          title = paste("Grafik 1.",input$sinif,"Sınıfının 1. Sınav Notları")) +
        theme_minimal() +
        coord_fixed(ratio = 0.1)+
        theme(axis.text.x = element_text(size = 12,angle = 90,vjust = 0.5,hjust = 1))+
        theme(axis.text.y = element_text(size = 12),
              plot.title = element_text(size = 16, vjust = 8))
      
    }
  })
  

  
  
  output$sutun_not_text <- renderText({
    if (input$sutun_not_button1 > 0) {
      showModal(modalDialog(
        title = "Not",
        paste("Bu grafikte 1. sınav sonucu 50 puanın altında kalan öğrencileri temsil eden sütunlar kırmızı renkle gösterilmiştir."),
        easyClose = TRUE,
        footer = modalButton("Kapat")
      ))
    }
  })
  
  observeEvent(input$sutun_not_button1, {
    updateTabsetPanel(session, "main", selected = "Sütun Grafiği")
  })
  
  output$ortalama_sutun_grafigi <- renderPlot({
    if (!is.null(input$dosya)) {
      veri <- veri()  
      y_degerleri <- rowMeans(veri[, c(names(veri())[2], names(veri())[3])], na.rm = TRUE)  
      
      ggplot(veri, aes(x = veri()[[1]], y = y_degerleri)) +
        geom_col(aes(fill = y_degerleri < 50), color = "black", width = 0.4) +
        ylim(0,110)+
        geom_text(aes(label = round(y_degerleri, 1)), vjust = -0.5, size = 4, color = "black") + 
        scale_fill_manual(values = c("TRUE" = "lightpink", "FALSE" = "lightblue"), guide = FALSE) +
        labs(x = "Öğrenci İsimleri", y = "Sınavların Not Ortalaması", title = paste("Grafik 3.",input$sinif,"Sınıfının Sınavlarının Not Ortalaması")) +
        theme_minimal() +
        coord_fixed(ratio = 0.1) +
        theme(axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1)) +
        theme(axis.text.y = element_text(size = 12),
              plot.title = element_text(size = 16, vjust = 8))
    }
  })
  
  
  output$ortalama_sutun_not_text <- renderText({
    if (input$ortalama_sutun_not_button > 0) {
      showModal(modalDialog(
        title = "Not",
        paste("Bu grafikte 1. ve 2. sınav puanlarının aritmetik ortalaması 50 puan altında kalan öğrencileri temsil eden sütunlar kırmızı renkle gösterilmiştir."),
        easyClose = TRUE,
        footer = modalButton("Kapat")
      ))
    }
  })
  
  observeEvent(input$ortalama_sutun_not_button, {
    updateTabsetPanel(session, "main", selected = "Sütun Grafiği")
  })
  
  
  
  
  
  output$sutun_grafigi_second <- renderPlot({
    if (!is.null(input$dosya)) {
      ggplot(veri(), aes(x = .data[[names(veri())[1]]], y = .data[[names(veri())[3]]])) +
        geom_col(aes(fill = .data[[names(veri())[3]]] < 50), color = "black",width = 0.4) +
        ylim(0,110)+
        geom_text(aes(label = .data[[names(veri())[3]]]), vjust = -0.5, size = 4, color = "black")+
        scale_fill_manual(values = c("TRUE" = "lightpink", "FALSE" = "lightblue"),guide=F) +
        labs(
          x = "Öğrenci İsimleri" , 
          y = "2. Sınav Notları", 
          title = paste("Grafik 2.", input$sinif,"Sınıfının 2. Sınav Notları")) +
        theme_minimal() +
        coord_fixed(ratio = 0.1)+
        theme(axis.text.x = element_text(size = 12,angle = 90,vjust = 0.5,hjust = 1))+
        theme(axis.text.y = element_text(size = 12),
              plot.title = element_text(size = 16, vjust = 8))
    }
  })
  
  output$sutun_second_not_text <- renderText({
    if (input$sutun_second_not_button > 0) {
      showModal(modalDialog(
        title = "Not",
        paste("Bu grafikte 2. sınav sonucu 50 puanın altında kalan öğrencileri temsil eden sütunlar kırmızı renkle gösterilmiştir."),
        easyClose = TRUE,
        footer = modalButton("Kapat")
      ))
    }
  })
  
  observeEvent(input$sutun_not_button, {
    updateTabsetPanel(session, "main", selected = "Sütun Grafiği")
  })
  
  
  
  output$secilen_ogr_ortalama_sutun_grafigi <- renderPlot({
    if (!is.null(input$dosya) && !is.null(input$eleman)) {
      
      secilen_veri <- veri()[veri()[[1]] == input$eleman, c(1, 2, 3)]
      
      ortalama <- apply(secilen_veri[, 2:3], 1, mean)
      
      ggplot(secilen_veri, aes(x = .data[[names(secilen_veri)[1]]], y = ortalama)) +
        geom_col(color = "black", fill = "green", width = 0.1) +
        geom_text(aes(label = .data[[names(secilen_veri)[2]]]),  
                  position = position_stack(vjust = 0.5),  
                  size = 5,  # Etiket boyutu
                  color = "black"  # Etiket rengi
                  ) +
        ylim(0, 110) +
        geom_text(aes(label = ifelse(ortalama >= 50, "Başarılı", "Başarısız")),
                  vjust = -0.5, size = 4, color = "black") +
        labs(x ="Seçilen Öğrenci", y = "Seçilen Öğrencinin Not Ortalaması", 
             title = "Grafik. 3") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1)) +
        theme(axis.text.y = element_text(size = 14),
              axis.title.x = element_text(size = 14),  # X ekseni etiket boyutu
              axis.title.y = element_text(size = 14)
        )
    }
  })
  
  
  output$ogr11 <- renderPlot({
    if (!is.null(input$dosya) && !is.null(input$eleman)) {
      
      secilen_veri <- veri()[veri()[[1]] == input$eleman, c(1, 2, 3)]
      
      
      
      ggplot(secilen_veri, aes(x = .data[[names(secilen_veri)[1]]], y = .data[[names(secilen_veri)[2]]])) +
        geom_col(color = "black", fill = "blue", width = 0.1) +
        geom_text(aes(label = .data[[names(secilen_veri)[2]]]),  
                  position = position_stack(vjust = 0.5),  
                  size = 5,  # Etiket boyutu
                  color = "white"  # Etiket rengi
                  ) +
        ylim(0, 110) +
        labs(x ="Seçilen Öğrenci", y = "Seçilen Öğrencinin 1. Sınav Notu", 
             title = "Grafik 1") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1)) +
        theme(axis.text.y = element_text(size = 14),
              axis.title.x = element_text(size = 14),  # X ekseni etiket boyutu
              axis.title.y = element_text(size = 14)
        )
    }
  })
  
  
  output$ogr12 <- renderPlot({
    if (!is.null(input$dosya) && !is.null(input$eleman)) {
      
      secilen_veri <- veri()[veri()[[1]] == input$eleman, c(1, 2, 3)]
      
      
      
      ggplot(secilen_veri, aes(x = .data[[names(secilen_veri)[1]]], y = .data[[names(secilen_veri)[3]]])) +
        geom_col(color = "black", fill = "yellow", width = 0.1) +
        geom_text(aes(label = .data[[names(secilen_veri)[3]]]),  
                  position = position_stack(vjust = 0.5),  
                  size = 5,  # Etiket boyutu
                  color = "black"  # Etiket rengi
                  ) +
        ylim(0, 110) +
        

        labs(x ="Seçilen Öğrenci", y = "Seçilen Öğrencinin 2. Sınav Notu", 
             title = "Grafik 2") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1)) +
        theme(axis.text.y = element_text(size = 14),
              axis.title.x = element_text(size = 14),  
              axis.title.y = element_text(size = 14)
        )
    }
  })
  
  
  
  
  
  
  output$secilen_eleman_text <- renderText({
    if (!is.null(input$dosya) && !is.null(input$eleman)) {
      secilen_eleman <- input$eleman
      secilen_veri <- veri()[veri()[[1]] == input$eleman, c(1, 2, 3)]
      
      
      ortalama <- apply(secilen_veri[, 2:3], 1, mean)
      paste("Seçilen Öğrenci:", secilen_eleman, " Ortalama Notu:", ortalama)
    }
  })
  
  output$secilen_sutun_not_text <- renderText({
    if (input$secilen_sutun_not_button > 0) {
      showModal(modalDialog(
        title = "Not",
        paste("İstediğiniz öğrencinin analizini görmek için lütfen soldaki menüden öğrenci ismi seçiniz."),
        easyClose = TRUE,
        footer = modalButton("Kapat")
      ))
    }
  })
  
  
  
  
  output$scatter_plot <- renderPlot({
    if (!is.null(input$dosya)) {
      ggplot(veri(), aes(x = .data[[names(veri())[1]]], y = .data[[names(veri())[2]]], label = .data[[names(veri())[2]]])) +
        geom_point(aes(color = .data[[names(veri())[2]]] < 50), size = 4) +
        ggrepel::geom_text_repel(box.padding = 0.5, size = 5) +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "lightblue"),guide=F) +
        labs(x = "Öğrenci İsimleri", y = "1. Sınav Notları", title =paste("Grafik 1.",input$sinif, "Sınıfının 1. Sınav Notları")) +
        theme_minimal()+
        theme(axis.text.x = element_text(size = 12,angle = 90,vjust = 0.5,hjust = 1))+
        theme(axis.text.y = element_text(size = 12),
              plot.title = element_text(size = 16, vjust = 8))
    }
  })
  
  
  
  output$scatter_not_text <- renderText({
    if (input$scatter_not_button > 0) {
      showModal(modalDialog(
        title = "Not",
        paste("Bu nokta grafiğinde 1. sınav sonucu 50 puanın altında kalan öğrencileri temsil eden noktalar kırmızı renkle gösterilmiştir."),
        easyClose = TRUE,
        footer = modalButton("Kapat")
      ))
    }
  })
  
  observeEvent(input$scatter_not_button, {
    updateTabsetPanel(session, "main", selected = "Scatter Plot")
  })
  
  output$scatter_plot_2 <- renderPlot({
    if (!is.null(input$dosya)) {
      ggplot(veri(), aes(x = .data[[names(veri())[1]]], y = .data[[names(veri())[3]]], label = .data[[names(veri())[3]]])) +
        geom_point(aes(color = .data[[names(veri())[3]]] < 50), size = 4) +
        ggrepel::geom_text_repel(box.padding = 0.5, size = 5) +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "lightblue"),guide=F) +
        labs(x = "Öğrenci İsimleri", y = "2. Sınav Notları", title = paste("Grafik 2.",input$sinif," Sınıfının 2. Sınav Notları")) +
        theme_minimal()+
        theme(axis.text.x = element_text(size = 12,angle = 90,vjust = 0.5,hjust = 1))+
        theme(axis.text.y = element_text(size = 12),
              plot.title = element_text(size = 16, vjust = 8))
    }
  })
  
  output$scatter_plot_2_not_text <- renderText({
    if (input$scatter_plot_2_button > 0) {
      showModal(modalDialog(
        title = "Not",
        paste("Bu nokta grafiğinde 2. sınav sonucu 50 puanın altında kalan öğrencileri temsil eden noktalar kırmızı renkle gösterilmiştir."),
        easyClose = TRUE,
        footer = modalButton("Kapat")
      ))
    }
  })
  
  
  output$scatter_plot_ortalama  <- renderPlot({
    if (!is.null(input$dosya)) {
      veri <- veri()  
      y_degerleri <- rowMeans(veri[, c(names(veri())[2], names(veri())[3])], na.rm = TRUE) 
      
      ggplot(veri, aes(x = .data[[names(veri())[1]]], y = y_degerleri)) +
        geom_point(aes(color = y_degerleri < 50),size=4) +
        ggrepel::geom_text_repel(aes(label = round(y_degerleri, 2)), box.padding = 0.5, size = 5) +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "lightblue"), guide = FALSE) +
        labs(x = "Öğrenci İsimleri", y = "Sınav Not Ortalamaları", title = paste("Grafik3.",input$sinif," Sınıfının 1. ve 2. Sınav Sonuçlarının Ortalamaları")) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1)) +
        theme(axis.text.y = element_text(size = 12),
              plot.title = element_text(size = 16, vjust = 8))
    }
  })
  
  output$scatter_plot_ortalama_not_text <- renderText({
    if (input$scatter_plot_ortalama_button > 0) {
      showModal(modalDialog(
        title = "Not",
        paste("Bu nokta grafiğinde 1. ve 2. Sınav Sonuçlarının Ortalamaları 50 puanın altında kalan öğrencileri temsil eden noktalar kırmızı renkle gösterilmiştir."),
        easyClose = TRUE,
        footer = modalButton("Kapat")
      ))
    }
  })
  
      

  
  observeEvent(input$buton_1, {
    showModal(modalDialog(
      title = "",
      plotOutput("lollipop_grafik"),
      size = "l",
      footer = modalButton("Kapat")
    ))
  })
  
  observeEvent(input$modal_button_1, {
    if (input$modal_button_1) {
      removeModal()
    }
  })
  
  output$lollipop_grafik <- renderPlot({
    ggplot(veri(), aes(x = .data[[names(veri())[1]]], y = .data[[names(veri())[2]]])) +
      geom_segment(aes(x = .data[[names(veri())[1]]], xend = .data[[names(veri())[1]]], y = 0, yend = .data[[names(veri())[2]]]), color = "grey") +
      geom_point(color = "orange", size = 4) +
      labs(x = "Öğrenciler", y = "Sınav Not Ortalaması", title = paste("Grafik.",input$sinif ,"Sınıfının Lolipop Grafiği")) +
      theme_minimal() +
      coord_fixed(ratio = 0.1)+
      theme(axis.text.x = element_text(size = 12,angle = 90,vjust = 0.5,hjust = 1))+
      theme(axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 16, vjust = 8))
  })
  
  observeEvent(input$buton_2, {
    showModal(modalDialog(
      title = "",
      plotOutput("kume_grafigi"),
      size = "l",
      footer = modalButton("Kapat")
    ))
  })
  
  observeEvent(input$modal_button_2, {
    if (input$modal_button_2) {
      removeModal()
    }
  })
  
  output$kume_grafigi <- renderPlot({
    # Kümeleme Analizi
    kume_verileri <- veri()[[2]]
    kume1 <- kume_verileri[kume_verileri >= 50]
    kume2 <- kume_verileri[kume_verileri < 50]
    
    # Verileri birleştirme
    kumeler <- data.frame(Grup = rep(c("Başarılı olanlar", "Başarısız olanlar"), c(length(kume1), length(kume2))), Not = c(kume1, kume2))
    
    # Kümeleme Grafiği
    ggplot(kumeler, aes(x = Grup, y = Not, fill = Grup)) +
      geom_jitter(width = 0.4, height = 0.2, alpha = 0.7,size=4,
                  color = ifelse(kumeler$Grup == "Başarısız olanlar", "red", "darkblue"),
                  fill = ifelse(kumeler$Grup == "Başarısız olanlar", "red", "darkblue")) +
      geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = min(kume_verileri[kume_verileri >= 50])-2, ymax = max(kumeler$Not)+2),
                color = "darkorange", fill = NA, linetype = "solid", size = 1)+
      
      labs(title = paste("Grafik.",input$sinif ,"Sınıfının Kümeleme Analizi Grafiği"), x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 16, vjust = 8)
            )+guides(fill = FALSE)
    
  })
  
  
  observeEvent(input$buton_3, {
    showModal(modalDialog(
      title = "",
      plotlyOutput("pasta_grafik_ortalama"),
      size = "l",
      footer = modalButton("Kapat")
    ))
  })
  
  observeEvent(input$modal_button_3, {
    if (input$modal_button_3) {
      removeModal()
    }
  })
  

  
  output$pasta_grafik_ortalama <- renderPlotly({
    if (!is.null(input$dosya)) {
      veri <- readxl::read_excel(input$dosya$datapath)
      
      # Üçüncü sütun
      sutun_2_verileri <- veri[[2]]
      
      # Dördüncü sütun
      sutun_3_verileri <- veri[[3]]
      
      # Her bir satırın ortalaması
      satir_ortalama <- rowMeans(cbind(sutun_2_verileri, sutun_3_verileri))
      
      # 50'nin altındaki veriler
      alt_50 <- satir_ortalama < 50
      
      # 50'nin üstündeki veriler
      ust_50 <- satir_ortalama >= 50
      
      labels <- c("Başarısız", "Başarılı")
      values <- c(sum(alt_50), sum(ust_50))
      
      fig <- plot_ly(labels = labels, values = values, type = 'pie',
                     textinfo = 'percent', insidetextorientation = 'radial')
      
      fig <- fig %>% layout(title = paste("Sınıfın Başarı Durumunu Gösteren Pasta Grafiği"))
    }
  })
  
  
  
  
  
  
  observeEvent(input$buton_4, {
    showModal(modalDialog(
      title = "",
      plotOutput("heatmap_grafik"),
      size = "l",
      footer = modalButton("Kapat")
    ))
  })
  
  observeEvent(input$modal_button_4, {
    if (input$modal_button_4) {
      removeModal()
    }
  })
  
  output$heatmap_grafik <- renderPlot({
    if (!is.null(input$dosya)) {
      ggplot(veri(), aes(x = .data[[names(veri())[2]]], y = .data[[names(veri())[3]]], fill = .data[[names(veri())[2]]])) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "white", high = "blue") +
        labs(x = "1. Sınav Notları", y = "2. Sınav Notları", title = paste("Grafik.",input$sinif ,"Sınıfının Isı Haritası Grafiği")) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              plot.title = element_text(size = 16, vjust = 8))
    }
  })
  observeEvent(input$buton_5, {
    showModal(modalDialog(
      title = "",
      plotlyOutput("bubble_chart"),
      size="l",
      footer = modalButton("Kapat")
    ))
  })
  
  observeEvent(input$modal_button_5, {
    if (input$modal_button_5) {
      removeModal()
    }
  })
  
  output$bubble_chart <- renderPlotly({

    if (!is.null(input$dosya)) {
      veri <- readxl::read_excel(input$dosya$datapath)
      

      sutun_isimleri <- names(veri)
      
      veri <- veri %>%
        select(İsim = sutun_isimleri[1], Sinav1 = sutun_isimleri[2], Sinav2 = sutun_isimleri[3])
      
      
      
      
  
      p <- veri %>%

        arrange(desc(Sinav1)) %>%
        mutate(İsim = factor(İsim, İsim)) %>%
        

        mutate(text = paste("İsim: ", İsim, "\n1.Sınav Notu: ", Sinav1, "\n2.Sınav Notu: ", Sinav2, sep="")) %>%
        

        ggplot(aes(x = Sinav1, y = Sinav2, size = Sinav1, color = İsim, text = text)) +
        geom_point(alpha = 0.7) +
        scale_size(range = c(1.4, 19), name = "1. SInav") +
        scale_color_viridis(discrete = TRUE, guide = FALSE) +
        theme_ipsum() +
        theme(legend.position = "none")+
        labs(x = "1. Sınav Notu", y = "2. Sınav Notu")
      

      pp <- ggplotly(p, tooltip = "text")
      
    }
  })
  
  observeEvent(input$buton_6, {
    showModal(modalDialog(
      title = "",
      plotlyOutput("renkli_sutun_grafigi"),
      size = "xl",
      footer = modalButton("Kapat")
    ))
  })
  
  observeEvent(input$modal_button_6, {
    if (input$modal_button_6) {
      removeModal()
    }
  })
  
  
  output$renkli_sutun_grafigi <- renderPlotly({
    fig <- plot_ly(data= veri(), x = ~veri()[[names(veri())[1]]], y = ~veri()[[names(veri())[2]]], type = 'bar',
                   marker = list(color = 'brown'),showlegend = FALSE,text = ~veri()[[names(veri())[2]]])
    fig <- fig %>% add_trace(y = ~veri()[[names(veri())[3]]], marker = list(color = 'blue'),showlegend = FALSE,text = ~veri()[[names(veri())[3]]])
    fig <- fig %>% layout(title = "",
                          xaxis = list(
                            title = "Öğrenci İsimleri",
                            tickfont = list(
                              size = 14,
                              color = 'black')),
                          yaxis = list(
                            title = "Notlar",
                            titlefont = list(
                              size = 16,
                              color = 'black'),
                            tickfont = list(
                              size = 14,
                              color = 'black')),
                          legend = list(x = "", y = ""),
                          barmode = 'group', bargap = 0.1)
    

    
  })
  
  
  observeEvent(input$buton_7, {
    showModal(modalDialog(
      title = "",
      plotlyOutput("yıgılmıs_sutun"),
      size = "xl",
      footer = modalButton("Kapat")
    ))
  })
  
  observeEvent(input$modal_button_7, {
    if (input$modal_button_7) {
      removeModal()
    }
  })
  
  
  output$yıgılmıs_sutun <- renderPlotly({
    fig <- plot_ly(data = veri(), x = ~veri()[[names(veri())[1]]],
                   
                   y = ~veri()[[names(veri())[2]]], type = 'bar',
                   name = '1. Sınav Notları',
                   text = ~veri()[[names(veri())[2]]])
    fig <- fig %>% add_trace(y = ~veri()[[names(veri())[3]]],
                             name = '2. Sınav Notları',
                             text = ~veri()[[names(veri())[3]]])
    fig <- fig %>% layout(
      yaxis = list(title = 'Notlar'),
      barmode = 'stack',
      xaxis = list(
        title = 'Öğrenci İsimleri',  
        tickfont = list(
          size = 14,
          color = 'rgb(107, 107, 107)'
        )
      )
    )
  })
  
  
  observeEvent(input$buton_8, {
    showModal(modalDialog(
      title = "",
      plotlyOutput("donut_grafigi"),
      size = "xl",
      footer = modalButton("Kapat")
    ))
  })
  
  observeEvent(input$modal_button_8, {
    if (input$modal_button_8) {
      removeModal()
    }
  })
  output$donut_grafigi <- renderPlotly({
    if (!is.null(input$dosya)) {

      veri_secti <- veri()[, c(1, 2, 3)]
      

      sutun_isimleri <- names(veri_secti)
      

      fig <- plot_ly(data = veri_secti, labels = ~veri_secti[[sutun_isimleri[1]]],
                     values = ~rowMeans(veri_secti[, c(sutun_isimleri[2], sutun_isimleri[3])], na.rm = TRUE),
                     type = 'pie',
                     textinfo = 'label+percent',
                     insidetextorientation = 'radial',
                     hole = 0.6)
      
      fig <- fig %>% layout(title = "")
      
    }
  })
  
  
  
  
  
  
  
  
  observe({
    if (!is.null(input$btnLanguage) && input$btnLanguage %% 2 == 1) {
      shinyjs::toggle("languageDropdown")
    }
  })
  
  observeEvent(input$languageSelect, {
    selected_language <- input$languageSelect
    
    # Seçilen dil ile ilgili gerekli işlemleri yapabilirsiniz.
    # Örneğin, seçilen dile göre metinleri değiştirebilirsiniz.
    if (selected_language == "Türkçe") {
      # Türkçe metin değişiklikleri burada
    } else {
      # İngilizce metin değişiklikleri burada
    }
  })
  
  # PDF dosyasını oluşturan downloadHandler fonksiyonunu tanımlıyoruz
  output$kayit_button <- downloadHandler(
    filename = function() {
      paste("sutun_grafigi", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file,width = 8, height = 5)
      print(ggplot(veri(), aes(x = .data[[names(veri())[1]]], y = .data[[names(veri())[2]]])) +
              geom_col(aes(fill = .data[[names(veri())[2]]] < 50), color = "black",width = 0.4) +
              scale_fill_manual(values = c("TRUE" = "lightpink", "FALSE" = "lightblue"),guide=F) +
              labs(x = names(veri())[1], y = names(veri())[2], title = "Sütun Grafiği") +
              theme_minimal() +
              coord_fixed(ratio = 0.1)+
              theme(axis.text.x = element_text(size = 12,angle = 90,vjust = 0.5,hjust = 1))+
              theme(axis.text.y = element_text(size = 12),
                    plot.title = element_text(size = 16, vjust = 8)))
      dev.off()
    }
  )
  
}
















shinyApp(ui, server)
