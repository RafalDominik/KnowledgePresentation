# W wersji interaktywnej poniższy program musi być uruchomiony poprzez RStudio, funkcje ustalające scieżkę skryptu
# nie są uniwersalne. Inne działają w RStudio, a inne w konsoli oraz inne przy uruchomieniu nieinteraktywnym.
# Biblioteki niezbędne do uruchomienia aplikacji
# here
# shiny
# shinyjs
# dplyr
# tidyr
# markdown
# DT
# lubridate

# Biblioteki
#####
library(here) # Biblioteka do znajdywania ścieżki pliku, działa w wersji nieinteraktywnej, w wersji interaktywnej
# odnosi się do aktualnego wd
library(shiny) # Biblioteka aplikacji
library(shinyjs) # Biblioteka do dezaktywowania elementów interfejsu
# library(RODBC) # Biblioteka połączenia ODBC
# library(RJDBC) # Biblioteka połączenia JDBC
library(dplyr) # Biblioteka do manipulacji danymi jak w SQL
library(tidyr) # Biblioteka do manipulacji danymi spread(), gather()
library(markdown) # Biblioteka do wizualizacji tutaj do HTML'a
library(DT) # Biblioteka do widżetu datatable()
library(lubridate) # Biblioteka do funkcji czasowych zawiera w sobie funkcję here() nadpisującą tą z bibliotweki here
#####

# w wersji R 3.6 wystarczało umieścić poniższą linijkę kodu na początku skryptu żeby biblioteka here uwzględniała
# nową przestrzeń roboczą (wd). Jednak od wersji 4.1 nastąpiła jakaś zmiana która sprawia, że biblioteki są zaczytywane
# zanim ta linijka kodu zadziała, co wymusza stworzenie dodatkowej funkcji umożliwiającej poprawne działanie zarówno
# w wersji interaktywnej uruchomionej poprzez RStudio jak i w wersji nieinteraktywnej uruchamianie np. poprzez plik
# wsadowy/batch file
if(interactive()){setwd(dirname(rstudioapi::getActiveDocumentContext()$path))}

fpath <- function(...) {
  if(interactive()) {
    paste(getwd(),..., sep = "/")
  } else {
    here::here(...)
  }
}

# SQL queries
# Prawdziwa wersja takiej aplikacji działałaby na zbiorach danych przechowywanych w bazach danych 
EnterData <- function() {
  paste0("Some SQL Query")
}

DataToComplete <- function() {
  paste0("Some SQL Query")
}

# Dummy data na potrzeby prezentacji działania
DummyDataToComplete <- data.frame(
  Data.Zapadalności = sample(seq(as.Date('2022-01-01'), as.Date('2022-12-31'), by = 'day'), 2000, replace = T),
  Region = rep(1:5, each = 400),
  Oddział = rep(1:20, 100)
)

Data.Wykorzystania = seq(as.Date('2021-01-01'), as.Date('2021-12-31'), by = 'day')
DummyEnterData <- data.frame(
  Data.Wykorzystania = sample(Data.Wykorzystania[
    !(Data.Wykorzystania %in% sample(DummyDataToComplete$Data.Zapadalności - lubridate::years(1), 100))], 20000, replace = T),
  Region = rep(1:5, each = 4000),
  Oddział = rep(1:20, 1000),
  Kwota = runif(20000, 100.00, 400.50), Kupujący = sample(1:100, 20000, replace = T)
) %>%
  mutate(
    Data.Zakupu = Data.Wykorzystania - sample(0:20, 10000, replace = T)
  )

# Additional functions
# Funkcja dająca mode/dominante
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Dodatkowa funkcja obsługująca rozwijane mini tabele z opisem poziomu dopasowania, kod HTML
CallbackJS <- function(number, color) {
  callback <- paste0(
    "var format = function(d) {
      return '<table cellpadding=\"5\" cellspacing=\"0\" border=\"0\" style=\"padding-left:50px;\">' +
        ")
  for (i in 1:number) {
    callback <- paste0(callback, "'<tr>'+
          '<td bgcolor = \"", color[i], "\">Dopasowanie: ", i, "</td>'+
        '</tr>'+
            "
    )
  }
  callback <- paste0(callback, "'</table>';
    };
    
    table.on('click', 'td.details-control', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
        td.html('&#10010');
      } else {
        row.child(format(row.data())).show();
        td.html('&#10006');
      }
    });"
  )
  return(callback)
}

# Input Data
# Tabela zapisywana w miejscu kodu zapamiętująca ostatnie ustawienia dostępnych pokręteł
InputVariables <- tryCatch(
  read.csv2(fpath('UiInput.csv')),
  error = function(e){
    data.frame(Number = c(1, 0, 3, rep(0, 120)))
  })

# Module functions
# Moduł obsługujący slidery
# Dopuki nie zostanie zaznaczona odpowiednia opcja slidery są nieaktywne
SliderForUI_UI <- function(id, values) {
  ns <- NS(id)
  fluidRow(
    column(
      3
      ,sliderInput(ns("January"), "Styczeń", min = -10, max = 10, value = values[1], step = 0.1, post = "%")
      ,sliderInput(ns("February"), "Luty", min = -10, max = 10, value = values[2], step = 0.1, post = "%")
      ,sliderInput(ns("March"), "Marzec", min = -10, max = 10, value = values[3], step = 0.1, post = "%")
    ),
    column(
      3
      ,sliderInput(ns("April"), "Kwiecień", min = -10, max = 10, value = values[4], step = 0.1, post = "%")
      ,sliderInput(ns("May"), "Maj", min = -10, max = 10, value = values[5], step = 0.1, post = "%")
      ,sliderInput(ns("Jun"), "Czerwiec", min = -10, max = 10, value = values[6], step = 0.1, post = "%")
    ),
    column(
      3
      ,sliderInput(ns("July"), "Lipiec", min = -10, max = 10, value = values[7], step = 0.1, post = "%")
      ,sliderInput(ns("August"), "Sierpień", min = -10, max = 10, value = values[8], step = 0.1, post = "%")
      ,sliderInput(ns("September"), "Wrzesień", min = -10, max = 10, value = values[9], step = 0.1, post = "%")
    ),
    column(
      3
      ,sliderInput(ns("October"), "Październik", min = -10, max = 10, value = values[10], step = 0.1, post = "%")
      ,sliderInput(ns("November"), "Listopad", min = -10, max = 10, value = values[11], step = 0.1, post = "%")
      ,sliderInput(ns("December"), "Grudzień", min = -10, max = 10, value = values[12], step = 0.1, post = "%")
    )
  )
}

SliderForUIServer <- function(id, Enable) {
  moduleServer(
    id,
    function(input, output, session) {
      SliderValues <- reactive({
        Values <- c(input$January, input$February, input$March, input$April, input$May, input$Jun, input$July,
                    input$August, input$September, input$October, input$November, input$December)
        Values
      })
      ListOfCategoryMonths <- c("January", "February", "March", "April", "May", "Jun", "July", "August", "September", "October", "November", "December")
      # Obsługa aktywacji/dezaktywacji
      observe({
        if(Enable) for(iToActive in ListOfCategoryMonths) {enable(iToActive)}
        else {for(iToDeactivate in ListOfCategoryMonths) {disable(iToDeactivate)}}
      })
      return(SliderValues)
    }
  )
}

ui <- {fluidPage(
  style="padding-top: 80px;",
  titlePanel("",
             windowTitle = "PKP IC - Kalkulator budżetujący"
  ),
  absolutePanel(
    top = 0,
    left = 0,
    right = 0,
    fixed = FALSE,
    div(
      style="padding: 8px; border-bottom: 1px solid #CCC; background: #0097d9;",
      HTML(
        markdownToHTML(
          fragment.only=TRUE,
          text=c(
            "<b><font color = 'white'>Kalkulator prezentacyjny</font></b>
            
<font color = 'White'>Autor: Rafał Dominik</font>"
          )
        )
      )
    )
  ),
  # useShinyjs() jest niezbędne żeby wykorzystywać obsługę zaimplementowaną w tej bibliotece, w tym przypadku
  # chodzi o aktywację i dezaktywację elementów ui
  useShinyjs(),
  {wellPanel(
    fluidRow(
      column(2,
             numericInput("TimeStep", "Krok czasowy (w dniach)", InputVariables$Number[1], min = 1),
             sliderInput("ddays", "Dopuszczalna minimalna i maksymalna rozbierzność dni między danymi objaśnianymi i objaśniającymi:"
                         ,min = 0, max = 10, value = c(InputVariables$Number[2], InputVariables$Number[3])),
             checkboxInput("RevenueCalculing", "Zmiany wpływów", F),
             checkboxInput("CustomerCalculing", "Zmiany konsumentów", F),
             actionButton("JoyCount", "Oblicz")
      ),
      column(10,
             tabsetPanel(
               id = "JoyTabPanel",
               tabPanel(
                 "Zmiany wpływów",
                 value = "RevenueCalculingTab",
                 {disabled(
                   tabsetPanel(
                     tabPanel("Region 1",
                              SliderForUI_UI("WRegion_1", InputVariables$Number[4:15])
                     ),
                     tabPanel("Region 2",
                              SliderForUI_UI("WRegion_2", InputVariables$Number[16:27])
                     ),
                     tabPanel("Region 3",
                              SliderForUI_UI("WRegion_3", InputVariables$Number[28:39])
                     ),
                     tabPanel("Region 4",
                              SliderForUI_UI("WRegion_4", InputVariables$Number[40:51])
                     ),
                     tabPanel("Region 5",
                              SliderForUI_UI("WRegion_5", InputVariables$Number[52:63])
                     )
                   )
                 )}
               ),
               tabPanel(
                 "Zmiany konsumentów",
                 value = "CustomerCalculingTab",
                 {disabled(
                   tabsetPanel(
                     tabPanel("Region 1",
                              SliderForUI_UI("KRegion_1", InputVariables$Number[64:75])
                     ),
                     tabPanel("Region 2",
                              SliderForUI_UI("KRegion_2", InputVariables$Number[76:87])
                     ),
                     tabPanel("Region 3",
                              SliderForUI_UI("KRegion_3", InputVariables$Number[88:99])
                     ),
                     tabPanel("Region 4",
                              SliderForUI_UI("KRegion_4", InputVariables$Number[100:111])
                     ),
                     tabPanel("Region 5",
                              SliderForUI_UI("KRegion_5", InputVariables$Number[112:123])
                     )
                   )
                 )}
               )
             )
      )
    )
  )},
  h3("Podwójne naciśnięcie na komórkę pozwala ręcznie zmodyfikować jej wartość, przycisk + w każdum wierszu jest interaktywny."),
  fluidRow(
    DTOutput("OutcomeFitTable")
  )
  
)}

server <- function(input, output, session) {
  
  # Wyciągnięcie danych z modułów
  WRegion_1_values <- SliderForUIServer("WRegion_1", input$RevenueCalculing)
  WRegion_2_values <- SliderForUIServer("WRegion_2", input$RevenueCalculing)
  WRegion_3_values <- SliderForUIServer("WRegion_3", input$RevenueCalculing)
  WRegion_4_values <- SliderForUIServer("WRegion_4", input$RevenueCalculing)
  WRegion_5_values <- SliderForUIServer("WRegion_5", input$RevenueCalculing)
  KRegion_1_values <- SliderForUIServer("KRegion_1", input$CustomerCalculing)
  KRegion_2_values <- SliderForUIServer("KRegion_2", input$CustomerCalculing)
  KRegion_3_values <- SliderForUIServer("KRegion_3", input$CustomerCalculing)
  KRegion_4_values <- SliderForUIServer("KRegion_4", input$CustomerCalculing)
  KRegion_5_values <- SliderForUIServer("KRegion_5", input$CustomerCalculing)
  
  
  # Zapisywanie w pliku zewnętrznym zmian w pokrętłach
  observe({
    InputsList <- c(
      input$TimeStep, input$ddays[1], input$ddays[2], WRegion_1_values(), WRegion_2_values(), WRegion_3_values(), WRegion_4_values(),
      WRegion_5_values(), KRegion_1_values(), KRegion_2_values(), KRegion_3_values(), KRegion_4_values(), KRegion_5_values()
    )
    if(sum(InputsList, na.rm = T) != sum(InputVariables$Number, na.rm = T)){
      InputVariables$Number <- InputsList
      write.csv2(InputVariables, fpath('UiInput.csv'), row.names = F)
    }
  })
  
  # Aktywacja, dezaktywacja zmian wpływów
  observeEvent(input$RevenueCalculing, {
    SliderForUIServer("WRegion_1", input$RevenueCalculing)
    SliderForUIServer("WRegion_2", input$RevenueCalculing)
    SliderForUIServer("WRegion_3", input$RevenueCalculing)
    SliderForUIServer("WRegion_4", input$RevenueCalculing)
    SliderForUIServer("WRegion_5", input$RevenueCalculing)
  })
  
  # Aktywacja, dezaktywacja zmian konsumentów
  observeEvent(input$CustomerCalculing, {
    SliderForUIServer("KRegion_1", input$CustomerCalculing)
    SliderForUIServer("KRegion_2", input$CustomerCalculing)
    SliderForUIServer("KRegion_3", input$CustomerCalculing)
    SliderForUIServer("KRegion_4", input$CustomerCalculing)
    SliderForUIServer("KRegion_5", input$CustomerCalculing)
  })
  
  # Przełączenie widoku na odpowiednią zakładkę po zaznaczeniu by były uwzględniane w obliczeniach
  observeEvent(input$RevenueCalculing, {
    if(input$RevenueCalculing) {
      updateTabsetPanel(session, "JoyTabPanel", selected = "RevenueCalculingTab")
    }
  })
  # Jak wyżej
  observeEvent(input$CustomerCalculing, {
    if(input$CustomerCalculing) {
      updateTabsetPanel(session, "JoyTabPanel", selected = "CustomerCalculingTab")
    }  
  })
  # Pierwsza część obliczeń
  JoyCounting <- reactive({
    # Obsługa paska postępu
    withProgress(message = "Przetwarzanie danych, dopasowywanie:", value = 0, {
      n2 <- 8 + ceiling((input$ddays[2] - input$ddays[1])/input$TimeStep)
      m2 <- 1
      # Obsługa błędów
      tryCatch({
        suppressWarnings({
          # Kroki pasku postępu
          incProgress(1/n2, detail = paste("pobieranie danych", paste0(round(m2/n2 * 100, 2), "%")))
          m2 = m2 + 1
          
          ErrorMessage <- "Nie można pobrać danych wejściowych obliczanych"
          
          # w Bardziej rzeczywistej sytuacji dane byłby pobierane z jakiejś bazy danych
          # Wersja z połączeniem ODBC
          # Outcome <- odbcConnect("Sql server name", readOnlyOptimize = TRUE) %>% sqlQuery(DataToComplete())
          # odbcCloseAll()
          
          Outcome <- DummyDataToComplete
          
          incProgress(1/n2, detail = paste("pobieranie danych", paste0(round(m2/n2 * 100, 2), "%")))
          m2 = m2 + 1
          
          ErrorMessage <- "Nie można pobrać danych wejściowych obliczających"
          
          # Wersja z połaczeniem JDBC
          # jdbcdrv<-JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/SpecificUser/Documents/sqldeveloper/sqldeveloper/jdbc/lib/ojdbc6.jar")
          # con <- dbConnect(jdbcdrv, "jdbc:oracle:thin:@//ip:port/ServerName", "login", "password")
          # Income <- dbGetQuery(con, EnterData())
          # dbDisconnect(con)
          
          Income <- DummyEnterData
          
          incProgress(1/n2, detail = paste("obróbka danych", paste0(round(m2/n2 * 100, 2), "%")))
          m2 = m2 + 1
          
          PrimalCalculations <- inner_join(DummyDataToComplete, DummyEnterData, by = c("Region", "Oddział"))
          
          incProgress(1/n2, detail = paste("obróbka danych", paste0(round(m2/n2 * 100, 2), "%")))
          m2 = m2 + 1
          Category <- 1
          MinDayVariance <- input$ddays[1]
          
          # Dopasowanie danych względem wytycznych wyznaczonych przez użytkownika w ui
          if(MinDayVariance == input$ddays[2]) {
            Calculations <- filter(PrimalCalculations, Data.Zapadalności >= (Data.Wykorzystania + lubridate::years(1) - lubridate::days(MinDayVariance))
                                   & Data.Zapadalności <= (Data.Wykorzystania + lubridate::years(1) + lubridate::days(MinDayVariance)))
            
            Calculations[length(Calculations) + 1] <- Category
            colnames(Calculations)[length(Calculations)] <- "Kategoria dopasowania"
            
          } else {
            Calculations <- filter(PrimalCalculations, Data.Zapadalności >= (Data.Wykorzystania + lubridate::years(1) - lubridate::days(MinDayVariance))
                                   & Data.Zapadalności <= (Data.Wykorzystania + lubridate::years(1) + lubridate::days(MinDayVariance)))
            
            TempCalculations <- anti_join(PrimalCalculations, Calculations, by = c("Data.Zapadalności", "Region", "Oddział"))
            
            Calculations[length(Calculations) + 1] <- Category
            colnames(Calculations)[length(Calculations)] <- "Kategoria dopasowania"
            
            MinDayVariance = MinDayVariance + input$TimeStep
            
            while(MinDayVariance < input$ddays[2]) {
              
              incProgress(1/n2, detail = paste("obróbka danych", paste0(round(m2/n2 * 100, 2), "%")))
              m2 = m2 + 1
              if(dim(TempCalculations)[1] != 0) {
                TempCalculations2 <- filter(TempCalculations, Data.Zapadalności >= (Data.Wykorzystania + lubridate::years(1) - lubridate::days(MinDayVariance))
                                            & Data.Zapadalności <= (Data.Wykorzystania + lubridate::years(1) + lubridate::days(MinDayVariance)))
                
                TempCalculations <- anti_join(TempCalculations, TempCalculations2, by = c("Data.Zapadalności", "Region", "Oddział"))
                
                Category <- Category + 1
                TempCalculations2[length(TempCalculations2) + 1] <- Category
                colnames(TempCalculations2)[length(TempCalculations2)] <- "Kategoria dopasowania"
                
                Calculations <- dplyr::union_all(Calculations, TempCalculations2)
              }
              
              MinDayVariance = MinDayVariance + input$TimeStep
            }
            
            incProgress(1/n2, detail = paste("obróbka danych", paste0(round(m2/n2 * 100, 2), "%")))
            m2 = m2 + 1
            if(dim(TempCalculations)[1] != 0) {
              TempCalculations2 <- filter(TempCalculations, Data.Zapadalności >= (Data.Wykorzystania + lubridate::years(1) - lubridate::days(input$ddays[2]))
                                          & Data.Zapadalności <= (Data.Wykorzystania + lubridate::years(1) + lubridate::days(input$ddays[2])))
              
              Category <- Category + 1
              TempCalculations2[length(TempCalculations2) + 1] <- Category
              colnames(TempCalculations2)[length(TempCalculations2)] <- "Kategoria dopasowania"
              
              Calculations <- dplyr::union_all(Calculations, TempCalculations2)
              rm(TempCalculations, TempCalculations2)
            }
          }
          
          incProgress(1/n2, detail = paste("obróbka danych", paste0(round(m2/n2 * 100, 2), "%")))
          m2 = m2 + 1
          
          # Mniej więcej w tym punkcie powinien wystąpić jakiś model predykcji na bazie dopasowanych danych
          # ale ten kalkulator to prezentacja tylko wiedzy na temat języka R
          
          Calculations <- mutate(Calculations, Miesiąc = lubridate::month(Data.Zapadalności))
          
          incProgress(1/n2, detail = paste("obróbka danych", paste0(round(m2/n2 * 100, 2), "%")))
          m2 = m2 + 1
          
          Calculations <- group_by(Calculations, Region, Oddział, Miesiąc) %>%
            summarise(SumKwota = sum(Kwota), SumKupujący = sum(Kupujący),
                      `Kategoria dopasowania` = getmode(`Kategoria dopasowania`), .groups = 'drop')
          
          Category <- max(Calculations$`Kategoria dopasowania`)
          
          incProgress(1/n2, detail = paste("obróbka danych", paste0(round(m2/n2 * 100, 2), "%")))
          
        })
        
        # Jeżeli chce się przypisać kilka obiektów do danego obiektu reactive nałatwiej wykorzystać listę
        JoyCounting <- vector(mode = "list", length = 2)
        
        JoyCounting[[1]] <- Calculations
        JoyCounting[[2]] <- Category
        
        JoyCounting
        
      }, error = function(e) {
        showNotification(paste(ErrorMessage), type = "error", duration = 10)
      })        
      
    })
  })
  
  # Druga część obliczeń, dzięki takiemu podziałowi same zmiany w sliderach nie powodują ponownego wykonywania wszystkich obliczeń
  JoyCounting2 <- eventReactive(input$JoyCount, {
    
    Calculations <- JoyCounting()[[1]]
    Category <- JoyCounting()[[2]]
    
    withProgress(message = "Przetwarzanie danych, poprawki:", value = 0, {
      n3 <- 5 + ifelse(input$RevenueCalculing, 3, 0) + ifelse(input$CustomerCalculing, 3, 0)
      m3 <- 1
      
      incProgress(1/n3, detail = paste("obróbka danych", paste0(round(m3/n3 * 100, 2), "%")))
      m3 = m3 + 1
      
      
      if(input$RevenueCalculing){
        incProgress(1/n3, detail = paste("obróbka danych", paste0(round(m3/n3 * 100, 2), "%")))
        m3 = m3 + 1
        
        Months <- rep(1:12, 5)
        Region <- c(rep(1, 12), rep(2, 12), rep(3, 12), rep(4, 12), rep(5, 12))
        ChangePerMonth <- c(
          WRegion_1_values()/100, WRegion_2_values()/100, WRegion_3_values()/100,
          WRegion_4_values()/100, WRegion_5_values()/100
        )
        ChangePerMonth <- c(
          1/100, 1/100, 1/100,
          1/100, 1/100
        )
        ChangingTable <- data.frame(Months, Region, ChangePerMonth)
        rm(Months, Region, ChangePerMonth)
        
        incProgress(1/n3, detail = paste("obróbka danych", paste0(round(m3/n3 * 100, 2), "%")))
        m3 = m3 + 1
        
        Calculations <- inner_join(Calculations, ChangingTable, by = c("Miesiąc" = "Months", "Region"))
        
        incProgress(1/n3, detail = paste("obróbka danych", paste0(round(m3/n3 * 100, 2), "%")))
        m3 = m3 + 1
        
        Calculations <- mutate(Calculations, SumKwota = SumKwota * (1 + ChangePerMonth))
        Calculations <- select(Calculations, names(Calculations)[1:6])
      }
      
      if(input$CustomerCalculing){
        incProgress(1/n3, detail = paste("obróbka danych", paste0(round(m3/n3 * 100, 2), "%")))
        m3 = m3 + 1
        
        Months <- rep(1:12, 5)
        Region <- c(rep(1, 12), rep(2, 12), rep(3, 12), rep(4, 12), rep(5, 12))
        ChangePerMonth <- c(
          KRegion_1_values()/100, KRegion_2_values()/100, KRegion_3_values()/100,
          KRegion_4_values()/100, KRegion_5_values()/100
        )
        
        ChangingTable <- data.frame(Months, Region, ChangePerMonth)
        rm(Months, Region, ChangePerMonth)
        
        incProgress(1/n3, detail = paste("obróbka danych", paste0(round(m3/n3 * 100, 2), "%")))
        m3 = m3 + 1
        
        Calculations <- inner_join(Calculations, ChangingTable, by = c("Miesiąc" = "Months", "Region"))
        
        incProgress(1/n3, detail = paste("obróbka danych", paste0(round(m3/n3 * 100, 2), "%")))
        m3 = m3 + 1
        
        Calculations <- mutate(Calculations, SumKupujący = SumKupujący * (1 + ChangePerMonth))
        Calculations <- select(Calculations, names(Calculations)[1:6])
      }
      
      Calculations <- mutate(Calculations, ŚredniPrzychód = SumKwota/SumKupujący) %>%
        select('Region', 'Oddział', 'Miesiąc', 'ŚredniPrzychód', 'Kategoria dopasowania')
      
      incProgress(1/n3, detail = paste("obróbka danych", paste0(round(m3/n3 * 100, 2), "%")))
      m3 = m3 + 1
      
      Calculations2 <- spread(select(Calculations, colnames(Calculations)[-4]), Miesiąc, `Kategoria dopasowania`)
      colnames(Calculations2)[3:14] <- paste0("Help", 1:12)
      
      incProgress(1/n3, detail = paste("obróbka danych", paste0(round(m3/n3 * 100, 2), "%")))
      m3 = m3 + 1
      
      Calculations <- spread(select(Calculations, colnames(Calculations)[-5]), Miesiąc, ŚredniPrzychód)
      
      incProgress(1/n3, detail = paste("obróbka danych", paste0(round(m3/n3 * 100, 2), "%")))
      m3 = m3 + 1
      
      Calculations <- inner_join(Calculations, Calculations2, by = c("Region", "Oddział"))
      
      rm(Calculations2)
      
      incProgress(1/n3, detail = paste("obróbka danych", paste0(round(m3/n3 * 100, 2), "%")))
      
      JoyCounting2 <- vector(mode = "list", length = 2)
      
      JoyCounting2[[1]] <- as.data.frame(cbind(' ' = '&#10010', Calculations))
      JoyCounting2[[2]] <- Category
      
      JoyCounting2
    })
  })
  
  # Fragment kodu odpowiadający za wykrywanie ręcznych zmian w wartościach komórek i zapisywanie tych zmian w tabeli
  # widocznej dla użytkownika. Nie jest to tabela występująca w kodzie z której generowana jest tabela do widżetu.
  # Wykorzystywana jest odpowiednia tabela proxy.
  OutcomeFitTableReactiveValue <- reactiveValues(DF = NULL)
  
  observe({
    OutcomeFitTableReactiveValue$DF <- JoyCounting2()[[1]]
  })
  
  OutcomeFitTableProxy <- dataTableProxy("OutcomeFitTable")
  
  # Kontrola gdzie nastąpiła zmiana i podmienienie tabel
  observeEvent(input$OutcomeFitTable_cell_edit, {
    i <- input$OutcomeFitTable_cell_edit$row
    # Tabele w DT mają kolumny numerowane od 0
    j <- input$OutcomeFitTable_cell_edit$col + 1
    v <- input$OutcomeFitTable_cell_edit$value
    
    suppressWarnings(OutcomeFitTableReactiveValue$DF[i[1], j] <<- DT::coerceValue(v, OutcomeFitTableReactiveValue$DF[i[1], j]))
    replaceData(OutcomeFitTableProxy, OutcomeFitTableReactiveValue$DF, resetPaging = F, rownames = FALSE)
  })
  
  output$OutcomeFitTable <- renderDT({
    
    datatable(
      JoyCounting2()[[1]],
      colnames = c(" ", "Region", "Oddział", "Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec",
                   "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień"),
      extensions = c('FixedHeader', 'Buttons'),
      selection = 'none',
      escape = 0,
      editable = list(target = 'row', disable = list(columns = c(0:2, 15:26))),
      options = list(
        paging = F
        ,dom = 'Bfrtip'
        ,width = "100%"
        ,fixedHeader = T
        ,buttons = c('excel')
        # Tabele w DT mają kolumny numerowane od 0
        ,columnDefs = list(list(targets = 15:26, visible = F),
                           list(targets = 3:26, searchable = F),
                           list(orderable = FALSE, className = 'details-control', targets = 0))
      )
      # hcl.colors nie obsługuje liczby 1
      ,callback = JS(CallbackJS(JoyCounting2()[[2]], if(JoyCounting2()[[2]] == 1) {
        '#008000'
      } else
      {hcl.colors(JoyCounting2()[[2]], palette = "Zissou 1")}))
      ,rownames = F
    ) %>% formatStyle(
      1, cursor = 'pointer'
    ) %>% formatStyle(
      names(JoyCounting2()[[1]])[4:15]
      ,names(JoyCounting2()[[1]])[16:27]
      ,backgroundColor = styleEqual(1:JoyCounting2()[[2]], if(JoyCounting2()[[2]] == 1) {
        '#008000'
      } else
        {hcl.colors(JoyCounting2()[[2]], palette = "Zissou 1")})
    ) %>% formatRound(
      names(JoyCounting2()[[1]])[4:15]
      ,digits = 2
    )
  })
  
  # Wyłącznie strony w przeglądarce zatrzyma też aplikację
  session$onSessionEnded(function(){
    stopApp()
  })
  
}

RDapp <- shinyApp(ui = ui, server = server)

# Uruchomienie w przeglądarce internetowej
runApp(RDapp, launch.browser = T)
