library("shiny")
#library("dplyr")
library("ggplot2")
library('scales')

# -------------------------- Dodatkowe dane ---------------------------
df4 <- data.frame(year=c(1997, 1998, 1999, 2000, 2001), cnt=c(25, 30, 20, 10, 60))
df5 <- data.frame(value=c(11, 42, 5, 42), Item=c('A', 'B', 'C', 'D'))


p5_g <- ggplot(df5, aes(x="", y=value, fill=Item)) + 
    geom_bar(width = 1, stat='identity') +
    coord_polar('y', start=0) + 
    theme_minimal()+
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
    ) +
    theme(axis.text.x=element_blank()) +
    geom_text(aes(y = 100 - cumsum(value) + 0.5*value, 
                  label = percent(value/100)), size=5)



# -------------------- Wykresy złe  --------------------
# list("<typ>", "<pytanie>", <odpowiedź>, <wykres/url>)
p1_bad <- list(
    "plot", "Ile jest tu czerwonych kropek?", 3,
    ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_point())
p2_bad <- list(
    "plot", "?", "odp",
    ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point())
p3_bad <- list(
    "img", "?", "odp",
    "https://matplotlib.org/3.1.0/_images/sphx_glr_simple_plot_0011.png")
p4_bad <- list(
    "img", "Ile wynosi CNT w 1998r", 30,
    "https://support.sas.com/kb/24/addl/fusion_24875_1_g24875.gif")
p5_bad <- list(
    "img", "Porównaj B względem D (odp: '1': >, '2': <, '3': =)", "3",
    "https://upload.wikimedia.org/wikipedia/commons/8/88/Misleading_Pie_Chart.png")
p6_bad <- list(
    "plot", "?", "odp",
    NULL)
p7_bad <- list(
    "plot", "?", "odp",
    NULL)
p8_bad <- list(
    "plot", "?", "odp",
    NULL)


# -------------------- Wykresy poprawne --------------------
p1_good <- list(
    "plot", "Ile jest różowych kropek?", 15,
    ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) + geom_point())
p2_good <- list(
    "plot", "?", "odp",
    NULL)
p3_good <- list(
    "plot", "?", "odp",
    NULL)
p4_good <- list(
    "plot", "Ile wynosi CNT w 1998r", 30,
    ggplot(df4, aes(x=year, y=cnt)) + geom_bar(stat='identity'))
p5_good <- list(
    "plot", "Porównaj B względem D (odp: '1': >, '2': <, '3': =)", "3",
    p5_g)
p6_good <- list(
    "plot", "?", "odp",
    NULL)
p7_good <- list(
    "plot", "?", "odp",
    NULL)
p8_good <- list(
    "img", "?", "odp",
    "https://www.mathworks.com/help/examples/graphics/win64/CreateLinePlotExample_01.png")



plots <- list(
    p1_bad, p2_bad, p3_bad, p4_bad, p5_bad, p6_bad, p7_bad, p8_bad,
    p1_good, p2_good, p3_good, p4_good, p5_good, p6_good, p7_good, p8_good
)


# -------------------- Zdefiniowanie układu strony -------------------- 
input_fields <- c("input_n")  # nazwy pól które użytkownik aktualizuje 

ui <- fluidPage(
    sidebarLayout(
        
        # Panel boczny z polem do wpisywania odczytów
        sidebarPanel(
            numericInput("input_n", "Odpowiedź", value = NULL),  # Pole do wprowadzenia wartości
            actionButton("save_input", "Zapisz"),             # Zapisanie wpisanej wartości
            actionButton("clear_last_input", "Cofnij"),
            h3("Podane odpowiedzi"),
            tableOutput("user_inputs"),
            actionButton("clear_input", "Wyczyść odpowiedzi")       # Wyczyszczenie zapisanych wartości
        ),
        
        mainPanel(
            h1("Title"),
            textOutput("plot_number"),
            textOutput("plot_question"),
            textOutput("score"),
            tableOutput("score_table"),
            uiOutput("current_img"),
            plotOutput("current_plot")
        )
    )
)


server <- function(input, output, session) {
    
    # Reactive w którym trzymamy odpowiedzi użytkownika
    reVals <- reactiveValues(answers = NULL, current_plot = 1)
    
    # Zapisywanie odpowiedzi użytkownika
    saveData <- function(data) {
        if (length(data) == 1) { data <- t(data) }
        data <- as.data.frame(data)
        if (is.null(reVals[["answers"]])) {
            reVals[["answers"]] <- data
        } else {
            reVals[["answers"]] <- rbind(reVals[["answers"]], data)
        }
        colnames(reVals[["answers"]]) <- "input_n"
    }
    
    # Czyszczenie całości / ostatniego wiersza odpowiedzi użytkownika
    clearData <- function(last = FALSE) {
        # Domyślnie kasujemy zapisane wartości, dodatkowe dwie opcje to obsłużenie pojedynczej odpowiedzi i wyczyszczonych odpowiedzi
        if (!last || is.null(reVals[["answers"]]) || nrow(reVals[["answers"]]) == 1) {
            reVals[["answers"]] <- NULL 
        } else {
            answers_old <- reVals[["answers"]][1:(nrow(reVals[["answers"]])-1), ]  # Zostawiamy wiersze oprócz ostatniego
            names(answers_old) <- rep("input_n", length(answers_old))              # Nadajemy nazwy, żeby zachować spójność
            clearData()                                                            # Czyścimy obecnie zapisane dane
            saveData(answers_old)                                                  # Zapisujemy zachowane wiersze
        }
    }
    
    # Funkcja do wypisywania tabeli z odpowiedziami użytkownika
    printInputTable <- function() {
        output[["user_inputs"]] <- renderTable({
            user_data <- reVals[["answers"]]
            validate(need(nrow(user_data) > 0, "Lista odpowiedzi jest pusta"))
            user_data_formatted <- cbind(paste("Wykres", 1:nrow(user_data)), user_data)
            names(user_data_formatted) <- c("", "Odpowiedź")
            user_data_formatted
        })
    }
    
    # Funkcja rysująca kolejne strony
    printPlot <- function(plot_change = NULL, score = FALSE) {
        current_plot <- isolate(reVals[["current_plot"]]) # odczytanie wartości z reactive'a
        if (is.null(plot_change) || (current_plot + plot_change == 0)) {  # Pierwsze użycie i cofanie się z pierwszego wykresu
            current_plot  <- 1
        } else {
            current_plot  <- current_plot  + plot_change
        }
        reVals[["current_plot"]] <- current_plot  # Zapisanie zaktualizowanej wartości do reactive'a
        
        if (score) {
            output[["current_plot"]]  <- NULL
            output[["current_img"]] <- NULL
            output[["plot_number"]] <- NULL
            output[["plot_question"]] <- NULL
        } else {

            # Numer wykresu i pytanie do niego
            output[["plot_number"]] <- renderText(paste("Wykres nr", current_plot))
            output[["plot_question"]] <- renderText(plots[[current_plot]][[2]])
            
            # Wyświetlamy obrazek, wykres albo nic (na koniec)
            if (plots[[current_plot]][[1]] == "plot" && !score) {
                output[["current_plot"]] <- renderPlot({ plots[[current_plot]][[4]] })
                output[["current_img"]] <- NULL
            } else if (plots[[current_plot]][[1]] == "img" && !score) {
                output[["current_plot"]]  <- NULL
                output[["current_img"]] <- renderUI({ tags[["img"]](src = plots[[current_plot]][[4]]) })
            }
        }
    }
    
    # Początkowe pokazanie się tabelki i wykresu
    printInputTable()
    printPlot()
    
    # Odczytuje dane wprowadzone przez użytkownika
    getUserData <- reactive({ sapply(input_fields, function(x) input[[x]]) })
    
    # Liczba udzielonych odpowiedzi
    getAnswersNumber <- reactive({ ifelse(is.null(reVals[["answers"]]), 0, nrow(reVals[["answers"]])) }) 
    
    # Zapisuje podaną wartość
    observeEvent(input[["save_input"]], {
        current_plot <- reVals[["current_plot"]]
        if (getAnswersNumber() < 16) { saveData(getUserData()) } # Po 16 przestajemy przyjmować input
        
        printInputTable()                                                                     # Musimy odświeżać po kliknięciach
        printPlot(
            plot_change = current_plot <= 16,  # ostatnia zmiana na current_plot = 17, żeby cofanie dobrze działało
            score = getAnswersNumber() == 16)      # jeśli mamy komplet odpowiedzi to wykres
        
    }) 
    
    # Czyści ostatnią podaną wartość
    observeEvent(input[["clear_last_input"]], {
        clearData(last = TRUE)
        printInputTable()
        printPlot(plot_change = -1)
    })
    
    # Czyści podane wartości
    observeEvent(input[["clear_input"]], {
        clearData()
        printInputTable()
        printPlot()
    })
    
    score <- reactive({
        validate(need(getAnswersNumber() == 16, ""))  # Zapewnienie, że liczba odpowiedzi jest taka jak trzeba
        
        user_answers <- isolate(reVals[["answers"]])
        plot_names <- paste("Wykres", 1:16)
        correct_answers <- sapply(plots, function(p) p[[3]])
        
        answers_correctness <- c("Źle... :(", "OK! :)")[1 + (user_answers == correct_answers)]
        
        score <- as.data.frame(cbind(plot_names, correct_answers, user_answers, answers_correctness))
        colnames(score) <- c("", "Poprawny wynik", "Podana odpowiedź", "Zaliczone?")
        score
    })
    
    
    # Wypisanie wyników
    output[["score_table"]] <- renderTable({
        score()
    })
    
    output[["score"]] <- renderText({
        paste("Liczba poprawnych odpowiedzi:", sum(score()["Zaliczone?"] == "OK! :)"))
    })
    
}

shinyApp(ui = ui, server = server)
