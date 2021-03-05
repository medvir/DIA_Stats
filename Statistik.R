library(shiny)
library(tidyverse)
library(readxl)
library(cowplot)
library(shinythemes)
library(lubridate)


master = read.csv("~/Documents/R-Statistiken/Beispiel R Michael/master_table.csv")
raw_data = read_excel("~/Documents/R-Statistiken/Beispiel R Michael/test_export.xlsx")


data = raw_data %>%
    select(Einsender, MC, Eingangsdatum) %>%
    mutate(Eingangsdatum = as.Date(Eingangsdatum, "%d.%m.%Y")) %>%
    rename(Code = MC) %>%
    left_join(. , master) %>%
    filter(!(is.na(Analyse))) %>%
    
    ### assign new code to anonymous HIV tests
    mutate(Code = ifelse(Einsender == "HIVANO" & Code == "H06USD", "XXX", Code)) %>%
    mutate(Code = ifelse(Einsender == "HIVANO" & Code == "H06UST", "XXX", Code)) %>%
    
    ### assign new code to SARS PCR C23F*
    mutate(Code = ifelse(Code == "C23FKP", "C23FK", Code)) %>%
    mutate(Code = ifelse(Code == "C23FPU", "C23FK", Code)) #%>%

    ### filter out SARS
    #filter(!Code %in% c("C23FK", "C23RS", "C23PS"))

data_filtered = data %>%
#     filter(Eingangsdatum >= input$date[1] & Eingangsdatum <= input$date[2]) %>%
     select(-Eingangsdatum)


Bereich = data_filtered %>%
    mutate(type = paste(Arbeitsbereich, Unterbereich, Weitere_Unterteilung)) %>%
    group_by(type) %>%
    mutate(Subtotal = n()) %>%
    sample_n(1) %>%
    group_by(Arbeitsbereich) %>%
    mutate(Total = sum(Subtotal)) %>%
    ungroup() %>%
    mutate(Gesamttotal = sum(Subtotal)) %>%
    select(-Einsender, -type, -Code, -Pathogen, -Analyse)


Verfahren = data_filtered %>%
        group_by(Code) %>%
        mutate(Total = n()) %>%
        sample_n(1) %>%
        ungroup() %>%
        arrange(Pathogen) %>%
        select(Pathogen, Analyse, Total)
    
    
Plot = Bereich %>%
            mutate(Arbeitsbereich = case_when(
                Arbeitsbereich == "PCR-basierte Analytik" ~ "PCR",
                Arbeitsbereich == "Sequenzbasierte Analytik" ~ "Sequenz",
                TRUE ~ as.character(Arbeitsbereich)
            )) %>%
            ggplot(aes(x = Unterbereich, y = Subtotal, fill = Arbeitsbereich)) +
            geom_bar(stat="identity") +
            facet_grid(Arbeitsbereich ~ ., scales = "free") +
            panel_border() +
            background_grid(major = "xy") +
            xlab("") + ylab("") +
            theme(legend.position="none") +
            coord_flip() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

Plot
