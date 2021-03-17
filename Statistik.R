library(shiny)
library(tidyverse)
library(readxl)
library(cowplot)
library(shinythemes)
library(lubridate)
library(janitor)

master = read.csv("/Users/huber.michael/Dropbox/Repositories/DIA_Stats/master_table.csv")
blocklist = read.csv("/Users/huber.michael/Dropbox/Repositories/DIA_Stats/blocklist.csv") %>% pull(Code)
path = "/Users/huber.michael/Dropbox/Repositories/DIA_Stats/Data/"

export_raw =
    list.files(path, pattern = "*.xls*", full.names = TRUE) %>% 
    map_df(~read_excel(.)) %>%
    clean_names(case = "parsed")

data =
    export_raw %>%
    filter(!is.na(Anforderungsnr),
           !is.na(Patientennummer),
           !is.na(RES)) %>%
    # only keep one entry per test and patient sample
    unique() %>%
    select(Einsender, MC, Eingangsdatum) %>%
    mutate(Eingangsdatum = as.Date(Eingangsdatum, "%d.%m.%Y")) %>%
    # filter(Eingangsdatum >= input$date[1] & Eingangsdatum <= input$date[2]) %>%
    select(-Eingangsdatum) %>%
    rename(Code = MC) %>%
    left_join(. , master) %>%
    #filter(!(is.na(Analyse))) %>%
    
    ### remove MCs (MATD, cohort)
    filter(!Code %in% blocklist) %>%
    
    ### assign new code to anonymous HIV tests
    mutate(Code = ifelse(Einsender == "HIVANO" & Code == "H06USD", "XXX", Code)) %>%
    mutate(Code = ifelse(Einsender == "HIVANO" & Code == "H06UST", "XXX", Code)) %>%
    
    ### assign new code to SARS PCR C23F/G*
    mutate(Code = ifelse(Code %in% c("C23GK", "C23GKA", "C23GKU", "C23FKP", "C23FPU"), "C23FK", Code)) 


#%>% filter(is.na(Analyse)) %>% pull(Code) %>% unique()

Bereich = data %>%
    mutate(type = paste(Arbeitsbereich, Unterbereich, Weitere_Unterteilung)) %>%
    group_by(type) %>%
    mutate(Subtotal = n()) %>%
    sample_n(1) %>%
    group_by(Arbeitsbereich) %>%
    mutate(Total = sum(Subtotal)) %>%
    ungroup() %>%
    mutate(Gesamttotal = sum(Subtotal)) %>%
    select(-Einsender, -type, -Code, -Pathogen, -Analyse)
write_excel_csv(Bereich, paste0(path, "Bereich.csv"))

Verfahren = data %>%
        group_by(Code) %>%
        mutate(Total = n()) %>%
        sample_n(1) %>%
        ungroup() %>%
        arrange(Pathogen) %>%
        select(Pathogen, Analyse, Total)
write_excel_csv(Verfahren, paste0(path, "Verfahren.csv"))
    
plot = Bereich %>%
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

plot
ggsave(paste0(path, "Plot.jpeg"), plot)
