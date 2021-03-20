# 1.0 Libraries ----
# Interactive ----
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(commonmark)
library(httr)
# Core ----
library(tidyverse)
library(lubridate)
library(tidyquant)
library(googlesheets4)
library(googlesheets)
library(googledrive)
library(geofacet)
library(gghighlight)
library(data.table)
library(memoise)
library(sp)
# Visualizations ----
library(plotly)
library(maps)
library(leaflet)
library(DT)

# pre-authorization 
options(gargle_oauth_cache = ".secrets")
gs4_auth(cache = ".secrets", "sibacon360@gmail.com")








# 2.0 Metadata ----
metadata <- readr::read_csv2('https://www.data.gouv.fr/fr/datasets/r/3f0f1885-25f4-4102-bbab-edec5a58e34a')

metadata_tbl <- metadata %>% 
    select(Colonne, Description_EN) %>% 
    rename(
        col_name    = Colonne,
        description = Description_EN
    )








# 3.0 French COVID Department Data ----
# Reading in data from data.gouv.fr this is from a french government website
#covid_department_dataset <- readr::read_csv2("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7")
#new_dat <- read_csv2('https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7')



hospital_new <- read_csv2('https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c')







# 4.0 Other Data of Interest ----
#new_dat_age <- read_csv2('https://www.data.gouv.fr/fr/datasets/r/08c18e08-6780-452d-9b8c-ae244ad529b3')
#test_results_dep <- read_csv2('https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675,col_types = cols(pop = col_character()))

# memory.limit()
# memory.limit(size=56000)
# join_age_test_results_tbl <- new_dat_age %>% 
#     left_join(test_results_dep, by=c('jour'='jour')) %>% View
# 
# join_age_test_results_tbl







# 5.0 French Department Data ----
# Getting France departments from a web link to a Google sheet


departments <-
    range_read("1hKY0kjzLm55q1R2jD7FmbRkC6XTb0El4oEbVvi98pwo",
                sheet = "departement")

# 6.0 French Regions Data ----
# Getting France regions from a web link to a Google sheet

regions <-
    range_read("1hKY0kjzLm55q1R2jD7FmbRkC6XTb0El4oEbVvi98pwo",
                sheet = "region",)









# 7.0 Joining French Department, Regions, and COVID Department Datasets ----
covid_new_tbl <- hospital_new  %>%
    semi_join(departments,by = c("dep" = "code")) %>%
    left_join(regions,by = c("dep" = "code")) %>%
    arrange(dep)


# covid_join_reg_dep <- new_dat %>% 
#     semi_join(departments,by = c("dep" = "code")) %>%
#     left_join(regions,by = c("dep" = "code")) %>% 
#     #filter(sexe == 0) %>% 
#     arrange(dep)



#covid_new_tbl %>% 
  #  write_csv('covid_data_tbl',append = TRUE,col_names = TRUE)

#covid_new_tbl <- read_csv('covid_data_tbl')





# 7.0 Plotting Bar Plots ----
covid_daily_func <- function(data,column = 'incid_hosp'){
    
     vars_expr <- rlang::sym(column)
    
        metrics_tbl <- data %>%
            group_by(jour) %>% 
            summarise(daily_total = sum(!! vars_expr)) %>% 
            ungroup()
      
        return(metrics_tbl)
    
}


plot_daily_deaths <- function(data, calc = 'incid_dc'){
    
    if(calc == 'incid_dc'){
        min_date <- data %>% 
            pull(jour) %>% 
            min()
        max_date <- data %>% 
            pull(jour) %>% 
            max()
    
    g <- data %>% 
    mutate(label_text = str_glue('Date: {jour}
                                  Deaths: {daily_total}')) %>% 
    ggplot(aes(jour,daily_total),color = 'white') +
    geom_bar(aes(text = label_text),stat="identity",fill = '#2c3e50') +
    geom_smooth(size = 1, color = 'red',method = 'loess',se = FALSE, span = 0.2) +
    theme_tq() +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%Y") + 
    theme(
         axis.text.x  = element_text(angle = 45,hjust = 0.9)
        ,legend.title = element_blank()
        )  +
    labs(
         y = 'Daily Hospital Deaths'
        ,x = 'Date'
        ,title = 'French Daily Covid-19 Deaths'
        ,subtitle = str_glue('Deaths Occrrued While Hospitalized: {min_date} - {max_date}')
    )


   gply <- ggplotly(g, tooltip = "text",height = 450,width = 1000) %>%
       layout(title = list(text = paste0('French Daily Covid-19 Deaths',
                                    '<br>',
                                    '<sup>',
                                    str_glue('Deaths Occrrued While Hospitalized {min_date} - {max_date}'),
                                    '</sup>')))
   return(gply)
   
    }else if (calc == 'incid_hosp'){
        min_date <- data %>% 
            pull(jour) %>% 
            min()
        max_date <- data %>% 
            pull(jour) %>% 
            max()
        
        g <- data %>% 
    mutate(label_text = str_glue('Date: {jour}
                                  Deaths: {daily_total}')) %>% 
    ggplot(aes(jour,daily_total),color = 'white') +
    geom_bar(aes(text = label_text),stat="identity",fill = '#2c3e50') +
    geom_smooth(size = 1, color = 'red',method = 'loess',se = FALSE, span = 0.2) +
    theme_tq() +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%Y") + 
    theme(
         axis.text.x  = element_text(angle = 45,hjust = 0.9)
        ,legend.title = element_blank()
        )  +
    labs(
         y = 'Daily Hospitaliztions'
        ,x = 'Date'
        ,title = 'French Daily Covid-19 Hospitaliztions'
        ,subtitle = str_glue('Daily Total Hospitaliztions From: {min_date} - {max_date}')
    )


   gply <- ggplotly(g, tooltip = "text",height = 450,width = 1000) %>%
       layout(title = list(text = paste0('French Daily Covid-19 Hospitaliztions',
                                    '<br>',
                                    '<sup>',
                                    str_glue('Daily Total Hospitaliztions From: {min_date} - {max_date}'),
                                    '</sup>')))
   return(gply)
       
    }else if (calc == 'incid_rea'){
        min_date <- data %>% 
            pull(jour) %>% 
            min()
        max_date <- data %>% 
            pull(jour) %>% 
            max()
        
        g <- data %>% 
    mutate(label_text = str_glue('Date: {jour}
                                  Deaths: {daily_total}')) %>% 
    ggplot(aes(jour,daily_total),color = 'white') +
    geom_bar(aes(text = label_text),stat="identity",fill = '#2c3e50') +
    geom_smooth(size = 1, color = 'red',method = 'loess',se = FALSE, span = 0.1) +
    theme_tq() +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%Y") + 
    theme(
         axis.text.x  = element_text(angle = 45,hjust = 0.9)
        ,legend.title = element_blank()
        )  +
    labs(
         y = 'Daily ICU Patients'
        ,x = 'Date'
        ,title = 'French Daily Covid-19 ICU Patients'
        ,subtitle = str_glue('Daily Total ICU Patients From: {min_date} - {max_date}')
    )


   gply <- ggplotly(g, tooltip = "text",height = 450,width = 1000) %>%
       layout(title = list(text = paste0('French Daily Total Covid-19 ICU Patients',
                                    '<br>',
                                    '<sup>',
                                    str_glue('Daily Total ICU Patients From: {min_date} - {max_date}'),
                                    '</sup>')))
   return(gply)
       
    }else if (calc == 'incid_rad'){
        min_date <- data %>% 
            pull(jour) %>% 
            min()
        max_date <- data %>% 
            pull(jour) %>% 
            max()
        
        g <- data %>% 
    mutate(label_text = str_glue('Date: {jour}
                                  Deaths: {daily_total}')) %>% 
    ggplot(aes(jour,daily_total),color = 'white') +
    geom_bar(aes(text = label_text),stat="identity",fill = '#2c3e50') +
    geom_smooth(size = 1, color = 'red',method = 'loess',se = FALSE, span = 0.2) +
    theme_tq() +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%Y") + 
    theme(
         axis.text.x  = element_text(angle = 45,hjust = 0.9)
        ,legend.title = element_blank()
        )  +
    labs(
         y = 'Daily Recovered Patients'
        ,x = 'Date'
        ,title = 'French Daily Recovered Covid-19 Patients'
        ,subtitle = str_glue('Daily Recovered Covid-19 Patients From: {min_date} - {min_date}')
    )


   gply <- ggplotly(g, tooltip = "text",height = 450,width = 1000) %>%
       layout(title = list(text = paste0('French Daily Recovered Covid-19 Patients',
                                    '<br>',
                                    '<sup>',
                                    str_glue('Daily Recovered Covid-19 Patients From: {min_date} - {max_date}'),
                                    '</sup>')))
   return(gply)
       
    }else{
       warning('Error in Daily hist plotting function!!!!')
   }


    
}



daily_deaths <- covid_daily_func(covid_new_tbl,column = 'incid_hosp') %>% 
    plot_daily_deaths(calc = 'incid_hosp')

#daily_deaths


   



        

# 8.0 Data Pre-Processing & Analysis ----
gender_based_analysis_tbl <- function(data){
    
    data %>%
    # name column to all lower case    
    mutate(name = name %>% str_to_lower()) %>%
    arrange(name) %>%
        
    # Removing the hyphen from the department names 
    mutate(name = case_when(
        name == "ardèche"              ~ "ardeche",
        name == "ariège"               ~ "ariege",
        name == "bouches-du-rhône"     ~ "bouches-du-rhone",
        name == "corrèze"              ~ "correze",
        name == "côte-d'or"            ~ "cote-dor",
        name == "côtes-d'armor"        ~ "cotes-darmor",
        name == "deux-sèvres"          ~ "deux-sevres",
        name == "drôme"                ~ "drome",
        name == "finistère"            ~ "finistere",
        name == "haute-saône"          ~ "haute-saone",
        name == "hautes-pyrénées"      ~ "hautes-pyrenees",
        name == "hérault"              ~ "herault",
        name == "isère"                ~ "isere",
        name == "lozère"               ~ "lozere",
        name == "nièvre"               ~ "nievre",
        name == "val-d'oise"           ~ "val-doise",
        name == "rhône"                ~ "rhone",
        name == "pyrénées-orientales"  ~ "pyrenees-orientales",
        name == "saône-et-loire"       ~ "saone-et-loire",
        name == "corse-du-sud"         ~ "corse du sud",
        name == "essone"               ~ "essonne",
        name == "puy-de-dôme"          ~ "puy-de-dome",
        name == "sartre"               ~ "sarthe",
        name == "ainse"                ~ "aisne",
        name == "pyrénées-altantiques" ~ "pyrenees-atlantiques",
        name == "vandée"               ~ "vendee",
        TRUE ~ name

    )) %>% 
        
        # Removing the hyphen from the Region names 
        mutate(region = case_when(
            region == "Auvergne-Rhône-Alpes"    ~ "Auvergne-Rhone-Alpes",
            region == "Bourgogne-Franche-Comté" ~ "Bourgogne-Franche-Comte",
            region == "Île-de-France"           ~ "Ile-de-France",
            TRUE ~ region
           
        )) %>% 
        
   
    # Data pre-processing on the data column replacing / with -
    mutate(jour = jour %>% str_replace("/", "-")) %>%
    mutate(jour = jour %>% str_replace("/", "-")) %>%
    
    
    # Converting jour column to date data type 
    mutate(jour = jour %>% ymd())  
        
  
}

memoised_all_gender <- gender_based_analysis_tbl(covid_new_tbl)

#memoised_all_gender %>% pull(name) %>% unique()





# 10.0 Daily Covid calculations by population or by total hospitalized ----

covid_calculations_daily <- function(data
                                     ,column ='incid_hosp'){
    
    
        
    
        total_hospitalized <- data %>% 
        group_by(dep,name,inhabitant) %>% 
        summarise(
             total_hosp_dep = sum(incid_hosp)
            ,total_rea_dep  = sum(incid_rea)
            ,total_rad_dep  = sum(incid_rad)
            ,total_dc_dep   = sum(incid_dc)
            ,.groups  = 'drop' 
            )
        
        vars_expr <- rlang::sym(column)
    
    if(column == 'incid_hosp'){
         total_hospitalized %>%
            arrange(name) %>% 
            select(dep,name,inhabitant,total_hosp_dep)%>% 
            rename(total_dep = total_hosp_dep)
    } 
    else if (column == 'incid_rea'){
        
         total_hospitalized %>% 
            mutate(total_pct_rea_dep = total_rea_dep / total_hosp_dep) %>%
            arrange(name) %>% 
            select(dep,name,inhabitant,total_rea_dep,total_hosp_dep,total_pct_rea_dep) %>% 
            rename(total_dep = total_rea_dep,total_pct_dep = total_pct_rea_dep)
        
    }
    else if (column == 'incid_dc'){
        
        total_hospitalized %>% 
            mutate(total_pct_dc_dep = total_dc_dep / total_hosp_dep) %>%
            arrange(name) %>% 
            select(dep,name,inhabitant,total_dc_dep,total_hosp_dep,total_pct_dc_dep) %>% 
            rename(total_dep = total_dc_dep,total_pct_dep = total_pct_dc_dep)

        
    }
    else if (column == 'incid_rad'){
        
        total_hospitalized %>% 
            mutate(total_pct_rad_dep = total_rad_dep / total_hosp_dep) %>%
            arrange(name) %>% 
            select(dep,name,inhabitant,total_rad_dep,total_hosp_dep,total_pct_rad_dep)%>% 
            rename(total_dep = total_rad_dep,total_pct_dep = total_pct_rad_dep)
        

        
    }
    else{
        warning("Wrong Variable Names!!!!")
    }

        
    
    
    
}

 covid_data_name_tbl <- covid_calculations_daily(memoised_all_gender
                                                 ,column ='incid_rea')
 covid_data_name_tbl 



 
 

 
 
# 11.0 Daily Covid calculations  ----

covid_daily_sums <- function(data
                             ,column ='incid_hosp'){
    
    
        total_hospitalized <- data %>% 
        group_by(dep,name,inhabitant) %>% 
        summarise(
             total_hosp_dep = sum(incid_hosp)
            ,total_rea_dep  = sum(incid_rea)
            ,total_rad_dep  = sum(incid_rad)
            ,total_dc_dep   = sum(incid_dc)
            ,.groups  = 'drop' 
            )

    if(column == 'incid_hosp'){
         total_hospitalized %>%
            arrange(name) %>% 
            select(dep,inhabitant,name,total_hosp_dep)
    } 
    else if (column == 'incid_rea'){
        
         total_hospitalized %>% 
            arrange(name) %>% 
            select(dep,inhabitant,name,total_rea_dep)
        
    }
    else if (column == 'incid_dc'){
        
        total_hospitalized %>% 
            
            arrange(name) %>% 
            select(dep,inhabitant,name,total_dc_dep)

        
    }
    else if (column == 'incid_rad'){
        
        total_hospitalized %>% 
            arrange(name) %>% 
            select(dep,inhabitant,name,total_rad_dep)
    }
    else{
        warning("Wrong Variable Names!!!!")
    }

    
}

 covid_daily_sums_tbl <- covid_daily_sums(memoised_all_gender
                                        ,column ='incid_rea')
 covid_daily_sums_tbl 

 
 
 
 

 
 
 
 
# 12.0 Leaflet Sum Total Plots ----
 
leaflet_plot_func_2 <- function(data,
                               cols = 'incid_rea'
                              ,change_1 = 'total_rea_dep'){
     
    
    
    
    
    var_change_expr_1 <- enquo(change_1)
    
    data <- covid_daily_sums(
         data
        ,column = cols) %>% 
        
    rename(
         total_dep  = (!! var_change_expr_1)
    )
    
    
    
    
    
    # Getting France geo-codes from the map_data() function 
    france_data_name <- map_data('france') %>%
        tibble() %>% 

        dplyr::select(-subregion,-order) %>%
        mutate(group = group %>% as.character()) %>%
        rename(name = region) %>% 

        mutate(name = name %>% str_to_lower()) %>%
        left_join(data, by = c("name" = "name")) %>%
        mutate(name = name %>% str_to_title())
    
    
    # Removing small island polygons
    # This is rows removed based off the group numbers
    # Removed because had issue with polygons making line 
    # connections from the island to land 
    dat <- france_data_name %>% 
        rowid_to_column() %>% 
        filter(group != 49  & group != 53  & group != 54  & 
               group != 55  & group != 37  & group != 29  & 
               group != 4   & group != 27  & group != 109 & 
               group != 110 & group != 111 & group != 72  &
               group != 75  & group != 62  & group != 64)
    
    
    
    # Function for converting dataframe to spatial polygon dataframe
    polyFunc<-function(groupname, dat){
      poly <- dat %>% 
          filter(name == groupname) %>% 
          dplyr::select(long,lat)
      return(Polygons(list(Polygon(poly)), groupname))
    }
    

    # Getting variables from dataframe to be converted into spatial polygon dataframe
    tracts <- distinct(dat,name,inhabitant,total_dep)

    # getting department names 
    tractname <- tracts$name

    # Applying the polyFunc() function to convert all department names into SPD
    polygons <- lapply(tractname, function(x) polyFunc(x, dat=dat)) 
    sp.polygon <- SpatialPolygons(polygons)
    df.polygon <- SpatialPolygonsDataFrame(sp.polygon, 
                                         data=data.frame(row.names=tractname, tracts))

    # Calculating the quantities of total percentages of ICU patients 
    quan <- df.polygon$total_dep %>% 
        quantile(probs = seq(0, 1, length = 9)) %>% 
        as_tibble()  

    # Creating 10 bins 
    bins <- c(max(df.polygon$total_dep),quan[8,] %>% pull()
        ,quan[7,] %>% pull(),quan[6,] %>% pull(),quan[5,] %>% pull()
        ,quan[4,] %>% pull(),quan[3,] %>% pull(),quan[2,] %>% pull()
        ,quan[1,] %>% pull(),0)
    
    if(cols == 'incid_rea'){
        # maps data values to colors according to a given palette
    pal <- colorBin("YlOrRd", domain = df.polygon$total_dep, bins = bins)
        
    }else if (cols == 'incid_hosp'){
        # maps data values to colors according to a given palette
    pal <- colorBin("PuRd", domain = df.polygon$total_dep, bins = bins)
        
    }else if (cols == 'incid_rad'){
        # maps data values to colors according to a given palette
    pal <- colorBin("YlGn", domain = df.polygon$total_dep, bins = bins)
        
    }else if (cols == 'incid_dc'){
        # maps data values to colors according to a given palette
    pal <- colorBin("OrRd", domain = df.polygon$total_dep, bins = bins)
        
    }else{
        Warning('Error in color palette!!!!')
    }
    
    
    # Creating the first leaflet plotting layer 
    m <- leaflet(df.polygon) %>% 
        addTiles(group = "OSM") %>%
        addProviderTiles("Esri.NatGeoWorldMap", group="ESRI") %>%
        addProviderTiles("CartoDB.DarkMatter", group= "CartoDB") %>%
        addLayersControl(baseGroups = c("OSM", "ESRI", "CartoDB")) %>% 
        setView(lat = 46.22764, lng = 2.213749, zoom = 5)  
    
   
    
    if(cols == 'incid_rea'){
        # HTML for pop-up on leaflet plot
    popup = paste0(
        '<b>',"Department: ",'</b>',df.polygon$name,"<br>"
             ,'<b>',"Pop: ",'</b>',scales::comma(df.polygon$inhabitant),"<br>"
             ,'<b>',"ICU Count: ",'</b>',scales::comma(df.polygon$total_dep),"<br>")  %>% 
        lapply(htmltools::HTML)
    
    
    }else if (cols == 'incid_hosp'){
         # HTML for pop-up on leaflet plot
    popup = paste0(
        '<b>',"Department: ",'</b>',df.polygon$name,"<br>"
             ,'<b>',"Pop: ",'</b>',scales::comma(df.polygon$inhabitant),"<br>"
             ,'<b>',"Hospitalized Count: ",'</b>',scales::comma(df.polygon$total_dep),"<br>")  %>% 
        lapply(htmltools::HTML)
        
        
    }else if (cols == 'incid_rad'){
         # HTML for pop-up on leaflet plot
    popup = paste0(
        '<b>',"Department: ",'</b>',df.polygon$name,"<br>"
             ,'<b>',"Pop: ",'</b>',scales::comma(df.polygon$inhabitant),"<br>"
             ,'<b>',"Recovered Count: ",'</b>',scales::comma(df.polygon$total_dep),"<br>")  %>% 
        lapply(htmltools::HTML)
        
    }else if (cols == 'incid_dc'){
         # HTML for pop-up on leaflet plot
    popup = paste0(
        '<b>',"Department: ",'</b>',df.polygon$name,"<br>"
             ,'<b>',"Pop: ",'</b>',scales::comma(df.polygon$inhabitant),"<br>"
             ,'<b>',"Death Count: ",'</b>',scales::comma(df.polygon$total_dep),"<br>")  %>% 
        lapply(htmltools::HTML)
        
        
    }
    
    else{
        warning('Error in HTML popups!!!!')
        
    }
    
    
    
    # Mapping the polygons 
    department_leaflet <- m %>% 
        addPolygons(fillColor = ~pal(total_dep)
                ,weight = 2,opacity = 1,color = "white"
                ,dashArray = "3",fillOpacity = 0.7
                ,highlight = highlightOptions(
                    weight = 5,color = "#666",dashArray = ""
                   ,fillOpacity = 0.7,bringToFront = TRUE)
                ,label = popup,labelOptions = labelOptions(offset = c(50, 75),
                    style = list("font-weight" = "normal",padding = "3px 8px")
                   ,textsize = "15px",direction = "auto"))
    
    if(cols == 'incid_rea'){
        plot <- department_leaflet %>% 
            addLegend(pal = pal, values = df.polygon$total_dep , opacity = 0.7, title = 'ICU Total',
            position = "bottomright", labFormat = labelFormat(transform = function(x) round(x,0)))

        return(plot)  
        
        
    }else if (cols == 'incid_hosp'){
        plot <- department_leaflet %>% 
            addLegend(pal = pal, values = df.polygon$total_dep , opacity = 0.7, title = 'Hospitalized Total',
            position = "bottomright", labFormat = labelFormat(transform = function(x) round(x,0)))

        return(plot)
        
    }else if (cols == 'incid_rad'){
        plot <- department_leaflet %>% 
            addLegend(pal = pal, values = df.polygon$total_dep , opacity = 0.7, title = 'Recovered Total',
            position = "bottomright", labFormat = labelFormat(transform = function(x) round(x,0)))

        return(plot)
        
    }else if (cols == 'incid_dc'){
        plot <- department_leaflet %>% 
            addLegend(pal = pal, values = df.polygon$total_dep , opacity = 0.7, title = 'Death Total',
            position = "bottomright", labFormat = labelFormat(transform = function(x) round(x,0)))

        return(plot)
        
    }else{
        warning('Error in plot function!!! legend')
    }
 
     
    
}

leaflet_plot_func_2(
    memoised_all_gender
    ,cols = 'incid_rea'
    ,change_1 = 'total_rea_dep')

 










 











# 13.0 Leaflet Pct Plots ----
leaflet_plot_func <- function(data,
                               cols = 'incid_hosp'
                              ,per_pop = FALSE){
     
    
    
    data <- covid_calculations_daily(data,column = cols)  
        
    #data %>% pull(name) %>% unique()
    
    
    
    
    
    # Getting France geo-codes from the map_data() function 
    france_data_name <- map_data('france') %>%
        tibble() %>% 

        dplyr::select(-subregion,-order) %>%
        mutate(group = group %>% as.character()) %>%
        rename(name = region) %>% 

        mutate(name = name %>% str_to_lower()) %>%
        left_join(data, by = c("name" = "name")) %>%
        mutate(name = name %>% str_to_title())
    
    
    #france_data_name %>% is.na() %>% sum()
    
   
    # Removing small island polygons
    # This is rows removed based off the group numbers
    # Removed because had issue with polygons making line 
    # connections from the island to land 
    dat <- france_data_name %>% 
        rowid_to_column() %>% 
        filter(group != 49  & group != 53  & group != 54  & 
               group != 55  & group != 37  & group != 29  & 
               group != 4   & group != 27  & group != 109 & 
               group != 110 & group != 111 & group != 72  &
               group != 75  & group != 62  & group != 64)
    
    
    
    # Function for converting dataframe to spatial polygon dataframe
    polyFunc<-function(groupname, dat){
      poly <- dat %>% 
          filter(name == groupname) %>% 
          dplyr::select(long,lat)
      return(Polygons(list(Polygon(poly)), groupname))
    }
    

    # Getting variables from dataframe to be converted into spatial polygon dataframe
    if(cols == 'incid_hosp'){
        tracts <- distinct(dat,name,inhabitant,total_dep) 
    }else{
        tracts <- distinct(dat,name,inhabitant,total_dep,total_hosp_dep,total_pct_dep)
    }
    

    # getting department names 
    tractname <- tracts$name

    # Applying the polyFunc() function to convert all department names into SPD
    polygons <- lapply(tractname, function(x) polyFunc(x, dat=dat)) 
    sp.polygon <- SpatialPolygons(polygons)
    df.polygon <- SpatialPolygonsDataFrame(sp.polygon, 
                                         data=data.frame(row.names=tractname, tracts))

    # Calculating the quantities of total percentages of ICU patients
    if(cols == 'incid_hosp'){
        quan <- df.polygon$total_dep %>% 
            quantile(probs = seq(0, 1, length = 9)) %>% 
            as_tibble() 
    }else{
        quan <- df.polygon$total_pct_dep %>% 
            quantile(probs = seq(0, 1, length = 9)) %>% 
            as_tibble() 
    }
    

    # Creating 10 bins 
    if(cols == 'incid_hosp'){
        bins <- c(max(df.polygon$total_dep),quan[8,] %>% pull()
                  ,quan[7,] %>% pull(),quan[6,] %>% pull(),quan[5,] %>% pull()
                  ,quan[4,] %>% pull(),quan[3,] %>% pull(),quan[2,] %>% pull()
                  ,quan[1,] %>% pull(),0)
        
    }else{
        bins <- c(max(df.polygon$total_pct_dep),quan[8,] %>% pull()
                  ,quan[7,] %>% pull(),quan[6,] %>% pull(),quan[5,] %>% pull()
                  ,quan[4,] %>% pull(),quan[3,] %>% pull(),quan[2,] %>% pull()
                  ,quan[1,] %>% pull(),0)
        
    }
    
    
    
    
    if(cols == 'incid_rea'){
        # maps data values to colors according to a given palette
    pal <- colorBin("YlOrRd", domain = df.polygon$total_pct_dep, bins = bins)
        
    }else if (cols == 'incid_hosp'){
        # maps data values to colors according to a given palette
    pal <- colorBin("PuRd", domain = df.polygon$total_dep, bins = bins)
        
    }else if (cols == 'incid_rad'){
        # maps data values to colors according to a given palette
    pal <- colorBin("YlGn", domain = df.polygon$total_pct_dep, bins = bins)
        
    }else if (cols == 'incid_dc'){
        # maps data values to colors according to a given palette
    pal <- colorBin("OrRd", domain = df.polygon$total_pct_dep, bins = bins)
        
    }else{
        Warning('Error in color palette!!!!')
    }
    
    
    # Creating the first leaflet plotting layer 
    m <- leaflet(df.polygon) %>% 
        addTiles(group = "OSM") %>%
        addProviderTiles("Esri.NatGeoWorldMap", group="ESRI") %>%
        addProviderTiles("CartoDB.DarkMatter", group= "CartoDB") %>%
        addLayersControl(baseGroups = c("OSM", "ESRI", "CartoDB")) %>% 
        setView(lat = 46.22764, lng = 2.213749, zoom = 5)  
    
   
    
    if(cols == 'incid_rea'){
        # HTML for pop-up on leaflet plot
    popup = paste0(
        '<b>',"Department: ",'</b>',df.polygon$name,"<br>"
             ,'<b>',"Pop: ",'</b>',scales::comma(df.polygon$inhabitant),"<br>"
             ,'<b>',"ICU Count: ",'</b>',scales::comma(df.polygon$total_dep),"<br>"
             ,'<b>',"Total Hospitalized: ",'</b>',scales::comma(df.polygon$total_hosp_dep),"<br>"
             ,'<b>',"ICU Pct: ",'</b>',scales::percent(df.polygon$total_pct_dep,accuracy = .01),"<br>")  %>% 
        lapply(htmltools::HTML)
    
    }else if (cols == 'incid_hosp'){
         # HTML for pop-up on leaflet plot
    popup = paste0(
        '<b>',"Department: ",'</b>',df.polygon$name,"<br>"
             ,'<b>',"Pop: ",'</b>',scales::comma(df.polygon$inhabitant),"<br>"
             ,'<b>',"Hospitalized Count: ",'</b>',scales::comma(round(df.polygon$total_dep)),"<br>")  %>% 
        lapply(htmltools::HTML)
        
        
    }else if (cols == 'incid_rad'){
         # HTML for pop-up on leaflet plot
    popup = paste0(
        '<b>',"Department: ",'</b>',df.polygon$name,"<br>"
             ,'<b>',"Pop: ",'</b>',scales::comma(df.polygon$inhabitant),"<br>"
             ,'<b>',"Recovered Count: ",'</b>',scales::comma(df.polygon$total_dep),"<br>"
             ,'<b>',"Total Hospitalized: ",'</b>',scales::comma(df.polygon$total_hosp_dep),"<br>"
             ,'<b>',"Recovered Pct: ",'</b>',scales::percent(df.polygon$total_pct_dep,accuracy = .01),"<br>")  %>% 
        lapply(htmltools::HTML)
        
    }else if (cols == 'incid_dc'){
         # HTML for pop-up on leaflet plot
    popup = paste0(
        '<b>',"Department: ",'</b>',df.polygon$name,"<br>"
             ,'<b>',"Pop: ",'</b>',scales::comma(df.polygon$inhabitant),"<br>"
             ,'<b>',"Death Count: ",'</b>',scales::comma(df.polygon$total_dep),"<br>" 
             ,'<b>',"Total Hospitalized: ",'</b>',scales::comma(df.polygon$total_hosp_dep),"<br>"
             ,'<b>',"Death Pct: ",'</b>',scales::percent(df.polygon$total_pct_dep,accuracy = .01),"<br>")  %>% 
        lapply(htmltools::HTML)
        
        
    }else{
        warning('Error in HTML popups!!!!')
        
    }
    
    if(cols == 'incid_hosp'){
        # Mapping the polygons 
    department_leaflet <- m %>% 
        addPolygons(fillColor = ~pal(total_dep)
                ,weight = 2,opacity = 1,color = "white"
                ,dashArray = "3",fillOpacity = 0.7
                ,highlight = highlightOptions(
                    weight = 5,color = "#666",dashArray = ""
                   ,fillOpacity = 0.7,bringToFront = TRUE)
                ,label = popup,labelOptions = labelOptions(offset = c(50, 75),
                    style = list("font-weight" = "normal",padding = "3px 8px")
                   ,textsize = "15px",direction = "auto"))
        
        
    }else{
        
        # Mapping the polygons 
    department_leaflet <- m %>% 
        addPolygons(fillColor = ~pal(total_pct_dep)
                ,weight = 2,opacity = 1,color = "white"
                ,dashArray = "3",fillOpacity = 0.7
                ,highlight = highlightOptions(
                    weight = 5,color = "#666",dashArray = ""
                   ,fillOpacity = 0.7,bringToFront = TRUE)
                ,label = popup,labelOptions = labelOptions(offset = c(50, 75),
                    style = list("font-weight" = "normal",padding = "3px 8px")
                   ,textsize = "15px",direction = "auto"))
        
    }
    
    
    
    
    if(cols == 'incid_rea'){
        plot <- department_leaflet %>% 
            addLegend(pal = pal, values = df.polygon$total_pct_dep , opacity = 0.7, title = 'ICU Percentages',
            position = "bottomright", labFormat = labelFormat(suffix="%", transform = function(x) 100*x))

        return(plot)  
        
        
    }else if (cols == 'incid_hosp'){
        plot <- department_leaflet %>% 
            addLegend(pal = pal, values = df.polygon$total_dep , opacity = 0.7, title = 'Total Hospitalized Count',
            position = "bottomright", labFormat = labelFormat(transform = function(x) round(x)))

        return(plot)
        
    }else if (cols == 'incid_rad'){
        plot <- department_leaflet %>% 
            addLegend(pal = pal, values = df.polygon$total_pct_dep , opacity = 0.7, title = 'Recovered Percentages',
            position = "bottomright", labFormat = labelFormat(suffix="%", transform = function(x) 100*x))

        return(plot)
        
    }else if (cols == 'incid_dc'){
        plot <- department_leaflet %>% 
            addLegend(pal = pal, values = df.polygon$total_pct_dep , opacity = 0.7, title = 'Death Percentages',
            position = "bottomright", labFormat = labelFormat(suffix="%", transform = function(x) 100*x))

        return(plot)
        
    }else{
        warning('Error in plot function!!! legend')
    }
 
     
    
}

leaflet_plot_func(
     memoised_all_gender
    ,cols = 'incid_rad'
    ,per_pop = FALSE)


 






# 14.0 DT Tables ----
fr_covid_data_tables <- function(data,cases_or_deaths = 'hosp'){
    
    fr_covid_data_tbl <<- memoised_all_gender %>% 
        group_by(name) %>% 
        summarise(
            total_hosp = sum(incid_hosp)
            ,total_icu = sum(incid_rea)
            ,total_rad = sum(incid_rad)
            ,total_dc  = sum(incid_dc)
            ,.groups = 'drop'
            )
    
        
           
    
    
    if(cases_or_deaths == 'hosp'){
        
       
       
        total_hosp_tbl <- fr_covid_data_tbl %>% 
            select(name, total_hosp) %>% 
             arrange(desc(total_hosp)) %>% 
            mutate(total_hosp = total_hosp %>% 
                       scales::comma(accuracy = 1) %>% 
                       str_c(" Confirmed")) %>% 
            rename(
                Departments = name,
                `Total Hospitalized` = total_hosp
            ) %>% 
            mutate(Departments = Departments %>% str_to_title())
        
        DT::datatable(total_hosp_tbl,selection = 'cell',caption = htmltools::tags$caption(
                           style = 'caption-side: bottom; text-align: center;'
                          ,'Table Hosp: ', htmltools::em(HTML("<b>",'This table displays the total number of people hospitalized do to COVID-19',"</b>"))
                          
                    )) %>%
        DT::formatStyle("Total Hospitalized", backgroundColor = 'orange',  color = 'red', fontWeight = 'bold')
       
        
        
    } else if (cases_or_deaths == 'rea'){
        
        total_icu_tbl <- fr_covid_data_tbl %>% 
            select(name, total_icu) %>% 
             arrange(desc(total_icu)) %>% 
            mutate(total_icu = total_icu %>% 
                       scales::comma(accuracy = 1) %>% 
                       str_c(" Confirmed")) %>% 
            rename(
                Departments = name,
                `Total ICU` = total_icu
            ) %>% 
            mutate(Departments = Departments %>% str_to_title())
        
        DT::datatable(total_icu_tbl,caption = htmltools::tags$caption(
                           style = 'caption-side: bottom; text-align: center;'
                          ,'Table ICU: ', htmltools::em(HTML("<b>",'This table displays the total number of people that have been in resuscitation or critical care do to COVID-19'),"</b>")
                          
                    )) %>%
        DT::formatStyle("Total ICU", backgroundColor = 'orange',  color = 'red', fontWeight = 'bold')
       
        
        
        
        
    }else if (cases_or_deaths == 'rad'){
        
        total_rad_tbl <- fr_covid_data_tbl %>% 
            select(name, total_rad) %>% 
             arrange(desc(total_rad)) %>% 
            mutate(total_rad = total_rad %>% 
                       scales::comma(accuracy = 1) %>% 
                       str_c(" Confirmed")) %>% 
            rename(
                Departments = name,
                `Total Recovered` = total_rad
            ) %>% 
            mutate(Departments = Departments %>% str_to_title())
        
        DT::datatable(total_rad_tbl
                      ,caption = htmltools::tags$caption(
                           style = 'caption-side: bottom; text-align: center;'
                          ,'Table Rad: ', htmltools::em(HTML("<b>",'This table displays the total amount of patient that returned home/recovered from COVID-19</strong>',"</b>"))
  )) %>%
        DT::formatStyle("Total Recovered", backgroundColor = 'orange',  color = 'red', fontWeight = 'bold') 
        
       
        
        
        
        
    }else if (cases_or_deaths == 'dc'){
        
        total_dc_tbl <- fr_covid_data_tbl %>% 
            select(name, total_dc) %>% 
             arrange(desc(total_dc)) %>% 
            mutate(total_dc = total_dc %>% 
                       scales::comma(accuracy = 1) %>% 
                       str_c(" Confirmed")) %>% 
            rename(
                Departments = name,
                `Total Deaths` = total_dc
            ) %>% 
            mutate(Departments = Departments %>% str_to_title())
        
        DT::datatable(total_dc_tbl
                      ,caption = htmltools::tags$caption(
                           style = 'caption-side: bottom; text-align: center;'
                          ,'Table DC: ', htmltools::em(HTML("<b>",'This table displays the total amout of deaths at the hospital do to COVID-19',"</b>")))) %>%
        DT::formatStyle("Total Deaths", backgroundColor = 'orange',  color = 'red', fontWeight = 'bold') 
        
        
        
        
    }else{
        warning('Wrong Intery Only Can Inter cases or deaths')
    }
    
}
fr_covid_data_tables(memoised_all_gender,cases_or_deaths = 'dc')
    
    


