get_authors <- function(data, reference_df) {
  
  # We need to extract the edgelist from scopus references
  
  # asn_scopus_1 <- 
  #   data |> 
  #   dplyr::filter(ref_type == "scopus") |> 
  #   dplyr::mutate(ID_TOS = str_extract(SR, ".*,"))
  
  asn_scopus_ref <- 
    reference_df |> 
    dplyr::select(CR_ref, AU, PY) |> 
    dplyr::group_by(CR_ref) |> 
    dplyr::filter(n() == 1) |> # Remove duplicate references
    tidyr::separate_rows(AU, sep = ";") |> 
    dplyr::group_by(CR_ref) |> 
    dplyr::filter(n() != 1) |> # remove isolated authors
    dplyr::group_by(CR_ref, PY) |> 
    dplyr::mutate(AU = paste0(AU,
                              collapse = ";")) |> 
    unique()
  
  list_refs <- unique(asn_scopus_ref$CR_ref)
  
  edgelist_scopus_ref <- tibble()
  
  for (i in list_refs) {
    
    df_1 <- 
      asn_scopus_ref |> 
      dplyr::filter(CR_ref == i) |> 
      tidyr::separate_rows(AU, sep = ";")
    
    df_2 <- 
      t(combn(df_1$AU, m = 2)) |> 
      as_tibble() |> 
      dplyr::rename(from = V1,
                    to = V2)|> 
      mutate(PY = df_1$PY[1])
    
    edgelist_scopus_ref <- 
      edgelist_scopus_ref |> 
      bind_rows(df_2) 
    
    
  }
  
  edgelist_scopus_main <- 
    data |> 
    dplyr::filter(ref_type == "wos") |> 
    dplyr::group_by(SR) |> 
    dplyr::filter(n() == 1) |> 
    dplyr::select(SR, AU, PY) |> 
    tidyr::separate_rows(AU, sep = ";") |> 
    dplyr::group_by(SR) |> 
    dplyr::filter(n() != 1) |> # remove isolated authors
    dplyr::group_by(SR, PY) |> 
    dplyr::mutate(AU = paste0(AU,
                              collapse = ";")) |> 
    unique()
  
  list_main <- unique(edgelist_scopus_main$SR)
  
  edgelist_scopus_main_dummy <- tibble()
  
  for (i in list_main) {
    
    df_1 <- 
      edgelist_scopus_main |> 
      filter(SR == i) |> 
      tidyr::separate_rows(AU, sep = ";")
    
    df_2 <- 
      t(combn(df_1$AU, m = 2)) |> 
      as_tibble() |> 
      dplyr::rename(from = V1,
                    to = V2)|> 
      mutate(PY = df_1$PY[1])
    
    edgelist_scopus_main_dummy <- 
      edgelist_scopus_main_dummy |> 
      bind_rows(df_2) 
    
  }
  
  edgelist_total_scopus <- 
    edgelist_scopus_main_dummy |> 
    bind_rows(edgelist_scopus_ref) 
  
  ## WoS
  
  ### 1. Selecting dois from refs
  
  
  DOI <- 
    data |> 
    dplyr::filter(ref_type == "wos") |> 
    dplyr::select(CR) |> 
    tidyr::separate_rows(CR, sep = ";") |> 
    dplyr::filter(str_detect(CR, "DOI"))  |>   
    dplyr::mutate(DOI = str_remove(CR, ".*DOI ")) |> 
    distinct(DOI)    
  
  
  ### 2. Getting authors from dois ref
  
  
  ##########################################################################
  #### Extracci?n de la informaci?n de los art?culos de las referencias ####
  ##########################################################################
  references <- data.frame(DI = character(), 
                           PU = character(), 
                           SO = character(), 
                           J9 = character(), 
                           PD = character(), 
                           PY = character(), 
                           TI = character(), 
                           AF = character(), 
                           stringsAsFactors = FALSE)
  authors <- data.frame(doi = character(), 
                        author = character(), 
                        year = character(), 
                        # month = character(), 
                        stringsAsFactors = FALSE)
  for (i in DOI$DOI) {
    doi <- i
    url <- paste0("http://api.crossref.org/works/", doi, ".xml")
    xml_data_1 = try(xmlParse(url), silent = TRUE);
    if (class(xml_data_1)[1] == "try-error") {
      next
    } else  {
      xml_data_2 <- xmlToList(xml_data_1)
      
      notfound =try(as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi"]][[".attrs"]]) == "journal_article", silent = TRUE);
      if (class(notfound) == "try-error"){
        next
      }else{
        
        if (as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi"]][[".attrs"]]) == "journal_article"){
          
          # PUBLISHER-NAME
          
          
          # if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["crm-item"]])){
          #   PU <- as.character(NA) 
          # } else {
          #   
          #   publisher0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["crm-item"]])
          #   publisher <- data.frame(publisher0)
          #   if(nrow(publisher) == 0){
          #     PU <- as.character(NA)
          #   }else{
          #     PU <- as.character(publisher$text[1])
          #   }
          # }
          
          # JOURNAL FULL TITLE 
          
          
          # if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["full_title"]])){
          #   SO <- as.character(NA) 
          # } else {
          #   
          #   journal0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["full_title"]])
          #   journal <- data.frame(journal0)
          #   if(nrow(journal) == 0){
          #     SO <- as.character(NA)
          #   }else{
          #     SO <- as.character(journal[1,1])
          #   }
          # }
          
          # JOURNAL ABBREV TITLE 
          
          
          # if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["abbrev_title"]])){
          #   J9 <- as.character(NA) 
          # } else {
          #   
          #   journal0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["abbrev_title"]])
          #   journal <- data.frame(journal0)
          #   if(nrow(journal) == 0){
          #     J9 <- as.character(NA)
          #   }else{
          #     J9 <- as.character(journal[1,1])
          #   }
          # }
          
          # MONTH
          
          
          # if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["month"]])){
          #   PD <- as.character(NA)
          # } else {
          # 
          #   month0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["month"]])
          #   month <- data.frame(month0)
          #   if(nrow(month) == 0){
          #     PD <- as.character(NA)
          #   }else{
          #     PD <- as.character(month[1,1])
          #   }
          # }
          
          # YEAR
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["year"]])){
            PY <- as.character(NA) 
          } else {
            
            Year0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["year"]])
            Year <- data.frame(Year0)
            if(nrow(Year) == 0){
              PY <- as.character(NA)
            }else{
              PY <- as.character(Year[1,1]) 
            }
          }
          
          # TITLE
          
          
          # if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["titles"]][["title"]])){
          #   TI <- as.character(NA) 
          # } else {
          #   
          #   title0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["titles"]][["title"]])
          #   title <- try(data.frame(title0), silent = TRUE);
          #   
          #   if(class(title) == "try-error"){
          #     titlex <- try(ldply(title0, data.frame), silent = TRUE);
          #     if(class(titlex) == "try-error"){
          #       TI <- as.character(NA)
          #     }else{
          #       TI0 <- as.character(titlex[1,2])
          #       TI <- trimws(TI0)
          #     }
          #   }else{
          #     if(nrow(title) == 0){
          #       TI <- as.character(NA)
          #     }else{
          #       TI <- as.character(title[1,1])
          #     }
          #   }
          # }
          
          # CONTRIBUTORS
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["contributors"]]))
          {
            AF <- as.character(NA)
          } else {
            
            author_ref <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["contributors"]])
            
            # author_1_ref <- ldply(author_ref, data.frame)
            # author_2_ref <- author_1_ref[author_1_ref$.attrs == "author", c(2,3) ]
            # 
            author_2_ref <- author_ref[["person_name"]] |>
              as_tibble(.name_repair = "universal") |>
              filter(.attrs == "author")
            
            authorss <- data.frame(doi= doi, author = paste0(author_2_ref$surname, ", ", author_2_ref$given_name), year = PY)
            
            authors = rbind(authors, authorss)
            authorss$author <- trim(authorss$author)
            AF <- as.character(paste(authorss$author, collapse = ";   "))
          }
          
          # references0 <- data.frame(DI = doi, PU = PU, SO = SO, J9 = J9, PD = PD, PY = PY)#, TI = TI)#, AF = AF)
          # references = rbind(references, references0) 
          
        }else {
          next}
      }
    }
  } 
  # authors$month <- sub("^[0]+", "", authors$month) # Elimina los ceros a la izquierda 
  
  ### 3. Getting authors from main wos
  
  
  ##########################################################################
  #### Separaci?n de los nombres de los autores de los art?culos de WoS ####
  ##########################################################################
  wos_author <- # 1339 - 3
    data |> 
    dplyr::filter(ref_type == "wos") |> 
    dplyr::select(doi = DI, AU, PY) |> 
    tidyr::separate_rows(AU, sep = ";") |> 
    dplyr::rename(author = AU)
  
  
  ### 4. Merging main with refs dois with names
  
  
  #################################################################
  #### Uni?n de los datos de WoS y de las referencias de estos ####
  #################################################################
  authors$author <- gsub("(?<=, [[:alpha:]]).*", "", authors$author, perl=TRUE)
  wos_author_ref <- # 27051 3 doi author year 
    authors |> 
    dplyr::mutate(author = str_replace(author, ",", "")) |> 
    dplyr::select(doi, author, year) |> 
    dplyr::mutate(author = str_to_upper(author))
  
  
  ### 5. Getting edge list from (4)
  
  # We need to remove dois with only one author
  
  
  dois_one_author <- 
    wos_author_ref |> 
    dplyr::count(doi) |> 
    dplyr::filter(n == 1) |> 
    dplyr::select(doi)
  
  
  # We need to identify dois with high number of co-authors
  
  
  dois_high_author <- 
    wos_author_ref |> 
    dplyr::count(doi, sort = TRUE) |> 
    dplyr::slice(1:9) |> 
    dplyr::select(doi)
  
  
  # We need to merge these two dataframes
  
  
  dois_to_remove <- 
    dois_one_author |> 
    dplyr::bind_rows(dois_high_author) 
  
  
  # We need to remove the dois from wos_author_ref
  
  wos_author_removed <- 
    wos_author_ref |> 
    dplyr::filter(!(doi %in% dois_to_remove$doi))
  
  
  
  ###########################
  #### Listas de enlaces ####
  ###########################
  # # Para art?culos que tienen como m?nimo dos autores
  # author_1 <- # 4273 3 there a mistake with the duplicate values. 
  #   wos_author |> # 190
  #   filter(!is.na(PY)) |> 
  #   dplyr::rename(year = PY,
  #                 doi = DI) |> 
  #   bind_rows(wos_author_ref |> 
  #               mutate(year = as.numeric(year))) |> # 4104
  #   unique() |> 
  #   filter(!(doi %in% "10.3109/13880209.2014.922589" & 
  #          year == 2014)) # this is a duplicate value with != year
  edgelist_wos_authors <- data.frame(Source = character(), 
                                     Target = character(), 
                                     doi = character(),
                                     year = as.numeric(),
                                     stringsAsFactors = FALSE)
  # table_ids <- table(author_1$doi)
  # table_ids_0 <- data.frame(table_ids)
  # table_ids_1 <- table_ids_0[table_ids_0$Freq >= 2,]
  list_ids_1 <- unique(wos_author_removed$doi)
  for (i in list_ids_1) {
    df_1 = wos_author_removed[wos_author_removed$doi == i,] |> 
      filter(!is.na(doi))
    df_2 = combn(df_1$author, 2, simplify = FALSE)
    df_3 = data.frame((t(data.frame(df_2))), i)
    colnames(df_3) = c("Source", "Target", "doi")
    df_4 <- df_3 |> bind_cols(df_1 |> select(year) |> unique())
    edgelist_wos_authors = rbind(edgelist_wos_authors, df_4)
  }
  
  edgelist_scopus_wos <- 
    edgelist_total_scopus |> 
    dplyr::bind_rows(edgelist_wos_authors |> 
                       dplyr::select(from = Source, 
                                     to = Target, 
                                     PY = year) |> 
                       dplyr::mutate(PY = as.numeric(PY))) |> 
    dplyr::filter(!str_detect(from, pattern = "\\* | \\. | [0-9]"),
                  !str_detect(to, pattern = "\\* | \\. | [0-9]"),
                  !str_detect(to, pattern = "\\*"),
                  !str_detect(to, pattern = "[0-9]"),
                  !str_detect(from, pattern = "\\*"),
                  !str_detect(from, pattern = "[0-9]"))
  
  return(edgelist_scopus_wos = edgelist_scopus_wos)
  
  
}
