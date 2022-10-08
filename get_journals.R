get_journals <- function(data, reference_df) {
  
  # Getting journal names
  
  abbrTable <- 
    read_csv("https://docs.google.com/spreadsheets/d/1vCyke7hw-iaReOkQkuxMiWBLQxtQLbTxIMFHj7P3gJ0/export?format=csv&gid=74429560")
  
  journals_all_abbr <- # From Scimago data
    abbrTable |>
    dplyr::select(journal, journal_abbr) |>
    dplyr::mutate(journal_abbr = str_remove_all(journal_abbr, "\\.")) |>
    dplyr::mutate(journal_abbr = str_to_upper(journal_abbr)) |> 
    dplyr::mutate(journal = str_remove_all(journal, "\\.")) |>
    dplyr::mutate(journal = str_to_upper(journal)) |> 
    dplyr::select(journal, journal_abbr) |> 
    unique() |> 
    dplyr::filter(!duplicated(journal_abbr)) |> 
    tidyr::drop_na() |> 
    dplyr::mutate(journal = str_replace(string = journal, 
                                        pattern = " & ", 
                                        replacement = " AND "))
  
  df_1_journal <- 
    data |> 
    dplyr::select(SR, 
                  JI_main = JI, 
                  PY_main = PY) |> 
    dplyr::right_join(reference_df |> 
                        dplyr::select(SR, 
                                      JI_ref = JI, 
                                      PY_ref = PY)) |> 
    dplyr::mutate(JI_main = str_remove_all(JI_main, "\\.")) |> 
    # dplyr::inner_join(journals_all_abbr, 
    #                   by = c("JI_ref" = "journal_abbr")) |> 
    dplyr::left_join(journals_all_abbr,
                     by = c("JI_ref" = "journal")) |> 
    dplyr::mutate(JI_ref = if_else(!(is.na(journal_abbr)), 
                                   journal_abbr, 
                                   JI_ref )) |> 
    dplyr::select(-journal_abbr)
    # dplyr::filter(!(JI_ref == "")) |> 
    # dplyr::filter(!(JI_main == JI_ref)) 
  
  return(journal_df = df_1_journal)
  
}
