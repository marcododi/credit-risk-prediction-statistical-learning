library(data.table)
library(tidyverse)
library(readxl)
library(stringr)
library(janitor)
library(lubridate)
library(caret)
library(kableExtra)

#---- working on income statement ----
is_file_list <- list.files(path = "data/CE/",
                           pattern = "\\.xlsx$", 
                           full.names = TRUE) |> 
  str_sort(numeric = TRUE)

head(is_file_list, n = 10)

temp_is <- list()

is_types <- rep("guess", 137)
is_types[135] <- "text"

for(i in 1:length(is_file_list)){
  temp_is[[i]] <- read_xlsx(
    path = is_file_list[i], 
    sheet = "Risultati", 
    col_names = TRUE, 
    col_types = is_types
  )
}

# binding lists by row
income_statement <- rbindlist(temp_is, use.names = FALSE, fill = TRUE)

# brushing column names off 
income_statement <- clean_names(income_statement)

# parsing date
income_statement$anno_costituzione <- as_date(
  as.numeric(income_statement$anno_di_costituzione), 
  origin = "1899-12-30"
)

# new date column NAs
class(income_statement$anno_costituzione)
anyNA(income_statement[, anno_costituzione])
sum(is.na(income_statement[, anno_costituzione]))

# original date columns NAs
class(income_statement$anno_di_costituzione)
anyNA(income_statement[, anno_di_costituzione])
sum(is.na(income_statement$anno_di_costituzione))

# comparing NAs
which(is.na(income_statement$anno_di_costituzione) == TRUE)

which(is.na(income_statement$anno_costituzione) == TRUE)

# matching NAs. These are NAs that already were in the original Excel file. 
which(is.na(income_statement$anno_di_costituzione) == TRUE & 
        is.na(income_statement$anno_costituzione) == TRUE)

# extracting differences
which((is.na(income_statement$anno_costituzione) == TRUE) & 
        !(is.na(income_statement$anno_di_costituzione) == TRUE))

is_convert_index <- which(
  (is.na(income_statement$anno_costituzione) == TRUE) & 
    !(is.na(income_statement$anno_di_costituzione) == TRUE))

# printing dates of 22 NAs. These are pure text strings that were not converted 
# because of / /
income_statement$anno_di_costituzione[is_convert_index]

# converting to date format using dmy()
income_statement[is_convert_index,
                 anno_costituzione := dmy(anno_di_costituzione)]

# removing column named .x1
income_statement <- income_statement[, -1]

# extracting useful columns for all years
cols_ricavi <- paste0("ricavi_vendite_e_prestazioni_migl_eur_", 
                      as.numeric(2023:2015))
cols_ris_op <- paste0("risultato_operativo_migl_eur_", as.numeric(2023:2015))
cols_val_agg <- paste0("valore_aggiunto_migl_eur_", as.numeric(2023:2015))
cols_tot_cost_pers <- paste0("totale_costi_del_personale_migl_eur_",
                             as.numeric(2023:2015))
cols_ris_ante_imp <- paste0("risultato_prima_delle_imposte_migl_eur_",
                            as.numeric(2023:2015))
cols_ut_perd_es <- paste0("utile_perdita_di_esercizio_migl_eur_", 
                          as.numeric(2023:2015))

is_columns <- c("ragione_sociale", cols_ricavi, cols_ris_op, cols_val_agg, 
                cols_tot_cost_pers, cols_ris_ante_imp, cols_ut_perd_es, 
                "stato_giuridico", "forma_giuridica", "nace_rev_2", 
                "nace_rev_2_descrizione", "anno_costituzione")

subs_income_st <- income_statement[, .SD, .SDcols = is_columns]

# removing 68 NAs
valid_date_income_st <- subs_income_st[!(is.na(anno_costituzione)), ]
anyNA(valid_date_income_st[, anno_costituzione])

# checking "ragione_sociale" for any "liquidazione" pattern or other versions 
# caused by any kind of typos.
# str_to_lower, change all characters to lower case, regex are case sensitive!!
# str_squish, trim front and end spaces, reduce inner double space to one 
valid_date_income_st[, ragione_sociale := str_to_lower(ragione_sociale) |> 
                       str_squish()]
# in liquidazione
any(str_detect(
  valid_date_income_st$ragione_sociale, 
  pattern = "in liquidazione"
))

which(str_detect(
  valid_date_income_st$ragione_sociale, 
  pattern = "in liquidazione"
))

# liquidazione
any(str_detect(
  valid_date_income_st$ragione_sociale, 
  pattern = "liquidazione"
))

which(str_detect(
  valid_date_income_st$ragione_sociale, 
  pattern = "liquidazione"
))

# checking delta 
which(str_detect(
  valid_date_income_st$ragione_sociale, 
  pattern = "liquidazione") & !str_detect(
  valid_date_income_st$ragione_sociale, 
      pattern = "in liquidazione"))

valid_date_income_st$ragione_sociale[8793]
valid_date_income_st$ragione_sociale[20567]
valid_date_income_st$ragione_sociale[23090]
valid_date_income_st$ragione_sociale[26145]
valid_date_income_st$ragione_sociale[35847]
valid_date_income_st$ragione_sociale[55885]
valid_date_income_st$ragione_sociale[197567]
valid_date_income_st$ragione_sociale[234567]
valid_date_income_st$ragione_sociale[261691]
valid_date_income_st$ragione_sociale[267790]
valid_date_income_st$ragione_sociale[289579]
valid_date_income_st$ragione_sociale[322113]
valid_date_income_st$ragione_sociale[352292]
valid_date_income_st$ragione_sociale[359421]

# robust regex that includes different versions caused by any kind of typo
any(str_detect(valid_date_income_st$ragione_sociale, 
  pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidazz|", 
              "liquidat|liqne|lquidaz|liqidaz|liqudaz")))

which(str_detect(
  valid_date_income_st$ragione_sociale, 
  pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidaz|", 
                   "liquidat|liqne|lquidaz|liqidaz|liqudaz|lqudaz")))

clean_income_st <- valid_date_income_st[!(str_detect(
  valid_date_income_st$ragione_sociale, 
  pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidaz|", 
                   "liquidat|liqne|lquidaz|liqidaz|liqudaz|lqudaz"))), ]

# converting numeric columns from character to numeric 
any(clean_income_st[, 2:55] == "n.d.")
sum(clean_income_st[, 2:55] == "n.d.")

clean_income_st[, (2:55) := lapply(X = .SD, FUN =  as.numeric), .SDcols = 2:55] 

anyNA(clean_income_st[, 2:55])
sum(is.na(clean_income_st[, 2:55]))

# counting NAs and zeros for each numeric column 
is_nas <- colSums(is.na(clean_income_st[, 2:55]), na.rm = FALSE) |> 
  as.data.frame()|> 
  setNames("na_count") |> 
  rownames_to_column("original_names") |> 
  mutate(is_variables = str_remove(original_names, pattern = "_\\d+$"), 
         dates = str_extract(original_names, pattern = "\\d+$"), 
         zeros_count = colSums(clean_income_st[, 2:55] == 0, na.rm = TRUE)) |> 
  select(-original_names) |> 
  remove_rownames() |> 
  select(is_variables, dates, na_count, zeros_count) |> 
  arrange(dates, na_count, zeros_count) 

kable(is_nas, caption = "NAs & zeros count")

# extracting income statement variables for 2016, 2020, 2023
is_selected_cols <- str_detect(
  string = colnames(clean_income_st),
  pattern = paste0(
    "^ragione_sociale$|^forma_giuridica$|^stato_giuridico$|^nace_rev_2$|", 
    "^nace_rev_2_descrizione|^anno_costituzione$|_(2016|2020|2023)$"))

is_selected_years <- clean_income_st[, .SD, .SDcols = is_selected_cols]

# columns common to the other data set, used for further extraction. 
common_cols <- c("ragione_sociale", "forma_giuridica", "stato_giuridico", 
                     "anno_costituzione", "nace_rev_2", 
                     "nace_rev_2_descrizione")

numeric_is_cols <- setdiff(colnames(is_selected_years), common_cols)

is_new_columns <- c(common_cols, numeric_is_cols)

setcolorder(is_selected_years, neworder = is_new_columns)

# NAs check in string columns
anyNA(is_selected_years[, c("ragione_sociale", "stato_giuridico", 
                            "nace_rev_2", "nace_rev_2_descrizione")])

# checking for any duplicated row
is_dupl <- is_selected_years[, .SD, .SDcols = common_cols]
is_dupl_count <- duplicated(is_dupl[, ]) 
which(is_dupl_count == TRUE)
is_selected_years$ragione_sociale[(is_dupl_count == TRUE)]
is_selected_years$anno_costituzione[(is_dupl_count == TRUE)]
is_selected_years$nace_rev_2_descrizione[(is_dupl_count == TRUE)]

is_selected_years$ricavi_vendite_e_prestazioni_migl_eur_2023[
  (is_dupl_count == TRUE)]
is_selected_years$ricavi_vendite_e_prestazioni_migl_eur_2020[
  (is_dupl_count == TRUE)]
is_selected_years$ricavi_vendite_e_prestazioni_migl_eur_2016[
  (is_dupl_count == TRUE)]

# comparing revenue values of the original companies
is_five_dupl_rag <- is_selected_years$ragione_sociale[(is_dupl_count == TRUE)]
is_five_dupl_anno_cost <- is_selected_years$anno_costituzione[
  (is_dupl_count == TRUE)]

is_selected_years$ricavi_vendite_e_prestazioni_migl_eur_2023[
  is_selected_years$ragione_sociale %in% is_five_dupl_rag & 
  is_selected_years$anno_costituzione %in% is_five_dupl_anno_cost &
    !(is_dupl_count == TRUE)]

is_selected_years$ricavi_vendite_e_prestazioni_migl_eur_2020[
  is_selected_years$ragione_sociale %in% is_five_dupl_rag & 
    is_selected_years$anno_costituzione %in% is_five_dupl_anno_cost & 
    !(is_dupl_count == TRUE)]

is_selected_years$ricavi_vendite_e_prestazioni_migl_eur_2016[
  is_selected_years$ragione_sociale %in% is_five_dupl_rag & 
    is_selected_years$anno_costituzione %in% is_five_dupl_anno_cost & 
    !(is_dupl_count == TRUE)]

# removing the 5 duplicated companies 
final_is_selected_years <- is_selected_years[!(is_dupl_count == TRUE), ]

# cleaning global env & garbage collection 
rm(list = setdiff(ls(), c("final_is_selected_years", "common_cols")))
gc(reset = TRUE)

#---- working on assets ----
asset_file_list <- list.files(path = "data/SP/attivo/", 
                               pattern = "\\.xlsx$", 
                               full.names = TRUE) |> 
  str_sort(numeric = TRUE)

head(asset_file_list, n = 10)

temp_asset <- list()

asset_types <- rep("guess", 157)
asset_types[155] <- "text"

for(ii in 1:length(asset_file_list)){
  temp_asset[[ii]] <- read_xlsx(
    path = asset_file_list[ii], 
    sheet = "Risultati", 
    col_names = TRUE, 
    col_types = asset_types
  )
}

asset <- rbindlist(temp_asset, use.names = FALSE, fill = TRUE)

# brushing column names off 
asset <- clean_names (asset)

# parsing dates
asset$anno_costituzione <- as_date(
  as.numeric(asset$anno_di_costituzione), 
  origin = "1899-12-30"
)

# new column NAs
class(asset$anno_costituzione)
anyNA(asset[, anno_costituzione])
sum(is.na(asset[, anno_costituzione]))

# original column NAs
class(asset$anno_di_costituzione)
anyNA(asset[, anno_di_costituzione])
sum(is.na(asset$anno_di_costituzione))

# comparing NAs
which(is.na(asset$anno_di_costituzione) == TRUE)

which(is.na(asset$anno_costituzione) == TRUE)

# matching Nas These are NAs that already were in the original Excel file. 
which((is.na(asset$anno_di_costituzione) == TRUE) & 
        (is.na(asset$anno_costituzione) == TRUE))

# extracting differences
which((is.na(asset$anno_costituzione) == TRUE) & 
        !(is.na(asset$anno_di_costituzione) == TRUE))

asset_convert_index <- which(
  (is.na(asset$anno_costituzione) == TRUE) & 
    !(is.na(asset$anno_di_costituzione) == TRUE))

# printing dates of 22 NAs. These are pure text strings that were not converted 
# because of / /
asset$anno_di_costituzione[asset_convert_index]

# converting to date format using dmy()
asset[asset_convert_index, anno_costituzione := dmy(anno_di_costituzione)]

# removing column named .x1
asset <- asset[, -1]

# extracting useful columns for all years
cols_immat <- paste0("totale_immob_immateriali_migl_eur_", 
                     as.numeric(2023:2015))
cols_mat <- paste0("totale_immob_materiali_migl_eur_", as.numeric(2023:2015))
cols_att_circ <- paste0("attivo_circolante_migl_eur_", 
                        as.numeric(2023:2015))
cols_rim <- paste0("totale_rimanenze_migl_eur_", as.numeric(2023:2015))
cols_cred_clienti <- paste0("cred_vs_clienti_entro_migl_eur_", 
                            as.numeric(2023:2015))
cols_tot_liquid <- paste0("tot_dispon_liquide_migl_eur_", 
                          as.numeric(2023:2015))
cols_tot_att <- paste0("totale_attivo_migl_eur_", as.numeric(2023:2015))

asset_cols <- c("ragione_sociale", cols_immat, cols_mat, cols_att_circ, 
                cols_rim, cols_cred_clienti, cols_tot_liquid, cols_tot_att,
                "stato_giuridico", "forma_giuridica", "nace_rev_2", 
                "nace_rev_2_descrizione", "anno_costituzione")

subs_asset <- asset[, .SD, .SDcols = asset_cols]

# removing 68 Nas from anno_costituzione
valid_date_asset <- subs_asset[!(is.na(anno_costituzione)), ]
anyNA(valid_date_asset$anno_costituzione)

# checking "ragione_sociale" for any "liquidazione" pattern or other versions 
# caused by any kind of typos.
valid_date_asset[, ragione_sociale := str_to_lower(ragione_sociale) |> 
                    str_squish()]

any(str_detect(
  valid_date_asset$ragione_sociale, 
  pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidaz|",
                   "liquidat|liqne|lquidaz|liqidaz|liqudaz|lqudaz")))

which(
  str_detect(
    valid_date_asset$ragione_sociale, 
    pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidaz|",
                     "liquidat|liqne|lquidaz|liqidaz|liqudaz|lqudaz")))

clean_asset <- valid_date_asset[!(str_detect(
  valid_date_asset$ragione_sociale, 
  pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidaz|",
                     "liquidat|liqne|lquidaz|liqidaz|liqudaz|lqudaz"))), ]

# converting numeric columns from character to numeric 
any(clean_asset[, 2:64] == "n.d.")
sum(clean_asset[, 2:64] == "n.d.")

clean_asset[, (2:64) := lapply(X = .SD, FUN =  as.numeric), .SDcols = 2:64]

sum(is.na(clean_asset[, 2:64]))

# counting NAs and zeros for each numeric column
asset_nas <- colSums(is.na(clean_asset[, 2:64]), na.rm = FALSE) |> 
  as.data.frame() |> 
  setNames("na_count") |> 
  rownames_to_column("original_names") |> 
  mutate(asset_vars = str_remove(original_names, pattern = "_\\d+$"), 
         dates = str_extract(original_names, pattern = "\\d+$"), 
         zeros_count = colSums(clean_asset[, 2:64] == 0, na.rm = TRUE)) |> 
  select(-original_names) |> 
  remove_rownames() |> 
  select(asset_vars, dates, na_count, zeros_count) |> 
  arrange(dates, na_count, zeros_count)
  
kable(asset_nas, caption = "NAs & zeros count")

# extracting asset variables for 2016, 2020, 2023
asset_selected_cols <- str_detect(
  string = colnames(clean_asset), 
  pattern = paste0("^ragione_sociale$|^stato_giuridico$|^forma_giuridica$|",
                   "^nace_rev_2$|^nace_rev_2_descrizione$|^anno_costituzione$|", 
                   "_(2016|2020|2023)$"))

asset_selected_years <- clean_asset[, .SD, .SDcols = asset_selected_cols]

asset_numeric_cols <- setdiff(colnames(asset_selected_years), common_cols)
asset_new_cols <- c(common_cols, asset_numeric_cols)
setcolorder(asset_selected_years, neworder = asset_new_cols)

# check for NAs in character columns
anyNA(asset_selected_years[, c("ragione_sociale", "stato_giuridico", 
                            "nace_rev_2", "nace_rev_2_descrizione")])

# chechking for any duplicated
asset_dupl <- asset_selected_years[, .SD, .SDcols = common_cols]
asset_dupl_count <- duplicated(asset_dupl[, ])
which(asset_dupl_count == TRUE)
asset_selected_years$ragione_sociale[(asset_dupl_count == TRUE)]
asset_selected_years$anno_costituzione[(asset_dupl_count == TRUE)]
asset_selected_years$nace_rev_2_descrizione[(asset_dupl_count == TRUE)]

asset_selected_years$totale_attivo_migl_eur_2023[(asset_dupl_count == TRUE)]
asset_selected_years$totale_attivo_migl_eur_2020[(asset_dupl_count == TRUE)]
asset_selected_years$totale_attivo_migl_eur_2016[(asset_dupl_count == TRUE)]

# comparing asset values of the original companies
asset_five_dupl_rag <- asset_selected_years$ragione_sociale[
  (asset_dupl_count == TRUE)]
asset_five_dupl_anno_cost <- asset_selected_years$anno_costituzione[
  (asset_dupl_count == TRUE)]

asset_selected_years$totale_attivo_migl_eur_2023[
  asset_selected_years$ragione_sociale %in% asset_five_dupl_rag & 
    asset_selected_years$anno_costituzione %in% asset_five_dupl_anno_cost & 
    !(asset_dupl_count == TRUE)]

asset_selected_years$totale_attivo_migl_eur_2020[
  asset_selected_years$ragione_sociale %in% asset_five_dupl_rag & 
    asset_selected_years$anno_costituzione %in% asset_five_dupl_anno_cost & 
    !(asset_dupl_count == TRUE)]

asset_selected_years$totale_attivo_migl_eur_2016[
  asset_selected_years$ragione_sociale %in% asset_five_dupl_rag & 
    asset_selected_years$anno_costituzione %in% asset_five_dupl_anno_cost & 
    !(asset_dupl_count == TRUE)]

# removing the 5 duplicated companies 
final_asset_selected_years <- asset_selected_years[
  !(asset_dupl_count == TRUE), ]

# cleaning global env & garbge collection
rm(list = setdiff(
  ls(),
  c("final_is_selected_years", "common_cols", 
    "final_asset_selected_years")))

gc(reset = TRUE)

#---- working on liabilities-----
liabilities_file_list <- list.files(path = "data/SP/passivo/", 
                                    pattern = "\\.xlsx$", 
                                    full.names = TRUE) |> 
  str_sort(numeric = TRUE)

head(liabilities_file_list, n = 10)

temp_liab<- list()

liab_tpyes <- rep("guess", 227)
liab_tpyes[225] <- "text"

for(j in 1:length(liabilities_file_list)){
  temp_liab[[j]] <- read_xlsx(
    path = liabilities_file_list[j], 
    sheet = "Risultati", 
    col_names = TRUE, 
    col_types = liab_tpyes
  )
}

liabilities <- rbindlist(temp_liab, use.names = FALSE, fill = TRUE)

# brushing column names off 
liabilities <- clean_names (liabilities)

# parsing dates
liabilities$anno_costituzione <- as_date(
  as.numeric(liabilities$anno_di_costituzione), 
  origin = "1899-12-30"
)

# new date column NAs
class(liabilities$anno_costituzione)
anyNA(liabilities[ , anno_costituzione])
sum(is.na(liabilities[ , anno_costituzione]))

# original date column NAs
class(liabilities$anno_di_costituzione)
anyNA(liabilities[ , anno_di_costituzione])
sum(is.na(liabilities[ , anno_di_costituzione]))

# comparing NAs
which(is.na(liabilities$anno_di_costituzione) == TRUE)

which(is.na(liabilities$anno_costituzione) == TRUE)

# matching NAs. These are NAs that already were in the original Excel file
which((is.na(liabilities$anno_di_costituzione) == TRUE) & 
        (is.na(liabilities$anno_costituzione) == TRUE))

# extracting differences
which((is.na(liabilities$anno_costituzione) == TRUE) & 
        !(is.na(liabilities$anno_di_costituzione) == TRUE))

liab_convert_index <- which(
  (is.na(liabilities$anno_costituzione) == TRUE) &
    !(is.na(liabilities$anno_di_costituzione) == TRUE))

# printing dates of 22 NAs. These are pure text strings that were not converted 
# because of / /
liabilities$anno_di_costituzione[liab_convert_index]

# converting to date format using dmy()
liabilities[liab_convert_index, anno_costituzione := dmy(anno_di_costituzione)]

# removing column named .x1
liabilities <- liabilities[, -1]

# extracting useful columns for all years
cols_obblig_entro <- paste0("obblig_ni_entro_migl_eur_", 
                            as.numeric(2023:2015))
cols_obblig_oltre <- paste0("obblig_ni_oltre_migl_eur_", 
                            as.numeric(2023:2015))
cols_deb_vs_soci_entro <- paste0("soci_per_finanziamenti_entro_migl_eur_", 
                                 as.numeric(2023:2015))
cols_deb_vs_soci_oltre <- paste0("soci_per_finanziamenti_oltre_migl_eur_",
                                 as.numeric(2023:2015))
cols_banche_entro <- paste0("banche_entro_migl_eur_", as.numeric(2023:2015))
cols_banche_oltre <- paste0("banche_a_lungo_migl_eur_", as.numeric(2023:2015))
cols_finanziatori_entro <- paste0("altri_finanziatori_entro_migl_eur_", 
                                  as.numeric(2023:2015))
cols_finanziatori_oltre <- paste0("altri_finanziatori_oltre_migl_eur_", 
                                  as.numeric(2023:2015))
cols_acconti_entro <- paste0("acconti_entro_migl_eur_", as.numeric(2023:2015))
cols_fornitori_entro <- paste0("fornitori_entro_migl_eur_", 
                               as.numeric(2023:2015))
cols_altri_deb_entro <- paste0("altri_debiti_entro_migl_eur_", 
                               as.numeric(2023:2015))
cols_tot_liab <- paste0("totale_passivo_migl_eur_", as.numeric(2023:2015))

cols_liab <- c("ragione_sociale", cols_obblig_entro, cols_obblig_oltre, 
               cols_deb_vs_soci_entro, cols_deb_vs_soci_oltre, 
               cols_banche_entro, cols_banche_oltre, cols_finanziatori_entro, 
               cols_finanziatori_oltre, cols_acconti_entro, 
               cols_fornitori_entro, cols_altri_deb_entro, cols_tot_liab, 
               "stato_giuridico", "forma_giuridica", "nace_rev_2", 
               "nace_rev_2_descrizione", "anno_costituzione")

subs_liab <- liabilities[, .SD, .SDcols = cols_liab] 

# removing 68 NAs
valid_date_liab <- subs_liab[!(is.na(anno_costituzione)), ]
anyNA(valid_date_liab)

# checking "ragione_sociale" for any "liquidazione" pattern or other versions 
# caused by any kind of typos.
valid_date_liab[, ragione_sociale := str_to_lower(ragione_sociale) |> 
              str_squish()]

any(str_detect(
  valid_date_liab$ragione_sociale, 
  pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidaz|",
                   "liquidat|liqne|lquidaz|liqidaz|liqudaz|lqudaz")))

which(str_detect(
    valid_date_liab$ragione_sociale, 
    pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidaz|",
                     "liquidat|liqne|lquidaz|liqidaz|liqudaz|lqudaz")))

clean_liabilities <- valid_date_liab[!(str_detect(
  valid_date_liab$ragione_sociale, 
  pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidaz|",
                   "liquidat|liqne|lquidaz|liqidaz|liqudaz|lqudaz"))), ]

# converting numeric columns from character to numeric 
any(clean_liabilities[, 2:109] == "n.d.")
sum(clean_liabilities[, 2:109] == "n.d.")

clean_liabilities[, (2:109) := lapply(X = .SD, 
                                      FUN =  as.numeric), .SDcols = 2:109]

sum(is.na(clean_liabilities[, 2:109]))

# counting NAs and zeros for each numeric column
liab_nas <- colSums(is.na(clean_liabilities[, 2:109]), na.rm = FALSE)|> 
  as.data.frame() |> 
  setNames("na_count") |> 
  rownames_to_column("original_names") |> 
  mutate(liab_vars = str_remove(original_names, pattern = "_\\d+$"), 
         dates = str_extract(original_names, pattern = "\\d+$"), 
         zeros_count = colSums(clean_liabilities[, 2:109] == 0,
                               na.rm = TRUE)) |> 
  remove_rownames() |> 
  select(-original_names) |> 
  select(liab_vars, dates, na_count, zeros_count) |> 
  arrange(dates, na_count, zeros_count)

kable(liab_nas, caption = "NAs & zeros count")

# extracting liabilities variables for 2016, 2020, 2023
selected_liab_cols <- str_detect(
  string = colnames(clean_liabilities), 
  pattern = paste0("^ragione_sociale$|^stato_giuridico$|^forma_giuridica$|", 
                   "^nace_rev_2$|^nace_rev_2_descrizione$|^anno_costituzione|", 
                   "_(2016|2020|2023)$"))

liab_selected_years <- clean_liabilities[, .SD, .SDcols = selected_liab_cols]

liab_numeric_cols <- setdiff(colnames(liab_selected_years), common_cols)
liab_new_cols <- c(common_cols, liab_numeric_cols)
setcolorder(liab_selected_years, neworder = liab_new_cols)

# chechking for any duplicated
liab_dupl <- liab_selected_years[, .SD, .SDcols = common_cols]
liab_dupl_count <- duplicated(liab_dupl[, ])
which(liab_dupl_count == TRUE)  
liab_selected_years$ragione_sociale[(liab_dupl_count == TRUE)]
liab_selected_years$anno_costituzione[(liab_dupl_count == TRUE)]
liab_selected_years$nace_rev_2_descrizione[(liab_dupl_count == TRUE)]

liab_selected_years$totale_passivo_migl_eur_2023[(liab_dupl_count == TRUE)]
liab_selected_years$totale_passivo_migl_eur_2020[(liab_dupl_count == TRUE)]
liab_selected_years$totale_passivo_migl_eur_2016[(liab_dupl_count == TRUE)]

# comparing liabilities values of the original companies
liab_five_dupl_rag <- liab_selected_years$ragione_sociale[
  (liab_dupl_count == TRUE)]
liab_five_dupl_anno_cost <- liab_selected_years$anno_costituzione[
  (liab_dupl_count == TRUE)]

liab_selected_years$totale_passivo_migl_eur_2023[
  liab_selected_years$ragione_sociale %in% liab_five_dupl_rag & 
    liab_selected_years$anno_costituzione %in% liab_five_dupl_anno_cost & 
    !(liab_dupl_count == TRUE)]

liab_selected_years$totale_passivo_migl_eur_2020[
  liab_selected_years$ragione_sociale %in% liab_five_dupl_rag & 
    liab_selected_years$anno_costituzione %in% liab_five_dupl_anno_cost & 
    !(liab_dupl_count == TRUE)]

liab_selected_years$totale_passivo_migl_eur_2016[
  liab_selected_years$ragione_sociale %in% liab_five_dupl_rag & 
    liab_selected_years$anno_costituzione %in% liab_five_dupl_anno_cost & 
    !(liab_dupl_count == TRUE)]

# removing the 5 duplicated companies 
final_liab_selected_years <- liab_selected_years[!(liab_dupl_count == TRUE), ]

# cleaning global env & garbage collection
rm(list = setdiff(
  ls(), 
  c("final_is_selected_years", "common_cols", 
    "final_asset_selected_years", "final_liab_selected_years")))

gc(reset = TRUE)

#---- working on net-assets ----
net_asset_file_list <- list.files(path = "data/SP/PN/", 
                                   pattern = "\\.xlsx$", 
                                   full.names = TRUE) |> 
  str_sort(numeric = TRUE)

head(net_asset_file_list, n = 10)

temp_netasset <- list()

netasset_types <- rep("guess", 27)
netasset_types[25] <- "text"

for(jj in 1:length(net_asset_file_list)){
  temp_netasset[[jj]] <- read_xlsx(
    path = net_asset_file_list[jj], 
    sheet = "Risultati", 
    col_names = TRUE, 
    col_types = netasset_types
  )
}

net_asset <- rbindlist(temp_netasset, use.names = FALSE, fill = TRUE)

# brushing column names off 
net_asset <- clean_names(net_asset)

# parsing date
net_asset$anno_costituzione <- as_date(
  as.numeric(net_asset$anno_di_costituzione), 
  origin = "1899-12-30"
)

# new date column NAs
class(net_asset$anno_costituzione)
anyNA(net_asset[ , anno_costituzione])
sum(is.na(net_asset[ , anno_costituzione]))

# original date column NAs
class(net_asset$anno_di_costituzione)
anyNA(net_asset[, anno_di_costituzione])
sum(is.na(net_asset$anno_di_costituzione))

# comparing NAs
which(is.na(net_asset$anno_di_costituzione) == TRUE)

which(is.na(net_asset$anno_costituzione) == TRUE)

# matching NAs. These are NAs that already were in the original Excel file. 
which((is.na(net_asset$anno_di_costituzione) == TRUE) &
        (is.na(net_asset$anno_costituzione)))

# extracting differences
which((is.na(net_asset$anno_costituzione) == TRUE) & 
        !(is.na(net_asset$anno_di_costituzione) == TRUE))

net_asset_convert_index <- which(
  (is.na(net_asset$anno_costituzione) == TRUE) &
    !(is.na(net_asset$anno_di_costituzione) == TRUE))

# printing dates of 22 NAs. These are pure text strings that were not converted 
# because of / /
net_asset$anno_di_costituzione[net_asset_convert_index]

# converting to date format using dmy()
net_asset[net_asset_convert_index, 
          anno_costituzione := dmy(anno_di_costituzione)]

# removing column named .x1
net_asset <- net_asset[, -1]

# extracting useful columns for all years
cols_tot_pn <- paste0("totale_patrimonio_netto_migl_eur_", 
                      as.numeric(2023:2015))

cols_net_asset <- c("ragione_sociale", cols_tot_pn, "stato_giuridico", 
                    "forma_giuridica", "nace_rev_2", "nace_rev_2_descrizione", 
                    "anno_costituzione")

subs_net_asset <- net_asset[, .SD, .SDcols = cols_net_asset]

# removing 68 NAs
valid_date_netasset <- subs_net_asset[!(is.na(anno_costituzione)), ]
anyNA(valid_date_netasset$anno_costituzione)

# checking "ragione_sociale" for any "liquidazione" pattern or other versions 
# caused by any kind of typos.
valid_date_netasset[, ragione_sociale := str_to_lower(ragione_sociale) |> 
                       str_squish()]

any(str_detect(
  valid_date_netasset$ragione_sociale, 
  pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidaz|",
                   "liquidat|liqne|lquidaz|liqidaz|liqudaz|lqudaz")))

which(str_detect(
  valid_date_netasset$ragione_sociale, 
  pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidaz|",
                   "liquidat|liqne|lquidaz|liqidaz|liqudaz|lqudaz")))

clean_net_asset <- valid_date_netasset[!(str_detect(
  valid_date_netasset$ragione_sociale, 
  pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidaz|",
                   "liquidat|liqne|lquidaz|liqidaz|liqudaz|lqudaz"))), ]

# converting numeric columns from character to numeric 
any(clean_net_asset[, 2:10] == "n.d.")
sum(clean_net_asset[, 2:10] == "n.d.")

clean_net_asset[, (2:10) := lapply(X = .SD, FUN =  as.numeric), .SDcols = 2:10]

sum(is.na(clean_net_asset[, 2:10]))

# counting NAs and zeros for each numeric column
net_asset_nas <- colSums(is.na(clean_net_asset[, 2:10]), na.rm = FALSE) |> 
  as.data.frame() |> 
  setNames("na_count") |> 
  rownames_to_column("original_names") |> 
  mutate(net_asset_vars = str_remove(original_names, pattern = "_\\d+$"), 
         dates = str_extract(original_names, pattern = "\\d+$"), 
         zeros_count = colSums(clean_net_asset[, 2:10] == 0, na.rm = TRUE)) |> 
  select(-original_names) |> 
  remove_rownames() |> 
  select(net_asset_vars, dates, na_count, zeros_count) |> 
  arrange(dates, na_count, zeros_count)

kable(net_asset_nas, caption = "NAs & zeros count") 

#extracting net asset variables for 2016, 2020, 2023
selected_netasset_cols <- str_detect(
  string = colnames(clean_net_asset), 
  pattern = paste0("^ragione_sociale$|^stato_giuridico$|^forma_giuridica$|",
                   "^nace_rev_2$|^nace_rev_2_descrizione$|^anno_costituzione$|", 
                   "_(2016|2020|2023)$"))

net_asset_selected_years <- clean_net_asset[
  , .SD, .SDcols = selected_netasset_cols]

netasset_numeric_cols <- setdiff(colnames(net_asset_selected_years), 
                                 common_cols)
netasset_new_cols <- c(common_cols, netasset_numeric_cols)
setcolorder(net_asset_selected_years, neworder = netasset_new_cols)

# chechking for any duplicated
net_asset_dupl <- net_asset_selected_years[, .SD, .SDcols = common_cols]
net_asset_dupl_count <- duplicated(net_asset_dupl[, ])
which(net_asset_dupl_count == TRUE)
net_asset_selected_years$ragione_sociale[(net_asset_dupl_count == TRUE)]
net_asset_selected_years$anno_costituzione[(net_asset_dupl_count == TRUE)]
net_asset_selected_years$nace_rev_2_descrizione[(net_asset_dupl_count == TRUE)]

net_asset_selected_years$totale_patrimonio_netto_migl_eur_2023[
  (net_asset_dupl_count == TRUE)]

net_asset_selected_years$totale_patrimonio_netto_migl_eur_2020[
  (net_asset_dupl_count == TRUE)]

net_asset_selected_years$totale_patrimonio_netto_migl_eur_2016[
  (net_asset_dupl_count == TRUE)]

# comparing liabilities values of the original companies
net_asset_five_dupl_rag <- net_asset_selected_years$ragione_sociale[
  (net_asset_dupl_count == TRUE)]
net_asset_five_dup_anno_cost <- net_asset_selected_years$anno_costituzione[
  (net_asset_dupl_count == TRUE)]

net_asset_selected_years$totale_patrimonio_netto_migl_eur_2023[
  net_asset_selected_years$ragione_sociale %in% net_asset_five_dupl_rag & 
  net_asset_selected_years$anno_costituzione %in% net_asset_five_dup_anno_cost & 
  !(net_asset_dupl_count == TRUE)]

net_asset_selected_years$totale_patrimonio_netto_migl_eur_2020[
  net_asset_selected_years$ragione_sociale %in% net_asset_five_dupl_rag & 
  net_asset_selected_years$anno_costituzione %in% net_asset_five_dup_anno_cost & 
    !(net_asset_dupl_count == TRUE)]

net_asset_selected_years$totale_patrimonio_netto_migl_eur_2016[
  net_asset_selected_years$ragione_sociale %in% net_asset_five_dupl_rag & 
  net_asset_selected_years$anno_costituzione %in% net_asset_five_dup_anno_cost & 
    !(net_asset_dupl_count == TRUE)]

# removing the 5 duplicated companies 
final_net_asset_selected_years <- net_asset_selected_years[
  !(net_asset_dupl_count == TRUE), ]

# cleaning global env & garbage collection
rm(list = setdiff(
  ls(), 
  c("final_is_selected_years", "common_cols", "final_asset_selected_years", 
    "final_liab_selected_years","final_net_asset_selected_years")))

gc(reset = TRUE)

#---- working on market data ----
market_data_file_list <- list.files(path = "data/mktcap/", 
                                    pattern = "\\.xlsx$", 
                                    full.names = TRUE) |> 
  str_sort(numeric = TRUE)

temp_mktdata <- list()

mktdata_types <- rep("guess", 55)
mktdata_types[53] <- "text"

for(n in 1:length(market_data_file_list)){
  temp_mktdata[[n]] <- read_xlsx(
    path = market_data_file_list[n], 
    sheet = "Risultati", 
    col_names = TRUE, 
    col_types = mktdata_types
  )
}

market_data <- rbindlist(temp_mktdata, use.names = FALSE, fill = TRUE)

# brushing column names off 
market_data <- clean_names(market_data)

# parsing dates
market_data$anno_costituzione <- as_date(
  as.numeric(market_data$anno_di_costituzione), 
  origin = "1899-12-30"
)

# new date column NAs
class(market_data$anno_costituzione)
anyNA(market_data[ , anno_costituzione])
sum(is.na(market_data[ , anno_costituzione]))

# original date column NAs
class(market_data$anno_di_costituzione)
anyNA(market_data[ , anno_di_costituzione])
sum(is.na(market_data[ , anno_di_costituzione]))

# comparing NAs
which(is.na(market_data$anno_di_costituzione) == TRUE) 

which(is.na(market_data$anno_costituzione) == TRUE) 

# matching NAs. These are NAs that already were in the original Excel file. 
which((is.na(market_data$anno_costituzione) == TRUE) & 
        is.na(market_data$anno_di_costituzione) == TRUE)

# extracting differences
which((is.na(market_data$anno_costituzione) == TRUE) &
        (!is.na(market_data$anno_di_costituzione) == TRUE))

mkt_data_convert_index <- which(
  (is.na(market_data$anno_costituzione) == TRUE) &
    (!is.na(market_data$anno_di_costituzione) == TRUE))

# printing dates of 22 NAs. These are pure text strings that were not converted
# because of / /
market_data$anno_di_costituzione[mkt_data_convert_index]

# converting to date format using dmy()
market_data[mkt_data_convert_index, 
            anno_costituzione := dmy(anno_di_costituzione)]

# removing column named .x1
market_data <- market_data[, -1]

# removing 68 NAs
valid_date_mkt_data <- market_data[!(is.na(anno_costituzione)), ]
anyNA(valid_date_mkt_data)

# checking "ragione_sociale" for any "liquidazione" pattern or other versions 
# caused by any kind of typos.
valid_date_mkt_data[, ragione_sociale := str_to_lower(ragione_sociale) |> 
                      str_squish()]

any(str_detect(
  valid_date_mkt_data$ragione_sociale, 
  pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidaz|",
                   "liquidat|liqne|lquidaz|liqidaz|liqudaz|lqudaz")))

which(str_detect(
  valid_date_mkt_data$ragione_sociale, 
  pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidaz|",
                   "liquidat|liqne|lquidaz|liqidaz|liqudaz|lqudaz")))

clean_mkt_data<- valid_date_mkt_data[!(str_detect(
  valid_date_mkt_data$ragione_sociale, 
  pattern = paste0("liquidazione|\\bliq\\b|liquidaz|liquidazioni|liquidaz|",
                   "liquidat|liqne|lquidaz|liqidaz|liqudaz|lqudaz"))), ]

# converting numeric columns from character to numeric 
any(clean_mkt_data[, 2:49] == "n.d.")
sum(clean_mkt_data[, 2:49] == "n.d.")

clean_mkt_data[, (2:49) := lapply(X = .SD, FUN =  as.numeric), .SDcols = 2:49]

sum(is.na(clean_mkt_data[, 2:49]))

# counting NAs and zeros for each numeric column
mkt_nas <- colSums(is.na(clean_mkt_data[, 2:49]), na.rm = FALSE) |> 
  as.data.frame() |> 
  setNames("na_count") |> 
  rownames_to_column("original_names") |> 
  mutate(mkt_vars = str_remove(original_names, pattern = "_\\d+$"), 
         dates = str_extract(original_names, pattern = "\\d+$"), 
         zeros_count = colSums(clean_mkt_data[, 2:49] == 0, na.rm = TRUE)) |> 
  select(-original_names) |> 
  remove_rownames() |> 
  select(mkt_vars, dates, na_count, zeros_count) |> 
  arrange(dates, na_count, zeros_count) 


kable(mkt_nas, caption = "NAs & zeros count")

# cleaning global env & garbage collection
rm(list = setdiff(
  ls(), 
  c("final_is_selected_years", "common_cols", "final_asset_selected_years", 
    "final_liab_selected_years","final_net_asset_selected_years")))

gc(reset = TRUE)

#---- Joining dataset by column (left join) ---- 
whole_dataset <- merge(
  x = final_is_selected_years, 
  y = final_asset_selected_years, 
  by = common_cols, all.x = TRUE) |> 
  merge(final_liab_selected_years, by = common_cols, all.x = TRUE) |> 
  merge(final_net_asset_selected_years, by = common_cols, all.x = TRUE)

#---- extracting industry information ----
whole_dataset[, nace_rev_2_descrizione := str_to_lower(
  nace_rev_2_descrizione) |> 
    str_squish()]

industry_info <- whole_dataset[, .SD, .SDcols = c(
  "nace_rev_2", "nace_rev_2_descrizione")]

industry_info_ordered <- unique(industry_info)[order(nace_rev_2)]
  
info_tbl <- kable(
  industry_info_ordered, 
  format = "latex",
  col.names = c("Codice", "Descrizione Settore"),
  booktabs = TRUE, 
  longtable = TRUE,
  caption = "Codice NACE REV 2 Settori Inclusi Nel Campione",
  align = "cl",
  label = "table: codici_nace_rev_2_settori") |> 
  kable_styling(latex_options = "repeat_header") |> 
  column_spec(1, width = "1cm") |> 
  column_spec(2, width = "11cm", extra_css = NULL)

cat(info_tbl, 
    file = "tables/tabella_nace_settori.tex")
  
#---- splitting dataset by year ----
# 2016 
cols_2016 <- str_detect(
  string = colnames(whole_dataset), 
  pattern = paste0("^ragione_sociale$|^forma_giuridica$|^stato_giuridico$|", 
                   "^anno_costituzione$|^nace_rev_2$|^nace_rev_2_descrizione$|", 
                   "_(2016)$"))

whole_dataset_2016 <- whole_dataset[, .SD, .SDcols = cols_2016]

# 2020
cols_2020 <- str_detect(
  string = colnames(whole_dataset), 
  pattern = paste0("^ragione_sociale$|^forma_giuridica$|^stato_giuridico$|", 
                   "^anno_costituzione$|^nace_rev_2$|^nace_rev_2_descrizione$|", 
                   "_(2020)$"))

whole_dataset_2020 <- whole_dataset[, .SD, .SDcols = cols_2020]

# 2023
cols_2023 <- str_detect(
  string = colnames(whole_dataset),
  pattern = paste0("^ragione_sociale$|^forma_giuridica$|^stato_giuridico$|", 
                   "^anno_costituzione$|^nace_rev_2$|^nace_rev_2_descrizione$|", 
                   "_(2023)$"))

whole_dataset_2023 <- whole_dataset[, .SD, .SDcols = cols_2023]

# filtering NAs for each year
anyNA(whole_dataset[, 1:6])
any(whole_dataset[, c(1, 2, 3, 5, 6)] == "n.d.") 

# 2016
clean_2016_dats <- complete.cases(whole_dataset_2016[, ])
no_nas_dataset_2016 <- whole_dataset_2016[clean_2016_dats, ]

# 2020
clean_2020_dats <- complete.cases(whole_dataset_2020[, ])
no_nas_dataset_2020 <- whole_dataset_2020[clean_2020_dats, ]

# 2023
clean_2023_dats <- complete.cases(whole_dataset_2023[, ])
no_nas_dataset_2023 <- whole_dataset_2023[clean_2023_dats, ]

#---- studying correlation on 2016 dataset ----
num_dats_2016 <- no_nas_dataset_2016[,
                                     .SD,
                                     .SDcols = 7:length(no_nas_dataset_2016)]

corr_matr_2016 <- cor(num_dats_2016, method = "pearson", use = "everything")

# calibrating correlation value to exlude some variables and avoid 
# multicollinearity that would crash Logistic Regression with Lasso and 
# would make the Covariance matrix of LDA and QDA a singular matrix, thus not 
# invertible.

# testing a conservative value of rho = 0.7
high_corr_vars_2016_07 <- findCorrelation(
  corr_matr_2016, cutoff = 0.7, names = TRUE, exact = TRUE, verbose = TRUE
)

high_corr_vars_2016_07
setdiff(colnames(corr_matr_2016), high_corr_vars_2016_07)

# testing rho = 0.80, mid value
high_corr_vars_2016_08 <- findCorrelation(
  corr_matr_2016, cutoff = 0.80, names = TRUE, exact = TRUE, verbose = TRUE
)

high_corr_vars_2016_08
setdiff(high_corr_vars_2016_07, high_corr_vars_2016_08)
setdiff(colnames(corr_matr_2016), high_corr_vars_2016_08)

# prepping table to export in latex
clean_high_corr_vars_2016_08 <- data.frame(
  number = seq(1, 10, 1), 
  var_names = c(
    "Totale attivo", "Totale passivo", "Valore aggiunto", 
    "Totale immobilizzazioni immateriali", "Obbligazioni entro esercizio",
    "Debiti vs altri finanziatori oltre esercizio", "Risultato operativo", 
    "Debiti vs fornitori", "Risultato prima delle imposte",
    "Acconti entro esercizio"))

exluded_vars <- kable(
  clean_high_corr_vars_2016_08,
  format = "latex",
  col.names = c("", "Nomi Variabili"),
  booktabs = TRUE, 
  linesep = "", 
  caption = " Variabili Rimosse $\\rho$ = 0.8", 
  align = "cl", 
  label = "table:nomivariabiliescluse",
  position = "!h"
)

cat(exluded_vars, 
    file = "tables/tabella_variabili_escluse.tex")

# latex tbl vars included
survived_vars_2016_08 <- data.frame(
  index_numb = seq(1, 16, 1), 
  var_names = c(
    "Ricavi vendite e prestazioni", "Totale costi del personale", 
    "Utile / Perdita di esercizio", "Totale immobilizzazioni materiali", 
    "Attivo circolante", "Totale delle rimanenze", 
    "Crediti vs clienti entro esercizio", "Totale disponibilita' liquide", 
    "Obbligazioni oltre esercizio", 
    "Debiti vs soci per finanziamenti entro esercizio", 
    "Debiti vs soci per finanziamenti oltre esercizio", 
    "Debiti vs banche entro esercizio", "Debiti vs banche a lungo", 
    "Debiti vs altri finanziatori entro esercizio",
    "Altri debiti entro esercizio", "Totale patrimonio netto"))

tbl_survived_vars_2016_08 <- kable(
  survived_vars_2016_08, 
  format = "latex", 
  col.names = c("", "Nomi Variabili"), 
  booktabs = TRUE, 
  linesep = "", 
  caption = "Variabili Incluse $\\rho$ = 0.8", 
  align = "cl", 
  label = "table:variabiliinclusecampione", 
  position = "H"
)

cat(tbl_survived_vars_2016_08,
    file = "tables/tabella_variabili_incluse_campione.tex")

#--- filtering 2016 dataset based on rho = 0.8 ----
new_cols_2016 <- setdiff(colnames(corr_matr_2016), high_corr_vars_2016_08)

final_dataset_2016 <- no_nas_dataset_2016[, 
                                          .SD,
                                          .SDcols = c(
                                           common_cols, new_cols_2016)]

#---- filtering 2020 dataset based on rho = 0.8 -----
cols_filter_2020 <- str_replace_all(
  string = high_corr_vars_2016_08, 
  pattern = "_(2016)$", 
  replacement = "_2020")

new_cols_2020 <- setdiff(
  colnames(no_nas_dataset_2020)[7:length(no_nas_dataset_2020)],
  cols_filter_2020)

final_dataset_2020 <- no_nas_dataset_2020[,
                                          .SD,
                                          .SDcols = c(
                                            common_cols, new_cols_2020)]

#---- filtering 2023 dataset based on rho = 0.8 -----
cols_filter_2023 <- str_replace_all(
  string = high_corr_vars_2016_08, 
  pattern = "_(2016)$", 
  replacement = "_2023")

new_cols_2023 <- setdiff(
  colnames(no_nas_dataset_2023)[7:length(no_nas_dataset_2023)],
  cols_filter_2023)

final_dataset_2023 <- no_nas_dataset_2023[,
                                          .SD,
                                          .SDcols = c(
                                            common_cols, new_cols_2023)]

#---- creating column defaults for each data set ----

# I classify defaulted firms with 1, while active companies are classified 
# with 0. 
# In order to implement a prudent Credit Risk Prediction, I also classify 
# active companies with solvency problems with 1. 

# 2016 
final_dataset_2016[, stato_giuridico := str_to_lower(stato_giuridico) |> 
                     str_squish()]

final_dataset_2016[, defaults := ifelse(
  stato_giuridico %in% c("fallita", "attiva (stato di insolvenza)", 
                         "cessata (chiusura per fallimento)"), 1, 0)]

# 2020
final_dataset_2020[, stato_giuridico := str_to_lower(stato_giuridico) |> 
                     str_squish()]

final_dataset_2020[, defaults := ifelse(
  stato_giuridico %in% c("fallita", "attiva (stato di insolvenza)", 
                         "cessata (chiusura per fallimento)"), 1, 0)]

# 2023
final_dataset_2023[, stato_giuridico := str_to_lower(stato_giuridico) |> 
                     str_squish()]

final_dataset_2023[, defaults := ifelse(
  stato_giuridico %in% c("fallita", "attiva (stato di insolvenza)", 
                         "cessata (chiusura per fallimento)"), 1, 0)]

#----- creating the macrosector column for each year ---- 
# I extract the first two digits of the NACE REV 2 CODE that indicates the 
# macrosector 

# 2016 
final_dataset_2016 <- final_dataset_2016 |> 
  arrange(nace_rev_2) |> 
  mutate(macrosector_nace_rev_2 = str_extract(string = nace_rev_2, 
                                              pattern = "\\d{2}"))

# 2020
final_dataset_2020 <- final_dataset_2020 |> 
  arrange(nace_rev_2) |> 
  mutate(macrosector_nace_rev_2 = str_extract(string = nace_rev_2, 
                                              pattern = "\\d{2}"))

# 2023 
final_dataset_2023 <- final_dataset_2023 |> 
  arrange(nace_rev_2) |> 
  mutate(macrosector_nace_rev_2 = str_extract(string = nace_rev_2, 
                                              pattern = "\\d{2}"))

#---- importing xlsx file NACE 2 digits ----
nace_2_digits <- read_xlsx(path = "data/NACE_Rev2_Heading_All_Languages.xlsx", 
                           col_names = TRUE, col_types = "text") |> 
  clean_names()

clean_nace_2_digits <- nace_2_digits |> 
  select(nace_code, it_desc) |> 
  filter(str_detect(string = nace_code, pattern = "^\\d{2}$")) |> 
  rename(macrosector_nace_rev_2_descrizione = it_desc) |> 
  arrange(nace_code)

#---- joining company info & NACE code with 2 digits ----
# 2016
final_dataset_2016 <- left_join(
  final_dataset_2016, clean_nace_2_digits, 
  by = c("macrosector_nace_rev_2" = "nace_code"))

# 2020
final_dataset_2020 <- left_join(
  final_dataset_2020, clean_nace_2_digits, 
  by = c("macrosector_nace_rev_2" = "nace_code"))

# 2023
final_dataset_2023 <- left_join(
  final_dataset_2023, clean_nace_2_digits,
  by = c("macrosector_nace_rev_2" = "nace_code"))

#---- creating general industries ----
# now I group the companies into even bigger macro sector in order to 
# use them later as an input variable for models. I do this by using case_when 
# and model matrix to assign the sector dummy. 

# 2016 
final_dataset_2016 <- final_dataset_2016 |> 
  mutate(general_sector = case_when(
    macrosector_nace_rev_2 %in% c("06", "08", "09") ~ "estrazione", 
    macrosector_nace_rev_2 %in% c("10", "11", "12") ~ "alimentare", 
    macrosector_nace_rev_2 %in% c("13", "14", "15") ~ "tessile", 
    macrosector_nace_rev_2 %in% c("16", "17", "18") ~ "cellulosa", 
    macrosector_nace_rev_2 %in% as.character(seq(19, 33, 1)) ~ "manifattura", 
    macrosector_nace_rev_2 == "35" ~ "energia", 
    macrosector_nace_rev_2 %in% c("41", "42", "43") ~ "edilizia", 
    macrosector_nace_rev_2 %in% c("49", "50", "51", "52") ~ "trasporto", 
    macrosector_nace_rev_2 %in% c("55", "56") ~ "vitto_alloggio", 
    macrosector_nace_rev_2 %in% c("58", "59",
                                  "61", "62", "63") ~ "comunicazione", 
    macrosector_nace_rev_2 == "68" ~ "immobiliare", 
    macrosector_nace_rev_2 %in% c("70", "71", "72", "73") ~ "professionali", 
    macrosector_nace_rev_2 %in% c("79", "81", "82") ~ "servizi"))

# 2020
final_dataset_2020 <- final_dataset_2020 |> 
  mutate(general_sector = case_when(
    macrosector_nace_rev_2 %in% c("06", "08", "09") ~ "estrazione", 
    macrosector_nace_rev_2 %in% c("10", "11", "12") ~ "alimentare", 
    macrosector_nace_rev_2 %in% c("13", "14", "15") ~ "tessile", 
    macrosector_nace_rev_2 %in% c("16", "17", "18") ~ "cellulosa", 
    macrosector_nace_rev_2 %in% as.character(seq(19, 33, 1)) ~ "manifattura", 
    macrosector_nace_rev_2 == "35" ~ "energia", 
    macrosector_nace_rev_2 %in% c("41", "42", "43") ~ "edilizia", 
    macrosector_nace_rev_2 %in% c("49", "50", "51", "52") ~ "trasporto", 
    macrosector_nace_rev_2 %in% c("55", "56") ~ "vitto_alloggio", 
    macrosector_nace_rev_2 %in% c("58", "59",
                                  "61", "62", "63") ~ "comunicazione", 
    macrosector_nace_rev_2 == "68" ~ "immobiliare", 
    macrosector_nace_rev_2 %in% c("70", "71", "72", "73") ~ "professionali", 
    macrosector_nace_rev_2 %in% c("79", "81", "82") ~ "servizi"))

# 2023
final_dataset_2023 <- final_dataset_2023 |> 
  mutate(general_sector = case_when(
    macrosector_nace_rev_2 %in% c("06", "08", "09") ~ "estrazione", 
    macrosector_nace_rev_2 %in% c("10", "11", "12") ~ "alimentare", 
    macrosector_nace_rev_2 %in% c("13", "14", "15") ~ "tessile", 
    macrosector_nace_rev_2 %in% c("16", "17", "18") ~ "cellulosa", 
    macrosector_nace_rev_2 %in% as.character(seq(19, 33, 1)) ~ "manifattura", 
    macrosector_nace_rev_2 == "35" ~ "energia", 
    macrosector_nace_rev_2 %in% c("41", "42", "43") ~ "edilizia", 
    macrosector_nace_rev_2 %in% c("49", "50", "51", "52") ~ "trasporto", 
    macrosector_nace_rev_2 %in% c("55", "56") ~ "vitto_alloggio", 
    macrosector_nace_rev_2 %in% c("58", "59",
                                  "61", "62", "63") ~ "comunicazione", 
    macrosector_nace_rev_2 == "68" ~ "immobiliare", 
    macrosector_nace_rev_2 %in% c("70", "71", "72", "73") ~ "professionali", 
    macrosector_nace_rev_2 %in% c("79", "81", "82") ~ "servizi"))

# creating a table with labels to export in latex
df_label <- final_dataset_2016 |> 
  select(macrosector_nace_rev_2, general_sector) |> 
  unique() |> 
  group_by(general_sector) |> 
  summarise(nace_rev_2 = paste0(macrosector_nace_rev_2, collapse = ","))

tbl_sector_labels <- kable(
  df_label, 
  format = "latex", 
  col.names = c("Sector Label", "NACE REV 2"), 
  booktabs = TRUE, 
  linesep = "\\addlinespace", 
  caption = "General sector label for dummy variable", 
  align = c("l", "l"), 
  label = "table:gensectlable",
  position = "H")

cat(tbl_sector_labels, 
    file = "tables/table-sample/gen_sector_labs.tex")

#----- creating the dummy for the general industry ----
# 2016
dummy_sector_2016 <- model.matrix(
  object = ~ general_sector - 1, 
  data = final_dataset_2016)

# 2020
dummy_sector_2020 <- model.matrix(
  object = ~ general_sector - 1, 
  data = final_dataset_2020)

# 2023
dummy_sector_2023 <- model.matrix(
  object = ~ general_sector - 1, 
  data = final_dataset_2023)

#---- saving clean data in .rda format ----
save(final_dataset_2016, 
     file = "rdas/clean-data/final_dataset_2016.rda")
save(final_dataset_2020,
     file = "rdas/clean-data/final_dataset_2020.rda")
save(final_dataset_2023,
     file = "rdas/clean-data/final_dataset_2023.rda")

save(corr_matr_2016, 
     file = "rdas/clean-data/correlation_matrix_2016.rda")
save(high_corr_vars_2016_07,
     file = "rdas/clean-data/high_cor_variables_2016_07.rda")
save(high_corr_vars_2016_08, 
    file = "rdas/clean-data/high_cor_variables_2016_08.rda")

save(dummy_sector_2016,
     file = "rdas/dummy-sector/dummy_sector_2016.rda")
save(dummy_sector_2020,
     file = "rdas/dummy-sector/dummy_sector_2020.rda")
save(dummy_sector_2023,
     file = "rdas/dummy-sector/dummy_sector_2023.rda")

#---- final cleaning -----
rm(list = ls())
gc(reset = TRUE)
