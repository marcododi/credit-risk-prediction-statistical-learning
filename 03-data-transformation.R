library(tidyverse)
library(stringr)
library(bestNormalize)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(caret)

#---- uploading data ----
load("rdas/clean-data/final_dataset_2016.rda")
load("rdas/clean-data/final_dataset_2020.rda")
load("rdas/clean-data/final_dataset_2023.rda")

# converting data from data.table to tibble in order to use the tidyverse
df_2016 <- as_tibble(final_dataset_2016)
df_2020 <- as_tibble(final_dataset_2020)
df_2023 <- as_tibble(final_dataset_2023)

#---- extracting numeric columns -----
num_2016 <- df_2016 |> 
  select(ends_with("2016"))

num_2020 <- df_2020 |> 
  select(ends_with("2020"))

num_2023 <- df_2023 |> 
  select(ends_with("2023"))

#---- jittering ---- 
# I add random numbers from a uniform distribution between 0 and 1 where 
# variables have a value equals to zero in order to allow the data 
# transformation while keeping the underlying structure and information 

# 2016
num_2016 <- num_2016 |> 
  mutate(across(.cols = everything(), 
                .fns = ~ ifelse(.x == 0, runif(length(.x), 0, 1), .x)))

# 2020
num_2020 <- num_2020 |> 
  mutate(across(.cols = everything(), 
                .fns = ~ ifelse(.x == 0, runif(length(.x), 0, 1), .x)))

# 2023
num_2023 <- num_2023 |> 
  mutate(across(.cols = everything(), 
                .fns = ~ ifelse(.x == 0, runif(length(.x), 0, 1), .x)))

#---- applying Yeo-Johnson to negative variables ----

# 2016
num_2016 <- num_2016 |> 
  mutate(across(.cols = where(~ any(.x < 0)), 
                .fns = ~ yeojohnson(.x, standardize = TRUE)$x.t))

# 2020
num_2020 <- num_2020 |> 
  mutate(across(.cols = where(~ any(.x < 0)), 
                .fns = ~ yeojohnson(.x, standardize = TRUE)$x.t))

# 2023
num_2023 <- num_2023 |> 
  mutate(across(.cols = where(~ any(.x < 0)), 
                .fns = ~ yeojohnson(.x, standardize = TRUE)$x.t))

#---- applying Box-Cox transformation to positive variables -----

# 2016
num_2016 <- num_2016 |> 
  mutate(across(.cols = where(~ all(.x > 0)), 
                .fns = ~ boxcox(.x, standardize = TRUE)$x.t))

# 2020
num_2020 <- num_2020 |> 
  mutate(across(.cols = where(~ all(.x > 0)), 
                .fns = ~ boxcox(.x, standardize = TRUE)$x.t))

# 2023 
num_2023 <- num_2023 |> 
  mutate(across(.cols = where(~ all(.x > 0)), 
                .fns = ~ boxcox(.x, standardize = TRUE)$x.t))

#---- studying variable distributions ----

#####  PLOTTING REVENUES 2016   ######
hist_rev_2016 <- ggplot(
  num_2016, aes(ricavi_vendite_e_prestazioni_migl_eur_2016)) + 
  geom_histogram(binwidth = 0.1, colour = "#0072B2", fill = "#999999") +
  labs(x = "Revenues (transformed)", y = "N. Companies", 
       title = "Histogram Revenues 2016") +
  theme_minimal() 

# qqplot 
qqplot_rev_2016 <- ggplot(
  num_2016, aes(sample = ricavi_vendite_e_prestazioni_migl_eur_2016)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(x = "Gaussian Theoretical Quantiles", y = "Sample Quantiles",
    title = "Q-Q Plot Revenues 2016") +
  theme_bw() + 
  theme(panel.grid = element_blank())

transformed_revenues_2016 <- wrap_plots(
  hist_rev_2016, qqplot_rev_2016,
  nrow = 2, ncol = 1, byrow = FALSE)

ggsave(filename = "plot_revenues_transformation_2016.pdf", 
       plot = transformed_revenues_2016, 
       path = "graphics/data-transformation/", 
       width = 127,
       height = 180,
       units = "mm")

####  PLOTTING CURRENT ASSETS 2016   #########
curr_asst_2016 <- ggplot(num_2016, aes(attivo_circolante_migl_eur_2016)) +
  geom_histogram(binwidth = 0.1, colour = "#0072B2", fill = "#999999") +
  labs(x = "Current Assets (transformed)", y = "N. Companies", 
       title = "Histogram Current Assets 2016") +
  theme_minimal() 

# qqplot 
qq_plot_curr_asset_2016 <- ggplot(
  num_2016, aes(sample = attivo_circolante_migl_eur_2016)) + 
  stat_qq() + 
  stat_qq_line() +
  labs(x = "Gaussian Theoretical Quantiles", y = "Sample Quantiles",
       title = "Q-Q Plot Current Assets 2016") +
  theme_bw() + 
  theme(panel.grid = element_blank())

transformed_current_asset_2016 <- wrap_plots(
  curr_asst_2016, qq_plot_curr_asset_2016, 
  nrow = 2, ncol = 1, byrow = FALSE)

ggsave(filename = "plot_current_assets_transformation_2016.pdf", 
       plot = transformed_current_asset_2016, 
       path = "graphics/data-transformation/", 
       width = 127,
       height = 180,
       units = "mm")

######### cash & cash equivalent ############
hist_cash_2016 <- ggplot(num_2016, aes(x = tot_dispon_liquide_migl_eur_2016)) + 
  geom_histogram(binwidth = 0.1, colour = "#0072B2", fill = "#999999") + 
  labs(x = "Cash & Cash Equivalent", y = "N.Companies", 
       title = "Cash & Cash Equivalent") + 
  theme_minimal()

qq_plot_cash_2016 <- ggplot(num_2016, 
                            aes(sample = tot_dispon_liquide_migl_eur_2016)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(x = "Gaussian Theoretical Quantiles", y = "Sample Quantiles", 
       title = "Q-Q Plot Cash & Cash Equivalent") + 
  theme_bw() + 
  theme(panel.grid = element_blank())

transformed_cash_2016 <- wrap_plots(
  hist_cash_2016, qq_plot_cash_2016, 
  nrow = 2, ncol = 1, byrow = FALSE)

ggsave(filename = "plot_cash_transformation_2016.pdf", 
       plot = transformed_cash_2016, 
       path = "graphics/data-transformation/",
       width = 127, 
       height = 180, 
       units = "mm")

######  PLOTTING DEBIT VS BANKS (< 1Y) 2020   ###########
hist_deb_bank_2020 <- ggplot(num_2020, aes(banche_entro_migl_eur_2020)) +
  geom_histogram(binwidth = 0.1, colour = "#0072B2", fill = "#999999") +
  labs(x = "Debit vs Banks (< 1y)", y = "N. Companies", 
       title = "Histogram Debit vs Banks 2020") +
  theme_minimal() 

# qqplot 
qq_plot_debit_bank_2020 <- ggplot(
  num_2020, aes(sample = banche_entro_migl_eur_2020)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(x = "Gaussian Theoretical Quantiles", y = "Sample Quantiles",
       title = "Q-Q Plot Debit vs Banks 2020") +
  theme_bw() + 
  theme(panel.grid = element_blank())

transformed_debit_bank_2020 <- wrap_plots(
  hist_deb_bank_2020, qq_plot_debit_bank_2020, 
  nrow = 2, ncol = 1, byrow = FALSE)

ggsave(filename = "plot_debit_bank_transformation_2020.pdf", 
       plot = transformed_debit_bank_2020, 
       path = "graphics/data-transformation/", 
       width = 127,
       height = 180,
       units = "mm")

#########  PLOTTING CREDIT VS CLIENT 2020 #############
hist_credit_2020 <- ggplot(num_2020, aes(cred_vs_clienti_entro_migl_eur_2020)) +
  geom_histogram(binwidth = 0.1, colour = "#0072B2", fill = "#999999") + 
  labs(x = "Credit vs Clients", y = "N. Companies", 
       title = "Histogram Credit vs Clients 2020 (Box-Cox + Yeo-Johnson)") +
  theme_minimal()

qq_plot_credit_2020 <- ggplot(
  num_2020, aes(sample = cred_vs_clienti_entro_migl_eur_2020)) + 
  stat_qq() + 
  stat_qq_line() +
  labs(x = "Gaussian Theoretical Quantiles", y = "Sample Quantiles", 
       title = "Q-Q Plot Credit vs Clients 2020 (Box-Cox + Yeo-Johnosn)")

transformed_credit_2020 <- wrap_plots(
  hist_credit_2020, qq_plot_credit_2020, 
  nrow = 2, ncol = 1, byrow = FALSE)

#------ SCATTERPLOTS ---------
# 2016
scatt_rev_sal_2016 <- ggplot(
  num_2016, aes(ricavi_vendite_e_prestazioni_migl_eur_2016, 
                totale_costi_del_personale_migl_eur_2016)) + 
  geom_point(colour = "black", alpha = 0.2) + 
  labs(x = "Revenues 2016", y = "Salaries 2016") + 
  theme_minimal()

scatt_credit_profit_2016 <- ggplot(
  num_2016, aes(cred_vs_clienti_entro_migl_eur_2016, 
                utile_perdita_di_esercizio_migl_eur_2016)) + 
  geom_point(colour = "black", alpha = 0.2) + 
  labs(x = "Credit vs Clients 2016", y = "Profit 2016") + 
  theme_minimal()

scatter_2016 <- wrap_plots( scatt_rev_sal_2016,
  scatt_credit_profit_2016,nrow = 2, ncol = 1, byrow = FALSE)

ggsave(filename = "scatterplot_transformed_vars_2016.pdf", 
       plot = scatter_2016, 
       path = "graphics/data-transformation/",
       width = 127,
       height = 180,
       units = "mm")

# 2020
scatt_rev_debt_2020 <- ggplot(
  num_2020,aes(ricavi_vendite_e_prestazioni_migl_eur_2020,
               banche_entro_migl_eur_2020)) + 
  geom_point(colour = "black", alpha = 0.2) + 
  labs(x = "Revenues 2020", y = "Debit vs Banks (> 1y) 2020") + 
  theme_minimal() 

scatt_tangible_longdebit_2020 <- ggplot(
  num_2020, aes(totale_immob_materiali_migl_eur_2020, 
                banche_a_lungo_migl_eur_2020)) + 
  geom_point(colour = "black", alpha = 0.2) + 
  labs(x = "Tangible Assets 2020", y = "Long-Term Debit vs Banks 2020") + 
  theme_minimal()

scatterplot_2020 <- wrap_plots(
  scatt_rev_debt_2020, scatt_tangible_longdebit_2020, 
  nrow = 2, ncol = 1, byrow = FALSE)

ggsave(filename = "scatterplot_transformed_vars_2020.pdf", 
       plot = scatterplot_2020, 
       path = "graphics/data-transformation/",
       width = 127,
       height = 155,
       units = "mm")

# 2023
scatt_bond_net_asset_2023 <- ggplot(
  num_2023, aes(totale_immob_materiali_migl_eur_2023, 
                attivo_circolante_migl_eur_2023)) + 
  geom_point(colour = "black", alpha = 0.2) + 
  labs(x = "Tangible Asset 2023", y = "Current Asset 2023") + 
  theme_minimal()

scatt_stock_rev_2023 <- ggplot(
  num_2023, aes(ricavi_vendite_e_prestazioni_migl_eur_2023, 
                totale_rimanenze_migl_eur_2023)) + 
  geom_point(colour = "black", alpha = 0.2) + 
  labs(x = "Revenues 2023", y = "Inventory 2023") + 
  theme_minimal()

scatterplot_2023 <- wrap_plots(
  scatt_bond_net_asset_2023, scatt_stock_rev_2023, 
  nrow = 2, ncol = 1, byrow = FALSE
)

ggsave(filename = "scatterplot_transformed_vars_2023.pdf", 
       plot = scatterplot_2023, 
       path = "graphics/data-transformation/",
       width = 127,
       height = 180,
       units = "mm")

#---- correlation matrix 2016 ----
corr_2016 <- cor(num_2016, method = "pearson")

cut_off_08_2016 <- findCorrelation(corr_2016, cutoff = 0.8, 
                                   exact = TRUE, names = TRUE, verbose = TRUE)

cut_off_08_2016

cut_off_07_2016 <- findCorrelation(corr_2016, cutoff = 0.7, 
                                   exact = TRUE, names = TRUE, verbose = TRUE)

cut_off_07_2016

#---- correlation matrix 2020 ----
corr_2020 <- cor(num_2020, method = "pearson")
cut_off_08_2020 <- findCorrelation(corr_2020, cutoff = 0.8, 
                                   exact = TRUE, names = TRUE, verbose = TRUE)

cut_off_08_2020

cut_off_07_2020 <- findCorrelation(corr_2020, cutoff = 0.7, 
                                   exact = TRUE, names = TRUE, verbose = TRUE)

cut_off_07_2020

#---- correlation matrix 2023 ----
corr_2023 <- cor(num_2023, method = "pearson")
cut_off_08_2023 <- findCorrelation(corr_2023, cutoff = 0.8, 
                                   exact = TRUE, names = TRUE, verbose = TRUE)

cut_off_08_2023

cut_off_07_2023 <- findCorrelation(corr_2023, cutoff = 0.7, 
                                   exact = TRUE, names = TRUE, verbose = TRUE)

cut_off_07_2023

#---- transforming data into matrix objects ----
matrix_num_2016 <- as.matrix(num_2016)

matrix_num_2020 <- as.matrix(num_2020)

matrix_num_2023 <- as.matrix(num_2023)

#----- saving transformed numeric data ----
save(matrix_num_2016, 
    file = "rdas/transformed-data//transformed_num_2016.rda")

save(matrix_num_2020, 
     file = "rdas/transformed-data//transformed_num_2020.rda")

save(matrix_num_2023, 
     file = "rdas/transformed-data//transformed_num_2023.rda")
