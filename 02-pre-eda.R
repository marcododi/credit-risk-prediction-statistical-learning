library(tidyverse)
library(kableExtra)
library(stringr)
library(ggplot2)
library(ggthemes)
library(tidytext)
library(patchwork)
library(e1071)

#---- uploading data ----
load("rdas/clean-data/final_dataset_2016.rda")
load("rdas/clean-data/final_dataset_2020.rda")
load("rdas/clean-data/final_dataset_2023.rda")

# converting data from data.table to tibble in order to use the tidyverse
df_2016 <- as_tibble(final_dataset_2016)
df_2020 <- as_tibble(final_dataset_2020)
df_2023 <- as_tibble(final_dataset_2023)

#---- uploading colors for following graphics -----
colorblind_friendly_colors <- 
  c("#999999", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#---- extracting qualitative variables by year -----
info_2016 <- df_2016 |> 
  select(ragione_sociale, forma_giuridica, stato_giuridico, anno_costituzione, 
         nace_rev_2, nace_rev_2_descrizione, macrosector_nace_rev_2, 
         macrosector_nace_rev_2_descrizione, defaults) |> 
  arrange(macrosector_nace_rev_2)

info_2020 <- df_2020 |> 
  select(ragione_sociale, forma_giuridica, stato_giuridico, anno_costituzione, 
         nace_rev_2, nace_rev_2_descrizione, macrosector_nace_rev_2, 
         macrosector_nace_rev_2_descrizione, defaults) |> 
  arrange(macrosector_nace_rev_2)

info_2023 <- df_2023 |> 
  select(ragione_sociale, forma_giuridica, stato_giuridico, anno_costituzione, 
         nace_rev_2, nace_rev_2_descrizione, macrosector_nace_rev_2, 
         macrosector_nace_rev_2_descrizione, defaults) |> 
  arrange(macrosector_nace_rev_2)

#---- counting defaults by year -----
summary_year_2016 <- info_2016 |> 
  summarise(ref_year = "2016",
            n_companies = n(), 
            n_active_companies = sum(defaults == 0),
            n_defaults = sum(defaults == 1))

summary_year_2020 <- info_2020 |> 
  summarise(ref_year = "2020",
            n_companies = n(), 
            n_active_companies = sum(defaults == 0),
            n_defaults = sum(defaults == 1))

summary_year_2023 <- info_2023 |> 
  summarise(ref_year = "2023", 
            n_companies = n(), 
            n_active_companies = sum(defaults == 0),
            n_defaults = sum(defaults == 1))

complete_year_summary <- summary_year_2016 |> 
  bind_rows(summary_year_2020) |> 
  bind_rows(summary_year_2023)

# generating tables to export in latex
tbl_complete_year_summary <- kable(
  complete_year_summary, 
  format = "latex", 
  col.names = c("Anno", "Numero Imprese", "Numero Imprese Attive", 
                "Numero Insolvenze"), 
  booktabs = TRUE,
  linesep = "",
  caption = "Numero di insolvenze nei tre periodi", 
  align = "c", 
  label = "table:default_by_year",
  position = "H")

cat(tbl_complete_year_summary, 
    file = "tables/table-pre-eda/table_default_by_year.tex")

#---- counting default by legal form ----
summary_lf_2016 <- info_2016 |> 
  summarise(n_companies_2016 = n(), 
            n_active_companies_2016 = sum(defaults == 0),
            n_defaults_2016 = sum(defaults == 1), 
            default_rate_2016 = round(sum(defaults == 1) / n(), digits = 4), 
            .by = forma_giuridica)

summary_lf_2020 <- info_2020 |> 
  summarise(n_companies_2020 = n(), 
            n_active_companies_2020 = sum(defaults == 0), 
            n_defaults_2020 = sum(defaults == 1), 
            default_rate_2020 = round(sum(defaults == 1) / n(), digits = 4), 
            .by = forma_giuridica)

summary_lf_2023 <- info_2023 |> 
  summarise(n_companies_2023 = n(), 
            n_active_companies_2023 = sum(defaults == 0), 
            n_defaults_2023 = sum(defaults == 1), 
            default_rate_2023 = round(sum(defaults == 1) / n(), digits = 4), 
            .by = forma_giuridica)

complete_summary_lf <- summary_lf_2016 |> 
  left_join(summary_lf_2020, by = "forma_giuridica") |> 
  left_join(summary_lf_2023, by = "forma_giuridica")


long_complete_summary_lf <- complete_summary_lf |> 
  pivot_longer(-forma_giuridica, 
               names_to = c(".value", "ref_year"), 
               names_pattern = "^(\\w+)_(\\d+$)") |> 
  arrange(ref_year)

# graphic
long_summary_lf <- complete_summary_lf |> 
  select(forma_giuridica, n_defaults_2016, n_defaults_2020, 
         n_defaults_2023) |> 
  pivot_longer(-forma_giuridica, names_to = "ref_year",
               names_prefix = "n_defaults_",
               values_to = "number_default")

ggplot(long_summary_lf, aes(ref_year, fill = forma_giuridica)) + 
  geom_col(aes(y = number_default), position = "dodge") + 
  scale_fill_manual(values = colorblind_friendly_colors) + 
  labs(x = "Years", y = "N. Defaults", 
       title = "N. Default by Legal Form") + 
  theme_minimal()

ggsave(filename = "default_legal_form.pdf",
       plot = last_plot(), 
       path = "graphics/pre-EDA/", 
       width = 125, 
       height = 105, 
       units = "mm")

#---- counting default by macro sector ----
summary_sector_2016 <- info_2016 |> 
  summarise(n_companies_2016 = n(), 
            n_active_companies_2016 = sum(defaults == 0), 
            n_defaults_2016 = sum(defaults == 1), 
            default_rate_2016 = round(sum(defaults == 1) / n(), digits = 4), 
            .by = c(macrosector_nace_rev_2, macrosector_nace_rev_2_descrizione))

summary_sector_2020 <- info_2020 |> 
  summarise(n_companies_2020 = n(), 
            n_active_companies_2020 = sum(defaults == 0), 
            n_defaults_2020 = sum(defaults == 1), 
            default_rate_2020 = round(sum(defaults == 1) / n(), digits = 4), 
            .by = c(macrosector_nace_rev_2, macrosector_nace_rev_2_descrizione))

summary_sector_2023 <- info_2023 |> 
  summarise(n_companies_2023 = n(), 
            n_active_companies_2023 = sum(defaults == 0), 
            n_defaults_2023 = sum(defaults == 1), 
            default_rate_2023 = round(sum(defaults == 1) / n(), digits = 4), 
            .by = c(macrosector_nace_rev_2, macrosector_nace_rev_2_descrizione))

complete_sector_summary <- summary_sector_2016 |> 
  left_join(summary_sector_2020,
            by = c("macrosector_nace_rev_2", 
                   "macrosector_nace_rev_2_descrizione")) |> 
  left_join(summary_sector_2023, by = c("macrosector_nace_rev_2", 
                                        "macrosector_nace_rev_2_descrizione"))

# plot: number of companies per industry 
long_companies_sector <- complete_sector_summary |> 
  select(macrosector_nace_rev_2, macrosector_nace_rev_2_descrizione, 
         n_companies_2016, n_companies_2020, n_companies_2023) |> 
  pivot_longer(-c(macrosector_nace_rev_2, macrosector_nace_rev_2_descrizione), 
               names_to = "ref_year", names_prefix = "n_companies_", 
               values_to = "comp_number") |> 
  group_by(ref_year) |> 
  arrange(desc(comp_number)) |> 
  slice_max(comp_number, n = 10) |> 
  ungroup() |> 
  mutate(macrosector_nace_rev_2_descrizione = reorder_within(
    macrosector_nace_rev_2_descrizione, comp_number, ref_year))

ggplot(long_companies_sector, aes(x = comp_number, fill = ref_year )) +
  geom_col(aes(y = macrosector_nace_rev_2_descrizione)) + 
  scale_fill_manual(values = colorblind_friendly_colors) + 
  scale_y_reordered() + 
  labs(x = "N. Companies", y = "General Sector Description", 
       title = "Ten Industries with Higher Number of Companies") + 
  theme_minimal() + 
  facet_wrap(~ref_year, nrow = 3, scales = "free_y") + 
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 7, margin = margin(t = 3, b = 3)),
        plot.title.position = "plot")

ggsave(filename = "top_ten_sector_n_companies.pdf",
       plot = last_plot(), 
       path = "graphics/pre-EDA/", 
       width = 127, 
       height = 100, 
       units = "mm")

# graphic visualization top 10 number of defaults by year
long_summary_sector <- complete_sector_summary |> 
  select(macrosector_nace_rev_2,macrosector_nace_rev_2_descrizione, 
         n_defaults_2016, n_defaults_2020, n_defaults_2023) |> 
  pivot_longer(-c(macrosector_nace_rev_2, macrosector_nace_rev_2_descrizione), 
               names_to = "ref_years", names_prefix = "n_defaults_",
               values_to = "number_defaults") |> 
  group_by(ref_years) |> 
  arrange(desc(number_defaults)) |> 
  slice_max(number_defaults, n = 10) |> 
  ungroup() |> 
  mutate(macrosector_nace_rev_2_descrizione = reorder_within(
    macrosector_nace_rev_2_descrizione, 
    number_defaults, ref_years))

# default plot
ggplot(long_summary_sector,
       aes(number_defaults, fill = ref_years)) + 
  geom_col(aes(y = macrosector_nace_rev_2_descrizione)) + 
  scale_fill_manual(values = colorblind_friendly_colors) + 
  scale_y_reordered() + 
  labs(x = "N. Defaults", y = "General Sector Description", 
       title = "Ten Industries Higher Number of Defaults") + 
  theme_minimal() + 
  facet_wrap(~ref_years, nrow = 3, scales = "free_y") + 
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 7, margin = margin(t = 3, b = 3)), 
        plot.title.position = "plot")

ggsave(filename = "top_ten_sector_n_default.pdf",
       plot = last_plot(), 
       path = "graphics/pre-EDA/", 
       width = 127, 
       height = 105, 
       units = "mm")

#---- studying numeric data ----
numeric_2016 <- df_2016 |> 
  select(ends_with("2016"))

numeric_2020 <- df_2020 |> 
  select(ends_with("2020"))

numeric_2023 <- df_2023 |> 
  select(ends_with("2023"))

#########  PLOTTING REVENUES 2016   #####################

# histogram raw data
ggplot(numeric_2016, aes(ricavi_vendite_e_prestazioni_migl_eur_2016)) + 
  geom_histogram(bins = 100, colour = "#0072B2", fill = "#999999") +
  scale_x_continuous(breaks = seq(0, 100000, 20000), limits = c(0, 100000)) +
  labs(x = "Revenues", y = "N. Companies", 
       title = "Histogram Revenues 2016") +
  theme_minimal()

ggsave(filename = "ist_raw_revenues_2016.pdf",
       plot = last_plot(), 
       path = "graphics/pre-EDA/", 
       width = 115, 
       height = 70, 
       units = "mm")

# boxplot raw data
bxplot_rev_raw_2016 <- ggplot(
  numeric_2016, aes(x = ricavi_vendite_e_prestazioni_migl_eur_2016)) + 
  geom_boxplot(fill = "#999999", colour = "#0072B2", 
               outlier.colour = "#D55E00") + 
  labs(x = "Revenues", title = "Boxplot Revenues 2016") + 
  theme_minimal() + 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

# qqplot raw data
qqraw_rev_2016 <- ggplot(
  numeric_2016, aes(sample = ricavi_vendite_e_prestazioni_migl_eur_2016)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(x = "Gaussian Theoretical Quantiles", 
       y = "Sample Quantiles", 
       title = "Q-Q Plot Revenues 2016") + 
  theme_bw() + 
  theme(panel.grid = element_blank())

raw_rev_plot <- wrap_plots(
  bxplot_rev_raw_2016, qqraw_rev_2016, 
  nrow = 2, ncol = 1)

ggsave(filename = "box_qqplot_raw_revenues_2016.pdf",
       plot = raw_rev_plot, 
       path = "graphics/pre-EDA/", 
       width = 115, 
       height = 110, 
       units = "mm")

# histogram lo10 transformed data
hist_log_rev_16 <- ggplot(
  numeric_2016, aes(ricavi_vendite_e_prestazioni_migl_eur_2016)) + 
  geom_histogram(binwidth = 0.1, colour = "#0072B2", fill = "#999999") +
  scale_x_log10() +
  labs(x = "Revenues (log-10 scale)", y = "N. Companies", 
       title = "Histogram Revenues 2016") +
  theme_minimal() 
  
# boxplot log10 transformed data
box_log_rev_16 <- ggplot(
  numeric_2016, aes(ricavi_vendite_e_prestazioni_migl_eur_2016)) + 
  geom_boxplot(fill = "#999999", colour = "#0072B2", 
               outlier.colour = "#D55E00") + 
  scale_x_log10() +    
  labs(x = "Revenues (log-10 scale)", title = "Boxplot Revenues 2016") + 
  theme_minimal() + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank())

# qqplot vs theoretical gaussian quantiles
# log10 transformed data
rev_16_pos <- (numeric_2016$ricavi_vendite_e_prestazioni_migl_eur_2016 > 0)
val_rev_pos_16 <- as.data.frame(
  numeric_2016$ricavi_vendite_e_prestazioni_migl_eur_2016[rev_16_pos]) |> 
  rename(positive_revenues_16 = 
    "numeric_2016$ricavi_vendite_e_prestazioni_migl_eur_2016[rev_16_pos]") |> 
  mutate(log_rev_16 = log10(positive_revenues_16))

qq_rev_16 <- ggplot(val_rev_pos_16, aes(sample = log_rev_16)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(x = "Gaussian Theoretical Quantiles", 
       y = "Sample Quantiles", 
       title = "Q-Q Plot Revenues 2016 (log10 transformed)") + 
  theme_bw() + 
  theme(panel.grid = element_blank())

revenues_plot_2016 <- wrap_plots(hist_log_rev_16, box_log_rev_16, qq_rev_16,
                             nrow = 3, ncol = 1, byrow = FALSE)

ggsave(filename = "plot_rev_log_10_transform_2016.pdf", 
       plot = revenues_plot_2016, 
       path = "graphics/pre-EDA/", 
       width = 127,
       height = 180,
       units = "mm")


#########  PLOTTING CURRENT ASSETS 2016   #####################

# histogram raw data 
hist_raw_cur_asset_2016 <- ggplot(
  numeric_2016, aes(attivo_circolante_migl_eur_2016)) + 
  geom_histogram(bins = 100, colour = "#0072B2", fill = "#999999") +
  scale_x_continuous(breaks = seq(0, 100000, 20000), limits = c(0, 100000)) +
  labs(x = "Current Assets", y = "N. Companies", 
       title = "Histogram Current Assets 2016") +
  theme_minimal()

# histogram log10 current assets
hist_log_curr_asset_2016 <- ggplot(
  numeric_2016, aes(attivo_circolante_migl_eur_2016)) + 
  geom_histogram(binwidth = 0.1, colour = "#0072B2", fill = "#999999") + 
  scale_x_log10() + 
  labs(x = "Current Assets (log-10 scale)", y = "N. Companies", 
       title = "Histogram Current Assets 2016") +
  theme_minimal() 

# boxplot log10 current assets 
box_log_curr_asset_2016 <- ggplot(
  numeric_2016, aes(attivo_circolante_migl_eur_2016)) +
  geom_boxplot(fill = "#999999", colour = "#0072B2", 
               outlier.colour = "#D55E00") + 
  scale_x_log10() +    
  labs(x = "Current Assets (log-10 scale)", 
       title = "Boxplot Current Assets 2016") + 
  theme_minimal() + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank())

# qqplot vs theoretical gaussian quantiles
# log10 transformed data
curr_asset_2016_pos <- (numeric_2016$attivo_circolante_migl_eur_2016 > 0)
val_curr_asset2016_pos <- as.data.frame(
  numeric_2016$attivo_circolante_migl_eur_2016[curr_asset_2016_pos]) |> 
  rename(positive_current_asset =
           "numeric_2016$attivo_circolante_migl_eur_2016[curr_asset_2016_pos]") |> 
  mutate(log_10_curr_asset = log10(positive_current_asset))

qq_cur_asset_2016 <- ggplot(
  val_curr_asset2016_pos, aes(sample = log_10_curr_asset)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(x = "Gaussian Theoretical Quantiles", 
       y = "Sample Quantiles", 
       title = "Q-Q Plot Current Assets 2016 (log10 transformed)") + 
  theme_bw() + 
  theme(panel.grid = element_blank())

current_asset_2016_plot <- wrap_plots(
  hist_raw_cur_asset_2016, hist_log_curr_asset_2016,
  box_log_curr_asset_2016, qq_cur_asset_2016, 
  nrow = 4, ncol = 1, byrow = FALSE)

ggsave(filename = "plot_curr_asset_transform_2016.pdf", 
       plot = current_asset_2016_plot, 
       path = "graphics/pre-EDA/", 
       width = 127,
       height = 180,
       units = "mm")

#########  PLOTTING DEBIT VS BANKS LONG-TERM 2016 ##############

# histogram debit vs banks long-term raw data
hist_raw_long_deb_bank_2016 <- ggplot(numeric_2016, 
                                  aes(banche_a_lungo_migl_eur_2016)) +
  geom_histogram(bins = 100, colour = "#0072B2", fill = "#999999") +
  scale_x_continuous(breaks = seq(0, 100000, 20000), limits = c(0, 100000)) +
  labs(x = "Debit vs Banks ", y = "N. Companies", 
       title = "Histogram Debit vs Banks (long-term) 2016") +
  theme_minimal()

# histogram  log10 debit vs banks long-term
hist_log10_long_deb_bank_2016 <- ggplot(numeric_2016, 
                                        aes(banche_a_lungo_migl_eur_2016)) + 
  geom_histogram(binwidth = 0.1, colour = "#0072B2", fill = "#999999") +
  scale_x_log10() + 
  labs(x = "Debit vs Banks (log10 scale)", y = "N.Companies", 
       title = "Histogram Debit vs Banks (long-term) 2016") + 
  theme_minimal()

# boxplot log10 debit vs banks long-term
box_log10_long_deb_bank_2016 <- ggplot(numeric_2016, 
                                       aes(banche_a_lungo_migl_eur_2016)) + 
  geom_boxplot(fill = "#999999", colour = "#0072B2", 
               outlier.colour = "#D55E00") + 
  scale_x_log10() + 
  labs(x = "Debit vs Banks (log10 scale)", 
       title = "Boxplot Debit vs Banks (long-term) 2016") + 
  theme_minimal() + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank())

# qqplot vs theoretical gaussian quantiles
# log10 transformed data  
long_deb_pos_2016 <- (numeric_2016$banche_a_lungo_migl_eur_2016 > 0)  
val_long_deb_pos_2016 <- as.data.frame(
  numeric_2016$banche_a_lungo_migl_eur_2016[long_deb_pos_2016]) |> 
  rename(positive_long_deb = 
           "numeric_2016$banche_a_lungo_migl_eur_2016[long_deb_pos_2016]") |> 
  mutate(log_10_long_deb = log10(positive_long_deb))

qq_long_deb_bank_2016 <-ggplot(val_long_deb_pos_2016, 
                               aes(sample = log_10_long_deb)) +
  stat_qq() + 
  stat_qq_line() +
  labs(x = "Gaussian Theoretical Quantiles", 
       y = "Sample Quantiles", 
       title = "Q-Q Plot Debit vs Banks (long-term) 2016") + 
  theme_bw() + 
  theme(panel.grid = element_blank())

long_deb_banks_2016 <- wrap_plots(
  hist_raw_long_deb_bank_2016, hist_log10_long_deb_bank_2016, 
  box_log10_long_deb_bank_2016, qq_long_deb_bank_2016, 
  nrow = 4, ncol = 1, byrow = FALSE)

ggsave(filename = "plot_long_deb_banks_log10_transform_2016.pdf",
       plot = long_deb_banks_2016,
       path = "graphics/pre-EDA/",
       width = 127,
       height = 180,
       units = "mm")

#########    PLOTTING DEBIT VS BANKS (< 1Y) 2020     ############
# DEBT vs banks < 1y

# hist log10 data
hist_log_deb_2020 <-  ggplot(numeric_2020, aes(x = banche_entro_migl_eur_2020)) + 
  geom_histogram(binwidth = 0.1, colour = "#0072B2", fill = "#999999") +
  scale_x_log10() + 
  labs(x = "Debit vs Banks (log10-scale)", y = "N. Companies", 
       title = "Histogram Debit vs Banks (< 1y) 2020") +
  theme_minimal()

# boxplot log10 data
box_log_deb_2020 <- ggplot(numeric_2020, aes(x = banche_entro_migl_eur_2020)) + 
  geom_boxplot(fill = "#999999", colour = "#0072B2", 
               outlier.colour = "#D55E00") + 
  scale_x_log10() + 
  labs(x = "Debit vs Banks (log10-scale)", 
       title = "Boxplot Debit vs Banks (<1y) 2020") + 
  theme_minimal() + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank())

# qqplot log10 data 
deb_pos_2020 <- (numeric_2020$banche_entro_migl_eur_2020 > 0)
val_deb_pos_2020 <- as.data.frame(
  numeric_2020$banche_entro_migl_eur_2020[deb_pos_2020]) |> 
  rename(
  positive_deb_2020 = "numeric_2020$banche_entro_migl_eur_2020[deb_pos_2020]") |> 
  mutate(log_deb_2020 = log10(positive_deb_2020))

qq_log_deb_2020 <- ggplot(val_deb_pos_2020, aes(sample = log_deb_2020)) +
  stat_qq() + 
  stat_qq_line() + 
  labs(x = "Gaussian Theoretical Quantiles", 
       y = "Sample Quantiles", 
       title = "Q-Q Plot Debit vs Banks (< 1y) 2020") + 
  theme_bw() + 
  theme(panel.grid = element_blank())

short_deb_banks_plot_2020 <- wrap_plots(hist_log_deb_2020, box_log_deb_2020,
                               qq_log_deb_2020, 
                               nrow = 3, ncol = 1, byrow = FALSE)

ggsave(filename = "plot_deb_log_10_transform_2020.pdf",
       plot = single_plot_2020, 
       path = "graphics/pre-EDA/", 
       width = 127, 
       height = 180, 
       units = "mm")

######  PLOTTING REVENUES 2020   ###########
# histogram raw data 
hist_rev_raw_2020 <- ggplot(
  numeric_2020, aes(ricavi_vendite_e_prestazioni_migl_eur_2020)) + 
  geom_histogram(bins = 100, colour = "#0072B2", fill = "#999999") +
  scale_x_continuous(breaks = seq(0, 100000, 20000), limits = c(0, 100000)) +
  labs(x = "Revenues", y = "N. Companies", 
       title = "Histogram Revenues 2020") +
  theme_minimal()

# histogram lo10 transformed data
hist_log_rev_2020 <- ggplot(
  numeric_2020, aes(ricavi_vendite_e_prestazioni_migl_eur_2020)) + 
  geom_histogram(binwidth = 0.1, colour = "#0072B2", fill = "#999999") +
  scale_x_log10() +
  labs(x = "Revenues (log-10 scale)", y = "N. Companies", 
       title = "Histogram Revenues 2020") +
  theme_minimal() 

# boxplot log10 transformed data
box_log_rev_2020 <- ggplot(
  numeric_2020, aes(ricavi_vendite_e_prestazioni_migl_eur_2020)) + 
  geom_boxplot(fill = "#999999", colour = "#0072B2", 
               outlier.colour = "#D55E00") + 
  scale_x_log10() +    
  labs(x = "Revenues (log-10 scale)", title = "Boxplot Revenues 2020") + 
  theme_minimal() + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank())

# qqplot vs theoretical gaussian quantiles
# log10 transformed data
rev_2020_pos <- (numeric_2020$ricavi_vendite_e_prestazioni_migl_eur_2020 > 0)
val_rev_pos_2020 <- as.data.frame(
  numeric_2020$ricavi_vendite_e_prestazioni_migl_eur_2020[rev_2020_pos]) |> 
  rename(positive_revenues =
    "numeric_2020$ricavi_vendite_e_prestazioni_migl_eur_2020[rev_2020_pos]") |> 
  mutate(log_rev_2020 = log10(positive_revenues))

qq_log_rev_2020 <- ggplot(val_rev_pos_2020, aes(sample = log_rev_2020)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(x = "Gaussian Theoretical Quantiles", 
       y = "Sample Quantiles", 
       title = "Q-Q Plot Revenues 2020 (log10 transformed)") + 
  theme_bw() + 
  theme(panel.grid = element_blank())

revenues_plot_2020 <- wrap_plots(
  hist_rev_raw_2020, hist_log_rev_2020, box_log_rev_2020,
  qq_log_rev_2020, nrow = 4, ncol = 1, byrow = FALSE)

ggsave(filename = "plot_rev_log10_transform_2020.pdf", 
       plot = revenues_plot_2020, 
       path = "graphics/pre-EDA/", 
       width = 127,
       height = 180,
       units = "mm")

#########  PLOTTING CURRENT ASSETS 2020   ############
# histogram raw data 
hist_raw_cur_asset_2020 <- ggplot(
  numeric_2020, aes(attivo_circolante_migl_eur_2020)) + 
  geom_histogram(bins = 100, colour = "#0072B2", fill = "#999999") +
  scale_x_continuous(breaks = seq(0, 100000, 20000), limits = c(0, 100000)) +
  labs(x = "Current Assets", y = "N. Companies", 
       title = "Histogram Current Assets 2020") +
  theme_minimal()

# histogram log10 current assets
hist_log_curr_asset_2020 <- ggplot(
  numeric_2020, aes(attivo_circolante_migl_eur_2020)) + 
  geom_histogram(binwidth = 0.1, colour = "#0072B2", fill = "#999999") + 
  scale_x_log10() + 
  labs(x = "Current Assets (log-10 scale)", y = "N. Companies", 
       title = "Histogram Current Assets 2020") +
  theme_minimal() 

# boxplot log10 current assets 
box_log_curr_asset_2020 <- ggplot(
  numeric_2020, aes(attivo_circolante_migl_eur_2020)) +
  geom_boxplot(fill = "#999999", colour = "#0072B2", 
               outlier.colour = "#D55E00") + 
  scale_x_log10() +    
  labs(x = "Current Assets (log-10 scale)", 
       title = "Boxplot Current Assets 2020") + 
  theme_minimal() + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank())

# qqplot vs theoretical gaussian quantiles
# log10 transformed data
curr_asset_2020_pos <- (numeric_2020$attivo_circolante_migl_eur_2020 > 0)
val_curr_asset2020_pos <- as.data.frame(
  numeric_2020$attivo_circolante_migl_eur_2020[curr_asset_2020_pos]) |> 
  rename(positive_current_asset =
           "numeric_2020$attivo_circolante_migl_eur_2020[curr_asset_2020_pos]") |> 
  mutate(log_10_curr_asset = log10(positive_current_asset))

qq_cur_asset_2020 <- ggplot(
  val_curr_asset2020_pos, aes(sample = log_10_curr_asset)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(x = "Gaussian Theoretical Quantiles", 
       y = "Sample Quantiles", 
       title = "Q-Q Plot Current Assets 2020 (log10 transformed)") + 
  theme_bw() + 
  theme(panel.grid = element_blank())

current_asset_2020_plot <- wrap_plots(
  hist_raw_cur_asset_2020, hist_log_curr_asset_2020,
  box_log_curr_asset_2020, qq_cur_asset_2020, 
  nrow = 4, ncol = 1, byrow = FALSE)

ggsave(filename = "plot_curr_asset_transform_2020.pdf", 
       plot = current_asset_2020_plot, 
       path = "graphics/pre-EDA/", 
       width = 127,
       height = 180,
       units = "mm")

#---- computing kurthosis on each numeric column by year ---- 
# implementing type 2, thus the unbiased kurtosis estimator

# 2016
kurt_2016 <- apply(X = numeric_2016, MARGIN = 2,FUN = function(t){
  kurtosis(t, type = 2)
})

df_kurt_2016 <- as.data.frame(kurt_2016) |> 
  rownames_to_column(var = "financial_report_variables") |> 
  rename("kurt" = kurt_2016) |> 
  mutate(ref_year = str_extract(string = financial_report_variables, 
                                pattern = "\\d+$"), 
         financial_report_variables = str_remove(
           string = financial_report_variables,
           pattern = "[_](\\w{4})[_](\\w{3})[_](\\d+$)")) |> 
  select(financial_report_variables, ref_year, kurt)

# 2020 
kurt_2020 <- apply(X = numeric_2020, MARGIN = 2, FUN = function(t){
  kurtosis(x = t, type = 2)
})

df_kurt_2020 <- as.data.frame(kurt_2020) |> 
  rownames_to_column(var = "financial_report_variables") |> 
  rename("kurt" = kurt_2020) |> 
  mutate(ref_year = str_extract(string = financial_report_variables, 
                                pattern = "\\d+$"), 
         financial_report_variables = str_remove(
           string = financial_report_variables, 
           pattern = "[_](\\w{4})[_](\\w{3})[_](\\d+$)")) |> 
  select(financial_report_variables, ref_year, kurt)

# 2023
kurt_2023 <- apply(X = numeric_2023, MARGIN = 2, FUN = function(t){
  kurtosis(t, type = 2)
})

df_kurt_2023 <- as.data.frame(kurt_2023) |> 
  rownames_to_column(var = "financial_report_variables") |> 
  rename("kurt" = kurt_2023) |> 
  mutate(ref_year = str_extract(string = financial_report_variables, 
                                pattern = "\\d+$"), 
         financial_report_variables = str_remove(
           string = financial_report_variables, 
           pattern = "[_](\\w{4})[_](\\w{3})[_](\\d+$)")) |> 
  select(financial_report_variables, ref_year, kurt)


compl_kurt_df <- left_join(
  df_kurt_2016, df_kurt_2020, by = "financial_report_variables") |> 
  left_join(df_kurt_2023, by = "financial_report_variables") |> 
  rename_with(.fn = ~ c("Kurtosis_2016", "Kurtosis_2020", "Kurtosis_2023"), 
              .cols = starts_with("kurt")) |> 
  select(- starts_with("ref_year"))

tbl_kurtosis <- kable(
  compl_kurt_df, 
  format = "latex", 
  col.names = c("Variabili di bilancio", "Curtosi 2016", 
                "Curtosi 2020", "Curtosi 2023"), 
  booktabs = TRUE, 
  linesep = "\\addlinespace", 
  caption = "Indice di curtosi delle variabili di bilancio per anno", 
  align = c("l", "c", "c", "c"), 
  label = "table:kurtvariables", 
  position = "H") |>
  column_spec(2:4, width = "2cm")

cat(tbl_kurtosis, 
    file = "tables/table-pre-eda/kurtosis_vars_table.tex")

#---- saving info datasets ----
save(info_2016, file = "rdas/info/tbl_info_2016.rda")
save(info_2020, file = "rdas/info/tbl_info_2020.rda")
save(info_2023, file = "rdas/info/tbl_info_2023.rda")
