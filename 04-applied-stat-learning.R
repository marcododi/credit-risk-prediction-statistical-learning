library(tidyverse)
library(glmnet)
library(MASS)
library(rpart)
library(ggplot2)
library(ggthemes)
library(pROC)
library(stringr)
library(kableExtra)

#---- uploading data -----
load("rdas/clean-data/final_dataset_2016.rda")
load("rdas/clean-data/final_dataset_2020.rda")
load("rdas/clean-data/final_dataset_2023.rda")

# loading numeric data 
load("rdas/transformed-data/transformed_num_2016.rda")
load("rdas/transformed-data/transformed_num_2020.rda")
load("rdas/transformed-data/transformed_num_2023.rda")

# loading sector dummy
load("rdas/dummy-sector/dummy_sector_2016.rda")
load("rdas/dummy-sector/dummy_sector_2020.rda")
load("rdas/dummy-sector/dummy_sector_2023.rda")

#---- uploading colors for following graphics -----
colorblind_friendly_colors <- 
  c("#999999", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#---- applying Logistic Regression with Lasso on 2016 data -----

###### set.seed for cross-validation ##########
set.seed(1010)

x_2016 <- cbind(matrix_num_2016, dummy_sector_2016)

fit_lasso_2016 <- cv.glmnet(y = final_dataset_2016$defaults,
                            x = x_2016, family = "binomial", 
                            alpha = 1, nfolds = 10, type.measure = "deviance")

# cross-validation plot
plot(fit_lasso_2016)
mtext("Cross-Validation 2016 (K = 10)", side = 3, 
      line = 2.8, cex = 1.2, font = 2)

# extracting coefficient conservative lambda
coeff_lasso_2016 <- coef(fit_lasso_2016, "lambda.1se")
coeff_lasso_2016

# plotting coeff
lasso_2016_coef_df <- as.matrix(coeff_lasso_2016) |> 
  as.data.frame() |> 
  rownames_to_column(var = "beta_names")|> 
  rename(beta_values = "lambda.1se") |> 
  mutate(beta_names = str_remove_all(string = beta_names, 
                           pattern = "[_]\\w{4}[_]\\w{3}[_]\\d+$"),
         beta_names = str_remove_all(string = beta_names, 
                                     pattern = "^general_sector"),
         beta_names = str_remove_all(string = beta_names, 
                                       pattern = "^tot(ale)?_"),
         beta_names = as.factor(beta_names)) |> 
  filter(beta_values != 0) |> 
  filter(!beta_names %in% c("(Intercept)", "alimentare", "cellulosa", 
                         "comunicazione", "edilizia", "energia",
                         "estrazione", "immobiliare", "manifattura", 
                         "professionali", "servizi", "tessile", "trasporto", 
                         "vitto_alloggio")) |> 
  mutate(beta_names = fct_reorder(beta_names, beta_values,.desc = TRUE)) |> 
  mutate(beta_sign = ifelse(beta_values > 0, "positive", "negative"))

ggplot(lasso_2016_coef_df, aes(beta_names, beta_values, fill =  beta_sign)) + 
  geom_col(width = 0.5) + 
  coord_flip() + 
  geom_text(aes(x = beta_names, y = 0 ,label = beta_names),
            hjust = ifelse(
              lasso_2016_coef_df$beta_sign == "positive", 1.3, -0.5), size = 2) +
  scale_y_continuous(breaks = seq(-0.50, 0.30, 0.05), limits = c(-0.50, 0.30)) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), name = "Coeff. Sign") +
  labs(x = "", y = "Values") +
theme(legend.position = "bottom", 
      legend.title = element_text(size = 8),
      legend.key.size = unit(3, "mm"),
      axis.title.x = element_text(vjust = -2, size = 8), 
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank()) 

ggsave(filename = "lasso_coefficient_2016.pdf", 
       plot = last_plot(), 
       path = "results/logistic-lasso/", 
       height = 85, 
       width = 130,
       units = "mm")

# computing fitted values
hat_pi_2016 <- glmnet:::predict.cv.glmnet(
  fit_lasso_2016, newx = x_2016, s = "lambda.1se", type = "response"
)

summary_lasso_2016 <- summary(hat_pi_2016)

# roc curve 2016
rc_lasso_2016 <- roc(response = final_dataset_2016$defaults,
               predictor = as.numeric(hat_pi_2016))

ggroc(rc_lasso_2016, legacy.axes = TRUE) + 
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = "dashed") + 
  annotate("point", x = c(0, 1), y = c(0, 1), size = 1) + 
  labs(x = "1 - Specificity", y = "Sensitivity", 
       title = "ROC Curve 2016") + 
  theme_base() + 
  theme(panel.background = element_blank()) + 
  theme(plot.title = element_text(size = 13), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8))

ggsave(filename = "lasso_roc_curve_2016.pdf",
       plot = last_plot(),
       path = "results/logistic-lasso/", 
       height = 100, 
       width = 125, 
       units = "mm")

# area under the curve
auc(rc_lasso_2016)

# confusion matrix lasso 2016
tau_lasso_2016 <- mean(hat_pi_2016)
label_lasso_2016 <- ifelse(hat_pi_2016 > tau_lasso_2016, 1, 0)

lasso_2016_confusion_matrix <- table(final_dataset_2016$defaults, 
                                     label_lasso_2016)

# computing sensitivity, specficity and hit rate
sensitivity_lasso_2016 <- lasso_2016_confusion_matrix[2, 2] / sum(
  lasso_2016_confusion_matrix[2, ])


specificity_lasso_2016 <- lasso_2016_confusion_matrix[1, 1] / sum(
  lasso_2016_confusion_matrix[1, ])

hit_rate_lasso_2016 <- sum(
  lasso_2016_confusion_matrix[1, 1], lasso_2016_confusion_matrix[2, 2]) / sum(
    lasso_2016_confusion_matrix[1, ], lasso_2016_confusion_matrix[2, ])

rownames(lasso_2016_confusion_matrix) <- c("$Y = 0$", "$Y = 1$")

#---- applying Logistic Regression with Lasso on 2020 data -----

####### set.seed for cross-validation ##############
set.seed(1010)

x_2020 <- cbind(matrix_num_2020, dummy_sector_2020)

fit_lasso_2020 <- cv.glmnet(y = final_dataset_2020$defaults, 
                            x = x_2020, family = "binomial", 
                            alpha = 1, nfolds = 10, type.measure = "deviance")

# cross-validation plot
plot(fit_lasso_2020)
mtext("Cross-Validation Plot 2020 K = 10", side = 3, 
      line = 2.8, cex = 1.2, font = 2)

coeff_lasso_2020 <- coef(fit_lasso_2020)
coeff_lasso_2020
# plotting coefficient lasso 2020
lasso_2020_coef_df <- as.matrix(coeff_lasso_2020)|> 
  as.data.frame() |> 
  rownames_to_column(var = "beta_names") |> 
  rename(beta_values = "lambda.1se") |> 
  mutate(beta_names = str_remove_all(string = beta_names, 
                                     pattern = "^general_sector"), 
         beta_names = str_remove_all(string = beta_names, 
                                     pattern = "^tot(ale)?_"), 
         beta_names = str_remove_all(string = beta_names, 
                                     pattern = "[_]\\w{4}[_]\\w{3}[_]\\d+$"), 
         beta_names = as.factor(beta_names)) |> 
  filter(beta_values != 0) |> 
  filter(beta_names != "(Intercept)") |> 
  mutate(beta_names = fct_reorder(.f = beta_names, 
                                   .x = beta_values, .desc = TRUE)) |> 
  mutate(beta_sign = ifelse(beta_values > 0, "positive", "negative"))

ggplot(lasso_2020_coef_df,
       aes(x = beta_names, y = beta_values, fill = beta_sign)) + 
  geom_col(width = 0.3) + 
  coord_flip() +
  geom_text(aes(x = beta_names, y = 0, label = beta_names),
            inherit.aes = FALSE, 
            hjust = ifelse(
              lasso_2020_coef_df$beta_sign == "positive", 1.3, -0.5), size = 2) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), name = "Coeff. sign") +
  scale_y_continuous(breaks = seq(-0.40, 0.30, 0.05), limits = c(-0.40, 0.30)) + 
  labs(x = "", y = "Values") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y =  element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
        legend.position = "bottom", 
        legend.title = element_text(size = 8),
        legend.key.size = unit(3, "mm"),
        axis.title.x = element_text(vjust = -2, size = 8))

ggsave(filename = "lasso_coefficient_2020.pdf", 
       plot = last_plot(), 
       path = "results/logistic-lasso/", 
       height = 85, 
       width = 130,
       units = "mm")

# computing fitted values
hat_pi_2020 <- glmnet:::predict.cv.glmnet(
  fit_lasso_2020, newx = x_2020, s = "lambda.1se", type = "response"
)

summary_lasso_2020 <- summary(hat_pi_2020)

# roc curve
rc_lasso_2020 <- roc(response = final_dataset_2020$defaults,
                      predictor = as.numeric(hat_pi_2020))

ggroc(rc_lasso_2020, legacy.axes = TRUE) + 
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = "dashed") + 
  annotate("point", x = c(0, 1), y = c(0, 1), size = 1) + 
  labs(x = "1 - Specificity", y = "Sensitivity", 
       title = "ROC Curve 2020") + 
  theme_base() + 
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8))

ggsave(filename = "lasso_roc_curve_2020.pdf",
       plot = last_plot(),
       path = "results/logistic-lasso/", 
       height = 90, 
       width = 120, 
       units = "mm")

# area under the curve
auc(rc_lasso_2020)

# confusion matrix lasso 2020
tau_lasso_2020 <- mean(hat_pi_2020)
label_lasso_2020 <- ifelse(hat_pi_2020 > tau_lasso_2020, 1, 0)
lasso_2020_confusion_matrix <- table(final_dataset_2020$defaults, 
                                     label_lasso_2020)

# computing sensitivity, specficity and hit rate
sensitivity_lasso_2020 <- lasso_2020_confusion_matrix[2, 2] / sum(
  lasso_2020_confusion_matrix[2, ])

specificity_lasso_2020 <- lasso_2020_confusion_matrix[1, 1] / sum(
  lasso_2020_confusion_matrix[1, ])

hit_rate_lasso_2020 <- sum(
  lasso_2020_confusion_matrix[1, 1], lasso_2020_confusion_matrix[2, 2]) / sum(
    lasso_2020_confusion_matrix[1, ], lasso_2020_confusion_matrix[2, ])


rownames(lasso_2020_confusion_matrix) <- c("$Y = 0$", "$Y = 1$")

#---- applying Logistic Regression with Lasso on 2023 data -----

####### set.seed for cross-validation ############
set.seed(1010)

x_2023 <- cbind(matrix_num_2023, dummy_sector_2023)

fit_lasso_2023 <- cv.glmnet(y = final_dataset_2023$defaults,
                            x = x_2023, family = "binomial", 
                            alpha = 1, nfolds = 10, type.measure = "deviance")

# cross-validation plot 
plot(fit_lasso_2023)
mtext("Cross-Validation Plot 2023 K = 10", side = 3, 
      line = 2.8, cex = 1.2, font = 2)

coeff_lasso_2023 <- coef(fit_lasso_2023)
coeff_lasso_2023

# computing fitted values
hat_pi_2023 <- glmnet:::predict.cv.glmnet(
  fit_lasso_2023, newx = x_2023, s = "lambda.1se", type = "response"
)

summary_lasso_2023 <- summary(hat_pi_2023)

# roc curve
rc_lasso_2023 <- roc(response = final_dataset_2023$defaults,
                     predictor = as.numeric(hat_pi_2023))

ggroc(rc_lasso_2023, legacy.axes = TRUE) + 
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = "dashed") + 
  annotate("point", x = c(0, 1), y = c(0, 1), size = 1) + 
  labs(x = "1 - Specificity", y = "Sensitivity", 
       title = "ROC Curve 2023") + 
  theme_base() + 
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8))

ggsave(filename = "lasso_roc_curve_2023.pdf",
       plot = last_plot(),
       path = "results/logistic-lasso/", 
       height = 100, 
       width = 125, 
       units = "mm")

# area under the curve
auc(rc_lasso_2023)

######   TEST  ########
y_2023 <- final_dataset_2023$defaults

test_2023 <- glm(y_2023 ~ x_2023 - 1, family = "binomial")
summary(test_2023)

# boolean search 
data_2023 <- cbind(x_2023, y_2023) |> 
  as_tibble()

# examining real-estate
sum(data_2023$general_sectorimmobiliare)  
any(data_2023$general_sectorimmobiliare & data_2023$y_2023 == 0)
sum(data_2023$general_sectorimmobiliare & data_2023$y_2023 == 0)
any(data_2023$general_sectorimmobiliare & data_2023$y_2023 == 1)

# examining energy 
sum(data_2023$general_sectorenergia)
any(data_2023$general_sectorenergia & data_2023$y_2023 == 1)
any(data_2023$general_sectorenergia & data_2023$y_2023 == 0)
sum(data_2023$general_sectorenergia & data_2023$y_2023 == 0)

# examining mining 
sum(data_2023$general_sectorestrazione)
any(data_2023$general_sectorestrazione & data_2023$y_2023 == 1)
any(data_2023$general_sectorestrazione & data_2023$y_2023 == 0)
sum(data_2023$general_sectorestrazione & data_2023$y_2023 == 0)


boo_search_data_frame <- data.frame(
  sectors = c("immobiliare", "energia", "estrazioni"), 
  n_companies = c(1812, 2752, 357), 
  Y_1 = c(0, 0, 0), 
  Y_0 = c(1812, 2752, 357))

kable(boo_search_data_frame, format = "simple")

tbl_solvent_sector <- kable(
  boo_search_data_frame, 
  format = "latex",
  col.names = c("Sectors", "N. Companies", "Y = 1",  "Y = 0"),
  booktabs = TRUE,
  linesep = "",
  caption = "Settori privi di insolvenze anno 2023.",
  align = c("l", "c", "c"),
  label = "table:sector2023zerodefault",
  position = "H")

cat(tbl_solvent_sector, 
    file = "results/logistic-lasso/zero_default_industries_2023.tex")

# removing energy, mining, real-estate rows
theoric_dataset_2023 <- data_2023 |> 
  filter((general_sectorenergia == 0 & general_sectorestrazione == 0 & 
             general_sectorimmobiliare == 0)) |> 
  dplyr::select(- c(general_sectorenergia, general_sectorestrazione, 
                    general_sectorimmobiliare))

x_theoric_2023 <- theoric_dataset_2023 |> 
  dplyr::select(- y_2023) |> 
  as.matrix()

theoric_lasso_2023 <- cv.glmnet(y = theoric_dataset_2023$y_2023, 
                                x = x_theoric_2023, family = "binomial", 
                                alpha = 1, nfolds = 3,
                                type.measure = "deviance")

y_2023_clean <- theoric_dataset_2023$y_2023

glm_2023_clean <- glm(y_2023_clean ~ x_theoric_2023 - 1, family = "binomial")
summary(glm_2023_clean)

#---- logistic regression with lasso on sampled data 2023 ----
set.seed(NULL)

default_clean_2023 <- theoric_dataset_2023 |> 
  filter(y_2023 == 1)

solv_clean_2023 <- theoric_dataset_2023 |> 
  filter(y_2023 == 0)

sample_solv_clean_2023 <- solv_clean_2023 |> 
  slice_sample(prop = 0.05, replace = FALSE)


sampled_data_2023 <- bind_rows(default_clean_2023, 
                               sample_solv_clean_2023)

y_2023_sample <- sampled_data_2023$y_2023

x_sample_2023 <- sampled_data_2023 |> 
  dplyr::select(-y_2023)

x_sample_2023 <- as.matrix(x_sample_2023)

lasso_sampled_2023 <- cv.glmnet(y = y_2023_sample, x = x_sample_2023, 
                                     family = "binomial", alpha = 1, 
                                     nfolds = 10, type.measure = "deviance", 
                                     standardize = FALSE)

plot(lasso_sampled_2023)
mtext("Cross-Validation Plot Sample 2023 K = 10", side = 3, 
      line = 2.8, cex = 1.2, font = 2)

default_clean_2023 |> 
  dplyr::select(starts_with("general")) |> 
  colSums()

# extracting coeff from sampled lasso 2023 K = 10
coeff_sampled_lasso_2023 <- coef(lasso_sampled_2023)
coeff_sampled_lasso_2023

lasso_sampled_2023_coeff <- as.data.frame.matrix(coeff_sampled_lasso_2023) |> 
  rownames_to_column(var = "beta_names") |> 
  rename(beta_values = "lambda.1se") |> 
  mutate(beta_names = str_remove_all(string = beta_names, 
                                     pattern = "[_]\\w{4}[_]\\w{3}[_]\\d+$"), 
         beta_names = str_remove_all(string = beta_names, 
                                     pattern = "^general_sector"), 
         beta_names = str_remove_all(string = beta_names, 
                                     pattern = "^tot(ale)?_"), 
         beta_names = as.factor(beta_names)) |> 
  filter(beta_values != 0) |> 
  filter(beta_names != "(Intercept)") |> 
  mutate(beta_names = fct_reorder(beta_names, beta_values, .desc = TRUE)) |> 
  mutate(beta_sign = ifelse(beta_values > 0, "positive", "negative"))
  
ggplot(lasso_sampled_2023_coeff,
       aes(x = beta_names, beta_values, fill = beta_sign)) + 
  geom_col(width = 0.3) + 
  coord_flip() + 
  geom_text(aes(x = beta_names, y = 0, label = beta_names), 
            hjust = ifelse(
              lasso_sampled_2023_coeff$beta_sign == "positive", 1.3, -0.5), 
            size = 2) + 
  scale_y_continuous(breaks = seq(-0.40, 0.35, 0.05), limits = c(-0.40, 0.35)) + 
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), name = "Coeff. Sign") + 
  labs(x = "", y = "Values") + 
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 8), 
        legend.key.size = unit(3, "mm"), 
        axis.title.x = element_text(vjust = -2, size = 8), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())

ggsave(filename = "lasso_sample_2023_coefficient.pdf", 
       plot = last_plot(), 
       path = "results/logistic-lasso/", 
       height = 85, 
       width = 130,
       units = "mm")

hat_pi_sampled_2023 <- glmnet:::predict.cv.glmnet(
  lasso_sampled_2023, newx = x_sample_2023, 
  s = "lambda.1se", type = "response")

summary_lasso_sample_2023 <- summary(hat_pi_sampled_2023)
summary_lasso_sample_2023

# roc curve sampled lasso 2023 
rc_sampled_lasso_2023 <- roc(response = y_2023_sample, 
                             predictor = as.numeric(hat_pi_sampled_2023))

ggroc(rc_sampled_lasso_2023, legacy.axes = TRUE) + 
  annotate("segment", x = 0, xend = 1, y = 0, yend = 1, linetype = "dashed") + 
  annotate("point", x = c(0, 1), y = c(0, 1), size = 2) + 
  labs(x = "1 - Specificity", y = "Sensitivity", 
       title = "ROC Curve Sample 2023") + 
  theme_base() + 
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8))

ggsave(filename = "lasso_roc_curve_sample_2023.pdf",
       plot = last_plot(), 
       path = "results/logistic-lasso/", 
       height = 100, 
       width = 125, 
       units = "mm")

# auc 
auc(rc_sampled_lasso_2023)

# confusion matrix lasso sample 2023 
tau_lasso_sample_2023 <- mean(hat_pi_sampled_2023)
label_lasso_sample_2023 <- ifelse(
  hat_pi_sampled_2023 > tau_lasso_sample_2023, 1, 0)

lasso_sample_2023_confusion_matrix <- table(y_2023_sample, 
                                            label_lasso_sample_2023)

# computing sensitivity, specficity and hit rate
sensitivity_lasso_sample_2023 <- lasso_sample_2023_confusion_matrix[2, 2] / sum(
  lasso_sample_2023_confusion_matrix[2, ])

specificity_lasso_sample_2023 <- lasso_sample_2023_confusion_matrix[1, 1] / sum(
  lasso_sample_2023_confusion_matrix[1, ])

hit_rate_lasso_sample_2023 <- sum(
  lasso_sample_2023_confusion_matrix[1, 1], 
  lasso_sample_2023_confusion_matrix[2, 2]) / sum(
    lasso_sample_2023_confusion_matrix[1, ], 
    lasso_sample_2023_confusion_matrix[2, ])

rownames(lasso_sammple_2023_confusion_matrix) <- c("$Y = 0$", "$Y = 1$")

#----- DISCRIMINANT ANALYSIS -----
set.seed(NULL)

#---- applying LDA to 2016 ----
lda_fit_2016 <- lda(x = matrix_num_2016, 
                    grouping = final_dataset_2016$defaults,
                    method = "moment")

lda_2016_coeff <- lda_fit_2016$scaling |> 
  as.data.frame.matrix() |> 
  rownames_to_column(var = "financial_variables") |> 
  rename(lda_coefficients = "LD1") |> 
  mutate(financial_variables = str_remove_all(
    string = financial_variables, 
    pattern = "[_]\\w{4}[_]\\w{3}[_]\\d+$"),
    financial_variables = str_remove_all(string = financial_variables, 
                                         pattern = "^tot(ale)?_"),
    financial_variables = str_replace_all(
      string = financial_variables,
      pattern = "ricavi_vendite_e_prestazioni",
      replacement = "ricavi"), 
    financial_variables = str_replace_all(
        string = financial_variables, 
        pattern = "_di_|_vs_|_ni_|_per_|_del_",
        replacement = "_"), 
    financial_variables = as.factor(financial_variables)) |> 
  mutate(lda_sign = ifelse(lda_coefficients > 0, "positive", "negative")) |> 
  mutate(
    financial_variables = fct_reorder(
      .f = financial_variables,
      .x = lda_coefficients, .desc = TRUE))

ggplot(lda_2016_coeff, 
       aes(financial_variables, lda_coefficients, fill = lda_sign)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(x = financial_variables, 
                y = 0, label = financial_variables, 
                hjust = ifelse(lda_sign == "positive", 1.1, -0.04)), size = 2) +
  scale_y_continuous(breaks = seq(-0.85, 0.50, 0.10), limits = c(-0.85, 0.50)) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), name = "Coeff. Sign") +
  labs(x = "", y = "Values") +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 8), 
        legend.key.size = unit(3, "mm"), 
        axis.title.x = element_text(vjust = -2, size = 8), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave(filename = "lda_2016_coefficient.pdf", 
       plot = last_plot(), 
       path = "results/lda/", 
       height = 85, 
       width = 130,
       units = "mm")

# predicting prob
hat_lda_2016 <- MASS:::predict.lda(
  lda_fit_2016, newdata = matrix_num_2016
)

# extracting posterior 2016
summary_lda_2016 <- summary(hat_lda_2016$posterior)
df_summary_lda_2016 <- as.data.frame.matrix(summary_lda_2016) |> 
  remove_rownames() |>
  rename_with(.fn = ~ c("posterior_0","posterior_1"), 
              .cols = 1:2) |> 
  mutate(prob_progress = str_extract(string = posterior_0, 
                                    pattern = "^(\\S+)\\s*(\\S*)[:]"), 
         posterior_0 = str_remove_all(string = posterior_0, 
                                      pattern = "^(\\S+)\\s*(\\S*)[:]"), 
         posterior_1 = str_remove_all(string = posterior_1, 
                                  pattern = "^(\\S+)\\s*(\\S*)[:]")) |> 
  dplyr::select(prob_progress, posterior_0, posterior_1)

# plotting ROC CURVE LDA 2016
rc_lda_2016 <- roc(response = final_dataset_2016$defaults, 
                   predictor = hat_lda_2016$posterior[, 2])

ggroc(rc_lda_2016, legacy.axes = TRUE) + 
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = "dashed") + 
  annotate(geom = "point", x = c(0, 1), y = c(0, 1), size = 1) +
  labs(x = "1 - Specificity", y = "Sensitivity", 
       title = "ROC Curve LDA 2016") + 
  theme_base() + 
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8))

ggsave(filename = "lda_roc_curve_2016.pdf",
       plot = last_plot(),
       path = "results/lda/", 
       height = 100, 
       width = 125, 
       units = "mm")

# auc lda 2016
auc(rc_lda_2016)

# confusion matrix lda 2016
tau_lda_2016 <- 0.02291
lda_label_2016 <- ifelse(hat_lda_2016$posterior[, 2] > tau_lda_2016, 1, 0)

confusion_lda_2016 <- table(final_dataset_2016$defaults, 
                            lda_label_2016)

# computing sensitivity, specficity and hit rate
lda_specificity_2016 <- confusion_lda_2016[1, 1] / sum(confusion_lda_2016[1,])
lda_sensitivity_2016 <- confusion_lda_2016[2, 2] / sum(confusion_lda_2016[2, ])
lda_hit_rate_2016 <- sum(
  confusion_lda_2016[1, 1], confusion_lda_2016[2, 2]) / sum(
    confusion_lda_2016[1, ], confusion_lda_2016[2, ])

rownames(confusion_lda_2016) <- c("$Y = 0$", "$Y = 1$")

#---- applying LDA to 2020 ----
lda_fit_2020 <- lda(x = matrix_num_2020, 
                    grouping = final_dataset_2020$defaults, 
                    method = "moment")

lda_2020_coeff <- lda_fit_2020$scaling |> 
  as.data.frame.matrix() |> 
  rownames_to_column(var = "financial_variables") |> 
  rename(lda_coefficients = "LD1") |> 
  mutate(financial_variables = str_remove_all(
    string = financial_variables, 
    pattern = "[_]\\w{4}[_]\\w{3}[_]\\d+$"),
    financial_variables = str_remove_all(
      string = financial_variables, 
      pattern = "^tot(ale)?_"),
    financial_variables = str_replace_all(
      string = financial_variables,
      pattern = "ricavi_vendite_e_prestazioni",
      replacement = "ricavi"), 
    financial_variables = str_replace_all(
      string = financial_variables, 
      pattern = "_di_|_vs_|_ni_|_per_|_del_",
      replacement = "_"), 
    financial_variables = as.factor(financial_variables)) |> 
  mutate(lda_sign = ifelse(lda_coefficients > 0, "positive", "negative")) |> 
  mutate(financial_variables = fct_reorder(
      .f = financial_variables,
      .x = lda_coefficients, .desc = TRUE))

ggplot(lda_2020_coeff, 
       aes(financial_variables, lda_coefficients, fill = lda_sign)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(x = financial_variables, 
                y = 0, label = financial_variables, 
                hjust = ifelse(lda_sign == "positive", 1.1, -0.04)), size = 2) +
  scale_y_continuous(breaks = seq(-0.95, 0.60, 0.10), limits = c(-0.95, 0.60)) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), name = "Coeff. Sign") +
  labs(x = "", y = "Values") +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 8), 
        legend.key.size = unit(3, "mm"), 
        axis.title.x = element_text(vjust = -2, size = 8), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave(filename = "lda_2020_coefficient.pdf", 
       plot = last_plot(), 
       path = "results/lda/", 
       height = 85, 
       width = 130,
       units = "mm")

hat_lda_2020 <- MASS:::predict.lda(
  lda_fit_2020, newdata = matrix_num_2020
)


# extracting posterior 2020
summary_lda_2020 <- summary(hat_lda_2020$posterior)
df_summary_lda_2020 <- as.data.frame.matrix(summary_lda_2020) |> 
  remove_rownames() |> 
  rename_with(.fn = ~ c("posterior_0", "posterior_1"), 
              .cols = 1:2) |> 
  mutate(prob_progress = str_extract(string = posterior_0, 
                                     pattern = "^(\\S+)\\s*(\\S*)[:]"), 
         posterior_0 = str_remove_all(string = posterior_0, 
                                      pattern = "^(\\S+)\\s*(\\S*)[:]"), 
         posterior_1 = str_remove_all(string = posterior_1, 
                                      pattern = "^(\\S+)\\s*(\\S*)[:]")) |> 
  dplyr::select(prob_progress, posterior_0, posterior_1)

# plotting ROC CURVE LDA 2020
rc_lda_2020 <- roc(response = final_dataset_2020$defaults, 
                   predictor = hat_lda_2020$posterior[, 2])

ggroc(rc_lda_2020, legacy.axes = TRUE) + 
  annotate(geom = "segment", x = 0, y = 0,
           xend = 1, yend = 1, linetype = "dashed") + 
  annotate(geom = "point", x = c(0, 1), y = c(0, 1), size = 1) + 
  labs(x = "1 - Specificity", y = "Sensitivity", 
       title = "ROC Curve LDA 2020") + 
  theme_base() + 
  theme(panel.background = element_blank()) +
theme(plot.title = element_text(size = 13), 
      axis.title.x = element_text(size = 10), 
      axis.title.y = element_text(size = 10), 
      axis.text.x = element_text(size = 8), 
      axis.text.y = element_text(size = 8))

ggsave(filename = "lda_roc_curve_2020.pdf",
       plot = last_plot(),
       path = "results/lda/", 
       height = 107, 
       width = 125, 
       units = "mm")

# auc lda 2020
auc(rc_lda_2020)

# confusion matrix 2020
tau_lda_2020 <- 0.0048137
lda_label_2020 <- ifelse(hat_lda_2020$posterior[, 2] > tau_lda_2020, 1, 0)
confusion_lda_2020 <- table(final_dataset_2020$defaults, 
                            lda_label_2020)

# computing sensitivity, specficity and hit rate
lda_sensitivity_2020 <- confusion_lda_2020[2, 2] / sum(confusion_lda_2020[2, ])
lda_specificity_2020 <- confusion_lda_2020[1, 1] / sum(confusion_lda_2020[1, ])
lda_hit_rate_2020 <- sum(
  confusion_lda_2020[1, 1], confusion_lda_2020[2, 2]) / sum(
    confusion_lda_2020[1, ], confusion_lda_2020[2, ])

rownames(confusion_lda_2020) <-  c("$Y = 0$", "$Y = 1$")

#---- applying LDA to 2023 ----
lda_fit_2023 <- lda(x = matrix_num_2023,
                    grouping = final_dataset_2023$defaults, 
                    method = "moment")

lda_2023_coeff <- lda_fit_2023$scaling |> 
  as.data.frame.matrix() |> 
  rownames_to_column(var = "financial_variables") |> 
  rename(lda_coefficients = "LD1") |> 
  mutate(financial_variables = str_remove_all(
    string = financial_variables, 
    pattern = "[_]\\w{4}[_]\\w{3}[_]\\d+$"),
    financial_variables = str_remove_all(
      string = financial_variables, 
      pattern = "^tot(ale)?_"),
    financial_variables = str_replace_all(
      string = financial_variables,
      pattern = "ricavi_vendite_e_prestazioni",
      replacement = "ricavi"), 
    financial_variables = str_replace_all(
      string = financial_variables, 
      pattern = "_di_|_vs_|_ni_|_per_|_del_",
      replacement = "_"), 
    financial_variables = as.factor(financial_variables)) |> 
  mutate(lda_sign = ifelse(lda_coefficients > 0, "positive", "negative")) |> 
  mutate(financial_variables = fct_reorder(
    .f = financial_variables,
    .x = lda_coefficients, .desc = TRUE))

ggplot(lda_2023_coeff, 
       aes(financial_variables, lda_coefficients, fill = lda_sign)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(x = financial_variables, 
                y = 0, label = financial_variables, 
                hjust = ifelse(lda_sign == "positive", 1.1, -0.04)), size = 2) +
  scale_y_continuous(breaks = seq(-0.75, 0.50, 0.05), limits = c(-0.75, 0.50)) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), name = "Coeff. Sign") +
  labs(x = "", y = "Values") +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 8), 
        legend.key.size = unit(3, "mm"), 
        axis.title.x = element_text(vjust = -2, size = 8), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave(filename = "lda_2023_coefficient.pdf", 
       plot = last_plot(), 
       path = "results/lda/", 
       height = 85, 
       width = 130,
       units = "mm")


hat_lda_2023 <- MASS:::predict.lda(
  lda_fit_2023, newdata = matrix_num_2023
)

# extracting posterior prob 2023
summary_lda_2023 <- summary(hat_lda_2023$posterior)
df_summary_lda_2023 <- as.data.frame.matrix(summary_lda_2023) |> 
  remove_rownames() |> 
  rename_with(.fn = ~ c("posterior_0", "posterior_1"), 
              .cols = 1:2)|> 
  mutate(prob_progress = str_extract(string = posterior_0, 
                                     pattern = "^(\\S+)\\s*(\\S*)[:]"), 
         posterior_0 = str_remove_all(string = posterior_0, 
                                      pattern = "^(\\S+)\\s*(\\S*)[:]"), 
         posterior_1 = str_remove_all(string = posterior_1, 
                                      pattern = "^(\\S+)\\s*(\\S*)[:]")) |> 
  dplyr::select(prob_progress, posterior_0, posterior_1)

# plotting ROC CURVE LDA 2023
rc_lda_2023 <- roc(response = final_dataset_2023$defaults,
                   predictor = hat_lda_2023$posterior[, 2])

ggroc(rc_lda_2023, legacy.axes = TRUE) + 
  annotate(geom = "segment", x = 0, y = 0,
           xend = 1, yend = 1, linetype = "dashed") + 
  annotate(geom = "point", x = c(0, 1), y = c(0, 1), size = 1) + 
  labs(x = "1 - Specificity", y = "Sensitivity", 
       title = "ROC Curve LDA 2023") + 
  theme_base() + 
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8))

ggsave(filename = "lda_roc_curve_2023.pdf",
       plot = last_plot(),
       path = "results/lda/", 
       height = 100, 
       width = 125, 
       units = "mm")

auc(rc_lda_2023)

# confusion matrix lda 2023
tau_lda_2023 <-  0.00028379
lda_label_2023 <- ifelse(hat_lda_2023$posterior[, 2] > tau_lda_2023, 1, 0)

confusion_lda_2023 <- table(final_dataset_2023$defaults, 
                            lda_label_2023)

# computing sensitivity, specificity and hit rate
lda_sensitivity_2023 <- confusion_lda_2023[2, 2] / sum(confusion_lda_2023[2, ])
lda_specificity_2023 <- confusion_lda_2023[1, 1] / sum(confusion_lda_2023[1, ])
lda_hit_rate_2023 <- sum(
  confusion_lda_2023[1, 1], confusion_lda_2023[2, 2]) / sum(
    confusion_lda_2023[1, ], confusion_lda_2023[2, ])

rownames(confusion_lda_2023) <-  c("$Y = 0$", "$Y = 1$")

#---- computing lasso & lda coefficient correlations -----

# 2016
lasso_coef_2016 <- coeff_lasso_2016 |> 
  as.data.frame.matrix() |> 
  rownames_to_column(var = "beta_names") |> 
  rename(beta_values = "lambda.1se") |> 
  filter(beta_names != "(Intercept)") |> 
  filter(!str_detect(string = beta_names,
                     pattern = "^general_")) |> 
  dplyr::select(beta_values)
  

cor.test(lasso_coef_2016$beta_values, lda_fit_2016$scaling, 
         method = "pearson", alternative = "two.sided", 
         conf.level = 0.95)

# 2020
lasso_coef_2020 <- coeff_lasso_2020 |> 
  as.data.frame.matrix() |> 
  rownames_to_column(var = "beta_names") |> 
  rename(beta_values = "lambda.1se") |> 
  filter(beta_names != "(Intercept)") |> 
  filter(!str_detect(string = beta_names, 
                     pattern = "^general_")) |> 
  dplyr::select(beta_values)

cor.test(lasso_coef_2020$beta_values, 
         lda_fit_2020$scaling, method = "pearson", 
         alternative = "two.sided", 
         conf.level = 0.95)

# 2023
lasso_coef_2023_sample <- coeff_sampled_lasso_2023 |> 
  as.data.frame.matrix() |> 
  rownames_to_column(var = "beta_names") |> 
  rename(beta_values = "lambda.1se") |> 
  filter(beta_names != "(Intercept)") |> 
  filter(!str_detect(string = beta_names,
                     pattern = "^general_")) |> 
  dplyr::select(beta_values)


cor.test(lasso_coef_2023_sample$beta_values, 
         lda_fit_2023$scaling, method = "pearson", 
         alternative = "two.sided", 
         conf.level = 0.95)

#---- applying QDA to 2016 ----
qda_fit_2016 <- qda(x =  matrix_num_2016,
                    grouping = final_dataset_2016$defaults,
                    method = "moment")

hat_qda_2016 <- MASS:::predict.qda(
  qda_fit_2016, newdata = matrix_num_2016
)

summary_qda_2016 <- summary(hat_qda_2016$posterior)
df_summary_qda_2016 <- as.data.frame.matrix(summary_qda_2016) |> 
  remove_rownames() |>
  rename_with(.fn = ~ c("posterior_0","posterior_1"), 
              .cols = 1:2) |> 
  mutate(prob_progress = str_extract(string = posterior_0, 
                                     pattern = "^(\\S+)\\s*(\\S*)[:]"), 
         posterior_0 = str_remove_all(string = posterior_0, 
                                      pattern = "^(\\S+)\\s*(\\S*)[:]"), 
         posterior_1 = str_remove_all(string = posterior_1, 
                                      pattern = "^(\\S+)\\s*(\\S*)[:]")) |> 
  dplyr::select(prob_progress, posterior_0, posterior_1)

# roc curve qda 2016
rc_qda_2016 <- roc(response = final_dataset_2016$defaults, 
                   predictor = hat_qda_2016$posterior[, 2])

ggroc(rc_qda_2016, legacy.axes = TRUE) + 
  annotate(geom = "segment", x = 0, y = 0,
           xend = 1, yend = 1, linetype = "dashed") + 
  annotate(geom = "point", x = c(0, 1), y = c(0, 1), size = 1) + 
  labs(x = "1 - Specificity", y = "Sensitivity", 
       title = "ROC Curve QDA 2016") + 
  theme_base() + 
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8))

ggsave(filename = "qda_roc_curve_2016.pdf",
       plot = last_plot(),
       path = "results/qda/", 
       height = 100, 
       width = 125, 
       units = "mm")

auc(rc_qda_2016)

# confusion matrix qda 2016
tau_qda_2016 <- 0.053719
qda_label_2016 <- ifelse(hat_qda_2016$posterior[, 2] > tau_qda_2016, 1, 0)

qda_confusion_2016 <- table(final_dataset_2016$defaults, 
                            qda_label_2016)

# computing sensitivity, specficity and hit rate
qda_specificity_2016 <- qda_confusion_2016[1, 1] / sum(qda_confusion_2016[1, ])
qda_sensitivity_2016 <- qda_confusion_2016[2, 2] / sum(qda_confusion_2016[2, ])
qda_hit_rate_2016 <- sum(
  qda_confusion_2016[1, 1], qda_confusion_2016[2, 2]) / sum(
    qda_confusion_2016[1, ], qda_confusion_2016[2, ])


rownames(qda_confusion_2016) <- c("$Y = 0$", "$Y = 1$")

#---- applying QDA to 2020 ----
qda_fit_2020 <- qda(x =  matrix_num_2020, 
                    grouping = final_dataset_2020$defaults,
                    method = "moment")

hat_qda_2020 <- MASS:::predict.qda(
  qda_fit_2020, newdata = matrix_num_2020
)

summary_qda_2020 <- summary(hat_qda_2020$posterior)
df_summary_qda_2020 <- as.data.frame.matrix(summary_qda_2020) |> 
  remove_rownames() |>
  rename_with(.fn = ~ c("posterior_0","posterior_1"), 
              .cols = 1:2) |> 
  mutate(prob_progress = str_extract(string = posterior_0, 
                                     pattern = "^(\\S+)\\s*(\\S*)[:]"), 
         posterior_0 = str_remove_all(string = posterior_0, 
                                      pattern = "^(\\S+)\\s*(\\S*)[:]"), 
         posterior_1 = str_remove_all(string = posterior_1, 
                                      pattern = "^(\\S+)\\s*(\\S*)[:]")) |> 
  dplyr::select(prob_progress, posterior_0, posterior_1)


# roc curve qda 2020
rc_qda_2020 <- roc(response = final_dataset_2020$defaults, 
                   predictor = hat_qda_2020$posterior[, 2])

ggroc(rc_qda_2020, legacy.axes = TRUE) + 
  annotate(geom = "segment", x = 0, y = 0,
           xend = 1, yend = 1, linetype = "dashed") + 
  annotate(geom = "point", x = c(0, 1), y = c(0, 1), size = 1) + 
  labs(x = "1 - Specificity", y = "Sensitivity", 
       title = "ROC Curve QDA 2020") + 
  theme_base() + 
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8))

ggsave(filename = "qda_roc_curve_2020.pdf",
       plot = last_plot(),
       path = "results/qda/", 
       height = 100, 
       width = 125, 
       units = "mm")

auc(rc_qda_2020)

# confusion matrix qda 2020
tau_qda_2020 <- 0.002835
qda_label_2020 <- ifelse(hat_qda_2020$posterior[, 2] > tau_qda_2020, 1, 0)

qda_confusion_2020 <- table(final_dataset_2020$defaults,
                            qda_label_2020)

qda_specificity_2020 <- qda_confusion_2020[1, 1] / sum(qda_confusion_2020[1, ])
qda_sensitivity_2020 <- qda_confusion_2020[2, 2] / sum(qda_confusion_2020[2, ])
qda_hit_rate_2020 <- sum(
  qda_confusion_2020[1, 1], qda_confusion_2020[2, 2]) / sum(
    qda_confusion_2020[1, ], qda_confusion_2020[2, ])

rownames(qda_confusion_2020) <- c("$Y = 0$", "$Y = 1$")

#---- applying QDA to 2023 ----
qda_fit_2023 <- qda(x =  matrix_num_2023, 
                    grouping = final_dataset_2023$defaults,
                    method = "moment")

hat_qda_2023 <- MASS:::predict.qda(
  qda_fit_2023, newdata = matrix_num_2023
)

summary_qda_2023 <- summary(hat_qda_2023$posterior)
df_summary_qda_2023 <- as.data.frame.matrix(summary_qda_2023) |> 
  remove_rownames() |>
  rename_with(.fn = ~ c("posterior_0","posterior_1"), 
              .cols = 1:2) |> 
  mutate(prob_progress = str_extract(string = posterior_0, 
                                     pattern = "^(\\S+)\\s*(\\S*)[:]"), 
         posterior_0 = str_remove_all(string = posterior_0, 
                                      pattern = "^(\\S+)\\s*(\\S*)[:]"), 
         posterior_1 = str_remove_all(string = posterior_1, 
                                      pattern = "^(\\S+)\\s*(\\S*)[:]")) |> 
  dplyr::select(prob_progress, posterior_0, posterior_1)

# roc curve qda 2023
rc_qda_2023 <- roc(response = final_dataset_2023$defaults, 
                   predictor = hat_qda_2023$posterior[, 2])

ggroc(rc_qda_2023, legacy.axes = TRUE) + 
  annotate(geom = "segment", x = 0, y = 0,
           xend = 1, yend = 1, linetype = "dashed") + 
  annotate(geom = "point", x = c(0, 1), y = c(0, 1), size = 1) + 
  labs(x = "1 - Specificity", y = "Sensitivity", 
       title = "ROC Curve QDA 2023") + 
  theme_base() + 
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8))

ggsave(filename = "qda_roc_curve_2023.pdf",
       plot = last_plot(),
       path = "results/qda/", 
       height = 100, 
       width = 125, 
       units = "mm")

auc(rc_qda_2023)

# confusion matrix qda 2023
tau_qda_2023 <- 0.00037195
qda_label_2023 <- ifelse(hat_qda_2023$posterior[, 2] > tau_qda_2023, 1, 0)
qda_confusion_2023 <- table(final_dataset_2023$defaults, 
                            qda_label_2023)

qda_specificity_2023 <- qda_confusion_2023[1, 1] / sum(qda_confusion_2023[1, ])
qda_sensitivity_2023 <- qda_confusion_2023[2, 2] / sum(qda_confusion_2023[2, ])
qda_hit_rate_2023 <- sum(
  qda_confusion_2023[1, 1], qda_confusion_2023[2, 2]) / sum(
    qda_confusion_2023[1, ], qda_confusion_2023[2, ])

rownames(qda_confusion_2023) <- c("$Y = 0$", "$Y = 1$")

#----- applying classification tree 2016 -----
set.seed(1010)
x_tr_2016 <- as_tibble(matrix_num_2016)
y_tr_2016 <- final_dataset_2016$defaults

data_tree_2016 <- cbind(x_tr_2016, y_tr_2016)

tree_2016 <- rpart(formula = y_tr_2016 ~ .,
                   data =  data_tree_2016,
                   method = "class", 
                   control = rpart.control(cp = 0.00001, xval = 10))

printcp(tree_2016)
plotcp(tree_2016)

plot(tree_2016, 
     #main = "Classification Tree (Full) 2016", 
     uniform = TRUE)

coord_nodes_2016 <- plot(
  tree_2016, #main = "Classification Tree (only defaults leaves) 2016", 
  uniform = TRUE)

levels_y_2016 <- attr(tree_2016, which = "ylevels")
threshold_comp_2016 <- 1

cond_default <- (tree_2016$frame$var == "<leaf>") & 
  (levels_y_2016[tree_2016$frame$yval] == "1") & 
  (tree_2016$frame$n >= threshold_comp_2016)

x_default_2016 <- coord_nodes_2016$x[cond_default]
y_default_2016 <- coord_nodes_2016$y[cond_default]

text(x = x_default_2016, y = y_default_2016, 
     labels = "Default", col = "#D55E00", cex = 0.3)

indx_best_cp_2016 <- which.min(tree_2016$cptable[, "xerror"])

best_cp_2016 <- tree_2016$cptable[indx_best_cp_2016, "CP"]
pruned_2016 <- prune(tree_2016, cp = best_cp_2016)

plot(pruned_2016, uniform = TRUE, 
     main = "Pruned Tree 2016")

tree_prob_2016 <- predict(pruned_2016, type = "prob")

rc_tree_2016 <- roc(response = y_tr_2016, 
                    predictor = tree_prob_2016[, 2])

ggroc(rc_tree_2016, legacy.axes = TRUE) + 
  annotate(geom = "segment", x = 0, y = 0,
           xend = 1, yend = 1, linetype = "dashed") + 
  annotate(geom = "point", x = c(0, 1), y = c(0, 1), size = 1) + 
  labs(x = "1 - Specificity", y = "Sensitivity", 
       title = "ROC Curve Tree 2016") + 
  theme_base() + 
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8))

ggsave(filename = "tree_roc_curve_2016.pdf",
       plot = last_plot(),
       path = "results/trees/", 
       height = 100, 
       width = 125, 
       units = "mm")

auc(rc_tree_2016)

summary(tree_prob_2016)
# confusion 
tau_cart_2016 <- 0.019385
tree_label_2016 <- ifelse(tree_prob_2016[, 2] > tau_cart_2016, 1, 0)
table(y_tr_2016, 
      tree_label_2016)

#----- applying classification tree 2020 -----
set.seed(1010)
x_tr_2020 <- as_tibble(matrix_num_2020)
y_tr_2020 <- final_dataset_2020$defaults

data_tree_2020 <- cbind(x_tr_2020, y_tr_2020)

tree_2020 <- rpart(formula = y_tr_2020 ~ .,
                   data = data_tree_2020, 
                   method = "class", 
                   control = rpart.control(cp = 0.00001, xval = 10))

printcp(tree_2020)
plotcp(tree_2020)
plot(tree_2020)

indx_best_cp_2020 <- which.min(tree_2020$cptable[, "xerror"])
best_cp_2020 <- tree_2020$cptable[indx_best_cp_2020, "CP"]
best_cp_2020
pruned_2020 <- prune(tree = tree_2020, 
                     cp = best_cp_2020)

plot(pruned_2020)

tree_prob_2020 <- predict(pruned_2020, type = "prob")

summary(tree_prob_2020)

rc_tree_2020 <- roc(response = y_tr_2020, 
                    predictor = tree_prob_2020[, 2])

ggroc(rc_tree_2020, legacy.axes = TRUE) + 
  annotate(geom = "segment", x = 0, y = 0,
           xend = 1, yend = 1, linetype = "dashed") + 
  annotate(geom = "point", x = c(0, 1), y = c(0, 1), size = 1) + 
  labs(x = "1 - Specificity", y = "Sensitivity", 
       title = "ROC Curve Tree 2020") + 
  theme_base() + 
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8))

ggsave(filename = "tree_roc_curve_2020.pdf",
       plot = last_plot(),
       path = "results/trees/", 
       height = 100, 
       width = 125, 
       units = "mm")

#----- applying classification tree 2023 -----
set.seed(1010)
x_tr_2023 <- as_tibble(matrix_num_2023)
y_tr_2023 <- final_dataset_2023$defaults

data_tree_2023 <- cbind(x_tr_2023, y_tr_2023)

tree_2023 <- rpart(formula = y_tr_2023 ~ .,
                   data = data_tree_2023, 
                   method = "class", 
                   control = rpart.control(cp = 0.00001, xval = 10))

printcp(tree_2023)
plotcp(tree_2023)
plot(tree_2023)

indx_best_cp_2023 <- which.min(tree_2023$cptable[, "xerror"])
best_cp_2023 <- tree_2023$cptable[indx_best_cp_2023, "CP"]
best_cp_2023
pruned_2023 <- prune(tree = tree_2023, 
                     cp = best_cp_2023)

plot(pruned_2023)

tree_prob_2023 <- predict(pruned_2023, type = "prob")

summary(tree_prob_2023)

rc_tree_2023 <- roc(response = y_tr_2023, 
                    predictor = tree_prob_2023[, 2])

ggroc(rc_tree_2023, legacy.axes = TRUE) + 
  annotate(geom = "segment", x = 0, y = 0,
           xend = 1, yend = 1, linetype = "dashed") + 
  annotate(geom = "point", x = c(0, 1), y = c(0, 1), size = 1) + 
  labs(x = "1 - Specificity", y = "Sensitivity", 
       title = "ROC Curve Tree 2023") + 
  theme_base() + 
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8))

ggsave(filename = "tree_roc_curve_2023.pdf",
       plot = last_plot(),
       path = "results/trees/", 
       height = 100, 
       width = 125, 
       units = "mm")

auc(rc_tree_2023)

summary(tree_prob_2023)
tau_cart_2023 <- 0.0005261
tree_label_2023 <- ifelse(tree_prob_2023[, 2] > tau_cart_2023, 1, 0)

table(y_tr_2023, 
      tree_label_2023)
