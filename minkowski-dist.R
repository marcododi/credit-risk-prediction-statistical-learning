library(ggplot2)
library(ggthemes)
library(ggforce)
library(latex2exp)
library(patchwork)

r <- 0.15
k1 <- 1
k2 <- 2

x <- seq(-r, r, 0.001)

# formula: |y|^k + |x|^k = r^k

#----- computing taxicab distance -----
# only y1 > 0
y1_plus <- (r^k1 - abs(x)^k1)^(1/k1)

y1_neg <- - y1_plus

y1 <- c(y1_plus, y1_neg)


#---- computing euclidean distance -----
y2_plus <- (r^k2 - abs(x)^k2)^(1/k2)

y2_neg <- - y2_plus

y2 <- c(y2_plus, y2_neg)


#---- plotting ---- 
full_df <- data.frame(
  x_coordinate = c(x, x), 
  manhattan_coord = y1,
  euclidean_coord = y2
)

manhattan_dist <- ggplot(full_df, aes(x = x_coordinate, y = manhattan_coord)) +
  geom_point(colour = "#0072B2", size = 0.5) + 
  coord_fixed(xlim = c(-0.15, 0.28), ylim = c(-0.15, 0.46)) +
  geom_ellipse(
    data = data.frame(
      x0 = c(0.10, 0.10), 
      y0 = c(0.30, 0.30), 
      a = c(0.10, 0.197), 
      b = c(0.04, 0.08),
      angle = c(pi/4, pi/4)), 
    aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle), 
    inherit.aes = FALSE, 
    colour = "#CC79A7", 
    fill = NA) +
  annotate("segment", 
           x = -0.25, y = 0, xend = 0.28, yend = 0, 
           arrow = arrow(length = unit(0.10, "inches"), ends = "last")) +
  annotate("segment", x = 0, y = -0.25, xend = 0, yend = 0.46, 
           arrow = arrow(length = unit(0.10, "inches"), ends = "last")) + 
  labs(x = "", y = "") +
  annotate("text", x = 0.25, y = 0, label = TeX("$\\beta_{1}$"), vjust = 2.5) + 
  annotate("text", x = 0, y = 0.4, label = TeX("$\\beta_{2}$"), hjust = 2.5) +
  annotate("text", x = 0.10, y = 0.30, label = TeX("\\hat{\\beta}$")) +
  annotate("point", x = 0.118, y = 0.30, size = 0.8) +
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_blank())

manhattan_dist

euclid_dist <- ggplot(full_df, aes(x = x_coordinate, y = euclidean_coord)) + 
  geom_point(colour = "#0072B2", size = 0.5) +  
  coord_fixed(xlim = c(-0.15, 0.28), ylim = c(-0.15, 0.46)) + 
  geom_ellipse(
    data = data.frame(
      x0 = c(0.15, 0.15), 
      y0 = c(0.30, 0.30), 
      a = c(0.09, 0.197), 
      b = c(0.04, 0.08), 
      angle = c(pi/4, pi/4)), 
    aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle), 
    inherit.aes = FALSE, 
    colour = "#CC79A7",
    fill = NA) +
  annotate("segment", 
           x = -0.25, y = 0, xend = 0.28, yend = 0, 
           arrow = arrow(length = unit(0.10, "inches"), ends = "last")) +
  annotate("segment", x = 0, y = -0.25, xend = 0, yend = 0.46, 
           arrow = arrow(length = unit(0.10, "inches"), ends = "last")) + 
  labs(x = "", y = "") +
  annotate("text", x = 0.20, y = 0, label = TeX("$\\beta_{1}$"),
           vjust = 2.5, hjust = - 0.8) + 
  annotate("text", x = 0, y = 0.4, label = TeX("$\\beta_{2}$"), 
           hjust = 1.5, vjust = -0.2) +
  annotate("text", x = 0.15, y = 0.30, label = TeX("\\hat{\\beta}$")) +
  annotate("point", x = 0.17, y = 0.30, size = 0.8) +
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_blank())

euclid_dist  

ridge_lasso_constraint <- wrap_plots(
  manhattan_dist, euclid_dist, 
  nrow = 1, ncol = 2, byrow = FALSE)

ggsave(filename = "plot_manhattan_euclid_l1_l2_penalties.pdf", 
       plot = ridge_lasso_constraint, 
       path = "graphics/", 
       height = 95, 
       width = 127, 
       units = "mm")



