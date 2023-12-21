###############################################################################
# Code to visualize Figure Supplement 2A                                      #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################


# load the data
Scenario_liste <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_4.csv", header = T)

# Define the degree for each factor
Scenario_liste$half_life_TBV <- as.factor(Scenario_liste$half_life_TBV)
Scenario_liste$Degree_resistance <- as.factor(Scenario_liste$Degree_resistance)
Scenario_liste$Efficacy_TBV <- as.factor(Scenario_liste$Efficacy_TBV)

# Define constant
constant <- 2.5

# Panel A
p <- 
  ggplot(Scenario_liste,
         aes(x = Efficacy_TBV, 
             y = Indicator_10, 
             col = Degree_resistance)) +
  geom_boxplot() +
  labs(title = "", x = "Initial efficacy of TBV (%)", y = "Rate of spread") +
  scale_color_manual(name = "Resistance level",
                     values = c("#de7065ff", "#a65c85ff", "#403891ff", "#0c2a50ff", "black"),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("Senstive (0)",
                                "Low (0.25)",
                                "Medium (0.5)" ,
                                "High (0.75)",
                                "Very high (1)")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5,
                                  face = "bold"),
        legend.text = element_text(size = 18 / constant),
        legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black", 
                                    face = "bold"),
        strip.text.y = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold")) +
  theme(legend.position = "none")

# Panel B
p2 <- 
  ggplot(Scenario_liste,
         aes(x = half_life_TBV, 
             y = Indicator_10, 
             col = Degree_resistance)) +
  geom_boxplot() +
  labs(title = "", 
       x = "Half-life of TBV (years)", 
       y = "Rate of spread") +
  scale_color_manual(name = "Resistance level",
                     values = c("#de7065ff", "#a65c85ff", "#403891ff", "#0c2a50ff", "black"),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("Senstive (0)",
                                "Low (0.25)",
                                "Medium (0.5)" ,
                                "High (0.75)",
                                "Very high (1)")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5, 
                                  face = "bold"),
    legend.text = element_text(size = 18 / constant),
    legend.title = element_text(size = 18 / constant, face = "bold")) +
    theme(strip.text.x = element_text(size = 18 / constant,
                                      color = "black", 
                                      face = "bold"),
    strip.text.y = element_text(size = 18 / constant,
                                color = "black", 
                                face = "bold")) +
  theme(legend.position = "none")

# Panel C
p3 <- 
  ggplot(Scenario_liste,
         aes(x = half_life_TBV, 
             y = Prevalance_a * 100, 
             col = Degree_resistance)) +
  geom_boxplot() +
  labs(title = "", 
       x = "Half-life of TBV (years)", 
       y = "Prevalance of infection 15 years\n after first vaccine deployment (%)") +
  scale_color_manual(name = "Resistance level",
                     values = c("#de7065ff", "#a65c85ff", "#403891ff", "#0c2a50ff", "black"),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("Senstive (0)",
                                "Low (0.25)",
                                "Medium (0.5)" ,
                                "High (0.75)",
                                "Very high (0.9)")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold"),
        legend.text = element_text(size = 18 / constant),
        legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold"),
    strip.text.y = element_text(size = 18 / constant,
                                color = "black",
                                face = "bold")) +
  theme(legend.position = "none")

# Panel D
p4 <-
  ggplot(Scenario_liste,
         aes(x = Efficacy_TBV, 
             y = Prevalance_a * 100,
             col = Degree_resistance)) +
  geom_boxplot() +
  labs(title = "", 
       x = "Initial efficacy of TBV (%)", 
       y = "Prevalance of infection 15 years\n after 1st vaccine deployment (%)") +
  scale_color_manual(name = "Resistance level",
                     values = c("#de7065ff", "#a65c85ff", "#403891ff", "#0c2a50ff", "black"),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("Senstive (0)",
                                "Low (0.25)",
                                "Medium (0.5)" ,
                                "High (0.75)",
                                "Very high (0.9)")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5,
                                  face = "bold"),
        legend.text = element_text(18 / constant),
        legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold"),
        strip.text.y = element_text(size = 18 / constant,
                                    color = "black", 
                                    face = "bold")) +
  theme(legend.position = "none")

# Legend
LEG <-
  ggplot(Scenario_liste,
         aes(x = Efficacy_TBV, 
             y = Prevalance_a, 
             col = Degree_resistance)) +
  geom_boxplot() +
  labs(title = "", 
       x = "Initial efficacy of TBV (%)", 
       y = "Prevalance of infection 15 years\n after 1st vaccine deployment (%)") +
  scale_color_manual(name = "Degree of resistance\n to the AIV:",
                     values = c("#de7065ff", "#a65c85ff", "#403891ff", "#0c2a50ff", "black"),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("Sensitive (0)",
                                "Low (0.25)",
                                "Medium (0.5)" ,
                                "High (0.75)",
                                "Full (1)")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5,
                                  face = "bold"),
        legend.text = element_text(size = 18 / constant),
        legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold"),
    strip.text.y = element_text(size = 18 / constant, color = "black", face = "bold"))

#Get the legend
LEG <- get_legend(LEG)

# merge the different panel and legend
PLOT <- plot_grid(p2, p,  p3, p4, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"), label_size = 18 / constant, label_fontface = 2, scale = 0.99)
PLOT5 <- plot_grid(PLOT, LEG, ncol = 2, nrow = 1, rel_widths = c(3.5, 1),scale = 0.99)

# Save
ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_4.pdf", plot = PLOT5, width = 12, height = 11, device = "pdf", units = "cm", dpi = 300)
