###############################################################################
# Code to visualize Figure Supplement 11                                     #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# load the data
Quantil_final_final <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_11.csv", header = T)

# Define the level
Quantil_final_final$Coverage_reduced <- factor(Quantil_final_final$Coverage_reduced, levels = c("0", "0.2"))
Quantil_final_final$Seasonality <- factor(Quantil_final_final$Seasonality, levels = c("sesonality2", "sesonality1"))
Quantil_final_final$Vaccine_type <- factor(Quantil_final_final$Vaccine_type, levels = c("AIV", "BSV"))
Quantil_final_final$G <- factor(Quantil_final_final$G, levels = c("Access", "half_life", "Coverage", "eir", "initialEfficacy", "Degree_resistance"))

# Define the labels for each constrain factor
C.labs <- c("No coverage\nreduction", "Coverage\nreduction = 20%")
names(C.labs) <- c("0", "0.2")

S.labs <- c("Perennial deployment", "Seasonal deployment")
names(S.labs) <- c("sesonality1", "sesonality2")

V.labs <- c("Anti-infective vaccine", "Blood-stage vaccine")
names(V.labs) <- c("AIV", "BSV")

F.labs <- c("Degree of resistance\nto the vaccine", 
            "Vaccine half-life (years)\n",
            "EIR (inoculations per \n person per year)", 
            "Vaccine coverage (%)\n",
            "Access to treatment (%)\n", 
            "Initial vaccine efficacy (%)\n")

names(F.labs) <-c("Degree_resistance",
                  "half_life",
                  "eir",
                  "Coverage",
                  "Access",
                  "initialEfficacy")

# Define the break for the y axis
break_y <- c(1, 10, 20, 29)
Label_yy <- c("Min", "", "", "Max")

# Visualise
############

# Constant
constant <- 2.5

Quantil_final_2<-Quantil_final_final

# Panel 1
P1 <-
  ggplot(data = Quantil_final_2[Quantil_final_2$G == "Degree_resistance", ]) +
  geom_line(aes(x = Range, y = M, color = G, linetype = Vaccine_type),
            size = 3 / constant, 
            alpha = 0.75) +
  geom_ribbon(aes(x = Range, ymin = L, ymax = U, fill = G, linetype = Vaccine_type), 
              alpha = 0.1) +
  facet_wrap(~ G,
             scales = "free_x",
             nrow = 1,
             strip.position = "bottom" ,
             labeller = labeller(G = F.labs))  +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Rate of spread", lim = c(0, 0.22)) +
  scale_x_continuous(name = "") +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant,
                                    face = "bold",
                                    hjust = -0.2),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5, 
                                  face = "bold")) +
  theme(legend.position = "none") +
  scale_color_manual(name = "Factors:",
                     values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                     labels = c("Degree of resistance\nto the vaccine",
                                "Vaccine half-life (years)",
                                "EIR (inoculations per \n person per year)",
                                "Vaccine coverage (%)",
                                "Access to treatment\n(%)",
                                "Initial vaccine efficacy\n(%)",
                                "Decay of vaccine efficacy\nat the second dose"),
                     breaks = c("Degree_resistance",
                                "half_life",
                                "eir",
                                "Coverage",
                                "Access",
                                "initialEfficacy",
                                "decay_efficacy")) +
  scale_fill_manual(name = "Factors:",
                    values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                    labels = c("Degree of resistance\nto the vaccine",
                               "Vaccine half-life (years)",
                               "EIR (inoculations per \n person per year)",
                               "Vaccine coverage (%)",
                               "Access to treatment\n(%)",
                               "Initial vaccine efficacy\n(%)",
                               "Decay of vaccine efficacy\nat the second dose"),
                    breaks = c("Degree_resistance",
                               "half_life",
                               "eir",
                               "Coverage",
                               "Access",
                               "initialEfficacy",
                               "decay_efficacy")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold"),
        strip.text.y = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold")) +
  theme(plot.margin = unit(c(0.1,  0.1, 0.1, 0.1), "cm")) +
  theme(strip.background = element_blank(),  strip.placement = "outside")


# Panel 2
P2 <-
  ggplot(data = Quantil_final_2[Quantil_final_2$G == "initialEfficacy", ]) +
  geom_line(aes(x = Range, y = M, color = G, linetype = Vaccine_type),
            size = 3 / constant, 
            alpha = 0.75) +
  geom_ribbon(aes(x = Range, ymin = L, ymax = U, fill = G, linetype = Vaccine_type), 
              alpha = 0.1) +
  facet_wrap(~ G,
             scales = "free_x",
             nrow = 1,
             strip.position = "bottom" ,
             labeller = labeller(G = F.labs))  +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Rate of spread", lim = c(0, 0.22)) +
  scale_x_continuous(name = "") +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant,
                                    face = "bold",
                                    hjust = -0.2),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5, 
                                  face = "bold")) +
  theme(legend.position = "none") +
  scale_color_manual(name = "Factors:",
                     values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                     labels = c("Degree of resistance\nto the vaccine",
                                "Vaccine half-life (years)",
                                "EIR (inoculations per \n person per year)",
                                "Vaccine coverage (%)",
                                "Access to treatment\n(%)",
                                "Initial vaccine efficacy\n(%)",
                                "Decay of vaccine efficacy\nat the second dose"),
                     breaks = c("Degree_resistance",
                                "half_life",
                                "eir",
                                "Coverage",
                                "Access",
                                "initialEfficacy",
                                "decay_efficacy")) +
  scale_fill_manual(name = "Factors:",
                    values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                    labels = c("Degree of resistance\nto the vaccine",
                               "Vaccine half-life (years)",
                               "EIR (inoculations per \n person per year)",
                               "Vaccine coverage (%)",
                               "Access to treatment\n(%)",
                               "Initial vaccine efficacy\n(%)",
                               "Decay of vaccine efficacy\nat the second dose"),
                    breaks = c("Degree_resistance",
                               "half_life",
                               "eir",
                               "Coverage",
                               "Access",
                               "initialEfficacy",
                               "decay_efficacy")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold"),
        strip.text.y = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold")) +
  theme(plot.margin = unit(c(0.1,  0.1, 0.1, 0.1), "cm")) +
  theme(strip.background = element_blank(),  strip.placement = "outside")


# Panel 3
P3 <- ggplot(data = Quantil_final_2[Quantil_final_2$G == "eir", ]) +
  geom_line(aes(x = Range, y = M, color = G, linetype = Vaccine_type),
            size = 3 / constant, 
            alpha = 0.75) +
  geom_ribbon(aes(x = Range, ymin = L, ymax = U, fill = G, linetype = Vaccine_type), 
              alpha = 0.1) +
  facet_wrap(~ G,
             scales = "free_x",
             nrow = 1,
             strip.position = "bottom" ,
             labeller = labeller(G = F.labs))  +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Rate of spread", lim = c(0, 0.22)) +
  scale_x_continuous(name = "") +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant,
                                    face = "bold",
                                    hjust = -0.2),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5, 
                                  face = "bold")) +
  theme(legend.position = "none") +
  scale_color_manual(name = "Factors:",
                     values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                     labels = c("Degree of resistance\nto the vaccine",
                                "Vaccine half-life (years)",
                                "EIR (inoculations per \n person per year)",
                                "Vaccine coverage (%)",
                                "Access to treatment\n(%)",
                                "Initial vaccine efficacy\n(%)",
                                "Decay of vaccine efficacy\nat the second dose"),
                     breaks = c("Degree_resistance",
                                "half_life",
                                "eir",
                                "Coverage",
                                "Access",
                                "initialEfficacy",
                                "decay_efficacy")) +
  scale_fill_manual(name = "Factors:",
                    values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                    labels = c("Degree of resistance\nto the vaccine",
                               "Vaccine half-life (years)",
                               "EIR (inoculations per \n person per year)",
                               "Vaccine coverage (%)",
                               "Access to treatment\n(%)",
                               "Initial vaccine efficacy\n(%)",
                               "Decay of vaccine efficacy\nat the second dose"),
                    breaks = c("Degree_resistance",
                               "half_life",
                               "eir",
                               "Coverage",
                               "Access",
                               "initialEfficacy",
                               "decay_efficacy")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold"),
        strip.text.y = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold")) +
  theme(plot.margin = unit(c(0.1,  0.1, 0.1, 0.1), "cm")) +
  theme(strip.background = element_blank(),  strip.placement = "outside")


# Panel 4
P4 <- ggplot(data = Quantil_final_2[Quantil_final_2$G == "Access", ]) +
  geom_line(aes(x = Range, y = M, color = G, linetype = Vaccine_type),
            size = 3 / constant, 
            alpha = 0.75) +
  geom_ribbon(aes(x = Range, ymin = L, ymax = U, fill = G, linetype = Vaccine_type), 
              alpha = 0.1) +
  facet_wrap(~ G,
             scales = "free_x",
             nrow = 1,
             strip.position = "bottom" ,
             labeller = labeller(G = F.labs))  +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Rate of spread", lim = c(0, 0.22)) +
  scale_x_continuous(name = "") +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant,
                                    face = "bold",
                                    hjust = -0.2),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5, 
                                  face = "bold")) +
  theme(legend.position = "none") +
  scale_color_manual(name = "Factors:",
                     values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                     labels = c("Degree of resistance\nto the vaccine",
                                "Vaccine half-life (years)",
                                "EIR (inoculations per \n person per year)",
                                "Vaccine coverage (%)",
                                "Access to treatment\n(%)",
                                "Initial vaccine efficacy\n(%)",
                                "Decay of vaccine efficacy\nat the second dose"),
                     breaks = c("Degree_resistance",
                                "half_life",
                                "eir",
                                "Coverage",
                                "Access",
                                "initialEfficacy",
                                "decay_efficacy")) +
  scale_fill_manual(name = "Factors:",
                    values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                    labels = c("Degree of resistance\nto the vaccine",
                               "Vaccine half-life (years)",
                               "EIR (inoculations per \n person per year)",
                               "Vaccine coverage (%)",
                               "Access to treatment\n(%)",
                               "Initial vaccine efficacy\n(%)",
                               "Decay of vaccine efficacy\nat the second dose"),
                    breaks = c("Degree_resistance",
                               "half_life",
                               "eir",
                               "Coverage",
                               "Access",
                               "initialEfficacy",
                               "decay_efficacy")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold"),
        strip.text.y = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold")) +
  theme(plot.margin = unit(c(0.1,  0.1, 0.1, 0.1), "cm")) +
  theme(strip.background = element_blank(),  strip.placement = "outside")


# Panel 5
P5 <-
  ggplot(data = Quantil_final_2[Quantil_final_2$G == "half_life", ]) +
  geom_line(aes(x = Range, y = M, color = G, linetype = Vaccine_type),
            size = 3 / constant, 
            alpha = 0.75) +
  geom_ribbon(aes(x = Range, ymin = L, ymax = U, fill = G, linetype = Vaccine_type), 
              alpha = 0.1) +
  facet_wrap(~ G,
             scales = "free_x",
             nrow = 1,
             strip.position = "bottom" ,
             labeller = labeller(G = F.labs))  +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Rate of spread", lim = c(0, 0.22)) +
  scale_x_continuous(name = "") +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant,
                                    face = "bold",
                                    hjust = -0.2),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5, 
                                  face = "bold")) +
  theme(legend.position = "none") +
  scale_color_manual(name = "Factors:",
                     values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                     labels = c("Degree of resistance\nto the vaccine",
                                "Vaccine half-life (years)",
                                "EIR (inoculations per \n person per year)",
                                "Vaccine coverage (%)",
                                "Access to treatment\n(%)",
                                "Initial vaccine efficacy\n(%)",
                                "Decay of vaccine efficacy\nat the second dose"),
                     breaks = c("Degree_resistance",
                                "half_life",
                                "eir",
                                "Coverage",
                                "Access",
                                "initialEfficacy",
                                "decay_efficacy")) +
  scale_fill_manual(name = "Factors:",
                    values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                    labels = c("Degree of resistance\nto the vaccine",
                               "Vaccine half-life (years)",
                               "EIR (inoculations per \n person per year)",
                               "Vaccine coverage (%)",
                               "Access to treatment\n(%)",
                               "Initial vaccine efficacy\n(%)",
                               "Decay of vaccine efficacy\nat the second dose"),
                    breaks = c("Degree_resistance",
                               "half_life",
                               "eir",
                               "Coverage",
                               "Access",
                               "initialEfficacy",
                               "decay_efficacy")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold"),
        strip.text.y = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold")) +
  theme(plot.margin = unit(c(0.1,  0.1, 0.1, 0.1), "cm")) +
  theme(strip.background = element_blank(),  strip.placement = "outside")


# Panel 6
P6 <- ggplot(data = Quantil_final_2[Quantil_final_2$G == "Coverage", ]) +
  geom_line(aes(x = Range, y = M, color = G, linetype = Vaccine_type),
            size = 3 / constant, 
            alpha = 0.75) +
  geom_ribbon(aes(x = Range, ymin = L, ymax = U, fill = G, linetype = Vaccine_type), 
              alpha = 0.1) +
  facet_wrap(~ G,
             scales = "free_x",
             nrow = 1,
             strip.position = "bottom" ,
             labeller = labeller(G = F.labs))  +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Rate of spread", lim = c(0, 0.22)) +
  scale_x_continuous(name = "") +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant,
                                    face = "bold",
                                    hjust = -0.2),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5, 
                                  face = "bold")) +
  theme(legend.position = "none") +
  scale_color_manual(name = "Factors:",
                     values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                     labels = c("Degree of resistance\nto the vaccine",
                                "Vaccine half-life (years)",
                                "EIR (inoculations per \n person per year)",
                                "Vaccine coverage (%)",
                                "Access to treatment\n(%)",
                                "Initial vaccine efficacy\n(%)",
                                "Decay of vaccine efficacy\nat the second dose"),
                     breaks = c("Degree_resistance",
                                "half_life",
                                "eir",
                                "Coverage",
                                "Access",
                                "initialEfficacy",
                                "decay_efficacy")) +
  scale_fill_manual(name = "Factors:",
                    values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                    labels = c("Degree of resistance\nto the vaccine",
                               "Vaccine half-life (years)",
                               "EIR (inoculations per \n person per year)",
                               "Vaccine coverage (%)",
                               "Access to treatment\n(%)",
                               "Initial vaccine efficacy\n(%)",
                               "Decay of vaccine efficacy\nat the second dose"),
                    breaks = c("Degree_resistance",
                               "half_life",
                               "eir",
                               "Coverage",
                               "Access",
                               "initialEfficacy",
                               "decay_efficacy")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold"),
        strip.text.y = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold")) +
  theme(plot.margin = unit(c(0.1,  0.1, 0.1, 0.1), "cm")) +
  theme(strip.background = element_blank(),  strip.placement = "outside")


# Merge the panel
PLOT_S1 <- plot_grid(P1, P2, P3, P4, P5, P6, ncol = 2, nrow = 3, scale = 1, label_size = 18 / constant, label_fontface = 2)

# Get the legend
P6_led <-
  ggplot(data = Quantil_final_2[Quantil_final_2$G == "Coverage", ]) +
  geom_line(aes(x = Range, y = M,  linetype = Vaccine_type),
            size = 3 / constant,
            alpha = 0.75) +
  geom_ribbon(aes(x = Range, ymin = L, ymax = U, linetype = Vaccine_type), alpha = 0.1) +
  facet_wrap(~ G, 
             scales = "free_x",
             nrow = 1,
             strip.position = "bottom" , 
             labeller = labeller(G = F.labs))  +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Rate of spread", 
                     lim = c(0, 0.22)) +
  scale_x_continuous(name = "") +
  scale_linetype_manual(name = "Vaccine type:",
                        values = c(1, 2),
                        breaks = c("AIV", "BSV"),
                        labels = c("Anti-infective vaccine", "Blood-stage vaccine")) +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant,
                                    face = "bold",
                                    hjust = -0.2),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  theme(legend.text = element_text(size = 18 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold"))   +
  scale_color_manual(name = "Factors:",
                     values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                     labels = c("Degree of resistance\nto the vaccine",
                                "Vaccine half-life (years)", 
                                "EIR (inoculations per \n person per year)",
                                "Vaccine coverage (%)", 
                                "Access to treatment\n(%)", 
                                "Initial vaccine efficacy\n(%)", 
                                "Decay of vaccine efficacy\nat the second dose"),
                     breaks = c("Degree_resistance",
                                "half_life",
                                "eir", 
                                "Coverage", 
                                "Access", 
                                "initialEfficacy", 
                                "decay_efficacy")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
        strip.text.y = element_text(size = 18 / constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(0.1,  0.1, 0.1, 0.1), "cm")) +
  theme(strip.background = element_blank(),  strip.placement = "outside") +
  theme(legend.spacing.x = unit(0.25, "cm"),
        legend.spacing.y = unit(0.25, "cm")) +
  theme(legend.key.size = unit(0.9, "cm")) +
  theme(legend.key.width = unit(1.5, "cm")) +
  theme(legend.position = "top")


leg <- get_legend(P6_led)


# Add legend to the graph
PB <- plot_grid(leg, PLOT_S1, ncol = 1, nrow = 2, rel_heights  = c(1, 10))

# Save
ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_Supplement_11.pdf", plot = PB, width = 12, height = 19, device = "pdf", units = "cm", dpi = 300)
