###############################################################################
# Code to visualsise Figure Supplement 1A                                     #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# load package
library("rlang")
library("ggh4x")
library("ggplot2")

# load the data
Precision_final <-read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_4.csv", header = T)

# Define constrained variable as a factor
Precision_final$Coverage_reduced <- factor(Precision_final$Coverage_reduced, levels = c("0", "0.2"))
Precision_final$seasonality <- factor(Precision_final$seasonality, levels = c("sesonality1", "sesonality2"))
Precision_final$Age <- factor(Precision_final$Age, levels = c("18", "0.75"))
Precision_final$Blood_coverage <- factor(Precision_final$Blood_coverage, levels = c("0", "1"))


# Creat labels for each constrain factor
C.labs <- c("Coverage reduction = 0%", "Coverage reduction = 20%")
names(C.labs) <- c("0", "0.2")
S.labs <- c("Perennial deployment", "Seasonal deployment")
names(S.labs) <- c("sesonality1", "sesonality2")
A.labs <- c("Target the whole population", "Target only adult")
names(A.labs) <- c("0.75", "18")
B.labs <- c("No drug with vaccine", "Drug with vaccine")
names(B.labs) <- c("0", "1")

# ---- visualise the last round of adaptative sampling ----

# Select the data
Precision_final <- Precision_final[Precision_final$iteration == 4,]
cors <- ddply(Precision_final, c("Coverage_reduced", "seasonality", "Age", "Blood_coverage"), summarise, cor = round(cor(Test_True, Test_predicted), 3))
RMSEE <- ddply(Precision_final, c("Coverage_reduced", "seasonality", "Age", "Blood_coverage"), summarise, cor = round(RMSE(Test_True, Test_predicted), 3))

# Visualize
constant <- 2.5
PLOT <-
  ggplot(data = Precision_final, aes(x = Test_True, y = Test_predicted)) +
  facet_nested(seasonality * Blood_coverage ~ Age * Coverage_reduced ,
               labeller = labeller(seasonality = S.labs,
                                  Coverage_reduced = C.labs,
                                  Age = A.labs,
                                  Blood_coverage = B.labs)) +
  geom_point(size = 2.5 / constant) +
  theme_bw() +
  geom_smooth(method = lm,
              formula = y ~ x,
              se = FALSE,
              size = 2 / constant) +
  geom_text(data = cors,
            aes(label = paste("Cor=", cor, sep = "")),
            x = 0.4,
            y = 0.9,
            size = 5.5 / constant) +
  geom_text(data = RMSEE,
            aes(label = paste("RMSE=", cor, sep = "")),
            x = 0.4,
            y = 1.2,
            size = 5.5 / constant) +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(
          size = 18 / constant,
          hjust = 0.5,face = "bold")) +
  ylab("Predicted rate of spread") +
  xlab("Observed rate of spread") +
  ggtitle("") +
  theme(strip.text.x = element_text(size = 18 / constant, 
                                    color = "black",face = "bold"),
        strip.text.y = element_text(size = 18 / constant, 
                                    color = "black",
                                    face = "bold"))


# Save
ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_Supplement_4.pdf", plot = PLOT, width = 16, height = 16, device = "pdf", units = "cm", dpi = 300)



#--------------------------------------------------
# Function to estimate the Root mean squared error.
# --------------------------------------------------
RMSE <- function(x, y) {
  # do a liner regression
  model_regression <- lm(y ~ x)
  
  # estimate the residuals
  model_summary <- summary(model_regression)
  
  # estimate the Root mean squared error
  RMSE <- sqrt(mean((model_summary$residuals) ^ 2))
  
  # return RMSE
  return(RMSE)
}
