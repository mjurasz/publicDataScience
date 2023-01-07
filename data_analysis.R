# let's try to deal with memory limitations
memory.size (max=TRUE)
memory.size (max=FALSE)
gc()
memory.size (max=FALSE)

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(ggfortify)) install.packages("ggfortify", repos = "http://cran.us.r-project.org") 
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(visreg)) install.packages("visreg", repos = "http://cran.us.r-project.org") 
if(!require(gbm)) install.packages("visreg", repos = "http://cran.us.r-project.org") 

library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(ggthemes)
library(broom)
library(kableExtra)
library(lubridate)
library(reshape2)
library(Matrix)
library(ggfortify)
library(e1071)
library(visreg)
library(gbm)

# dataset: https://www.kaggle.com/nitindatta/graduate-admission-chances
# GitHub location (copied): https://github.com/mjurasz/edX-CYO/raw/main/archive_Admission_Predict.zip

# let's set some defaults for Kable
KableTidy = function(x) {
  knitr::kable(x, format.args = list(decimal.mark = '.', big.mark = ",", booktabs = TRUE)) %>% 
    kable_styling(latex_options = c("striped", "hold_position"), font_size = 8)
}

# let's download data from GitHub - m.jurasz repo
# I was struggling trying download csv files from original location, so uploaded it to GitHub as zip archive
dl <- tempfile()
download.file("https://github.com/mjurasz/edX-CYO/raw/main/archive_Admission_Predict.zip", dl)

# let's read all data into data frame
# and let's add underscores instead of spaces to avoid issues
admissions <- fread(text = readLines(unzip(dl, "Admission_Predict_Ver1.1.csv")),
                 col.names = c("Serial_No.","GRE_Score","TOEFL_Score","University_Rating","SOP","LOR","CGPA","Research","Chance_of_Admit"))

###########################################################
# let's split data to train and test set
###########################################################
set.seed(1, sample.kind="Rounding") # if using a later version than R 3.5

test_index <- createDataPartition(admissions$GRE_Score, times = 1, p = 0.7, list = F)
training_set <- admissions %>% dplyr::slice(-test_index)
testing_set <- admissions %>% dplyr::slice(test_index)

# let's summarize all data and put into Kable
summary(admissions) %>%
  kbl(booktabs = T) %>% 
  kable_styling(full_width = FALSE, position = "left", bootstrap_options = "bordered", font_size = 10)

# let's see how Research information distributes on histogram
ggplot(admissions, aes(Research)) +
geom_histogram(binwidth = 0.6, colour = "grey", fill = "blue", alpha = 0.6) +
scale_x_continuous(breaks = seq(0, 2, 1)) +
scale_y_continuous(breaks = seq(0, 300, 25)) +
labs(title = "Research information - stats", x = "Research", y = "Count") +
  theme_bw()

###########################################################
# GRE Score
###########################################################
# Let's have a look at GRE Score boxplot 
# and check if there is any pattern in the presence of overplotting
# this is to see basic details not using table
grid.arrange(
  admissions %>% 
     group_by(GRE_Score) %>%
     ggplot(aes(University_Rating, GRE_Score, group = 1)) + 
     geom_boxplot(fill = "steelblue", alpha = 0.3) +
     labs(title = "Analysis of GRE Score - boxplot", y = "GRE Score", fill = element_blank()) +
    theme_bw(), 
  ggplot(admissions, aes(GRE_Score)) +
    geom_histogram(binwidth = 5.3, colour = "black", fill = "steelblue", alpha = 0.4) +
    scale_x_continuous(breaks = seq(280, 340, 10)) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    labs(title = "GRE Score information - histogram", x = "GRE Score", y = "Frequency") +
    theme_bw(), 
  ncol = 2
  )

ggplot(admissions, aes(Chance_of_Admit, GRE_Score)) +
  geom_point(aes(color = factor(Research), alpha = 0.5, size = 2.0)) + 
  theme_bw()

###########################################################
# TOEFL Score
###########################################################
# Let's have a look at TOEFL Score boxplot 
# and check if there is any pattern in the presence of overplotting
# this is to see basic details not using table
grid.arrange(
  admissions %>% 
    group_by(TOEFL_Score) %>%
    ggplot(aes(University_Rating, TOEFL_Score, group = 1)) + 
    geom_boxplot(fill = "steelblue", alpha = 0.3) +
    labs(title = "Analysis of TOEFL Score", y = "TOEFL Score", fill = element_blank()) +
    theme_bw(),
  admissions %>% 
    group_by(TOEFL_Score) %>%
    ggplot(aes(GRE_Score, TOEFL_Score, group = 1)) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_smooth(aes(GRE_Score, TOEFL_Score), group = 1, formula = y ~ x, method = loess) +
    labs(title = "TOEFL Score information - trends", x = "GRE_Score", y = "TOEFL Score") +
    theme_bw(), 
  ggplot(admissions, aes(TOEFL_Score)) +
    geom_histogram(binwidth = 2.2, colour = "black", fill = "steelblue", alpha = 0.4) +
    scale_x_continuous(breaks = seq(80, 130, 5)) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    labs(title = "TOEFL Score information - histogram", x = "TOEFL Score", y = "Frequency") +
    theme_bw(),
  ncol = 3
)

ggplot(admissions, aes(Chance_of_Admit, TOEFL_Score)) +
  geom_point(aes(color = factor(Research), alpha = 0.5, size = 2.0)) + 
  theme_bw()

###########################################################
# University Rating
###########################################################
# Let's have a look at University Rating boxplot 
# and check if there is any pattern in the presence of overplotting
# this is to see basic details not using table
grid.arrange(
  admissions %>% 
    group_by(University_Rating) %>%
    ggplot(aes(GRE_Score, University_Rating, group = 1)) + 
    geom_point(alpha = 0.4, size = 1.5) +
    geom_smooth(aes(GRE_Score, University_Rating), group = 1, formula = y ~ x, method = loess) +
    labs(title = "University Rating information - trends", x = "GRE_Score", y = "University Rating") +
    theme_bw(), 
  ggplot(admissions, aes(University_Rating)) +
    geom_histogram(binwidth = 1.0, colour = "black", fill = "steelblue", alpha = 0.4) +
    scale_x_continuous(breaks = seq(0, 6, 1)) +
    scale_y_continuous(breaks = seq(0, 180, 10)) +
    labs(title = "University Rating information - histogram", x = "University Rating", y = "Frequency") +
    theme_bw(),
  ncol = 2
)

ggplot(admissions, aes(Chance_of_Admit, University_Rating)) +
  geom_point(aes(color = factor(Research), alpha = 0.5, size = 2.0)) + 
  theme_bw()

###########################################################
# SOP
###########################################################
# Let's have a look at SOP boxplot 
# and check if there is any pattern in the presence of overplotting
# this is to see basic details not using table
grid.arrange(
  admissions %>% 
    group_by(SOP) %>%
    ggplot(aes(University_Rating, SOP, group = 1)) + 
    geom_boxplot(fill = "steelblue", alpha = 0.3) +
    labs(title = "Analysis of SOP", y = "SOP", fill = element_blank()) +
    theme_bw(), 
  admissions %>% 
    group_by(SOP) %>%
    ggplot(aes(GRE_Score, SOP, group = 1)) + 
    geom_point(alpha = 0.4, size = 1.5) +
    geom_smooth(aes(GRE_Score, SOP), group = 1, formula = y ~ x, method = loess) +
    labs(title = "SOP information - trends", x = "GRE_Score", y = "SOP") +
    theme_bw(),
  ggplot(admissions, aes(SOP)) +
    geom_histogram(binwidth = 0.5, colour = "black", fill = "steelblue", alpha = 0.4) +
    scale_x_continuous(breaks = seq(0, 6, 1)) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    labs(title = "SOP information - histogram", x = "SOP", y = "Frequency") +
    theme_bw(), 
  ncol = 3
)

#hist(admissions$SOP, col="steelblue",
#     xlab="Class for SOP", ylab="Frequency", main="Histogram for SOP", labels=TRUE)

plot(admissions$SOP, admissions$Chance_of_Admit,
     col="steelblue",
     main="Scatter Plot Between SOP and Chance of Admission",
     xlab="SOP",
     ylab="Chance of Admission") +

ggplot(admissions, aes(Chance_of_Admit, SOP)) +
  geom_point(aes(color = factor(Research), alpha = 0.5, size = 2.0)) + 
  theme_bw()

###########################################################
# LOR
###########################################################
# Let's have a look at LOR boxplot 
# and check if there is any pattern in the presence of overplotting
# this is to see basic details not using table
grid.arrange(
  admissions %>% 
    group_by(LOR) %>%
    ggplot(aes(University_Rating, LOR, group = 1)) + 
    geom_boxplot(fill = "steelblue", alpha = 0.3) +
    labs(title = "Analysis of LOR", y = "LOR", fill = element_blank()) +
    theme_bw(), 
  admissions %>% 
    group_by(LOR) %>%
    ggplot(aes(GRE_Score, LOR, group = 1)) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_smooth(aes(GRE_Score, LOR), group = 1, formula = y ~ x, method = loess) +
    labs(title = "LOR information - trends", x = "GRE_Score", y = "LOR") +
    theme_bw(), 
  ggplot(admissions, aes(LOR)) +
    geom_histogram(binwidth = 0.5, colour = "black", fill = "steelblue", alpha = 0.4) +
    scale_x_continuous(breaks = seq(0, 6, 1)) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    labs(title = "LOR information - histogram", x = "LOR", y = "Frequency") +
    theme_bw(),
  ncol = 3
)

plot(admissions$LOR, admissions$Chance_of_Admit,
     col="steelblue",
     main="Scatter Plot Between LOR and Chance of Admission",
     xlab="LOR",
     ylab="Chance of Admission") +

ggplot(admissions, aes(Chance_of_Admit, LOR)) +
  geom_point(aes(color = factor(Research), alpha = 0.5, size = 2.0)) + 
  theme_bw()

###########################################################
# CGPA
###########################################################
# Let's have a look at CPGA boxplot 
# and check if there is any pattern in the presence of overplotting
# this is to see basic details not using table
grid.arrange(
  admissions %>% 
    group_by(CGPA) %>%
    ggplot(aes(University_Rating, CGPA, group = 1)) + 
    geom_boxplot(fill = "steelblue", alpha = 0.3) +
    labs(title = "Analysis of CGPA", y = "CGPA", fill = element_blank()) +
    theme_bw(), 
  admissions %>% 
    group_by(CGPA) %>%
    ggplot(aes(GRE_Score, CGPA, group = 1)) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_smooth(aes(GRE_Score, CGPA), group = 1, formula = y ~ x, method = loess) +
    labs(title = "CGPA information - trends", x = "GRE_Score", y = "CGPA") +
    theme_bw(), 
  ggplot(admissions, aes(CGPA)) +
    geom_histogram(binwidth = 0.55, colour = "black", fill = "steelblue", alpha = 0.4) +
    scale_x_continuous(breaks = seq(5, 11, 0.5)) +
    scale_y_continuous(breaks = seq(0, 160, 10)) +
    labs(title = "CGPA information - histogram", x = "CGPA", y = "Frequency") +
    theme_bw(),
  ncol = 3
)

ggplot(admissions, aes(Chance_of_Admit, CGPA)) +
  geom_point(aes(color = factor(Research), alpha = 0.5, size = 2.0)) + 
  theme_bw()

###########################################################
# Research
###########################################################
# no sense to draw boxplot and geom_smooth

ggplot(admissions, aes(Research)) +
  geom_histogram(binwidth = 1, colour = "black", fill = "steelblue", alpha = 0.4) +
  scale_x_continuous(breaks = seq(0, 2, 1)) +
  scale_y_continuous(breaks = seq(0, 300, 25)) +
  labs(title = "Research information - histogram", x = "Research", y = "Frequency") +
  theme_bw()

cor(admissions$Research,admissions$Chance_of_Admit)
chisq.test(admissions$Research, admissions$Chance_of_Admit)

###########################################################
# Boxplots - for most interesting scores
###########################################################
grid.arrange(
  # GRE score depending on the rating of University
admissions %>% 
  group_by(University_Rating) %>% 
  ggplot(aes(University_Rating, GRE_Score, group = University_Rating)) + 
  geom_boxplot(fill = "steelblue", alpha = 0.3) +
  theme_bw(),
# We can notice some outliers with low GRE score applying to Universities of Ranking 4 and 5.
# TOEFL score depending on the rating of University
admissions %>% 
  group_by(University_Rating) %>% 
  ggplot(aes(University_Rating, TOEFL_Score, group = University_Rating)) + 
  geom_boxplot(fill = "steelblue", alpha = 0.3) +
  theme_bw(),
# CGPA score depending on the rating of University
admissions %>% 
  group_by(University_Rating) %>% 
  ggplot(aes(University_Rating, CGPA, group = University_Rating)) + 
  geom_boxplot(fill = "steelblue", alpha = 0.3) +
  theme_bw(),
  ncol = 3
)

###########################################################
# Let's have a look at correlation now
###########################################################
correlations <- cor(admissions)
corrplot::corrplot(correlations, method = "circle", outline = T, addgrid.col = "darkgray",
         addrect = 4, rect.col = "black", rect.lwd = 1, tl.col = "darkblue", tl.cex = 0.8, cl.cex = 0.8,
         col = colorRampPalette(c("darkred", "white", "midnightblue"))(100))

###########################################################
# ############### REGRESSION ALGORITHMS ################# #
###########################################################

###########################################################
# linear regression scenario
###########################################################
scenario_lm <- lm(Chance_of_Admit ~ ., data = training_set)
summary(scenario_lm)
autoplot(scenario_lm, which = 1:6, ncol = 2, label.size = 3)

y_hat_lm <- predict(scenario_lm, testing_set)

# calculating and collecting RMSE for linear regression scenario
roundup_RMSE <- data.frame(scenario = "Linear regression", 
                      RMSE = sqrt(mean((y_hat_lm - testing_set$Chance_of_Admit) ^ 2)))

###########################################################
# linear regression scenario - limited to selected params
###########################################################
scenario_lmLimited = lm(Chance_of_Admit ~ GRE_Score + TOEFL_Score + CGPA + Research, data = training_set)
summary(scenario_lmLimited)
autoplot(scenario_lmLimited, which = 1:6, ncol = 2, label.size = 3)

y_hat_lmLimited <- predict(scenario_lmLimited, testing_set)

# calculating and collecting RMSE for logistic regression
roundup_RMSE <- rbind(roundup_RMSE, data.frame(scenario = "Logistic regression - extended", 
                                     RMSE = sqrt(mean((y_hat_lmLimited - testing_set$Chance_of_Admit) ^ 2))))

###########################################################
# logistic regression scenario
###########################################################
scenario_glm <- glm(Chance_of_Admit ~ ., data = training_set)
summary(scenario_glm)
autoplot(scenario_glm, which = 1:6, ncol = 2, label.size = 3)

y_hat_glm <- predict(scenario_glm, testing_set, type = "response")

# calculating and collecting RMSE for logistic regression
roundup_RMSE <- rbind(roundup_RMSE, data.frame(scenario = "Logistic regression", 
                      RMSE = sqrt(mean((y_hat_glm - testing_set$Chance_of_Admit) ^ 2))))

###########################################################
# randomForest scenario
###########################################################
scenario_randomForest <- randomForest(Chance_of_Admit ~ ., training_set)

plot(scenario_randomForest, log="y")
varImpPlot(scenario_randomForest)
visreg(scenario_randomForest, "CGPA", ylab="Chance_of_Admit", gg=TRUE) + 
  theme_bw()

y_hat_randomForest <- predict(scenario_randomForest, testing_set)

# Calculating and collecting RMSE for randomForest
roundup_RMSE <- rbind(roundup_RMSE, data.frame(scenario = "Random forest", 
                      RMSE = sqrt(mean(y_hat_randomForest - testing_set$Chance_of_Admit) ^ 2)))

###########################################################
# Gradient boosted trees
###########################################################
# default settings for gbm do not produce a very good fit
# In particular, the default number of trees (100) is too low to capture 
# the relationship between predictors. 
# By increasing the number of trees, we obtain a much more reasonable result.

scenariogbm <- gbm(Chance_of_Admit ~ GRE_Score + TOEFL_Score + CGPA + Research, 
                   data = training_set, distribution = "gaussian", n.trees = 5000)

# The implementation of gradient boosted trees in the gbm package does not
# offer a residuals method. This would normally cause visreg to omit plotting 
# the partial residuals. However, we can supply our own user-defined residuals method
residuals.gbm <- function(scenariogbm) { scenariogbm$data$y - scenariogbm$scenariogbm }

# insight into setting tuning parameters
visreg(scenariogbm, "CGPA", ylab="Chance_of_Admit", gg=TRUE) + 
  theme_bw()

y_hat_gbm <- predict(scenariogbm, testing_set)

# Calculating and collecting RMSE for Gradient boosted trees
roundup_RMSE <- rbind(roundup_RMSE, data.frame(scenario = "Gradient boosted trees", 
                RMSE = sqrt(mean(y_hat_gbm - testing_set$Chance_of_Admit) ^ 2)))

###########################################################
# Support vector machines scenario
###########################################################

scenarioSVM <- svm(Chance_of_Admit ~ GRE_Score + TOEFL_Score + 
                     CGPA + Research, data = training_set)

visreg(scenarioSVM, "CGPA", ylab="Chance_of_Admit", gg=TRUE) + 
  theme_bw()

y_hat_SVM <- predict(scenarioSVM, testing_set)

# Calculating and collecting RMSE for Support vector machines
roundup_RMSE <- rbind(roundup_RMSE, data.frame(scenario = "Support vector machines", 
                RMSE = sqrt(mean(y_hat_SVM - testing_set$Chance_of_Admit) ^ 2)))

###########################################################
# let's print summary
###########################################################
roundup_RMSE %>% 
  kbl(booktabs = T) %>% 
  kable_styling(full_width = FALSE, position = "left", bootstrap_options = "bordered", font_size = 10) 

###########################################################
# ############# CLASSIFICATION ALGORITHMS ############### #
###########################################################

###########################################################
# let's change the data a bit
###########################################################
# cutoff find based on a logical condition - admitted and not admitted, 1 and 0 if greater than mean
Admitted <- ifelse(admissions$Chance_of_Admit >= mean(admissions$Chance_of_Admit), 1, 0)
admissions <- mutate(admissions, Admitted)

# column used for producing the cutoff no longer required (this should not be a predictor)
admissions <- admissions[,-9]

# are columns "Research" and "Admitted" identical? No, they are NOT
identical(admissions$Research, admissions$Admitted)

set.seed(1, sample.kind="Rounding") # if using a later version than R 3.5

# this part requires a bit different approach
test_index <- createDataPartition(y = admissions$Admitted, times = 1, p = 0.1, list = FALSE)
admit_set <- admissions[-test_index, ]
validation_set <- admissions[test_index, ]

set.seed(1, sample.kind="Rounding") # if using a later version than R 3.5

test_index <- createDataPartition(y = admit_set$Admitted, times = 1, p = 0.1, list = FALSE)
training_set <- admissions[-test_index, ]
testing_set <- admissions[test_index, ]

###########################################################
# GRE score based scenario 
###########################################################

# GRE score seems to be strongly correlated with the admission rate.
# let's have a look at the estimation using GRE score predictor only. 
# GRE score is not categorical, so let's use as cutoff the average GRE minus 
# two standard deviations of the average obtained from admitted. 
# param has been imposed

# The value of 'Chance of Admit' is the chance each applicant thought of being accepted
# Actual information (strict) on admittion (or vice versa) is not part of the dataset
# We can apply a cutoff equal to the average of 'Chance of Admit'. 
admittedFiltered <- testing_set %>% filter(Admitted == 1)

# Table admittedFiltered only includes the values of those admitted.
cutoff_GRE_admit <- mean(admittedFiltered$GRE_Score) - 2 * sd(admittedFiltered$GRE_Score)
cutoff_GRE_admit

GRE_scenario <- ifelse(testing_set$GRE_Score >= cutoff_GRE_admit, 1, 0) 

# calculating and collecting accuracy for GRE score based scenario
GRE_scenario_accuracy <- mean(GRE_scenario == testing_set$Admitted)

roundup_accuracy <- data.frame(scenario = "GRE score based scenario", accuracy = GRE_scenario_accuracy)

###########################################################
# CPGA based scenario
###########################################################
# CGPA presents strong correlation with the admission rate as well.
# The cutoff again will be the average CGPA minus two standard deviations
cutoff_CGPA_admit <- mean(admittedFiltered$CGPA)- 2 * sd(admittedFiltered$CGPA)
cutoff_CGPA_admit

CGPA_scenario <- ifelse(testing_set$CGPA >= cutoff_CGPA_admit, 1, 0) 
# a bit good improvement in comparison to the GRE score

# calculating and collecting accuracy for CPGA score based scenario
accuracy_CGPA_scenario <- mean(CGPA_scenario == testing_set$Admitted)

roundup_accuracy <- rbind(roundup_accuracy, 
                          data.frame(scenario = "CPGA based scenario", accuracy = accuracy_CGPA_scenario))

###########################################################
# TOEFL based scenario
###########################################################
# TOEFL has less strong correlation
cutoff_TOEFL_admit <- mean(admittedFiltered$TOEFL) - 2 * sd(admittedFiltered$TOEFL)
cutoff_TOEFL_admit

# Let's try using directly as cutoff the average TOEFL score
TOEFL_scenario <- ifelse(testing_set$TOEFL >= cutoff_TOEFL_admit, 1, 0) 

# calculating and collecting accuracy for TOEFL score based scenario
accuracy_TOEFL_scenario <- mean(TOEFL_scenario == testing_set$Admitted)
# accuracy looks better than GRE and CGPA.

roundup_accuracy <- rbind(roundup_accuracy, 
                          data.frame(scenario = "TOEFL based scenario", accuracy = accuracy_TOEFL_scenario))

###########################################################
# Combined TOEFL + CGPA + GRE scenario
###########################################################
# Let's try a scenario that includes all above predictors (predictors with highest correlation)
# cutoff again will be the mean of each predictor minus two times the standard deviation

TOEFL_CGPA_GRE_scenario <- ifelse(testing_set$TOEFL >= cutoff_TOEFL_admit &
                                             testing_set$CGPA >= cutoff_CGPA_admit &
                                             testing_set$GRE >= cutoff_GRE_admit,
                                   1, 0) 

# calculating and collecting accuracy for combined scenario
accuracy_TOEFL_CGPA_GRE_scenario <- mean(TOEFL_CGPA_GRE_scenario == testing_set$Admitted)
# accuracy looks better 

roundup_accuracy <- rbind(roundup_accuracy, 
                          data.frame(scenario = "TOEFL CGPA GRE combined scenario", accuracy = accuracy_TOEFL_CGPA_GRE_scenario))

###########################################################
# CONFUSION MATRICES
###########################################################

print("Confusion Matrix for GRE scenario")
confusionMatrix(data = factor(GRE_scenario), reference = factor(testing_set$Admitted))

print("Confusion Matrix for CGPA scenario")
confusionMatrix(data = factor(CGPA_scenario), reference = factor(testing_set$Admitted))

print("Confusion Matrix for TOEFL scenario")
confusionMatrix(data = factor(TOEFL_scenario), reference = factor(testing_set$Admitted))

print("Confusion Matrix for TOEFL CGPA GRE combined scenario")
confusionMatrix(data = factor(TOEFL_CGPA_GRE_scenario), reference = factor(testing_set$Admitted))

###########################################################
# can other scenarios increase the accuracy?
###########################################################
###########################################################
# kNN scenario
###########################################################
set.seed(1, sample.kind="Rounding") # if using a later version than R 3.5
fit_knn_all <- train(factor(Admitted) ~ GRE_Score + TOEFL_Score + CGPA + Research + SOP + LOR + University_Rating, method = "knn", 
                     data = training_set, 
                     tuneGrid = data.frame(k = seq(3, 20, 1)))
accuracy_knn_all <- confusionMatrix(predict(fit_knn_all, testing_set), as_factor(testing_set$Admitted))$overall["Accuracy"]

roundup_accuracy <- rbind(roundup_accuracy, 
                          data.frame(scenario = "kNN scenario", accuracy = accuracy_knn_all))

plot(fit_knn_all)

best_k <- fit_knn_all$bestTune
# good accuracy obtained, but not strongly better than previous
best_k

###########################################################
# let's try to cross validate kNN
###########################################################
set.seed(1, sample.kind="Rounding") # if using a later version than R 3.5
control <- trainControl(method = "cv", number = 5, p = .1)
fit_knn_crossValidation <- train(factor(Admitted) ~ GRE_Score + TOEFL_Score + CGPA + 
                                   Research + SOP + LOR + University_Rating, method = "knn", 
                    data = training_set,
                    tuneGrid = data.frame(k = seq(3, 20, 1)),
                    trControl = control)
accuracy_knn_crossValidation <- confusionMatrix(predict(fit_knn_crossValidation, testing_set), 
                                                as_factor(testing_set$Admitted))$overall["Accuracy"]

roundup_accuracy <- rbind(roundup_accuracy, 
                          data.frame(scenario = "K-nearest neighbor cross-validated scenario", 
                                     accuracy = accuracy_knn_crossValidation))

plot(fit_knn_crossValidation)

best_k <- fit_knn_crossValidation$bestTune
best_k

###########################################################
# again Random Forest this time classification approach
###########################################################
# let's find best parameter selection - mtry 
set.seed(1, sample.kind="Rounding") # if using a later version than R 3.5

fitRandomForest <- train(as_factor(Admitted) ~ ., method = "rf", data = training_set, 
                         tuneGrid = data.frame(mtry = seq(1, 8, 0.1)), 
                         ntree = 50)
confusionMatrix(predict(fitRandomForest, testing_set),
                as_factor(testing_set$Admitted))$overall["Accuracy"]

plot(fitRandomForest)
best_mtry <- fitRandomForest$bestTune
best_mtry

# selection of number of trees 
numberOfTrees <- seq(1, 800, 50)

fitRandomForest <- sapply(numberOfTrees, function(n) {
  train(as_factor(Admitted) ~ ., method = "rf", data = training_set, tuneGrid = data.frame(mtry = best_mtry), numberOfTrees = n)$results$Accuracy
})

qplot(numberOfTrees, fitRandomForest)
best_tree <- numberOfTrees[which.max(fitRandomForest)]
best_tree

# Then, the final model will be with the best MTRY and best NTREE
fitRandomForest <- train(as_factor(Admitted) ~ ., method = "rf", data = training_set, tuneGrid = data.frame(mtry = best_mtry), ntree = best_tree)
accuracyRandomForest <- confusionMatrix(predict(fitRandomForest, testing_set), as_factor(testing_set$Admitted))$overall["Accuracy"]

roundup_accuracy <- rbind(roundup_accuracy, 
                          data.frame(scenario = "Random Forest scenario", accuracy = accuracyRandomForest))

varImp(fitRandomForest)

#####################################################################
# xgboost try - ignore
#####################################################################
# training_set_matrix <- as.matrix(training_set)
# testing_set_matrix <- as.matrix(testing_set)
# title <- "extreme gradient boosting"
#
# m1_xgb <-
#  xgboost(
#    data = training_set_matrix[, 1:8],
#    label = training_set_matrix[, 9],
#    nrounds = 1000,
#    objective = "reg:squarederror",
#    early_stopping_rounds = 3,
#    max_depth = 6,
#    eta = .25
# )   

# pred_xgb <- predict(m1_xgb, testing_set_matrix[, 1:8])

# yhat <- pred_xgb
# y <- testing_set_matrix[, 9]
# postResample(yhat, y)

# r <- y - yhat
# plot(r, ylab = "residuals", main = title)

# plot(y, yhat, xlab = "actual", ylab = "predicted", main = title)
# abline(lm(yhat ~ y))

###########################################################
# let's print final summary
###########################################################
roundup_accuracy %>% 
  kbl(booktabs = T) %>% 
  kable_styling(full_width = FALSE, position = "left", bootstrap_options = "bordered", font_size = 10) 

###########################################################
# Analysis of solutions and findings
###########################################################
# kNN cross validated validation
###########################################################
set.seed(1, sample.kind="Rounding") # if using a later version than R 3.5

control <- trainControl(method = "cv", number = 5, p = .1)
fit_knncvFinal <- train(factor(Admitted) ~ GRE_Score + TOEFL_Score + CGPA + 
                      Research + SOP + LOR + University_Rating, method = "knn", 
                    data = admit_set,
                    tuneGrid = data.frame(k = best_k),
                    trControl = control)

accuracy_kNN_CV_validation <- confusionMatrix(predict(fit_knncvFinal, validation_set), 
                                              as_factor(validation_set$Admitted))$overall["Accuracy"]

roundup_validationaccuracy <- data.frame(scenario = "K-nearest neighbor cross-validated scenario", 
                                         accuracy = accuracyRandomForest)

###########################################################
# randomForest solution validation
###########################################################
set.seed(1, sample.kind="Rounding") # if using a later version than R 3.5

fit_randomForestFinal <- train(as_factor(Admitted) ~ ., method = "rf", 
                      data = admit_set, 
                      tuneGrid = data.frame(mtry = best_mtry), ntree = best_tree)

accuracyRandomForest <- confusionMatrix(predict(fit_randomForestFinal, validation_set), 
                                        as_factor(validation_set$Admitted))$overall["Accuracy"]

# accuracy decreased probably due to some overfitting.

varImp(fit_randomForestFinal)

roundup_validationaccuracy <- rbind(roundup_validationaccuracy, 
                          data.frame(scenario = "Random Forest scenario", accuracy = accuracyRandomForest))

###########################################################
# let's print validation summary
###########################################################
roundup_validationaccuracy %>% 
  kbl(booktabs = T) %>% 
  kable_styling(full_width = FALSE, position = "left", bootstrap_options = "bordered", font_size = 10) 
