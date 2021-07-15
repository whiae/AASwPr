library(factoextra)
library(cluster)
library(genieclust)
library(e1071)
library(caTools)
library(caret)
library(MLmetrics)

#ZADANIE 1
spam <- read.csv(file = "C:/spambase.csv", 
                     sep=",", dec=".", 
                     header = TRUE)

split <- sample.split(spam, SplitRatio = 0.7)
train_cl <- subset(spam, split == "TRUE")
test_cl <- subset(spam, split == "FALSE")

train_scale <- scale(train_cl[, 1:57])
test_scale <- scale(test_cl[, 1:57])

set.seed(120)
classifier_cl <- naiveBayes(class ~ ., data = train_scale)
classifier_cl

y_pred <- predict(classifier_cl, newdata = test_scale)

cm <- table(test_cl$class, y_pred)
cm

confusionMatrix(cm)
F1_Score(y_pred, test_cl$class)

#ZADANIE 2
factbook <- read.csv(file = "C:/factbook.csv", 
                 sep=",", dec=".", 
                 header = FALSE)

colnames(factbook) <- c("country","area","population","median_age","population_growth_rate","birth_rate","death_rate","net_migration_rate","maternal_mortality_rate","infant_mortality_rate","life_expectancy_at_birth","total_fertility_rate","hiv_aids_adult_prevalence_rate","hiv_aids_people_living_with","hiv_aids_deaths","obesity_adult_prevalence_rate","children_under_age_5_underweight","education_expenditures","unemployment_youth_ages_15_to_24","gdp_purchasing_power_parity","gdp_real_growth_rate","gdp_per_capita_ppp","gross_national_saving","industrial_production_growth_rate","labor_force","unemployment_rate","taxes_and_other_revenues","budget_surplus_or_deficit","public_debt","inflation_rate_consumer_prices","current_account_balance","exports","imports","reserves_of_foreign_exchange_and_gold","debt_external","electricity_production","electricity_consumption","electricity_exports","electricity_imports","electricity_installed_generating_capacity","electricity_from_fossil_fuels","electricity_from_nuclear_fuels","electricity_from_hydroelectric_plants","electricity_from_other_renewable_sources","crude_oil_production","crude_oil_exports","crude_oil_imports","crude_oil_proved_reserves","refined_petroleum_products_production","refined_petroleum_products_consumption","refined_petroleum_products_exports","refined_petroleum_products_imports","natural_gas_production","natural_gas_consumption","natural_gas_exports","natural_gas_imports","natural_gas_proved_reserves","carbon_dioxide_emissions_from_consumption_of_energy","telephones_fixed_lines","telephones_mobile_cellular","internet_users","broadband_fixed_subscriptions","military_expenditures","airports","railways","roadways","waterways","merchant_marine")
factbook

for (i in (1:length(factbook))) {
  if ((sum(is.na(factbook[i]))) >= 40) {
    factbook <- subset(factbook, select = -c(i))
    print(is.numeric(factbook[i]))
  }
}

factbook <- na.omit(factbook)

for (i in (2:length(factbook))) {
  factbook[i] <- as.numeric(unlist(factbook[i]))
}

for (i in (2:length(factbook))) {
  factbook[i] <- scale(factbook[i])
}

gr <- dist(factbook, method = "euclidean")
a <- kmeans(gr, 2)
a

b <- hclust(gr, method = "ward.D")
plot(b)
cutree(b, k = 2)

h <- gclust(factbook)
plot(h)
cutree(h, k = 2)

