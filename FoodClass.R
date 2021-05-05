df<-read.csv("C:/Users/RX/Desktop/last/train.csv")
df$FoodGroup <- as.factor(df$FoodGroup)
df$Descrip <- as.factor(df$Descrip)
summary(df)
####PLOT
library(ggplot2)
library(plotly)
plot_ly(as.data.frame(table(df$FoodGroup)),
x = ~Var1,
y = ~Freq,
marker = list(color = "rgb(60,179,113)")) %>%
layout( title = "Histogram corresponding to the number of foods in each group")%>%
layout(xaxis = list(title = "Names of food groups"), yaxis = list(title = "Nombre of food"))
#####To numeric
library(purrr)
df3 <- df$FoodGroup %>% map_if(is.factor, as.numeric)
str(df3)
#View(df3)
fd <- as.integer(df3)
df$FoodGroup <- fd
df <- df[-c(1)]
df <- df[-c(2)]
View(df)
######################
##Linear regression
#####################
energie <- df$Energy_kcal
sugar <- df$Sugar_g
fat <- df$Fat_g
carb <- df$Carb_g
folat <- df$Folate_mcg
vitamineA <- df$VitA_mcg
vitamineB <- df$VitB12_mcg
model1 <- lm(energie ~ fat)
model2 <- lm(energie ~ carb)
model3 <- lm(vitamineB ~ folat)
model4 <- lm(carb ~ sugar)
model5 <- lm(vitamineA ~ vitamineB)
model <- lm(FoodGroup~., data=df)
#summary(model1)
#summary(model2)
summary(model3)
#summary(model4)
#summary(model5)
#summary(model)
#### energie~ fat
ggplot(df, aes(x = fat, y = energie,color=dfn$FoodGroup)) +
geom_point() +
stat_smooth(method = "lm", col = "red")
#############energie ~ carb
ggplot(df, aes(x = carb, y = energie,color=dfn$FoodGroup)) +
geom_point() +
stat_smooth(method = "lm", col = "red")
#############vitamineB ~ folat
ggplot(df, aes(x = folat, y = vitamineB,color=dfn$FoodGroup)) +
geom_point() +
stat_smooth(method = "lm", col = "red")
############carb ~ sugar
ggplot(df, aes(x = sugar, y = carb,color=dfn$FoodGroup)) +
geom_point() +
stat_smooth(method = "lm", col = "red")
#############vitamineA ~ vitamineB
ggplot(df, aes(x = vitamineB, y = vitamineA,color=dfn$FoodGroup)) +
geom_point() +
stat_smooth(method = "lm", col = "red")
res1 <- summary(model1)
pc<- predict(model1, data=poum)
################ #pour obtenir AIC et BIC #############
AIC(model1)
BIC<-AIC(model1, k=log(dim(df)[1]))
BIC
AIC(model2)
BIC<-AIC(model2, k=log(dim(df)[1]))
BIC
AIC(model3)
BIC<-AIC(model3, k=log(dim(df)[1]))
BIC
AIC(model4)
BIC<-AIC(model4, k=log(dim(df)[1]))
BIC
AIC(model5)
BIC<-AIC(model5, k=log(dim(df)[1]))
BIC
###########################linear model all variables
model <- lm(FoodGroup ~., data=df)
summary(model)
AIC(model)
BIC<-AIC(model, k=log(dim(df)[1]))
BIC
modl$coef
pc<- predict(modl, data=fd)
RSS2<-sum((pc-df$fd)^2)
RSS2
#####################
library(leaps)
##using the seqrep method "exhaustive"
modelb<- regsubsets(FoodGroup~. , data=df,nvmax=6, method="seqrep")
sb<-summary(modelb)
sb$cp
sb$bic
sb$adjr2
#without the H variables
###############################################
model_a <- lm(df$FoodGroup ~ df$Energy_kcal+df$Protein_g+df$Fat_g+df$Carb_g+df$Sugar_g
+df$Fiber_g+df$VitB12_mcg+df$Folate_mcg+df$Riboflavin_mg+df$Thiamin_mg+df$Copper_USRDA
+df$Iron_mg+df$Magnesium_mg+df$Manganese_mg+df$Phosphorus_mg+df$Zinc_mg, data=df)
aic<-AIC(model_a)
BIC<-AIC(model_a, k=log(dim(df)[1]))
print(paste(’The AIC of the model is ’, aic))
print(paste(’The BIC of the model is ’, BIC))
aic<-AIC(model1)
BIC<-AIC(model1, k=log(dim(df)[1]))
print(’energy~ fat’)
print(paste(’The AIC of the model is ’, aic))
print(paste(’The BIC of the model is ’, BIC))
aic<-AIC(model2)
BIC<-AIC(model2, k=log(dim(df)[1]))
print(’energy ~ carb’)
print(paste(’The AIC of the model is ’, aic))
print(paste(’The BIC of the model is ’, BIC))
aic<-AIC(model3)
BIC<-AIC(model3, k=log(dim(df)[1]))
print(’vitamineB ~ folat’)
print(paste(’The AIC of the model is ’, aic))
print(paste(’The BIC of the model is ’, BIC))
aic<-AIC(model4)
BIC<-AIC(model4, k=log(dim(df)[1]))
print(’carb ~ sugar’)
print(paste(’The AIC of the model is ’, aic))
print(paste(’The BIC of the model is ’, BIC))
aic<-AIC(model5)
BIC<-AIC(model5, k=log(dim(df)[1]))
print(’vitamin A ~ vitamin B’)
print(paste(’The AIC of the model is ’, aic))
print(paste(’The BIC of the model is ’, BIC))
#########################################
##model with the lowest significant variables
modele <- lm(FoodGroup~ Energy_kcal+Protein_g+Fat_g+Carb_g+Sugar_g+Fiber_g+VitA_mcg
+VitB6_mg+Iron_mg+ VitB12_mcg+VitC_mg+VitE_mg+Folate_mcg+Niacin_mg+Riboflavin_mg
+Thiamin_mg+Calcium_mg+Copper_mcg+Iron_mg+Magnesium_mg+Manganese_mg+Phosphorus_mg
+Selenium_mcg+Zinc_mg, data = df)
##anova() to choose the best model
anova(model_a,modele)
anova(model_a,model)
anova(modele,model)
##############################
##Correlation
##############################
library("corrplot")
M <- as.matrix(df)
R<-cor(M)
corrplot(R[-c(11:38),-c(11:38)], is.corr=FALSE)
######################################
########RIDGE REGRESSION
######################################
##### the command gives coeffcients of the linear model
library(lmridge)
lmr <- lmridge(FoodGroup~ Energy_kcal+Protein_g+Fat_g+Carb_g+Sugar_g+Fiber_g+VitA_mcg
+VitB6_mg+Iron_mg+ VitB12_mcg+VitC_mg+VitE_mg+Folate_mcg+Niacin_mg+Riboflavin_mg
+Thiamin_mg+Calcium_mg+Copper_mcg+Iron_mg+Magnesium_mg+Manganese_mg+Phosphorus_mg
+Selenium_mcg+Zinc_mg, data = df,lambda=1)
lmr$coef
#########################################
#############ACP
#########################################
library("FactoMineR")
library("factoextra")
pcc<- PCA(df)
#print(pcc)
fviz_eig(pcc, addlabels = TRUE, ylim = c(0, 30))
fviz_pca_var(pcc, col.var = colnames(df))
fviz_pca_ind(pcc, geom.ind = "point", habillage = 1, palette = rainbow(25),
legend.title = "Food group",title = "Projection of individuals on the first
2 main components")
#########################################
########TEST DATASET
#########################################
library(purrr)
df_test<-read.csv("C:/Users/RX/Desktop/last/test.csv")
df_test <- df_test[-c(1)]
df_test <- df_test[-c(2)]
df_t <- df_test
df_test$FoodGroup <- as.factor(df_test$FoodGroup)
#df_test$Descrip <- as.factor(df_test$Descrip)
df_test1 <- df_test$FoodGroup %>% map_if(is.factor, as.numeric)
str(df_test1)
fd_test <- as.integer(df_test1)
df_test$FoodGroup <- fd_test
View(df_test)
#########################################
########SVM
#########################################
library(e1071)
#, type = "C-classification"
mod = svm(FoodGroup ~ ., data = df)
y_pd<-predict(mod, df_test)
levels(y_pd) = c("American Indian/Alaska Native Foods","Baby Foods","Baked Products",
"Beef Products","Beverages","Breakfast Cereals","Cereal Grains and Pasta",
"Dairy and Egg Products","Fast Foods","Fats and Oils","Fin fish and Shellfish Products",
"Fruits and Fruit Juices","Lamb, Veal, and Game Products","Legumes and Legume Products",
"Meals, Entrees, and Side Dishes","Nut and Seed Products","Pork Products","Poultry Products"
,"Restaurant Foods","Sausages and Luncheon Meats","Snacks","Soups, Sauces, and Gravies",
"Spices and Herbs","Sweets","Vegetables and Vegetable Products")
data_pred <- cbind(y_pd, df_t)
data_pred[c(60:80), c(1,2)]
###Accuracy of SVM model
table_mat <- table(df_test$FoodGroup, y_pd)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste(’Accuracy for test’, accuracy_Test))
#########################################
########DECISON TREE##############
#########################################
library(rpart)
dec_mod <- rpart(FoodGroup~., data = df, method = ’class’)
y_prddt <- predict(dec_mod, df_test, type = ’class’)
levels(y_prddt) = c("American Indian/Alaska Native Foods","Baby Foods","Baked Products",
"Beef Products","Beverages","Breakfast Cereals","Cereal Grains and Pasta",
"Dairy and Egg Products","Fast Foods","Fats and Oils","Fin fish and Shellfish Products",
"Fruits and Fruit Juices","Lamb, Veal, and Game Products","Legumes and Legume Products",
"Meals, Entrees, and Side Dishes","Nut and Seed Products","Pork Products",
"Poultry Products","Restaurant Foods","Sausages and Luncheon Meats","Snacks",
"Soups, Sauces, and Gravies","Spices and Herbs","Sweets",
"Vegetables and Vegetable Products")
data_predtr <- cbind(y_prddt, df_t)
data_predtr[c(120:180), c(1,2)]
###Accuracy of DECISON TREE model
table_mat <- table(df_test$FoodGroup, y_prddt)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste(’Accuracy for test’, accuracy_Test))
#####################################
####Random forest
#####################################
library(randomForest)
library(caTools)
#ntree=
rf <- randomForest(FoodGroup ~ .,data=df, trees=100)
y_prd<-predict(rf, newdata=df_test[-c(1)])
y_prd<- round(y_prd)
# lavels(y_prd) = c("American Indian/Alaska Native Foods","Baby Foods","Baked Products",
"Beef Products","Beverages","Breakfast Cereals","Cereal Grains and Pasta",
"Dairy and Egg Products","Fast Foods","Fats and Oils","Fin fish and Shellfish Products",
"Fruits and Fruit Juices","Lamb, Veal, and Game Products","Legumes and Legume Products",
"Meals, Entrees, and Side Dishes","Nut and Seed Products","Pork Products",
"Poultry Products","Restaurant Foods","Sausages and Luncheon Meats","Snacks",
"Soups, Sauces, and Gravies","Spices and Herbs","Sweets",
"Vegetables and Vegetable Products")
data_pred <- cbind(y_prd, df_test)
data_pred[c(60:120), c(1,2)]
###Accuracy of random forest model
table_mat <- table(df_test$FoodGroup, y_prd)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste(’Accuracy for test’, accuracy_Test))
#####################################
####Regression Ridge
#####################################
library(lmridge)
lmr <- lmridge(FoodGroup~ Energy_kcal+Protein_g+Fat_g+Carb_g+Sugar_g+Fiber_g+VitA_mcg
+VitB6_mg+Iron_mg+ VitB12_mcg+VitC_mg+VitE_mg+Folate_mcg+Niacin_mg+Riboflavin_mg
+Thiamin_mg+Calcium_mg+Copper_mcg+Iron_mg+Magnesium_mg+Manganese_mg+Phosphorus_mg
+Selenium_mcg+Zinc_mg, data = df,lambda=1)
y_prdlmr<-predict(lmr, newdata=df_test[-c(1)])
y_prdlmr<- round(y_prdlmr)
data_pred <- cbind(y_prdlmr, df_test)
data_pred[c(160:220), c(1,2)]
##Accuracy of regression Ridge model
table_mat <- table(df_test$FoodGroup, y_prdlmr)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste(’Accuracy for test’, accuracy_Test))
