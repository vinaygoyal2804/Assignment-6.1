#Import the Titanic Dataset

par(mar=c(1,1,1,1))

Full<- titanic3

#Q1. Pre-process the passenger names to come up with a list of titles that represent families and represent using appropriate visualization graph.
titanic3$Title <- gsub('(.*, )|(\\..*)', '', titanic3$name)
View(titanic3)
table(titanic3$sex, titanic3$Title)

#Q2. Represent the proportion of people survived by family size using a graph.
titanic3$Fsize <- titanic3$sibsp + titanic3$parch + 1
titanic3$Family <- paste(titanic3$surname, titanic3$Fsize, sep='_')
ggplot(titanic3[1:891,], aes(x = Fsize, fill = factor(survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size')

#Q3. Impute the missing values in Age variable using Mice library,create two different graphs showing Age distribution before and after imputation
sum(is.na(titanic3$age))
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')
help("lapply")

titanic3[factor_vars] <- lapply(titanic3[factor_vars], function(x) as.factor(x))
# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf')
# Save the complete output 
mice_output <- complete(mice_mod)
# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))