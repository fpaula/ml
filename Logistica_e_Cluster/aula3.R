#install.packages("tidyverse", dependencies = T)
#install.packages("modelr", dependencies = T)
#install.packages("broom", dependencies = T)
#install.packages("ROCR", dependencies = T)
#install.packages("ISLR", dependencies = T)
#install.packages("pscl", dependencies = T)


library("tidyverse")
library("modelr")
library("broom")
library("ROCR")
library("ISLR")
library("pscl")

(default <- as_tibble(ISLR::Default))

set.seed(123)

sample <- sample(c(TRUE, FALSE), nrow(default),
                 replace = TRUE, prob = c(0.6, 0.4))

train <- default[sample, ]
test <- default[!sample, ]

model1 <- glm(default ~ balance, family = "binomial", data = train)

default %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) + ggtitle("Logistic regression model fit" ) +
  xlab("Balance") +
  ylab("Probability of Default")

summary(model1)
tidy(model1)

# aplica o modelo
predict(model1, data.frame(balance = c(1000, 2000)),
        type = "response")


model2 <- glm(default ~ student, family = "binomial", data = train)
tidy(model2)

predict(model2, data.frame(student = factor(c("Yes", "No"))), type = "response")


model3 <- glm(default ~ balance + income + student, family = "binomial", data = train)
tidy(model3)

new.df <- tibble(balance = 1500, income = 40, student = c("Yes", "No"))
predict(model3, new.df, type = "response")

test.predicted.m1 <- predict(model1, newdata = test, type = "response")
test.predicted.m2 <- predict(model2, newdata = test, type = "response")
test.predicted.m3 <- predict(model3, newdata = test, type = "response")

list(model1 = pscl::pR2(model1)["McFadden"], model2 = pscl::pR2(model2)["McFadden"], model3 = pscl::pR2(model3)["McFadden"])

test %>%
  mutate(m1.pred = ifelse(test.predicted.m1 > 0.5, "Yes", "No"),
         m2.pred = ifelse(test.predicted.m2 > 0.5, "Yes", "No"),
         m3.pred = ifelse(test.predicted.m3 > 0.5, "Yes", "No")) %>% summarise(m1.error = mean(default != m1.pred),
                                                                               m2.error = mean(default != m2.pred), m3.error = mean(default != m3.pred))

par(mfrow=c(1, 2))
prediction(test.predicted.m1, test$default) %>% performance(measure = "tpr", x.measure = "fpr") %>% plot()
prediction(test.predicted.m2, test$default) %>% performance(measure = "tpr", x.measure = "fpr") %>% plot()


prediction(test.predicted.m1, test$default) %>% performance(measure = "auc") %>%
  .@y.values
prediction(test.predicted.m2, test$default) %>% performance(measure = "auc") %>%
  .@y.values
