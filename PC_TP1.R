X = mtcars

library(quantreg)

# On crée un modèle de régression médiane mpg ~ wt

model_med = rq(mpg ~ wt, data = X)

# On crée maintenant un modèle de régression linéaire

model_lin = lm(mpg~wt, data = X)

summary(model_med)
summary(model_lin)

plot(mpg~wt, data = X)
abline(reg = model_med, col = "blue")
abline(reg = model_lin, col = "red")

# Les deux modèles "semblent très proches".

# Maintenant, on va comparer le comportement des deux modèles
# Dans le cas où des Outliers forts sont présents.
#On en rajoute donc 3 artificiellement.

X1 = rbind(data.frame(X[1],X[6]), c(0,5), c(60,2), c(50,1.5))
plot(mpg ~ wt, data = X1)

model_med1 = rq(mpg ~ wt, data = X1)

summary(model_med)
summary(model_med1)

(model_med$coefficients-model_med1$coefficients)/model_med$coefficients
# Les coefficients du modèle médian on bougé de 12% pour l'intercept
# et 26% pour wt

model_lin1 = lm(mpg~wt, data = X1)

summary(model_lin)
summary(model_lin1)

(model_lin$coefficients-model_lin1$coefficients)/model_lin$coefficients

#C'est drastiquement plus tranché pour le modèle linéaire

plot(mpg~wt, data = X)
abline(reg = model_med1, col = "blue")
abline(reg = model_lin1, col = "red")



###Régressions quantile multiple

model_quant005 = rq(mpg~wt, tau = 0.05, data = X)$coefficients
model_quant025 = rq(mpg~wt, tau = 0.25, data = X)$coefficients
model_quant075 = rq(mpg~wt, tau = 0.75, data = X)$coefficients
model_quant095 = rq(mpg~wt, tau = 0.95, data = X)$coefficients

x = seq(min(X$wt), max(X$mpg),by = 0.1)

library(ggplot2)
ggplot(data = X,aes(x= wt, y=mpg)) +
  geom_point()+
  geom_abline(aes(intercept = model_quant005[1], slope = model_quant005[2]), color = 'red')+
  geom_abline(aes(intercept = model_quant095[1], slope = model_quant095[2]),color = 'red') +
  geom_abline(aes(intercept = model_med$coefficients[1], slope = model_med$coefficients[2]), color = 'blue')+
  geom_ribbon(aes(x = wt, ymin = model_quant005[2]*wt + model_quant005[1], ymax = model_quant095[2]*wt + model_quant095[1]),alpha=0.2,fill = "steelblue2")
  

ggplot(data = X,aes(x= wt, y=mpg)) +
  geom_point()+
  geom_abline(aes(intercept = model_quant025[1], slope = model_quant025[2]), color = 'red')+
  geom_abline(aes(intercept = model_quant075[1], slope = model_quant075[2]),color = 'red') +
  geom_ribbon(aes(x = wt, ymin = model_quant025[2]*wt + model_quant025[1], ymax = model_quant075[2]*wt + model_quant075[1]),alpha=0.2,fill = "steelblue2")


#Calculons le taux de couverture effectif pour l'intervalle à hauteur de 0.9 :


model_quant005_tx = rq(mpg~wt, tau = 0.05, data = X)
model_quant095_tx = rq(mpg~wt, tau = 0.95, data = X)

tx = mean( (X$mpg<model_quant095_tx$fitted.values) * (X$mpg>model_quant005_tx$fitted.values) )
