load('ME6.Rdata')

ls()

model_linear <- lm(y ~ x, data = data)

#make the model according to the provided form
model_quadratic <- lm(y ~ x + I(x^2), data = data)


anova(model_linear, model_quadratic)


