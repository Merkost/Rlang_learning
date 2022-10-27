plot(c(1:10, 20:30, 40:50), main="4/Dotted steps")
plot(cars, main = "4/20s century autos")
plot(trees, main = "4/Trees")
xt <- seq(-1, 1, len = 100)
yt <- tan(xt)
plot(xt, yt, type = "b", main = "tg(x)")

xs <- seq(-pi, pi, len = 100)
ys <- sin(xs)
plot(xs, ys, type = "o", main = "sin(x)")

library(ggplot2)
qplot(xt, yt, main = "tg")

t = seq(0, 10, by = 0.1)
y1 = 1.03*exp(-0.25*t)*sin(0.97*t)
y2 = 1.07*exp(-0.35*t)*sin(0.94*t)
y3 = 1.15*exp(-0.5*t)*sin(0.87*t)
y4 = 0.45*(exp(-0.38*t) - exp(-2.62*t))
y5 = 0.22*(exp(-0.21*t) - exp(-4.80*t))
plot(t, y1, type = "n", main = "Oscillation")
colors = c("blue", "red", "green", "cyan", "magenta")
lines(t, y1, col = colors[1])
lines(t, y2, col = colors[2])
lines(t, y3, col = colors[3])
lines(t, y4, col = colors[4])
lines(t, y5, col = colors[5])
legends = paste0("y", 1:5)
# Создает вектор legends=(“y1”, “y2”, “y3”, “y4”, “y5”)
legend(8, 0.7, legends, lty = "solid", col = colors)
abline(h = 0) # Горизонтальная ось
abline(v = 0) # Вертикальная ось

plot(xs, ys, type = "b", main = "sin(x)", col = "dark red")
abline(h = 0, col = "dark green") # Горизонтальная ось
abline(v = 0, col = "dark blue") # Вертикальная ось
legend(-3, 0.95, "sin(x)", lty = "solid", col = "dark red")
