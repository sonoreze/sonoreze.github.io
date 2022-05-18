#library(rstudioapi)
#library(fBasics)
#https://www.coloringnoise.com/theoretical_background/new-color-scheme/

r <- c(255,130,160,184,206,226,243,232,205,161,117,67)
g <- c(255,166,186,214,228,242,198,126,70,26,8,10)
b <- c(255,173,191,209,204,191,131,77,62,77,92,74)

sound <- function (n, name = c("soundcolors"))
{
  soundcolors = rgb(r, g, b, maxColorValue = 255)
  name = match.arg(name)
  orig = eval(parse(text = name))
  rgb = t(col2rgb(orig))
  temp = matrix(NA, ncol = 3, nrow = n)
  x = seq(0, 1, ,length(orig))
  xg = seq(0, 1, , n)
  for (k in 1:3) {
    hold = spline(x, rgb[, k], n = n)$y
    hold[hold < 0] = 0
    hold[hold > 255] = 255
    temp[, k] = round(hold)
  }
  palette = rgb(temp[, 1], temp[, 2], temp[, 3], maxColorValue = 255)
  palette
}

pal_sound100 <- sound(n=100)
pal_sound10 <- sound(n=10)
palsound100 <- sound(n=100)
palsound10 <- sound(n=10)
# par(mar = rep(0, 4))
#pie(rep(1, 100), col = palsound100)
#
# pal_sound <- sound(n=10)
# par(mar = rep(0, 4))
# pie(rep(1, length(pal2)), col = pal_sound)
