## Estimate Pi with Using Monte Carlo Integration

## Recall that the area of a circle is A = pi*r^2
## Thus, a circle of radius 1 has an area of pi
## To estimate pi, we can just approximate the area of a circle with radius 1

## By way of analogy, imagine you are firing a shotgun at a square paper target (say 1 ft x 1 ft)

## But this shotgun sends many tiny pellets at the target UNIFORMLY at random

## This means all parts of the target are equally likely to be hit (uniform scatter)

#####################################################################

set.seed(322)  #this makes it so we get the same random draws

N <- 1000      #number of pellets
x <- runif(N)  #x-axis coordinates where the pellets hit the target
y <- runif(N)  #y-axis coordinates where the pellets hit the target

plot(x,y) #picture of the target after firing

#####################################################################

## Cool! What if we drew a quarter circle on the target?

## Recall that if the center is (0,0) the formula for a circle with radius 1 is 
## x^2 + y^2 = 1, so y = sqrt(1-x^2)

curve(sqrt(1-x^2),add=T)

## The area of a quarter circle with radius 1 is therefore a quarter of pi, pi/4

## Thus, the proportion of shots that fall inside the curve should approximate pi/4

## Remember, x^2+y^2=1 traces out the curve of the circle. 
## If x^2+y^2 < 1, that means the point (x,y) is inside of the circle!
## We can count up the number inside of the circle and divide by the total

sum(x^2 + y^2 < 1)/N  #equivalently, we could use mean(x^2+y^2 < 1)

## so .781 of the dots lie inside the circle

plot(x,y, col = ifelse(x^2 + y^2 < 1, "red","black"))
curve(sqrt(1-x^2),add=T)

## Since this is an estimate of a pi/4, we can just multiply by 4 to estimate pi

4 * sum(x^2 + y^2 < 1)/N 

## Close, but only good to the tenths place. Let's crank up N to 50,000 and rerun it.

#####################################################################

N <- 50000      #number of pellets
x <- runif(N)  #x-axis coordinates where the pellets hit the target
y <- runif(N)  #y-axis coordinates where the pellets hit the target

plot(x,y, col = ifelse(x^2 + y^2 < 1, "red","black"))
curve(sqrt(1-x^2),add=T)

4 * sum(x^2 + y^2 < 1)/N 

## Ah, much better: now we're good to the ten-thousandths place!

#####################################################################

## How does sample size affect the approximation?
## Let's compute the estimate for N=1 up to N=50,000 and plot it.

## This bit of code is a little tricky: See if you can parse it
## It might take a few seconds to run so be patient!
## In effect, you are repeating the process above with N=1, N=2, ... to N=50,000

est <- sapply(1:N, function(i)mean(x[1:i]^2+y[1:i]^2 < 1)*4)

plot(est, type="l")
abline(h=pi, col="red")
