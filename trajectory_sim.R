#R code for 3D confidence error intervals
install.packages("plotly")
library(plotly)

# Generate Simulated Data Points
x <- seq(-5,5, .1)
y <- seq(0, 10, .1)
y <- cbind(y, sin(y), cos(y))

# Generate Simulated Data Values
z1 = sin(x+y[,1])
z2 = sin((x+y[,1])/2.)
z3 = sin((x+y[,1])/3.)

z <- cbind(z1, z2, z3)
# Generate Simulated Standard Deviations
sd <- sqrt(abs(z) * .05)

n = length(x)
i = seq(0,n-1)
# Create Plots for each of three simulated trajectories
p <- plot_ly(type = 'scatter3d')
for (index in 1:3){
  p <- add_trace(p, x = x, y = y[,index], z = z[,index], mode = 'lines', line = list(width = 8, color=index))
  p <- add_trace(p, type = 'mesh3d',
                 # Setup triangle vertices
                 x = c(x, x),
                 y = c(y[,index] - 2 * sd[,index], y[,index]  + 2 * sd[,index]),
                 z = c(z[,index] , z[,index]),
                 # Create triangles
                 i = c(i[1:n - 1], i[1:n - 1]),
                 j = c(n + i[1:n - 1], n + i[2:n]) ,
                 k = c(n + i[2:n], i[2:n]),
                 color = index
  )
}
p

fir <- cbind(x, y[,1], z[,1])
sec <- cbind(x, y[,2], z[,2])
thi <- cbind(x, y[,3], z[,3])
all <- cbind(fir, sec, thi)
all2 <- rbind(fir, sec, thi)
all_m <- data.matrix(all2)

fir_2 <- data.frame(x, y = y[,1], z = z[,1], group = 1)
sec_2 <- data.frame(x, y = y[,2], z = z[,2], group = 2)
thi_2 <- data.frame(x, y = y[,3], z = z[,3], group = 3)
all3 <- rbind(fir_2, sec_2, thi_2)

install.packages("fpc")
library(fpc)
install.packages("scatterplot3d")
library(scatterplot3d)

#dbscan
cluster <- dbscan(all_m[,c(1,3)], eps=5, MinPts=100)
plot(cluster, all_m)
plot(cluster, all_m[,c(1,3)])

#scatterplot
scatterplot3d(all_m[,1], all_m[,2], all_m[,3], highlight.3d = TRUE, col.axis = "blue",col.grid = "lightblue", main = "Simulated Routes", pch = 20)
scatterplot3d(all3[,1], all3[,2], all3[,3], main = "Simulated Routes", pch = 20, color = all3$group)


#END