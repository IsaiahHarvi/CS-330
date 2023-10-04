# Project:  CS 330
# Program:  Dynamic movement, 1 Support
# Purpose:  Provide a library of support functions for dynamic movement algorithms.
# Author:   Mikel D. Petty, Ph.D., 256-824-6140, pettym@uah.edu
# Created:  2019-2-9
# Modified: 2022-1-29

# To Do
# - Restore automated testing for closest approach; see earlier versions.

#----------------------------------------------------------------

# Vector and geometry functions

# Calculate length of a 2D vector.

vector.length <- function(v) { return(sqrt(v[1]^2 + v[2]^2)) }

# Normalize a 2D vector.

vector.normalize <- function(v) {
  if (vector.length(v) != 0) {
    return(c(v[1] / vector.length(v), v[2] / vector.length(v)))
  } else {
    return(c(0, 0))
  }
}

# Calculate scalar dot product of two 2D vectors.

vector.dot <- function(A, B) {
  return(sum(A * B))
}

# Convert an orientation (in radians) to a unit vector; see [Millington, 2019] p. 46.

as.vector <- function(omega) {
  result <- c(sin(omega), cos(omega))
  return(result)
}

# Calculate distance between two points in 2D.

distance.point.point <- function(A, B) {
  return(sqrt((B[1] - A[1])^2 + (B[2] - A[2])^2))
}

# Calculate distance from a point to a line in 2D.
# Q is the point; A and B are two distinct points on the line.

distance.point.line <- function(Q, A, B) {
  x0 <- Q[1]; y0 <- Q[2]
  x1 <- A[1]; y1 <- A[2]
  x2 <- B[1]; y2 <- B[2]
  numerator   <- abs(((x2 - x1) * (y1 - y0)) - ((x1 - x0) * (y2 - y1)))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  return(numerator / denominator)
}

# Find point on line closest to query point in 2D.
# Q is the query point, A and B are distinct points on the line, as vectors.
# Source:  https://diego.assencio.com/  Computing the closest point on a segment to a point.

closest.point.line <- function(Q, A, B) {
  T <- vector.dot((Q - A), (B - A)) / vector.dot((B - A), (B - A))
  return(A + (T * (B - A)))
}

# Find point on segment closest to query point in 2D.
# Q is the query point, A and B are segment endpoints, as vectors.
# Source:  https://diego.assencio.com/  Computing the closest point on a segment to a point.

closest.point.segment <- function(Q, A, B) {
  T <- vector.dot((Q - A), (B - A)) / vector.dot((B - A), (B - A))
  if      (T <= 0) { return(A) }
  else if (T >= 1) { return(B) }
  else             { return(A + (T * (B - A))) }
}

# Convert an angle (in radians) to the interval [-pi, pi]

convert.angle <- function(theta) {
  theta <- theta %% (2 * pi)
  if (abs(theta) > pi) {
    theta <- theta - ((2 * pi) * sign(theta))
  }
  return(theta)
}

#----------------------------------------------------------------

# Path functions

# Assemble a complete path data structure from its coordinates.

path.assemble <- function(path.id, path.x, path.y) {
  path.segments <- length(path.x) - 1
  path.distance <- rep(0, path.segments + 1)
  for (i in 2:(path.segments + 1)) {
    path.distance[i] <- path.distance[i - 1] + distance.point.point(c(path.x[i - 1], path.y[i - 1]), c(path.x[i], path.y[i]))
  }
  path.param <- rep(0, path.segments + 1)
  for (i in 2:(path.segments + 1)) {
    path.param[i] <- path.distance[i] / max(path.distance)
  }
  return(list(id=path.id, x=path.x, y=path.y, distance=path.distance, param=path.param, segments=path.segments))
}

# Calculate the position on a path corresponding to a given path parameter.

path.get.position <- function(path, param) {
  i <- max(which(param > path$param))
  A <- c(path$x[i], path$y[i])
  B <- c(path$x[i + 1], path$y[i + 1])
  T <- (param - path$param[i]) / (path$param[i + 1] - path$param[i])
  P <- A + (T * (B - A))
  return(P)
}

# Find the path parameter of the point on the path closest to a given position.

path.get.param <- function(path, position) {

  # Find point on path closest to given position.

  closest.distance <- Inf
  for (i in 1:path$segments) {
    A <- c(path$x[i], path$y[i])
    B <- c(path$x[i + 1], path$y[i + 1])
    check.point    <- closest.point.segment(position, A, B)
    check.distance <- distance.point.point(position, check.point)
    if (check.distance < closest.distance) {
      closest.point    <- check.point
      closest.distance <- check.distance
      closest.segment  <- i
    }
  }

  # Calculate path parameter of closest point; see. p. 70.136.

  A       <- c(path$x[closest.segment], path$y[closest.segment])
  A.param <- path$param[closest.segment]
  B       <- c(path$x[closest.segment + 1], path$y[closest.segment + 1])
  B.param <- path$param[closest.segment + 1]
  C       <- closest.point
  T       <- vector.length(C - A) / vector.length(B - A)
  C.param <- A.param + (T * (B.param - A.param))

  return(C.param)
}

#----------------------------------------------------------------

# Movement functions

# Find time and distance of closest approach, given two moving points.
# Based on formulas in [Millington, 2019] p. 87, with point A as character and point B as target.
# If relative velocity d.v is 0, characters are not approaching each other and will not collide.

closest.approach <- function(A.position, A.velocity, B.position, B.velocity) {
  d.p       <- B.position - A.position
  d.v       <- B.velocity - A.velocity
  if (vector.length(d.v) != 0) {
    closest.t <- -vector.dot(d.p, d.v) / vector.length(d.v)^2
    closest.A <- A.position + (A.velocity * closest.t)
    closest.B <- B.position + (B.velocity * closest.t)
    closest.d <- distance.point.point(closest.A, closest.B)
  } else {
    closest.t <- 0
    closest.A <- A.position
    closest.B <- B.position
    closest.d <- distance.point.point(A.position, B.position)
  }
  return(c(closest.t, closest.d, closest.A, closest.B))
}

#----------------------------------------------------------------

# Mathematics functions

random.binomial <- function() { runif(1, min=0, max=1) - runif(1, min=0, max=1) }

#----------------------------------------------------------------

# Plotting functions

# Plot a circle as a many-sided polygon; assumes a plot is active.

plot.circle <- function(center=c(0, 0), radius=1, sides=64, color="darkgray") {
    sides.x <- (cos((0:sides / sides) * 2 * pi) * radius) + center[1]
    sides.z <- (sin((0:sides / sides) * 2 * pi) * radius) + center[2]
    lines(type="l", x=sides.x, y=sides.z, col=color, lty="dashed", lwd=0.75)
}

#----------------------------------------------------------------

# General support functions

text.out  <- function(msg, textfile, first=FALSE) { write(msg, textfile, ncolumns=1, append=!first) }
num.width <- function(x, left, right) { format(round(x, right),  nsmall=right, width=(left + right + ifelse(right > 0, 1, 0))) }

#----------------------------------------------------------------

# Automated testing for support functions

if (FALSE) {  # Change to FALSE to disable testing.

  support.test <- logical(0)

  # Test case 1 (p. 70.136)
  Q <- c(-6, 3); A <- c(-8, 5); B <- c(-4, 5); Sl <- c(-6, 5); Ss <- c(-6, 5)
  support.test <- c(support.test, Sl == closest.point.line(Q, A, B))
  support.test <- c(support.test, Ss == closest.point.segment(Q, A, B))
  
  # Test case 2 (p. 70.136)
  Q <- c( 3, 3); A <- c( 1, 2); B <- c( 1, 6); Sl <- c( 1, 3); Ss <- c( 1, 3)
  support.test <- c(support.test, Sl == closest.point.line(Q, A, B))
  support.test <- c(support.test, Ss == closest.point.segment(Q, A, B))

  # Test case 3 (p. 70.136)
  Q <- c(6, 0); A <- c( 6, 2); B <- c( 9, 5); Sl <- c( 5, 1); Ss <- c( 6, 2)
  support.test <- c(support.test, Sl == closest.point.line(Q, A, B))
  support.test <- c(support.test, Ss == closest.point.segment(Q, A, B))

  # Test case 4 (p. 70.136)
  Q <- c(-3,-1); A <- c(-8, 1); B <- c(-4, 0); Sl <- c(-2.9,-0.3); Ss <- c(-4, 0)
  support.test <- c(support.test, abs(Sl - closest.point.line(Q, A, B)) < 0.1)
  support.test <- c(support.test, Ss == closest.point.segment(Q, A, B))

  # Test case 5 (p. 70.136)
  Q <- c(-8,-3); A <- c(-7,-3); B <- c(-5,-3); Sl <- c(-8,-3); Ss <- c(-7,-3)
  support.test <- c(support.test, Sl == closest.point.line(Q, A, B))
  support.test <- c(support.test, Ss == closest.point.segment(Q, A, B))

  # Test case 6 (p. 70.136)
  Q <- c( 3,-3); A <- c(-1,-3); B <- c( 2,-3); Sl <- c( 3,-3); Ss <- c( 2,-3)
  support.test <- c(support.test, Sl == closest.point.line(Q, A, B))
  support.test <- c(support.test, Ss == closest.point.segment(Q, A, B))

  # Test case 7 (p. 70.136)
  Q <- c( 8,-3); A <- c( 9,-3); B <- c( 6,-3); Sl <- c( 8,-3); Ss <- c( 8,-3)
  support.test <- c(support.test, Sl == closest.point.line(Q, A, B))
  support.test <- c(support.test, Ss == closest.point.segment(Q, A, B))
  
}

# End of program
