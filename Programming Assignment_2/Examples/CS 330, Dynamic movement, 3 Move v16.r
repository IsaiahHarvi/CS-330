# Project:  CS 330
# Program:  Dynamic movement, 3 Move
# Purpose:  Provide example implementations of dynamic movement algorithms.
# Author:   Mikel D. Petty, Ph.D., 256-824-6140, pettym@uah.edu
# Source:   I. Millington, Artificial Intelligence for Games, Third Edition, CRC Press, Boca Raton FL, 2019.
# Created:  2019-1-29
# Modified: 2022-1-29

# To Do
# - Investigate attraction of Wander movement towards UR and LL corners; orientation > 2pi?
# - Investigate scenarios/behaviors where character exceeds maximums; scenarios 5, 6, 7, 8, 24

#----------------------------------------------------------------

# Define dynamic movement functions, aka steering behaviors.

# Continue; continue moving without changing velocity or orientation.

dynamic.get.steering.continue <- function(mover) {
  result <- list(linear=mover$linear, angular=mover$angular)
  return(result)
}

# Stop; bring character to a stop, with slowing limited by character's maximum acceleration.

dynamic.get.steering.stop <- function(mover) {
  result          <- list(linear=c(0, 0), angular=0)
  result$linear   <- -mover$velocity

  if (vector.length(result$linear) > mover$max.linear) {
    result$linear <- vector.normalize(result$linear)
    result$linear <- result$linear * mover$max.linear
  }
  result$angular  <- -mover$rotation
  return(result)
}

# Align; match orientation to orientation of target.

dynamic.get.steering.align <- function(mover, target) {
  result   <- list(linear=c(0, 0), angular=0)
  rotation <- target$orientation - mover$orientation
  rotation <- convert.angle(rotation)
  if (abs(rotation) < mover$align.radius) {
    result$angular <- -result$angular
  }
  if (abs(rotation) > mover$align.slow) {
    align.rotation <- mover$max.rotation
  } else {
    align.rotation <- mover$max.rotation * abs(rotation) / mover$align.slow
  }
  align.rotation <- align.rotation * sign(rotation)
  result$angular <- (align.rotation - mover$rotation) / mover$align.time
  if (abs(result$angular) > mover$max.angular) {
    result$angular <- mover$max.angular * sign(result$angular)
  }
  return(result)
}

# Face target; match orientation to direction of target.
# Face in [Millington, 2019].

dynamic.get.steering.face.target <- function(mover, target) {
  result    <- list(linear=c(0, 0), angular=0)
  direction <- target$position - mover$position
  if (vector.length(direction) == 0) {
    return(result)
  }
  target$orientation <- atan2(direction[2], direction[1])  
  return(dynamic.get.steering.align(mover, target))  
}

# Face movement; match orientation to direction of movement.
# LookWhereYoureGoing in [Millington, 2019].

dynamic.get.steering.face.movement <- function(mover) {
  result <- list(linear=c(0, 0), angular=0)
  if (vector.length(mover$velocity) == 0) {
    return(result)
  }
  target <- mover
  target$orientation <- atan2(mover$velocity[2], mover$velocity[1])
  return(dynamic.get.steering.align(mover, target))
}

# Seek; move directly towards target as fast as possible.

dynamic.get.steering.seek <- function(mover, target) {
  result         <- list(linear=c(0, 0), angular=0)
  result$linear  <- target$position - mover$position
  result$linear  <- vector.normalize(result$linear)
  result$linear  <- result$linear * mover$max.linear
  result$angular <- 0
  return(result)
}

# Flee;  move directly away from target as fast as possible.

dynamic.get.steering.flee <- function(mover, target) {
  result         <- list(linear=c(0, 0), angular=0)
  result$linear  <- mover$position - target$position
  result$linear  <- vector.normalize(result$linear)
  result$linear  <- result$linear * mover$max.linear
  result$angular <- 0
  return(result)
}

# Arrive; move directly towards target, slowing down when near.

dynamic.get.steering.arrive <- function(mover, target) {
  result    <- list(linear=c(0, 0), angular=0)
  direction <- target$position - mover$position
  distance  <- vector.length(direction)
 
  if (distance < mover$arrive.radius) { 
    arrive.speed  <- 0
  } else if (distance > mover$arrive.slow) {
    arrive.speed  <- mover$max.velocity
  } else {
    arrive.speed  <- mover$max.velocity * distance / mover$arrive.slow
  }
 
  arrive.velocity <- vector.normalize(direction) * arrive.speed
  result$linear   <- arrive.velocity - mover$velocity
  result$linear   <- result$linear / mover$arrive.time

  if (vector.length(result$linear) > mover$max.linear) {
    result$linear <- vector.normalize(result$linear)
    result$linear <- result$linear * mover$max.linear
  }
  return(result)
}

# Pursue; move towards target's predicted future position.

dynamic.get.steering.pursue <- function(mover, target) {
  direction <- target$position - mover$position
  distance  <- vector.length(direction)
  speed     <- vector.length(mover$velocity)
  if (speed <= (distance / mover$max.prediction)) {
    prediction <- mover$max.prediction
  } else {
    prediction <- distance / speed
  }
  seek.target          <- target
  seek.target$position <- seek.target$position + (target$velocity * prediction)
  return(dynamic.get.steering.seek(mover, seek.target))
}

# Wander; move randomly, in a seemingly purposeful manner.

dynamic.get.steering.wander <- function(mover) {
  result                   <- list(linear=c(0, 0), angular=0)
  target                   <- character.0
  mover$wander.orientation <- mover$wander.orientation + (random.binomial() * mover$wander.rate)
  target$orientation       <- mover$wander.orientation + mover$orientation
  wander.center            <- mover$position + (mover$wander.offset * as.vector(mover$orientation))
  target$position          <- mover$wander.radius * as.vector(target$orientation)
  result                   <- dynamic.get.steering.face.target(mover, target)
  result$linear            <- mover$max.linear * as.vector(mover$orientation)
  return(result)
}

# Follow path; move along a given path.

dynamic.get.steering.follow.path <- function(mover, path) {
  current.param   <- path.get.param(path, mover$position)
  target.param    <- min(1, current.param + mover$path.offset)
  target.position <- path.get.position(path, target.param)
  target          <- list(position=target.position)
  return(dynamic.get.steering.seek(mover, target))
}

# Separate; move away from too-close characters.

dynamic.get.steering.separate <- function(mover) {
  result <- list(linear=c(0, 0), angular=0)
  for (i in 1:characters) {
    if (mover$id != Character[[i]]$id) {
      direction <- mover$position - Character[[i]]$position
      distance  <- vector.length(direction)
      if (distance <= mover$sep.threshold) {
        strength      <- min(mover$sep.decay / (distance * distance), mover$max.linear)
        direction     <- vector.normalize(direction)
        result$linear <- result$linear + (strength * direction)
      }
    }
  }
  return(result)
}

# Avoid collisions; move while anticipating other characters' future positions to avoid collisions.

dynamic.get.steering.avoid.collisions <- function(i, mover, lookahead) {
  col.found   <- FALSE       # Is a collision expected for mover?
  col.time    <- Inf         # Time of earliest expected collision
  col.rel.pos <- numeric(0)  # Relative position at time of collision

  # Determine if character is expected to collide with any other character.

  for (j in (1:characters)[-i]) {
    target          <- Character[[j]]
    col.clo.app     <- closest.approach(mover$position, mover$velocity, target$position, target$velocity)
    col.with.target <- (col.clo.app[1] > 0) &&                                        # Closest approach time in the future
                       (col.clo.app[1] < col.time) &&                                 # Closest approach time earliest so far
                       (col.clo.app[1] < lookahead) &&                                # Closest approach time within lookahead time
                       (col.clo.app[2] < (mover$avoid.radius + target$avoid.radius))  # Closest approach distance too close
    if (col.with.target) {
      col.found   <- TRUE                                 # New earliest collision found
      col.time    <- col.clo.app[1]                       # Collision with target predicted at this time
      col.rel.pos <- col.clo.app[3:4] - col.clo.app[5:6]  # Relative position at time of collision
    }
  }

  # Calculate steering based on collision target, if any.

  result  <- list(linear=c(0, 0), angular=0)
  if (col.found) {
    col.rel.pos   <- vector.normalize(col.rel.pos)
    result$linear <- col.rel.pos * mover$max.linear
  }
  return(result)
}

# Swirl; move along a "swirling" trajectory; for movement examples only.

dynamic.get.steering.swirl <-function(mover) {
  result         <- list(linear=c(0, 0), angular=0)
  result$linear  <- c(mover$position[1] / -100 * delta.time, mover$position[2] / -100 * delta.time) * mover$swirl.scale
  result$angular <- mover$angular
  return(result)
}

#----------------------------------------------------------------

dynamic.update <- function(mover, steering, delta.time, physics) {
  if (physics) {  # High School physics
    half.t.sq   <- 0.5 * delta.time * delta.time
    mover$position    <- mover$position    + (mover$velocity   * delta.time) + (steering$linear  * half.t.sq)
    mover$orientation <- mover$orientation + (mover$rotation   * delta.time) + (steering$angular * half.t.sq)
    
  } else {  # Newton-Euler-1 integration
    mover$position    <- mover$position    + (mover$velocity   * delta.time)
    mover$orientation <- mover$orientation + (mover$rotation   * delta.time)
  }
  mover$orientation   <- mover$orientation %% (2 * pi)
  
  mover$velocity      <- mover$velocity    + (steering$linear  * delta.time)
  mover$rotation      <- mover$rotation    + (steering$angular * delta.time)
  
  mover$linear        <- steering$linear
  mover$angular       <- steering$angular

  if (vector.length(mover$velocity) < stop.velocity) { mover$velocity <- c(0, 0) }  # Stop moving at very low velocities; avoids jitter

  if (vector.length(mover$velocity) > mover$max.velocity) {
    if (warnings) {
      cat("character exceeded max velocity",
          "scenario=",           scenario,
          "mover$id=",           mover$id,
          "mover$max.velocity=", mover$max.velocity,
          "mover$velocity=",     mover$velocity,
          "\n")
    }
    mover$velocity <- mover$max.velocity * vector.normalize(mover$velocity)
  }  
  if (vector.length(mover$linear) > mover$max.linear) {
    if (warnings) {
      cat("character exceeded max linear  ",
          "scenario=",           scenario,
          "mover$id=",           mover$id,
          "mover$max.linear=",   mover$max.linear,
          "mover$linear=",       mover$linear,
          "\n")
    }
    mover$linear   <- mover$max.linear * vector.normalize(mover$linear)
  } 
  if (abs(mover$rotation) > mover$max.rotation) {
    if (warnings) {
      cat("character exceeded max rotation",
          "scenario=",           scenario,
          "mover$id=",           mover$id,
          "mover$max.rotation=", mover$max.rotation,
          "mover$rotation=",     mover$rotation,
          "\n")
    }
    mover$rotation <- mover$max.rotation * sign(mover$rotation)
  }
  if (abs(mover$angular) > mover$max.angular) {
    if (warnings) {
      cat("character exceeded max angular",
          "scenario=",           scenario,
          "mover$id=",           mover$id,
          "mover$max.angular=",  mover$max.angular,
          "mover$angular=",      mover$angular,
          "\n")
    }
    mover$angular  <- mover$max.angular * sign(mover$angular)
  }

  return(mover)
}

#----------------------------------------------------------------

# Write initial positions and movement variables for all characters to trajectory file.

for (i in 1:characters) {
  char.out <- paste(Time,
                    Character[[i]]$id,
                    Character[[i]]$position[1], Character[[i]]$position[2],
                    Character[[i]]$velocity[1], Character[[i]]$velocity[2],
                    Character[[i]]$linear[1],   Character[[i]]$linear[2],
                    Character[[i]]$orientation,
                    Character[[i]]$steer,
                    Character[[i]]$col.collided,
                    sep=",")
  text.out(char.out, trajectory.file, first=(i == 1))
}

# Calculate trajectory, timestep by timestep.

while (Time < stop.time) {
  Time <- Time + delta.time  # Increment time by the timestep duration

  # For each timestep, for each character, update the character as follows:
  # 1. Call the character's steering (movement) behavior to get linear and angular accelerations.
  # 2. Update the character's position, orientation, velocity, and rotation
  #    using the linear and angular accelerations returned by the steering behavior.
  # 3. If collision detection active for the scenario, determine if any characters have collided.
  # 4. Write the character's updated data to the output trajectory file.
  
  for (i in 1:characters) {
  
    # Select and call a steering behavior.

     if (Character[[i]]$steer == CONTINUE) {
      steering <- dynamic.get.steering.continue(Character[[i]])

     } else if (Character[[i]]$steer == STOP) {
      steering <- dynamic.get.steering.stop(Character[[i]])       
      
    } else if (Character[[i]]$steer == ALIGN) {
      target   <- Character[[i]]$target
      steering <- dynamic.get.steering.align(Character[[i]], Character[[target]])
      
    } else if (Character[[i]]$steer == FACE.TARGET) {
      target   <- Character[[i]]$target
      steering <- dynamic.get.steering.face.target(Character[[i]], Character[[target]])
      
    } else if (Character[[i]]$steer == FACE.MOVEMENT) {
      steering <- dynamic.get.steering.face.movement(Character[[i]])  
    
    } else if (Character[[i]]$steer == SEEK) {
      target   <- Character[[i]]$target
      steering <- dynamic.get.steering.seek(Character[[i]], Character[[target]])

    } else if (Character[[i]]$steer == FLEE) {
      target   <- Character[[i]]$target
      steering <- dynamic.get.steering.flee(Character[[i]], Character[[target]])

    } else if (Character[[i]]$steer == ARRIVE) {
      target   <- Character[[i]]$target
      steering <- dynamic.get.steering.arrive(Character[[i]], Character[[target]])

    } else if (Character[[i]]$steer == PURSUE) {
      target   <- Character[[i]]$target
      steering <- dynamic.get.steering.pursue(Character[[i]], Character[[target]])

    } else if (Character[[i]]$steer == WANDER) {
      steering <- dynamic.get.steering.wander(Character[[i]])

    } else if (Character[[i]]$steer == FOLLOW.PATH) {
      path     <- Character[[i]]$path.to.follow
      steering <- dynamic.get.steering.follow.path(Character[[i]], Path[[path]])

    } else if (Character[[i]]$steer == SEPARATE) {
      steering <- dynamic.get.steering.separate(Character[[i]])

    } else if (Character[[i]]$steer == AVOID.COLLISIONS) {
      target   <- Character[[i]]$target
      steering <- dynamic.get.steering.avoid.collisions(i, Character[[i]], delta.time * Character[[i]]$col.lookahead)
    
    } else if (Character[[i]]$steer == SWIRL) {
      steering <- dynamic.get.steering.swirl(Character[[i]])
    }   

    # Update the character's movement variables.

    Character[[i]]         <- dynamic.update(Character[[i]], steering, delta.time, physics)
  }
      
  # Check whether any characters have collided; if so, immediately stop both.

  if (check.collisions) {
    for (i in 1:(characters - 1)) {
      for (j in (i + 1):characters) {
        if (!Character[[i]]$col.collided || !Character[[j]]$col.collided) {
          col.distance <- distance.point.point(Character[[i]]$position, Character[[j]]$position)
          col.radii    <- Character[[i]]$col.radius + Character[[j]]$col.radius
          if (col.distance <= col.radii) {
            col.position <- (Character[[i]]$position + Character[[j]]$position) / 2
            for (k in c(i, j)) {
              Character[[k]]$position     <- col.position
              Character[[k]]$velocity     <- c(0, 0)
              Character[[k]]$linear       <- c(0, 0)
              Character[[k]]$rotation     <- 0
              Character[[k]]$angular      <- 0
              Character[[k]]$steer        <- STOP
              Character[[k]]$col.collided <- TRUE
            }
          }
        }
      }
    }
  }

  # Write updated positions and movement variables for each character to trajectory file.

  for (i in 1:characters) {
    char.out <- paste(Time,
                      Character[[i]]$id,
                      Character[[i]]$position[1], Character[[i]]$position[2],
                      Character[[i]]$velocity[1], Character[[i]]$velocity[2],
                      Character[[i]]$linear[1],   Character[[i]]$linear[2],
                      Character[[i]]$orientation,
                      Character[[i]]$steer,
                      Character[[i]]$col.collided,
                      sep=",")
    text.out(char.out, trajectory.file, first=FALSE)
  }
}

# End of program
