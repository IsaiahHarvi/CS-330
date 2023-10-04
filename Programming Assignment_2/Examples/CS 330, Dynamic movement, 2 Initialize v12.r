# Project:  CS 330
# Program:  Dynamic movement, 2 Initialize
# Purpose:  Initialize dynamic movement algorithms.
# Author:   Mikel D. Petty, Ph.D., 256-824-6140, pettym@uah.edu
# Created:  2019-2-16
# Modified: 2022-2-15

# Scenario  Description
#     1     Seek and Flee; Seek orbits stationary target
#     2     Seek and Flee; Seek orbits stationary target
#     3     Arrive and Flee; same initial conditions as scenario 1
#     4     Arrive and Flee; same initial conditions as scenario 2
#     5     Seek and Pursue; Pursue's trajectory more efficient; revised 22S
#     6     Seek and Pursue; Pursue's trajectory overshoots moving target; revised 22S
#     7     Path following; slower characters with narrower turns
#     8     Path following; faster characters with wider turns and orbit
#     9     Collision avoidance; no avoidance, 3 collisions
#    10     Collision avoidance; collision lookahead 100 time steps
#    11     Collision avoidance; collision lookahead 10 time steps
#    12     Crossing traffic, without collision avoidance
#    13     Crossing traffic, with collision avoidance
#    14     Separate
#    15     Separate and Continue
#    16     21S Programming Assignment 1
#    17     21S Programming Assignment 2
#    18     21S Midterm trajectories 1 and 2 (Seek and Arrive)
#    19     21S Midterm trajectories 3 and 4 (Avoid collisions, Pursue)
#    20     22S Lecture 3, Seek and Pursue comparison, Seek
#    21     22S Lecture 3, Seek and Pursue comparison, Pursue
#    22     22S Lecture 4, NE1 and HS comparison
#    23     22S Test dynamic Align, Face target, and Face movement
#    24     Wander
#    25     Path following with walls
#    26     22S Program 1 scenario (character data removed in posted version)
#    27     22S Program 2 scenario (character data removed in posted version)
#    28     22S Midterm 1, Seek, Arrive, Wander
#    29     22S Midterm 1, Continue, Avoid collisions, Pursue)
#    30     22S Midterm 1, manually calculate position and acceleration

# Initialize steering behavior constants.

CONTINUE         <-  1
STOP             <-  2
ALIGN            <-  3
FACE.TARGET      <-  4
FACE.MOVEMENT    <-  5
SEEK             <-  6
FLEE             <-  7
ARRIVE           <-  8
PURSUE           <-  9
WANDER           <- 10
FOLLOW.PATH      <- 11
SEPARATE         <- 12
AVOID.COLLISIONS <- 13
SWIRL            <- 14

# Initialize general movement parameters and working variables.

Time             <- 0     # Current simulated time
stop.velocity    <- 0.02  # Stop moving at velocities below this; avoids jitter

# Initialize generic character.
# This character has all character variables used by any scenario, initialized to default values.
# Scenarios initialize characters by copying this character and then changing only the scenario-specific variables.

character.0 <- list(id                 =0,
                    steer              =STOP, 
                    position           =c(0, 0),
                    velocity           =c(0, 0),
                    linear             =c(0, 0),
                    orientation        =0,
                    rotation           =0,
                    angular            =0,
                    max.velocity       =0,
                    max.linear         =0,
                    max.rotation       =0,
                    max.angular        =0,
                    target             =0,
                    arrive.radius      =0,
                    arrive.slow        =0,
                    arrive.time        =0,
                    align.radius       =0,
                    align.slow         =0,
                    align.time         =0,
                    max.prediction     =0,
                    avoid.radius       =0,
                    col.radius         =0,
                    col.lookahead      =0,
                    col.collided       =FALSE,
                    wander.offset      =0,
                    wander.radius      =0,
                    wander.rate        =0,
                    wander.orientation =0,
                    path.to.follow     =0,
                    path.offset        =0,
                    sep.decay          =0,
                    sep.threshold      =0,
                    swirl.scale        =c(0, 0))

# Initialize scenario-specific variables, including characters, targets, and paths.

if (scenario == 1) {

  character.1.01                <- character.0
  character.1.01$id             <- 101
  character.1.01$steer          <- STOP
  character.1.01$position       <- c( 50,  20)

  character.1.02                <- character.0
  character.1.02$id             <- 102
  character.1.02$steer          <- SEEK
  character.1.02$position       <- c(  5, -35)
  character.1.02$velocity       <- c( -4,   6)
  character.1.02$max.velocity   <- 8
  character.1.02$max.linear     <- 3
  character.1.02$target         <- 3
  
  character.1.03                <- character.0
  character.1.03$id             <- 103
  character.1.03$steer          <- FLEE
  character.1.03$position       <- c(  5, -25)
  character.1.03$velocity       <- c( -4,   6)
  character.1.03$max.velocity   <- 8
  character.1.03$max.linear     <- 2
  character.1.03$target         <- 3
  
  Character        <- list(character.1.01, character.1.02, character.1.03)
  characters       <- 3

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 100    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE) # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                         # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 2) {

  character.2.01                <- character.0
  character.2.01$id             <- 201
  character.2.01$steer          <- STOP
  
  character.2.02                <- character.0
  character.2.02$id             <- 202
  character.2.02$steer          <- SEEK 
  character.2.02$position       <- c( 75,  25)
  character.2.02$velocity       <- c(  0, -12)
  character.2.02$max.velocity   <- 12
  character.2.02$max.linear     <- 4
  character.2.02$target         <- 1
 
  character.2.03                <- character.0
  character.2.03$id             <- 203
  character.2.03$steer          <- FLEE 
  character.2.03$position       <- c(-75, -25)
  character.2.03$velocity       <- c( 12,   8)
  character.2.03$max.velocity   <- 16
  character.2.03$max.linear     <- 4
  character.2.03$target         <- 1
  
  Character        <- list(character.2.01, character.2.02, character.2.03)
  characters       <- 3

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 100    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                          # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 3) {

  character.3.01                <- character.0
  character.3.01$id             <- 301
  character.3.01$steer          <- STOP
  character.3.01$position       <- c( 50,  20)
  
  character.3.02                <- character.0
  character.3.02$id             <- 302
  character.3.02$steer          <- ARRIVE 
  character.3.02$position       <- c(  5, -35)
  character.3.02$velocity       <- c( -4,   6)
  character.3.02$max.velocity   <- 8
  character.3.02$max.linear     <- 3
  character.3.02$target         <- 1
  character.3.02$arrive.radius  <- 4
  character.3.02$arrive.slow    <- 20
  character.3.02$arrive.time    <- 1
  
  character.3.03                <- character.0
  character.3.03$id             <- 303
  character.3.03$steer          <- FLEE 
  character.3.03$position       <- c(  5, -25)
  character.3.03$velocity       <- c( -4,   6)
  character.3.03$max.velocity   <- 8
  character.3.03$max.linear     <- 2
  character.3.03$target         <- 1

  Character        <- list(character.3.01, character.3.02, character.3.03)
  characters       <- 3

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 100    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                          # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 4) {

  character.4.01                <- character.0
  character.4.01$id             <- 401
  character.4.01$steer          <- STOP
  
  character.4.02                <- character.0
  character.4.02$id             <- 402
  character.4.02$steer          <- SEEK
  character.4.02$position       <- c( 75,  25)
  character.4.02$velocity       <- c(  0, -12)
  character.4.02$max.velocity   <- 12
  character.4.02$max.linear     <- 4
  character.4.02$target         <- 1
  
  character.4.03                <- character.0
  character.4.03$id             <- 403
  character.4.03$steer          <- FLEE
  character.4.03$position       <- c(-75, -25)
  character.4.03$velocity       <- c( 12,   8)
  character.4.03$max.velocity   <- 16
  character.4.03$max.linear     <- 4
  character.4.03$target         <- 1
  
  Character        <- list(character.4.01, character.4.02, character.4.03)
  characters       <- 3

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 100    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                          # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 5) {

  character.5.01                <- character.0
  character.5.01$id             <- 501
  character.5.01$steer          <- CONTINUE
  character.5.01$position       <- c( 80, -50)
  character.5.01$velocity       <- c( -3,-1.5)
  character.5.01$linear         <- c(  0,0.10)
  character.5.01$max.velocity   <- 10
  character.5.01$max.linear     <- 1
 
  character.5.02                <- character.0
  character.5.02$id             <- 502
  character.5.02$steer          <- SEEK 
  character.5.02$position       <- c(-20,  30)
  character.5.02$velocity       <- c(  0,   0)
  character.5.02$max.velocity   <- 2.5
  character.5.02$max.linear     <- 1.5
  character.5.02$target         <- 1
  
  character.5.03                <- character.0
  character.5.03$id             <- 503
  character.5.03$steer          <- PURSUE
  character.5.03$position       <- c( 10,  35)
  character.5.03$velocity       <- c(  0,   0)
  character.5.03$max.velocity   <- 2.5
  character.5.03$max.linear     <- 1.5
  character.5.03$target         <- 1
  character.5.03$max.prediction <- 15
  
  Character        <- list(character.5.01, character.5.02, character.5.03)
  characters       <- 3

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 75     # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(6.0, 6.0, 2.0)                          # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE


} else if (scenario == 6) {

  character.6.01                <- character.0
  character.6.01$id             <- 601
  character.6.01$steer          <- CONTINUE
  character.6.01$position       <- c(  0,  75)
  character.6.01$velocity       <- c(  0,  -4)
  character.6.01$max.velocity   <- 4
  
  character.6.02                <- character.0
  character.6.02$id             <- 602
  character.6.02$steer          <- SEEK
  character.6.02$position       <- c( 75,  25)
  character.6.02$max.velocity   <- 4
  character.6.02$max.linear     <- 2
  character.6.02$target         <- 1
  
  character.6.03                <- character.0
  character.6.03$id             <- 603
  character.6.03$steer          <- PURSUE
  character.6.03$position       <- c(-75,  25)
  character.6.03$max.velocity   <- 5
  character.6.03$max.linear     <- 2
  character.6.03$target         <- 1
  character.6.03$max.prediction <- 10
  
  Character        <- list(character.6.01, character.6.02, character.6.03)
  characters       <- 3

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 50     # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                          # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 7) {

  character.7.01                <- character.0
  character.7.01$id             <- 701
  character.7.01$steer          <- FOLLOW.PATH
  character.7.01$position       <- c(-90,  60)
  character.7.01$max.velocity   <- 2.10
  character.7.01$max.linear     <- 1
  character.7.01$path.to.follow <- 1
  character.7.01$path.offset    <- 0.05
  
  character.7.02                <- character.0
  character.7.02$id             <- 702
  character.7.02$steer          <- FOLLOW.PATH
  character.7.02$position       <- c( 75, -10)
  character.7.02$max.velocity   <- 2.30
  character.7.02$max.linear     <- 1
  character.7.02$path.to.follow <- 2
  character.7.02$path.offset    <- 0.05
  
  Character        <- list(character.7.01, character.7.02)
  characters       <- 2

  path.1           <- path.assemble(73, c(-80, -40, 40, 80), c(40, 70, 10, 40))
  path.2           <- path.assemble(74, c(70, 25, 45, -55, -35, -80), c(-25, -20, -80, -80, -20, -25))
  Path             <- list(path.1, path.2)
  paths            <- 2

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 105    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(6.0, 6.0, 2.0)                         # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 8) {  

  character.8.01                <- character.0
  character.8.01$id             <- 801
  character.8.01$steer          <- FOLLOW.PATH
  character.8.01$position       <- c(-90,  60)
  character.8.01$max.velocity   <- 3
  character.8.01$max.linear     <- 1
  character.8.01$path.to.follow <- 1
  character.8.01$path.offset    <- 0.05
  
  character.8.02                <- character.0
  character.8.02$id             <- 802
  character.8.02$steer          <- FOLLOW.PATH
  character.8.02$position       <- c( 75, -10)
  character.8.02$max.velocity   <- 3
  character.8.02$max.linear     <- 1
  character.8.02$path.to.follow <- 2
  character.8.02$path.offset    <- 0.05
 
  Character        <- list(character.8.01, character.8.02)
  characters       <- 2

  path.1           <- path.assemble(83, c(-80, -40, 40, 80), c(40, 70, 10, 40))
  path.2           <- path.assemble(84, c(70, 25, 45, -55, -35, -80), c(-25, -20, -80, -80, -20, -25))
  Path             <- list(path.1, path.2)
  paths            <- 2

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 105    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(6.0, 6.0, 2.0)                         # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 9) {

  character.9.01                <- character.0
  character.9.01$id             <- 901
  character.9.01$steer          <- CONTINUE
  character.9.01$position       <- c(-80, -20)
  character.9.01$velocity       <- c(  1,   1)
  character.9.01$max.velocity   <- 2
  character.9.01$max.linear     <- 1
  character.9.01$avoid.radius   <- 2
  character.9.01$col.radius     <- 0.5
  
  character.9.02                <- character.0
  character.9.02$id             <- 902
  character.9.02$steer          <- CONTINUE
  character.9.02$position       <- c(-72, -28)
  character.9.02$velocity       <- c(  1,   1)
  character.9.02$max.velocity   <- 2
  character.9.02$max.linear     <- 1
  character.9.02$avoid.radius   <- 2
  character.9.02$col.radius     <- 0.5

  character.9.03                <- character.0
  character.9.03$id             <- 903
  character.9.03$steer          <- CONTINUE
  character.9.03$position       <- c(-63, -31)
  character.9.03$velocity       <- c(  1,   1)
  character.9.03$max.velocity   <- 2
  character.9.03$max.linear     <- 1
  character.9.03$col.radius     <- 0.5
  character.9.03$avoid.radius   <- 2
  
  character.9.04                <- character.0
  character.9.04$id             <- 904
  character.9.04$steer          <- CONTINUE
  character.9.04$position       <- c(-72, -34)
  character.9.04$velocity       <- c(  1,   1)
  character.9.04$max.velocity   <- 2
  character.9.04$max.linear     <- 1
  character.9.04$avoid.radius   <- 2
  character.9.04$col.radius     <- 0.5

  character.9.05                <- character.0
  character.9.05$id             <- 905
  character.9.05$steer          <- CONTINUE
  character.9.05$position       <- c( 72, -28)
  character.9.05$velocity       <- c( -1,   1)
  character.9.05$max.velocity   <- 2
  character.9.05$max.linear     <- 1
  character.9.05$avoid.radius   <- 2
  character.9.05$col.radius     <- 0.5

  character.9.06                <- character.0
  character.9.06$id             <- 906
  character.9.06$steer          <- CONTINUE
  character.9.06$position       <- c( 76, -38)
  character.9.06$velocity       <- c( -1,   1)
  character.9.06$max.velocity   <- 2
  character.9.06$max.linear     <- 1
  character.9.06$col.radius     <- 0.5
  character.9.06$avoid.radius   <- 2

  character.9.07                <- character.0
  character.9.07$id             <- 907
  character.9.07$steer          <- CONTINUE
  character.9.07$position       <- c( 75, -22)
  character.9.07$velocity       <- c( -1,   1)
  character.9.07$max.velocity   <- 2
  character.9.07$max.linear     <- 1
  character.9.07$avoid.radius   <- 2
  character.9.07$col.radius     <- 0.5

  character.9.08                <- character.0
  character.9.08$id             <- 908
  character.9.08$steer          <- CONTINUE
  character.9.08$position       <- c( 64, -35)
  character.9.08$velocity       <- c( -1,   1)
  character.9.08$max.velocity   <- 2
  character.9.08$max.linear     <- 1
  character.9.08$avoid.radius   <- 2
  character.9.08$col.radius     <- 0.5

  character.9.09                <- character.0
  character.9.09$id             <- 909
  character.9.09$steer          <- CONTINUE
  character.9.09$position       <- c(-25, -20)
  character.9.09$velocity       <- c( 0.25, 1)
  character.9.09$max.velocity   <- 2
  character.9.09$max.linear     <- 2
  character.9.09$avoid.radius   <- 2
  character.9.09$col.radius     <- 0.5

  character.9.10                <- character.0
  character.9.10$id             <- 910
  character.9.10$steer          <- CONTINUE
  character.9.10$position       <- c( 20, -25)
  character.9.10$velocity       <- c(-0.25, 1)
  character.9.10$max.velocity   <- 2
  character.9.10$max.linear     <- 2
  character.9.10$avoid.radius   <- 2
  character.9.10$col.radius     <- 0.5

  Character        <- list(character.9.01, character.9.02, character.9.03, character.9.04,
                           character.9.05, character.9.06, character.9.07, character.9.08,
                           character.9.09, character.9.10)
  characters       <- 10

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 100    # Time of last time step
  check.collisions <- TRUE

  plot.what        <- c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                           # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 10) {

  character.10.01                <- character.0
  character.10.01$id             <- 1001
  character.10.01$steer          <- CONTINUE
  character.10.01$position       <- c(-80, -20)
  character.10.01$velocity       <- c(  1,   1)
  character.10.01$max.velocity   <- 2
  character.10.01$max.linear     <- 1
  character.10.01$avoid.radius   <- 2
  character.10.01$col.radius     <- 0.5
  
  character.10.02                <- character.0
  character.10.02$id             <- 1002
  character.10.02$steer          <- CONTINUE
  character.10.02$position       <- c(-72, -28)
  character.10.02$velocity       <- c(  1,   1)
  character.10.02$max.velocity   <- 2
  character.10.02$max.linear     <- 1
  character.10.02$avoid.radius   <- 2
  character.10.02$col.radius     <- 0.5

  character.10.03                <- character.0
  character.10.03$id             <- 1003
  character.10.03$steer          <- CONTINUE
  character.10.03$position       <- c(-63, -31)
  character.10.03$velocity       <- c( 1, 1)
  character.10.03$max.velocity   <- 2
  character.10.03$max.linear     <- 1
  character.10.03$avoid.radius   <- 2
  character.10.03$col.radius     <- 0.5
  
  character.10.04                <- character.0
  character.10.04$id             <- 1004
  character.10.04$steer          <- CONTINUE
  character.10.04$position       <- c(-72, -34)
  character.10.04$velocity       <- c(  1,   1)
  character.10.04$max.velocity   <- 2
  character.10.04$max.linear     <- 1
  character.10.04$avoid.radius   <- 2
  character.10.04$col.radius     <- 0.5

  character.10.05                <- character.0
  character.10.05$id             <- 1005
  character.10.05$steer          <- AVOID.COLLISIONS
  character.10.05$position       <- c( 72, -28)
  character.10.05$velocity       <- c( -1,   1)
  character.10.05$max.velocity   <- 2
  character.10.05$max.linear     <- 1
  character.10.05$avoid.radius   <- 2
  character.10.05$col.radius     <- 0.5
  character.10.05$col.lookahead  <- 100

  character.10.06                <- character.0
  character.10.06$id             <- 1006
  character.10.06$steer          <- AVOID.COLLISIONS
  character.10.06$position       <- c( 76, -38)
  character.10.06$velocity       <- c( -1,   1)
  character.10.06$max.velocity   <- 2
  character.10.06$max.linear     <- 1
  character.10.06$avoid.radius   <- 2
  character.10.06$col.radius     <- 0.5
  character.10.06$col.lookahead  <- 100

  character.10.07                <- character.0
  character.10.07$id             <- 1007
  character.10.07$steer          <- AVOID.COLLISIONS
  character.10.07$position       <- c( 75, -22)
  character.10.07$velocity       <- c( -1,   1)
  character.10.07$max.velocity   <- 2
  character.10.07$max.linear     <- 1
  character.10.07$avoid.radius   <- 2
  character.10.07$col.radius     <- 0.5
  character.10.07$col.lookahead  <- 100

  character.10.08                <- character.0
  character.10.08$id             <- 1008
  character.10.08$steer          <- AVOID.COLLISIONS
  character.10.08$position       <- c( 64, -35)
  character.10.08$velocity       <- c( -1,   1)
  character.10.08$max.velocity   <- 2
  character.10.08$max.linear     <- 1
  character.10.08$avoid.radius   <- 2
  character.10.08$col.radius     <- 0.5
  character.10.08$col.lookahead  <- 100

  character.10.09                <- character.0
  character.10.09$id             <- 1009
  character.10.09$steer          <- CONTINUE
  character.10.09$position       <- c(-25, -20)
  character.10.09$velocity       <- c( 0.25, 1)
  character.10.09$max.velocity   <- 2
  character.10.09$max.linear     <- 2
  character.10.09$avoid.radius   <- 2
  character.10.09$col.radius     <- 0.5

  character.10.10                <- character.0
  character.10.10$id             <- 1010
  character.10.10$steer          <- CONTINUE
  character.10.10$position       <- c( 20, -25)
  character.10.10$velocity       <- c(-0.25, 1)
  character.10.10$max.velocity   <- 2
  character.10.10$max.linear     <- 2
  character.10.10$avoid.radius   <- 2
  character.10.10$col.radius     <- 0.5

  Character        <- list(character.10.01, character.10.02, character.10.03, character.10.04,
                           character.10.05, character.10.06, character.10.07, character.10.08,
                           character.10.09, character.10.10)
  characters       <- 10

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 100    # Time of last time step
  check.collisions <- TRUE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(6.0, 6.0, 2.0)                           # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 11) {

  character.11.01                <- character.0
  character.11.01$id             <- 1101
  character.11.01$steer          <- CONTINUE
  character.11.01$position       <- c(-80, -20)
  character.11.01$velocity       <- c(  1,   1)
  character.11.01$max.velocity   <- 2
  character.11.01$max.linear     <- 1
  character.11.01$avoid.radius   <- 2
  character.11.01$col.radius     <- 0.5
  
  character.11.02                <- character.0
  character.11.02$id             <- 1102
  character.11.02$steer          <- CONTINUE
  character.11.02$position       <- c(-72, -28)
  character.11.02$velocity       <- c(  1,   1)
  character.11.02$max.velocity   <- 2
  character.11.02$max.linear     <- 1
  character.11.02$avoid.radius   <- 2
  character.11.02$col.radius     <- 0.5

  character.11.03                <- character.0
  character.11.03$id             <- 1103
  character.11.03$steer          <- CONTINUE
  character.11.03$position       <- c(-63, -31)
  character.11.03$velocity       <- c(  1,   1)
  character.11.03$max.velocity   <- 2
  character.11.03$max.linear     <- 1
  character.11.03$avoid.radius   <- 2
  character.11.03$col.radius     <- 0.5
  
  character.11.04                <- character.0
  character.11.04$id             <- 1104
  character.11.04$steer          <- CONTINUE
  character.11.04$position       <- c(-72, -34)
  character.11.04$velocity       <- c(  1  , 1)
  character.11.04$max.velocity   <- 2
  character.11.04$max.linear     <- 1
  character.11.04$avoid.radius   <- 2
  character.11.04$col.radius     <- 0.5

  character.11.05                <- character.0
  character.11.05$id             <- 1105
  character.11.05$steer          <- AVOID.COLLISIONS
  character.11.05$position       <- c( 72, -28)
  character.11.05$velocity       <- c( -1,   1)
  character.11.05$max.velocity   <- 2
  character.11.05$max.linear     <- 1
  character.11.05$avoid.radius   <- 2
  character.11.05$col.radius     <- 0.5
  character.11.05$col.lookahead  <- 10

  character.11.06                <- character.0
  character.11.06$id             <- 1106
  character.11.06$steer          <- AVOID.COLLISIONS
  character.11.06$position       <- c( 76, -38)
  character.11.06$velocity       <- c( -1,   1)
  character.11.06$max.velocity   <- 2
  character.11.06$max.linear     <- 1
  character.11.06$avoid.radius   <- 2
  character.11.06$col.radius     <- 0.5
  character.11.06$col.lookahead  <- 10

  character.11.07                <- character.0
  character.11.07$id             <- 1107
  character.11.07$steer          <- AVOID.COLLISIONS
  character.11.07$position       <- c( 75, -22)
  character.11.07$velocity       <- c( -1,   1)
  character.11.07$max.velocity   <- 2
  character.11.07$max.linear     <- 1
  character.11.07$avoid.radius   <- 2
  character.11.07$col.radius     <- 0.5
  character.11.07$col.lookahead  <- 10

  character.11.08                <- character.0
  character.11.08$id             <- 1108
  character.11.08$steer          <- AVOID.COLLISIONS
  character.11.08$position       <- c( 64, -35)
  character.11.08$velocity       <- c( -1,   1)
  character.11.08$max.velocity   <- 2
  character.11.08$max.linear     <- 1
  character.11.08$avoid.radius   <- 2
  character.11.08$col.radius     <- 0.5
  character.11.08$col.lookahead  <- 10

  character.11.09                <- character.0
  character.11.09$id             <- 1109
  character.11.09$steer          <- CONTINUE
  character.11.09$position       <- c(-25, -20)
  character.11.09$velocity       <- c( 0.25, 1)
  character.11.09$max.velocity   <- 2
  character.11.09$max.linear     <- 2
  character.11.09$avoid.radius   <- 2
  character.11.09$col.radius     <- 0.5

  character.11.10                <- character.0
  character.11.10$id             <- 1110
  character.11.10$steer          <- CONTINUE
  character.11.10$position       <- c( 20, -25)
  character.11.10$velocity       <- c(-0.25, 1)
  character.11.10$max.velocity   <- 2
  character.11.10$max.linear     <- 2
  character.11.10$avoid.radius   <- 2
  character.11.10$col.radius     <- 0.5

  Character        <- list(character.11.01, character.11.02, character.11.03, character.11.04,
                           character.11.05, character.11.06, character.11.07, character.11.08,
                           character.11.09, character.11.10)
  characters       <- 10

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 100    # Time of last time step
  check.collisions <- TRUE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(6.0, 6.0, 2.0)                           # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 12) {

  character.12.01                <- character.0
  character.12.01$id             <- 1201
  character.12.01$steer          <- STOP
  character.12.01$position       <- c(-60,  80)
  character.12.01$velocity       <- c( 14, -17)
  character.12.01$max.velocity   <- 22
  character.12.01$max.linear     <- 1
  character.12.01$avoid.radius   <- 2
  character.12.01$col.radius     <- 1.5
  
  character.12.02                <- character.0
  character.12.02$id             <- 1202
  character.12.02$steer          <- STOP
  character.12.02$position       <- c(-63,  74)
  character.12.02$velocity       <- c( 14, -16)
  character.12.02$max.velocity   <- 22
  character.12.02$max.linear     <- 1
  character.12.02$avoid.radius   <- 2
  character.12.02$col.radius     <- 1.5

  character.12.03                <- character.0
  character.12.03$id             <- 1203
  character.12.03$steer          <- STOP
  character.12.03$position       <- c(-68,  72)
  character.12.03$velocity       <- c( 14, -17)
  character.12.03$max.velocity   <- 22
  character.12.03$max.linear     <- 1
  character.12.03$avoid.radius   <- 2
  character.12.03$col.radius     <- 1.5
  
  character.12.04                <- character.0
  character.12.04$id             <- 1204
  character.12.04$steer          <- STOP
  character.12.04$position       <- c(-67,  68)
  character.12.04$velocity       <- c( 13, -15)
  character.12.04$max.velocity   <- 22
  character.12.04$max.linear     <- 1
  character.12.04$avoid.radius   <- 2
  character.12.04$col.radius     <- 1.5
  
  character.12.05                <- character.0
  character.12.05$id             <- 1205
  character.12.05$steer          <- STOP
  character.12.05$position       <- c(-74,  65)
  character.12.05$velocity       <- c( 12, -16)
  character.12.05$max.velocity   <- 22
  character.12.05$max.linear     <- 1
  character.12.05$avoid.radius   <- 2
  character.12.05$col.radius     <- 1.5
  
  character.12.06                <- character.0
  character.12.06$id             <- 1206
  character.12.06$steer          <- STOP
  character.12.06$position       <- c(-74,  57)
  character.12.06$velocity       <- c( 11, -13)
  character.12.06$max.velocity   <- 22
  character.12.06$max.linear     <- 1
  character.12.06$avoid.radius   <- 2
  character.12.06$col.radius     <- 1.5
  
  character.12.07                <- character.0
  character.12.07$id             <- 1207
  character.12.07$steer          <- STOP
  character.12.07$position       <- c(-88,  56)
  character.12.07$velocity       <- c( 12, -15)
  character.12.07$max.velocity   <- 22
  character.12.07$max.linear     <- 1
  character.12.07$avoid.radius   <- 2
  character.12.07$col.radius     <- 1.5

  character.12.08                <- character.0
  character.12.08$id             <- 1208
  character.12.08$steer          <- STOP
  character.12.08$position       <- c(-81,  53)
  character.12.08$velocity       <- c( 12, -14)
  character.12.08$max.velocity   <- 22
  character.12.08$max.linear     <- 1
  character.12.08$avoid.radius   <- 2
  character.12.08$col.radius     <- 1.5

  character.12.09                <- character.0
  character.12.09$id             <- 1209
  character.12.09$steer          <- STOP
  character.12.09$position       <- c(-56,  78)
  character.12.09$velocity       <- c( 14, -17)
  character.12.09$max.velocity   <- 22
  character.12.09$max.linear     <- 1
  character.12.09$avoid.radius   <- 2
  character.12.09$col.radius     <- 1.5

  character.12.10                <- character.0
  character.12.10$id             <- 1210
  character.12.10$steer          <- STOP
  character.12.10$position       <- c(-59,  71)
  character.12.10$velocity       <- c( 14, -16)
  character.12.10$max.velocity   <- 22
  character.12.10$max.linear     <- 1
  character.12.10$avoid.radius   <- 2
  character.12.10$col.radius     <- 1.5

  character.12.11                <- character.0
  character.12.11$id             <- 1211
  character.12.11$steer          <- STOP
  character.12.11$position       <- c(-64,  68)
  character.12.11$velocity       <- c( 14, -17)
  character.12.11$max.velocity   <- 22
  character.12.11$max.linear     <- 1
  character.12.11$avoid.radius   <- 2
  character.12.11$col.radius     <- 1.5

  character.12.12                <- character.0
  character.12.12$id             <- 1212
  character.12.12$steer          <- STOP
  character.12.12$position       <- c(-63,  67)
  character.12.12$velocity       <- c( 13, -15)
  character.12.12$max.velocity   <- 22
  character.12.12$max.linear     <- 1
  character.12.12$avoid.radius   <- 2
  character.12.12$col.radius     <- 1.5

  character.12.13                <- character.0
  character.12.13$id             <- 1213
  character.12.13$steer          <- STOP
  character.12.13$position       <- c(-70,  63)
  character.12.13$velocity       <- c( 12, -16)
  character.12.13$max.velocity   <- 22
  character.12.13$max.linear     <- 1
  character.12.13$avoid.radius   <- 2
  character.12.13$col.radius     <- 1.5

  character.12.14                <- character.0
  character.12.14$id             <- 1214
  character.12.14$steer          <- STOP
  character.12.14$position       <- c(-70,  54)
  character.12.14$velocity       <- c( 11, -13)
  character.12.14$max.velocity   <- 22
  character.12.14$max.linear     <- 1
  character.12.14$avoid.radius   <- 2
  character.12.14$col.radius     <- 1.5

  character.12.15                <- character.0
  character.12.15$id             <- 1215
  character.12.15$steer          <- STOP
  character.12.15$position       <- c(-84,  52)
  character.12.15$velocity       <- c( 12, -15)
  character.12.15$max.velocity   <- 22
  character.12.15$max.linear     <- 1
  character.12.15$avoid.radius   <- 2
  character.12.15$col.radius     <- 1.5

  character.12.16                <- character.0
  character.12.16$id             <- 1216
  character.12.16$steer          <- STOP
  character.12.16$position       <- c(-77,  52)
  character.12.16$velocity       <- c( 12, -14)
  character.12.16$max.velocity   <- 22
  character.12.16$max.linear     <- 1
  character.12.16$avoid.radius   <- 2
  character.12.16$col.radius     <- 1.5

  character.12.17                <- character.0
  character.12.17$id             <- 1217
  character.12.17$steer          <- STOP
  character.12.17$position       <- c( 40,  18)
  character.12.17$velocity       <- c( -9,  -6)
  character.12.17$max.velocity   <- 20
  character.12.17$max.linear     <- 2
  character.12.17$avoid.radius   <- 2
  character.12.17$col.radius     <- 1.5

  character.12.18                <- character.0
  character.12.18$id             <- 1218
  character.12.18$steer          <- STOP
  character.12.18$position       <- c( 44,  24)
  character.12.18$velocity       <- c( -9,  -6)
  character.12.18$max.velocity   <- 20
  character.12.18$max.linear     <- 2
  character.12.18$avoid.radius   <- 2
  character.12.18$col.radius     <- 1.5

  Character        <- list(character.12.01, character.12.02, character.12.03, character.12.04,
                           character.12.05, character.12.06, character.12.07, character.12.08,
                           character.12.09, character.12.10, character.12.11, character.12.12,
                           character.12.13, character.12.14, character.12.15, character.12.16,
                           character.12.17, character.12.18)
  characters       <- 18

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 10     # Time of last time step
  check.collisions <- TRUE

  plot.what        <- c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                           # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 13) {

  character.13.01                <- character.0
  character.13.01$id             <- 1301
  character.13.01$steer          <- STOP
  character.13.01$position       <- c( -60,  80)
  character.13.01$velocity       <- c(  14, -17)
  character.13.01$max.velocity   <- 22
  character.13.01$max.linear     <- 1
  character.13.01$avoid.radius   <- 2
  character.13.01$col.radius     <- 1.5
  
  character.13.02                <- character.0
  character.13.02$id             <- 1302
  character.13.02$steer          <- STOP
  character.13.02$position       <- c( -63,  74)
  character.13.02$velocity       <- c(  14, -16)
  character.13.02$max.velocity   <- 22
  character.13.02$max.linear     <- 1
  character.13.02$avoid.radius   <- 2
  character.13.02$col.radius     <- 1.5

  character.13.03                <- character.0
  character.13.03$id             <- 1303
  character.13.03$steer          <- STOP
  character.13.03$position       <- c( -68,  72)
  character.13.03$velocity       <- c(  14, -17)
  character.13.03$max.velocity   <- 22
  character.13.03$max.linear     <- 1
  character.13.03$avoid.radius   <- 2
  character.13.03$col.radius     <- 1.5
  
  character.13.04                <- character.0
  character.13.04$id             <- 1304
  character.13.04$steer          <- STOP
  character.13.04$position       <- c( -67,  68)
  character.13.04$velocity       <- c(  13, -15)
  character.13.04$max.velocity   <- 22
  character.13.04$max.linear     <- 1
  character.13.04$avoid.radius   <- 2
  character.13.04$col.radius     <- 1.5
  
  character.13.05                <- character.0
  character.13.05$id             <- 1305
  character.13.05$steer          <- STOP
  character.13.05$position       <- c( -74,  65)
  character.13.05$velocity       <- c(  12, -16)
  character.13.05$max.velocity   <- 22
  character.13.05$max.linear     <- 1
  character.13.05$avoid.radius   <- 2
  character.13.05$col.radius     <- 1.5
  
  character.13.06                <- character.0
  character.13.06$id             <- 1306
  character.13.06$steer          <- STOP
  character.13.06$position       <- c(-74,  57)
  character.13.06$velocity       <- c( 11, -13)
  character.13.06$max.velocity   <- 22
  character.13.06$max.linear     <- 1
  character.13.06$avoid.radius   <- 2
  character.13.06$col.radius     <- 1.5

  character.13.07                <- character.0
  character.13.07$id             <- 1307
  character.13.07$steer          <- STOP
  character.13.07$position       <- c(-88,  56)
  character.13.07$velocity       <- c( 12, -15)
  character.13.07$max.velocity   <- 22
  character.13.07$max.linear     <- 1
  character.13.07$avoid.radius   <- 2
  character.13.07$col.radius     <- 1.5

  character.13.08                <- character.0
  character.13.08$id             <- 1308
  character.13.08$steer          <- STOP
  character.13.08$position       <- c(-81,  53)
  character.13.08$velocity       <- c( 12, -14)
  character.13.08$max.velocity   <- 22
  character.13.08$max.linear     <- 1
  character.13.08$avoid.radius   <- 2
  character.13.08$col.radius     <- 1.5

  character.13.09                <- character.0
  character.13.09$id             <- 1309
  character.13.09$steer          <- STOP
  character.13.09$position       <- c(-56,  78)
  character.13.09$velocity       <- c( 14, -17)
  character.13.09$max.velocity   <- 22
  character.13.09$max.linear     <- 1
  character.13.09$avoid.radius   <- 2
  character.13.09$col.radius     <- 1.5

  character.13.10                <- character.0
  character.13.10$id             <- 1310
  character.13.10$steer          <- STOP
  character.13.10$position       <- c(-59,  71)
  character.13.10$velocity       <- c( 14, -16)
  character.13.10$max.velocity   <- 22
  character.13.10$max.linear     <- 1
  character.13.10$avoid.radius   <- 2
  character.13.10$col.radius     <- 1.5

  character.13.11                <- character.0
  character.13.11$id             <- 1311
  character.13.11$steer          <- STOP
  character.13.11$position       <- c(-64,  68)
  character.13.11$velocity       <- c( 14, -17)
  character.13.11$max.velocity   <- 22
  character.13.11$max.linear     <- 1
  character.13.11$avoid.radius   <- 2
  character.13.11$col.radius     <- 1.5

  character.13.12                <- character.0
  character.13.12$id             <- 1312
  character.13.12$steer          <- STOP
  character.13.12$position       <- c(-63,  67)
  character.13.12$velocity       <- c( 13, -15)
  character.13.12$max.velocity   <- 22
  character.13.12$max.linear     <- 1
  character.13.12$avoid.radius   <- 2
  character.13.12$col.radius     <- 1.5

  character.13.13                <- character.0
  character.13.13$id             <- 1313
  character.13.13$steer          <- STOP
  character.13.13$position       <- c(-70,  63)
  character.13.13$velocity       <- c( 12, -16)
  character.13.13$max.velocity   <- 22
  character.13.13$max.linear     <- 1
  character.13.13$avoid.radius   <- 2
  character.13.13$col.radius     <- 1.5

  character.13.14                <- character.0
  character.13.14$id             <- 1314
  character.13.14$steer          <- STOP
  character.13.14$position       <- c(-70,  54)
  character.13.14$velocity       <- c( 11, -13)
  character.13.14$max.velocity   <- 22
  character.13.14$max.linear     <- 1
  character.13.14$avoid.radius   <- 2
  character.13.14$col.radius     <- 1.5

  character.13.15                <- character.0
  character.13.15$id             <- 1315
  character.13.15$steer          <- STOP
  character.13.15$position       <- c(-84,  52)
  character.13.15$velocity       <- c( 12, -15)
  character.13.15$max.velocity   <- 22
  character.13.15$max.linear     <- 1
  character.13.15$avoid.radius   <- 2
  character.13.15$col.radius     <- 1.5

  character.13.16                <- character.0
  character.13.16$id             <- 1316
  character.13.16$steer          <- STOP
  character.13.16$position       <- c(-77,  52)
  character.13.16$velocity       <- c( 12, -14)
  character.13.16$max.velocity   <- 22
  character.13.16$max.linear     <- 1
  character.13.16$avoid.radius   <- 2
  character.13.16$col.radius     <- 1.5

  character.13.17                <- character.0
  character.13.17$id             <- 1317
  character.13.17$steer          <- AVOID.COLLISIONS
  character.13.17$position       <- c( 40,  18)
  character.13.17$velocity       <- c( -9,  -6)
  character.13.17$max.velocity   <- 20
  character.13.17$max.linear     <- 2
  character.13.17$avoid.radius   <- 2
  character.13.17$col.radius     <- 1.5
  character.13.17$col.lookahead  <- 100

  character.13.18                <- character.0
  character.13.18$id             <- 1318
  character.13.18$steer          <- AVOID.COLLISIONS
  character.13.18$position       <- c( 44,  24)
  character.13.18$velocity       <- c( -9,  -6)
  character.13.18$max.velocity   <- 20
  character.13.18$max.linear     <- 2
  character.13.18$avoid.radius   <- 2
  character.13.18$col.radius     <- 1.5
  character.13.18$col.lookahead  <- 100

  Character        <- list(character.13.01, character.13.02, character.13.03, character.13.04,
                           character.13.05, character.13.06, character.13.07, character.13.08,
                           character.13.09, character.13.10, character.13.11, character.13.12,
                           character.13.13, character.13.14, character.13.15, character.13.16,
                           character.13.17, character.13.18)
  characters       <- 18

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 10     # Time of last time step
  check.collisions <- TRUE

  plot.what        <- c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                           # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 14) {

  character.14.01                <- character.0
  character.14.01$id             <- 1401
  character.14.01$steer          <- SEPARATE
  character.14.01$position       <- c(  -4,  16)
  character.14.01$max.velocity   <- 8
  character.14.01$max.linear     <- 4
  character.14.01$sep.decay      <- 3
  character.14.01$sep.threshold  <- 20

  character.14.02                <- character.0
  character.14.02$id             <- 1402
  character.14.02$steer          <- SEPARATE
  character.14.02$position       <- c(  0,  12)
  character.14.02$max.velocity   <- 8
  character.14.02$max.linear     <- 4
  character.14.02$sep.decay      <- 3
  character.14.02$sep.threshold  <- 20

  character.14.03                <- character.0
  character.14.03$id             <- 1403
  character.14.03$steer          <- SEPARATE
  character.14.03$position       <- c(-16,   8)
  character.14.03$max.velocity   <- 8
  character.14.03$max.linear     <- 4
  character.14.03$sep.decay      <- 3
  character.14.03$sep.threshold  <- 20

  character.14.04                <- character.0
  character.14.04$id             <- 1404
  character.14.04$steer          <- SEPARATE
  character.14.04$position       <- c(  4,   8)
  character.14.04$max.velocity   <- 8
  character.14.04$max.linear     <- 4
  character.14.04$sep.decay      <- 3
  character.14.04$sep.threshold  <- 20

  character.14.05                <- character.0
  character.14.05$id             <- 1405
  character.14.05$steer          <- SEPARATE
  character.14.05$position       <- c( -4,   4)
  character.14.05$max.velocity   <- 8
  character.14.05$max.linear     <- 4
  character.14.05$sep.decay      <- 3
  character.14.05$sep.threshold  <- 20

  character.14.06                <- character.0
  character.14.06$id             <- 1406
  character.14.06$steer          <- SEPARATE
  character.14.06$position       <- c(  8,   4)
  character.14.06$max.velocity   <- 8
  character.14.06$max.linear     <- 4
  character.14.06$sep.decay      <- 3
  character.14.06$sep.threshold  <- 20

  character.14.07                <- character.0
  character.14.07$id             <- 1407
  character.14.07$steer          <- SEPARATE
  character.14.07$position       <- c(  0,   0)
  character.14.07$max.velocity   <- 8
  character.14.07$max.linear     <- 4
  character.14.07$sep.decay      <- 3
  character.14.07$sep.threshold  <- 20

  character.14.08                <- character.0
  character.14.08$id             <- 1408
  character.14.08$steer          <- SEPARATE
  character.14.08$position       <- c( -8,  -4)
  character.14.08$max.velocity   <- 8
  character.14.08$max.linear     <- 4
  character.14.08$sep.decay      <- 3
  character.14.08$sep.threshold  <- 20

  character.14.09                <- character.0
  character.14.09$id             <- 1409
  character.14.09$steer          <- SEPARATE
  character.14.09$position       <- c(  4,  -4)
  character.14.09$max.velocity   <- 8
  character.14.09$max.linear     <- 4
  character.14.09$sep.decay      <- 3
  character.14.09$sep.threshold  <- 20

  character.14.10                <- character.0
  character.14.10$id             <- 1410
  character.14.10$steer          <- SEPARATE
  character.14.10$position       <- c( -4,  -8)
  character.14.10$max.velocity   <- 8
  character.14.10$max.linear     <- 4
  character.14.10$sep.decay      <- 3
  character.14.10$sep.threshold  <- 20

  character.14.11                <- character.0
  character.14.11$id             <- 1411
  character.14.11$steer          <- SEPARATE
  character.14.11$position       <- c( 12,  -8)
  character.14.11$max.velocity   <- 8
  character.14.11$max.linear     <- 4
  character.14.11$sep.decay      <- 3
  character.14.11$sep.threshold  <- 20

  character.14.12                <- character.0
  character.14.12$id             <- 1412
  character.14.12$steer          <- SEPARATE
  character.14.12$position       <- c( -8, -16)
  character.14.12$max.velocity   <- 8
  character.14.12$max.linear     <- 4
  character.14.12$sep.decay      <- 3
  character.14.12$sep.threshold  <- 20

  character.14.13                <- character.0
  character.14.13$id             <- 1413
  character.14.13$steer          <- SEPARATE
  character.14.13$position       <- c(  4, -12)
  character.14.13$max.velocity   <- 8
  character.14.13$max.linear     <- 4
  character.14.13$sep.decay      <- 3
  character.14.13$sep.threshold  <- 20

  Character       <- list(character.14.01, character.14.02, character.14.03, character.14.04,
                          character.14.05, character.14.06, character.14.07, character.14.08,
                          character.14.09, character.14.10, character.14.11, character.14.12,
                          character.14.13)
  characters       <- 13

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 100    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(6.0, 6.0, 2.0)                          # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 15) {

  character.15.01                <- character.0
  character.15.01$id             <- 1501
  character.15.01$steer          <- SEPARATE
  character.15.01$position       <- c( -4,  16)
  character.15.01$max.velocity   <- 8
  character.15.01$max.linear     <- 4
  character.15.01$sep.decay      <- 10
  character.15.01$sep.threshold  <- 8

  character.15.02                <- character.0
  character.15.02$id             <- 1502
  character.15.02$steer          <- SEPARATE
  character.15.02$position       <- c(  0,  12)
  character.15.02$max.velocity   <- 8
  character.15.02$max.linear     <- 4
  character.15.02$sep.decay      <- 10
  character.15.02$sep.threshold  <- 8

  character.15.03                <- character.0
  character.15.03$id             <- 1503
  character.15.03$steer          <- SEPARATE
  character.15.03$position       <- c(-16,   8)
  character.15.03$max.velocity   <- 8
  character.15.03$max.linear     <- 4
  character.15.03$sep.decay      <- 10
  character.15.03$sep.threshold  <- 8

  character.15.04                <- character.0
  character.15.04$id             <- 1504
  character.15.04$steer          <- SEPARATE
  character.15.04$position       <- c(  4,   8)
  character.15.04$max.velocity   <- 8
  character.15.04$max.linear     <- 4
  character.15.04$sep.decay      <- 10
  character.15.04$sep.threshold  <- 8

  character.15.05                <- character.0
  character.15.05$id             <- 1505
  character.15.05$steer          <- SEPARATE
  character.15.05$position       <- c( -4,   4)
  character.15.05$max.velocity   <- 8
  character.15.05$max.linear     <- 4
  character.15.05$sep.decay      <- 10
  character.15.05$sep.threshold  <- 8

  character.15.06                <- character.0
  character.15.06$id             <- 1506
  character.15.06$steer          <- SEPARATE
  character.15.06$position       <- c(  8,   4)
  character.15.06$max.velocity   <- 8
  character.15.06$max.linear     <- 4
  character.15.06$sep.decay      <- 10
  character.15.06$sep.threshold  <- 8

  character.15.07                <- character.0
  character.15.07$id             <- 1507
  character.15.07$steer          <- SEPARATE
  character.15.07$position       <- c(  0,   0)
  character.15.07$max.velocity   <- 8
  character.15.07$max.linear     <- 4
  character.15.07$sep.decay      <- 10
  character.15.07$sep.threshold  <- 8

  character.15.08                <- character.0
  character.15.08$id             <- 1508
  character.15.08$steer          <- SEPARATE
  character.15.08$position       <- c( -8,  -4)
  character.15.08$max.velocity   <- 8
  character.15.08$max.linear     <- 4
  character.15.08$sep.decay      <- 10
  character.15.08$sep.threshold  <- 8

  character.15.09                <- character.0
  character.15.09$id             <- 1509
  character.15.09$steer          <- SEPARATE
  character.15.09$position       <- c(  4,  -4)
  character.15.09$max.velocity   <- 8
  character.15.09$max.linear     <- 4
  character.15.09$sep.decay      <- 10
  character.15.09$sep.threshold  <- 8

  character.15.10                <- character.0
  character.15.10$id             <- 1510
  character.15.10$steer          <- SEPARATE
  character.15.10$position       <- c( -4,  -8)
  character.15.10$max.velocity   <- 8
  character.15.10$max.linear     <- 4
  character.15.10$sep.decay      <- 10
  character.15.10$sep.threshold  <- 8

  character.15.11                <- character.0
  character.15.11$id             <- 1511
  character.15.11$steer          <- SEPARATE
  character.15.11$position       <- c( 12,  -8)
  character.15.11$max.velocity   <- 8
  character.15.11$max.linear     <- 4
  character.15.11$sep.decay      <- 10
  character.15.11$sep.threshold  <- 8

  character.15.12                <- character.0
  character.15.12$id             <- 1512
  character.15.12$steer          <- SEPARATE
  character.15.12$position       <- c( -8, -16)
  character.15.12$max.velocity   <- 8
  character.15.12$max.linear     <- 4
  character.15.12$sep.decay      <- 10
  character.15.12$sep.threshold  <- 8

  character.15.13                <- character.0
  character.15.13$id             <- 1513
  character.15.13$steer          <- SEPARATE
  character.15.13$position       <- c(  4, -12)
  character.15.13$max.velocity   <- 8
  character.15.13$max.linear     <- 4
  character.15.13$sep.decay      <- 10
  character.15.13$sep.threshold  <- 8
  
  character.15.14                <- character.0
  character.15.14$id             <- 1514
  character.15.14$steer          <- CONTINUE
  character.15.14$position       <- c(-40, -40)
  character.15.14$velocity       <- c(  3,   2)
  character.15.14$max.velocity   <- 8
  character.15.14$max.linear     <- 4
  character.15.14$sep.decay      <- 10
  character.15.14$sep.threshold  <- 8  
  
  character.15.15                <- character.0
  character.15.15$id             <- 1515
  character.15.15$steer          <- CONTINUE
  character.15.15$position       <- c(-40,  40)
  character.15.15$velocity       <- c(  2,  -2)
  character.15.15$max.velocity   <- 8
  character.15.15$max.linear     <- 4
  character.15.15$sep.decay      <- 10
  character.15.15$sep.threshold  <- 8 

  Character        <- list(character.15.01, character.15.02, character.15.03, character.15.04,
                           character.15.05, character.15.06, character.15.07, character.15.08,
                           character.15.09, character.15.10, character.15.11, character.15.12,
                           character.15.13, character.15.14, character.15.15)
  characters       <- 15

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 100    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(6.0, 6.0, 2.0)                          # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 16) {

  character.16.01                <- character.0
  character.16.01$id             <- 1601
  character.16.01$steer          <- STOP

  character.16.02                <- character.0
  character.16.02$id             <- 1602
  character.16.02$steer          <- FLEE
  character.16.02$position       <- c(-25, -50)
  character.16.02$velocity       <- c(  0,   8)
  character.16.02$orientation    <- (pi / 4)
  character.16.02$max.velocity   <- 10
  character.16.02$max.linear     <- 2
  character.16.02$target         <- 1
  
  character.16.03                <- character.0
  character.16.03$id             <- 1603
  character.16.03$steer          <- SEEK
  character.16.03$position       <- c( 50,  25)
  character.16.03$velocity       <- c(  0,   8)
  character.16.03$orientation    <- (3 * pi / 2)
  character.16.03$max.velocity   <- 8
  character.16.03$max.linear     <- 2
  character.16.03$target         <- 1

  character.16.04                <- character.0
  character.16.04$id             <- 1604
  character.16.04$steer          <- ARRIVE
  character.16.04$position       <- c(-50,  75)
  character.16.04$velocity       <- c( -6,  -4)
  character.16.04$orientation    <- pi
  character.16.04$max.velocity   <- 8
  character.16.04$max.linear     <- 2
  character.16.04$target         <- 1
  character.16.04$arrive.radius  <- 4
  character.16.04$arrive.slow    <- 32
  character.16.04$arrive.time    <- 1

  Character        <- list(character.16.01, character.16.02, character.16.03, character.16.04)
  characters       <- 4

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 50     # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE) # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                        # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 17) {

  character.17.01                <- character.0
  character.17.01$id             <- 1701
  character.17.01$steer          <- FOLLOW.PATH
  character.17.01$position       <- c( 70, -40)
  character.17.01$max.velocity   <- 4
  character.17.01$max.linear     <- 2
  character.17.01$path.to.follow <- 1
  character.17.01$path.offset    <- 0.05
                       
  Character        <- list(character.17.01)
  characters       <- 1

  path.1           <- path.assemble(171, c(75, 45, 15, -15, -45, -75), c(-20, 20, -40, 40, -60, 60))
  Path             <- list(path.1)
  paths            <- 1

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 100    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(4.0, 4.0, 2.0)                        # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 18) {

  character.18.01                <- character.0
  character.18.01$id             <- 1801
  character.18.01$steer          <- STOP
  character.18.01$position       <- c(  0, -20)
  
  character.18.02                <- character.0
  character.18.02$id             <- 1802
  character.18.02$steer          <- SEEK
  character.18.02$position       <- c( 50, -70)
  character.18.02$velocity       <- c(  6,   0)
  character.18.02$orientation    <- (pi / 2)
  character.18.02$max.velocity   <- 8
  character.18.02$max.linear     <- 3
  character.18.02$target         <- 1
 
  character.18.03                <- character.0
  character.18.03$id             <- 1803
  character.18.03$steer          <- STOP
  character.18.03$position       <- c(  0, -20)
  
  character.18.04                <- character.0
  character.18.04$id             <- 1804
  character.18.04$steer          <- ARRIVE
  character.18.04$position       <- c( 30,  50)
  character.18.04$velocity       <- c( -4,   6)
  character.18.04$orientation    <- (pi / 2)
  character.18.04$max.velocity   <- 8
  character.18.04$max.linear     <- 3
  character.18.04$target         <- 3
  character.18.04$arrive.radius  <- 4
  character.18.04$arrive.slow    <- 20
  character.18.04$arrive.time    <- 1
  
  Character        <- list(character.18.01, character.18.02, character.18.03, character.18.04)
  characters       <- 4

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 100    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE) # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                        # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 19) {

  character.19.01                <- character.0
  character.19.01$id             <- 1901
  character.19.01$steer          <- CONTINUE
  character.19.01$position       <- c(-20, -15)
  character.19.01$velocity       <- c( 0.25, 1)
  character.19.01$orientation    <- (pi / 2)
  character.19.01$max.velocity   <- 2
  character.19.01$max.linear     <- 2
  character.19.01$avoid.radius   <- 2
  character.19.01$col.radius     <- 0.5
  
  character.19.02                <- character.0
  character.19.02$id             <- 1902
  character.19.02$steer          <- AVOID.COLLISIONS
  character.19.02$position       <- c(-72, -18)
  character.19.02$velocity       <- c(  1,   1)
  character.19.02$orientation    <- (pi / 2)
  character.19.02$max.velocity   <- 2
  character.19.02$max.linear     <- 0.75
  character.19.02$avoid.radius   <- 2
  character.19.02$col.radius     <- 0.5
  character.19.02$col.lookahead  <- 8
  
  character.19.03                <- character.0
  character.19.03$id             <- 1903
  character.19.03$steer          <- PURSUE
  character.19.03$position       <- c( 60, -65)
  character.19.03$velocity       <- c( 0, -2.5)
  character.19.03$orientation    <- (pi / 2)
  character.19.03$max.velocity   <- 2.1
  character.19.03$max.linear     <- 1
  character.19.03$target         <- 1
  character.19.03$max.prediction <- 30
  character.19.03$avoid.radius   <- 2
  character.19.03$col.radius     <- 0.5

  Character        <- list(character.19.01, character.19.02, character.19.03)
  characters       <- 3

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 100    # Time of last time step
  check.collisions <- TRUE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                         # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE
  
} else if (scenario == 20) {

  character.20.01                <- character.0
  character.20.01$id             <- 2001
  character.20.01$steer          <- CONTINUE
  character.20.01$position       <- c( 75,  60)
  character.20.01$velocity       <- c( -4,   0)
  character.20.01$max.velocity   <- 4

  character.20.02                <- character.0 
  character.20.02$id             <- 52
  character.20.02$steer          <- SEEK
  character.20.02$position       <- c( 35, -55)
  character.20.02$orientation    <- (pi / 2)
  character.20.02$max.velocity   <- 4.10
  character.20.02$max.linear     <- 2
  character.20.02$target         <- 1
  
  Character        <- list(character.20.01, character.20.02)
  characters       <- 2

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 50     # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE) # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                        # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE
  
} else if (scenario == 21) {

  character.21.01                <- character.0
  character.21.01$id             <- 2101
  character.21.01$steer          <- CONTINUE
  character.21.01$position       <- c( 75,  60)
  character.21.01$velocity       <- c( -4,   0)
  character.21.01$max.velocity   <- 4

  character.21.02                <- character.0
  character.21.02$id             <- 2102
  character.21.02$steer          <- PURSUE
  character.21.02$position       <- c( 35, -55)
  character.21.02$orientation    <- (pi / 2)
  character.21.02$max.velocity   <- 4.10
  character.21.02$max.linear     <- 2
  character.21.02$target         <- 1
  character.21.02$max.prediction <- 30
  
  Character        <- list(character.21.01, character.21.02)
  characters       <- 2

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 50     # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE) # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                         # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 22) {

  character.22.01                <- character.0
  character.22.01$id             <- 2201
  character.22.01$steer          <- SWIRL
  character.22.01$position       <- c(  0,  60)
  character.22.01$velocity       <- c( -8,  -4)
  character.22.01$linear         <- c(0, 1)
  character.22.01$rotation       <- 0.2
  character.22.01$max.velocity   <- 48
  character.22.01$max.linear     <- 6
  character.22.01$swirl.scale    <- c(9, 3)

  Character        <- list(character.22.01)
  characters       <- 1

  physics          <- TRUE   # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 100    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE) # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 6.0)                        # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE
  
} else if (scenario == 23) {

  character.23.01                <- character.0
  character.23.01$id             <- 2301
  character.23.01$steer          <- STOP
  character.23.01$position       <- c( 20,   0)
  character.23.01$orientation    <- 3 * pi / 4
  
  character.23.02                <- character.0
  character.23.02$id             <- 2302
  character.23.02$steer          <- FACE.TARGET
  character.23.02$position       <- c(-40, -75)
  character.23.02$velocity       <- c(  0,   5)
  character.23.02$orientation    <- 3* pi / 4
  character.23.02$max.velocity   <- 6
  character.23.02$max.linear     <- 0
  character.23.02$max.rotation   <- pi / 16
  character.23.02$max.angular    <- pi / 32
  character.23.02$target         <- 1
  character.23.02$align.radius   <- pi / 64
  character.23.02$align.slow     <- pi / 16
  character.23.02$align.time     <- 1
  
  character.23.03                <- character.0
  character.23.03$id             <- 2303
  character.23.03$steer          <- ALIGN
  character.23.03$position       <- c(-55, -85)
  character.23.03$velocity       <- c(  0,   5)
  character.23.03$orientation    <- 7 * pi / 4
  character.23.03$max.velocity   <- 6
  character.23.03$max.linear     <- 0
  character.23.03$max.rotation   <- pi / 16
  character.23.03$max.angular    <- pi / 32
  character.23.03$target         <- 1
  character.23.03$align.radius   <- pi / 64
  character.23.03$align.slow     <- pi / 16
  character.23.03$align.time     <- 1
  
  character.23.04                <- character.0
  character.23.04$id             <- 2304
  character.23.04$steer          <- FACE.MOVEMENT
  character.23.04$position       <- c(-70, -95)
  character.23.04$velocity       <- c(  0,   5)
  character.23.04$orientation    <- 5 * pi / 4
  character.23.04$max.velocity   <- 6
  character.23.04$max.linear     <- 0
  character.23.04$max.rotation   <- pi / 32
  character.23.04$max.angular    <- pi / 64
  character.23.04$target         <- 1
  character.23.04$align.radius   <- pi / 64
  character.23.04$align.slow     <- pi / 16
  character.23.04$align.time     <- 1
  
  Character        <- list(character.23.01, character.23.02, character.23.03, character.23.04)
  characters       <- 4

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 35     # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 6.0)                           # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE
  
} else if (scenario == 24) {

  character.24.01                    <- character.0
  character.24.01$id                 <- 2401
  character.24.01$steer              <- WANDER
  character.24.01$position           <- c(-60, -50)
  character.24.01$velocity           <- c(  0,   0)
  character.24.01$orientation        <- 3 * pi / 4
  character.24.01$max.velocity       <- 5
  character.24.01$max.linear         <- 2
  character.24.01$max.rotation       <- pi / 4
  character.24.01$max.angular        <- pi / 8
  character.24.01$align.radius       <- pi / 64
  character.24.01$align.slow         <- pi / 16
  character.24.01$align.time         <- 1
  character.24.01$wander.offset      <- 4
  character.24.01$wander.radius      <- 16
  character.24.01$wander.rate        <- pi / 4
  character.24.01$wander.orientation <- 0
  
  character.24.02                    <- character.0
  character.24.02$id                 <- 2402
  character.24.02$steer              <- WANDER
  character.24.02$position           <- c(-20, -15)
  character.24.02$velocity           <- c(  0,   0)
  character.24.02$orientation        <- 5 * pi / 5
  character.24.02$max.velocity       <- 5
  character.24.02$max.linear         <- 2
  character.24.02$max.rotation       <- pi / 4
  character.24.02$max.angular        <- pi / 8
  character.24.02$align.radius       <- pi / 64
  character.24.02$align.slow         <- pi / 16
  character.24.02$align.time         <- 1
  character.24.02$wander.offset      <- 4
  character.24.02$wander.radius      <- 16
  character.24.02$wander.rate        <- pi / 4
  character.24.02$wander.orientation <- 0 

  character.24.03                    <- character.0
  character.24.03$id                 <- 2403
  character.24.03$steer              <- WANDER
  character.24.03$position           <- c( 20,  15)
  character.24.03$velocity           <- c(  0,   0)
  character.24.03$orientation        <- 7 * pi / 4
  character.24.03$max.velocity       <- 5
  character.24.03$max.linear         <- 2
  character.24.03$max.rotation       <- pi / 4
  character.24.03$max.angular        <- pi / 8
  character.24.03$align.radius       <- pi / 64
  character.24.03$align.slow         <- pi / 16
  character.24.03$align.time         <- 1
  character.24.03$wander.offset      <- 4
  character.24.03$wander.radius      <- 16
  character.24.03$wander.rate        <- pi / 4
  character.24.03$wander.orientation <- 0
  
  character.24.04                    <- character.0
  character.24.04$id                 <- 2404
  character.24.04$steer              <- WANDER
  character.24.04$position           <- c( 60,  50)
  character.24.04$velocity           <- c(  0,   0)
  character.24.04$orientation        <- 1 * pi / 4
  character.24.04$max.velocity       <- 5
  character.24.04$max.linear         <- 2
  character.24.04$max.rotation       <- pi / 4
  character.24.04$max.angular        <- pi / 8
  character.24.04$align.radius       <- pi / 64
  character.24.04$align.slow         <- pi / 16
  character.24.04$align.time         <- 1
  character.24.04$wander.offset      <- 4
  character.24.04$wander.radius      <- 16
  character.24.04$wander.rate        <- pi / 4
  character.24.04$wander.orientation <- 0 
 
 
  Character        <- list(character.24.01, character.24.02, character.24.03, character.24.04)
  characters       <- 4

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 100    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 6.0)                          # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE
  
} else if (scenario == 25) {

  character.25.01                <- character.0
  character.25.01$id             <- 2501
  character.25.01$steer          <- FOLLOW.PATH
  character.25.01$position       <- c(-10,  50)
  character.25.01$max.velocity   <- 3
  character.25.01$max.linear     <- 1
  character.25.01$path.to.follow <- 1
  character.25.01$path.offset    <- 0.05
  
  Character        <- list(character.25.01)
  characters       <- 1

  path.1.x  <- rev(c( 20,  20,  60,  60,  20,  20, -60, -60, -10))
  path.1.y  <- rev(c( 80,  25,  25, -15, -15, -50, -50,  40,  40))

  path.1           <- path.assemble(251, path.1.x, path.1.y)
  Path             <- list(path.1)
  paths            <- 1

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 135    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(6.0, 6.0, 2.0)                         # Scale factor for VLO vectors in plot
  plot.cross.refs  <- FALSE

} else if (scenario == 26) {

  character.26.01                <- character.0
  character.26.01$id             <- 2601
  character.26.01$steer          <- CONTINUE

  character.26.02                <- character.0
  character.26.02$id             <- 2602
  character.26.02$steer          <- FLEE
  character.26.02$position       <- c(-30, -50)
  character.26.02$velocity       <- c(  2,   7)
  character.26.02$orientation    <- (pi / 4)
  character.26.02$max.velocity   <- 8
  character.26.02$max.linear     <- 1.5
  character.26.02$target         <- 1
  
  character.26.03                <- character.0
  character.26.03$id             <- 2603
  character.26.03$steer          <- SEEK
  character.26.03$position       <- c(-50,  40)
  character.26.03$velocity       <- c(  0,   8)
  character.26.03$orientation    <- (3 * pi / 2)
  character.26.03$max.velocity   <- 8
  character.26.03$max.linear     <- 2
  character.26.03$target         <- 1

  character.26.04                <- character.0
  character.26.04$id             <- 2604
  character.26.04$steer          <- ARRIVE
  character.26.04$position       <- c( 50,  75)
  character.26.04$velocity       <- c( -9,   4)
  character.26.04$orientation    <- pi
  character.26.04$max.velocity   <- 10
  character.26.04$max.linear     <- 2
  character.26.04$target         <- 1
  character.26.04$arrive.radius  <- 4
  character.26.04$arrive.slow    <- 32
  character.26.04$arrive.time    <- 1

  Character        <- list(character.26.01, character.26.02, character.26.03, character.26.04)
  characters       <- 4

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 50     # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE) # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                         # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 27) {

  # Scenario 27; most character data removed in version posted to Canvas

  character.27.01                <- character.0
  character.27.01$id             <- 2701
  character.27.01$steer          <- FOLLOW.PATH
  character.27.01$position       <- c( 20,  95)
  character.27.01$max.velocity   <- 4
  character.27.01$max.linear     <- 2
  character.27.01$path.to.follow <- 1
  character.27.01$path.offset    <- 0.04
                       
  Character        <- list(character.27.01)
  characters       <- 1

  path.1.x         <- c(  0, -20, 20, -40,  40, -60,  60,   0)
  path.1.y         <- c( 90,  65, 40,  15, -10, -35, -60, -85)
  path.1           <- path.assemble(2701, path.1.x, path.1.y)
  Path             <- list(path.1)
  paths            <- 1

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 125    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE) # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(4.0, 4.0, 2.0)                        # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE
  
} else if (scenario == 28) {

  character.28.01                <- character.0
  character.28.01$id             <- 2801
  character.28.01$steer          <- STOP
  character.28.01$position       <- c(  0, -20)
  
  character.28.02                <- character.0
  character.28.02$id             <- 2802
  character.28.02$steer          <- SEEK
  character.28.02$position       <- c(-70,  50)
  character.28.02$velocity       <- c( -7,  -2)
  character.28.02$orientation    <- (pi / 2)
  character.28.02$max.velocity   <- 8
  character.28.02$max.linear     <- 2.5
  character.28.02$target         <- 1
 
  character.28.03                <- character.0
  character.28.03$id             <- 2803
  character.28.03$steer          <- ARRIVE
  character.28.03$position       <- c( 50,  30)
  character.28.03$velocity       <- c(  8,  -1)
  character.28.03$orientation    <- (pi / 2)
  character.28.03$max.velocity   <- 8
  character.28.03$max.linear     <- 3
  character.28.03$target         <- 1
  character.28.03$arrive.radius  <- 4
  character.28.03$arrive.slow    <- 20
  character.28.03$arrive.time    <- 1
  
  character.28.04                    <- character.0
  character.28.04$id                 <- 2408
  character.28.04$steer              <- WANDER
  character.28.04$position           <- c( 45,  60)
  character.28.04$velocity           <- c(  0,   0)
  character.28.04$orientation        <- 1 * pi / 4
  character.28.04$max.velocity       <- 5
  character.28.04$max.linear         <- 2
  character.28.04$max.rotation       <- pi / 4
  character.28.04$max.angular        <- pi / 8
  character.28.04$align.radius       <- pi / 64
  character.28.04$align.slow         <- pi / 16
  character.28.04$align.time         <- 1
  character.28.04$wander.offset      <- 4
  character.28.04$wander.radius      <- 16
  character.28.04$wander.rate        <- pi / 4
  character.28.04$wander.orientation <- 0 
  
  Character        <- list(character.28.01, character.28.02, character.28.03, character.28.04)
  characters       <- 4

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 50    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE) # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(2.0, 2.0, 2.0)                        # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 29) {

  character.29.01                <- character.0
  character.29.01$id             <- 2901
  character.29.01$steer          <- CONTINUE
  character.29.01$position       <- c(-40, -50)
  character.29.01$velocity       <- c(  0,   2)
  character.29.01$orientation    <- (pi / 2)
  character.29.01$max.velocity   <- 2
  character.29.01$max.linear     <- 2
  character.29.01$avoid.radius   <- 2
  character.29.01$col.radius     <- 0.5
 
  character.29.02                <- character.0
  character.29.02$id             <- 2902
  character.29.02$steer          <- PURSUE
  character.29.02$position       <- c( 45, -30)
  character.29.02$velocity       <- c(  0,  -3)
  character.29.02$orientation    <- (pi / 2)
  character.29.02$max.velocity   <- 3
  character.29.02$max.linear     <- 1
  character.29.02$target         <- 3
  character.29.02$max.prediction <- 10
  character.29.02$avoid.radius   <- 2
  character.29.02$col.radius     <- 0.5
 
  character.29.03                <- character.0
  character.29.03$id             <- 2903
  character.29.03$steer          <- AVOID.COLLISIONS
  character.29.03$position       <- c( 45,  30)
  character.29.03$velocity       <- c( -2,   0)
  character.29.03$orientation    <- (pi / 2)
  character.29.03$max.velocity   <- 2
  character.29.03$max.linear     <- 2.5
  character.29.03$avoid.radius   <- 2
  character.29.03$col.radius     <- 0.5
  character.29.03$col.lookahead  <- 12
  
  Character        <- list(character.29.01, character.29.02, character.29.03)
  characters       <- 3

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 50    # Time of last time step
  check.collisions <- TRUE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(4.0, 4.0, 2.0)                         # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 30) {

  character.30.01                <- character.0
  character.30.01$id             <- 3001
  character.30.01$steer          <- CONTINUE
  character.30.01$position       <- c(  4,   0)
  character.30.01$velocity       <- c( -2,   0)
  character.30.01$orientation    <- 0
  character.30.01$max.velocity   <- 2
  character.30.01$max.linear     <- 0
  
  character.30.02                <- character.0
  character.30.02$id             <- 3002
  character.30.02$steer          <- SEEK
  character.30.02$position       <- c(  0,  -8)
  character.30.02$velocity       <- c(  0,   0)
  character.30.02$orientation    <- 0
  character.30.02$max.velocity   <- 4
  character.30.02$max.linear     <- 1
  character.30.02$target         <- 1

  character.30.03                <- character.0
  character.30.03$id             <- 3003
  character.30.03$steer          <- PURSUE
  character.30.03$position       <- c(  0,   8)
  character.30.03$velocity       <- c(  0,   0)
  character.30.03$orientation    <- 0
  character.30.03$max.velocity   <- 4
  character.30.03$max.linear     <- 1
  character.30.03$target         <- 1
  character.30.03$max.prediction <- 10
  
  Character        <- list(character.30.01, character.30.02, character.30.03)
  characters       <- 3

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 0.50   # Duration of time step
  stop.time        <- 20     # Time of last time step
  check.collisions <- TRUE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(4.0, 4.0, 2.0)                         # Scale factor for VLO vectors in plot
  plot.cross.refs  <- TRUE

} else if (scenario == 31) {

  character.31.01                <- character.0
  character.31.01$id             <- 3101
  character.31.01$steer          <- FOLLOW.PATH
  character.31.01$position       <- c(-125, -15)
  character.31.01$max.velocity   <- 2
  character.31.01$max.linear     <- 0.75
  character.31.01$path.to.follow <- 1
  character.31.01$path.offset    <- 0.05
  
  character.31.02                <- character.0
  character.31.02$id             <- 3102
  character.31.02$steer          <- FOLLOW.PATH
  character.31.02$position       <- c(-125,  -5)
  character.31.02$max.velocity   <- 2
  character.31.02$max.linear     <- 0.75
  character.31.02$path.to.follow <- 2
  character.31.02$path.offset    <- 0.05
  
  character.31.03                <- character.0
  character.31.03$id             <- 3103
  character.31.03$steer          <- FOLLOW.PATH
  character.31.03$position       <- c(-125,  5)
  character.31.03$max.velocity   <- 2
  character.31.03$max.linear     <- 0.75
  character.31.03$path.to.follow <- 3
  character.31.03$path.offset    <- 0.05
  
  character.31.04                <- character.0
  character.31.04$id             <- 3104
  character.31.04$steer          <- FOLLOW.PATH
  character.31.04$position       <- c(-125, 15)
  character.31.04$max.velocity   <- 2.25
  character.31.04$max.linear     <- 0.75
  character.31.04$path.to.follow <- 4
  character.31.04$path.offset    <- 0.05
  
  Character        <- list(character.31.01, character.31.02, character.31.03, character.31.04)
  characters       <- 4

  path.1.x  <- c( -95, -85, -47, -34, -25,  20,  50,  55,  75, 115)
  path.1.y  <- c( -15, -20, -40, -28, -40, -45, -40, -30, -20, -15)
  
  path.2.x  <- c( -95, -85, -20,  -6,  -2,  60, 115)
  path.2.y  <- c(  -5,   0, -19,  -5, -22, -10,  -5)
  
  path.3.x  <- c( -95, -85, -44, -37, -17, -10, 10, 35, 61, 80, 115)
  path.3.y  <- c(   5,  25,  13,  25,  30,  10,  9, 14,  9, 15, 5)  
  
  path.4.x  <- c( -95, -85, -50, -45,  15,  35,  70,  90, 115)
  path.4.y  <- c(  15,  45,  35,  55,  60,  40,  55,  55,  15)  
  
  path.1           <- path.assemble(311, path.1.x, path.1.y)
  path.2           <- path.assemble(312, path.2.x, path.2.y)
  path.3           <- path.assemble(313, path.3.x, path.3.y)
  path.4           <- path.assemble(314, path.4.x, path.4.y)
  Path             <- list(path.1, path.2, path.3, path.4)
  paths            <- 4

  physics          <- FALSE  # TRUE=HS physics, FALSE=NE1 integration
  delta.time       <- 1.00   # Duration of time step
  stop.time        <- 126    # Time of last time step
  check.collisions <- FALSE

  plot.what        <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)  # Plot position, velocity, linear, orientation, paths, collisions 
  plot.scale       <- c(8.0, 8.0, 2.0)                         # Scale factor for VLO vectors in plot
  plot.cross.refs  <- FALSE

}

# End of program
