# Author: Isaiah Harville
# Purpose: Initialize dynamic movement algorithms.

import numpy as np
import os

scenario = 26
trajectory_file = "Trajectory_Data.txt"

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


# Steering behavior constants
CONTINUE = 1
STOP = 2
ALIGN = 3
FACE_TARGET = 4
FACE_MOVEMENT = 5
SEEK = 6
FLEE = 7
ARRIVE = 8
PURSUE = 9
WANDER = 10
FOLLOW_PATH = 11
SEPARATE = 12
AVOID_COLLISIONS = 13
SWIRL = 14

# General movement parameters and working variables
Time = 0
stop_velocity = 0.02

# Initialize generic character
character0 = {
    "id": 0,
    "steer": STOP,
    "position": np.array([0, 0]),
    "velocity": np.array([0, 0]),
    "linear": np.array([0, 0]),
    "orientation": 0,
    "rotation": 0,
    "angular": 0,
    "max_velocity": 0,
    "max_linear": 0,
    "max_rotation": 0,
    "max_angular": 0,
    "target": 0,
    "arrive_radius": 0,
    "arrive_slow": 0,
    "arrive_time": 0,
    "align_radius": 0,
    "align_slow": 0,
    "align_time": 0,
    "max_prediction": 0,
    "avoid_radius": 0,
    "col_radius": 0,
    "col_lookahead": 0,
    "col_collided": False,
    "wander_offset": 0,
    "wander_radius": 0,
    "wander_rate": 0,
    "wander_orientation": 0,
    "path_to_follow": 0,
    "path_offset": 0,
    "sep_decay": 0,
    "sep_threshold": 0,
    "swirl_scale": np.array([0, 0])
}

# Initialize scenario-specific variables, including characters, targets, and paths.
if (scenario == 26):
    import math

    # Define a template character
    character_0 = {}

    # Create character instances
    character_26_01 = character0.copy()
    character_26_01['id'] = 2601
    character_26_01['steer'] = CONTINUE

    character_26_02 = character0.copy()
    character_26_02['id'] = 2602
    character_26_02['steer'] = FLEE
    character_26_02['position'] = np.array([-30, -50])  # Initialize as numpy array
    character_26_02['velocity'] = np.array([2, 7])   
    character_26_02['orientation'] = math.pi / 4
    character_26_02['max_velocity'] = 8
    character_26_02['max_linear'] = 1.5
    character_26_02['target'] = 1

    character_26_03 = character0.copy()
    character_26_03['id'] = 2603
    character_26_03['steer'] = SEEK
    character_26_03['position'] = np.array([-50, 40])   # Initialize as numpy array
    character_26_03['velocity'] = np.array([0, 8])
    character_26_03['orientation'] = 3 * math.pi / 2
    character_26_03['max_velocity'] = 8
    character_26_03['max_linear'] = 2
    character_26_03['target'] = 1

    character_26_04 = character0.copy()
    character_26_04['id'] = 2604
    character_26_04['steer'] = ARRIVE
    character_26_04['position'] = np.array([50, 75])    # Initialize as numpy array
    character_26_04['velocity'] = np.array([-9, 4]) 
    character_26_04['orientation'] = math.pi
    character_26_04['max_velocity'] = 10
    character_26_04['max_linear'] = 2
    character_26_04['target'] = 1
    character_26_04['arrive_radius'] = 4
    character_26_04['arrive_slow'] = 32
    character_26_04['arrive_time'] = 1

    # Combine characters into a list
    Character = [character_26_01, character_26_02, character_26_03, character_26_04]
    characters_count = 4

    # Other parameters
    physics = False  # True for HS physics, False for NE1 integration
    delta_time = 0.50  # Duration of time step
    stop_time = 50  # Time of last time step
    check_collisions = False

    # Plot parameters
    plot_what = {
        "position": True,
        "velocity": True,
        "linear": True,
        "orientation": False,
        "paths": False,
        "collisions": False
    }
    plot_scale = [2.0, 2.0, 2.0]  # Scale factor for VLO vectors in plot
    plot_cross_refs = True

