# Author: Isaiah Harville
# Purpose: State Machine
# Date 11/16/2023

import random
import numpy as np

# Define the states
FOLLOW = 1
PULL_OUT = 2
ACCELERATE = 3
PULL_IN_AHEAD = 4
PULL_IN_BEHIND = 5
DECELERATE = 6
DONE = 7

# Initialize program state and transition counters
state_count = [0] * 7
transition_count = [0] * 9

# Select scenario and set scenario-specific parameters
SCENARIO = 1 # 1 or 2
trace = [True, False][SCENARIO - 1]
iterations = [100, 1000000][SCENARIO - 1]
transition_probability = [
    [0.8, 0.4, 0.3, 0.4, 0.3, 0.3, 0.8, 0.8, 0.8],
    [0.9, 0.6, 0.3, 0.2, 0.2, 0.4, 0.7, 0.9, 0.7]
][SCENARIO - 1]

# Define state "action" functions (stubs)
def follow_action():
    state_count[FOLLOW - 1] += 1

def pull_out_action():
    state_count[PULL_OUT - 1] += 1

def accelerate_action():
    state_count[ACCELERATE - 1] += 1

def pull_in_ahead_action():
    state_count[PULL_IN_AHEAD - 1] += 1

def pull_in_behind_action():
    state_count[PULL_IN_BEHIND - 1] += 1

def decelerate_action():
    state_count[DECELERATE - 1] += 1

def done_action():
    state_count[DONE - 1] += 1


# Execute iterations and transitions
for i in range(1, iterations + 1):
    state = FOLLOW
    follow_action()

    while state != DONE:
        R = random.uniform(0.0, 1.0)

        # Check transitions
        if state == FOLLOW:
            if R < transition_probability[0]:
                transition_count[0] += 1
                state = PULL_OUT
                pull_out_action()
            else:
                state = FOLLOW
                follow_action()

        elif state == PULL_OUT:
            if R < transition_probability[1]:
                transition_count[1] += 1
                state = ACCELERATE
                accelerate_action()
            elif R < sum(transition_probability[:3]):
                transition_count[2] += 1
                state = PULL_IN_BEHIND
                pull_in_behind_action()
            else:
                state = PULL_OUT
                pull_out_action()

        elif state == ACCELERATE:
            if R < transition_probability[2]:
                transition_count[2] += 1
                state = PULL_IN_AHEAD
                pull_in_ahead_action()
            elif R < sum(transition_probability[:4]):
                transition_count[3] += 1
                state = PULL_IN_BEHIND
                pull_in_behind_action()
            elif R < sum(transition_probability[:5]):
                transition_count[4] += 1
                state = DECELERATE
                decelerate_action()
            else:
                state = ACCELERATE
                accelerate_action()

        elif state == PULL_IN_AHEAD:
            if R < transition_probability[8]:
                transition_count[8] += 1
                state = DONE
                done_action()
            else:
                state = PULL_IN_AHEAD
                pull_in_ahead_action()

        elif state == PULL_IN_BEHIND:
            if R < transition_probability[6]:
                transition_count[6] += 1
                state = FOLLOW
                follow_action()
            else:
                state = PULL_IN_BEHIND
                pull_in_behind_action()

        elif state == DECELERATE:
            if R < transition_probability[7]:
                transition_count[7] += 1
                state = PULL_IN_BEHIND
                pull_in_behind_action()
            else:
                state = DECELERATE
                decelerate_action()

        elif state == DONE:
            print("Error: unexpected state value=", state)
            break

        else:
            print("Error: unexpected state value=", state)
            break

# Define state and transition sequences
state_sequence = [list(range(1, 8)), [7] + list(range(1, 7))][SCENARIO-1]
transition_sequence = list(range(1, 10))  

# Calculate state and transition frequencies
state_frequencies = np.array(state_count) / sum(state_count)
state_frequency = state_frequencies[[i - 1 for i in state_sequence]]
transition_frequencies = np.array(transition_count) / sum(transition_count)
transition_frequency = transition_frequencies[[i - 1 for i in transition_sequence]]

# Write output to file
with open('output_scenario%g.txt'%SCENARIO, 'w') as output_file:
    output_file.write(f"scenario                = {SCENARIO}\n")
    output_file.write(f"trace                   = {trace}\n")
    output_file.write(f"iterations              = {iterations}\n")
    output_file.write(f"transition probabilities= {' '.join(map(str, transition_probability))}\n")
    output_file.write(f"state counts            = {' '.join(map(str, state_count))}\n")
    output_file.write(f"state frequencies       = {' '.join(map(lambda x: f'{x:.3f}', state_frequency))}\n")
    output_file.write(f"transition counts       = {' '.join(map(str, transition_count))}\n")
    output_file.write(f"transition frequencies  = {' '.join(map(lambda x: f'{x:.3f}', transition_frequency))}\n")

# Close output file
output_file.close()