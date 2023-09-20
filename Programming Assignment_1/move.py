import numpy as np
from Init import *
from src import * 

# Define dynamic movement functions, aka steering behaviors.

# Continue; return current linear and angular values.
def dynamic_get_steering_continue(mover):
    return {"linear": mover["linear"], "angular": mover["angular"]}

# Stop; bring character to a stop, with slowing limited by character's maximum acceleration.
def dynamic_get_steering_stop(mover):
    result = {"linear": np.array([0.0, 0.0], dtype=np.float64), "angular": 0.0}
    result["linear"] = -mover["velocity"]
    
    if magnitude(result["linear"]) > mover["max_linear"]:
        result["linear"] = normalize(result["linear"]) * mover["max_linear"]
    
    result["angular"] = -mover["rotation"]
    return result

# Align; match orientation to orientation of target.
def dynamic_get_steering_align(mover, target):
    result = {"linear": np.array([0.0, 0.0], dtype=np.float64), "angular": 0.0}
    rotation = target["orientation"] - mover["orientation"]
    rotation = convertAngle(rotation)
    
    if abs(rotation) < mover["align_radius"]:
        result["angular"] = -result["angular"]
    
    if abs(rotation) > mover["align_slow"]:
        align_rotation = mover["max_rotation"]
    else:
        align_rotation = mover["max_rotation"] * abs(rotation) / mover["align_slow"]
    
    align_rotation = align_rotation * np.sign(rotation)
    result["angular"] = (align_rotation - mover["rotation"]) / mover["align_time"]
    
    if abs(result["angular"]) > mover["max_angular"]:
        result["angular"] = mover["max_angular"] * np.sign(result["angular"])
    
    return result

# Seek; move toward target.
def dynamic_get_steering_seek(mover, target):
    result = {"linear": np.array([0.0, 0.0], dtype=np.float64), "angular": 0.0}
    direction = target['position'] - mover['position']
    result['linear'] = normalize(direction) * mover['max_linear']
    
    return result

# Flee; move away from target.
def dynamic_get_steering_flee(mover, target):
    result = {"linear": np.array([0.0, 0.0], dtype=np.float64), "angular": 0.0}
    direction = mover['position'] - target['position']
    result['linear'] = normalize(direction) * mover['max_linear']
   
    return result

# Arrive; move toward target, slowing as distance decreases.
def dynamic_get_steering_arrive(mover, target):
    result = {"linear": np.array([0.0, 0.0], dtype=np.float64), "angular": 0.0}
    direction = target['position'] - mover['position']
    distance = magnitude(direction)
    
    if distance < mover['arrive_radius']:
        arrive_speed = 0
    elif distance > mover['arrive_slow']:
        arrive_speed = mover['max_velocity']
    else:
        arrive_speed = mover['max_velocity'] * distance / mover['arrive_slow']
    
    arrive_velocity = normalize(direction) * arrive_speed
    result['linear'] = ((arrive_velocity - mover['velocity']) / mover['arrive_time'])

    if magnitude(result['linear']) > mover['max_linear']:
        result['linear'] = normalize(result['linear']) * mover['max_linear']
    
    return result


## Dynamic Update ##
def dynamic_update(mover, steering, delta_time, physics, warnings=False, scenario=None):
    mover['position'] = np.array(mover['position']).astype(np.float64)
    mover['velocity'] =  np.array(mover['velocity']).astype(np.float64)
    steering['linear'] = np.array(steering['linear']).astype(np.float64)

    if physics:  # High School physics
        half_t_sq = 0.5 * delta_time * delta_time
        mover['position'] += mover['velocity'] * delta_time + steering['linear'] * half_t_sq
        mover['orientation'] += mover['rotation'] * delta_time + steering['angular'] * half_t_sq
    else:  # Newton-Euler-1 integration
        mover['position'] += mover['velocity'] * delta_time
        mover['orientation'] += mover['rotation'] * delta_time

    mover['orientation'] = mover['orientation'] % (2 * np.pi)

    mover['velocity'] += steering['linear'] * delta_time
    mover['rotation'] += steering['angular'] * delta_time

    mover['linear'] = steering['linear']
    mover['angular'] = steering['angular']

    # Stop moving at very low velocities; avoids jitter
    if magnitude(mover['velocity']) < stop_velocity:
        mover['velocity'] = np.array([0, 0])

    if magnitude(mover['velocity']) > mover['max_velocity']:
        if warnings:
            print(f"character exceeded max velocity scenario={scenario} mover_id={mover['id']} max_velocity={mover['max_velocity']} velocity={mover['velocity']}")
        mover['velocity'] = mover['max_velocity'] * (mover['velocity'] / magnitude(mover['velocity']))

    if magnitude(mover['linear']) > mover['max_linear']:
        if warnings:
            print(f"character exceeded max linear scenario={scenario} mover_id={mover['id']} max_linear={mover['max_linear']} linear={mover['linear']}")
        mover['linear'] = mover['max_linear'] * (mover['linear'] / magnitude(mover['linear']))

    if abs(mover['rotation']) > mover['max_rotation']:
        if warnings:
            print(f"character exceeded max rotation scenario={scenario} mover_id={mover['id']} max_rotation={mover['max_rotation']} rotation={mover['rotation']}")
        mover['rotation'] = mover['max_rotation'] * np.sign(mover['rotation'])

    if abs(mover['angular']) > mover['max_angular']:
        if warnings:
            print(f"character exceeded max angular scenario={scenario} mover_id={mover['id']} max_angular={mover['max_angular']} angular={mover['angular']}")
        mover['angular'] = mover['max_angular'] * np.sign(mover['angular'])

    return mover

def write_trajectory(character, time, trajectory_file):
    # Verifying the data
    if np.isnan(character['position']).any():
        print("NaN detected in position:", character['position'])
    if np.isnan(character['velocity']).any():
        print("NaN detected in velocity:", character['velocity'])
    if np.isnan(character['linear']).any():
        print("NaN detected in linear:", character['linear'])
  
    char_out = f"{time},{character['id']},{character['position'][0]},{character['position'][1]},{character['velocity'][0]},{character['velocity'][1]},{character['linear'][0]},{character['linear'][1]},{character['orientation']},{character['steer']},{character['col_collided']}"
    with open(trajectory_file, 'a') as file:
        file.write(char_out + "\n")


## Write initial positions and movement variables for all characters to trajectory file. ##
for char in Character:
    write_trajectory(char, Time, trajectory_file)

# Calculate trajectory, timestep by timestep.
while Time < stop_time:
    Time += delta_time

    for i in range(len(Character)):
        # Select and call a steering behavior.
        if Character[i]['steer'] == CONTINUE:
            steering = dynamic_get_steering_continue(Character[i])
        elif Character[i]['steer'] == STOP:
            steering = dynamic_get_steering_stop(Character[i])
        elif Character[i]['steer'] == SEEK:
            steering = dynamic_get_steering_seek(Character[i], Character[i]['target'])
        elif Character[i]['steer'] == FLEE:
            steering = dynamic_get_steering_flee(Character[i], Character[i]['target'])
        elif Character[i]['steer'] == ARRIVE:
            steering = dynamic_get_steering_arrive(Character[i], Character[i]['target'])

        # Update the character's movement variables.
        Character[i] = dynamic_update(Character[i], steering, delta_time, physics, warnings=False, scenario=29)

    # Check whether any characters have collided; if so, immediately stop both.
    if check_collisions:
        for i in range(len(Character) - 1):
            for j in range(i + 1, len(Character)):
                if not Character[i]['col_collided'] or not Character[j]['col_collided']:
                    col_distance = magnitude(Character[i]['position'] - Character[j]['position'])
                    col_radii = Character[i]['col_radius'] + Character[j]['col_radius']
                    if col_distance <= col_radii:
                        col_position = (Character[i]['position'] + Character[j]['position']) / 2
                        for k in [i, j]:
                            Character[k]['position'] = col_position
                            Character[k]['velocity'] = np.array([0, 0])
                            Character[k]['linear'] = np.array([0, 0])
                            Character[k]['rotation'] = 0
                            Character[k]['angular'] = 0
                            Character[k]['steer'] = STOP
                            Character[k]['col_collided'] = True

    # Write updated positions and movement variables for each character to trajectory file.
    for char in Character:
        write_trajectory(char, Time, trajectory_file)