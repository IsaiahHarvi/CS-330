import math
from matplotlib import pyplot as plt
from Init import plot_what

# Created by Joshua Payne
# Updated by Isaiah Harville on 9/9/2023

inFileName = 'Trajectory_Data.txt' #Change to any file name, if left empty the user will be prompted for the file name upon execution

class Character:
    def __init__(self, steerType):
        self.rows = 0
        self.steerType = steerType
        self.posX = []
        self.posZ = []
        self.velX = []
        self.velZ = []
        self.linX = []
        self.linZ = []
        self.orientationX = []
        self.orientationZ = []


    def plotPosition(self):
        plt.plot(self.posX, self.posZ, color = 'red', linewidth = 1.2)

        startPos = (self.posX[0] , self.posZ[0])

        circle = plt.Circle(startPos, 2, color = 'red', fill = True) 
        plt.gcf().gca().add_artist(circle)

        steeringBehaviorCode = {1 : 'Continue', 2 : 'Stop',  3 : 'Align', 6 : 'Seek', 7 : 'Flee', 8 : 'Arrive', 11 : 'Follow Path'}
        plt.text(startPos[0] + 3, startPos[1] + 1, steeringBehaviorCode[self.steerType], fontsize = 10, color = 'red')


    def plotVelocity(self):
        for i in range(self.rows):
            x = [self.posX[i], self.posX[i] + self.velX[i] * 2]
            z = [self.posZ[i], self.posZ[i] + self.velZ[i] * 2]

            plt.plot(x, z, color = 'lime', linewidth = 1)


    def plotLinear(self):
        for i in range(self.rows):
            x = [self.posX[i], self.posX[i] + self.linX[i]]
            z = [self.posZ[i], self.posZ[i] + self.linZ[i]]

            plt.plot(x, z, color = 'blue', linewidth = 1)


    def plotOrientation(self):
        for i in range(self.rows):
            x = [self.posX[i], self.posX[i] + self.orientationX[i]]
            z = [self.posZ[i], self.posZ[i] + self.orientationZ[i]]

            plt.plot(x, z, color = 'blue', linewidth = 1)

# For Path Following
class PathFollow:
    def __init__(self, x, y):
        self.x = x
        self.y = y


    def plotPath():
        followPathX = []
        followPathY = []

        followPath = [PathFollow(0, 90), PathFollow(-20, 65), PathFollow(20, 40),
                    PathFollow(-40, 15), PathFollow(40, -10), PathFollow(-60, -35),
                    PathFollow(60, -60), PathFollow(0, -85)]

        for i in followPath:
            followPathX.append(i.x)
            followPathY.append(i.y)
        
        plt.plot(followPathX, followPathY, color = 'grey', linestyle = 'dashed', linewidth = 0.9)
       
        for i in range(len(followPathX)):
            plt.annotate("({:.1f}, {:.1f})".format(followPathX[i], followPathY[i]), (followPathX[i], followPathY[i]), fontsize = 7, color = 'grey')


# Setting up graph design
plt.figure(edgecolor = 'black', figsize = [10, 10])
plt.xlim([-100, 100])
plt.ylim([-100, 100])
plt.plot([-100, 100], [0, 0], color = 'lightgrey', linestyle = 'dashed', linewidth = 2)
plt.plot([0, 0], [-100, 100], color = 'lightgrey', linestyle = 'dashed', linewidth = 2)
plt.title('Movement Trajectory', fontsize = 20)
plt.xlabel('X', fontsize = 20)
plt.ylabel('Z', fontsize = 20)

# Setting up legend
# Plot position, velocity, linear, orientation, TODO: Add paths, collisions
plt.plot([0, 0], [0, 0], color = 'red', label = 'position') if plot_what['position'] == True else None
plt.plot([0, 0], [0, 0], color = 'lime', label = 'velocity') if plot_what['velocity'] == True else None
plt.plot([0, 0], [0, 0], color = 'blue', label = 'linear') if plot_what['linear'] == True else None
plt.plot([0, 0], [0, 0], color = 'yellow', label = 'orientation') if plot_what['orientation'] == True else None
plt.plot([0, 0], [0, 0], color = 'grey', label = 'path') if plot_what['paths'] == True else None
plt.legend(loc = 'lower right')

# Opening input file
if inFileName == '':
    inFileName = input('Enter the name of the input file: ')

inFile = open(inFileName, 'r')
lines = inFile.readlines()

characters = {} # Dictionary of all characters to be plotted

for line in lines: # Reading through input file line by line
    data = line.split(',') # Splitting line up into an array of values
    data = [float(i) for i in data if i not in ["True", "False", "True\n", "False\n"]] # Converting all values to floats, except for boolean values

    ID = data[1]
    
    # If character is not already in dict, add it
    if ID not in characters:
        steerType = data[9]
        characters[ID] = Character(steerType)

    # Adding all line data to specified character
    characters[ID].rows += 1
    characters[ID].posX.append(data[2])
    characters[ID].posZ.append(data[3])
    characters[ID].velX.append(data[4])
    characters[ID].velZ.append(data[5])
    characters[ID].linX.append(data[6])
    characters[ID].linZ.append(data[7])
    characters[ID].orientationX.append(math.cos(data[8]) + data[2])
    characters[ID].orientationZ.append(math.sin(data[8]) + data[3])

# Looping through all characters and plotting their data
for ID in characters:
    character = characters[ID]

    character.plotLinear() if plot_what['linear'] == True else None
    character.plotVelocity() if plot_what['velocity'] == True else None
    character.plotPosition()  if plot_what['position'] == True else None
    character.plotOrientation() if plot_what['orientation'] == True else None
    PathFollow.plotPath() if plot_what['paths'] == True else None


plt.gca().invert_yaxis()
plt.savefig("outputPlot.png")
plt.close()