#########################################
# Programming Languages, 2019-2020		#
# Set 2, Exercise 3: stayhome			#
# Language: python3						#
# Polyvios Papakonstantinou 03114892	#
#########################################

import sys
from os import path
import queue as Q

class sotiris(object):
    def __init__(self, path, x, y):
        self.path = path
        self.x = x
        self.y = y
        return

class virus(object):
    def __init__(self, time, x, y):
        self.time = time
        self.x = x
        self.y = y
        return


#Open the input file.
filePath = path.relpath(sys.argv[1])
inputFile = open(filePath, 'r')

#Create 2 queues, one for Sotiris and one for the virus.
sQueue = Q.Queue()
vQueue = Q.Queue()

lines = inputFile.readlines()

#Create the empty grid in which we will store the lines.
grid = []
for line in lines:
    #Add 2 'X's at the start and at the end of every line...
    grid.append(['X'] + list(line.strip()) + ['X'])

#...and a whole row of 'X's at the start and at the end of the grid.
tmpRow = len(grid[0])*['X']
grid.insert(0, tmpRow)
grid.insert(len(grid), tmpRow)

#Scan the whole grid to find Sotiris' and the virus'blocks
for i in range(len(grid)):
    for j in range(len(grid[i])):
        if (grid[i][j] == 'S'):
            sQueue.put(sotiris([], i, j))

        if (grid[i][j] == 'W'):
            vQueue.put(virus(0, i, j))

sotiris_ = 1
gotHome = 0
time = 1
arrTime = -1

while (1):
	
    expanded = 0
    tmpQueue = Q.Queue()

    while (not sQueue.empty()):
        #We will move Sotiris.
        tmpSotiris = sQueue.get()

        #If the virus hasn't got to this block yet...
        if grid[tmpSotiris.x][tmpSotiris.y] != 'W':

            #down.
            if (grid[tmpSotiris.x + 1][tmpSotiris.y] == '.'):
                grid[tmpSotiris.x + 1][tmpSotiris.y] = 'S'
                sotiris_ += 1
                tmpQueue.put(sotiris(tmpSotiris.path + ['D'], tmpSotiris.x + 1, tmpSotiris.y))
                expanded = 1

            if (grid[tmpSotiris.x + 1][tmpSotiris.y] == 'A'):
                sotiris_ += 1
                tmpQueue.put(sotiris(tmpSotiris.path + ['D'], tmpSotiris.x + 1, tmpSotiris.y))
                expanded = 1

            if (grid[tmpSotiris.x + 1][tmpSotiris.y] == 'T'):
                tmpSotiris.path += ['D']
                gotHome = 1
                break;

            #left.
            if (grid[tmpSotiris.x][tmpSotiris.y - 1] == '.'):
                grid[tmpSotiris.x][tmpSotiris.y - 1] = 'S'
                sotiris_ += 1
                tmpQueue.put(sotiris(tmpSotiris.path + ['L'], tmpSotiris.x, tmpSotiris.y - 1))
                expanded = 1

            if (grid[tmpSotiris.x][tmpSotiris.y - 1] == '.'):
                sotiris_ += 1
                tmpQueue.put(sotiris(tmpSotiris.path + ['L'], tmpSotiris.x, tmpSotiris.y - 1))
                expanded = 1

            if (grid[tmpSotiris.x][tmpSotiris.y - 1] == 'T'):
                tmpSotiris.path += ['L']
                gotHome = 1
                break;

            #right.
            if (grid[tmpSotiris.x][tmpSotiris.y + 1] == '.'):
                grid[tmpSotiris.x][tmpSotiris.y + 1] = 'S'
                sotiris_ += 1
                tmpQueue.put(sotiris(tmpSotiris.path + ['R'], tmpSotiris.x, tmpSotiris.y + 1))
                expanded = 1

            if (grid[tmpSotiris.x][tmpSotiris.y + 1] == '.'):
                sotiris_ += 1
                tmpQueue.put(sotiris(tmpSotiris.path + ['R'], tmpSotiris.x, tmpSotiris.y + 1))
                expanded = 1

            if (grid[tmpSotiris.x][tmpSotiris.y + 1] == 'T'):
                tmpSotiris.path += ['R']
                gotHome = 1
                break;

            #up.
            if (grid[tmpSotiris.x - 1][tmpSotiris.y] == '.'):
                grid[tmpSotiris.x - 1][tmpSotiris.y] = 'S'
                sotiris_ += 1
                tmpQueue.put(sotiris(tmpSotiris.path + ['U'], tmpSotiris.x - 1, tmpSotiris.y))
                expanded = 1

            if (grid[tmpSotiris.x - 1][tmpSotiris.y] == '.'):
                sotiris_ += 1
                tmpQueue.put(sotiris(tmpSotiris.path + ['U'], tmpSotiris.x - 1, tmpSotiris.y))
                expanded = 1

            if (grid[tmpSotiris.x - 1][tmpSotiris.y] == 'T'):
                tmpSotiris.path += ['U']
                gotHome = 1
                break;       

    sQueue = tmpQueue

    tmpQueue = Q.Queue()

    while (not vQueue.empty()):
        #We will move the virus.
        tmpVirus = vQueue.get()

        if ((time - tmpVirus.time) % 2 == 0):
            #down.
            if (grid[tmpVirus.x + 1][tmpVirus.y] == '.'):
                grid[tmpVirus.x + 1][tmpVirus.y] = 'W'
                tmpQueue.put(virus(time, tmpVirus.x + 1, tmpVirus.y))

            if (grid[tmpVirus.x + 1][tmpVirus.y] == 'S'):
                grid[tmpVirus.x + 1][tmpVirus.y] = 'W'
                sotiris_ -= 1
                tmpQueue.put(virus(time, tmpVirus.x + 1, tmpVirus.y))

            if (grid[tmpVirus.x + 1][tmpVirus.y] == 'A'):
                grid[tmpVirus.x + 1][tmpVirus.y] = 'W'
                arrTime = time + 5

            #up.
            if (grid[tmpVirus.x - 1][tmpVirus.y] == '.'):
                grid[tmpVirus.x - 1][tmpVirus.y] = 'W'
                tmpQueue.put(virus(time, tmpVirus.x - 1, tmpVirus.y))

            if (grid[tmpVirus.x - 1][tmpVirus.y] == 'S'):
                grid[tmpVirus.x - 1][tmpVirus.y] = 'W'
                sotiris_ -= 1
                tmpQueue.put(virus(time, tmpVirus.x - 1, tmpVirus.y))

            if (grid[tmpVirus.x - 1][tmpVirus.y] == 'A'):
                grid[tmpVirus.x - 1][tmpVirus.y] = 'W'
                arrTime = time + 5

            #right.
            if (grid[tmpVirus.x][tmpVirus.y + 1] == '.'):
                grid[tmpVirus.x][tmpVirus.y + 1] = 'W'
                tmpQueue.put(virus(time, tmpVirus.x, tmpVirus.y + 1))

            if (grid[tmpVirus.x][tmpVirus.y + 1] == 'S'):
                grid[tmpVirus.x][tmpVirus.y + 1] = 'W'
                sotiris_ -= 1
                tmpQueue.put(virus(time, tmpVirus.x, tmpVirus.y + 1))

            if (grid[tmpVirus.x][tmpVirus.y + 1] == 'A'):
                grid[tmpVirus.x][tmpVirus.y + 1] = 'W'
                arrTime = time + 5

            #left.
            if (grid[tmpVirus.x][tmpVirus.y - 1] == '.'):
                grid[tmpVirus.x][tmpVirus.y - 1] = 'W'
                tmpQueue.put(virus(time, tmpVirus.x, tmpVirus.y - 1))

            if (grid[tmpVirus.x][tmpVirus.y - 1] == 'S'):
                grid[tmpVirus.x][tmpVirus.y - 1] = 'W'
                sotiris_ -= 1
                tmpQueue.put(virus(time, tmpVirus.x, tmpVirus.y - 1))

            if (grid[tmpVirus.x][tmpVirus.y - 1] == 'A'):
                grid[tmpVirus.x][tmpVirus.y - 1] = 'W'
                arrTime = time + 5

        else:
            tmpQueue.put(tmpVirus)

    #If the virus has arrived to the airports...
    if (time == arrTime):
        for i in range(len(grid)):
            for j in range(len(grid[i])):
                if (grid[i][j] == 'A'):
                    grid[i][j] = 'W'
                    tmpQueue.put(virus(time, i, j))

    vQueue = tmpQueue

    if (gotHome == 1):
        print(time)
        print(*(tmpSotiris.path), sep = '')
        break;

    if (sotiris_ == 0) or (expanded == 0):
        print("IMPOSSIBLE")
        break;

    time += 1