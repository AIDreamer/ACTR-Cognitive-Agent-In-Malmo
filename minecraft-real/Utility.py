## Name: Son Pham
## Summer Research 2016
## Prof. Dancy
## Utilities for ACTR-Malmo project

import sys, os
from Constants import *

# =================
# UTILITY FUNCTIONS
# =================

def flush_print():
    sys.stdout = os.fdopen(sys.stdout.fileno(), 'w', 0)  # flush print output immediately   

def create_map(LENGTH, WIDTH):
    main_map = [0] * (LENGTH * 2 - 1)
    for i in range(LENGTH * 2 - 1):
        main_map + [[0]*(WIDTH * 2 - 1)]

def determine_position(x, z):
    ''' get the unit position of the agent on the map '''
    x_unit = x // (COMBINED_LENGTH)
    x_rem = x % (COMBINED_LENGTH)
    x_pos = x_unit + (x_rem > SQUARE_WIDTH) * 0.5

    z_unit = z // (COMBINED_LENGTH)
    z_rem = z % (COMBINED_LENGTH)
    z_pos = z_unit + (z_rem > SQUARE_WIDTH) * 0.5

    return x_pos, z_pos

def within_tolerance(deg1, deg2):
    diff = abs(deg1 - deg2)
    return diff < AIM_TOLERANCE or diff > (360 - AIM_TOLERANCE)

def create_table(length, width):
    table = [None] * length
    for i in range(length):
        table[i] = [None] * width
        for j in range(width):
            table[i][j] = [-1] * 4
    return table

def create_visited_table(length, width):
    table = [False] * length
    for i in range(length):
        table[i] = [False] * width
    return table
