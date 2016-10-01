## Name: Son Pham
## Summer Research 2016
## Prof. Dancy
## Constants for ACTR-Malmo project

# =========
# CONSTANTS
# =========

# WORLD CONSTANT
LENGTH = 4
WIDTH = 4
SQUARE_WIDTH = 8
HEIGHT = 70
CORRIDOR_LENGTH = 12
COMBINED_LENGTH = SQUARE_WIDTH + CORRIDOR_LENGTH
MISSION_LENGTH = SQUARE_WIDTH * LENGTH + (LENGTH - 1) * CORRIDOR_LENGTH
MISSION_WIDTH = SQUARE_WIDTH * WIDTH + (LENGTH - 1) * CORRIDOR_LENGTH

# INITIAL CONSTANT
X = 1
Y = HEIGHT + 1
Z = 1
YAW = 270

# CONTROL CONSTANT
FLOOR_RADIUS = 2
FLOOR_WIDTH = FLOOR_RADIUS * 2 + 1
FLOOR_ITEMS_NUM = FLOOR_WIDTH * FLOOR_WIDTH
AIM_TOLERANCE = 10
X_LINE = [(SQUARE_WIDTH / 2 + COMBINED_LENGTH * i) for i in range(0, LENGTH)]
Z_LINE = [(SQUARE_WIDTH / 2 + COMBINED_LENGTH * i) for i in range(0, WIDTH)]
AIM_ADJUST1 = str(0.2)
AIM_ADJUST2 = str(0.5)
MIDDLE_TOLERANCE = 1
