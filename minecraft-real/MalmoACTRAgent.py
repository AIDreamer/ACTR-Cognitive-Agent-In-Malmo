## Name: Son Pham
## Summer Research 2016
## Prof. Dancy
## Agent class wrappning around Malmo Agent (to make life easier to manage)

import MalmoPython, json
from Utility import *
from Constants import *
import time

class MalmoACTRAgent(MalmoPython.AgentHost):
    
    # A bunch of states for Agent
    AGENT_TURNING = 4
    AGENT_DECIDING = 5
    AGENT_CHOOSING = 6
    AGENT_MOVING = 7

    # Decision
    DECISION_UP = 0
    DECISION_DOWN = 1
    DECISION_LEFT = 2
    DECISION_RIGHT = 3

    def __init__(self, mission, mission_record):

        # Environment variable
        self.my_mission = mission
        self.my_mission_record = mission_record

        # Malmo Agent variable
        self.agent = MalmoPython.AgentHost()
        ''' NOTE: I wasn't able to wrap around MalmoPython.AgentHost
            with class inheritance so this a way to wrap around AgentHost '''

        # Controlling variable
        self.yaw = YAW
        self.x = X
        self.y = Y
        self.z = Z
        self.x_unit = 0
        self.z_unit = 0
        self.agent_state = self.AGENT_TURNING
        self.damage_taken = 0
        self.floor_info = None

        # Aiming variable
        self.yaw_aim = 0
        self.x_unit_aim = 0
        self.z_unit_aim = 0
        self.main_line = 0

        # Decision cost variable
        self.dec_table = create_table(LENGTH, WIDTH)
        self.visited_table = create_visited_table(LENGTH, WIDTH)
        self.visited_table[0][0] = True

        # Timer and last decision (To measure decision)
        self.pre_dec_time = time.time()
        self.post_dec_time = time.time()
        self.last_dec = None
        self.pre_dec_x_unit = 0
        self.pre_dec_z_unit = 0

    def turn_to(self, deg):
        ''' Make the agent turn to a specific direction '''
        self.yaw_aim = deg
        rotate_direction = ((deg - self.yaw + 360) % 360) > 180
        if not rotate_direction: # Right
            self.agent.sendCommand("turn 1")
            print("Turn right")
        else:
            self.agent.sendCommand("turn -1")
            print("Turn left")

    def aim_adjust(self, deg):
        ''' Adjust the agent aim to be in line with the mission '''
        self.yaw_aim = deg
        if within_tolerance(self.yaw_aim, self.yaw):
            self.agent.sendCommand("turn 0")
            return
        rotate_direction = ((deg - self.yaw + 360) % 360) > 180
        if not rotate_direction: # Right
            self.agent.sendCommand("turn "+AIM_ADJUST2)
            print("Turn adjust right")
        else:
            self.agent.sendCommand("turn -"+AIM_ADJUST2)
            print("Turn adjust left")

    def line_adjust(self):
        ''' Adjust the agent aim to the center of the mission '''
        if self.yaw_aim == 0: # Eastward
            if self.x - self.main_line > MIDDLE_TOLERANCE:
                self.agent.sendCommand("turn "+AIM_ADJUST1)
                print("Adjust right")
            elif self.x - self.main_line < -MIDDLE_TOLERANCE:
                self.agent.sendCommand("turn -"+AIM_ADJUST1)
                print("Adjust left")
            else: self.aim_adjust(0)
            
        elif self.yaw_aim == 180: # Westward
            if self.x - self.main_line > MIDDLE_TOLERANCE:
                self.agent.sendCommand("turn -"+AIM_ADJUST1)
                print("Adjust left")
            elif self.x - self.main_line < -MIDDLE_TOLERANCE:
                self.agent.sendCommand("turn "+AIM_ADJUST1)
                print("Adjust right")
            else: self.aim_adjust(180)
            
        elif self.yaw_aim == 270: # Northward
            if self.z - self.main_line > MIDDLE_TOLERANCE:
                self.agent.sendCommand("turn -"+AIM_ADJUST1)
                print("Adjust left")
            elif self.z - self.main_line < -MIDDLE_TOLERANCE:
                self.agent.sendCommand("turn "+AIM_ADJUST1)
                print("Adjust right")
            else: self.aim_adjust(270)
            
        elif self.yaw_aim == 90: # Southward
            if self.z - self.main_line < -MIDDLE_TOLERANCE:
                self.agent.sendCommand("turn -"+AIM_ADJUST1)
                print("Adjust right")
            elif self.z - self.main_line > MIDDLE_TOLERANCE:
                self.agent.sendCommand("turn "+AIM_ADJUST1)
                print("Adjust left")
            else: self.aim_adjust(90)

    def is_legal_move(self, move):
        if move == self.DECISION_LEFT: # A: Westward
            return self.z_unit_aim > 0
        elif move == self.DECISION_DOWN: # S: Southward
            return self.x_unit_aim > 0
        elif move == self.DECISION_UP: # W: Northward
            return self.x_unit_aim < LENGTH - 1
        elif move == self.DECISION_RIGHT: # D: Eastward
            return self.z_unit_aim < WIDTH - 1

    def legal_moves(self):
        up = "True" if (self.x_unit_aim < LENGTH - 1) else "False"
        down = "True" if (self.x_unit_aim < 0) else "False"
        left = "True" if (self.z_unit_aim > 0) else "False"
        right = "True" if (self.z_unit_aim < WIDTH - 1) else "False"
        return up, down, left, right

    def move_values(self, x_unit, z_unit):
        dec_values = [0] * 4
        dec_values[0] = self.dec_table[x_unit][z_unit][self.DECISION_UP]
        dec_values[1] = self.dec_table[x_unit][z_unit][self.DECISION_DOWN]
        dec_values[2] = self.dec_table[x_unit][z_unit][self.DECISION_LEFT]
        dec_values[3] = self.dec_table[x_unit][z_unit][self.DECISION_RIGHT]

        # Penalize if we go back to the previous point
        if self.last_dec == self.DECISION_UP:
            dec_values[self.DECISION_DOWN] += LAST_DECISION_PENALTY
        elif self.last_dec == self.DECISION_DOWN:
            dec_values[self.DECISION_UP] += LAST_DECISION_PENALTY
        elif self.last_dec == self.DECISION_LEFT:
            dec_values[self.DECISION_RIGHT] += LAST_DECISION_PENALTY
        elif self.last_dec == self.DECISION_RIGHT:
            dec_values[self.DECISION_LEFT] += LAST_DECISION_PENALTY

        # if move is illegal, punish even more
        if not self.is_legal_move(self.DECISION_UP):
            dec_values[self.DECISION_UP] = ILLEGAL_DECISION_PENALTY
        if not self.is_legal_move(self.DECISION_DOWN):
            dec_values[self.DECISION_DOWN] = ILLEGAL_DECISION_PENALTY
        if not self.is_legal_move(self.DECISION_LEFT):
            dec_values[self.DECISION_LEFT] = ILLEGAL_DECISION_PENALTY
        if not self.is_legal_move(self.DECISION_RIGHT):
            dec_values[self.DECISION_RIGHT] = ILLEGAL_DECISION_PENALTY

        # if destination is visited, punish
        if self.is_legal_move(self.DECISION_UP) and self.visited_table[x_unit + 1][z_unit] == True:
            dec_values[self.DECISION_UP] += VISITED_PENALTY
        if self.is_legal_move(self.DECISION_DOWN) and self.visited_table[x_unit - 1][z_unit] == True:
            dec_values[self.DECISION_DOWN] += VISITED_PENALTY
        if self.is_legal_move(self.DECISION_LEFT) and self.visited_table[x_unit][z_unit - 1] == True:
            dec_values[self.DECISION_LEFT] += VISITED_PENALTY
        if self.is_legal_move(self.DECISION_RIGHT) and self.visited_table[x_unit][z_unit + 1] == True:
            dec_values[self.DECISION_RIGHT] += VISITED_PENALTY

        return dec_values[0], dec_values[1], dec_values[2], dec_values[3]
            
    def handle_key_press(self, key, code):
        print "Model presses key", chr(key)
        if (not self.is_turning()):
            self.pre_dec_time = time.time()
            self.agent_state = self.AGENT_TURNING
            self.pre_dec_x_unit = self.x_unit
            self.pre_dec_z_unit = self.z_unit
            if (key == 97): # A move westward
                self.z_unit_aim = self.z_unit - 1
                self.turn_to(180)
                self.main_line = Z_LINE[int(self.x_unit_aim)]
                self.last_dec = self.DECISION_LEFT
            elif (key == 115): # S move southward
                self.x_unit_aim = self.x_unit - 1
                self.turn_to(90)
                self.main_line = X_LINE[int(self.z_unit_aim)]
                self.last_dec = self.DECISION_DOWN
            elif (key == 119): # W move northward
                self.x_unit_aim = self.x_unit + 1
                self.turn_to(270)
                self.main_line = X_LINE[int(self.z_unit_aim)]
                self.last_dec = self.DECISION_UP
            elif (key == 100): # D move eastward
                self.z_unit_aim = self.z_unit + 1
                self.turn_to(0)
                self.main_line = Z_LINE[int(self.x_unit_aim)]
                self.last_dec = self.DECISION_RIGHT
            self.agent.sendCommand("move 0.2")

    def update(self):
        # Update world state and local variables
        self.world_state = self.getWorldState()
        if self.world_state.number_of_observations_since_last_state > 0:
            msg = self.world_state.observations[-1].text
            observations = json.loads(msg)
            self.x = observations[u'XPos']
            self.y = observations[u'YPos']
            self.z = observations[u'ZPos']
            self.yaw = observations[u'Yaw']
            self.damage_taken = observations[u'DamageTaken']
            self.floor_info = observations[u'floor']
            if self.yaw < 0:
                self.yaw += 360
            self.x_unit, self.z_unit = determine_position(self.x, self.z)
            
        # Check aim, stop turning if aim is reached
        if (self.agent_state == self.AGENT_TURNING) and (abs(self.yaw - self.yaw_aim) < AIM_TOLERANCE or abs(self.yaw - self.yaw_aim) > (360 - AIM_TOLERANCE)):
            print("stop turn")
            self.agent.sendCommand("turn 0")
            self.agent.sendCommand("move 1")
            self.agent_state = self.AGENT_MOVING

        # Continually adjust direction while moving
        if (self.agent_state == self.AGENT_MOVING):
            self.line_adjust()

        # Check action, make new decision if aim is reached
        if (self.x_unit == self.x_unit_aim) and (self.z_unit == self.z_unit_aim):
            # Measure the time of the last decision and put it in to the decision table
            if (self.last_dec != None):
                self.post_dec_time = time.time()
                dec_time = self.post_dec_time - self.pre_dec_time
                self.dec_table[int(self.pre_dec_x_unit)][int(self.pre_dec_z_unit)][self.last_dec] = dec_time
                self.visited_table[int(self.x_unit)][int(self.z_unit)] = True
            self.agent.sendCommand("move 0.2")
            self.agent_state = self.AGENT_DECIDING

    # ===================
    # GETTERS AND SETTERS
    # ===================
    
    def get_state(self):
        return self.agent_state

    def get_status_info(self):
        state = "deciding" if self.is_deciding() else "moving"
        health = 10 - self.damage_taken
        return state, health

    def get_floor_info(self):
        return self.floor_info

    def get_pos_aim(self):
        return self.x_unit_aim, self.z_unit_aim

    def get_yaw_aim(self):
        return self.yaw_aim

    # =============
    # QUERY METHODS
    # =============

    def is_deciding(self):
        return self.agent_state == self.AGENT_DECIDING

    def is_turning(self):
        return self.agent_state == self.AGENT_TURNING

    def is_choosing(self):
        return self.agent_state == self.AGENT_CHOOSING

    # ====================
    # TRANSLATION FUNCTION
    # ====================

    def getWorldState(self):
        return self.agent.getWorldState()

    def startMission(self):
        self.agent.startMission(self.my_mission, self.my_mission_record)
