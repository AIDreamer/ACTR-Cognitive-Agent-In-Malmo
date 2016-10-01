## Name: Son Pham
## Summer Research 2016
## Prof. Dancy
## An attempt to conenct JNI to Python

import sys,os
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)), "../json-network-interface/lib/python"))

# For general environment
from twisted.internet import reactor
from twisted.internet.task import LoopingCall, Cooperator

ACTR6 = True
from actr6_jni import Dispatcher, JNI_Server, Twisted_MPClock

import MalmoPython, time, json, random, math

## -----------------
## CLASS DEFINITIONS
## -----------------
from itertools import count
       
class Chunk(object):
    
    _ids = count(0)
    
    def __init__(self, name, isa, **slots):
        self._id = self._ids.next()
        if name == None:
            self.name = "vc%d" % self._id
        else:
            self.name = name
        self.isa = isa
        self.slots = slots
        
    def get_chunk(self, name=None, isa=None, empty=False):
        if name == None:
            name = str(self.name)
        if isa == None:
            isa = self.isa
        chunk = {"name": name, "isa": isa, "slots": {}}
        if not empty:
            for s, v in self.slots.iteritems():
                chunk["slots"][s] = v
        return chunk

class VisualChunk(Chunk):

    def __init__(self, name, isa, screenx, screeny, width = None, height = None, color = None, size = None, value = None, **slots):
        super(VisualChunk, self).__init__(name, isa, **slots)
        self.screenx = screenx
        self.screeny = screeny
        self.width = width
        self.height = height
        self.color = color
        self.size = size
        self.value = value

    def __repr__(self):
        return "{x: " + str(self.screenx) + " y: " + str(self.screeny) + " type: " + str(self.value) + "}"

    def get_visual_object(self):
        chunk = super(VisualChunk, self).get_chunk(name="%s-obj" % str(self.name))
        if self.width:
            chunk["slots"]["width"] = self.width
        if self.height:
            chunk["slots"]["height"] = self.height
        if self.color:
            chunk["slots"]["color"] = self.color
        if self.value:
            chunk["slots"]["value"] =  self.value
        return chunk

    def get_visual_location(self, isa=None):
        if isa == None:
            isa = "visual-location"
        chunk = super(VisualChunk, self).get_chunk(name="%s-loc" % str(self.name), isa=isa, empty=True)
        chunk["slots"]["kind"] = ":%s" % self.isa
        chunk["slots"]["screen-x"] = self.screenx
        chunk["slots"]["screen-y"] = self.screeny
        if self.width:
            chunk["slots"]["width"] = self.width
        if self.height:
            chunk["slots"]["height"] = self.height
        if self.color:
            chunk["slots"]["color"] = self.color
        if self.size:
            chunk["slots"]["size"] = self.size
        if self.value:
            chunk["slots"]["value"] =  self.value
        return chunk

class PAAVChunk(VisualChunk):
    
    def get_visual_location(self):
        chunk = super(PAAVChunk, self).get_visual_location()
        for s, v in self.slots.iteritems():
            if s in ["fcolor", "fshape", "fsize", "fshading", "forient"]:
                chunk["slots"][s] = v
        return chunk

class Block(object):

    def __init__(self, screenx, screeny, blockType):
        self.x = screenx
        self.y = screeny
        self.blockType = blockType

    def __repr__(self):
        return self.blockType

    def toChunk(self):
        return VisualChunk(None, "blockobj", self.x, self.y, width = None, height = None, color = None, letter = None, value = self.blockType)
   

## -----------------------
## Define the mission here
## -----------------------

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
AIM_TOLERANCE = 10
X_LINE = [(SQUARE_WIDTH / 2 + COMBINED_LENGTH * i) for i in range(0, LENGTH)]
Z_LINE = [(SQUARE_WIDTH / 2 + COMBINED_LENGTH * i) for i in range(0, WIDTH)]
AIM_ADJUST1 = str(0.2)
AIM_ADJUST2 = str(0.5)
MIDDLE_TOLERANCE = 1

missionXML='''<?xml version="1.0" encoding="UTF-8" standalone="no" ?>
            <Mission xmlns="http://ProjectMalmo.microsoft.com" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://ProjectMalmo.microsoft.com Mission.xsd">
              <About>
                <Summary>Hello world!</Summary>
              </About>
              
            <ServerSection>
              <ServerInitialConditions>
                <Time>
                    <StartTime>1000</StartTime>
                    <AllowPassageOfTime>false</AllowPassageOfTime>
                </Time>
                <Weather>clear</Weather>
              </ServerInitialConditions>
              <ServerHandlers>
                  <FlatWorldGenerator/>
                  <ServerQuitWhenAnyAgentFinishes/>
                </ServerHandlers>
              </ServerSection>
              
              <AgentSection mode="Survival">
                <Name>MalmoTutorialBot</Name>
                <AgentStart>
                    <Placement x="''' + str(X) +'''" y="''' + str(Y) + '''" z="'''+ str(Z) +'''" yaw="'''+ str(YAW)+'''"/>
                </AgentStart>
                <AgentHandlers>
                  <ObservationFromFullStats/>
                  <ObservationFromGrid>
                      <Grid name="floor">
                        <min x="-2" y="-1" z="-2"/>
                        <max x="2" y="-1" z="2"/>
                      </Grid>
                  </ObservationFromGrid>
                  <ContinuousMovementCommands turnSpeedDegs="90"/>
                </AgentHandlers>
              </AgentSection>
            </Mission>'''

def drawSquare(mission, x1, z1, x2, z2, y, block_type):
    mission.drawLine(x1, y, z1, x1, y, z2, block_type)
    mission.drawLine(x1, y, z1, x2, y, z1, block_type)
    mission.drawLine(x1, y, z2, x2, y, z2, block_type)
    mission.drawLine(x2, y, z1, x2, y, z2, block_type)

def create_mission():

    # Create mission
    mission = MalmoPython.MissionSpec(missionXML, True)
    #mission.forceWorldReset() # The world is reset every time
    drawSquare(mission, -1, -1, MISSION_LENGTH, MISSION_WIDTH, HEIGHT,  "stone")
    drawSquare(mission, -1, -1, MISSION_LENGTH, MISSION_WIDTH, HEIGHT + 1,  "glass")
    drawSquare(mission, -1, -1, MISSION_LENGTH, MISSION_WIDTH, HEIGHT + 2,  "glass")
    drawSquare(mission, -1, -1, MISSION_LENGTH, MISSION_WIDTH, HEIGHT + 3,  "glass")
    drawSquare(mission, -1, -1, MISSION_LENGTH, MISSION_WIDTH, HEIGHT + 4,  "glass")
    mission.drawCuboid(0,  HEIGHT,  0, MISSION_LENGTH - 1, HEIGHT, MISSION_WIDTH - 1,  "grass")
    mission.drawCuboid(0,  HEIGHT - 1,  0, MISSION_LENGTH - 1, HEIGHT - 1, MISSION_WIDTH - 1,  "cobblestone")
    mission.drawCuboid(0,  HEIGHT + 1, 0, MISSION_LENGTH - 1, HEIGHT + 15, MISSION_WIDTH - 1, "air")

    for i in range(LENGTH):
        mission.drawCuboid( (CORRIDOR_LENGTH + SQUARE_WIDTH) * i, HEIGHT, 0, (CORRIDOR_LENGTH + SQUARE_WIDTH) * i + SQUARE_WIDTH - 1, HEIGHT, MISSION_WIDTH - 1, "grass")

    for j in range(WIDTH):    
        mission.drawCuboid( 0, HEIGHT, (CORRIDOR_LENGTH + SQUARE_WIDTH) * j, MISSION_LENGTH - 1, HEIGHT, (CORRIDOR_LENGTH + SQUARE_WIDTH) * j + SQUARE_WIDTH - 1, "grass")

    for i in range(LENGTH):
        for j in range(WIDTH):
            mission.drawCuboid((CORRIDOR_LENGTH + SQUARE_WIDTH) * i,  HEIGHT, (CORRIDOR_LENGTH + SQUARE_WIDTH) * j, (CORRIDOR_LENGTH + SQUARE_WIDTH) * i + SQUARE_WIDTH - 1,  HEIGHT,  (CORRIDOR_LENGTH + SQUARE_WIDTH) * j + SQUARE_WIDTH - 1,  "glowstone")

    for i in range(LENGTH - 1):
        for j in range(WIDTH - 1):
            mission.drawCuboid((CORRIDOR_LENGTH + SQUARE_WIDTH) * i + SQUARE_WIDTH, HEIGHT + 1, (CORRIDOR_LENGTH + SQUARE_WIDTH) * j + SQUARE_WIDTH, (CORRIDOR_LENGTH + SQUARE_WIDTH) * i + SQUARE_WIDTH + CORRIDOR_LENGTH - 1, HEIGHT + 1, (CORRIDOR_LENGTH + SQUARE_WIDTH) * j + SQUARE_WIDTH + CORRIDOR_LENGTH - 1, "lava")
            drawSquare(mission, (CORRIDOR_LENGTH + SQUARE_WIDTH) * i + SQUARE_WIDTH, (CORRIDOR_LENGTH + SQUARE_WIDTH) * j + SQUARE_WIDTH, (CORRIDOR_LENGTH + SQUARE_WIDTH) * i + SQUARE_WIDTH + CORRIDOR_LENGTH - 1, (CORRIDOR_LENGTH + SQUARE_WIDTH) * j + SQUARE_WIDTH + CORRIDOR_LENGTH - 1, HEIGHT + 1,  "cobblestone")

    # Starting position
    return mission
    
## -----------------
## UTILITY FUNCTIONS
## -----------------

'''
    Wrapper for agent host attempting to start a mission
'''

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

## ------------------------------------------
## Environment code for interacting with lisp
## ------------------------------------------

class Environment(object):
    if ACTR6:
        d = Dispatcher()

    # A bunch of states for Environment
    STATE_WAIT_CONNECT = -4
    STATE_WAIT_MODEL = -3
    STATE_START_MISSION = -2
    STATE_MISSION_INITIATING = -1
    STATE_RESET = 0
    STATE_MISSION_RUNNING = 1
    STATE_STOP_MISSION = 2

    # A bunch of states for Agent
    AGENT_TURNING = 0
    AGENT_DECIDING = 1
    AGENT_CHOOSING = 2
    AGENT_MOVING = 3
    
    '''
        Initiate environment
    '''
    def __init__(self, actr = False):

        self.actr = actr
        self.actr_time_lock = False
        self.objects = [] # List of chunks
        
        # Malmo objects
        self.agent_host = MalmoPython.AgentHost()
        self.my_mission = create_mission()
        self.my_mission_record = MalmoPython.MissionRecordSpec()
        self.state = self.STATE_START_MISSION

        # Controlling variable
        self.yaw = YAW
        self.x = X
        self.y = Y
        self.z = Z
        self.x_unit = 0
        self.z_unit = 0
        self.agent_state = self.AGENT_TURNING

        # Aiming variable
        self.yaw_aim = 0
        self.x_unit_aim = 0
        self.z_unit_aim = 0
        self.main_line = 0

        if ACTR6 and self.actr:
            self.state = self.STATE_WAIT_CONNECT
            self.actr = JNI_Server(self, clock=Twisted_MPClock())
            self.actr.addDispatcher(self.d)
            print "Waiting for connection fron JNI module"
            reactor.listenTCP(5555, self.actr)

        # Looping call
        self.lc1 = LoopingCall(self.update_env)
        self.lc1.start(1.0 / 30)

    def update_env(self):
        if self.state == self.STATE_WAIT_CONNECT:
            self.write_wait_actr_connect()
        elif self.state == self.STATE_WAIT_MODEL:
            self.write_wait_actr_model()
        elif self.state == self.STATE_START_MISSION:
            self.start_mission()
        elif self.state == self.STATE_MISSION_INITIATING:
            pass # Mission being generated
        elif self.state == self.STATE_MISSION_RUNNING:
            self.mission_running()
        elif self.state == self.STATE_STOP_MISSION:
            self.stop_mission()

    def reset(self):
        pass

    def validate(self):
        pass

    # Utility functions
    
    def turn_to(self, deg):
        print("Aim: ", deg, "Current: ", self.yaw)
        self.yaw_aim = deg
        rotate_direction = ((deg - self.yaw + 360) % 360) > 180
        if not rotate_direction: # Right
            self.agent_host.sendCommand("turn 1")
            print("Turn right")
        else:
            self.agent_host.sendCommand("turn -1")
            print("Turn left")

    def turn_adjust(self, deg):
        self.yaw_aim = deg
        if within_tolerance(self.yaw_aim, self.yaw):
            self.agent_host.sendCommand("turn 0")
            return
        rotate_direction = ((deg - self.yaw + 360) % 360) > 180
        if not rotate_direction: # Right
            self.agent_host.sendCommand("turn "+AIM_ADJUST2)
            print("Turn adjust right")
        else:
            self.agent_host.sendCommand("turn -"+AIM_ADJUST2)
            print("Turn adjust left")

    def self_adjust(self):
        if self.yaw_aim == 0: # Eastward
            if self.x - self.main_line > MIDDLE_TOLERANCE:
                self.agent_host.sendCommand("turn "+AIM_ADJUST1)
                print("Adjust right")
            elif self.x - self.main_line < -MIDDLE_TOLERANCE:
                self.agent_host.sendCommand("turn -"+AIM_ADJUST1)
                print("Adjust left")
            else: self.turn_adjust(0)
            
        elif self.yaw_aim == 180: # Westward
            if self.x - self.main_line > MIDDLE_TOLERANCE:
                self.agent_host.sendCommand("turn -"+AIM_ADJUST1)
                print("Adjust left")
            elif self.x - self.main_line < -MIDDLE_TOLERANCE:
                self.agent_host.sendCommand("turn "+AIM_ADJUST1)
                print("Adjust right")
            else: self.turn_adjust(180)
            
        elif self.yaw_aim == 270: # Northward
            if self.z - self.main_line > MIDDLE_TOLERANCE:
                self.agent_host.sendCommand("turn -"+AIM_ADJUST1)
                print("Adjust left")
            elif self.z - self.main_line < -MIDDLE_TOLERANCE:
                self.agent_host.sendCommand("turn "+AIM_ADJUST1)
                print("Adjust right")
            else: self.turn_adjust(270)
            
        elif self.yaw_aim == 90: # Southward
            if self.z - self.main_line < -MIDDLE_TOLERANCE:
                self.agent_host.sendCommand("turn -"+AIM_ADJUST1)
                print("Adjust right")
            elif self.z - self.main_line > MIDDLE_TOLERANCE:
                self.agent_host.sendCommand("turn "+AIM_ADJUST1)
                print("Adjust left")
            else: self.turn_adjust(90)
        print self.agent_state, self.main_line, self.x, self.z, self.yaw_aim

    def legal_move(self, move):
        if move == 0: # A: Westward
            return self.z_unit_aim > 0
        elif move == 1: # S: Southward
            return self.x_unit_aim > 0
        elif move == 2: # W: Northward
            return self.x_unit_aim < LENGTH - 1
        elif move == 3: # D: Eastward
            return self.z_unit_aim < WIDTH - 1

    # Game functions
    
    def handle_key_press(self, key, code):
        print "Model presses key", chr(key)
        if (key == 104): # H Get data
            self.update_objects()
        elif (key == 106): # J Make a random decision
            self.agent_state = self.AGENT_CHOOSING
            while True:
                dec = random.randint(0, 3)
                if self.legal_move(dec): break
            if dec == 0: # A
                self.handle_key_press(97, None)
            elif dec == 1: # S
                self.handle_key_press(115, None)
            elif dec == 2: # W
                self.handle_key_press(119, None)
            else: # D
                self.handle_key_press(100, None)
        elif (key == 97): # A move westward
            self.z_unit_aim -= 1
            self.turn_to(180)
            self.main_line = Z_LINE[int(self.x_unit_aim)]
            self.agent_state = self.AGENT_TURNING
            self.agent_host.sendCommand("move 0.2")
        elif (key == 115): # S move southward
            self.x_unit_aim -= 1
            self.turn_to(90)
            self.main_line = X_LINE[int(self.z_unit_aim)]
            self.agent_state = self.AGENT_TURNING
            self.agent_host.sendCommand("move 0.2")
        elif (key == 119): # W move northward
            self.x_unit_aim += 1
            self.turn_to(270)
            self.main_line = X_LINE[int(self.z_unit_aim)]
            self.agent_state = self.AGENT_TURNING
            self.agent_host.sendCommand("move 0.2")
        elif (key == 100): # D move eastward
            self.z_unit_aim += 1
            self.turn_to(0)
            self.main_line = Z_LINE[int(self.x_unit_aim)]
            self.agent_state = self.AGENT_TURNING
            self.agent_host.sendCommand("move 0.2")

    def write_wait_actr_connect(self):
        pass

    def write_wait_actr_model(self):
        pass

    def start_mission(self):
        # Try to start the mission
        try:
            self.agent_host.startMission( self.my_mission, self.my_mission_record)
        except RuntimeError as e:
            print "Error starting mission:",e
            exit(1)

        # Loop until mission starts
        print "Waiting for the mission to start ",
        self.world_state = self.agent_host.getWorldState()
        
        while not self.world_state.is_mission_running:
            sys.stdout.write(".")
            time.sleep(0.1)
            self.world_state = self.agent_host.getWorldState()
            for error in self.world_state.errors:
                print "Error:",error.text

        self.state = self.STATE_MISSION_RUNNING
        print "\nMission running"

    def mission_running(self):

        # Update stats
        if self.world_state.number_of_observations_since_last_state > 0:
            msg = self.world_state.observations[-1].text
            observations = json.loads(msg)
            self.x = observations[u'XPos']
            self.y = observations[u'YPos']
            self.z = observations[u'ZPos']
            self.yaw = observations[u'Yaw']
            if self.yaw < 0:
                self.yaw += 360
            self.x_unit, self.z_unit = determine_position(self.x, self.z)
            
        # Check aim, stop turning if aim is reached
        if (self.agent_state == self.AGENT_TURNING) and (abs(self.yaw - self.yaw_aim) < AIM_TOLERANCE or abs(self.yaw - self.yaw_aim) > (360 - AIM_TOLERANCE)):
            print("stop turn")
            self.agent_host.sendCommand("turn 0")
            self.agent_host.sendCommand("move 1")
            self.agent_state = self.AGENT_MOVING

        # Continually adjust direction while moving
        if (self.agent_state == self.AGENT_MOVING):
            self.self_adjust()

        # Check action, make new decision if aim is reached
        if (self.x_unit == self.x_unit_aim) and (self.z_unit == self.z_unit_aim):
            self.agent_host.sendCommand("move 0.2")
            self.agent_state = self.AGENT_DECIDING
            
        # Stop if mission is no longer running
        self.world_state = self.agent_host.getWorldState()

        # Stop mission if it's no longer running
        if (not self.world_state.is_mission_running):
            self.state = self.STATE_STOP_MISSION

    def update_objects(self):
        if self.actr:
            state_object = Block(1,1, "deciding" if self.agent_state == self.AGENT_DECIDING else "moving")
            self.actr.display_new([state_object.toChunk()])
            
    def stop_mission(self):
        print "\nMission ended"
        reactor.stop()
        exit(0)
    
    def setDefaultClock(self):
        self.lc1.stop()
        self.lc1.clock = reactor
        self.lc1.start(1.0 / 20)
        pass

    '''
        Handling ACTR request
    '''
    if ACTR6:

        @d.listen('connectionMade')
        def ACTR6_JNI_Event(self, model, params):
            self.state = self.STATE_WAIT_MODEL
            print "Connection has been made"

        @d.listen('connectionLost')
        def ACTR6_JNI_Event(self, model, params):
            self.setDefaultClock()
            self.state = self.STATE_WAIT_CONNECT

        @d.listen('reset')
        def ACTR6_JNI_Event(self, model, params):
            self.actr_time_lock = params['time-lock']
            self.setDefaultClock()
            self.state = self.STATE_WAIT_MODEL
            
        @d.listen('model-run')
        def ACTR6_JNI_Event(self, model, params):
            if not params['resume']:
                self.state = self.STATE_START_MISSION
                self.actr_running = True
            if self.actr_time_lock:
                self.lc1.stop()
                self.lc1.clock = self.actr.clock
                self.lc1.start(1.0 / 30)
            pass

        @d.listen('model-stop')
        def ACTR6_JNI_Event(self, model, params):
            pass

        @d.listen('keypress')
        def ACTR6_JNI_Event(self, model, params):
            self.handle_key_press(params['keycode'], chr(params['keycode']))

        @d.listen('mousemotion')
        def ACTR6_JNI_Event(self, model, params):
            pass

        @d.listen('mouseclick')
        def ACTR6_JNI_Event(self, model, params):
            pass

        @d.listen('attention-loc')
        def ACTR6_JNI_Event(self, model, params):
            print "Attention Location"
            print params
        
if __name__ == '__main__':
    env = Environment(actr = True)
    reactor.run()
