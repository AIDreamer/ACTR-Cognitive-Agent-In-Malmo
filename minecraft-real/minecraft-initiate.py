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
from MalmoACTRAgent import MalmoACTRAgent
from Utility import *
from Constants import *
from Chunks import *


## -----------------------
## Define the mission here
## -----------------------

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
                  <DefaultWorldGenerator/>
                  <ServerQuitWhenAnyAgentFinishes/>
                </ServerHandlers>
              </ServerSection>
              
              <AgentSection mode="Survival">
                <Name>MalmoTutorialBot</Name>
                <AgentStart>
                    <Placement x="''' + str(X) +'''" y="''' + str(Y+1) + '''" z="'''+ str(Z) +'''" yaw="'''+ str(YAW)+'''"/>
                </AgentStart>
                <AgentHandlers>
                  <ObservationFromFullStats/>
                  <ObservationFromGrid>
                      <Grid name="floor">
                        <min x="-'''+str(FLOOR_RADIUS)+'''" y="-1" z="-'''+str(FLOOR_RADIUS)+'''"/>
                        <max x="'''+str(FLOOR_RADIUS)+'''" y="0" z="'''+str(FLOOR_RADIUS)+'''"/>
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
            drawSquare(mission, (CORRIDOR_LENGTH + SQUARE_WIDTH) * i + SQUARE_WIDTH, (CORRIDOR_LENGTH + SQUARE_WIDTH) * j + SQUARE_WIDTH, (CORRIDOR_LENGTH + SQUARE_WIDTH) * i + SQUARE_WIDTH + CORRIDOR_LENGTH - 1, (CORRIDOR_LENGTH + SQUARE_WIDTH) * j + SQUARE_WIDTH + CORRIDOR_LENGTH - 1, HEIGHT+1,  "cobblestone")

    # Starting position
    return mission

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
    
    '''
        Initiate environment
    '''
    def __init__(self, actr = False):

        self.actr = actr
        self.actr_time_lock = False
        self.actr_running = False
        self.objects = [] # List of chunks
        
        # Malmo objects and agent_host
        self.my_mission = create_mission()
        self.my_mission_record = MalmoPython.MissionRecordSpec()
        self.agent_host = MalmoACTRAgent(self.my_mission, self.my_mission_record)
        self.state = self.STATE_START_MISSION

        # Start the mission
        self.start_mission()

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

    # Game functions
    def handle_key_press(self, key, code):
        print "Model presses key", chr(key)
        if (key == 104): # H Get data
            self.update_objects()
        elif (key == 97): # A
            self.agent_host.handle_key_press(97, None)
        elif (key == 115):
            self.agent_host.handle_key_press(115, None)
        elif (key == 119):
            self.agent_host.handle_key_press(119, None)
        elif (key == 100):
            self.agent_host.handle_key_press(100, None)

    def write_wait_actr_connect(self):
        pass

    def write_wait_actr_model(self):
        pass

    def start_mission(self):
        # Try to start the mission
        self.state = self.STATE_MISSION_INITIATING
        try:
            self.agent_host.startMission()
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

        print "\nNow wait for the ACT-R agent to connect"

    def mission_running(self):
        if (self.actr_running == True):
            # Keep updating the agent
            self.agent_host.update()
            
            # Stop if mission is no longer running
            self.world_state = self.agent_host.getWorldState()
            if (not self.world_state.is_mission_running):
                self.state = self.STATE_STOP_MISSION

    def get_info_objects(self):
            info_objects = []
            
            # State information
            state, health = self.agent_host.get_status_info()
            state_object = StatsBlock(-1, 0, state, health)
            info_objects.append(state_object)

            # Floor information
            floor_info = self.agent_host.get_floor_info()
            ground_info = floor_info[: FLOOR_ITEMS_NUM // 2]
            air_info = floor_info[FLOOR_ITEMS_NUM // 2 :]
            yaw_aim = self.agent_host.get_yaw_aim()
            if floor_info != None:
                # Block objects: Objects on the ground (level -1)
                for i in range(FLOOR_ITEMS_NUM // 2):
                    if yaw_aim == 270:
                        x = i % FLOOR_WIDTH
                        y = i // FLOOR_WIDTH
                    elif yaw_aim == 0:
                        x = 4 - i // FLOOR_WIDTH
                        y = 4 - i % FLOOR_WIDTH
                    elif yaw_aim == 90:
                        x = 4 - i % FLOOR_WIDTH
                        y = 4 - i // FLOOR_WIDTH
                    elif yaw_aim == 180:
                        x = i // FLOOR_WIDTH
                        y = i % FLOOR_WIDTH
                    info_objects.append(GroundBlock(x, y, ground_info[i]))

                # Air objects: Objects around the agent (lavel 0)
                for i in range(FLOOR_ITEMS_NUM // 2):
                    if yaw_aim == 270:
                        x = i % FLOOR_WIDTH
                        y = i // FLOOR_WIDTH + FLOOR_WIDTH
                    elif yaw_aim == 0:
                        x = 4 - i // FLOOR_WIDTH
                        y = 4 - i % FLOOR_WIDTH + FLOOR_WIDTH
                    elif yaw_aim == 90:
                        x = 4 - i % FLOOR_WIDTH
                        y = 4 - i // FLOOR_WIDTH + FLOOR_WIDTH
                    elif yaw_aim == 180:
                        x = i // FLOOR_WIDTH
                        y = i % FLOOR_WIDTH + FLOOR_WIDTH
                    info_objects.append(AirBlock(x, y, air_info[i]))

            # Map object, which describes the current location of the agent
            x_unit_aim, z_unit_aim = self.agent_host.get_pos_aim()
            up, down, left, right = self.agent_host.move_values(int(x_unit_aim), int(z_unit_aim))

            print up, down, left, right
            map_object = MapBlock(-1, 1, up, down, left, right, x_unit_aim, z_unit_aim)
            info_objects.append(map_object)

            return info_objects

    def update_objects(self):
        if self.actr:
            info_objects = self.get_info_objects()
            
            # Display these chunks        
            self.actr.display_new([obj.toChunk() for obj in info_objects])
            
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
                self.actr_running = True
                self.state = self.STATE_MISSION_RUNNING
                pass
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
            print params
        
if __name__ == '__main__':
    env = Environment(actr = True)
    reactor.run()
