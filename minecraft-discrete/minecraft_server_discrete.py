## Name: Son Pham
## Summer Research 2016
## Prof. Dancy
## An attempt to conenct JNI to Python with Malmo discrete movement

import sys,os
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)), "../json-network-interface/lib/python"))

# For general environment
from twisted.internet import reactor
from twisted.internet.task import LoopingCall, Cooperator

ACTR6 = True
from actr6_jni import Dispatcher, JNI_Server, Twisted_MPClock

import MalmoPython, time, json

## ---------------------------------------------
## Utility functions to make life easier to read
## ---------------------------------------------

'''
    Wrapper for agent host attempting to start a mission
'''

def flush_print():
    sys.stdout = os.fdopen(sys.stdout.fileno(), 'w', 0)  # flush print output immediately   
    

## ---------------------------------
## Chunk code
## ---------------------------------

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
          

## ------------------------------------
## Mission XML. Define the mission here
## ------------------------------------

def drawBlock(x, y, z, blockType):
    return '''<DrawBlock x="''' + str(x) + '''"  y="''' + str(y) +'''" z="'''+ str(z) +'''" type="''' + blockType + '''" />'''

def drawInput(inputArr, baseX = 0, baseZ = 0, baseHeight = 45):
    # Determine the length
    m = len(inputArr)
    n = len(inputArr[0])

    # XML
    xmlString = ""
    for i in range(m):
        for j in range(n):
            if inputArr[i][j] == 1:
                xmlString += drawBlock(i + baseX, baseHeight, j + baseZ, "sandstone")
            elif inputArr[i][j] == 2:
                xmlString += drawBlock(i + baseX, baseHeight, j + baseZ, "cobblestone")
            elif inputArr[i][j] == 3:
                xmlString += drawBlock(i + baseX, baseHeight, j + baseZ, "lapis_block")

    return xmlString

# Read the data into inputArr
f = file("input.txt", "r")
data = f.readline()
data = data.split()
m = int(data[0])
n = int(data[1])
                                 
inputArr = []
for i in range(m):
    data = f.readline()
    data = data.split()
    inputArr.append([int(x) for x in data])
f.close()

missionXML='''<?xml version="1.0" encoding="UTF-8" standalone="no" ?>
<Mission xmlns="http://ProjectMalmo.microsoft.com" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://ProjectMalmo.microsoft.com Mission.xsd">
  <About>
    <Summary>Cliff walking mission based on Sutton and Barto.</Summary>
  </About>
  <ServerSection>
    <ServerInitialConditions>
        <Time><StartTime>1</StartTime></Time>
    </ServerInitialConditions>
    <ServerHandlers>
      <FlatWorldGenerator generatorString="3;7,220*1,5*3,2;3;,biome_1"/>
      <DrawingDecorator>
        <!-- coordinates for cuboid are inclusive -->
        <DrawCuboid x1="-2" y1="46" z1="-2" x2="7" y2="50" z2="13" type="air" />            <!-- limits of our arena -->
        <DrawCuboid x1="-2" y1="45" z1="-2" x2="7" y2="45" z2="13" type="lava" />           <!-- lava floor -->
        ''' + drawInput(inputArr, baseX = 0, baseZ = 0) + '''
        </DrawingDecorator>
      <ServerQuitWhenAnyAgentFinishes/>
    </ServerHandlers>
  </ServerSection>

  <AgentSection mode="Survival">
    <Name>Cristina</Name>
    <AgentStart>
      <Placement x="0" y="46" z="0" pitch="30" yaw="0"/>
    </AgentStart>
    <AgentHandlers>
      <DiscreteMovementCommands/>
      <ObservationFromFullStats/>
      <ObservationFromGrid>
         <Grid name="floor3x3">
           <min x="-1" y="-1" z="-1"/>
           <max x="1" y="-1" z="1"/>
         </Grid>
      </ObservationFromGrid>
      <RewardForTouchingBlockType>
        <Block reward="-100.0" type="lava" behaviour="onceOnly"/>
        <Block reward="100.0" type="lapis_block" behaviour="onceOnly"/>
      </RewardForTouchingBlockType>
      <RewardForSendingCommand reward="-1" />
      <AgentQuitFromTouchingBlockType>
          <Block type="lava" />
          <Block type="lapis_block" />
      </AgentQuitFromTouchingBlockType>
    </AgentHandlers>
  </AgentSection>

</Mission>'''

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
        self.objects = [] # List of chunks

        # Malmo objects
        self.agent_host = MalmoPython.AgentHost()
        self.my_mission = MalmoPython.MissionSpec(missionXML, True)
        self.my_mission_record = MalmoPython.MissionRecordSpec()

        # Change state of the program to avoid starting mission twice
        self.state = self.STATE_MISSION_INITIATING

        # Try to start the mission
        try:
            self.agent_host.startMission( self.my_mission, self.my_mission_record )
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

        # After mission starts, connect ACT-R to the mission
        self.state = self.STATE_MISSION_RUNNING
        print "\nMission running"

        if ACTR6 and self.actr:
            self.state = self.STATE_WAIT_CONNECT
            self.actr = JNI_Server(self, clock=Twisted_MPClock())
            self.actr.addDispatcher(self.d)
            print "Waiting for connection fron JNI module"
            reactor.listenTCP(5555, self.actr)
        
        # Looping call
        self.lc1 = LoopingCall(self.update_env)
        self.lc1.start(1.0 / 20)

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
            # Stop if mission is no longer running
            self.world_state = self.agent_host.getWorldState()    
            if (not self.world_state.is_mission_running):
                self.state = self.STATE_STOP_MISSION
        elif self.state == self.STATE_STOP_MISSION:
            self.stop_mission()

    def reset(self):
        pass

    def validate(self):
        pass

    def handle_key_press(self, key, code):
        if (key == 119):
            self.agent_host.sendCommand("movenorth 1")
        elif (key == 97):
            self.agent_host.sendCommand("movewest 1")
        elif (key == 115):
            self.agent_host.sendCommand("movesouth 1")
        elif (key == 100):
            self.agent_host.sendCommand("moveeast 1")
        time.sleep(0.1)
    
    def write_wait_actr_connect(self):
        pass

    def write_wait_actr_model(self):
        pass

    def start_mission(self):
        self.state = self.STATE_MISSION_RUNNING
        
    def update_objects(self):
        self.world_state = self.agent_host.getWorldState()
        # Receive block information from minecraft
        print self.world_state.number_of_observations_since_last_state
        if self.world_state.number_of_observations_since_last_state > 0:
            msg = self.world_state.observations[-1].text
            observations = json.loads(msg)
            grid = observations.get(u'floor3x3', 0)
            
            # Convert to Block objects
            self.objects = [None] * 9
            for i in range(0, len(grid)):
                x = i % 3
                y = i // 3
                blockType = grid[i]
                self.objects[i] = Block(x, y, blockType)

    def stop_mission(self):
        print "\nMission ended"
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
            self.update_objects() # Send information for the first time
            self.state = self.STATE_WAIT_MODEL
            print "Connection has been made"
            # Send chunks info to ACTR
            if self.actr:
                chunks = [obj.toChunk() for obj in self.objects]
                self.actr.display_new(chunks)

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
                self.lc1.start(1.0 / 20)
            pass

        @d.listen('model-stop')
        def ACTR6_JNI_Event(self, model, params):
            pass

        @d.listen('keypress')
        def ACTR6_JNI_Event(self, model, params):
            self.handle_key_press(params['keycode'], chr(params['keycode']))
            self.update_objects()
            # Send chunks info to ACTR
            if self.actr:
                chunks = [obj.toChunk() for obj in self.objects]
                self.actr.display_new(chunks)
            
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
