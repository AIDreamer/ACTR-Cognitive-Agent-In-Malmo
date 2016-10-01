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

def Menger(xorg, yorg, zorg, size, blocktype, variant, holetype):
    #draw solid chunk
    genstring = GenCuboidWithVariant(xorg,yorg,zorg,xorg+size-1,yorg+size-1,zorg+size-1,blocktype,variant) + "\n"
    #now remove holes
    unit = size
    while (unit >= 3):
        w=unit/3
        for i in xrange(0, size, unit):
            for j in xrange(0, size, unit):
                x=xorg+i
                y=yorg+j
                genstring += GenCuboid(x+w,y+w,zorg,(x+2*w)-1,(y+2*w)-1,zorg+size-1,holetype) + "\n"
                y=yorg+i
                z=zorg+j
                genstring += GenCuboid(xorg,y+w,z+w,xorg+size-1, (y+2*w)-1,(z+2*w)-1,holetype) + "\n"
                genstring += GenCuboid(x+w,yorg,z+w,(x+2*w)-1,yorg+size-1,(z+2*w)-1,holetype) + "\n"
        unit/=3
    return genstring

def GenCuboid(x1, y1, z1, x2, y2, z2, blocktype):
    return '<DrawCuboid x1="' + str(x1) + '" y1="' + str(y1) + '" z1="' + str(z1) + '" x2="' + str(x2) + '" y2="' + str(y2) + '" z2="' + str(z2) + '" type="' + blocktype + '"/>'

def GenCuboidWithVariant(x1, y1, z1, x2, y2, z2, blocktype, variant):
    return '<DrawCuboid x1="' + str(x1) + '" y1="' + str(y1) + '" z1="' + str(z1) + '" x2="' + str(x2) + '" y2="' + str(y2) + '" z2="' + str(z2) + '" type="' + blocktype + '" variant="' + variant + '"/>'

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
                  <FlatWorldGenerator generatorString="3;7,44*49,73,35:1,159:4,95:13,35:13,159:11,95:10,159:14,159:6,35:6,95:6;12;"/>
                  <DrawingDecorator>
                    <DrawSphere x="-27" y="70" z="0" radius="30" type="air"/>''' + Menger(-40, 40, -13, 27, "stone", "smooth_granite", "air") + '''
                    <DrawCuboid x1="-25" y1="39" z1="-2" x2="-29" y2="39" z2="2" type="lava"/>
                    <DrawCuboid x1="-26" y1="39" z1="-1" x2="-28" y2="39" z2="1" type="obsidian"/>
                    <DrawBlock x="-27" y="39" z="0" type="diamond_block"/>
                  </DrawingDecorator>
                  <ServerQuitWhenAnyAgentFinishes/>
                </ServerHandlers>
              </ServerSection>
              
              <AgentSection mode="Survival">
                <Name>MalmoTutorialBot</Name>
                <AgentStart>
                    <Placement x="0" y="56" z="0" yaw="90"/>
                    <Inventory>
                        <InventoryItem slot="8" type="diamond_pickaxe"/>
                    </Inventory>
                </AgentStart>
                <AgentHandlers>
                  <ObservationFromFullStats/>
                  <ObservationFromGrid>
                      <Grid name="floor">
                        <min x="-2" y="-1" z="-2"/>
                        <max x="2" y="-1" z="2"/>
                      </Grid>
                  </ObservationFromGrid>
                  <ContinuousMovementCommands turnSpeedDegs="180"/>
                  <AgentQuitFromTouchingBlockType>
                      <Block type="diamond_block" />
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
        print "Model presses key", chr(key)
        if (key == 104): # Special command for updating the states of the model.
            # Update all objects to chunk
            self.update_objects()
        elif (key == 115):
            self.agent_host.sendCommand("move -1")
        elif (key == 119):
            self.agent_host.sendCommand("move 0.5")
            self.agent_host.sendCommand("jump 0")
        elif (key == 102):
            self.agent_host.sendCommand("jump 1")
            self.agent_host.sendCommand("move 1")

    def write_wait_actr_connect(self):
        pass

    def write_wait_actr_model(self):
        pass

    def start_mission(self):
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

        self.state = self.STATE_MISSION_RUNNING
        print "\nMission running"
        
        # Code necessary
        self.agent_host.sendCommand("hotbar.9 1") #Press the hotbar key
        self.agent_host.sendCommand("hotbar.9 0") #Release hotbar key - agent should now be holding diamond_pickaxe

        self.agent_host.sendCommand("pitch -0.2") #Start looking downward slowly
        time.sleep(1)                        #Wait a second until we are looking in roughly the right direction
        self.agent_host.sendCommand("pitch 0")    #Stop tilting the camera
        self.agent_host.sendCommand("move 0.7")     #And start running...
        self.agent_host.sendCommand("attack 1")   #Whilst flailing our pickaxe!
        pass
    
    def update_objects(self):
        # Receive block information from minecraft
        if self.world_state.number_of_observations_since_last_state > 0:
            msg = self.world_state.observations[-1].text
            observations = json.loads(msg)
            grid = observations.get(u'floor', 0)
            print grid

            # Convert to Block objects
            self.objects = [None] * 25
            for i in range(0, len(grid)):
                x = i % 5
                y = i // 5
                blockType = grid[i]
                self.objects[i] = Block(x, y, blockType)

            if self.actr:
                chunks = [obj.toChunk() for obj in self.objects]
                self.actr.display_new(chunks)

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
