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
from actr6_jni import Dispatcher, JNI_Server, Twisted_MPClock, VisualChunk

## ------------------------------------------
## Environment code for interacting with lisp
## -------------------------------------------

class Environment(object):
    if ACTR6:
        d = Dispatcher()

    # A bunch of states for Environment
    STATE_WAIT_CONNECT = -3
    STATE_WAIT_MODEL = -2
    STATE_INTRO = -1
    STATE_RESET = 0
    STATE_FIXATION = 1
    STATE_UPDATE = 2
    STATE_SEARCH = 3
    STATE_DONE = 4
    
    '''
        Initiate environment
    '''
    def __init__(self, actr = False):

        self.actr = actr
        self.actr_time_lock = False
        
        if ACTR6 and self.actr:
            self.state = self.STATE_WAIT_CONNECT
            self.actr = JNI_Server(self, clock=Twisted_MPClock())
            self.actr.addDispatcher(self.d)
            print "Waiting for connection fron JNI module"
            reactor.listenTCP(5555, self.actr)

        self.lc1 = LoopingCall(self.update_env)
        self.lc1.start(1.0 / 30)

    def reset(self):
        pass

    def validate(self):
        pass

    def update(self):
        pass

    def handle_key_press(self, key, code):
        print key

    def update_env(self):
        pass

    def process_event(self):
        pass
    
    def setDefaultClock(self):
        self.lc1.stop()
        self.lc1.clock = reactor
        self.lc1.start(1.0 / 30)
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

if __name__ == '__main__':

    env = Environment(actr=True)
    reactor.run()
