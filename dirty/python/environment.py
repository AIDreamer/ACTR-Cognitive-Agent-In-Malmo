#!/usr/bin/env python


import sys,os
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)), "../../lib/python"))

# For general environment
from twisted.internet import reactor
from twisted.internet.task import LoopingCall, Cooperator

import pygame
import pygame.font

from random import sample, randint, choice
import string

ACTR6 = True
from actr6_jni import Dispatcher, JNI_Server, VisualChunk, Twisted_MPClock

class Environment(object):
    
    if ACTR6:
        d = Dispatcher()

    STATE_WAIT_CONNECT = -3
    STATE_WAIT_MODEL = -2
    STATE_INTRO = -1
    STATE_RESET = 0
    STATE_FIXATION = 1
    STATE_UPDATE = 2
    STATE_SEARCH = 3
    STATE_DONE = 4

    colors = {':white':(255, 255, 255), ':black':(0, 0, 0)}

    def __init__(self, actr=False):

        self.state = self.STATE_INTRO
        self.actr = actr
        self.actr_time_lock = False
        if ACTR6 and self.actr:
            self.state = self.STATE_WAIT_CONNECT
            self.actr = JNI_Server(self, clock=Twisted_MPClock())
            self.actr.addDispatcher(self.d)
            reactor.listenTCP(5555, self.actr)

        self.lc1 = LoopingCall(self.update_env)
        self.lc1.start(1.0 / 30)

        self.coop = Cooperator()
        self.coop.coiterate(self.process_event())

    def reset(self):
        pass

    def validate(self):
        self.state = self.STATE_DONE

    def update_objects(self):
        pass

    def draw_intro(self):
        pass

    def draw_actr_wait_connect(self):
        pass
        
    def draw_actr_wait_model(self):
        pass

    def draw_fixation(self):
        pass

    def draw_search(self):
        pass

    def update_env(self):
        pass
            
    def handle_mouse_event(self, pos):
        pass
                
    def handle_key_press(self, key, code):
        pass

    def process_event(self):
        while True:
            yield

    def setDefaultClock(self):
        self.lc1.stop()
        self.lc1.clock = reactor
        self.lc1.start(1.0 / 30)

    if ACTR6:

        @d.listen('connectionMade')
        def ACTR6_JNI_Event(self, model, params):
            self.state = self.STATE_WAIT_MODEL

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
            # Simulate a button press using the "ACT-R" cursor loc
            self.handle_mouse_event(self.fake_cursor)

if __name__ == '__main__':

    pygame.display.init()
    pygame.font.init()
    pygame.mixer.init()

    env = Environment(actr=True)
    reactor.run()
