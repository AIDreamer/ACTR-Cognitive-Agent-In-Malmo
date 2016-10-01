## Name: Son Pham
## Summer Research 2016
## Prof. Dancy
## Different kinds of Chunks used to transfer information

## =================
## CHUNK DEFINITIONS
## =================

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

    def __init__(self, name, isa, screenx, screeny, width = None, height = None, color = None, size = None,
                 value = None,
                 health = None, state = None,
                 up = None, down = None, left = None, right = None,
                 item_array = None,
                 **slots):
        super(VisualChunk, self).__init__(name, isa, **slots)
        self.screenx = screenx
        self.screeny = screeny
        self.width = width
        self.height = height
        self.color = color
        self.size = size

        # Block type variables
        self.value = value

        # Stats variables
        self.state = state
        self.health = health

        # Map variables
        self.up = up
        self.down = down
        self.left = left
        self.right = right

        # Inventory variables
        self.item_array = item_array

    def set_health(self, health):
        self.health = health

    def set_up(self, up):
        self.up = up

    def set_down(self, down):
        self.down = down

    def set_left(self, left):
        self.left = left

    def set_right(self, right):
        self.right = right

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
        if self.health:
            chunk["slots"]["health"] = self.health
        if self.state:
            chunk["slots"]["status"] = self.state
        if self.up:
            chunk["slots"]["up"] = self.up
        if self.down:
            chunk["slots"]["down"] = self.down
        if self.left:
            chunk["slots"]["left"] = self.left
        if self.right:
            chunk["slots"]["right"] = self.right
        if self.item_array:
            for i in self.item_array:
                chunk["slots"]["item"+str(i)] = self.item_array[i] if self.item_array[i] else None
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
        if self.state:
            chunk["slots"]["status"] = self.state
        if self.health:
            chunk["slots"]["health"] = self.health
        if self.up:
            chunk["slots"]["up"] = self.up
        if self.down:
            chunk["slots"]["down"] = self.down
        if self.left:
            chunk["slots"]["left"] = self.left
        if self.right:
            chunk["slots"]["right"] = self.right
        if self.item_array:
            for i in self.item_array:
                chunk["slots"]["item"+str(i)] = self.item_array[i] if self.item_array[i] else None
        return chunk

class PAAVChunk(VisualChunk):
    
    def get_visual_location(self):
        chunk = super(PAAVChunk, self).get_visual_location()
        for s, v in self.slots.iteritems():
            if s in ["fcolor", "fshape", "fsize", "fshading", "forient"]:
                chunk["slots"][s] = v
        return chunk

class GroundBlock(object):

    def __init__(self, screenx, screeny, block_type):
        self.x = screenx
        self.y = screeny
        self.block_type = block_type

    def __repr__(self):
        return self.block_type

    def toChunk(self):
        return VisualChunk(None, "groundobj", self.x, self.y, value = self.block_type)

class AirBlock(object):

    def __init__(self, screenx, screeny, block_type):
        self.x = screenx
        self.y = screeny
        self.block_type = block_type

    def toChunk(self):
        return VisualChunk(None, "airobj", self.x, self.y, value = self.block_type)

class StatsBlock(object):
    def __init__(self, screenx, screeny, state, health):
        self.x = screenx
        self.y = screeny
        self.state = state
        self.health = health

    def toChunk(self):
        return VisualChunk(None, "statsobj", self.x, self.y, state = self.state, health = self.health)

class MapBlock(object):
    def __init__(self, screenx, screeny, up, down, left, right, posX, posY):
        self.x = screenx
        self.y = screeny
        self.up = up
        self.down = down
        self.left = left
        self.right = right
        self.posX = posX
        self.posY = posY

    def toChunk(self):
        return VisualChunk(None, "mapobj", self.x, self.y, up = self.up, down = self.down, left = self.left, right = self.right)

class InventoryBlock(object):
    def __init__(self, screenx, screeny,
                 item_array):
        self.x = screenx
        self.y = screeny
        self.item_array = item_array

    def toChunk(self):
        return VisualChunk(None, "inventoryobj", self.x, self.y, item_array = self.item_array)
