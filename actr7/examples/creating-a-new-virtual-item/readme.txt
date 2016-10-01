This directory contains files that demonstrate how to add a new
visual feature to the virtual windows with which ACT-R models
can interact. 

The creating-a-new-virtual-item.lisp file has an example which
shows how to add a new feature for an "image" item which is 
sensitive to mouse clicks and will call an action function in
response to a model click.

The creating-a-new-virtual-item-model.lisp file shows how
to create instances of that new feature, add them to a display,
and have the model interact with them.

In addition to that there is also an extension for the ACT-R
Environment which shows how one can extend the visible virtual
windows to actually display new virtual features in the real
environment window.  For the new image features, a .gif file
can be displayed in the experiment window and it will respond
to both model and user clicks.  That extension is in the 
999-creating-a-new-virtual-item.tcl file and there are two
.gif files in the included images directory which can be used
with the example model.

Details on how the extensions work and how to use the example 
model can be found in the comments of the files.