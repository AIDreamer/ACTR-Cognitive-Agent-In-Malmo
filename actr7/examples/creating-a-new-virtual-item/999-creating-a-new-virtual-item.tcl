# This file is an extension for the ACT-R Environment to actually 
# display .gif files as images in the visible-virtual-windows
# using the image-vdi objects created in the creating-a-new-virtual-item.lisp
# file.
# 
# To use this extension of the Environment place this file into
# the environment/GUI/dialogs directory of the Environment before
# starting it, and also place the images directory and the .gif
# files it contains into the environment/GUI directory.
#
# The code below replaces the procedure that displays the
# items in the window in the Environment based on the data
# provided from Lisp.  It includes a new case for the image
# item that gets sent over and makes appropriate changes to
# ensure that the item will also be removed when the window
# is cleared. 

# Mouse clicks on the item will be handled in the virtual
# view on the Lisp side just like they would if the window
# were not shown even if a person clicks on it (the window
# sends all clicks back to Lisp where they get processed).

# This is the procedure that is responsible for handling
# the display of visible-virtual-windows.  We are redefining
# it here to add the handling for the new image items.
# Only the changes being made are commented below, and again
# the full details of the Environment communication are beyond
# the scope of this example.

proc process_env_window {dummy cmd} {
  global id_references
  global cursor_id
  global attention_id
  global eyeloc_id
  global button_references
  global tcl_env_dir

  set win ".[lindex $cmd 1]"
 
  switch [lindex $cmd 0] {
    ignore {
    }
    select {
      wm deiconify $win
      raise $win
      focus -force $win
    }
    close {
      # Make sure to delete any images that may
      # be in the display to avoid memory issues

      set images [$win.can find withtag image]

      foreach i $images {
        $win.can dtag $i image
        set name [lindex [$win.can gettags $i] 0]
        image delete $name
      }

      $win.can delete all
      destroy $win 
    }
    clear {
      $win.can delete line button text image # add the new tag so it gets cleared
    }
    open {
      create_env_window $win [lindex $cmd 2] [lindex $cmd 5] [lindex $cmd 6] [lindex $cmd 3] [lindex $cmd 4]  
    }
    remove {

      # If the item being removed is an image
      # then delte the image item to save memory

      set tags [$win.can gettags [lindex $cmd 2]]
      set tindex [lsearch -exact $tags image]
      if {$tindex != -1} {
        image delete [lindex $cmd 2]
      }

      $win.can delete [lindex $cmd 2]
    }

    attention {
      $win.can delete "[lindex $cmd 4]-attention"
      set x [lindex $cmd 2]
      set y [lindex $cmd 3]
      $win.can create oval [expr $x - 10] [expr $y - 10] [expr $x + 10] [expr $y + 10] -outline [lindex $cmd 5] -width 4 -tags [list "[lindex $cmd 4]-attention" attention]
    }

    clearattention {
      $win.can delete "[lindex $cmd 2]-attention"
    }

    eyeloc {
      $win.can delete "[lindex $cmd 4]-eyeloc"
      set x [lindex $cmd 2]
      set y [lindex $cmd 3]
      $win.can create oval [expr $x - 5] [expr $y - 5] [expr $x + 5] [expr $y + 5] -outline blue -width 3 -tags [list "[lindex $cmd 4]-eyeloc" eyeloc]
    }

    cleareyeloc {
      $win.can delete "[lindex $cmd 2]-eyeloc"
    }


    cursor {
      $win.can delete cursor
      set x [lindex $cmd 2]
      set y [lindex $cmd 3]
      $win.can create polygon $x $y $x [expr $y + 15] [expr $x + 5] [expr $y + 12] [expr $x + 9] [expr $y + 20] \
                              [expr $x + 13] [expr $y + 19] [expr $x + 9] [expr $y + 12] [expr $x + 15] [expr $y + 12] \
                              -fill white -outline black -tag cursor
    }

    line {
      $win.can create line [lindex $cmd 3] [lindex $cmd 4] \
                         [lindex $cmd 6] [lindex $cmd 7] \
                         -fill [lindex $cmd 5] -width 2 -tags [list [lindex $cmd 2] line]
    }
    text {
      $win.can create text [lindex $cmd 3] [lindex $cmd 4] \
                              -font [list courier [lindex $cmd 7] roman] -fill [lindex $cmd 5] \
                              -text [lindex $cmd 6] -anchor nw -tags [list [lindex $cmd 2] text]
    }
    button {
      set x1 [lindex $cmd 3]
      set y1 [lindex $cmd 4]
      set x2 [expr $x1 + [lindex $cmd 5]]
      set y2 [expr $y1 + [lindex $cmd 6]]
      set base_color [lindex $cmd 8]
      set name [lindex $cmd 2]                                                                                                     

      $win.can create rectangle $x1 $y1 $x2 $y2 \
                                -fill $base_color -outline white -width 1 -tags [list $name button "$name-box-up"]

      $win.can create rectangle $x1 $y1 $x2 $y2 \
                                -fill [lighten_color $win.can $base_color 2] -outline black -width 1 -tags [list $name button "$name-box-down"]
                                                                                                     

      $win.can create text [expr $x1 + round([lindex $cmd 5] / 2)] [expr $y1 + round([lindex $cmd 6] / 2)] \
                           -font button_font -fill black -text [lindex $cmd 7] -anchor center -tags [list $name button "$name-up"]

      $win.can create text [expr $x1 + round([lindex $cmd 5] / 2) + 1] [expr $y1 + round([lindex $cmd 6] / 2) + 1] \
                           -font button_font -fill black -text [lindex $cmd 7] -anchor center -tags [list $name button "$name-down"]



      $win.can create line $x2 $y1 $x2 $y2 -width 1 -fill black -tags [list $name button "$name-up"]
      $win.can create line $x2 $y2 $x1 $y2 -width 1 -fill black -tags [list $name button "$name-up"]
      $win.can create line $x2 $y1 $x2 $y2 -width 1 -fill white -tags [list $name button "$name-down"]
      $win.can create line $x2 $y2 $x1 $y2 -width 1 -fill white -tags [list $name button "$name-down"]

      $win.can create line [expr $x1 + 1] [expr $y1 + 1] [expr $x1 + 1] [expr $y2 - 1] -width 1 -fill darkgray -tags [list $name button "$name-down"]
      $win.can create line [expr $x1 + 1] [expr $y1 + 1] [expr $x2 - 1] [expr $y1 + 1] -width 1 -fill darkgray -tags [list $name button "$name-down"]
      $win.can create line [expr $x2 - 1] [expr $y1 + 1] [expr $x2 - 1] [expr $y2 - 1] -width 1 -fill darkgray -tags [list $name button "$name-up"]
      $win.can create line [expr $x2 - 1] [expr $y2 - 1] [expr $x1 + 1] [expr $y2 - 1] -width 1 -fill darkgray -tags [list $name button "$name-up"]

      $win.can raise "$name-box-up" "$name-box-down"
      $win.can raise "$name-up" "$name-box-up"
      $win.can lower "$name-down" "$name-box-up"
   
    }
    click {
      
      set name [lindex $cmd 2]

 
      $win.can raise "$name-box-down" "$name-box-up"
      $win.can raise "$name-down" "$name-box-down"
      $win.can lower "$name-up" "$name-box-down"

      after 100 "flip_button_view $win \"$name-box-down\" \"$name-box-up\" \"$name-down\" \"$name-up\""
    }

    # this is our new item and the data provided in the list
    # from the add-visual-items-to-rpm-window method are used
    # here.
                                                                        
    image { 

      # Read the image file specified from the images directory which is in the GUI directory
      # of the running Environment -- environment/GUI/images for a source distribution or
      # the GUI/images directory of a standalone Environment distribution.  Assume that it
      # exists to keep the example simple.    

      image create photo [lindex $cmd 2] -file [file join $tcl_env_dir "images" [lindex $cmd 5]] -width [lindex $cmd 6] -height [lindex $cmd 7]
                                                                                                     
      # Place that image into the display.  Again, this is kept simple for an example, but it
      # should probably actually first create a 'dummy' image and subsample or zoom to fit 
      # the width and height specified instead of just clipping the image.

      $win.can create image [lindex $cmd 3] [lindex $cmd 4] -anchor nw -image [lindex $cmd 2] -tags [list [lindex $cmd 2] image] 
    } 
  }

  $win.can raise cursor all
  $win.can raise eyeloc all
  $win.can raise attention all
}
