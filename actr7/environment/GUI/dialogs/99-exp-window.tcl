# This file creates windows on the Environment side for showing
# Experiments generated with the AGI.
# There can be any number of experiment windows on the environment side,
# but there's only one "main" handler which isn't visible but is used
# as the target for all the communication: .env_window since I need 
# something to setup the handler on the Lisp side.

toplevel .env_window
wm withdraw .env_window

send_environment_cmd \
  "create env-window-handler .env_window .env_window \
    (lambda (x) \
      (declare (ignore x)) \
      (setf (environment-control-use-env-windows *environment-control*) $options_array(use_env_window) ) \
      (list 'ignore)) ()"

bind .env_window <Destroy> {
  remove_handler [get_handler_name .env_window]
}



proc send_keypress_to_actr {win key} {
  send_environment_cmd "update [get_handler_name .env_window] \
                        (lambda (x) \
                          (declare (ignore x)) \
                          (env-window-key-pressed \"$win\" \"$key\") \
                          (list 'ignore))"
  }


proc send_click_to_actr {win x y} {
  send_environment_cmd "update [get_handler_name .env_window] \
                        (lambda (x) \
                          (declare (ignore x)) \
                          (env-window-click \"$win\" $x $y) \
                               (list 'ignore))"
  }



proc create_env_window {win title width height x y} {


  toplevel $win
  wm withdraw $win

  # don't really close it when the close button pressed
  wm protocol $win WM_DELETE_WINDOW "wm withdraw $win"

  # send all the keypresses to ACT-R

  bind $win <KeyPress> "send_keypress_to_actr $win %K"
 
  

  wm title $win $title
  wm geometry $win [format "%dx%d+%d+%d" $width $height $x $y]

  canvas $win.can
  place $win.can -x 0 -y 0 -relwidth 1.0 -relheight 1.0

  # Send all mouse clicks now too

  bind $win <Button-1> "send_click_to_actr $win %x %y"

  wm deiconify $win
  focus -force $win
}

 
proc lighten_color {w c degree} {
 set max [lindex [winfo rgb $w white] 0]
 set offset [expr ($max + 1) / $degree]
 set size [string length [format "%x" $max]]                 
 set clist [winfo rgb $w $c]
 set c1 [lindex $clist 0]
 set c2 [lindex $clist 1]
 set c3 [lindex $clist 2]
 return [format "\#%0*X%0*X%0*X" \
                $size [expr ($c1 + $offset) < $max ? $c1 + $offset : $max] \
                $size [expr ($c2 + $offset) < $max ? $c2 + $offset : $max] \
                $size [expr ($c3 + $offset) < $max ? $c3 + $offset : $max]]
}

proc flip_button_view {win box_down box_up down up} {
  # assume that if one of the tagged items still exists
  # that they all do since it's single threaded

  set check [$win.can find withtag $box_up]
 
  if {$check != ""} {

      $win.can raise "$box_up" "$box_down"
      $win.can raise "$up" "$box_up"
      $win.can lower "$down" "$box_up"
  }
}


global id_references
global button_references


set cursor_id ""
set attention_id ""
set eyeloc_id ""


proc process_env_window {dummy cmd} {
  global id_references
  global cursor_id
  global attention_id
  global eyeloc_id
  global button_references
  global options_array

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
      $win.can delete all
      destroy $win 
    }
    clear {
      $win.can delete line button text
    }
    open {
      create_env_window $win [lindex $cmd 2] [lindex $cmd 5] [lindex $cmd 6] [lindex $cmd 3] [lindex $cmd 4]  
    }
    remove {
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
   # NOT actually making that an option, but keeping the code for reference!
   #  if {$options_array(real_buttons) == 1} {
   #    set but \
   #      [button [new_variable_name $win.can.but] -text [lindex $cmd 7] -bg [lindex $cmd 8] -font button_font\
   #              -command "send_button_press_to_actr $win [lindex $cmd 2]"]

   #     $win.can create window [lindex $cmd 3] [lindex $cmd 4] \
   #                               -window $but -anchor nw \
   #                               -height [lindex $cmd 6] -width [lindex $cmd 5] -tags [list [lindex $cmd 2] button]
   #  } else {

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

      # set things in the "up" position

      $win.can raise "$name-box-up" "$name-box-down"
      $win.can raise "$name-up" "$name-box-up"
      $win.can lower "$name-down" "$name-box-up"
   
   #  }
    }
    click {
     # if {$options_array(real_buttons) == 1} {
     # 
     #   set but [$win.can itemcget [$win.can find withtag [lindex $cmd 2]] -window]
     #   set old "[$but cget -command]"
     # 
     #   $but configure -command ""
     #   lappend suppress_button_clicks $but
     #
     #   event generate $but <ButtonPress> -button 1
     #   after 100 "restore_suppressed_button $but [list $old]"
     # } else {
      
     set name [lindex $cmd 2]

      # set things in the down position

      $win.can raise "$name-box-down" "$name-box-up"
      $win.can raise "$name-down" "$name-box-down"
      $win.can lower "$name-up" "$name-box-down"

      after 100 "flip_button_view $win \"$name-box-down\" \"$name-box-up\" \"$name-down\" \"$name-up\""
     # }
    }
  }

  # make sure these show over everything

  $win.can raise cursor all
  $win.can raise eyeloc all
  $win.can raise attention all
}
