# This file was mostly generated by vtcl-1.5.2
# I did the layout there, then hacked it up mostly
# because I'm not sure how to get vtcl to do what
# I want so it's easier to fix it by hand.  In the process
# I removed the font 'support' stuff that was added automatically
# (things like get_font etc) because I've got my own font
# mechanism - there's a global set of font names for the
# environment that are set in the fonts.tcl init file.


# First, I define some variables and a procedure so that
# until ACT-R is up and running it will show a little
# text indicator in the control panel

set waiting_counter 0
set got_actr_connection 0

proc display_waiting_message {} {
  global waiting_counter
  global global_status
  global got_actr_connection 
  global init_error

  if {$waiting_counter == 0} {
    set global_status "Waiting for ACT-R |"
    set waiting_counter 1
  } elseif {$waiting_counter == 1} {
    set global_status "Waiting for ACT-R /"
    set waiting_counter 2
  } elseif {$waiting_counter == 2} {
    set global_status "Waiting for ACT-R -"
    set waiting_counter 3
  } elseif {$waiting_counter == 3} {
    set global_status "Waiting for ACT-R \\"
    set waiting_counter 0
  } else {
    # always need a saftey case, and it's getting late...
    set global_status "You shouldn't see this"
  }

  if {$got_actr_connection == 0} {
    after 1000 {display_waiting_message}
  } else {
    if {$init_error} {
      set global_status "Init Error"
    } else {
      set global_status ""
    }
  }
} 

global copyrightlab30var 
   
proc select_copyrights {} {
  global copyrightlab30var 
  global got_actr_connection
  global environment_socket
  global local_connection
  global options_array
  global screen_center_x
  global screen_center_y

  if {[winfo exists .copyright] == 1} {
    wm deiconify .copyright
    focus -force .copyright
  } else {
    # here the window gets created and set up

    toplevel .copyright
    wm withdraw .copyright

    
    # Don't want to do this now since the
    # "shrunk" state ends up getting saved and
    # people don't see this...
    # wm geometry .copyright [get_configuration .copyright]


    wm geometry .copyright \
    "400x290+[expr $screen_center_x - 200]+[expr $screen_center_y - 145]"

    wm overrideredirect .copyright 1

    label .copyright.lab22 -font intro_l_font -height 1 -text {ACT-R Environment} -anchor center
    label .copyright.lab30 -font intro_l_font -height 2 -textvariable copyrightlab30var -anchor center
    label .copyright.lab24 -font intro_s_font -height 1 -text {� 2002-2016} -anchor center
    label .copyright.lab33 -font intro_s_font -height 1 -anchor center -text {Dan Bothell} 
    label .copyright.lab43 -font intro_s_font -height 1 -anchor center -text {John R. Anderson}  
    label .copyright.lab35 -font intro_s_font -height 1 -anchor center -text {Department of Psychology, Carnegie Mellon University}  

    global tcl_env_dir

    image create photo small_onr_logo -file [file join $tcl_env_dir dialogs smalllogo.gif]

    label .copyright.image -width 400 -height 142 -anchor center -image small_onr_logo


    ###################
    # SETTING GEOMETRY
    ###################
    pack .copyright.lab22
    pack .copyright.lab30 
    pack .copyright.lab24
    pack .copyright.lab33
    pack .copyright.lab43
    pack .copyright.lab35
    pack .copyright.image

    # get the version strings, but being careful because an
    # overzelous clicker could clear the dialog before the 
    # register messages arrive, so make sure the update messages
    # arrive (they're sent after the registers) before showing
    # the window

    set copyrightlab30var ""
    set local_connection ""
     
    # actually wait for the Lisp side to get up to speed...

    wait_for_non_null environment_socket

    # signal that the connection has been made
    # and stop the spinning indicator

    set got_actr_connection 1

    send_environment_cmd \
      "create simple-handler .copyright.lab30 copyrightlab30var \
         (lambda (x) (declare (ignore x)) (format nil \"ACT-R ~a~%version ~a\" *actr-architecture-version* *actr-version-string*)) nil"

    send_environment_cmd \
      "create simple-handler .checkstate local_connection \
         (lambda (x) (declare (ignore x)) *local-connection*) nil"

    wait_for_non_null copyrightlab30var

    wait_for_non_null local_connection


    # show the window if the user wants it
    # sort of cheats because it still gets "built"

    if {$options_array(show_copyrights) == 1} {
      wm deiconify .copyright
    }
  }
}

# start the lisp side running

if {$standalone_mode == 1} {
  
   set cur_dir [pwd]

   global top_dir

   cd [file join $top_dir apps]

   if [catch {exec "actr-s-64.exe" -V}] {
     if [catch {exec "actr-s-32.exe" -V}] {
       tk_messageBox -icon warning -title "No ACT-R available" \
                     -message "Cannot run the ACT-R application. Contact Dan for help." -type ok
       exit
     } else {
      exec "./run-32.bat" &
     }
   } else {
      exec "./run-64.bat" &
   }

   cd $cur_dir
}


# show something until it starts...

display_waiting_message

select_copyrights

# then if it's supposed to be shown 

if {$options_array(show_copyrights) == 1} {

  # create a variable that will trigger the destruction of the window
  # after 5 seconds or when the window is clicked

  set clear_copyright 0

  # set the variable that triggers destruction after 3 seconds
  # or as soon as a button is pressed on the window

  after 5000 {set clear_copyright 1}
  bind .copyright <ButtonPress> {set clear_copyright 1}

  # wait for one of the triggering events to happen

  tkwait variable clear_copyright
}

# close the window

destroy .copyright

remove_handler .copyright.lab30
remove_handler .checkstate
