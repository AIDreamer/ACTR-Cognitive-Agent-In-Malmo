
global history_recorder_dummy
global history_buffer_list
global history_save_result
global history_initial_states

global history_traces
global history_buttons

set history_buttons ""
set history_traces ""


proc kill_history_recorder {win kill} {
 
  if $kill {
    
    remove_handler $win.l1

    bind $win.l1 <Destroy> ""

    destroy $win
  }
}

proc make_history_recorder {} {

  global history_buffer_list
  global history_recorder_dummy
  global history_initial_states
  global history_traces
  global history_buttons

  if {[currently_selected_model] == "nil"} {
    tk_messageBox -icon info -type ok -title "Select Data" -message "Select Data tool requires a current model."
  } else {
    set win_name .history_recorder_[currently_selected_model]

    if {[winfo exists $win_name] == 1} {
      wm deiconify $win_name
      raise $win_name
    } else {
  
      set win [toplevel .history_recorder_[currently_selected_model]]

      wm withdraw $win

      record_new_window $win "Select Data"

      wm geometry $win [get_configuration .history_recorder $win]


      send_environment_cmd "create simple-funcall-handler $win kill_history_recorder (lambda (x) (format nil \"$win ~a~%\" (if (and (null x) (eq '[currently_selected_model] (current-model))) 1 0))) (delete)"

      bind $win <Destroy> "remove_handler $win"
  
      set l1 [label $win.l1 -text "Information to record for selected buffers" -justify left -font label_font]

      set history_recorder_dummy ""

      send_environment_cmd "create simple-handler $l1 history_recorder_dummy history-recorder-set-initial-params (reset) [send_model_name]"

      bind $l1 <Destroy> {remove_history_recorder %W}

      wait_for_non_null history_recorder_dummy
 
      # frame and checkboxes for available traces to record

      set checkbox_frame [frame $win.checkbox_frame -borderwidth 0]  

      set history_initial_states ""

      set traces ""
      set data ""
      set saves ""

      foreach x $history_traces {
        lappend traces [lindex $x 0]
        lappend data [lindex $x 2]
        lappend saves [lindex $x 3]
      }

      send_environment_cmd \
        "create list-handler $checkbox_frame history_initial_states \
            (lambda (x) (declare (ignore x)) \
                  (history-recorder-initial-flags (list $traces) '($data) '($saves))) \
             nil [send_model_name]"

      bind $checkbox_frame <Destroy> "remove_handler $checkbox_frame"

      wait_for_non_null history_initial_states


      for {set i 0} {$i < [llength $history_traces]} {incr i} {
        set cb [checkbutton $checkbox_frame.cb$i -text [lindex [lindex $history_traces $i] 1] -anchor w -variable $checkbox_frame.cb$i]
 
        set param [lindex [lindex $history_traces $i] 0]

        if [lindex $history_initial_states $i] {
          $cb select
        } else {
          $cb deselect
        }

        $cb configure -command "update_history_state $cb [get_handler_name $checkbox_frame] $param"
        pack $cb -side top -fill x
      }
                     
      # frame and checkboxes for buffers

      set buffer_frame [frame $win.buffer_frame -borderwidth 0]  

      set history_buffer_list ""

      send_environment_cmd \
        "create list-handler $buffer_frame history_buffer_list \
                history-recorder-initial-buffers nil [send_model_name]"

      wait_for_non_null history_buffer_list

      bind $buffer_frame <Destroy> "remove_handler $buffer_frame"

      foreach x $history_buffer_list {
        set name [lindex $x 0]
        set state [lindex $x 1]
 
        set b [checkbutton $buffer_frame.$name -text $name -anchor w -variable $buffer_frame.$name]

        if $state {
          $b select
        } else {
          $b deselect
        }
        
        $b configure -command "update_history_buffer_state $b [get_handler_name $buffer_frame] $name"
        pack $b -side top -fill x
      }

      button $win.save -text "Save Currently Recorded Information" -font button_font -command "save_history_information $win.save"

      send_environment_cmd \
        "create list-handler $win.save history_save_result \
                dummy-env-handler nil [send_model_name]"

      send_environment_cmd "update [get_handler_name $l1] history-recorder-reset-params"


      bind $win.save <Destroy> "remove_handler $win.save"

      global local_connection 

      if {$local_connection == 0} {
        $win.save configure -state disabled
      }


      place $l1 -relx 0.0 -y 0 -height 25 -relwidth 1.0
                        
      place $checkbox_frame -relx 0.0 -y 25 -relheight 1.0 -height -50 -relwidth .5
      place $buffer_frame -relx .5 -y 25 -relheight 1.0 -height -50 -relwidth .5
  
      place $win.save -x 20 -y -25 -rely 1.0 -height 25 -relwidth 1.0 -width -40

                 
      # now show the window 

      wm deiconify $win
      focus $win

      return $win
    }
  }
}

proc remove_history_recorder {win} {
  send_environment_cmd "update [get_handler_name $win] disable-history-recorder" 
  remove_handler $win
}

proc update_history_state {box handler param} {

  global $box
  upvar $box box_state

  if $box_state {
    send_environment_cmd "update $handler \
    (lambda (x) \
      (declare (ignore x)) \
      (add-history-trace-param $param))"

  } else {
    send_environment_cmd "update $handler \
    (lambda (x) \
      (declare (ignore x)) \
      (remove-history-trace-param $param))"
  }
}


proc update_history_buffer_state {box handler param} {

  global $box
  upvar $box box_state

  if $box_state {
    send_environment_cmd "update $handler \
    (lambda (x) \
      (declare (ignore x)) \
      (add-history-traced-buffer '$param))"
  } else {
    send_environment_cmd "update $handler \
    (lambda (x) \
      (declare (ignore x)) \
      (remove-history-traced-buffer '$param))"
  }
}

proc save_history_information {button} {

  global history_save_result

  global options_array
  global local_connection
  global top_dir

  set fname ""

  if {$local_connection == 0} {
    tk_messageBox -icon warning -type ok -title "Save Data warning" \
                  -message "You cannot use the Save Currently Recorded Information button if the\
                            Environment is not running on the same machine as ACT-R."  
  } else {
    set fname [tk_getSaveFile -title "Save Recorded Data As" \
                              -filetypes {{"All Files" *}} \
                              -initialdir $top_dir]
  

    if {$fname != ""} {
      set history_save_result ""

      send_environment_cmd \
        "update [get_handler_name $button] \
           (lambda (x) \
              (declare (ignore x)) \
                (if (environment-busy-p) \
                  (list 0 \"Environment is busy.\") \
                  (if (and (mp-running?) (not (stepper-open-p))) \
                    (list 0 \"Model is running and Stepper not open.\") \
                    (unwind-protect \
                      (progn \
                        (set-environment-busy) \
                        (save-history-information \"$fname\")) \
                      (set-environment-free)))))"
  
      wait_for_non_null history_save_result
 
      set win [toplevel [new_variable_name .save_history_response]]
  
      wm withdraw $win

      wm geometry $win [get_configuration .save_history_response $win]


      set text_frame [frame $win.text_frame -borderwidth 0]  
 
      set text_box [text $text_frame.text -yscrollcommand \
                     "$text_frame.text_scrl set" -state normal \
                     -font text_font]
    
      set text_scroll_bar [scrollbar $text_frame.text_scrl \
                                     -command "$text_box yview"]


      set the_button [button $win.but -text "Ok" -font button_font -command "destroy $win"]

     place $text_frame -x 0 -y 0 -relheight 1.0 -height -30 -relwidth 1.0
     place $the_button -relx .5 -x -30 -width 60 -rely 1.0 -y -30 -height 30

     pack $text_scroll_bar -side right -fill y
     pack $text_box -side left -expand 1 -fill both

     if {[lindex $history_save_result 0] == 0} {
        wm title $win "ERROR Saving Recorded Data"
        $text_box insert 1.0 [lindex $history_save_result 1]
     } else {
        wm title $win "SUCCESSFUL Saving Recorded Data"
        $text_box insert 1.0 "Successful Save:\n[lindex $history_save_result 1]"
     }

     wm deiconify $win
     focus $win
    }
  }              
}

proc add_history_button {command text parameter label side live save} {
  global history_buttons
  global history_traces

  if {[lsearch $history_traces "$parameter *"] == -1} {
    lappend history_traces [list $parameter $label $live $save]
  }

  lappend history_buttons [list $parameter $text $command $side]
}
  


proc record_window_with_model {win_name model} {
  
  global window_to_model
  global model_to_windows
  global options_array

  if {$options_array(multiple_models) == 1} {

    set window_to_model($win_name) $model

    wm title $win_name "$win_name ($model)"

    if {[array names model_to_windows -exact $model] == ""} {
      set model_to_windows($model) [list $win_name]
    } else {
      set model_to_windows($model) [concat $model_to_windows($model) $win_name]
    }
  } else {
    wm title $win_name "$win_name ($model)"
  }
}

global available_tools

proc make_history_playback {} {

  global history_traces
  global history_buttons

  set win [toplevel [new_variable_name .view_data]]

  wm withdraw $win

#  it's not tied to the model that was current
#  when it was opened -- can be retargeted to any
#  current model
#  record_new_window $win $win

  wm geometry $win [get_configuration .history_playback $win]

  set data_frame [frame $win.data -borderwidth 3 -relief ridge]

  label $data_frame.source -text "Source:" -font label_font
    
  set source [label $data_frame.source_data -font text_font -anchor w -textvar $win.source_var]

  global $win.source_var

  set $win.source_var "Current"
   
  label $data_frame.model -font label_font -text "Model:"

  set model [label $data_frame.model_data -font text_font -anchor w -text [currently_selected_model]]

  place $data_frame.source -x 4 -y 2 -height 25 -width 50
  place $data_frame.model -x 4 -y 27 -height 25 -width 50
  place $source -x 55 -y 2 -height 25 -relwidth 1.0 -width -60
  place $model -x 55 -y 27 -height 25 -relwidth 1.0 -width -60


  set left [frame $win.left -borderwidth 0]
  set right [frame $win.right -borderwidth 0]

  set i 0

  global $win.but_array

  upvar 0 $win.but_array buts

  foreach x $history_traces {
   set buts([lindex $x 0]) [list]
  }

  foreach x $history_buttons {
    set side [lindex $x 3]

    set b [button $win.$side.b$i -text [lindex $x 1] -state disabled -command "open_history_viewer [lindex $x 2] $data_frame"]

    pack $b -side top 
    incr i

    set buts([lindex $x 0]) [linsert $buts([lindex $x 0]) end $b]
  }


  set choice_frame [frame $win.choice -borderwidth 0]

  set live [button $choice_frame.live -text "Current"]
  $live configure -command "switch_to_live_data $live $model $win"
    
  set load [button $choice_frame.load -text "Load Saved"]
  $load configure -command "switch_to_load_data $load $model $win"

  global local_connection 

  if {$local_connection == 0} {
    $load configure -state disabled
  }

  place $live -relx .25 -y 0 -height 25 -width 110 -anchor n
  place $load -relx .75 -y 0 -height 25 -width 110 -anchor n
    
  place $choice_frame -x 0 -y 3 -height 30 -relwidth 1.0
  place $data_frame -x 0 -y 33 -height 60 -relwidth 1.0
  place $left -relx 0.0 -y 96 -relheight 1.0 -relwidth .5 -height -96
  place $right -relx .5 -y 96 -relheight 1.0 -relwidth .5 -height -96

  $live invoke

  bind $source <Destroy> "close_playback $win"


  wm deiconify $win
  focus $win
}

proc open_history_viewer {command data_frame} {
  if {[$data_frame.source_data cget -text] == "Current"} {

    set win [$command [$data_frame.model_data cget -text] 1]
    
    record_window_with_model $win [$data_frame.model_data cget -text]

  } else {
    set key "(\"[$data_frame.source_data cget -text]\")"

    global history_recorder_dummy

    set history_recorder_dummy ""

    send_environment_cmd \
      "create simple-handler $command history_recorder_dummy \
                (lambda (x) (declare (ignore x)) (add-saved-file-user '$key)) nil"

    wait_for_non_null history_recorder_dummy

    remove_handler $command

    set win [$command $key 0]

    wm title $win "$win $key"

    bind $win <Destroy> [list remove_window_user %W $win $key]
  }
}

proc remove_window_user {window win key} {

  if { $window == $win } {
    global history_recorder_dummy

    set history_recorder_dummy ""

    send_environment_cmd \
      "create simple-handler $win history_recorder_dummy \
              (lambda (x) (declare (ignore x)) (remove-saved-file-user '$key)) nil"
 
    wait_for_non_null history_recorder_dummy
  
    remove_handler $win
  }
}

proc close_playback {win} {

  global $win.source_var
  upvar $win.source_var source

  if {$source != "Current"} {

    set key "(\"$source\")"

    remove_window_user $win $win $key
  }
}



proc switch_to_live_data {but model win} {

  set model_name [currently_selected_model]

  global $win.but_array
  global history_recorder_dummy
  global $win.source_var
  global available_tools


  upvar 0 $win.but_array buts
  upvar 0 $win.source_var source

  if {$source != "Current"} {

    set key "(\"$source\")"

    remove_window_user $but $but $key
  }

  if {$model_name == "nil"} {
    set source "Current"
    $model configure -text "No current model available"


    set searchid [array startsearch buts]

    while { [array anymore buts $searchid] } {
    
      set element [array nextelement buts $searchid]

      foreach y $buts($element) {
        $y configure -state disabled
      }
    }

    array donesearch buts $searchid

  } else {

    set history_recorder_dummy ""

    send_environment_cmd \
      "create list-handler $but history_recorder_dummy \
                (lambda (x) (declare (ignore x)) (select-live-data '$model_name '([array names buts]))) nil $model_name"


    wait_for_non_null history_recorder_dummy

    remove_handler $but

    if [lindex $history_recorder_dummy 0] {
      set source "Current"
      $model configure -text $model_name
    } else {
      set source "Current"
      $model configure -text "No current model available"
    }    

    set available_tools($model_name) ""

    set searchid [array startsearch buts]

    while { [array anymore buts $searchid] } {
    
      set element [array nextelement buts $searchid]

      foreach y $buts($element) {
        if { [lindex $history_recorder_dummy 0] && [lsearch $history_recorder_dummy $element] > 0 } {
          $y configure -state normal

          lappend available_tools($model_name) [lindex [$y cget -command] 1]

        } else {
          $y configure -state disabled
        }
      }
    }
  
    array donesearch buts $searchid    
  }
}


proc switch_to_load_data {but model win} {

  global local_connection

  if {$local_connection == 0} {
    tk_messageBox -icon warning -type ok -title "Save Data warning" \
                  -message "You cannot load Saved Information if the Environment \
                            is not running on the same machine as ACT-R."  
  } else {

    global $win.but_array

    upvar 0 $win.but_array buts


    global $win.source_var
  
    upvar 0 $win.source_var source

    global history_recorder_dummy
    global options_array
    global top_dir
    global available_tools

    set fname ""

    set fname [tk_getOpenFile -title "Load saved history information" \
                              -filetypes {{"All Files" *}} \
                              -initialdir $top_dir]
  
    if {$fname != ""} {

      set history_recorder_dummy ""

      send_environment_cmd \
        "create list-handler $but history_recorder_dummy \
            (lambda (x) \
              (declare (ignore x)) \
                (if (environment-busy-p) \
                  (list 0 \"Environment is busy.\") \
                  (if (and (mp-running?) (not (stepper-open-p))) \
                    (list 0 \"Model is running and Stepper not open.\") \
                    (unwind-protect \
                      (progn \
                        (set-environment-busy) \
                        (load-history-information \"$fname\" '([array names buts]))) \
                      (set-environment-free))))) nil"
  
      wait_for_non_null history_recorder_dummy

      remove_handler $but

      if [lindex $history_recorder_dummy 0] {
      
        if {$source != "Current"} {
          set key "(\"$source\")"
          remove_window_user $but $but $key
        }

        set source $fname
        $model configure -text [lindex $history_recorder_dummy 1]

        set key "(\"$source\")"
      
        set available_tools($key) ""
 
        set searchid [array startsearch buts]

        while { [array anymore buts $searchid] } {
    
          set element [array nextelement buts $searchid]

          foreach y $buts($element) {
            if { [lindex $history_recorder_dummy 0] && [lsearch $history_recorder_dummy $element] > 1 } {
              $y configure -state normal
              lappend available_tools($key) [lindex [$y cget -command] 1]
            } else {
              $y configure -state disabled
            }
          }
        }
  
        array donesearch buts $searchid    

      } else {
        tk_messageBox -icon warning -type ok -title "Load History warning" \
                      -message "Unable to load history from $fname because [lindex $history_recorder_dummy 1]."  
      }    
    }
  }
}

proc history_custom_buffer_view {w key current buffer start_time end_time} {
 
## For now I'm going to hard-code the handlers, but the "right"
## solution would be to have the tools indicate that...
## Should also access the buttons in the parent window to note
## if the tool is available, but for now just going to "try"
## and open it with the window able to handle the failure.

  global handler_names

  set result [open_trace_viewer $w $buffer $key $current]

  set exists [lindex $result 0]

  set win [lindex $result 1]

  if {$exists == 2} {return 0}

  switch $buffer {
    blending -
    retrieval {

      set time_box "$win.list_frame_1.list_box"
      set chunk_box "$win.list_frame.list_box"

      set button "$win.get"

      if {$exists == 0 && [winfo viewable $time_box] == 0} {tkwait visibility $time_box}
      if {$exists == 0 && [winfo viewable $chunk_box] == 0} {tkwait visibility $chunk_box}

      wait_for_handler_ready $time_box
      wait_for_handler_ready $chunk_box

      # should probably also wait for the text box handlers...

      $button invoke

      global  $win.list_frame_1.list_box.var
      wait_for_non_null $win.list_frame_1.list_box.var

      set index [lsearch -exact [$time_box get 0 end] [start_time_in_seconds $start_time] ]

      if {$exists == 1} {$time_box selection clear 0 end}

      $time_box selection set $index
      
      $time_box see $index

      global  $win.list_frame.list_box.var

      if {$exists == 1} {set $win.list_frame.list_box.var ""}

      event generate $time_box <<ListboxSelect>>

      wait_for_non_null $win.list_frame.list_box.var

      $chunk_box selection set 0

      set selections [$chunk_box curselection]
   
      while {[llength $selections] == 0} {
        set selections [$chunk_box curselection]   
      }

      event generate $chunk_box <<ListboxSelect>>
    }
    production {
      
      set button "$win.update"
      if {$exists == 0 && [winfo viewable $button] == 0} {tkwait visibility $button}

      upvar $win.textvar display

      if {$exists == 1 && $display == "Busy"} {

      # skip it since it's still trying to process one

      } else {
        set display ""

        $button invoke

        global hack_ticker

        while {$display != "Done"} {
          tkwait var hack_ticker
        }

        # columns are always 80 pixels wide at this point and the text is centered
        set x [expr [lindex [$win.frame.canvas coords [start_time_in_seconds $start_time]] 0] - 40]

        set width [$win.frame.canvas cget -width]
  
        set fract [expr (1.0 * $x) / $width]

        $win.frame.canvas xview moveto $fract
      }
    
    }
    
    default { 

      set time_box "$win.list_frame_1.list_box"

      set buffer_box "$win.list_frame.list_box"

      set button "$win.get"

      if {$exists == 0 && [winfo viewable $buffer_box] == 0} {tkwait visibility $buffer_box}
      if {$exists == 0 && [winfo viewable $time_box] == 0} {tkwait visibility $time_box}

      wait_for_handler_ready $time_box

      wait_for_handler_ready $buffer_box

      wait_for_handler_ready $win.text_frame_1.text

      $button invoke

      global  $win.list_frame.list_box.var
      wait_for_non_null $win.list_frame.list_box.var

      set index [lsearch -exact [$buffer_box get 0 end] $buffer]

      if {$exists == 1} {$buffer_box selection clear 0 end}

      $buffer_box selection set $index

      set selections [$buffer_box curselection]
   
      while {[llength $selections] == 0} {
        set selections [$buffer_box curselection]   
      }

#      don't need to generate both selections do I cuz that can lead
#      to a race condition on the return values ...
#      event generate $buffer_box <<ListboxSelect>>

      global  $win.list_frame_1.list_box.var
      wait_for_non_null $win.list_frame_1.list_box.var

      set index [lsearch -exact [$time_box get 0 end] [start_time_in_seconds $end_time] ]

      if {$exists == 1} {$time_box selection clear 0 end}

      $time_box see $index

      $time_box selection set $index
       
      set selections [$time_box curselection]
   
      while {[llength $selections] == 0} {
        set selections [$time_box curselection]   
      }

      event generate $time_box <<ListboxSelect>>
    }
  }
}


proc open_trace_viewer {w buffer key current} {

  global $w.subviews
  upvar 0 $w.subviews check

  global available_tools

  set exists 0

  if {[llength [array names check $buffer]] && [winfo exists $check($buffer)]} {
    set exists $check($buffer)
  }

   
  if {$exists != 0} {

    raise $exists

    return [list 1 $exists]
  } else {

    switch $buffer { 
      retrieval {set command make_declarative_history_viewer}
      blending {set command make_blending_history_viewer}
      production {set command select_ptrace}
      default {set command make_buffer_history_viewer}
    }

    if {[lsearch $available_tools($key) $command] == -1} {
    
      return [list 2 0]
    }

    set win [$command $key $current]

    set $w.subviews($buffer) $win

    if $current {
 
      record_window_with_model $win $key

    } else {

      global history_recorder_dummy

      set history_recorder_dummy ""

      send_environment_cmd \
        "create simple-handler $command history_recorder_dummy \
                (lambda (x) (declare (ignore x)) (add-saved-file-user '$key)) nil"

      wait_for_non_null history_recorder_dummy

      remove_handler $command
    
      wm title $win "$win $key"

      bind $win <Destroy> [list remove_window_user %W $win $key]
    }

    return [list 0 $win]
  }
}



proc start_time_in_seconds {time} {
  return [format "%.3f" [expr 0.001 * $time]]
}


# Make a button for the control panel that will open a new history recorder window

button [control_panel_name].history_recorder -command {make_history_recorder} \
       -text "Select Data" -font button_font

# put that button on the control panel

pack [control_panel_name].history_recorder


# Make a button for the control panel that will open a new history recorder window

button [control_panel_name].history_playback -command {make_history_playback} \
       -text "View Data" -font button_font

# put that button on the control panel

pack [control_panel_name].history_playback
