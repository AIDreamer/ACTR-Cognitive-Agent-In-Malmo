
proc make_text_trace_history_viewer {key current} {
  
  set win [toplevel [new_variable_name .text_trace_history]]
  
  wm withdraw $win

  # Don't considier it associated with current model   record_new_window $win $win
  # just set its title to reflect the key

  wm geometry $win [get_configuration .text_trace_history $win]
  
  # frame and radio buttons for level

  set level_frame_1 [frame $win.level_frame_1 -borderwidth 0]  
  
  set rb1 [radiobutton $level_frame_1.rb1 -text "low" -variable $level_frame_1.level -value "'low" -anchor w]
  set rb2 [radiobutton $level_frame_1.rb2 -text "medium" -variable $level_frame_1.level -value "'medium" -anchor w]
  set rb3 [radiobutton $level_frame_1.rb3 -text "high" -variable $level_frame_1.level -value "'high" -anchor w]
  set rb4 [radiobutton $level_frame_1.rb4 -text "ALL" -variable $level_frame_1.level -value "t" -anchor w]

  $rb1 invoke

  # set the range for the trace

  label $win.range -font text_font -text "From"
  label $win.to -font text_font -text "to"
 
  entry $win.min -text "" -font text_font
  entry $win.max -text "" -font text_font

  # The lables for the sections

  set l1 [label $win.l1 -text "Level" -justify left -font label_font]
  set l3 [label $win.l3 -text "Trace" -justify left -font label_font]


  # frame for the trace display

  set text_frame_1 [frame $win.text_frame_1 -borderwidth 0]  
 
  set text_box_1 [text $text_frame_1.text -yscrollcommand "$text_frame_1.text_scrl set"  \
                                          -xscrollcommand "$text_frame_1.hscrl set" \
                                          -wrap none -font text_font -state disabled]
  

  if $current {
    send_environment_cmd "create text-handler $text_box_1 $text_box_1 (lambda (x) (declare (ignore x)) \" \") nil $key"
  } else {
    send_environment_cmd "create text-handler $text_box_1 $text_box_1 (lambda (x) (declare (ignore x)) \" \") nil"
  }

  bind $text_box_1 <Destroy> {
    remove_handler %W
  }
  
  bind $text_box_1 <1> {focus %W}

  # create the scroll bars for the text box
  
  set text_scroll_bar_1 [scrollbar $text_frame_1.text_scrl -command "$text_box_1 yview"]
  set h_scrl_bar [scrollbar $text_frame_1.hscrl -command "$text_box_1 xview" -orient horizontal]


  button $win.get -text "Get Trace" -font button_font -command [list get_trace_history_data $text_box_1 $level_frame_1.level $win.min $win.max $key]
    
  pack $rb1 -side top -expand 1 -fill x
  pack $rb2 -side top -expand 1 -fill x
  pack $rb3 -side top -expand 1 -fill x
  pack $rb4 -side top -expand 1 -fill x

  pack $text_scroll_bar_1 -side right -fill y
  pack $h_scrl_bar -side bottom -fill x 
  pack $text_box_1 -side left -expand 1 -fill both

  place $win.get -relx 0 -y 0 -height 25 -relwidth .15
  place $win.range -relx .15 -y 0 -height 25 -relwidth .20
  place $win.min -relx .35 -y 0 -height 25 -relwidth .25
  place $win.to -relx .6 -y 0 -height 25 -relwidth .10
  place $win.max -relx .70 -y 0 -height 25 -relwidth .25

  place $l1 -relx 0.0 -y 25 -height 25 -relwidth .15
  place $level_frame_1 -relx 0.0 -y 50 -height 100 -relwidth .15

  place $l3 -relx .15 -y 25 -height 25 -relwidth .85
  place $text_frame_1 -relx .15 -y 50 -relheight 1.0 -height -50 -relwidth .85
  
  # now show the window 

  wm deiconify $win
  focus $win

  return $win
}


proc get_trace_history_data {trace_window level_var min_entry max_entry key} {

  global $level_var

  set min "nil"
  set max "nil"

  scan [$min_entry get] "%f" min
  scan [$max_entry get] "%f" max
  upvar $level_var level

  $trace_window configure -state normal
  $trace_window delete 1.0 end
  $trace_window insert 1.0 "Waiting..."
  $trace_window configure -state disabled

  send_environment_cmd "update [get_handler_name $trace_window] \
    (lambda (x) \
      (declare (ignore x)) \
      (text-history-trace :detail $level :start $min :end $max :key '$key))"
}


add_history_button make_text_trace_history_viewer "Text Trace" :save-trace "Text trace" left get-saved-text-trace default-save-history-info
