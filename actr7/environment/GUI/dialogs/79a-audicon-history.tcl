
proc make_audicon_history_viewer {key current} {
  
  set win [toplevel [new_variable_name .audicon_history]]
  
  wm withdraw $win

  wm geometry $win [get_configuration .audicon_history $win]
  
  # frame and list box for times

  set list_frame_1 [frame $win.list_frame_1 -borderwidth 0]  
  
  set list_box_1 [listbox $list_frame_1.list_box -listvar \
                        $list_frame_1.list_box.var \
                        -yscrollcommand "$list_frame_1.list_scrl set" \
                        -selectmode single \
                        -exportselection 0 -font list_font -bd 0]

  
  if $current {
    send_environment_cmd "create list-box-handler $list_box_1 $list_box_1 dummy-env-handler nil $key"
  } else {
    send_environment_cmd "create list-box-handler $list_box_1 $list_box_1 dummy-env-handler nil"
  }
  
  bind $list_box_1 <Destroy> {
    remove_handler %W
  }
  
  set list_scroll_bar_1 [scrollbar $list_frame_1.list_scrl \
                                 -command "$list_box_1 yview"]


  # The lables for the sections

  set l1 [label $win.l1 -text "Times" -justify left -font label_font]
  set l3 [label $win.l3 -text "Audicon" -justify left -font label_font]

  # frame for the audicon display

  set text_frame_1 [frame $win.text_frame_1 -borderwidth 0]  
 
  set text_box_1 [text $text_frame_1.text -yscrollcommand "$text_frame_1.text_scrl set"  \
                                          -xscrollcommand "$text_frame_1.hscrl set" \
                                          -wrap none -font text_font]
  
  if $current {
    send_environment_cmd "create text-handler $text_box_1 $text_box_1 (lambda (x)(declare (ignore x)) \" \") nil $key"
  } else {
    send_environment_cmd "create text-handler $text_box_1 $text_box_1 (lambda (x)(declare (ignore x)) \" \") nil"
  }

  bind $text_box_1 <Destroy> {
    remove_handler %W
  }
  
  bind $text_box_1 <1> {focus %W}

  # create the scroll bars for the text box
  
  set text_scroll_bar_1 [scrollbar $text_frame_1.text_scrl -command "$text_box_1 yview"]
  set h_scrl_bar [scrollbar $text_frame_1.hscrl -command "$text_box_1 xview" -orient horizontal]


  # make the selection list call the display updater for the audicon

  bind $list_box_1 <<ListboxSelect>> [list select_audicon_history %W $text_box_1 $key]


  button $win.get -text "Get History" -font button_font -command [list get_audicon_history_data $list_box_1 $key]

    
  pack $list_scroll_bar_1 -side right -fill y 
  pack $list_box_1 -side left -expand 1 -fill both
  pack $text_scroll_bar_1 -side right -fill y
  pack $h_scrl_bar -side bottom -fill x 
  pack $text_box_1 -side left -expand 1 -fill both

  place $win.get -relx 0 -y 0 -height 25 -relwidth .10
  place $l1 -relx 0.0 -y 25 -height 25 -relwidth .10
  place $list_frame_1 -relx 0.0 -y 50 -relheight 1.0 -height -50 -relwidth .10

  place $l3 -relx .1 -y 0 -height 25 -relwidth .9
  place $text_frame_1 -relx .1 -y 25 -relheight 1.0 -height -25 -relwidth .9
  
  # now show the window 

  wm deiconify $win
  focus $win

  return $win
}


proc get_audicon_history_data {timelst key} {

  send_environment_cmd "update [get_handler_name $timelst] \
    (lambda (x) \
      (declare (ignore x)) \
      (audicon-history-time-list '$key))"
}



proc select_audicon_history {timewin target_win key} {
  if [valid_handler_name $target_win] {
    set selections [$timewin curselection]
    if {[llength $selections] != 0} {
      set time [$timewin get [lindex $selections 0]]
    
      send_environment_cmd "update [get_handler_name $target_win] \
          (lambda (x) \
             (declare (ignore x)) \
             (audicon-history-text \"$time\" '$key))"
    } else {
      send_environment_cmd \
        "update [get_handler_name $target_win] (lambda (x) (declare (ignore x))\" \")" 
    }
  }
}


add_history_button make_audicon_history_viewer "Audicon" :save-audicon-history "Audicon history" right get-audicon-history default-save-history-info
