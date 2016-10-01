

proc select_pgraph {key current} {

  set win [toplevel [new_variable_name .pgraph]]

  global $win.scale

  global $win.p_viewer
  set $win.p_viewer 0

  wm withdraw $win

  wm geometry $win [get_configuration .pgraph $win]


  frame $win.frame -borderwidth 0  
    
  canvas $win.frame.canvas  \
         -xscrollcommand "$win.frame.scrlx set" \
         -yscrollcommand "$win.frame.scrly set" \
         -width 900 -height 300 -scrollregion {0 0 900 300} -bg white
   
       
  scrollbar $win.frame.scrlx \
              -command "$win.frame.canvas xview" -orient horizontal

  scrollbar $win.frame.scrly \
              -command "$win.frame.canvas yview" -orient vertical

  global $win.return

  if $current {
    send_environment_cmd "create list-handler $win.frame.canvas $win.return (lambda (x) (declare (ignore x))) nil $key"
  } else {
    send_environment_cmd "create list-handler $win.frame.canvas $win.return (lambda (x) (declare (ignore x))) nil"
  }

  bind $win.frame.canvas <Destroy> "clear_p_history_data $win
                                    remove_handler $win.frame.canvas"


  global $win.which
  global $win.count
  global $win.max

  button $win.update -command [list pgraph_button_action $win "All Transitions" :all disabled $key] -text "All Transitions" -font button_font

  label $win.text -font text_font  -textvariable $win.textvar
  
  set $win.textvar ""

  label $win.notes -font text_font  -textvariable $win.notesvar -anchor nw
  
  set $win.notesvar ""

  button $win.grid -command [list pgraph_button_action $win Frequencies :freq normal $key] -text "Frequencies" -font button_font

  button $win.grid1 -command [list pgraph_button_action $win Cycles :cycle normal $key] -text "Cycles" -font button_font

  button $win.grid2 -command [list pgraph_button_action $win "Unique Cycles" :color normal $key] -text "Unique Cycles" -font button_font

  button $win.grid3 -command [list pgraph_button_action $win Runs :run normal $key] -text "Runs" -font button_font

  button $win.grid4 -command [list pgraph_button_action $win "Unique Runs" :run-color normal $key] -text "Unique Runs" -font button_font

  button $win.grid5 -command [list pgraph_button_action $win Utilities :utility normal $key] -text "Utilities" -font button_font


  label $win.cur_d -font text_font  -textvariable $win.current_display -anchor n
  label $win.of -font text_font  -text "of" -anchor n
  label $win.max_d -font text_font  -textvariable $win.max_display -anchor n
  
  button $win.next -command [list pgraph_flip_button_action increment_p_graph_count $win $key] -text "+" -font button_font -state disable 

  button $win.previous -command [list pgraph_flip_button_action decrement_p_graph_count $win $key] -text "-" -font button_font -state disable
 

  label $win.type_box -font text_font  -textvariable $win.type -anchor nw

  button $win.save -command "save_phistory_graph $win" -text "Save as .eps" -font button_font
  button $win.save2 -command "save_phistory_graph_dot $win" -text "Save as .dot" -font button_font

  checkbutton $win.check \
              -text "Hide unused productions" \
              -font checkbox_font \
              -variable $win.check_val \
              -onvalue nil -offvalue t

  global $win.scale
  set $win.scale 1.0

  global $win.labels
  global $win.boxes
  global $win.links
  global $win.graph_name 

  set $win.graph_name "$key"


  pack $win.frame.scrlx -side bottom -fill x
  pack $win.frame.scrly -side right -fill y
  pack $win.frame.canvas -side left -fill both
 
  place $win.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0 -height -75
    
  place $win.notes -x 500 -rely 1.0 -y -25 -relwidth 1.0 -width -500 -height 24
  place $win.text -x 0 -rely 1.0 -y -25 -width 100 -height 24

  place $win.update -x 0 -rely 1.0 -y -50 -width 100 -height 24
  place $win.grid -x 100 -rely 1.0 -y -50 -width 100 -height 24
  place $win.grid1 -x 200 -rely 1.0 -y -50 -width 100 -height 24
  place $win.grid2 -x 300 -rely 1.0 -y -50 -width 100 -height 24
  place $win.grid3 -x 400 -rely 1.0 -y -50 -width 100 -height 24
  place $win.grid4 -x 500 -rely 1.0 -y -50 -width 100 -height 24
  place $win.grid5 -x 600 -rely 1.0 -y -50 -width 100 -height 24

  place $win.type_box -relx 0 -relwidth .5 -width -135 -rely 1.0 -y -75 -height 24
  place $win.of -relx .5 -x -15 -width 30 -rely 1.0 -y -75 -height 24
  place $win.cur_d -relx .5 -x -95 -width 80 -rely 1.0 -y -75 -height 24
  place $win.max_d -relx .5 -x 15 -width 80 -rely 1.0 -y -75 -height 24
  place $win.next -relx .5 -x 95 -width 40 -rely 1.0 -y -75 -height 24
  place $win.previous -relx .5 -x -135 -width 40 -rely 1.0 -y -75 -height 24

  place $win.save -x 100 -rely 1.0 -y -25 -width 100 -height 24
  place $win.save2 -x 200 -rely 1.0 -y -25 -width 100 -height 24
  place $win.check -x 300 -rely 1.0 -y -25 -width 175 -height 24

  wm deiconify $win

  return $win
} 

proc pgraph_button_action {win type which state key} {
  
  global $win.count
  global $win.which
  global $win.type

  set $win.type $type
  set $win.which $which
  set $win.count 0
  $win.next configure -state $state
  $win.previous configure -state $state
  draw_p_graph $win [send_model_name] t $key
}

proc pgraph_flip_button_action {action win key} {

  $action $win
  draw_p_graph $win [send_model_name] nil $key
}

proc clear_p_history_data {win} {
 
  send_environment_cmd "update [get_handler_name $win.frame.canvas] (lambda (x) (declare (ignore x)) (remove-p-history-entry (cons '[get_handler_name $win.frame.canvas] '$win)) (list t))"
}
                                     

proc increment_p_graph_count {win} {
  global $win.count
  upvar 0 $win.count count
  global $win.max
  upvar 0 $win.max max

  if {$count != $max} {
    incr count 1
  }
}

proc decrement_p_graph_count {win} {
  global $win.count
  upvar 0 $win.count count
  if {$count != 0} {
    incr count -1
  }
}


proc draw_p_graph {win model clear_all key} {

  if {[$win.next cget -state] == "normal"} {
    set enable_next 1
  } else {
    set enable_next 0
  }

  $win.update configure -state disabled
  $win.grid configure -state disabled
  $win.grid1 configure -state disabled
  $win.grid2 configure -state disabled
  $win.grid3 configure -state disabled
  $win.grid4 configure -state disabled
  $win.grid5 configure -state disabled
  $win.next configure -state disabled
  $win.previous configure -state disabled
  $win.save configure -state disabled
  $win.save2 configure -state disabled

  global $win.return
  
  if {$clear_all == "nil"} {
    $win.frame.canvas delete transient
  } else {
    $win.frame.canvas delete all 
  }

  upvar 2 $win.textvar display
  upvar 2 $win.notesvar notes
  upvar 2 $win.current_display current_display
  upvar 2 $win.max_display max_display

              
  global $win.max
  global $win.count
  global $win.which

  upvar 2 $win.which kind
  upvar 2 $win.count count
  upvar 2 $win.max max

  global $win.check_val

  upvar 2 $win.check_val show_unused


  set display "Busy"

  global $win.scale

  upvar 2 $win.scale scale

  set c_height 0
  set c_width 0
  set t_height 0
  set t_width 0
  set n_width 0
  set x_display 0
   

  global $win.labels
  global $win.boxes
  global $win.links

  upvar 2 $win.labels labels
  upvar 2 $win.boxes boxes
  upvar 2 $win.links links

  set links [list]
  set boxes [list]
  if {$clear_all == "t"} {
    set labels [list]
  }

  set done 0

  set min_time ""
  set max_time ""
  set max 0

  while {$done == 0} {
             
    set $win.return ""
                  
    send_environment_cmd "update [get_handler_name $win.frame.canvas] (lambda (x) (declare (ignore x)) (create-production-graph-coords (cons '[get_handler_name $win.frame.canvas] '$win) $kind $count $clear_all $show_unused '$key))"

    wait_for_non_null $win.return

    upvar 2 $win.return result

    foreach x $result {
      switch [lindex $x 0] {
        label { 
        
          set p [lindex $x 1]

          if {[lindex $x 8] == "gray"} {
            lappend labels "\"$p\" \[ color = gray \]"
          } else {
            lappend labels "\"$p\""
          }

          $win.frame.canvas create text [lindex $x 2] [lindex $x 3] -anchor center -font text_font -text $p -tag $p
          $win.frame.canvas bind $p <ButtonPress> "p_graph_p_view $win $p {[lindex $x 9]}"
          $win.frame.canvas create rectangle [lindex $x 4] [lindex $x 5] [lindex $x 6] [lindex $x 7] -outline [lindex $x 8] -width 2
        }

        box { 
          lappend boxes "\"[lindex $x 1]\" \[ color = [lindex $x 6] \]"

          $win.frame.canvas create rectangle [lindex $x 2] [lindex $x 3] [lindex $x 4] [lindex $x 5] -outline [lindex $x 6] -width 2 -tag transient
        }
        link {
          if {[lindex $x 3] == "gray"} {
            lappend links "\"[lindex $x 1]\" -> \"[lindex $x 2]\" \[ style=dashed color=gray \]"
            $win.frame.canvas create line [lrange $x 5 end] -fill [lindex $x 3] -arrow last -smooth 1 -width [lindex $x 4] -dash ".-" -tag transient
          } else {
            lappend links "\"[lindex $x 1]\" -> \"[lindex $x 2]\""
            $win.frame.canvas create line [lrange $x 5 end] -fill [lindex $x 3] -arrow last -smooth 1 -width [lindex $x 4] -tag transient
          }
        }  
        min_time {
          set min_time [lindex $x 1]
        }
        max_time {
          set max_time [lindex $x 1]
        }
        cycles {
          set max [lindex $x 1]
        }
        size { 
         $win.frame.canvas configure -width [lindex $x 1] -height [lindex $x 2] -scrollregion "0 0 [lindex $x 1] [lindex $x 2]"
        }

        done {
         set done 1
        }
      }
    }
  }

  $win.update configure -state normal
  $win.grid configure -state normal
  $win.grid1 configure -state normal
  $win.grid2 configure -state normal
  $win.grid3 configure -state normal
  $win.grid4 configure -state normal
  $win.grid5 configure -state normal

  if $enable_next {
    $win.next configure -state normal
    $win.previous configure -state normal
  }

  $win.save configure -state normal
  $win.save2 configure -state normal

  set display "Done"
 
  if {$min_time != "" && $max_time != ""} {
    set notes "$min_time - $max_time"
  } else {
    set notes ""
  }

  set current_display [expr $count + 1]
  set max_display [expr $max + 1]
}






proc p_graph_p_view {win name text} {

  # make a new window 
  set top [toplevel [new_variable_name ".p_graph_p_view"]]

  wm geometry $top [get_configuration .p_graph_p_view $top]

  wm title $top "[wm title $win] Production: $name"     

  frame $top.frame -borderwidth 0  
    
  set text_box [text $top.frame.text -font text_font \
                                     -yscrollcommand "$top.frame.scrl set" ]

  $text_box insert 0.0 $text
  
  bind $text_box <1> {focus %W}

  $text_box configure -state disabled

  set scrl_bar [scrollbar $top.frame.scrl -command "$top.frame.text yview"]
   
  place $top.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0
        
  pack $scrl_bar -side right -fill y 
  pack $text_box -side left -expand 1 -fill both
 
}





proc save_phistory_graph {win} {
  set fname [tk_getSaveFile -title "Save production graph as"\
                                  -filetypes {{"Encapsulated PostScript" "*.eps"}}]

  if {$fname != ""} {
    $win.frame.canvas postscript -file $fname -width [$win.frame.canvas cget -width] -height [$win.frame.canvas cget -height] -x 0 -y 0  -pageanchor nw -pagex 0.0 -pagey [$win.frame.canvas cget -height] -pagewidth [$win.frame.canvas cget -width]
  }
}


proc save_phistory_graph_dot {win} {
  set fname [tk_getSaveFile -title "Save production graph as"\
                                  -filetypes {{"DOT file" "*.dot"}}]

  if {$fname != ""} {
    global $win.labels
    global $win.boxes
    global $win.links
    global $win.graph_name

    upvar $win.labels labels
    upvar $win.boxes boxes
    upvar $win.links links
    upvar $win.graph_name name
 
    write_data "digraph $name \{\n" $fname

    foreach label $labels {
      append_data "  $label ;\n" $fname
    }

    foreach box $boxes {
      append_data "  $box ;\n" $fname
    }

    foreach link $links {
      append_data "  $link ;\n" $fname
    }

    append_data "\}" $fname
  }
}

add_history_button select_pgraph "Production Graph" :save-p-history "Production history" left get-p-history-data default-save-history-info
