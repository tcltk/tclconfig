###
# Practcl
# An object oriented templating system for stamping out Tcl API calls to C
###
package require TclOO

proc debug args {
  ::practcl::cputs ::DEBUG_INFO $args
}

###
# Drop in a static copy of Tcl
###
proc ::doexec args {
  puts [list {*}$args]
  exec {*}$args >&@ stdout
}

proc ::domake {args} {
  puts [list make {*}$args]
  exec make {*}$args >&@ stdout
}

proc ::fossil {path args} {
  set PWD [pwd]
  cd $path
  puts [list {*}$args]
  exec fossil {*}$args >&@ stdout
  cd $PWD
}


###
# Build utility functions
###
namespace eval ::practcl {}

###
# Bits stolen from fileutil
###
proc ::practcl::cat fname {
    set fname [open $fname r]
    set data [read $fname]
    close $fname
    return $data
}

###
# Convert an MSYS path to a windows native path
###
if {$::tcl_platform(platform) eq "windows"} {
proc ::practcl::msys_to_tclpath msyspath {
  return [exec sh -c "cd $msyspath ; pwd -W"]
}
} else {
proc ::practcl::msys_to_tclpath msyspath {
  return [file normalize $msyspath]
}
}


proc ::practcl::file_lexnormalize {sp} {
    set spx [file split $sp]

    # Resolution of embedded relative modifiers (., and ..).

    if {
	([lsearch -exact $spx . ] < 0) &&
	([lsearch -exact $spx ..] < 0)
    } {
	# Quick path out if there are no relative modifiers
	return $sp
    }

    set absolute [expr {![string equal [file pathtype $sp] relative]}]
    # A volumerelative path counts as absolute for our purposes.

    set sp $spx
    set np {}
    set noskip 1

    while {[llength $sp]} {
	set ele    [lindex $sp 0]
	set sp     [lrange $sp 1 end]
	set islast [expr {[llength $sp] == 0}]

	if {[string equal $ele ".."]} {
	    if {
		($absolute  && ([llength $np] >  1)) ||
		(!$absolute && ([llength $np] >= 1))
	    } {
		# .. : Remove the previous element added to the
		# new path, if there actually is enough to remove.
		set np [lrange $np 0 end-1]
	    }
	} elseif {[string equal $ele "."]} {
	    # Ignore .'s, they stay at the current location
	    continue
	} else {
	    # A regular element.
	    lappend np $ele
	}
    }
    if {[llength $np] > 0} {
	return [eval [linsert $np 0 file join]]
	# 8.5: return [file join {*}$np]
    }
    return {}
}

proc ::practcl::file_relative {base dst} {
    # Ensure that the link to directory 'dst' is properly done relative to
    # the directory 'base'.

    if {![string equal [file pathtype $base] [file pathtype $dst]]} {
	return -code error "Unable to compute relation for paths of different pathtypes: [file pathtype $base] vs. [file pathtype $dst], ($base vs. $dst)"
    }

    set base [file_lexnormalize [file join [pwd] $base]]
    set dst  [file_lexnormalize [file join [pwd] $dst]]

    set save $dst
    set base [file split $base]
    set dst  [file split $dst]

    while {[string equal [lindex $dst 0] [lindex $base 0]]} {
	set dst  [lrange $dst  1 end]
	set base [lrange $base 1 end]
	if {![llength $dst]} {break}
    }

    set dstlen  [llength $dst]
    set baselen [llength $base]

    if {($dstlen == 0) && ($baselen == 0)} {
	# Cases:
	# (a) base == dst

	set dst .
    } else {
	# Cases:
	# (b) base is: base/sub = sub
	#     dst  is: base     = {}

	# (c) base is: base     = {}
	#     dst  is: base/sub = sub

	while {$baselen > 0} {
	    set dst [linsert $dst 0 ..]
	    incr baselen -1
	}
	# 8.5: set dst [file join {*}$dst]
	set dst [eval [linsert $dst 0 file join]]
    }

    return $dst
}

###
# Unpack the source of a fossil project into a designated location
###
proc ::practcl::fossil_sandbox {pkg args} {
  if {[llength $args]==1} {
    set info [lindex $args 0]
  } else {
    set info $args
  }
  set result $info
  if {[dict exists $info srcroot]} {
    set srcroot [dict get $info srcroot]
  } elseif {[dict exists $info sandbox]} {
    set srcroot [file join [dict get $info sandbox] $pkg]
  } else {
    set srcroot [file join [pwd] .. $pkg]
  }
  dict set result srcroot $srcroot
  if {[dict exists $info download]} {
    ###
    # Source is actually a zip archive
    ###
    set download [dict get $info download]
    if {[file exists [file join $download $pkg.zip]]} {
      if {![info exists $srcroot]} {
        package require zipfile::decode
        ::zipfile::decode::unzipfile [file join $download $pkg.zip] $srcroot
      }
      return
    }
  }
  variable fossil_dbs
  if {![::info exists fossil_dbs]} {
    # Get a list of local fossil databases
    set fossil_dbs [exec fossil all list]
  }
  set CWD [pwd]
  if {![dict exists $info tag]} {
    set tag trunk
  } else {
    set tag [dict get $info tag]
  }
  dict set result tag $tag
  try {
    if {[file exists [file join $srcroot .fslckout]]} {
      puts "FOSSIL UPDATE"
      cd $srcroot
      doexec fossil update $tag
    } elseif {[file exists [file join $srcroot _FOSSIL_]]} {
      puts "FOSSIL UPDATE"
      cd $srcroot
      doexec fossil update $tag
    } else {
      puts "OPEN AND UNPACK"
      set fosdb {}
      foreach line [split $fossil_dbs \n] {
        set line [string trim $line]
        if {[file rootname [file tail $line]] eq $pkg} {
          set fosdb $line
          break
        }
      }
      if {$fosdb eq {}} {
        set fosdb [file join $download fossil $pkg.fos]
        set cloned 0
        if {[dict exists $info localmirror]} {
          set localmirror [dict get $info localmirror]
          catch {
            doexec fossil clone $localmirror/$pkg $fosdb
            set cloned 1
          }
        }
        if {!$cloned && [dict exists $info fossil_url]} {
          set localmirror [dict get $info fossil_url]
          catch {
            doexec fossil clone $localmirror/$pkg $fosdb
            set cloned 1
          }
        }
        if {!$cloned} {
          doexec fossil clone http://fossil.etoyoc.com/fossil/$pkg $fosdb
        }
      }
      file mkdir $srcroot
      cd $srcroot
      puts "FOSSIL UNPACK"
      doexec fossil open $fosdb $tag
    }
  } on error {result opts} {
    puts [list ERR [dict get $opts -errorinfo]]
    return {*}$opts
  } finally {
    cd $CWD
  }
  return $result
}

###
# topic: e71f3f61c348d56292011eec83e95f0aacc1c618
# description: Converts a XXX.sh file into a series of Tcl variables
###
proc ::practcl::read_sh_subst {line info} {
  regsub -all {\x28} $line \x7B line
  regsub -all {\x29} $line \x7D line

  #set line [string map $key [string trim $line]]
  foreach {field value} $info {
    catch {set $field $value}
  }
  if [catch {subst $line} result] {
    return {}
  }
  set result [string trim $result]
  return [string trim $result ']
}

###
# topic: 03567140cca33c814664c7439570f669b9ab88e6
###
proc ::practcl::read_sh_file {filename {localdat {}}} {
  set fin [open $filename r]
  set result {}
  if {$localdat eq {}} {
    set top 1
    set local [array get ::env]
    dict set local EXE {}
  } else {
    set top 0
    set local $localdat
  }
  while {[gets $fin line] >= 0} {
    set line [string trim $line]
    if {[string index $line 0] eq "#"} continue
    if {$line eq {}} continue
    catch {
    if {[string range $line 0 6] eq "export "} {
      set eq [string first "=" $line]
      set field [string trim [string range $line 6 [expr {$eq - 1}]]]
      set value [read_sh_subst [string range $line [expr {$eq+1}] end] $local]
      dict set result $field [read_sh_subst $value $local]
      dict set local $field $value
    } elseif {[string range $line 0 7] eq "include "} {
      set subfile [read_sh_subst [string range $line 7 end] $local]
      foreach {field value} [read_sh_file $subfile $local] {
        dict set result $field $value
      }
    } else {
      set eq [string first "=" $line]
      if {$eq > 0} {
        set field [read_sh_subst [string range $line 0 [expr {$eq - 1}]] $local]
        set value [string trim [string range $line [expr {$eq+1}] end] ']
        #set value [read_sh_subst [string range $line [expr {$eq+1}] end] $local]
        dict set local $field $value
        dict set result $field $value
      }
    }
    } err opts
    if {[dict get $opts -code] != 0} {
      #puts $opts
      puts "Error reading line:\n$line\nerr: $err\n***"
      return $err {*}$opts
    }
  }
  return $result
}

###
# A simpler form of read_sh_file tailored
# to pulling data from (tcl|tk)Config.sh
###
proc ::practcl::read_Config.sh filename {
  set fin [open $filename r]
  set result {}
  while {[gets $fin line] >= 0} {
    set line [string trim $line]
    if {[string index $line 0] eq "#"} continue
    if {$line eq {}} continue
    catch {
      set eq [string first "=" $line]
      if {$eq > 0} {
        set field [string range $line 0 [expr {$eq - 1}]]
        set value [string trim [string range $line [expr {$eq+1}] end] ']
        #set value [read_sh_subst [string range $line [expr {$eq+1}] end] $local]
        dict set result $field $value
      }
    } err opts
    if {[dict get $opts -code] != 0} {
      #puts $opts
      puts "Error reading line:\n$line\nerr: $err\n***"
      return $err {*}$opts
    }
  }
  return $result
}

proc ::practcl::cputs {varname args} {
  upvar 1 $varname buffer
  if {[llength $args]==1 && [string length [string trim [lindex $args 0]]] == 0} {
    
  }
  if {[info exist buffer]} {
    if {[string index $buffer end] ne "\n"} {
      append buffer \n
    }
  } else {
    set buffer \n
  }
  # Trim leading \n's
  append buffer [string trimleft [lindex $args 0] \n] {*}[lrange $args 1 end]
}

proc ::practcl::_tagblock {text {style tcl} {note {}}} {
  if {[string length [string trim $text]]==0} {
    return {}
  }
  set output {}
  switch $style {
    tcl {
      ::practcl::cputs output "# BEGIN $note"
    }
    c {
      ::practcl::cputs output "/* BEGIN $note */"
    }
    default {
      ::practcl::cputs output "# BEGIN $note"
    }
  }
  ::practcl::cputs output $text
  switch $style {
    tcl {
      ::practcl::cputs output "# END $note"
    }
    c {
      ::practcl::cputs output "/* END $note */"
    }
    default {
      ::practcl::cputs output "# END $note"
    }
  }
  return $output
}

proc ::practcl::_isdirectory name {
  return [file isdirectory $name]
}
###
# topic: ebd68484cb7f18cad38beaab3cf574e2de5702ea
###
proc ::practcl::_istcl name {
  return [string match *.tcl $name]
}

###
# topic: 2e481bd24d970304a1dd0acad3d75198b56c122e
###
proc ::practcl::_istm name {
  return [string match *.tm $name]
}

###
# Return true if the pkgindex file contains
# any statement other than "package ifneeded"
# and/or if any package ifneeded loads a DLL
###
proc ::practcl::_pkgindex_directory {path} {
  set buffer {}
  set pkgidxfile [file join $path pkgIndex.tcl]
  if {![file exists $pkgidxfile]} {
    # No pkgIndex file, read the source
    foreach file [glob -nocomplain $path/*.tm] {
      set file [file normalize $file]
      set fname [file rootname [file tail $file]]
      ###
      # We used to be able to ... Assume the package is correct in the filename
      # No hunt for a "package provides"
      ###
      set package [lindex [split $fname -] 0]
      set version [lindex [split $fname -] 1]
      ###
      # Read the file, and override assumptions as needed
      ###
      set fin [open $file r]
      set dat [read $fin]
      close $fin
      # Look for a teapot style Package statement
      foreach line [split $dat \n] {
        set line [string trim $line]
        if { [string range $line 0 9] != "# Package " } continue
        set package [lindex $line 2]
        set version [lindex $line 3]
        break
      }
      # Look for a package provide statement
      foreach line [split $dat \n] {
        set line [string trim $line]              
        if { [string range $line 0 14] != "package provide" } continue
        set package [lindex $line 2]
        set version [lindex $line 3]
        break
      }
      append buffer "package ifneeded $package $version \[list source \[file join \$dir [file tail $file]\]\]" \n
    }
    foreach file [glob -nocomplain $path/*.tcl] {
      if { [file tail $file] == "version_info.tcl" } continue
      set fin [open $file r]
      set dat [read $fin]
      close $fin
      if {![regexp "package provide" $dat]} continue
      set fname [file rootname [file tail $file]]
      # Look for a package provide statement
      foreach line [split $dat \n] {
        set line [string trim $line]              
        if { [string range $line 0 14] != "package provide" } continue
        set package [lindex $line 2]
        set version [lindex $line 3]
        if {[string index $package 0] in "\$ \["} continue
        if {[string index $version 0] in "\$ \["} continue
        append buffer "package ifneeded $package $version \[list source \[file join \$dir [file tail $file]\]\]" \n
        break
      }
    }
    return $buffer
  }
  set fin [open $pkgidxfile r]
  set dat [read $fin]
  close $fin
  set thisline {}
  foreach line [split $dat \n] {
    append thisline $line \n
    if {![info complete $thisline]} continue
    set line [string trim $line]
    if {[string length $line]==0} {
      set thisline {} ; continue
    }
    if {[string index $line 0] eq "#"} {
      set thisline {} ; continue
    }
    try {
      # Ignore contditionals
      if {[regexp "if.*catch.*package.*Tcl.*return" $thisline]} continue
      if {[regexp "if.*package.*vsatisfies.*package.*provide.*return" $thisline]} continue
      if {![regexp "package.*ifneeded" $thisline]} {
        # This package index contains arbitrary code
        # source instead of trying to add it to the master
        # package index
        return {source [file join $dir pkgIndex.tcl]} 
      }
      append buffer $thisline \n
    } on error {err opts} {
      puts ***
      puts "GOOF: $pkgidxfile"
      puts $line
      puts $err
      puts [dict get $opts -errorinfo]
      puts ***
    } finally {
      set thisline {}
    }
  }
  return $buffer
}


proc ::practcl::_pkgindex_path_subdir {path} {
  set result {}
  foreach subpath [glob -nocomplain [file join $path *]] {
    if {[file isdirectory $subpath]} {
      lappend result $subpath {*}[_pkgindex_path_subdir $subpath]
    }
  }
  return $result
}
###
# Index all paths given as though they will end up in the same
# virtual file system
###
proc ::practcl::pkgindex_path args {
  set stack {}
  set buffer {
lappend ::PATHSTACK $dir
  }
  foreach base $args {
    set base [file normalize $base]
    set paths [::practcl::_pkgindex_path_subdir $base]
    set i    [string length  $base]
    # Build a list of all of the paths
    foreach path $paths {
      if {$path eq $base} continue
      set path_indexed($path) 0
    }
    set path_indexed($base) 1
    set path_indexed([file join $base boot tcl]) 1
    #set path_index([file join $base boot tk]) 1
  
    foreach path $paths {
      if {$path_indexed($path)} continue
      set thisdir [file_relative $base $path]
      #set thisdir [string range $path $i+1 end]
      set idxbuf [::practcl::_pkgindex_directory $path]
      if {[string length $idxbuf]} {
        incr path_indexed($path)
        append buffer "set dir \[set PKGDIR \[file join \[lindex \$::PATHSTACK end\] $thisdir\]\]" \n
        append buffer [string map {$dir $PKGDIR} [string trimright $idxbuf]] \n
      } 
    }
  }
  append buffer {
set dir [lindex $::PATHSTACK end]  
set ::PATHSTACK [lrange $::PATHSTACK 0 end-1]
}
  return $buffer
}

###
# topic: 64319f4600fb63c82b2258d908f9d066
# description: Script to build the VFS file system
###
proc ::practcl::installDir {d1 d2} {

  puts [format {%*sCreating %s} [expr {4 * [info level]}] {} [file tail $d2]]
  file delete -force -- $d2
  file mkdir $d2

  foreach ftail [glob -directory $d1 -nocomplain -tails *] {
    set f [file join $d1 $ftail]
    if {[file isdirectory $f] && [string compare CVS $ftail]} {
      installDir $f [file join $d2 $ftail]
    } elseif {[file isfile $f]} {
	    file copy -force $f [file join $d2 $ftail]
	    if {$::tcl_platform(platform) eq {unix}} {
        file attributes [file join $d2 $ftail] -permissions 0644
	    } else {
        file attributes [file join $d2 $ftail] -readonly 1
	    }
    }
  }

  if {$::tcl_platform(platform) eq {unix}} {
    file attributes $d2 -permissions 0755
  } else {
    file attributes $d2 -readonly 1
  }
}

proc ::practcl::copyDir {d1 d2} {
  #puts [list $d1 -> $d2]
  #file delete -force -- $d2
  file mkdir $d2

  foreach ftail [glob -directory $d1 -nocomplain -tails *] {
    set f [file join $d1 $ftail]
    if {[file isdirectory $f] && [string compare CVS $ftail]} {
      copyDir $f [file join $d2 $ftail]
    } elseif {[file isfile $f]} {
      file copy -force $f [file join $d2 $ftail]
    }
  }
}



###
# Define the metaclass
###
::oo::class create ::practcl::object {

  method target {method args} {
    switch $method {
      is_unix { return [expr {$::tcl_platform(platform) eq "unix"}] }
    }
  }
  
  method include_dir args {
    my define add include_dir {*}$args
  }
  
  method include_directory args {
    my define add include_dir {*}$args
  }

  method child {method} {
    return {}
  }
  
  constructor {parent args} {
    my variable links define
    my graft {*}[$parent child organs]
    array set define [$parent child organs]
    array set define [$parent child define]
    array set links {}
    if {[llength $args]==1 && [file exists [lindex $args 0]]} {
      my InitializeSourceFile [lindex $args 0]
    } elseif {[llength $args] == 1} {
      array set define [lindex $args 0]
      my select
      my initialize
    } else {
      array set define $args
      my select
      my initialize
    }
  }  

  method select {} {
    my variable define
    set class {}
    if {[info exists define(class)]} {
      if {[info command $define(class)] ne {}} {
        set class $define(class)
      } elseif {[info command ::practcl::$define(class)] ne {}} {
        set class ::practcl::$define(class)
      } else {
        switch $define(class) {
          default {
            set class ::practcl::object
          }
        }
      }
    }
    if {$class ne {}} {
      oo::objdefine [self] class $class
    }
  }
  
  method InitializeSourceFile filename {
    my define set filename $filename
    set class {}
    switch [file extension $filename] {
      .tcl {
        set class ::practcl::dynamic
      }
      .h {
        set class ::practcl::cheader
      }
      .c {
        set class ::practcl::csource
      }
      .ini {
        switch [file tail $filename] {
          module.ini {
            set class ::practcl::module
          }
        }
      }
    }
    if {$class ne {}} {
      oo::objdefine [self] class $class
      my initialize
    }
  }
  
  method graft args {
    my variable organs
    if {[llength $args] == 1} {
      error "Need two arguments"
    }
    set object {}
    foreach {stub object} $args {
      dict set organs $stub $object
      oo::objdefine [self] forward <${stub}> $object
      oo::objdefine [self] export <${stub}>
    }
    return $object
  }
  
  method organ {{stub all}} {
    my variable organs
    if {![info exists organs]} {
      return {}
    }
    if { $stub eq "all" } {
      return $organs
    }
    if {[dict exists $organs $stub]} {
      return [dict get $organs $stub]
    }
  }
  
  method link {command args} {
    my variable links
    switch $command {
      add {
        ###
        # Add a link to an object that was externally created
        ###
        if {[llength $args] ne 2} { error "Usage: link add LINKTYPE OBJECT"}
        lassign $args linktype object
        if {[info exists links($linktype)] && $object in $links($linktype)} {
          return
        }
        lappend links($linktype) $object
      }
      remove {
        set object [lindex $args 0]
        if {[llength $args]==1} {
          set ltype *
        } else {
          set ltype [lindex $args 1]
        }
        foreach {linktype elements} [array get links $ltype] {
          if {$object in $elements} {
            set nlist {}
            foreach e $elements {
              if { $object ne $e } { lappend nlist $e }
            }
            set links($linktype) $nlist
          }
        }
      }
      list {
        if {[llength $args]==0} {
          return [array get links]
        }
        if {[llength $args] ne 1} { error "Usage: link list LINKTYPE"}
        lassign $args linktype
        if {![info exists links($linktype)]} {
          return {}
        }
        return $links($linktype)
      }
      dump {
        return [array get links]
      }
    }
  }
  
  method add args {
    my variable links
    set object [::practcl::object new [self] {*}$args]
    foreach linktype [$object linktype] {
      lappend links($linktype) $object
    }
    return $object
  }
  
  method initialize {} {}
    
  method define {submethod args} {
    my variable define
    switch $submethod {
      add {
        set field [lindex $args 0]
        if {![info exists define($field)]} {
          set define($field) {}
        }
        foreach arg [lrange $args 1 end] {
          if {$arg ni $define($field)} {
            lappend define($field) $arg
          }
        }
        return $define($field)
      }
      remove {
        set field [lindex $args 0]
        if {![info exists define($field)]} {
          return
        }
        set rlist [lrange $args 1 end]
        set olist $define($field)
        set nlist {}
        foreach arg $olist {
          if {$arg in $rlist} continue
          lappend nlist $arg
        }
        set define($field) $nlist
        return $nlist
      }
      exists {
        set field [lindex $args 0]
        return [info exists define($field)]
      }
      getnull -
      get -
      cget {
        set field [lindex $args 0]
        if {[info exists define($field)]} {
          return $define($field)
        }
        return [lindex $args 1]
      }
      set {
        if {[llength $args]==1} {
          array set define [lindex $args 0]
        } else {
          array set define $args
        }
      }
      default {
        array $submethod define {*}$args
      }
    }
  }
  
  method go {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    my variable links
    foreach {linktype objs} [array get links] {
      foreach obj $objs {
        $obj go
      }
    }
    debug [list /[self] [self method] [self class]]
  }
    
  method code {section body} {
    my variable code
    ::practcl::cputs code($section) $body
  }
  method script script {
    eval $script
  }
  
  method source filename {
    source $filename
  }
  
  method compile-products {} {
    set filename [my define get filename]
    set result {}
    if {$filename ne {}} {
      if {[my define exists ofile]} {
        set ofile [my define get ofile]
      } else {
        set ofile build/[my <module> define get localpath]_[file rootname [file tail $filename]].o
        my define set ofile $ofile
      }
      lappend result $ofile [list cfile $filename extra [my define get extra]]
    }
    foreach item [my link list subordinate] {
      lappend result {*}[$item compile-products]
    }
    return $result
  }
  
  method generate-include-directory {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    set result [my define get include_dir]
    foreach obj [my link list subordinate] {
      foreach path [$obj generate-include-directory] {
        lappend result $path
      }
    }
    return $result
  }
  
  method generate-debug {{spaces {}}} {
    set result {}
    ::practcl::cputs result "$spaces[list [self] [list class [info object class [self]] filename [my define get filename]] links [my link list]]"
    foreach item [my link list subordinate] {
      practcl::cputs result [$item generate-debug "$spaces  "]
    }
    return $result
  }

  # Empty template methods
  method generate-cheader {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    my variable code cfunct cstruct methods tcltype tclprocs
    set result {}
    if {[info exists code(header)]} {
      ::practcl::cputs result $code(header)
    }
    foreach obj [my link list subordinate] {
      ::practcl::cputs result "/* BEGIN [$obj define get filename] generate-cheader */"
      ::practcl::cputs result [$obj generate-cheader]
      ::practcl::cputs result "/* END [$obj define get filename] generate-cheader */"
    }
    debug [list cfunct [info exists cfunct]]
    if {[info exists cfunct]} {
      foreach {funcname info} $cfunct {
        if {[dict get $info public]} continue
        ::practcl::cputs result "[dict get $info header]\;"
      }
    }
    debug [list tclprocs [info exists tclprocs]]
    if {[info exists tclprocs]} {
      foreach {name info} $tclprocs {
        if {[dict exists $info header]} {
          ::practcl::cputs result "[dict get $info header]\;"
        }
      }
    }
    debug [list methods [info exists methods] [my define get cclass]]

    if {[info exists methods]} {
      set thisclass [my define get cclass]
      foreach {name info} $methods {
        if {[dict exists $info header]} {
          ::practcl::cputs result "[dict get $info header]\;"
        }
      }
      # Add the initializer wrapper for the class
      ::practcl::cputs result "static int ${thisclass}_OO_Init(Tcl_Interp *interp)\;"
    }
    return $result
  }
  
  method generate-public-define {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    my variable code
    set result {}
    if {[info exists code(public-define)]} {
      ::practcl::cputs result $code(public-define)
    }
    set result [::practcl::_tagblock $result c [my define get filename]]
    foreach mod [my link list subordinate] {
      ::practcl::cputs result [$mod generate-public-define]
    }
    return $result
  }
  method generate-public-macro {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    my variable code
    set result {}
    if {[info exists code(public-macro)]} {
      ::practcl::cputs result $code(public-macro)
    }
    set result [::practcl::_tagblock $result c [my define get filename]]
    foreach mod [my link list subordinate] {
      ::practcl::cputs result [$mod generate-public-macro]
    }
    return $result
  }
  method generate-public-typedef {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    my variable code cstruct
    set result {}
    if {[info exists code(public-typedef)]} {
      ::practcl::cputs result $code(public-typedef)
    }
    if {[info exists cstruct]} {
      # Add defintion for native c data structures
      foreach {name info} $cstruct {
        ::practcl::cputs result "typedef struct $name ${name}\;"
        if {[dict exists $info aliases]} {
          foreach n [dict get $info aliases] {
            ::practcl::cputs result "typedef struct $name ${n}\;"
          }
        }
      }
    }
    set result [::practcl::_tagblock $result c [my define get filename]]
    foreach mod [my link list subordinate] {
      ::practcl::cputs result [$mod generate-public-typedef]
    }
    return $result
  }
  method generate-public-structure {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    my variable code cstruct
    set result {}
    if {[info exists code(public-structure)]} {
      ::practcl::cputs result $code(public-structure)
    }
    if {[info exists cstruct]} {
      foreach {name info} $cstruct {
        if {[dict exists $info comment]} {
          ::practcl::cputs result [dict get $info comment]
        }
        ::practcl::cputs result "struct $name \{[dict get $info body]\}\;"
      }
    }
    set result [::practcl::_tagblock $result c [my define get filename]]
    foreach mod [my link list subordinate] {
      ::practcl::cputs result [$mod generate-public-structure]
    }
    return $result
  }
  method generate-public-headers {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    my variable code tcltype
    set result {}
    if {[info exists code(public-header)]} {
      ::practcl::cputs result $code(public-header)
    }
    if {[info exists tcltype]} {
      foreach {type info} $tcltype {
        if {![dict exists $info cname]} {
          set cname [string tolower ${type}]_tclobjtype
          dict set tcltype $type cname $cname
        } else {
          set cname [dict get $info cname]
        }
        ::practcl::cputs result "extern const Tcl_ObjType $cname\;"
      }
    }
    if {[info exists code(public)]} {
      ::practcl::cputs result $code(public)
    }
    set result [::practcl::_tagblock $result c [my define get filename]]
    foreach mod [my link list subordinate] {
      ::practcl::cputs result [$mod generate-public-headers]
    }
    return $result
  }
  
  method generate-stub-function {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    my variable code cfunct tcltype
    set result {}
    foreach mod [my link list subordinate] {
      foreach {funct def} [$mod generate-stub-function] {
        dict set result $funct $def
      }
    }
    if {[info exists cfunct]} {
      foreach {funcname info} $cfunct {
        if {![dict get $info export]} continue
        dict set result $funcname [dict get $info header]
      }
    } 
    return $result
  }
  
  method generate-public-function {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]    
    my variable code cfunct tcltype
    set result {}
    
    if {[my define get initfunc] ne {}} {
      ::practcl::cputs result "int [my define get initfunc](Tcl_Interp *interp);"
    }
    if {[info exists cfunct]} {
      foreach {funcname info} $cfunct {
        if {![dict get $info public]} continue
        ::practcl::cputs result "[dict get $info header]\;"
      }
    } 
    set result [::practcl::_tagblock $result c [my define get filename]]
    foreach mod [my link list subordinate] {
      ::practcl::cputs result [$mod generate-public-function]
    }
    return $result
  }
  
  method generate-public-includes {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]    
    set includes {}
    foreach item [my define get public-include] {
      if {$item ni $includes} {
        lappend includes $item
      }
    }
    foreach mod [my link list subordinate] {
      foreach item [$mod generate-public-includes] {
        if {$item ni $includes} {
          lappend includes $item
        }
      }
    }
    return $includes
  }
  method generate-public-verbatim {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    set includes {}
    foreach item [my define get public-verbatim] {
      if {$item ni $includes} {
        lappend includes $item
      }
    }
    foreach mod [my link list subordinate] {
      foreach item [$mod generate-public-verbatim] {
        if {$item ni $includes} {
          lappend includes $item
        }
      }
    }
    return $includes
  }
  ###
  # This methods generates the contents of an amalgamated .h file
  # which describes the public API of this module
  ###
  method generate-h {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    set result {}
    set includes [my generate-public-includes]
    foreach inc $includes {
      if {[string index $inc 0] ni {< \"}} {
        ::practcl::cputs result "#include \"$inc\""
      } else {
        ::practcl::cputs result "#include $inc"        
      }
    }
    foreach file [my generate-public-verbatim] {
      ::practcl::cputs result "/* BEGIN $file */"
      ::practcl::cputs result [::practcl::cat $file]
      ::practcl::cputs result "/* END $file */"
    }
    foreach method {
      generate-public-define
      generate-public-macro
      generate-public-typedef
      generate-public-structure
      generate-public-headers
      generate-public-function
    } {
      ::practcl::cputs result "/* BEGIN SECTION $method */"
      ::practcl::cputs result [my $method]
      ::practcl::cputs result "/* END SECTION $method */"
    }
    return $result
  }
  
  ###
  # This methods generates the contents of an amalgamated .c file
  # which implements the loader for a batch of tools
  ###
  method generate-c {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    set result {
/* This file was generated by practcl */
    }
    set includes {}
    lappend headers <tcl.h> <tclOO.h>
    if {[my define get tk 0]} {
      lappend headers <tk.h>
    }
    lappend headers {*}[my define get include]
    
    foreach mod [my link list product] {
      # Signal modules to formulate final implementation
      $mod go
    }
    foreach mod [my link list dynamic] {
      foreach inc [$mod define get include] {
        if {$inc ni $headers} {
          lappend headers $inc
        }
      }
    }
    foreach inc $headers {
      if {[string index $inc 0] ni {< \"}} {
        ::practcl::cputs result "#include \"$inc\""
      } else {
        ::practcl::cputs result "#include $inc"        
      }
    }
    foreach {method children} {
      generate-cheader      subordinate
      generate-cstruct      dynamic
      generate-constant     dynamic
      generate-cfunct       dynamic
      generate-cmethod      dynamic
    } {
      ::practcl::cputs result "/* BEGIN $method [my define get filename] */"
      ::practcl::cputs result [my $method]
      ::practcl::cputs result "/* END $method [my define get filename] */"
    }
    debug [list /[self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    return $result
  }

  ###
  # This methods generates any Tcl script file
  # which is required to pre-initialize the C library
  ###
  method generate-tcl {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    set result {}
    my variable code
    if {[info exists code(tcl)]} {
      ::practcl::cputs result $code(tcl)
    }
    set result [::practcl::_tagblock $result tcl [my define get filename]]
    foreach mod [my link list subordinate] {
      ::practcl::cputs result [$mod generate-tcl]
    }
    if {[my define get shared_library] ne {}} {
      set LIBFILE [my define get shared_library]
      set PKGINIT [my define get pkginit]
      ::practcl::cputs result [string map \
        [list @LIBFILE@ $LIBFILE @PKGINIT@ $PKGINIT] \
        {load [file join [file dirname [file join [pwd] [info script]]] @LIBFILE@] @PKGINIT@}]
    }
    return $result
  }
}

::oo::class create ::practcl::product {
  superclass ::practcl::object
  
  method linktype {} {
    return {subordinate product}
  }
  
  method include header {
    my define add include $header
  }
  
  method cstructure {name definition {argdat {}}} {
    my variable cstruct
    dict set cstruct $name body $definition
    foreach {f v} $argdat {
      dict set cstruct $name $f $v
    }
  }
  
  method generate-cinit {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    my variable code
    set result {}
    if {[info exists code(cinit)]} {
      ::practcl::cputs result $code(cinit)
    }
    if {[my define get initfunc] ne {}} {
      ::practcl::cputs result "  if([my define get initfunc](interp)!=TCL_OK) return TCL_ERROR\;"
    }
    set result [::practcl::_tagblock $result c [my define get filename]]
    foreach obj [my link list subordinate] {
      ::practcl::cputs result [$obj generate-cinit]
    }
    return $result
  }
}

###
# Dynamic blocks do not generate their own .c files,
# instead the contribute to the amalgamation
# of the main library file
###
::oo::class create ::practcl::dynamic {
  superclass ::practcl::product
  
  method compile-products {} {
    set result {}
    foreach item [my link list subordinate] {
      lappend result {*}[$item compile-products]
    }
    return $result
  }
  
  method initialize {} {
    set filename [my define get filename]
    if {$filename eq {}} {
      return
    }
    if {[my define get name] eq {}} {
      my define set name [file tail [file rootname $filename]]
    }
    if {[my define get localpath] eq {}} {
      my define set localpath [my <module> define get localpath]_[my define get name]
    }
    debug [self] SOURCE $filename
    my source $filename
  }
  
  method linktype {} {
    return {subordinate dynamic}
  }
  method compile_products {} {}
  
  ###
  # Populate const static data structures
  ###
  method generate-cstruct {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    my variable code cstruct methods tcltype
    set result {}
    if {[info exists code(struct)]} {
      ::practcl::cputs result $code(struct)
    }
    foreach obj [my link list dynamic] {
      ::practcl::cputs result [$obj generate-cstruct]
    }
    return $result
  }
  
  method generate-constant {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    set result {}
    my variable code cstruct methods tcltype
    if {[info exists code(constant)]} {
      ::practcl::cputs result "/* [my define get filename] CONSTANT */"
      ::practcl::cputs result $code(constant)
    }
    if {[info exists cstruct]} {
      foreach {name info} $cstruct {
        set map {}
        lappend map @NAME@ $name
        lappend map @MACRO@ GET[string toupper $name]

        if {[dict exists $info deleteproc]} {
          lappend map @DELETEPROC@ [dict get $info deleteproc]
        } else {
          lappend map @DELETEPROC@ NULL
        }
        if {[dict exists $info cloneproc]} {
          lappend map @CLONEPROC@ [dict get $info cloneproc]
        } else {
          lappend map @CLONEPROC@ NULL
        }
        ::practcl::cputs result [string map $map {
const static Tcl_ObjectMetadataType @NAME@DataType = {
  TCL_OO_METADATA_VERSION_CURRENT,
  "@NAME@",
  @DELETEPROC@,
  @CLONEPROC@
};
#define @MACRO@(OBJCONTEXT) (@NAME@ *) Tcl_ObjectGetMetadata(Tcl_ObjectContextObject(objectContext),&@NAME@DataType)
}]
      }
    }
    if {[info exists tcltype]} {
      foreach {type info} $tcltype {
        dict with info {}
        ::practcl::cputs result "const Tcl_ObjType $cname = \{\n  .freeIntRepProc = &${freeproc},\n  .dupIntRepProc = &${dupproc},\n  .updateStringProc = &${updatestringproc},\n  .setFromAnyProc = &${setfromanyproc}\n\}\;"
      }
    }    

    if {[info exists methods]} {
      set mtypes {}
      foreach {name info} $methods {   
        set callproc   [dict get $info callproc]
        set methodtype [dict get $info methodtype]
        if {$methodtype in $mtypes} continue
        lappend mtypes $methodtype
        ###
        # Build the data struct for this method
        ###
        ::practcl::cputs result "const static Tcl_MethodType $methodtype = \{"
        ::practcl::cputs result "  .version = TCL_OO_METADATA_VERSION_CURRENT,\n  .name = \"$name\",\n  .callProc = $callproc,"
        if {[dict exists $info deleteproc]} {
          set deleteproc [dict get $info deleteproc]
        } else {
          set deleteproc NULL
        }
        if {$deleteproc ni { {} NULL }} {
          ::practcl::cputs result "  .deleteProc = $deleteproc,"
        } else {
          ::practcl::cputs result "  .deleteProc = NULL,"
        }
        if {[dict exists $info cloneproc]} {
          set cloneproc [dict get $info cloneproc]
        } else {
          set cloneproc NULL
        }
        if {$cloneproc ni { {} NULL }} {
          ::practcl::cputs result "  .cloneProc = $cloneproc\n\}\;"
        } else {
          ::practcl::cputs result "  .cloneProc = NULL\n\}\;"
        }
        dict set methods $name methodtype $methodtype
      }
    }
    foreach obj [my link list dynamic] {
      ::practcl::cputs result [$obj generate-constant]
    }
    return $result
  }
  
  ###
  # Generate code that provides subroutines called by
  # Tcl API methods
  ###
  method generate-cfunct {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    my variable code cfunct
    set result {}
    if {[info exists code(funct)]} {
      ::practcl::cputs result $code(funct)
    }
    if {[info exists cfunct]} {
      foreach {funcname info} $cfunct {
        ::practcl::cputs result "[dict get $info header]\{[dict get $info body]\}\;"
      }
    }
    foreach obj [my link list dynamic] {
      ::practcl::cputs result [$obj generate-cfunct]
    }
    return $result
  }

  ###
  # Generate code that provides implements Tcl API
  # calls
  ###
  method generate-cmethod {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    my variable code methods tclprocs
    set result {}
    if {[info exists code(method)]} {
      ::practcl::cputs result $code(method)
    }
    
    if {[info exists tclprocs]} {
      foreach {name info} $tclprocs {
        if {![dict exists $info body]} continue
        set callproc [dict get $info callproc]
        set header [dict get $info header]
        set body [dict get $info body]
        ::practcl::cputs result "${header} \{${body}\}"
      }
    }

    
    if {[info exists methods]} {
      set thisclass [my define get cclass]
      foreach {name info} $methods {
        if {![dict exists $info body]} continue
        set callproc [dict get $info callproc]
        set header [dict get $info header]
        set body [dict get $info body]
        ::practcl::cputs result "${header} \{${body}\}"
      }
      # Build the OO_Init function
      ::practcl::cputs result "static int ${thisclass}_OO_Init(Tcl_Interp *interp) \{"
      ::practcl::cputs result [string map [list @CCLASS@ $thisclass @TCLCLASS@ [my define get class]] {
  /*
  ** Build the "@TCLCLASS@" class
  */
  Tcl_Obj* nameObj;		/* Name of a class or method being looked up */
  Tcl_Object curClassObject;  /* Tcl_Object representing the current class */
  Tcl_Class curClass;		/* Tcl_Class representing the current class */

  /* 
   * Find the wallset class, and attach an 'init' method to it.
   */

  nameObj = Tcl_NewStringObj("@TCLCLASS@", -1);
  Tcl_IncrRefCount(nameObj);
  if ((curClassObject = Tcl_GetObjectFromObj(interp, nameObj)) == NULL) {
      Tcl_DecrRefCount(nameObj);
      return TCL_ERROR;
  }
  Tcl_DecrRefCount(nameObj);
  curClass = Tcl_GetObjectAsClass(curClassObject);
}]
      if {[dict exists $methods constructor]} {
        set mtype [dict get $methods constructor methodtype]
        ::practcl::cputs result [string map [list @MTYPE@ $mtype] {
  /* Attach the constructor to the class */
  Tcl_ClassSetConstructor(interp, curClass, Tcl_NewMethod(interp, curClass, NULL, 1, &@MTYPE@, NULL));
    }]
      }
      foreach {name info} $methods {
        dict with info {}
        if {$name in {constructor destructor}} continue
        ::practcl::cputs result [string map [list @NAME@ $name @MTYPE@ $methodtype] {
  nameObj=Tcl_NewStringObj("@NAME@",-1);
  Tcl_NewMethod(interp, curClass, nameObj, 1, &@MTYPE@, (ClientData) NULL);
  Tcl_DecrRefCount(nameObj);
}]
        if {[dict exists $info aliases]} {
          foreach alias [dict get $info aliases] {
            if {[dict exists $methods $alias]} continue
            ::practcl::cputs result [string map [list @NAME@ $alias @MTYPE@ $methodtype] {
  nameObj=Tcl_NewStringObj("@NAME@",-1);
  Tcl_NewMethod(interp, curClass, nameObj, 1, &@MTYPE@, (ClientData) NULL);
  Tcl_DecrRefCount(nameObj);
}]
          }
        }
      }
      ::practcl::cputs result "  return TCL_OK\;\n\}\n"  
    }
    foreach obj [my link list dynamic] {
      ::practcl::cputs result [$obj generate-cmethod]
    }
    return $result
  }
  
  ###
  # Generate code that runs when the package/module is
  # initialized into the interpreter
  ###
  method generate-cinit {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    set result {}
    my variable code methods tclprocs
    if {[info exists code(nspace)]} {
      ::practcl::cputs result "  \{\n    Tcl_Namespace *modPtr;"
      foreach nspace $code(nspace) {
        ::practcl::cputs result [string map [list @NSPACE@ $nspace] {
    modPtr=Tcl_FindNamespace(interp,"@NSPACE@",NULL,TCL_NAMESPACE_ONLY);
    if(!modPtr) {
      modPtr = Tcl_CreateNamespace(interp, "@NSPACE@", NULL, NULL);
    }
}]
      }
      ::practcl::cputs result "  \}"      
    }
    if {[info exists code(tclinit)]} {
      ::practcl::cputs result $code(tclinit)
    }
    if {[info exists code(cinit)]} {
      ::practcl::cputs result $code(cinit)
    }
    if {[info exists code(initfuncts)]} {
      foreach func $code(initfuncts) {
        ::practcl::cputs result "  if (${func}(interp) != TCL_OK) return TCL_ERROR\;"
      }
    }
    if {[info exists tclprocs]} {
      foreach {name info} $tclprocs {
        set map [list @NAME@ $name @CALLPROC@ [dict get $info callproc]]
        ::practcl::cputs result [string map $map {  Tcl_CreateObjCommand(interp,"@NAME@",(Tcl_ObjCmdProc *)@CALLPROC@,NULL,NULL);}]
        if {[dict exists $info aliases]} {
          foreach alias [dict get $info aliases] {
            set map [list @NAME@ $alias @CALLPROC@ [dict get $info callproc]]
            ::practcl::cputs result [string map $map {  Tcl_CreateObjCommand(interp,"@NAME@",(Tcl_ObjCmdProc *)@CALLPROC@,NULL,NULL);}]
          }
        }
      }
    }
    
    if {[info exists code(nspace)]} {
      ::practcl::cputs result "  \{\n    Tcl_Namespace *modPtr;"
      foreach nspace $code(nspace) {
        ::practcl::cputs result [string map [list @NSPACE@ $nspace] {
    modPtr=Tcl_FindNamespace(interp,"@NSPACE@",NULL,TCL_NAMESPACE_ONLY);
    Tcl_CreateEnsemble(interp, modPtr->fullName, modPtr, TCL_ENSEMBLE_PREFIX);
    Tcl_Export(interp, modPtr, "[a-z]*", 1);
}]
      }
      ::practcl::cputs result "  \}"
    }
    set result [::practcl::_tagblock $result c [my define get filename]]
    foreach obj [my link list subordinate] {
      ::practcl::cputs result [$obj generate-cinit]
    }
    return $result
  }

  method tcltype {name argdat} {
    my variable tcltype
    foreach {f v} $argdat {
      dict set tcltype $name $f $v
    }
    if {![dict exists tcltype $name cname]} {
      dict set tcltype $name cname [string tolower $name]_tclobjtype
    }
    lappend map @NAME@ $name
    set info [dict get $tcltype $name]
    foreach {f v} $info {
      lappend map @[string toupper $f]@ $v
    }
    foreach {func fpat template} {
      freeproc         {@Name@Obj_freeIntRepProc}       {void @FNAME@(Tcl_Obj *objPtr)}
      dupproc          {@Name@Obj_dupIntRepProc}        {void @FNAME@(Tcl_Obj *srcPtr,Tcl_Obj *dupPtr)}
      updatestringproc {@Name@Obj_updateStringRepProc} {void @FNAME@(Tcl_Obj *objPtr)}
      setfromanyproc   {@Name@Obj_setFromAnyProc}       {int @FNAME@(Tcl_Interp *interp,Tcl_Obj *objPtr)}
    } {
      if {![dict exists $info $func]} {
        error "$name does not define $func"
      }
      set body [dict get $info $func]
      # We were given a function name to call
      if {[llength $body] eq 1} continue
      set fname [string map [list @Name@ [string totitle $name]] $fpat]
      my c_function [string map [list @FNAME@ $fname] $template] [string map $map $body]
      dict set tcltype $name $func $fname
    }
  }
  method c_header body {
    my variable code
    ::practcl::cputs code(header) $body
  }
  method c_code body {
    my variable code
    ::practcl::cputs code(funct) $body
  }
  method c_function {header body} {
    my variable code cfunct
    foreach regexp {
         {(.*) ([a-zA-Z_][a-zA-Z0-9_]*) *\((.*)\)}
         {(.*) (\x2a[a-zA-Z_][a-zA-Z0-9_]*) *\((.*)\)}
    } {
      if {[regexp $regexp $header all keywords funcname arglist]} {
        dict set cfunct $funcname header $header
        dict set cfunct $funcname body $body
        dict set cfunct $funcname keywords $keywords
        dict set cfunct $funcname arglist $arglist
        dict set cfunct $funcname public [expr {"static" ni $keywords}]
        dict set cfunct $funcname export [expr {"STUB_EXPORT" in $keywords}]

        return
      }
    }
    ::practcl::cputs code(header) "$header\;"
    # Could not parse that block as a function
    # append it verbatim to our c_implementation
    ::practcl::cputs code(funct) "$header [list $body]"
  }

  
  method cmethod {name body {arginfo {}}} {
    my variable methods code
    foreach {f v} $arginfo {
      dict set methods $name $f $v
    }
    dict set methods $name body $body
  }
  
  method c_tclproc_nspace nspace {
    my variable code
    if {![info exists code(nspace)]} {
      set code(nspace) {}
    }
    if {$nspace ni $code(nspace)} {
      lappend code(nspace) $nspace
    }
  }
  
  method c_tclproc_raw {name body {arginfo {}}} {
    my variable tclprocs code

    foreach {f v} $arginfo {
      dict set tclprocs $name $f $v
    }
    dict set tclprocs $name body $body
  }

  method go {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    next
    my variable methods code cstruct tclprocs
    if {[info exists methods]} {
      debug [self] methods [my define get cclass]
      set thisclass [my define get cclass]
      foreach {name info} $methods {   
        # Provide a callproc
        if {![dict exists $info callproc]} {
          set callproc [string map {____ _ ___ _ __ _} [string map {{ } _ : _} OOMethod_${thisclass}_${name}]]
          dict set methods $name callproc $callproc
        } else {
          set callproc [dict get $info callproc]
        }
        if {[dict exists $info body] && ![dict exists $info header]} {
          dict set methods $name header "static int ${callproc}(ClientData clientData, Tcl_Interp *interp, Tcl_ObjectContext objectContext ,int objc ,Tcl_Obj *const *objv)"
        }
        if {![dict exists $info methodtype]} {
          set methodtype [string map {{ } _ : _} MethodType_${thisclass}_${name}]
          dict set methods $name methodtype $methodtype
        }
      }
      if {![info exists code(initfuncts)] || "${thisclass}_OO_Init" ni $code(initfuncts)} {
        lappend code(initfuncts) "${thisclass}_OO_Init"
      }
    }
    set thisnspace [my define get nspace]

    if {[info exists tclprocs]} {
      debug [self] tclprocs [dict keys $tclprocs]
      foreach {name info} $tclprocs {
        if {![dict exists $info callproc]} {
          set callproc [string map {____ _ ___ _ __ _} [string map {{ } _ : _} Tclcmd_${thisnspace}_${name}]]
          dict set tclprocs $name callproc $callproc
        } else {
          set callproc [dict get $info callproc]
        }    
        if {[dict exists $info body] && ![dict exists $info header]} {
          dict set tclprocs $name header "static int ${callproc}(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv\[\])"
        }
      }
    }
    debug [list /[self] [self method] [self class]]
  }
}

::oo::class create ::practcl::cheader {
  superclass ::practcl::product

  method compile-products {} {}
  method generate-cinit {} {}
}

::oo::class create ::practcl::csource {
  superclass ::practcl::product
}



###
# In the end, all C code must be loaded into a module
# This will either be a dynamically loaded library implementing
# a tcl extension, or a compiled in segment of a custom shell/app
###
::oo::class create ::practcl::module {
  superclass ::practcl::dynamic

  method linktype {} {
    return {subordinate dynamic module}
  }
  
  
  method initialize {} {
    set filename [my define get filename]
    if {$filename eq {}} {
      return
    }
    if {[my define get name] eq {}} {
      my define set name [file tail [file dirname $filename]]
    }
    if {[my define get localpath] eq {}} {
      my define set localpath [my <project> define get name]_[my define get name]
    }
    debug [self] SOURCE $filename
    my source $filename
  }
  
  method implement path {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    set filename [my define get output_c]
    if {$filename eq {}} {
      debug [list /[self] [self method] [self class]]
      return
    }
    
    set cout [open [file join $path [file rootname $filename].c] w]
    puts $cout [subst {/*
** This file is generated by the [info script] script
** any changes will be overwritten the next time it is run
*/}]
    puts $cout [my generate-c]
    close $cout
    debug [list /[self] [self method] [self class]]
  }
  
  method child which {
    switch $which {
      organs {
        return [list project [my define get project] module [self]]
      }
    }
  }
}


::oo::class create ::practcl::library {
  superclass ::practcl::module
  
  constructor args {
    my variable define
    if {[llength $args]==1} {
      array set define [lindex $args 0]
    } else {
      array set define $args
    }
    my select
    my initialize
  }
  
  method linktype {} {
    return library
  }
  # Create a "package ifneeded"
  # Args are a list of aliases for which this package will answer to
  method package-ifneeded {args} {
    set result {}
    set name [my define get pkg_name [my define get name]]
    set version [my define get pkg_vers [my define get version]]
    if {$version eq {}} {
      set version 0.1a
    }
    set output_tcl [my define get output_tcl]
    if {$output_tcl ne {}} {
      set script "\[list source \[file join \$dir $output_tcl\]\]"
    } else {
      set script "\[list load \[file join \$dir [my define get shared_library]\] [my define get pkginit]\]"
    }
    set result "package ifneeded [list $name] [list $version] $script"
    foreach alias $args {
      set script "package require $name $version \; package provide $alias $version"
      append result \n\n [list package ifneeded $alias $version $script]
    }
    return $result
  }
  
  method select {} {}
  
  method child which {
    switch $which {
      organs {
        return [list project [self]]
      }
    }
  }
  
  method go {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    set name [my define getnull name]
    if {$name eq {}} {
      set name generic
      my define name generic
    }
    set output_c [my define getnull output_c]
    if {$output_c eq {}} {
      set output_c [file rootname $name].c
      my define set output_c $output_c
    }
    set output_h [my define getnull output_h]
    if {$output_h eq {}} {
      set output_h [file rootname $output_c].h
      my define set output_h $output_h
    }
    set output_tcl [my define getnull output_tcl]
    if {$output_tcl eq {}} {
      set output_tcl [file rootname $output_c].tcl
      my define set output_tcl $output_tcl
    }
    set output_mk [my define getnull output_mk]
    if {$output_mk eq {}} {
      set output_mk [file rootname $output_mk].mk
      my define set output_mk $output_mk
    }
    set output_decls [my define getnull output_decls]
    if {$output_decls eq {}} {
      set output_decls [file rootname $output_c].decls
      my define set output_decls $output_decls
    }
    my variable links
    foreach {linktype objs} [array get links] {
      foreach obj $objs {
        $obj go
      }
    }
    debug [list /[self] [self method] [self class] -- [my define get filename] [info object class [self]]]
  }
  
  method implement path {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    my go
    foreach item [my link list subordinate] {
      $item implement $path
    }
    set cout [open [file join $path [my define get output_c]] w]
    puts $cout [subst {/*
** This file is generated by the [info script] script
** any changes will be overwritten the next time it is run
*/}]
    puts $cout [my generate-c]
    puts $cout [my generate-loader]
    close $cout
    
    set macro HAVE_[string toupper [file rootname [my define get output_h]]]_H
    set hout [open [file join $path [my define get output_h]] w]
    puts $hout [subst {/*
** This file is generated by the [info script] script
** any changes will be overwritten the next time it is run
*/}]
    puts $hout "#ifndef ${macro}"
    puts $hout "#define ${macro}"
    puts $hout [my generate-h]
    puts $hout "#endif"
    close $hout
    
    set tclout [open [file join $path [my define get output_tcl]] w]
    puts $tclout "###
# This file is generated by the [info script] script
# any changes will be overwritten the next time it is run
###"
    puts $tclout [my generate-tcl]
    close $tclout
    
    
    
    set mkout [open [file join $path [my define get output_mk]] w]
    puts $mkout "###
# This file is generated by the [info script] script
# any changes will be overwritten the next time it is run
###"
    puts $mkout [my generate-make $path]
    close $mkout
    my generate-decls [my define get name] $path
    debug [list /[self] [self method] [self class]]
  }
  
  method generate-make {path} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    set result {}
    set products {}
    set name [string tolower [my define get name [my define get pkg_name]]]
    set NAME [string toupper $name]
    set version [my define get version [my define get pkg_vers]]
    if {$version eq {}} {
      set version 0.1a
    }
    set includedir .
    foreach include [my generate-include-directory] {
      set cpath [::practcl::file_relative $path [file normalize $include]]
      if {$cpath ni $includedir} {
        lappend includedir $cpath
      }
    }
    set thisline {}
    set defs {}
    foreach item $::project(DEFS) {
      if {[string length $thisline] > 40} {
        #append defs " \\\n    "
        set thisline {}
      }
      if {[string range $item 0 9] eq "-DPACKAGE_"} continue
      set eqidx [string first = $item ]
      if {$eqidx < 0} {
        append thisline { } $item
        append defs { } $item      
        continue
      }
      set field [string range $item 0 [expr {$eqidx-1}]]
      set value [string range $item [expr {$eqidx+1}] end]
      if {[string is integer -strict $value]} {
        append defs { } "${field}=$value"
        append thisline { } "${field}=$value"
      } else {
        append defs { } "${field}=\"${value}\""
        append thisline { } "${field}=\"${value}\""
      }
    }
    
    append defs " -DPACKAGE_NAME=\"${name}\" -DPACKAGE_VERSION=\"${version}\""
    append defs " -DPACKAGE_TARNAME=\"${name}\" -DPACKAGE_STRING=\"${name} ${version}\""   
    ::practcl::cputs result "${NAME}_DEFS = $defs\n"
    ::practcl::cputs result "${NAME}_INCLUDES = -I\"[join $includedir "\" -I\""]\"\n"
    ::practcl::cputs result "${NAME}_COMPILE		= \$(CC) \$(CFLAGS) \$(${NAME}_DEFS) \$(${NAME}_INCLUDES) \$(INCLUDES) \$(AM_CPPFLAGS) \$(CPPFLAGS) \$(AM_CFLAGS)"

    foreach {ofile info} [my compile-products] {
      puts [list $ofile $info]
      puts [list dict keys $info]
      puts [dict keys $info]

      dict set products $ofile $info
      set agline {}
      if {[dict exists $info depend]} {
        ::practcl::cputs result "\n${ofile}: [dict get $info depend]"
      } else {
        ::practcl::cputs result "\n${ofile}:"
      }
      set cmd "\t\$\(${NAME}_COMPILE\)"
      if {[dict exists $info extra]} {
        append cmd " [dict get $info extra]"
      }
      append cmd " -c [dict get $info cfile] -o \$@\n\t"
      ::practcl::cputs result  $cmd
    }

    set map {}
    lappend map %LIBRARY_NAME% $name    
    lappend map %LIBRARY_VERSION% $version
    lappend map %LIBRARY_VERSION_NODOTS% [string map {. {}} $version]
    lappend map %LIBRARY_PREFIX% [my define getnull libprefix]
    foreach flag {
      SHLIB_LD
      STLIB_LD
      SHLIB_LD_LIBS
      SHLIB_SUFFIX
      LDFLAGS_DEFAULT
    } {
      lappend map "%${flag}%" "\$\{${flag}\}"
    }
    set outfile [string map $map $::project(PRACTCL_NAME_LIBRARY)]$::project(SHLIB_SUFFIX)
    if {[my define get shared_library] eq {}} {
      my define set shared_library $outfile
    }
    ::practcl::cputs result "
${NAME}_SHLIB = $outfile
${NAME}_OBJS = [dict keys $products]
"
    #lappend map %OUTFILE% {\[$]@}
    lappend map %OUTFILE% $outfile
    lappend map %LIBRARY_OBJECTS% "\$(${NAME}_OBJS)"
    ::practcl::cputs result "$outfile: \$(${NAME}_OBJS)" 
    ::practcl::cputs result "\t[string map $map $::project(PRACTCL_SHARED_LIB)]"
    if {$::project(PRACTCL_VC_MANIFEST_EMBED_DLL) ni {: {}}} {
      ::practcl::cputs result "\t[string map $map $::project(PRACTCL_VC_MANIFEST_EMBED_DLL)]"
    }
    ::practcl::cputs result {}
    set outfile $name.a
    dict set map %OUTFILE% $outfile
    ::practcl::cputs result "$outfile: \$(${NAME}_OBJS)"
    ::practcl::cputs result "\t-rm -rf $outfile"
    ::practcl::cputs result "\t[string map $map $::project(PRACTCL_STATIC_LIB)]"
    return $result
  }
  
  method shared_library {} {
    set name [string tolower [my define get name [my define get pkg_name]]]
    set NAME [string toupper $name]
    set version [my define get version [my define get pkg_vers]]
    set map {}
    lappend map %LIBRARY_NAME% $name    
    lappend map %LIBRARY_VERSION% $version
    lappend map %LIBRARY_VERSION_NODOTS% [string map {. {}} $version]
    lappend map %LIBRARY_PREFIX% [my define getnull libprefix]
    foreach flag {
      SHLIB_LD
      STLIB_LD
      SHLIB_LD_LIBS
      SHLIB_SUFFIX
      LDFLAGS_DEFAULT
    } {
      lappend map "%${flag}%" "\$\{${flag}\}"
    }
    set outfile [string map $map $::project(PRACTCL_NAME_LIBRARY)]$::project(SHLIB_SUFFIX)
    return $outfile
  }
  
  method generate-loader {} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    set result {}
    ::practcl::cputs result  "
extern int DLLEXPORT [my define get init_funct]( Tcl_Interp *interp ) \{"
    ::practcl::cputs result  {
  /* Initialise the stubs tables. */
  #ifdef USE_TCL_STUBS
    if (Tcl_InitStubs(interp, "8.6", 0)==NULL) return TCL_ERROR;
    if (TclOOInitializeStubs(interp, "1.0") == NULL) return TCL_ERROR;
}
    if {[my define get tk 0]} {
      ::practcl::cputs result  {    if (Tk_InitStubs(interp, "8.6", 0)==NULL) return TCL_ERROR;}
    }
    ::practcl::cputs result {  #endif}
    foreach item [my link list subordinate] {
      ::practcl::cputs result [$item generate-cinit]
    }
    if {[my define exists pkg_name]} {
      ::practcl::cputs result  "    if (Tcl_PkgProvide(interp, \"[my define get pkg_name]\" , \"[my define get pkg_vers]\" )) return TCL_ERROR\;"
    }
    ::practcl::cputs result  "  return TCL_OK\;\n\}\n"
    return $result
  }
  
  method generate-decls {pkgname path} {
    debug [list [self] [self method] [self class] -- [my define get filename] [info object class [self]]]
    set outfile [file join $path/$pkgname.decls]
  
  ###
  # Build the decls file
  ###
  set fout [open $outfile w]
  puts $fout [subst {###
  # $outfile
  #
  # This file was generated by [info script]
  ###
  
  library $pkgname
  interface $pkgname
  }]
  
  ###
  # Generate list of functions
  ###
  set stubfuncts [my generate-stub-function]
  set thisline {}
  set functcount 0
  foreach {func header} $stubfuncts {
    puts $fout [list declare [incr functcount] $header]
  }
  puts $fout [list export "int [my define get init_funct](Tcl_Inter *interp)"]
  puts $fout [list export "char *[string totitle [my define get name]]_InitStubs(Tcl_Inter *interp, char *version, int exact)"]

  close $fout
  
  ###
  # Build [package]Decls.h
  ###
  set hout [open [file join $path ${pkgname}Decls.h] w]
  
  close $hout

  set cout [open [file join $path ${pkgname}StubInit.c] w]
puts $cout [string map [list %pkgname% $pkgname %PkgName% [string totitle $pkgname]] {
#ifndef USE_TCL_STUBS
#define USE_TCL_STUBS
#endif
#undef USE_TCL_STUB_PROCS

#include "tcl.h"
#include "%pkgname%.h"

 /*
 ** Ensure that Tdom_InitStubs is built as an exported symbol.  The other stub
 ** functions should be built as non-exported symbols.
 */

#undef TCL_STORAGE_CLASS
#define TCL_STORAGE_CLASS DLLEXPORT

%PkgName%Stubs *%pkgname%StubsPtr;

 /*
 **----------------------------------------------------------------------
 **
 **  %PkgName%_InitStubs --
 **
 **        Checks that the correct version of %PkgName% is loaded and that it
 **        supports stubs. It then initialises the stub table pointers.
 **
 **  Results:
 **        The actual version of %PkgName% that satisfies the request, or
 **        NULL to indicate that an error occurred.
 **
 **  Side effects:
 **        Sets the stub table pointers.
 **
 **----------------------------------------------------------------------
 */

char *
%PkgName%_InitStubs (Tcl_Interp *interp, char *version, int exact)
{
  char *actualVersion;
  actualVersion = Tcl_PkgRequireEx(interp, "%pkgname%", version, exact,(ClientData *) &%pkgname%StubsPtr);
  if (!actualVersion) {
    return NULL;
  }
  if (!%pkgname%StubsPtr) {
    Tcl_SetResult(interp,"This implementation of %PkgName% does not support stubs",TCL_STATIC);
    return NULL;
  }
  return actualVersion;
}
}]
  close $cout
  }
  
  method compile-products {} {
    set result {}
    foreach item [my link list subordinate] {
      lappend result {*}[$item compile-products]
    }
    set filename [my define get output_c]
    if {$filename ne {}} {
      set ofile build/[file rootname [file tail $filename]]_main.o
      lappend result $ofile [list cfile $filename extra [my define get extra]]
    }
    return $result
  }
}



::oo::class create ::practcl::exe {
  superclass ::practcl::library
}

::oo::class create ::practcl::tclkit {
  superclass ::practcl::exe
}


oo::class create ::practcl::subproject {
  superclass ::practcl::object
    
  method linktype {} {
    return subproject
  }
  
  method implement {PWD} {
    debug [list [self] [my define get filename] [info object class [self]] IMPLEMENT]
    set name [my define get name]
    set product [my define get product]
    if {[file exists [file join $PWD $name $product]]} return
    set distribution [my define get distribution fossil]
    my unpack-${distribution} [file join $::project(sandbox) $name]
    file mkdir [file join $PWD $name]
    set CWD [pwd]
    cd [file join $PWD $name]
    doexec sh [file join $::project(sandbox) $name {*}[my define get localsrcroot] configure] --enable-shared=0 {*}[my define get options]
    catch {doexec make $product}
    cd $CWD
    debug [list /[self] [my define get filename] [info object class [self]] IMPLEMENT]
  }
  
  method select {} {}
  
  method FindFossilDb {} {
    set name [my define get name]
    foreach item [exec fossil all list] {
      set dbname [file rootname [file tail $item]]
      if {$dbname eq $name} {
        return $item
      }
    }
    if {[file exists [file join $::project(download) $name.fos]]} {
      return [file join $::project(download) $name.fos]
    }
    if {[file exists [file join $::project(download) $name.fossil]]} {
      return [file join $::project(download) $name.fossil]
    }
    set fosdb [file join $::project(download) fossil $name.fos]
    if {![file exists $fosdb]} {
      doexec fossil clone $::env(LOCAL_FOSSIL_MIRROR)/$name $fosdb
    }
    return $fosdb
  }
  
  method unpack-fossil path {
    if {[file exists [file join $path .fslckout]]} {
      #::fossil $path update [my define get tag trunk]
      return
    }
    if {[file exists [file join $path _FOSSIL_]]} {
      #::fossil $path update [my define get tag trunk]
      return
    }
    set fosdb [my FindFossilDb]
    file mkdir $path
    set PWD [pwd]
    ::fossil $path open $fosdb [my define get tag trunk]
  }

  method practcl-integrate {} {
    set distribution [my define get distribution fossil]
    my unpack-${distribution} [file join $::project(sandbox) [my define get name]]
    source [file join $::project(sandbox) [my define get name] library.ini]
  }
}

oo::class create ::practcl::subproject.core {
  superclass ::practcl::subproject
  
  method initialize {} {
    set name [my define get name]
    my define add include_dir [file join $::project(sandbox) $name generic]
    switch $::project(TEA_PLATFORM) {
      windows {
        my define set localsrcroot win
        my define add include_dir [file join $::project(sandbox) $name win]
      }
      default {
        my define set localsrcroot unix
        my define add include_dir [file join $::project(sandbox) $name unix]
      }
    }
  }
}

package provide practcl 0.2