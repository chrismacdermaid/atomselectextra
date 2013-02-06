# Stop atomselect from sucking
#Copyright (c) 2013 Chris MacDermaid <chris.macdermaid@gmail.com>

namespace eval ::AtomselectExtra:: {

    variable version 1.0

    variable as_lookup
    variable as_id
    variable atom_props
    variable atom_props_list

    array set as_lookup {}
    array set atom_props {}
    set atom_props_list {}
    set as_id 0

    namespace export AtomselectExtra
}

proc ::AtomselectExtra::usage {} {

}

proc ::AtomselectExtra::atomselect_extra { args } {

}

proc ::AtomselectExtra::make_proc { molid selection_text } {

    global vmd_molecule

    variable as_id
    variable as_lookup

    set procname ::atomselect_extra$as_id

    ## Create an atomselection and globalize it
    ## Link into the created proc
    set as_lookup($procname) [atomselect $molid $selection_text]
    $as_lookup($procname) global

    ## IDs associated with the selection
    set ids [$as_lookup($procname) list]
    set molid [$as_lookup($procname) molid]

    set cmd "proc $procname"
    set cmd [concat $cmd "{ args } {\n"]

    ## Proc name
    set cmd [concat $cmd "set myname \[lindex \[info level 0\] 0\];\n"]

    ## Get Associated Atomselection
    set cmd [concat $cmd "upvar 0 ::AtomselectExtra::as_lookup(\$myname) sel;\n"]

    ## The mol ID associated with the selection
    set cmd [concat $cmd "set molid $molid;\n"]

    ## Store a list of atoms specific to this proc for cross-reference
    set cmd [concat $cmd "set ids \[list $ids\];\n"]

    ## Parse the command, pass to real atomselection if standard args
    set cmd [concat $cmd "switch \[lindex \$args 0\] \{\n"]

    ## Get Values
    set cmd [concat $cmd "get \{return \[::AtomselectExtra::__get \$molid \$sel ids \[lindex \$args 1\]\]\}\n"]

    ## Set Values
    set cmd [concat $cmd "set \{::AtomselectExtra::__set \$molid \$sel ids \[lindex \$args 1\] \[lindex \$args 2\]\}\n"]

    ## Return the associated selection
    ## so things like [$sel sel] get {x y z} can work if needed
    set cmd [concat $cmd "sel \{return \$sel\}\n"]

    ## Copy the atributes from one to the other within the specified molecule
    ## The traditional $sel set {x y z} [$sel get {x y z}] for between molecules
    ## still works as intended
    set cmd [concat $cmd "copy \{::AtomselectExtra::__set \$molid \$sel ids \[lindex \$args 2 \] \[::AtomselectExtra::get \$molid \$sel ids \[lindex \$args 1\]\]\}\n"]

    ## Swap attributes {x y z} {z y x} == x = z, y = y, z = x
    set cmd [concat $cmd "swap \{::AtomselectExtra::__swap \$myname \[lindex \$args 1\] \[lindex \$args 2\]\}\n"]

    ## Print out the entire proc
    set cmd [concat $cmd "showme \{return \[::AtomselectExtra::info:wholeproc \$myname\]\}\n"]

    ## Return the procs name
    set cmd [concat $cmd "myname \{return \$myname\}\n"]

    ## Delete Selection and cleanup
    set cmd [concat $cmd "delete \{::AtomselectExtra::__delete \$myname \$molid\}\n"]

    ## Pass through to linked atomselection for compatibility
    set cmd [concat $cmd "default \{return \[\$sel \{\*\}\$args\]\}\n"]

    ## Terminate the proc
    set cmd [concat $cmd "\};\n"]

    ## Close the proc body
    set cmd [concat $cmd "\n}"]

    #create the proc
    if {[catch {eval $cmd} err]} {
        vmdcon -err "Unable to create atomselection: $err"
        return -1
    }

    ## Put traces on the proc to make sure it gets deleted
    ## when written over or unset or when the mol is deleted
    trace var ::atomselect_extra$as_id wu [list ::atomselect_extra$as_id delete]
    trace var ::vmd_molecule($molid) wu [list ::atomselect_extra$as_id delete]

    ## Return name of proc
    return "::atomselect_extra$as_id"
}

## Kill the associated atomselection,
## array lookup, trace and finally proc
proc ::AtomselectExtra::__delete {myname molid} {

    global vmd_molecule
    variable as_lookup

    ## Kill the associated atomselection
    #upvar 0 as_lookup($myname) sel
    #uplevel #0 $sel delete
    uplevel #0 [list $as_lookup($myname) delete]

    ## Clear the lookup array of the selection
    array unset as_lookup $myname

    ## Delete the traces on the proc and the mol
    trace vdelete $myname wu [list $myname delete]
    trace vdelete ::vmd_molecule($molid) wu [list $myname delete]

    ## Kill the proc
    rename $myname ""
}

## Add a field to associated atom properties
## The fields are mol and property dependent
## and are stored in atom_props([list molid property])
proc ::AtomselectExtra::addproperty {field_name {molid all}} {

    variable atom_props 
    variable atom_props_list

    ## If user doesn't specify, create for all loaded mols
    if {$molid == "all"} {set molid [molinfo list]}

    ## Add the properties to the global list
    foreach name $field_name {
        if {$name ni $atom_props_list} {
            lappend atom_props_list $name
        }
    }

    ## Create per-atom props list if it doesn't
    ## exist for each loaded mol
    foreach mol $molid {
        foreach name $field_name {
            # Array Key Name
            set key [list $mol $name]

            ## Check if this mol already has this property
            if {[array get atom_props $key] == ""} {

                ## Get the number of atoms per mol
                set natoms [molinfo $mol get numatoms]

                ## Create a property list, initialize it to all zeros
                set atom_props($key) [lrepeat $natoms 0]
            }
        }
    }
}

proc ::AtomselectExtra::delproperty {field_name {molid all}} {

    variable atom_props 
    variable atom_props_list

    ## If user doesn't specify, destroy propertes
    ## for all loaded mols
    if {$molid == "all"} {set molid [molinfo list]}

    foreach mol $molid {
        foreach name $field_name {
            # Array Key Name
            set key [list $mol $name]

            ## Unset the property
            if {[llength [array get atom_props $key]] > 0} {
                array unset atom_props $key
            }
        }
    }

    ## Check to see if this property is no longer
    ## represented with any molecule
    foreach name $field_name {
        if {[array get atom_props "*$name*"] == ""} {

            ## Find it's positions in the array
            set idx [lsearch -ascii -exact $atom_props_list $name]

            ## Delete it from the list
            if {$idx >= 0} {
                set atom_props_list [lreplace $atom_props_list $idx $idx]
            }
        }
    }
}

proc ::AtomselectExtra::__set {molid sel index keys values} {

    ## Check to see if any keys are in the
    ## atom_props_list

    variable atom_props 
    variable atom_props_list

    ## Pass ids by reference since they can be large
    upvar $index ids

    set custom_keys {}
    set legacy_keys {}
    foreach k $keys {
        if {$k in $atom_props_list} {
            lappend custom_keys $k
        } else {
            lappend legacy_keys $k
        }
    }

    ## If we don't have any special keys, just do
    ## the normal atomselection set and return
    ## If the user passes uninitialize custom keys, this will
    ## catch the error
    if {$custom_keys == ""} {return [$sel set $keys $values]}

    ## The user specified special keys in their settings
    ## check for different senarios and procede

    ## Make sure the mol in question has these properties defined
    ## This is a redundant, consistency check and can probably be removed
    foreach k $custom_keys {
        if {[array get atom_props [list $molid $k]] == ""} {
            vmdcon -err "Unknown property $k defined for mol $molid"
            return -1
        }
    }

    # Set all values to the provided value
    # $sel set beta 0.0
    if {[llength $keys] == 1 && [llength $values] == 1} {

        set prop [list $molid $keys]
        foreach x $ids {
            lset atom_props($prop) $x $values
        }

        # Set all values to those explicitly provided in a list
        # $sel set beta {0.0 0.0 0.0 0.0}
    } elseif {[llength $keys] == 1 && [llength $values] == [$sel num]} {

        set prop [list $molid $keys]
        foreach x $ids y $values {
            lset atom_props($prop) $x $y
        }

        ## Multiple properties
        ## {x y z beta} {{1 2 3 4} {5 6 7 9}}
    } elseif {[llength $keys] > 1 && [llength $values] == [$sel num]} {

        ## This can be tricky if there is a mixture of legacy atomselection
        ## fields and user created ones

        ## Extract each list into a temp array stored by atom property
        set i 0
        foreach k $keys {
            set p($k) [lsearch -all -inline -index $i -subindices $values *]
            incr i
        }

        ## Set user defined properties
        foreach k $custom_keys {
            set prop [list $molid $k]
            foreach x $ids y $p($k) {
                lset atom_props($prop) $x $y
            }
        }

        ## Faster to call set many times? This is probably faster since the lists don't have to
        ## be decomposed internally and we already have everything split up by property,
        ## should probably test this
        foreach k $legacy_keys {
            $sel set $k $p($k)
        }

    } elseif { [llength $values] != [$sel num] } {
        vmdcon -err "atomselect_extra set: [llength $values] data items doesn't match [$sel num] selected atoms"
    } else {
        vmdcom -err "This shouldn't happen: Shoot the programmer?"
    }
}

proc ::AtomselectExtra::__get {molid sel index keys} {

    ## Similar to set, first we check if the
    ## user requested any special fields

    variable atom_props 
    variable atom_props_list

    ## Pass ids by reference since they can be large
    upvar $index ids

    set custom_keys {}
    set legacy_keys {}
    foreach k $keys {
        if {$k in $atom_props_list} {
            lappend custom_keys $k
        } else {
            lappend legacy_keys $k
        }
    }

    ## If we don't have any special keys, just do
    ## the normal atomselection set and return
    ## If the user passes uninitialize custom keys, this will
    ## catch the error
    if {$custom_keys == ""} {return [$sel get $keys]}

    ## The user specified special keys in their settings
    ## check for different senarios and procede

    ## Make sure the mol in question has these properties defined
    ## This is a redundant consistency check and can probably be removed
    foreach k $custom_keys {
        if {[array get atom_props [list $molid $k]] == ""} {
            vmdcon -err "Unknown property $k defined for mol $molid"
            return -1
        }
    }

    ## If the user specified only 1 attribute
    ## then get that and return it
    if {[llength $keys] == 1} {return $atom_props([list $molid $keys])}

    ## Get all the attributes in the order
    ## that the user specified
    set p {}
    foreach k $keys {
        if {$k in $custom_keys} {
            lappend p $atom_props([list $molid $k])
        } else {
            lappend p [$sel get $k]
        }
    }

    ## Do the transpose and return
    set pt {}
    set natoms [$sel num]
    for {set i 0} {$i < $natoms} {incr i} {
        lappend pt [lsearch -all -inline -index $i -subindices $p *]
    }

    return $pt
}

## Swap fields
proc ::AtomselectExtra::__swap {myname p1 p2} {

    ## The number of atributes in p1 and p2 should be
    ## equal

    if {[llength $p1] != [llength $p2]} {
        vmdcon -err "atomselect_extra swap: Unequal number of atributes"
    }

    ## Store temp values
    set ptemp [uplevel #0 [list $myname get $p1]]

    ## Copy
    uplevel #0 [list $myname copy $p2 $p1]

    # Set
    uplevel #0 [list $myname set $p2 $ptemp]
}

proc ::AtomselectExtra::cleanup {} {

    global vmd_molecule
    variable atom_props

    ## Remove properties from molecules
    ## that no-longer exist
    foreach {key value} [array get vmd_molecule *] {
        if {$value == 0} {
            array unset atom_props "$key *"
        }
    }
}

# +------------------------------------------------+
# | Print the proc, great for checking the syntax  |
# | of the created atomselect_extra routine        |
# +------------------------------------------------+

proc ::AtomselectExtra::info:wholeproc procname {
    set result [list proc $procname]
    set args {}
    foreach arg [info args $procname] {
        if {[info default $procname $arg value]} {
            lappend args [list $arg $value]
        } else {
            lappend args $arg
        }
    }
    lappend result [list $args]
    lappend result [list [info body $procname]]
    return [join $result]
}

interp alias {} atomselect_extra {} ::AtomselectExtra::atomselect_extra
package provide AtomselectExtra $::AtomselectExtra::version
