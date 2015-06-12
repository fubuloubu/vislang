<?xml version="1.0" encoding="UTF-8"?>
<PROGRAM name="timed-blinking-light.bin">
<!-- This block denotes the contents of a program. Everything 
     contained within (including file references would be compiled
     as a single binary. -->
<INPUT scope="device" name="digital_input_1" size="1"
     type="boolean" address="0x0123|0"/>
<!-- Hardware address 123 bit 0 is DI_1 for ATMega328 -->
<!-- TODO: Find real hardware address for this DI -->
<!-- Literal constants for unsigned integers can be hex (0x20), binary
     (2x110101), octal (8x266), or decimal (268). Literal constants for
     signed intergers must have a sign prefixed (e.g. +0x26A8 or -268).
     Neither int nor uint can have a decimal point. -->
<SIGNAL scope="global" name="count_expired" size="1" type="boolean"/>
<!-- It is good practice to specify Inputs, Signals, and Constants at the
     top of a document, although not strictly necessary -->
<NOT name="not_di_1"/>
<!-- The | operator on a name denotes an available connection -->
<CONNECTION to="not_di_1|input" from="digital_input_1">
    <!-- A GUI Program could specify the shape of the connection
         here. Not relevant for the compiler -->
</CONNECTION>
<!-- literal constants for booleans are "true" and "false"-->
<MEM name ="count_expired_lp" initial_condition="false"/>
<!-- Memory block would store the state each pass of the variable
     specified by current_pass_value at the end of execution 
     such that the last_pass_value can be used in the local scope
     without suffering from algebraic loops -->
<CONNECTION to="count_expired_lp|current" from="count_expired"/>
<OR name="reset_blink"/>
<!-- OR, AND, etc. Gates can specify any number of inputs via
     incrementing the input specifiers "input1", "input2",
     "input3", etc. -->
<CONNECTION to="reset_blink|input1" from="not_di_1|output"/>
<!-- The "block/output_name" syntax is how to create a connection 
     directly without an intermediate signal -->
<CONNECTION to="reset_blink|input2" from="count_expired_lp|stored"/>
<!-- The | operator on a reference denotes a member of the referenced
     file or library -->
<BLOCK name="timer_instance_1" reference="./timer.vs|timer">
    <!-- Subsystem block can either be locally defined (no reference
         attribute) or reference an external file (with reference
         attribute). File location is referenced relatively, or via
         include statements for library parts folders -->
    <!-- If a subsystem block were locally defined, everything inside
         the tag would be considered a part of that subsystem  -->
</BLOCK>
<CONNECTION to="timer_instance_1|start" from="digital_input_1"/>
<!-- Input and output connections to blocks are partially ambigious.
     However for a Connection to work, one and only one of "to" or
     "from" attributes must be an input/output of the part. -->
<CONNECTION to="timer_instance_1|reset" from="reset_blink|output"/>
<!-- You can plug in hardcoded constants as inputs to blocks
     directly. Literal Constants for singles have decimal points
     (e.g. 2.000). Literal Constants for doubles have "L" as a
     suffix (e.g. 2.00L) -->
<CONNECTION to="timer_instance_1|time" from="2.000"/>
<CONNECTION to="count_expired" from="timer_instance_1|count_expired"/>
<!-- An output can be used in a from statement as many times as desired.
     However, a name can only be assigned to a to statement once. -->
<CONNECTION to="digital_output_1" from="timer_instance_1|count_expired"/>
<!-- Any un-attached outputs to a block are optimized out, e.g. 
     elapsed_time. All inputs are required -->
<OUTPUT scope="device" name="digital_output_1"
     size="1" type="boolean" address="0x0456|0"/>
<!-- It is good practice to define outputs at the bottom of a document -->
<!-- Hardware address 456 bit 0 is DO_1 for ATMega328 -->
<!-- TODO: Find real hardware address for this DO -->
</PROGRAM>
