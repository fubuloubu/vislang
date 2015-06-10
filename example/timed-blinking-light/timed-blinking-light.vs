<?xml version="1.0" encoding="UTF-8"?>
<!-- Hardware address 123 is DI_1 for ATMega328 -->
<!-- TODO: Find real hardware address for this DI -->
<Input scope=device name="digital_input_1" type=boolean address=0x0123/>
<!-- Literal constants for unsigned integers can be hex (0x20), binary (2x110101),
     octal (8x266), or decimal (268). Literal constants for signed intergers 
     must have a sign prefixed (e.g. +0x26A8 or -268). Neither int nor uint can
     have a decimal point. -->
<Signal scope=global name="timer_reset" type=boolean/>
<!-- It is good practice to specify Inputs, Signals, and Constants at the top
     of a document, although not strictly necessary -->
<NOT name="not_di_1">
    <!-- All gates have input(s) and an output to connect to -->
    <Connection to="input" from="digital_input_1">
        <!-- A GUI Program could specify the shape of the connection here -->
        <!-- Not relevant for the compiler -->
    </Connection>
    <!-- You can reference outputs of blocks as inputs into other blocks 
         without defining an intermediate signal. See OR gate below -->
</NOT>
<!-- literal constants for booleans are "true" and "false"-->
<MEM name ="count_expired_lp" initial_condition=false>
    <!-- Memory block would store the state each pass of the variable
         specified by current_pass_value at the end of execution 
         such that the last_pass_value can be used in the local scope
         without suffering from algebraic loops -->
    <Connection to="current" from="count_expired"/>
</MEM>
<OR name="reset_blink">
    <!-- OR, AND, etc. Gates can specify any number of inputs via incrementing
         the input specifiers "input1", "input2", "input3", etc. -->
    <Connection to="input1" from="not_di_1/output"/>
    <!-- The "block/output_name" syntax is how to create a connection 
         directly without an intermediate signal -->
    <Connection to="input2" from="count_expired_lp/stored"/>
</OR>
<Subsystem name="timer_instance_1" reference="./timer.vs">
    <!-- Subsystem block can either be locally defined (no reference attribute)
         or reference an external file (with reference attribute). File location
         is referenced relatively, or via include statements for library parts 
         folders -->
    <Connection to="start" from="digital_input_1"/>
    <!-- Input and output connections to blocks are partially ambigious. However
         for a Connection to work, one and only one of "to" or "from" attributes
         must be an input/output of the part. -->
    <Connection to="reset" from="reset_blink/output"/>
    <!-- You can plug in hardcoded constants as inputs to blocks directly -->
    <!-- Literal Constants for singles have decimal points (e.g. 2.000) -->
    <!-- Literal Constants for doubles have "L" as a suffix (e.g. 2.00L)-->
    <Connection to="time" from=2.000/>
    <Connection to="digital_output_1" from="count_expired"/>
    <!-- Any un-attached outputs are optimized out, e.g. elapsed_time -->
    <!-- All inputs are required -->
</Subsystem>
<!-- It is good practice to define outputs at the bottom of a document -->
<!-- Hardware address 456 is DO_1 for ATMega328 -->
<!-- TODO: Find real hardware address for this DO -->
<Output scope=device name="digital_output_1" type=boolean address=0x0456/>
