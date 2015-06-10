<?xml version="1.0" encoding="UTF-8"?>
<!-- All "parts" added by the user can use Inputs and/or
     Outputs for utilization elsewhere in project. The
     reference will search the path for that file -->
<!-- All Inputs do not have to be used and will be optimized out -->
<Input scope=local name="start" type=boolean/>
<Input scope=local name="reset" type=boolean/>
<Input scope=local name="time" type=single/>
<!-- Constants can be defined as a seperate block as well -->
<Constant name="zero_constant" type=single value=0.000/>
<!-- The DT block puts out the difference in time between
     successive passes of program. In a Soft RTOS, this
     would be a variable number. In a Hard RTOS, this
     would be a constant number. -->
<DT name="time_since_last_pass"/>
<NOT name="count_not_expired">
    <Connection to="input" from="count_expired_lp/stored"/>
</NOT>
<AND name="start_enb">
    <Connection to="input1" from="start"/>
    <Connection to="input2" from="count_not_expired/output"/>
</AND>
<IF name="increment_value">
    <!-- Control flow IF switch: If Control is true, execute
         True assignment, else execute False assignment -->
    <Connection to="control" from="start_enb/output"/>
    <Connection to="true_input" from="time_since_last_pass/dt"/>
    <Connection to="false_input" from="zero_constant"/>
</IF>
<SUM name="summer">
    <!-- The summer will add all the inputs together. If you want
         add a negative number, use the NEG part to negate the
         signal before connecting to this part. -->
    <!-- Additionally, the PROD part exists for taking the PI
         product of a set of inputs, and the INV command for taking
         the recipicral of a number (divide by zero runtime error
         possible) -->
    <Connection to="input1" from="increment_value/output"/>
    <Connection to="input2" from="elapsed_time_lp/stored"/>
</SUM>
<IF name="reset_switch">
    <Connection to="control" from="reset"/>
    <Connection to="true_input" from="zero_constant"/>
    <Connection to="false_input" from="summer/output"/>
    <!-- Outputs of a subsystem need to have a connection specified -->
    <Connection to="elapsed_time" from="output"/>
</IF>
<COMPARE name="count_expired" operator=">=">
    <Connection to="input1" from="elapsed_time"/>
    <Connection to="input2" from="time"/>
    <Connection to="count_expired" from="output"/>
</COMPARE>
<MEM name="elapsed_time_lp" initial_condition=0.000>
    <Connection to="current" from="elapsed_time"/>
</MEM>
<MEM name="count_expired_lp" initial_condition=false>
    <Connection to="current" from="count_expired"/>
</MEM>
<!-- All Outputs need to have a connection in the part,
     at least to a constant -->
<Output scope=local name="count_expired" type=boolean/>
<Output scope=local name="elapsed_time" type=single/>
