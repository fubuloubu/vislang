<?xml version="1.0" encoding="UTF-8"?>
<BLOCK name="timer">
<!-- The BLOCK element denotes a subsystem of parts -->
<!-- All "parts" added by the user can use Inputs and/or
     Outputs for utilization elsewhere in project. The
     reference will search the path for that file -->
<!-- All Inputs do not have to be used and will be optimized out -->
<INPUT scope=local name="start" type=boolean/>
<INPUT scope=local name="reset" type=boolean/>
<INPUT scope=local name="time" type=single/>
<!-- Constants can be defined as a seperate block as well -->
<CONSTANT name="zero_constant" type=single value=0.000/>
<!-- The DT block puts out the difference in time between
     successive passes of program. In a Soft RTOS, this
     would be a variable number. In a Hard RTOS, this
     would be a constant number. -->
<DT name="time_since_last_pass"/>
<NOT name="count_not_expired"/>
<CONNECTION to="count_not_expired|input" from="count_expired_lp|stored"/>
<AND name="start_enb"/>
<CONNECTION to="start_enb|input1" from="start"/>
<CONNECTION to="start_enb|input2" from="count_not_expired|output"/>
<IF name="increment_value"/>
<!-- Control flow IF switch: If Control is true, execute
     True assignment, else execute False assignment -->
<CONNECTION to="increment_value|control" from="start_enb|output"/>
<CONNECTION to="increment_value|true_input" from="time_since_last_pass|dt"/>
<CONNECTION to="increment_value|false_input" from="zero_constant"/>
<SUM name="summer"/>
<!-- The summer will add all the inputs together. If you want
     add a negative number, use the NEG part to negate the
     signal before connecting to this part. -->
<!-- Additionally, the PROD part exists for taking the PI
     product of a set of inputs, and the INV command for taking
     the recipicral of a number (divide by zero runtime error
     possible) -->
<Connection to="summer|input1" from="increment_value|output"/>
<Connection to="summer|input2" from="elapsed_time_lp|stored"/>
<IF name="reset_switch"/>
<CONNECTION to="reset_switch|control" from="reset"/>
<CONNECTION to="reset_switch|true" from="zero_constant"/>
<CONNECTION to="reset_switch|false" from="summer|output"/>
<!-- Outputs of a subsystem need to have a connection specified -->
<CONNECTION to="elapsed_time" from="reset_switch|output"/>
<COMPARE name="is_count_expired" operator=">="/>
<CONNECTION to="is_count_expired|lhs" from="elapsed_time"/>
<CONNECTION to="is_count_expired|rhs" from="time"/>
<CONNECTION to="count_expired" from="is_count_expired|output"/>
<MEM name="elapsed_time_lp" initial_condition=0.000/>
<CONNECTION to="elapsed_time_lp|current" from="elapsed_time"/>
<MEM name="count_expired_lp" initial_condition=false/>
<CONNECTION to="count_expired_lp|current" from="count_expired"/>
<!-- All Outputs need to have a connection in the part,
     at least to a constant -->
<OUTPUT scope=local name="count_expired" type=boolean/>
<OUTPUT scope=local name="elapsed_time" type=single/>
</BLOCK>
