<Style Tag>
<!-- All "parts" added by the user can use Inputs and/or
     Outputs for utilization elsewhere in project. The
     reference will search the path for that file -->
<!-- All Inputs do not have to be used and will be optimized out -->
<Input scope=local name="start" type=boolean/>
<Input scope=local name="reset" type=boolean/>
<Input scope=local name="time" type=single/>
<!-- All Outputs need to have a connection, at least to a constant -->
<Output scope=local name="count_expired" type=boolean/>
<Output scope=local name="elapsed_time" type=single/>
<Signal scope=local name="not_gate_output" type=boolean/>
<Signal scope=local name="and_gate_output" type=boolean/>
<Signal scope=local name="count_expired_lp" type=boolean/>
<Signal scope=local name="enable_state_output" type=single/>
<Signal scope=local name="reset_state_output" type=single/>
<Signal scope=local name="summer_output" type=single/>
<Signal scope=local name="elapsed_time_lp" type=single/>
<Signal scope=local name="time_since_last_pass" type=single/>
<Constant name="zero_constant" type=single value=0.000/>
<DT>
    <!-- The DT block puts out the difference in time between
         successive passes of program. In a Soft RTOS, this
         would be a variable number. In a Hard RTOS, this
         would be a constant number. -->
    <Connection to="time_since_last_pass" from="output">
</DT>
<NOT>
    <Connection to="input" from="count_expired_lp"/>
    <Connection to="not_gate_output" from="output"/>
</NOT>
<AND>
    <Connection to="input1" from="start"/>
    <Connection to="input2 from="count_expired_lp"/>
    <Connection to="and_gate_output" from="output"/>
</AND>
<IF>
    <!-- Control flow IF switch: If Control is true, execute
         True assignment, else execute False assignment -->
    <Connection to="control" from="and_gate_output"/>
    <Connection to="true_input" from="time_since_last_pass"/>
    <Connection to="false_input" from="zero_constant"/>
    <Connection to="enable_state_output" from="output"/>
</IF>
<SUM>
    <!-- The summer will add all the inputs together. If you want
         add a negative number, use the NEG part to negate the
         signal before connecting to this part. -->
    <!-- Additionally, the PROD part exists for taking the PI
         product of a set of inputs, and the INV command for taking
         the recipicral of a number (divide by zero runtime error
         possible) -->
    <Connection to="input1" from="enable_state_output"/>
    <Connection to="input2" from="elapsed_time_lp"/>
    <Connection to="summer_output" from="output"/>
</SUM>
<IF>
    <Connection to="control" from="reset"/>
    <Connection to="true_input" from="zero_constant"/>
    <Connection to="false_input" from="summer_output"/>
    <Connection to="elapsed_time" from="output"/>
</IF>
<COMPARE operator=">=">
    <Connection to="input1" from="elapsed_time"/>
    <Connection to="input2" from="time"/>
    <Connection to="count_expired" from="output"/>
</COMPARE>
<MEM initial_condition=0.000>
    <Connection to="current_pass_value" from="elapsed_time"/>
    <Connection to="elapsed_time_lp" from="last_pass_value"/>
</MEM>
<MEM initial_condition=0>
    <Connection to="current_pass_value" from="count_expired"/>
    <Connection to="count_expired_lp" from="last_pass_value"/>
</MEM>
