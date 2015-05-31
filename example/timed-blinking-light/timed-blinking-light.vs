<Style Tag>
<Input scope=device name="digital_input_1" type=boolean>
    <!-- Hardware address 123 is DI_1 for ATMega328 -->
    <!-- TODO: Find real hardware address for this DI -->
    <Address>0x0123</Address>
</Input>
<Signal scope=global name="timer_reset" type=boolean/>
<Signal scope=local name="not_gate_output" type=boolean/>
<Signal scope=local name="or_gate_output" type=boolean/>
<Signal scope=local name="count_expired_lp" type=boolean/>
<NOT>
    <!-- All gates have input(s) and an output to connect to -->
    <Connection to="input" from="digital_input_1">
        <!-- A GUI Program could specify the shape of the connection here -->
        <!-- Not relevant for the compiler -->
    </Connection>
    <Connection to="not_gate_output" from="output"/>
</NOT>
<MEM initial_condition=0>
    <!-- Memory block would store the state each pass of the variable
         specified by current_pass_value at the end of execution 
         such that the last_pass_value can be used in the local scope
         without suffering from algebraic loops -->
    <Connection to="current_pass_value" from="count_expired"/>
    <Connection to="count_expired_lp" from="last_pass_value"/>
</MEM>
<OR>
    <!-- OR, AND, etc. Gates can specify any number of inputs via incrementing
         the input specifiers "input1", "input2", "input3", etc. -->
    <Connection to="input1" from="not_gate_output"/>
    <Connection to="input2" from="count_expired_lp"/>
    <Connection to="or_gate_output" from="output"/>
</OR>
<Constant name="timer_time" type=single value=10/>
<Subsystem name="timer_instance_1" reference="timer.vs">
    <Connection to="start" from="digital_input_1"/>
    <Connection to="reset" from="or_gate_output"/>
    <Connection to="time" from="timer_time"/>
    <Connection to="digital_output_1" from="count_expired"/>
    <!-- Any un-attached outputs are optimized out, e.g. elapsed_time -->
    <!-- All inputs are required -->
</Subsystem>
<Output scope=device name="digital_output_1" type=boolean>
    <!-- Hardware address 456 is DO_1 for ATMega328 -->
    <!-- TODO: Find real hardware address for this DO -->
    <Address>0x0456</Address>
</Output>
