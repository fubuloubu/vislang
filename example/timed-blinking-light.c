#include <stdbool.h>
#include <stdint.h>
#include <float.h>
#include <math.h>

#include "./timer.c"

/* I/O Structures for block timed_blinking_light */
struct timed_blinking_light_in {
	bool digital_input_1;
};

struct timed_blinking_light_out {
	bool digital_output_1;
};

/* Initialize static variables */
static bool count_expired_lp = false;
static float_t time = 2.;

struct timed_blinking_light_out timed_blinking_light(struct timed_blinking_light_in inputs) {
	/* Inputs for block timed_blinking_light */
	bool digital_input_1 = inputs.digital_input_1;

	/* Body for block timed_blinking_light */
	bool not_di_1 = !(digital_input_1);
	bool reset_blink = not_di_1 || count_expired_lp;
	struct timer_in timer_instance_1_inputs = { .time = time, .reset = reset_blink_outputs.output, .start = digital_input_1 };
	struct timer_out timer_instance_1_outputs = timer(timer_instance_1_inputs);
	bool digital_output_1 = timer_instance_1_outputs.count_expired;
	count_expired_lp = timer_instance_1_outputs.count_expired; /* Update for next pass */

	/* Outputs for block timed_blinking_light */
	struct timed_blinking_light_out outputs;
	outputs.digital_output_1 = digital_output_1;

	return outputs;
}

/* Generated using VLCC */
