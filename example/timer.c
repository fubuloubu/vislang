#include <stdbool.h>
#include <stdint.h>
#include <float.h>
#include <math.h>

/* I/O Structures for block timer */
struct timer_in {
	bool reset;
	bool start;
	float_t time;
};

struct timer_out {
	bool count_expired;
	float_t elapsed_time;
};

/* Initialize static variables */
static bool count_expired_lp = false;
static float_t elapsed_time_lp = 0.;
static float_t zero_constant = 0.;
static float_t time_since_last_pass = 0.1;

struct timer_out
/* Function def */ timer(struct timer_in inputs)
{
	/* Inputs for block timer */
	bool reset = inputs.reset;
	bool start = inputs.start;
	float_t time = inputs.time;

	/* Body for block timer */
	bool count_not_expired = !(count_expired_lp);
	bool start_enb = start && count_not_expired;
	float_t increment_value = (start_enb) ? (time_since_last_pass) : (zero_constant);
	float_t summer = increment_value + elapsed_time_lp;
	float_t reset_switch = (reset) ? (zero_constant) : (summer);
	float_t elapsed_time = reset_switch;
	bool is_count_expired = (elapsed_time >= time);
	bool count_expired = is_count_expired;
	elapsed_time_lp = elapsed_time;
	count_expired_lp = count_expired;

	/* Outputs for block timer */
	struct timer_out outputs;
	outputs.count_expired = count_expired;
	outputs.elapsed_time = elapsed_time;

	return outputs;
}

/* Generated using VLCC */
