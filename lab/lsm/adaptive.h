/*-
 * libdb LSM prototype: unified adaptive controller.
 *
 * One rolling-window + cooldown core drives two orthogonal adaptation axes
 * (see docs/design/lsm.md):
 *
 *   - STRUCTURE axis: how much LSM to run for the whole store
 *       SINGLE  <->  HYBRID  <->  MULTILEVEL        (aether-style)
 *     decided by env-wide write/read/flush rates with hysteresis.
 *
 *   - POLICY axis: how to compact each segment
 *       LEVELED  <->  TIERED                        (Amethyst-style)
 *     decided by that segment's read/write hotness.
 *
 * Both axes use the same primitives: a sliding-window event counter
 * (adc_roll) and a cooldown gate (adc_cooldown_ok). Time is a caller-supplied
 * monotonic "tick" so the controller is deterministic and unit-testable; in
 * the engine the tick is a millisecond clock.
 *
 * This is a self-contained prototype with no dependency on the BDB engine; it
 * exists to validate the control logic against phase-shifting workloads before
 * it is wired under an access method.
 */
#ifndef LIBDB_LSM_ADAPTIVE_H
#define LIBDB_LSM_ADAPTIVE_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Sliding-window event counter over the last `window` ticks, bucketed. */
#define ADC_BUCKETS 16
typedef struct {
	uint64_t	window;			/* total window length in ticks */
	uint64_t	bucket_span;		/* window / ADC_BUCKETS (>=1) */
	uint64_t	epoch[ADC_BUCKETS];	/* bucket index currently stored */
	uint64_t	count[ADC_BUCKETS];	/* events in that bucket */
} adc_roll;

void	 adc_roll_init(adc_roll *, uint64_t window);
void	 adc_roll_add(adc_roll *, uint64_t now, uint64_t n);
/* Events observed within the trailing window as of `now`. */
uint64_t adc_roll_count(const adc_roll *, uint64_t now);

/* True if `cooldown` ticks have elapsed since `last` (or never switched). */
int	 adc_cooldown_ok(uint64_t last, uint64_t now, uint64_t cooldown);

/* ----- STRUCTURE axis ----- */
typedef enum {
	ADC_SINGLE = 0,		/* direct single index, no LSM overhead */
	ADC_HYBRID = 1,		/* memtable + one persistent index */
	ADC_MULTILEVEL = 2	/* full multi-level LSM */
} adc_mode;

typedef struct {
	adc_roll	writes;
	adc_roll	reads;		/* reserved for read-aware tuning */
	adc_mode	mode;
	uint64_t	last_switch;
	uint64_t	cooldown;
	/* write-rate thresholds (events per window); spawn > collapse = hysteresis */
	uint64_t	spawn_hybrid;	/* SINGLE   -> HYBRID     */
	uint64_t	spawn_multi;	/* HYBRID   -> MULTILEVEL */
	uint64_t	collapse_hybrid;/* MULTILEVEL -> HYBRID   */
	uint64_t	collapse_single;/* HYBRID   -> SINGLE     */
} adc_structure;

void	 adc_structure_init(adc_structure *, uint64_t window, uint64_t cooldown);
void	 adc_structure_record_write(adc_structure *, uint64_t now);
void	 adc_structure_record_read(adc_structure *, uint64_t now);
/* Re-evaluate and possibly transition (one step toward target); returns mode. */
adc_mode adc_structure_eval(adc_structure *, uint64_t now);

/* ----- POLICY axis (per segment) ----- */
typedef enum {
	ADC_LEVELED = 0,	/* read-optimized: low read amplification */
	ADC_TIERED = 1		/* write-optimized: low write amplification */
} adc_policy;

typedef struct {
	adc_roll	reads;
	adc_roll	writes;
	adc_policy	policy;
	uint64_t	last_switch;
	uint64_t	cooldown;
	/* switch when one side exceeds the other by this ratio (percent, e.g. 150). */
	uint64_t	ratio_pct;
} adc_segment;

void	 adc_segment_init(adc_segment *, uint64_t window, uint64_t cooldown,
	    adc_policy initial);
void	 adc_segment_record_read(adc_segment *, uint64_t now);
void	 adc_segment_record_write(adc_segment *, uint64_t now);
adc_policy adc_segment_eval(adc_segment *, uint64_t now);

#ifdef __cplusplus
}
#endif
#endif /* LIBDB_LSM_ADAPTIVE_H */
