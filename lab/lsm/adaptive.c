/*-
 * libdb LSM prototype: unified adaptive controller implementation.
 * See adaptive.h and docs/design/lsm.md.
 */
#include "adaptive.h"

/* ------------------------------------------------------------------ */
/* Sliding-window bucketed counter.                                   */
/* ------------------------------------------------------------------ */
void
adc_roll_init(adc_roll *r, uint64_t window)
{
	int i;

	if (window < ADC_BUCKETS)
		window = ADC_BUCKETS;
	r->window = window;
	r->bucket_span = window / ADC_BUCKETS;
	if (r->bucket_span == 0)
		r->bucket_span = 1;
	for (i = 0; i < ADC_BUCKETS; i++) {
		r->epoch[i] = 0;
		r->count[i] = 0;
	}
}

void
adc_roll_add(adc_roll *r, uint64_t now, uint64_t n)
{
	uint64_t b = now / r->bucket_span;
	int slot = (int)(b % ADC_BUCKETS);

	/* If this slot holds a stale bucket, recycle it. */
	if (r->epoch[slot] != b) {
		r->epoch[slot] = b;
		r->count[slot] = 0;
	}
	r->count[slot] += n;
}

uint64_t
adc_roll_count(const adc_roll *r, uint64_t now)
{
	uint64_t b = now / r->bucket_span;
	uint64_t oldest = (b + 1 >= ADC_BUCKETS) ? (b + 1 - ADC_BUCKETS) : 0;
	uint64_t total = 0;
	int i;

	for (i = 0; i < ADC_BUCKETS; i++)
		if (r->count[i] != 0 && r->epoch[i] >= oldest && r->epoch[i] <= b)
			total += r->count[i];
	return (total);
}

int
adc_cooldown_ok(uint64_t last, uint64_t now, uint64_t cooldown)
{
	if (last == 0)			/* never switched */
		return (1);
	return (now >= last && (now - last) >= cooldown);
}

/* ------------------------------------------------------------------ */
/* STRUCTURE axis.                                                    */
/* ------------------------------------------------------------------ */
void
adc_structure_init(adc_structure *s, uint64_t window, uint64_t cooldown)
{
	adc_roll_init(&s->writes, window);
	adc_roll_init(&s->reads, window);
	s->mode = ADC_SINGLE;
	s->last_switch = 0;
	s->cooldown = cooldown;
	/* Defaults expressed as events-per-window; hysteresis gap is 2x. */
	s->spawn_hybrid = 100;
	s->spawn_multi = 1000;
	s->collapse_hybrid = 500;
	s->collapse_single = 50;
}

void
adc_structure_record_write(adc_structure *s, uint64_t now)
{
	adc_roll_add(&s->writes, now, 1);
}

void
adc_structure_record_read(adc_structure *s, uint64_t now)
{
	adc_roll_add(&s->reads, now, 1);
}

adc_mode
adc_structure_eval(adc_structure *s, uint64_t now)
{
	uint64_t w;

	if (!adc_cooldown_ok(s->last_switch, now, s->cooldown))
		return (s->mode);

	w = adc_roll_count(&s->writes, now);

	/* Move one step toward the workload-indicated complexity. */
	switch (s->mode) {
	case ADC_SINGLE:
		if (w >= s->spawn_hybrid) {
			s->mode = ADC_HYBRID;
			s->last_switch = now;
		}
		break;
	case ADC_HYBRID:
		if (w >= s->spawn_multi) {
			s->mode = ADC_MULTILEVEL;
			s->last_switch = now;
		} else if (w <= s->collapse_single) {
			s->mode = ADC_SINGLE;
			s->last_switch = now;
		}
		break;
	case ADC_MULTILEVEL:
		if (w <= s->collapse_hybrid) {
			s->mode = ADC_HYBRID;
			s->last_switch = now;
		}
		break;
	}
	return (s->mode);
}

/* ------------------------------------------------------------------ */
/* POLICY axis (per segment).                                         */
/* ------------------------------------------------------------------ */
void
adc_segment_init(adc_segment *seg, uint64_t window, uint64_t cooldown,
    adc_policy initial)
{
	adc_roll_init(&seg->reads, window);
	adc_roll_init(&seg->writes, window);
	seg->policy = initial;
	seg->last_switch = 0;
	seg->cooldown = cooldown;
	seg->ratio_pct = 150;	/* need a 1.5x majority to flip */
}

void
adc_segment_record_read(adc_segment *seg, uint64_t now)
{
	adc_roll_add(&seg->reads, now, 1);
}

void
adc_segment_record_write(adc_segment *seg, uint64_t now)
{
	adc_roll_add(&seg->writes, now, 1);
}

adc_policy
adc_segment_eval(adc_segment *seg, uint64_t now)
{
	uint64_t r, w;

	if (!adc_cooldown_ok(seg->last_switch, now, seg->cooldown))
		return (seg->policy);

	r = adc_roll_count(&seg->reads, now);
	w = adc_roll_count(&seg->writes, now);

	/*
	 * Write-dominated -> TIERED (cheap writes); read-dominated -> LEVELED
	 * (cheap reads). Require a ratio_pct majority to switch so balanced or
	 * idle segments keep their current policy (avoids churn).
	 */
	if (seg->policy == ADC_LEVELED) {
		if (w * 100 >= r * seg->ratio_pct && w > 0) {
			seg->policy = ADC_TIERED;
			seg->last_switch = now;
		}
	} else { /* ADC_TIERED */
		if (r * 100 >= w * seg->ratio_pct && r > 0) {
			seg->policy = ADC_LEVELED;
			seg->last_switch = now;
		}
	}
	return (seg->policy);
}
