# Perl script to generate a DTrace provider specification from dist/events.in
$class = undef;
print "provider bdb {\n";
while (<>) {
	$lineno++;
	next if (/^#/);
	chop;

	# A line starting with a letter is an event class name.
	if (/^[a-z]/) {
		$class = $_;
		next;
	}
	# <whitespace> type (arglist); -- other comments possible
	if (/([a-z-_]*)[	 ]*(\([^)]*\);)/) {
		printf("\tprobe %s__%s%s\n", $class, $1, $2);
	} else {
	    printf("** Error in line %d: %s\n", $lineno, $_);
	    printf("** Missing or unrecognized parameter list under class %s\n", $class);
	    exit(1);
	}
}
print "};\n"
