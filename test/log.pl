#!/usr/bin/perl
#
# @(#)log.pl	11.5 (Sleepycat) 8/31/99

use English;
use Getopt::Long;

# We asume we are being run in the build directory.
#
# --read <dirname> causes us to dump the logs for directory
#     <dirname> and record which log records were seen
# --summary prints out the log record types that were seen but
#     should not have have been seen and the log record types that
#     were not seen but should have been seen.

my $save_file = "log.save";
my %existing;
my %seen;
my $record;
my $key;

GetOptions ("init", "read=s", "summary=s");

if ($opt_init) {
	unlink ($save_file);
}

if ($opt_read ne "") {
	if (-e $save_file) {
		open (SEEN, "<$save_file");
		while ($record = <SEEN>) {
			chomp($record);
			$seen{$record} = 1;
		}
		close(SEEN);
	}

	open(DB_PRINTLOG, "./db_printlog -N -h '$opt_read'|");
	while ($record = <DB_PRINTLOG>) {
		$record =~ m{\[[^\]]*\]\[[^\]]*\]([^\:]*)\:};
		if ($1 ne "") {
			$seen{$1} = 1;
		}
	}
	close (DB_PRINTLOG);

	open (SEEN, ">$save_file");
	print SEEN (join("\n", sort(keys(%seen))));
	close(SEEN);
}

if ($opt_summary) {
	my @srcFiles =
	    ("$opt_summary/../db/db.src", "$opt_summary/../db/crdel.src",
	    "$opt_summary/../btree/btree.src", "$opt_summary/../hash/hash.src",
	    "$opt_summary/../qam/qam.src", "$opt_summary/../log/log.src",
	    "$opt_summary/../txn/txn.src");
	my $srcFile;
	my $contents;

	my $prefix;
	my $comment;
	my $log_type;

	foreach $srcFile (@srcFiles) {
		open (FILE, "<$srcFile") || die "Cannot find $srcFile.";
		read (FILE, $contents, -s $srcFile);
		close (FILE);

		$contents =~ m{\nPREFIX\s+(.*)};
		$prefix = $1;

		while( $contents =~
			m{
				(
					/\*
					#(?:.|\n)*?
					(?:[^\*]|\*(?!/))*
					\*/
				)?
				\s*
				\nBEGIN\s*(.*)
			}xgo ) {
			$comment = $1;
			$log_type = $2;

			if ($comment !~ m{DEPRECATED}) {
				if( $log_type ne "debug" && $log_type ne "noop" ) {
					$existing{$prefix . "_" . $log_type} = 1;
					if( $log_type =~ m{split*ta} ) {
						print $log_type, "\n";
					}
				}
			}
		}
	}

	if (-e $save_file) {
		open (SEEN, "<$save_file");
		while ($record = <SEEN>) {
			chomp($record);
			$seen{$record} = 1;
		}
		close(SEEN);
	}

	foreach $key (sort(keys(%existing))) {
		if ($seen{$key} != 1) {
			print STDOUT ("FAIL: log record type $key not seen\n");
		}
	}

	foreach $key (sort(keys(%seen))) {
		if ($existing{$key} != 1) {
			print STDOUT
	("FAIL: log record type $key seen but should not have been seen\n");
		}
	}

	unlink($srcFile);
}
