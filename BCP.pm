#	@(#)BCP.pm	1.4	2/20/96

# Copyright (c) 1996
#   Michael Peppler
#
#   You may copy this under the terms of the GNU General Public License,
#   or the Artistic License, copies of which should have accompanied
#   your Perl kit.

package Sybase::BCP;

=head1 NAME

Sybase::BCP - Simple front end to the Sybase BCP API

=head1 SYNOPSIS

use Sybase::BCP;

C<$bcp = new Sybase::BCP ...;>

C<$bcp->config(...);>

C<$bcp->run;>

=head1 DESCRIPTION

The Sybase::BCP module serves as a simplified front end for Sybase's Bulk
Copy library. So how does it work?

Let's say we want to copy the contents of a file name 'foo.bcp' into the
table 'mydb.dbo.bar'. The fields in the file are separated by a '|'.

    #!/usr/local/bin/perl

    use Sybase::BCP;

    $bcp = new Sybase::BCP $user, $passwd;
    $bcp->config(INPUT => 'foo.bcp',
		 OUTPUT => 'mydb.dbo.bar',
		 SEPARATOR => '|');
    $bcp->run;

That's it!

Of course, there are several things you can do to cater for non-standard
input files (see B<Configuration Parameters>, below).

=head2 Features

=over 4

=item * Automatic conversions from non-standard date formats.

=item * Automatic retries of failed batches.

If there are errors in the input file, or if there are duplicat rows that are
rejected, the invalid rows are stored in an error log file, and the batch is
retried, so that only the failed rows are not uploaded.

=item * Handles column reordering and/or skipping of unneeded data.

=item * Row or column based callbacks.

Allows vetoing of rows, or arbitrary processing of data on input.

=back

=head2 The following methods are available:

=over 4

=item $bcp = new Sybase::BCP [$user [, $password [, $server [, $appname]]]]

Allocate a new B<BCP> handle. Opens a new connection to Sybase via the
B<Sybase::DBlib> module, and enables BCP IN on this handle.

=item $bcp->config([parameters])

Sets up the Bulk Copy operation. See B<Configuration Parameters> below for
details.

=item $bcp->describe($colid, {parameters})

Adds a specific configuration element for column $colid. Columns are numbered
starting at 1, as is standard in the Sybase APIs.

=item $bcp->run

Perform the B<BCP> operation.

=back

=head2 Configuration Parameters

The general form for configuration is to pass (parameter => value) pairs
via the config() or describe() methods. Some parameters take slightly more
complex arguments (see B<REORDER>).

=head2 Paramaters for config()

=over 4

=item DIRECTION

The direction in which the bulkcopy operation is done. Can be 'IN' or 'OUT'.
Default: 'IN' (I<Note:> 'OUT' is not implemented yet.)

=item INPUT

Where B<BCP> should take it's input from. It's a filename for B<bcp IN>, it's
a table name for B<bcp OUT>.

=item OUTPUT

Where B<BCP> should place it's output. It's a table name for B<bcp IN>, a
filename for B<bcp OUT>.

=item ERRORS

The file where invalid rows should be recorded. Default: bcp.err.

=item SEPARATOR

The pattern that separates fields in the input file, or that should be used
to separate fields in the output file. Default: TAB.

=item FIELDS

Number of fields in the input file for B<bcp IN> operations. Default: Number
of fields found in the first line. This parameter is ignored for B<bcp OUT>.

=item BATCH_SIZE

Number of rows to be batched together before committing to the server for
B<bcp IN> operations. Defaults to 100. If there is a risk that retries could
be requiered due to failed batches (e.g. duplicat rows/keys errors) then
you should not use a large batch size: one failed row in a batch requires
the entire batch to be resent.

=item NULL

A pattern to be used to detect NULL values in the input file. Defaults to
a zero length string.

=item DATE

The default format for DATE fields in the input file. The parameter should
be a symbolic value representing the format. Currently, the following values
are recognized: CTIME (the Unix ctime(3) format), or the numbers 0-12,
100-112, corresponding to the conversion formats defined in table 2-4 of
the I<SQL Server Reference Manual>.

B<BCP> detects I<datetime> targets by looking up the target table
structure in the Sybase system tables.

=item REORDER

The ordering of the fields in the input file does not correspond to the
order of columns in the table, or there are columns that you wish to
skip. The REORDER parameter takes a hash that describes the reordering
operation:

    $bcp->config(...
		 REORDER => { 1 => 2,
			      3 => 1,
			      2 => 'foobar',
			      12 => 4},
		 ...);

In this example, field 1 of the input file goes in column 2 of the table,
field 3 goes in column 1, field 2 goes in the column named I<foobar>, and
field 12 goes in column 4. Fields 4-11, and anything beyond 12 is skipped.
As you can see you can use the column I<name> instead of its position.
The default is to not do any reordering.

=item CALLBACK

The callback subroutine is called for each row (after any reordering), and
allows the user to do global processing on the row, or vetoing it's
processing. Example:

    $bcp->config(...
                 CALLBACK => \&row_cb,
                 ...);

    sub row_cb {
	my $row_ref = shift;

	# Skip rows where the first field starts with FOO:
	return undef if $$row_ref[0] =~ /^FOO/;

	1;
    }

=item CONDITION

A I<where> clause to be used in B<bcp OUT> operations. Not implemented.

=back

=head2 Parameters for describe()

=over 4

=item CALLBACK

Specify a callback for this column. The field value is passed as the first
parameter, and the callback should return the value that it wants B<BCP>
to use. Example:

    $dbh->describe(2, {CALLBACK, \&col_cb});

    sub col_cb {
	my $data = shift;

	# Convert to lower case...
	$data =~ tr/A-Z/a-z/;
    }

=item SKIP

If this is defined then this field is skipped. This is useful if only one or
two fields need to be skipped and you don't want to define a big REORDER hash
to handle the skipping.

=back

=head1 EXAMPLES

    #!/usr/local/bin/perl
    
    use Sybase::BCP;
    require 'sybutil.pl';

    $bcp = new Sybase::BCP sa, undef, TROLL;

    $bcp->config(INPUT => '../../Sybperl/xab',
	         OUTPUT => 'excalibur.dbo.t3',
   	         BATCH_SIZE => 200,
	         FIELDS => 4,
	         REORDER => {1 => 'account',
			     3 => 'date',
			     2 => 'seq_no',
			     11 => 'broker'},
	         SEPARATOR => '|');
    $bcp->run;


=head1 BUGS

The current implementation seems to run about 2.5 to 3 times slower than
plain bcp.

=head1 AUTHOR

Michael Peppler F<E<lt>mpeppler@itf.chE<gt>>. Contact the sybperl mailing
list C<mailto:sybperl-l@trln.lib.unc.edu> if you have any questions.

=cut

# A module implementing a generalized Bulk Copy API.

use Sybase::DBlib qw(2.03);
use Carp;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(dbmsghandle dberrhandle TRUE FALSE INT_CANCEL);

$VERSION = '0.01';

@g_keys = qw(INPUT OUTPUT ERRORS SEPARATOR FIELDS BATCH_SIZE
	     NULL DATE REORDER CALLBACK TAB_INFO DIRECTION CONDITION);
@f_keys = qw(CALLBACK SKIP);
%g_keys = map { $_ => 1 } @g_keys;
%f_keys = map { $_ => 1 } @f_keys;

%date_fmt =
(CTIME => \&_datectime,
 101 => \&_date101,
# 102 => \&_date102, This one is probably automatic...
 103 => \&_date103,
 104 => \&_date104,
 105 => \&_date105,
 106 => \&_date106,
# 107 => \&_date107, This one's probably automatic...
 110 => \&_date110,
 111 => \&_date111,
 112 => \&_date112);
	    

# Handle both 2.04 bcp_sendrow (which can take a reference to an array
# and 2.03 bcp_sendrow which can't
if($Sybase::DBlib::VERSION >= 2.04) {
    *Sybase::DBlib::_sendrow = \&Sybase::DBlib::bcp_sendrow;
} else {
    *Sybase::DBlib::_sendrow = \&send;
}
sub send {
    my $self = shift;
    my $ref = shift;
    $self->bcp_sendrow(@$ref);
}

sub new {
    my(@in) = @_;
    my($package, %h);

    BCP_SETL(TRUE);		# Turn BCP_IN on.
    $package = shift(@in);

    $h{DBlib} = new Sybase::DBlib @in;
    bless \%h, $package;
}

sub DESTROY {
    undef($$_[0]{DBlib});
}

sub config {
    my($self, %ref) = @_;
    my $key, $errs;
    
    foreach $key (keys(%ref)) {
	if(!defined($g_keys{$key})) {
	    carp "$key is not a valid Sybase::BCP key";
	    ++$errs;
	}
    }
    croak "Sybase::BCP processing aborted because of errors\n" if($errs>0);
    $self->{Global} = \%ref;
    # Get the table definition from Sybase system tables:
    $self->{Global}->{TAB_INFO} = _gettabinfo($self->{DBlib},
					      $self->{Global}->{OUTPUT});
}

sub describe {
    my($self, $colid, $ref) = @_;
    my $key, $errs;

    foreach $key (keys(%$ref)) {
	if(!defined($f_keys{$key})) {
	    carp "$key is not a valid Sybase::BCP key";
	    ++$errs;
	}
    }
    croak "Sybase::BCP processing aborted because of errors\n" if($errs>0);
    $self->{Cols}->{$colid-1} = $ref;
}

sub run {
    my $self = shift;

    if($self->{Global}->{DIRECTION} eq 'OUT') {
	$self->do_out(@_);
    } else {
	$self->do_in(@_);
    }
}

sub do_out {
    croak "This ain't implemented yet...";
}

sub do_in {
    my $self = shift;
    my $verbose = shift;
	
    # Initialize:
    my $infile 	= $self->{Global}->{INPUT};
    my $table 	= $self->{Global}->{OUTPUT};
    my $logfile = $self->{Global}->{ERRORS};
    my $sep 	= $self->{Global}->{SEPARATOR};
    my $cols 	= $self->{Global}->{FIELDS};
    my $batch_size = $self->{Global}->{BATCH_SIZE};
    my $null_pattern = $self->{Global}->{NULL};
    my $date_fmt = $self->{Global}->{DATE};
    my %cols 	= %{$self->{Cols}};
    my $d 	= $self->{DBlib};
    my $g_cb 	= $self->{Global}->{CALLBACK};
    my %reorder = %{$self->{Global}->{REORDER}};
    my @tabinfo = @{$self->{Global}->{TAB_INFO}};
    my $i;
    
    croak "You must define a table name!" if(!defined($table));
    croak "You must define an input file name!" if(!defined($infile));

    # The user has defined a reordering pattern of columns:
    # If the target columns are entered as column names, we must
    # convert that back to column numbers...
    if(defined(%reorder)) {
	foreach (keys(%reorder)) {
	    if($reorder{$_} =~ /\D+/) {
		for($i = 0; $i < @tabinfo; ++$i) {
		    if(${$tabinfo[$i]}[0] eq $reorder{$_}) {
			$reorder{$_} = $i+1;
		    }
		}
	    }
	}
    }
    # If one of the target fields is a DATETIME field, then we
    # check to see if the user has defined a default conversion:
    if(defined($self->{Global}->{DATE})) {
	for($i = 0; $i < @tabinfo; ++$i) {
	    if(${$tabinfo[$i]}[1] =~ /datetim/ &&
	       !defined($cols{$i}->{CALLBACK})) {
	        $cols{$i}->{CALLBACK} = $date_fmt{$self->{Global}->{DATE}};
						  #\&_datecvrt;
	    }
	}
    }

    $logfile = 'bcp.err' unless $logfile;
    $sep = "\t" unless $sep;
    $null_pattern = '^$' unless $null_pattern; # '''
    $batch_size = 100 unless $batch_size;
    
    open(IN, $infile) || croak "Can't open file $infile: $!";
    ($d->bcp_init($table, undef, undef, DB_IN) == SUCCEED) ||
	croak "bcp_init failed.";
    open(LOG, ">$logfile") || croak "Can't open file $logfile: $!";
    
    my $count = 0;
    my @data;
    my @t_data;
    my @rows;
    my $row;

    local $" = $sep;			# Set the output field separator."

    while(<IN>) {
	chop;
	@data = split(/\Q$sep\E/o);

	if(defined(%reorder)) {
	    foreach $i (keys(%reorder)) {
		$t_data[$reorder{$i}-1] = $data[$i-1];
	    }
	    @data = @t_data;
	}
			
	if(defined($g_cb)){
	    next unless &$g_cb(\@data);
	}
	# Here we use the number of columns found in the first row of data to
	# define the COPY IN operation.
	if($count == 0) {
	    # Get the number of fields from the first data row if
	    # we didn't get that info via config().
            $cols = scalar(@data) unless $cols;
            $d->bcp_meminit($cols);	# This sets up the copy_in operation.
        }
	
	# If the row is short, push undef values onto the row:
	while(scalar(@data) < $cols) {
	    push(@data, undef);
	}
	# Do any special data handling: set NULL fields, maybe convert dates,
	# call the callbacks if they are defined.
	for($i = 0; $i < $cols; ++$i) {
	    if($cols{$i}->{SKIP} == TRUE) {
		splice(@data, $i, 1);
		next;
	    }
	    if(defined($cols{$i}->{CALLBACK})) {
#		my $sub = $cols{$i}->{CALLBACK};
		$data[$i] = &{$cols{$i}->{CALLBACK}}($data[$i]);
	    }
	    if($data[$i] =~ /\Q$null_pattern\E/o) {
		$data[$i] = undef;
	    }
	}
        # Send the row to the server. A failure here indicates a
        # conversion error of data from the @data array. The row has NOT been sent to
        # the server. We log the row data and move on to the next row.
        if($d->_sendrow(\@data) == FAIL) {
	    print LOG "$_\n";
	    next;
	}
        # Remember this row until we are certain that this batch is OK.
        push(@rows, [@data]);
        #If we've sent $batch_size rows to the server, commit them.
        if((++$count % $batch_size) == 0) {
	    if($d->bcp_batch <= 0) {
		my $r_count = 0;
		carp "bcp_batch failed - redoing";
		# The batch failed, so re-run it one row at a time.
		foreach $row (@rows) {
		    if($d->_sendrow($row) == FAIL) {
			print LOG "@$row\n";
			next;
		    }
		    if($d->bcp_batch != 1) { # batch each row, so that we can find which is wrong...
			print LOG "@$row\n";
		    }
		    else
		    {
			++$r_count;
		    }
		}
		printf STDERR ("bcp sent %d rows to the server (%d failed)\n",
			       $r_count, $batch_size - $r_count);
	    }
	    else
	    {
		print STDERR "bcp sent $batch_size rows to the server...\n";
	    }
	    @rows = ();		# The batch was successfull, flush the row cache.
	}
    }
    # Commit any outstanding rows.
    if(scalar(@rows)) {
	if($d->bcp_batch <= 0) {
	    carp "bcp_batch failed - redoing";
	    foreach $row (@rows) {
		if($d->_sendrow($row) == FAIL) {
		    print LOG "@$row\n";
		    next;
		}
		if($d->bcp_batch != 1) { # batch each row, so that we can find which is wrong...
		    print LOG "@$row\n";
		}
	    }
	}
    }
    $d->bcp_done;

    close(LOG);
    close(IN);
}


# Extracts information about the column names and column types from
# the database. Uses the system tables for this.
sub _gettabinfo {
    my $dbh = shift;
    my $table = shift;
    my($db, $user, $tab);
    my $ref;

    ($db, $user, $tab) = split(/\./, $table);
    croak "Must specify the Sybase table as database.user.table"
	if (!defined($tab));
    $user = 'dbo' if(!defined($user) || $user =~ /^$/);

    $ref = $dbh->sql("
select c.name, t.name
from $db.dbo.syscolumns c, $db.dbo.systypes t
where c.id = object_id('$table')
and   c.usertype *= t.usertype
");
}


# Date conversion routines.

# Convert from Unix ctime(3) format:
sub _datectime {
    my $date = shift;
    my @f;

    @f = split(' ', $date);
    $date = "$f[1] $f[2] $f[4] $f[3]";
}
# Convert from the Sybase datetime convert() formats:
sub _date101 {
    my $date = shift;
    my @f;

    @f = split(/\//, $date);
    $date = "$f[2]$f[0]$f[1]";
}
sub _date103 {
    my $date = shift;
    my @f;

    @f = split(/\//, $date);
    $date = "$f[2]$f[1]$f[0]";
}
sub _date104 {
    my $date = shift;
    my @f;

    @f = split(/\./, $date);
    $date = "$f[2]$f[1]$f[0]";
}
sub _date105 {
    my $date = shift;
    my @f;

    @f = split(/\-/, $date);
    $date = "$f[2]$f[1]$f[0]";
}
sub _date106 {
    my $date = shift;
    my @f;

    @f = split(' ', $date);
    $date = "$f[1] $f[0] $f[2]";
}
sub _date110 {
    my $date = shift;
    my @f;

    @f = split(/\-/, $date);
    $date = "$f[2]$f[0]$f[1]";
}
sub _date111 {
    my $date = shift;

    $date =~ s/\///g;
}



1;

    
