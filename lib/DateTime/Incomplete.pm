package DateTime::Incomplete;

use strict;
use vars qw($VERSION);

use vars qw( $UNDEF_CHAR $UNDEF2 $UNDEF4 );
use vars qw( $CAN_RECURRENCE $RECURRENCE_MODULE );

BEGIN
{
    $VERSION = '0.00_04';

    $UNDEF_CHAR = 'x';
    $UNDEF4 = $UNDEF_CHAR x 4;   # xxxx
    $UNDEF2 = $UNDEF_CHAR x 2;   # xx

    # to_recurrence() method requires a recurrence module.
    # otherwise, it is not required.
    $RECURRENCE_MODULE = 'DateTime::Event::Recurrence';
    $CAN_RECURRENCE = 0;
    eval "
        use $RECURRENCE_MODULE;
        \$CAN_RECURRENCE = 1;
    ";
}

# DATETIME-LIKE METHODS


sub new 
{
    # parameter checking is done in "set" method.
    my $class = shift;
    my %param = @_;

    # There is no point in accepting 'language', because there is no
    # set_language() method in DateTime (yet)
    die "Parameter 'language' is not supported" if exists $param{language};

    my $base = delete $param{base};
    die "base must be a datetime" if defined $base && 
                             ! UNIVERSAL::can( $base, 'utc_rd_values' );

    my $self = bless { 
        has => \%param,
    }, $class;

    $self->set_base( $base );

    return $self;
}

sub set_base 
{
    my $self = shift;
    $self->{base} = shift;
    if ( defined $self->{base} ) 
    {
        my ($key, $value);
        while (($key, $value) = each %{$self->{has}} ) {
            next unless defined $value;
            if ( $key eq 'time_zone' )
            {
                $self->{base}->set_time_zone( $value );
                next;
            }        
            $self->{base}->set( $key => $value );
        }
    }
}

sub base
{
    return undef unless defined $_[0]->{base};
    $_[0]->{base}->clone;
}

sub set
{
    # parameter checking is done in "base" class.
    die "set() requires a field name and a value" unless $#_ == 2;
    $_[0]->{base}->set( $_[1] => $_[2] ) if defined $_[0]->{base};
    $_[0]->{has}{ $_[1] } = $_[2];
}

sub get
{
    $_[0]->{has}{$_[1]};
}

sub has
{
    defined $_[0]->{has}{$_[1]} ? 1 : 0;
}

sub set_time_zone
{
    die "set() requires a time_zone value" unless $#_ == 1;
    $_[0]->{base}->set_time_zone( $_[1] ) if defined $_[0]->{base};
    $_[0]->{has}{time_zone} = $_[1];
}

sub clone 
{ 
    my $base;
    $base = $_[0]->{base}->clone if defined $_[0]->{base};
    bless { 
        has => { %{ $_[0]->{has} } }, 
        base => $base,
    }, 
    ref $_[0]; 
}

sub is_finite { 1 }
sub is_infinite { 0 }


for ( qw/ year month day hour minute second nanosecond time_zone / )
{
    eval " sub $_ { \$_[0]->get( '$_' ) } ";      # year()
    eval " sub has_$_ { \$_[0]->has( '$_' ) } ";  # has_year()
}


# Internal stringification methods.
# some of these methods are not used, but they are defined just in case.
# TODO: generate these subs using a hash
sub _year       { defined $_[0]->year   ? sprintf( "%0.4d", $_[0]->year )   : $UNDEF4 }
sub _month      { defined $_[0]->month  ? sprintf( "%0.2d", $_[0]->month )  : $UNDEF2 }
sub _day        { defined $_[0]->day    ? sprintf( "%0.2d", $_[0]->day )    : $UNDEF2 }
sub _hour       { defined $_[0]->hour   ? sprintf( "%0.2d", $_[0]->hour )   : $UNDEF2 }
sub _minute     { defined $_[0]->minute ? sprintf( "%0.2d", $_[0]->minute ) : $UNDEF2 }
sub _second     { defined $_[0]->second ? sprintf( "%0.2d", $_[0]->second ) : $UNDEF2 }
sub _nanosecond { defined $_[0]->nanosecond ? sprintf( "%0.9d", $_[0]->nanosecond ) : $UNDEF_CHAR x 9 }

sub ymd
{
    my ( $self, $sep ) = ( @_, '-' );
    return $self->_year . $sep. $self->_month . $sep . $self->_day;
}
*date = \&ymd;

sub mdy
{
    my ( $self, $sep ) = ( @_, '-' );
    return $self->_month . $sep. $self->_day . $sep . $self->_year;
}

sub dmy
{
    my ( $self, $sep ) = ( @_, '-' );
    return $self->_day . $sep. $self->_month . $sep . $self->_year;
}

sub hms
{
    my ( $self, $sep ) = ( @_, ':' );
    return $self->_hour . $sep. $self->_minute . $sep . $self->_second;
}
# don't want to override CORE::time()
*DateTime::Incomplete::time = \&hms;

sub iso8601 { join 'T', $_[0]->ymd('-'), $_[0]->hms(':') }
*datetime = \&iso8601;


# DATETIME::INCOMPLETE METHODS


sub is_undef 
{
    for ( values %{$_[0]->{has}} )
    {
        return 0 if defined $_;
    }
    return 1;
}


sub to_datetime
{
    my $self = shift;
    my %param = @_;
    return $self->{base}->clone if defined $self->{base} &&
                                  ! exists $param{base};
    die "no base datetime" unless exists $param{base} && 
                                  UNIVERSAL::can( $param{base}, 'utc_rd_values' );

    my $result = $param{base}->clone;
    my ($key, $value);
    while (($key, $value) = each %{$self->{has}} ) {
        next unless defined $value;
        if ( $key eq 'time_zone' )
        {
            $result->set_time_zone( $value );
            next;
        }        
        $result->set( $key => $value );
    }
    return $result;
}


sub contains
{
    my $self = shift;
    my $dt = shift;
    die "no datetime" unless defined $dt && 
                             UNIVERSAL::can( $dt, 'utc_rd_values' );

    my ($key, $value);
    while (($key, $value) = each %{$self->{has}} ) {
        next unless defined $value;
        if ( $key eq 'time_zone' )
        {
            # TODO! - time_zone is ignored.
            next;
        }        
        return 0 unless $dt->$key == $value;
    }
    return 1;
}


sub next
{
    # returns 'next or equal'

    my $self = shift;
    my $base = shift;
    $base = $self->{base} if defined $self->{base} &&
                                  ! defined $base;
    die "no base datetime" unless defined $base && 
                                  UNIVERSAL::can( $base, 'utc_rd_values' );

    my $result = $base->clone;

    # warn "self ".$self->datetime." base ".$result->datetime;

    if ( defined $self->year )
    {
        return undef if $self->year < $result->year;
        if ( $self->year > $result->year )
        {
            # first date in year
            $result->set( year => $self->year, month => 1, day => 1, hour => 0, 
                          minute => 0, second => 0, nanosecond => 0 );
        }
    }
    if ( defined $self->month )
    {
        if ( $self->month < $result->month )
        {
            $result->set( month => $self->month );
            $result->add( years => 1 );
            return $self->next( $result );
        }
        if ( $self->month > $result->month )
        {
            $result->set( month => $self->month, day => 1, hour => 0, minute => 0, 
                          second => 0, nanosecond => 0 );
        }
    }
    if ( defined $self->day )
    {
        if ( $self->day < $result->day )
        {
            $result->set( day => $self->day );
            $result->add( months => 1 ); 
            return $self->next( $result );
        }
        if ( $self->day > $result->day )
        {
            $result->set( day => $self->day, hour => 0, minute => 0, second => 0, 
                          nanosecond => 0 );
        }
    }

    if ( defined $self->hour )
    {
        if ( $self->hour < $result->hour )
        {
            $result->set( hour => $self->hour );
            $result->add( days => 1 ); 
            return $self->next( $result );
        }
        if ( $self->hour > $result->hour )
        {
            $result->set( hour => $self->hour, minute => 0, second => 0, nanosecond => 0 );
        }
    }

    if ( defined $self->minute )
    {
        if ( $self->minute < $result->minute )
        {
            $result->set( minute => $self->minute );
            $result->add( hours => 1 ); 
            return $self->next( $result );
        }
        if ( $self->minute > $result->minute )
        {
            $result->set( minute => $self->minute, second => 0, nanosecond => 0 );
        }
    }

    if ( defined $self->second )
    {
        if ( $self->second < $result->second )
        {
            $result->set( second => $self->second );
            $result->add( minutes => 1 ); 
            return $self->next( $result );
        }
        if ( $self->second > $result->second )
        {
            $result->set( second => $self->second, nanosecond => 0 );
        }
    }

    if ( defined $self->nanosecond )
    {
        if ( $self->nanosecond < $result->nanosecond )
        {
            $result->set( nanosecond => $self->nanosecond );
            $result->add( seconds => 1 ); 
            return $self->next( $result );
        }
    }

    return $result;

}


sub previous
{
    # returns 'previous or equal'

    my $self = shift;
    my $base = shift;
    $base = $self->{base} if defined $self->{base} &&
                                  ! defined $base;
    die "no base datetime" unless defined $base && 
                                  UNIVERSAL::can( $base, 'utc_rd_values' );

    my $result = $base->clone;

    # warn "previous: self ".$self->datetime." base ".$result->datetime;

    my @fields = ( year => 0, month => 1, day => 1, hour => 0,
                   minute => 0, second => 0, nanosecond => 0 );
    my ( $field, $overflow, $bigger_field );
    while ( @fields ) 
    {
        ( $field, undef ) = ( shift @fields, shift @fields );
        if ( defined $self->$field )
        {
            $overflow = ( $self->$field > $result->$field );
            return undef if $overflow && $field eq 'year';

            if ( $self->$field != $result->$field )
            {
                $result->set( $field => $self->$field );
                $result->subtract( $bigger_field . 's' => 1 ) if $overflow;
                $result->add( $field . 's' => 1 );
                $result->set( @fields );
                $result->subtract( nanoseconds => 1 );
                return $self->previous( $result ) if $overflow;
            }
        }
        $bigger_field = $field;
    }
    return $result;
}

sub closest
{
    # returns 'closest datetime'

    my $self = shift;
    my $base = shift;
    $base = $self->{base} if defined $self->{base} &&
                                  ! defined $base;
    die "no base datetime" unless defined $base &&
                                  UNIVERSAL::can( $base, 'utc_rd_values' );

    my $dt1 = $self->previous( $base );
    my $dt2 = $self->next( $base );

    # warn "self ".$self->datetime." base ".$base->datetime;
    # warn "dt1 ".$dt1->datetime." dt2 ".$dt2->datetime;

    my $delta = $base - $dt1;
    return $dt1 if ( $dt2 - $delta ) >= $base;
    return $dt2;
}

sub to_recurrence
{
    die "to_recurrence() is not available because ".
        $RECURRENCE_MODULE . " is not installed" unless $CAN_RECURRENCE;

    my $self = shift;
    my %param;

    my $freq = '';
    my $year;
    for ( qw( second minute hour day month year ) )
    {
        my $by = $_ . 's';  # months, hours
        if ( exists $self->{has}{$_} && defined $self->{has}{$_} )
        {
            if ( $_ eq 'year' ) 
            {
                $year = $self->$_;
                next;
            }
            $param{$by} = [ $self->$_ ];
            next;
        }
        $freq = $_ unless $freq;
        # TODO: use a hash
        $param{$by} = [ 1 .. 12 ] if $_ eq 'month';
        $param{$by} = [ 1 .. 31 ] if $_ eq 'day';
        $param{$by} = [ 0 .. 23 ] if $_ eq 'hour';
        $param{$by} = [ 0 .. 59 ] if $_ eq 'minute';
        $param{$by} = [ 0 .. 59 ] if $_ eq 'second';
    }
    if ( $freq eq '' )
    {
        # it is a single date
        my $dt = DateTime->new( %{$self->{has}} );
        return DateTime::Set->from_datetimes( dates => [ $dt ] );
    }

    # for ( keys %param ) { print STDERR " param $_ = @{$param{$_}} \n"; }

    my $r = yearly $RECURRENCE_MODULE ( %param );
    if ( defined $year ) {
        my $span = DateTime::Span->from_datetimes( 
                       start => DateTime->new( year => $year ),
                       before => DateTime->new( year => $year + 1 ) );
        $r = $r->intersection( $span );
    }
    return $r;
}


1;

__END__

=head1 NAME

DateTime::Incomplete - The partial date & time thing


=head1 SYNOPSIS

  my $dti = DateTime::Incomplete->new( year => 2003 );
  # 2003-xx-xx
  $dti->set( month => 12 );
  # 2003-12-xx
  $dt = $dti->to_datetime( base => DateTime->now );
  # 2003-12-19T16:54:33


=head1 DESCRIPTION

DateTime::Incomplete is a class for representing partial
dates and times.

Such values are generated by expressions like '10:30',
'2003', and 'dec-14'.


=head1 "DATETIME" METHODS

=over 4

=item * new()

Creates a new incomplete date:

  my $dti = DateTime::Incomplete->new( year => 2003 );
  # 2003-xx-xx

This class method accepts parameters for each date and
time component: "year", "month", "day", "hour",
"minute", "second", "nanosecond".  Additionally, it
accepts a "time_zone" parameter and a "base" parameter.

The "base" parameter is used as a default base datetime 
in the "to_datetime" method. It is also used for validating
inputs to the "set" method. 
There is no default "base".

Note: There is no "language" or "locale" parameter.

C<new> without parameters creates a completely undefined datetime:

  my $dti = DateTime::Incomplete->new();

The parameters can be explicitly undefined:

  my $dti = DateTime::Incomplete->new( year => 2003, day => undef );


=item * set

Use this to define or undefine a datetime field:

  $dti->set( month => 12 );
  $dti->set( day => 24 );
  $dti->set( day => undef );


=item * clone

Creates a new object with the same datetime.


=item * set_time_zone

This method accepts either a time zone object or a string that can be
passed as the "name" parameter to C<< DateTime::TimeZone->new() >>.

Incomplete dates don't know the "local time" concept:
If the new time zone's offset is different from the old time zone,
no local time adjust is made.


=item * year, month, day, hour, minute, second, nanosecond

Return the field value, or C<undef>.


=item * has_year, has_month, has_day, has_hour, has_minute, has_second, has_nanosecond

Returns 1 if the value is defined; otherwise it returns 0.


=item * time_zone

This returns the C<DateTime::TimeZone> object for the datetime object,
or C<undef>.

=item * datetime, ymd, date, hms, time, iso8601, mdy, dmy

These are equivalent to DateTime stringification methods with the same name, 
except that undefined fields are replaced by 'xx' or 'xxxx'.


=item * is_finite, is_infinite

These methods allow you to distinguish normal datetime objects from
infinite ones.  Infinite datetime objects are documented in
L<DateTime::Infinite|DateTime::Infinite>.

Incomplete dates are not "Infinite".

=back


=head1 "DATETIME::INCOMPLETE" METHODS

=over 4

=item * get

  $kin = $dti->get( 'kin' );  # a Mayan time

Return the datetime field value, or C<undef>.


=item * has

  $isfrac = $dti->has( 'nanosecond' ); 

Returns 1 if the datetime field value is defined; otherwise it returns 0.


=item * set_base

  $dti->set_base( $dt );

The "base" parameter is used as a default base datetime 
in the "to_datetime" method. It is also used for validating
inputs to the "set" method.


=item * base

Returns the C<base> datetime value, or C<undef>.


=item * is_undef

Returns true if the datetime is completely undefined.


=item * to_datetime

  $dt = $dti->to_datetime( base => DateTime->now );

  $dti->set_base( DateTime->now );
  $dt = $dti->to_datetime;

Returns a "complete" datetime.

The resulting datetime is in the same Calendar as C<base>. 

The following example creates a Julian Calendar date, within
year 1534:

  $dtj = DateTime::Calendar::Julian->new( ... );
  $dti = DateTime::Incomplete->new( year => 1534 );
  $dtj2 = $dti->to_datetime( base => $dtj );
 
The resulting datetime can be either before of after the
C<base> datetime. No adjustments are made, besides setting
the missing fields.

This method may C<die> if it results in a datetime that
doesn't exist.


=item * to_recurrence

  $dti = DateTime::Incomplete->new( month => 12, day => 24 );
  $dtset= $dti->to_recurrence;   # Christmas day recurrence

This method generates the set of all possible datetimes
that fit into an incomplete datetime definition.

Those recurrences are DateTime::Set objects:

  $dt_next_xmas = $dti->to_recurrence->next( DateTime->today );

Recurrence generation has only been tested with
Gregorian dates so far.

This method will die if the
C<DateTime::Event::Recurrence> package is not installed.

Incomplete dates that have the I<year> defined will
generate finite sets. This kind of sets can take a lot of 
resources (RAM and CPU). 
The following datetime would generate the set of all seconds in 2003:

  2003-xx-xxTxx:xx:xx


=item * contains

  $bool = $dti->contains( $dt );

Returns a true value if the incomplete datetime range 
I<contains> a given datetime value.

Note: the incomplete-datetime time-zone value is ignored,
that is, the value of "local time" is compared.
This may lead to unexpected results if the time zone field is set.

For example:

  2003-xx-xx contains 2003-12-24
  2003-xx-xx does not contain 1999-12-14


=item * previous / next / closest

  $dt2 = $dti->next( $dt );

C<next> returns the first complete date I<after or equal> to the given datetime.

C<previous> returns the first complete date I<before or equal> to the given datetime.

C<closest> returns the closest complete date (previous or next) to the given datetime.

All of these methods return C<undef> if there is no
matching complete datetime.

If no datetime is given, these methods use the "base" datetime.

Note: The definition of C<previous> and C<next> is different from the
methods in C<DateTime::Set> class.

The datetimes are generated with 1 nanosecond precision. The last "time"
value of a given day is 23:59:59.999999999 (non leapsecond days).

=back


=head1 DIFFERENCES BETWEEN "DATETIME" AND "DATETIME::INCOMPLETE"

These methods are not implemented in C<DateTime::Incomplete>
(some will be implemented in next versions):

  from_epoch, epoch, hires_epoch
  now, today
  from_object
  last_day_of_month
  from_day_of_year
  ce_year, era, year_with_era
  month_name, month_abbr
  day_of_month, mday
  day_of_week, wday, dow
  day_name, day_abbr
  day_of_year, doy
  quarter, day_of_quarter, doq
  weekday_of_month
  hour_1, hour_12, hour_12_0
  min, sec
  fractional_second, millisecond, microsecond
  is_leap_year
  week, week_year, week_number, week_of_month
  jd, mjd
  offset, is_dst, time_zone_short_name
  strftime
  utc_rd_values, utc_rd_as_seconds, local_rd_as_seconds
  truncate
  add_duration, add, subtract_duration, subtract, subtract_datetime

There are no class methods. The following are not implemented:

  DefaultLanguage
  compare, compare_ignore_floating

There are no "Storable" class hooks.

The C<new> method doesn't have the 'language' parameter.


=head1 AUTHORS

Flavio S. Glock <fglock[at]cpan.org>

With
Ben Bennett <fiji[at]ayup.limey.net>, 
Claus Farber <claus[at]xn--frber-gra.muc.de>,
Dave Rolsky <autarch[at]urth.org>,
Eugene Van Der Pijll <pijll[at]gmx.net>,
and the DateTime team.


=head1 COPYRIGHT

Copyright (c) 2003 Flavio S. Glock.  All rights reserved.
This program is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

The full text of the license can be found in the LICENSE
file included with this module.


=head1 SEE ALSO

datetime@perl.org mailing list

http://datetime.perl.org/

=cut

