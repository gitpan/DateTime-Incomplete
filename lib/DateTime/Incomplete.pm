package DateTime::Incomplete;

use strict;

use DateTime::Event::Recurrence;
use Params::Validate qw( validate );

use vars qw( $VERSION );

my $UNDEF_CHAR;
my ( @FIELDS, %FIELD_LENGTH );

BEGIN
{
    $VERSION = '0.00_07';

    $UNDEF_CHAR = 'x';

    @FIELDS = ( year => 0, month => 1, day => 1, 
                hour => 0, minute => 0, second => 0, nanosecond => 0 );
    %FIELD_LENGTH = ( 
                year => 4, month => 2, day => 2, 
                hour => 2, minute => 2, second => 2, nanosecond => 9,
                time_zone => 0, locale => 0 );

    # Generate named accessors

    for my $field ( keys %FIELD_LENGTH )
    {
	no strict 'refs';
	*{$field} = sub { $_[0]->_get($field) };
	*{"has_$field"} = sub { $_[0]->_has($field) };

        next if $field eq 'nanosecond';

	my $length = $FIELD_LENGTH{$field};

	next unless $length;

	*{"_$field"} = sub { defined $_[0]->$field() ?
			     sprintf( "%0.${length}d", $_[0]->$field() ) :
			     $UNDEF_CHAR x $length };
    }

    # Generate DateTime read-only functions
    for my $meth ( qw/
        week week_year week_number week_of_month
        day_name day_abbr 
        day_of_week wday dow
        day_of_year doy
        quarter day_of_quarter doq
        weekday_of_month
        jd mjd
        / )
    {
	no strict 'refs';
	*{$meth} = sub { $_[0]->_datetime_method( $meth, 'year', 'month', 'day' ) };
    }

    for my $meth ( qw/
        is_leap_year ce_year era year_with_era
        / )
    {
	no strict 'refs';
	*{$meth} = sub { $_[0]->_datetime_method( $meth, 'year' ) };
    }

    for my $meth ( qw/
        month_name month_abbr
        / )
    {
	no strict 'refs';
	*{$meth} = sub { $_[0]->_datetime_method( $meth, 'month' ) };
    }

    for my $meth ( qw/
        hour_1 hour_12 hour_12_0
        / )
    {
	no strict 'refs';
	*{$meth} = sub { $_[0]->_datetime_method( $meth, 'hour' ) };
    }

    for my $meth ( qw/
        millisecond microsecond
        / )
    {
	no strict 'refs';
	*{$meth} = sub { $_[0]->_datetime_method( $meth, 'nanosecond' ) };
    }
}

sub _nanosecond {
    defined $_[0]->nanosecond ? 
    $_[0]->nanosecond :
    $UNDEF_CHAR x 9
}

*mon = \&month;
*day_of_month = \&day;
*mday = \&day;
*min = \&minute;
*sec = \&second;

# Internal sub to call "DateTime" methods
sub _datetime_method
{
    my ( $self, $method ) = ( shift, shift );
    my @fields = @_;   # list of required fields
    my $date;
    for ( @fields )
    {
        return undef unless ( $self->_has($_) )
    }
    my %param; 

    # if we don't need 'year', then we can safely set it to whatever.
    $param{year} = 1970 if ! @fields || $fields[0] ne 'year';

    $param{locale} = $self->locale if $self->has_locale;
    $param{time_zone} = $self->time_zone if $self->has_time_zone;
    $param{$_} = $self->$_ for @fields;
    $date = DateTime->new( %param );
    
    return $date->$method;
}

sub fractional_second {
    $_[0]->_datetime_method( 'fractional_second', 'second', 'nanosecond' );
}

sub offset {
    $_[0]->_datetime_method( 'offset' );
}
sub time_zone_short_name {
    $_[0]->_datetime_method( 'time_zone_short_name' );
}
sub time_zone_long_name  {
    $_[0]->_datetime_method( 'time_zone_long_name' );
}

sub _from_datetime
{
    my $class = shift;
    my $dt = shift;
    my %param;
    $param{$_} = $dt->$_ for ( keys %FIELD_LENGTH );
    return $class->new( %param );
}

sub last_day_of_month {
    return (shift)->_from_datetime( DateTime->last_day_of_month(@_) );
}
sub from_epoch {
    return (shift)->_from_datetime( DateTime->from_epoch( @_ ) );
}
sub now {
    return (shift)->_from_datetime( DateTime->now( @_ ) );
}
sub from_object {
    return (shift)->_from_datetime( DateTime->from_object( @_ ) );
}
sub from_day_of_year {
    return (shift)->_from_datetime( DateTime->from_day_of_year( @_ ) );
}

sub today
{
    my $class = shift;
    my $now = DateTime->now( @_ );
    my %param;
    my %fields = ( %FIELD_LENGTH );
    delete $fields{$_} for ( qw/ hour minute second nanosecond / );
    $param{$_} = $now->$_ for ( keys %fields );
    return $class->new( %param );
}

# DATETIME-LIKE METHODS

sub new 
{
    # parameter checking is done in "set" method.
    my $class = shift;
    my %param = @_;
    my $base = delete $param{base};
    die "base must be a datetime" if defined $base && 
                             ! UNIVERSAL::can( $base, 'utc_rd_values' );
    my $self = bless { 
        has => \%param,
    }, $class;
    $self->set_base( $base );
    $self->set_locale( $self->{has}{locale} ) if $self->{has}{locale};
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

sub has_base
{
    return defined $_[0]->{base} ? 1 : 0;
}

sub set
{
    my $self = shift;
    my %p = @_;

    while ( my ( $k, $v ) = each %p )
    {
	if ( $k eq 'locale' )
	{
	    $self->set_locale($v);
	}

	$self->{base}->set( $k => $v ) if $self->{base} && defined $v;

	$self->{has}{ $k } = $v;
    }
}

sub _get
{
    $_[0]->{has}{$_[1]};
}

sub _has
{
    defined $_[0]->{has}{$_[1]} ? 1 : 0;
}

sub set_time_zone
{
    die "set_time_zone() requires a time_zone value" unless $#_ == 1;
    my $time_zone = $_[1];
    if ( defined $time_zone )
    {
        # $time_zone = DateTime::TimeZone->load( $time_zone ) unless ref $time_zone;
        $_[0]->{base}->set_time_zone( $time_zone ) if defined $_[0]->{base};
    }
    $_[0]->{has}{time_zone} = $time_zone;
}

sub set_locale
{
    die "set_locale() requires a locale value" unless $#_ == 1;
    my $locale = $_[1];
    if ( defined $locale )
    {
        $locale = DateTime::Locale->load( $locale ) unless ref $locale;
        $_[0]->{base}->set( locale => $locale ) if defined $_[0]->{base};
    }
    $_[0]->{has}{locale} = $locale;
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


sub truncate
{
    my $self = shift;
    my %p = validate( @_,
                      { to =>
                        { regex => qr/^(?:year|month|day|hour|minute|second)$/ },
                      },
                    );

    my @fields = @FIELDS;
    my $field;
    my $value;
    my $set = 0;
    while ( @fields )
    {
        ( $field, $value ) = ( shift @fields, shift @fields );
        $self->set( $field => $value ) if $set;
        $set = 1 if $p{to} eq $field;
    }
    return $self;
}


# Stringification methods

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


# "strftime"

# Copied from DateTime - we can't import %formats 
# because it is a local variable.
#
# The changes made here may be imported back to DateTime in a future version.
# Changes are marked with " # <--- change ";
# _format_nanosecs was rewritten.
# Added method: _am_pm

my %formats =
    ( 'a' => sub { $_[0]->day_abbr },
      'A' => sub { $_[0]->day_name },
      'b' => sub { $_[0]->month_abbr },
      'B' => sub { $_[0]->month_name },
      'c' => sub { $_[0]->has_locale ?
                   $_[0]->strftime( $_[0]->locale->default_datetime_format ) :
                   $_[0]->datetime }, # <--- change
      'C' => sub { int( $_[0]->year / 100 ) },
      'd' => sub { sprintf( '%02d', $_[0]->day_of_month ) },
      'D' => sub { $_[0]->strftime( '%m/%d/%y' ) },
      'e' => sub { sprintf( '%2d', $_[0]->day_of_month ) },
      'F' => sub { $_[0]->ymd('-') },
      'g' => sub { substr( $_[0]->week_year, -2 ) },
      'G' => sub { $_[0]->week_year },
      'H' => sub { $_[0]->_hour },    # <--- change
      'I' => sub { sprintf( '%02d', $_[0]->hour_12 ) },
      'j' => sub { $_[0]->day_of_year },
      'k' => sub { $_[0]->_hour },    # <--- change
      'l' => sub { sprintf( '%2d', $_[0]->hour_12 ) },
      'm' => sub { $_[0]->_month },   # <--- change
      'M' => sub { $_[0]->_minute },  # <--- change
      'n' => sub { "\n" }, # should this be OS-sensitive?
      'N' => sub { (shift)->_format_nanosecs( @_ ) },     # <--- change
      'p' => sub { $_[0]->_am_pm( $_[0] ) },              # <--- change
      'P' => sub { lc $_[0]->_am_pm( $_[0] ) },           # <--- change
      'r' => sub { $_[0]->strftime( '%I:%M:%S %p' ) },
      'R' => sub { $_[0]->strftime( '%H:%M' ) },
      's' => sub { $_[0]->_epoch },   # <--- change
      'S' => sub { $_[0]->_second },  # <--- change
      't' => sub { "\t" },
      'T' => sub { $_[0]->strftime( '%H:%M:%S' ) },
      'u' => sub { $_[0]->day_of_week },
      # algorithm from Date::Format::wkyr
      'U' => sub { my $dow = $_[0]->day_of_week;
                   $dow = 0 if $dow == 7; # convert to 0-6, Sun-Sat
                   my $doy = $_[0]->day_of_year - 1;
                   return int( ( $doy - $dow + 13 ) / 7 - 1 )
                 },
      'w' => sub { my $dow = $_[0]->day_of_week;
                   return $dow % 7;
                 },
      'W' => sub { my $dow = $_[0]->day_of_week;
                   my $doy = $_[0]->day_of_year - 1;
                   return int( ( $doy - $dow + 13 ) / 7 - 1 )
                 },
      'x' => sub { $_[0]->has_locale ? 
                   $_[0]->strftime( $_[0]->locale->default_date_format ) :
                   $_[0]->date }, # <--- change
      'X' => sub { $_[0]->locale ?
                   $_[0]->strftime( $_[0]->locale->default_time_format ) :
                   $_[0]->time }, # <--- change
      'y' => sub { sprintf( '%02d', substr( $_[0]->year, -2 ) ) },
      'Y' => sub { $_[0]->_year },    # <--- change
      'z' => sub { DateTime::TimeZone::offset_as_string( $_[0]->offset ) },
      'Z' => sub { $_[0]->time_zone_short_name },      # <--- change
      '%' => sub { '%' },
    );

$formats{h} = $formats{b};

sub epoch {
    die "not implemented";
}

sub _epoch {
    return $UNDEF_CHAR x 6
}

sub _am_pm { 
  defined $_[0]->locale ?
  $_[0]->locale->am_pm( $_[0] ) :
  $UNDEF_CHAR x 2
}

sub _format_nanosecs
{
    my $self = shift;
    my $precision = shift || 9;

    # rd_nanosecs might contain a fractional separator
    my ( $ret, $frac ) = split /[.,]/, $self->_nanosecond;
    $ret = sprintf "09d" => $ret unless length( $ret ) == 9;
    $ret .= $frac if $frac;

    return substr( $ret, 0, $precision );
}

sub strftime
{
    my $self = shift;
    # make a copy or caller's scalars get munged
    my @formats = @_;

    my @r;
    foreach my $f (@formats)
    {
        $f =~ s/
                %{(\w+)}
               /
                $self->$1() if $self->can($1);
               /sgex;

        # regex from Date::Format - thanks Graham!
       $f =~ s/
                %([%a-zA-Z])
               /
                $formats{$1} ? $formats{$1}->($self) : $1
               /sgex;

        # %3N
        $f =~ s/
                %(\d+)N
               /
                $formats{N}->($self, $1)
               /sgex;

        return $f unless wantarray;

        push @r, $f;
    }

    return @r;
}

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
        if ( $key eq 'locale' )
        {
            $result->set_locale( $value );
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
        if ( $key eq 'time_zone' ||
             $key eq 'locale' )
        {
            # TODO! - time_zone and locale are ignored.
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

    REDO: for (1..10) 
    {
        # warn "next: self ".$self->datetime." base ".$result->datetime;

        my @fields = @FIELDS;
        my ( $field, $overflow, $bigger_field );
        while ( @fields ) 
        {
            ( $field, undef ) = ( shift @fields, shift @fields );
            if ( defined $self->$field )
            {
                $overflow = ( $self->$field < $result->$field );
                return undef if $overflow && $field eq $FIELDS[0];

                if ( $self->$field != $result->$field )
                {
                    eval { $result->set( $field => $self->$field ) };
                    if ( $@ ) 
                    {
                        $result->set( @fields );
                        eval { $result->set( $field => $self->$field ) };
                        if ( $@ )
                        {
                            $overflow = 1;
                        }
                    }

                    if ( $overflow ) 
                    {
                        $result->add( $bigger_field . 's' => 1 );
                        next REDO; 
                    }
                    else
                    {
                        $result->set( @fields );
                    }
                }
            }
            $bigger_field = $field;
        }
        return $result;
    }
    return undef;
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

    my ( $field, $value, $overflow, $bigger_field );

    REDO: for (1..10)
    {
        my @fields = @FIELDS;
        while ( @fields ) 
        {
            ( $field, $value ) = ( shift @fields, shift @fields );
            if ( defined $self->$field )
            {
                $overflow = ( $self->$field > $result->$field );
                return undef if $overflow && $field eq $FIELDS[0];

                if ( $self->$field != $result->$field )
                {
                    if ( $overflow )
                    {
                        $result->set( $field => $value, @fields );
                        $result->subtract( nanoseconds => 1 );
                        next REDO;
                    }
                    my $diff = $result->$field - $self->$field ;
                    $diff--;
                    $result->subtract( $field  . 's' => $diff );
                    $result->set( @fields );
                    $result->subtract( nanoseconds => 1 );
                    if ( $result->$field != $self->$field )
                    {
                        $result->set( @fields );
                        $result->subtract( nanoseconds => 1 );
                    } 
                }
            }
            $bigger_field = $field;
        }
        return $result;
    }
    return undef;
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

    return $dt1 unless defined $dt2;
    return $dt2 unless defined $dt1;

    # warn "self ".$self->datetime." base ".$base->datetime;
    # warn "dt1 ".$dt1->datetime." dt2 ".$dt2->datetime;

    my $delta = $base - $dt1;
    return $dt1 if ( $dt2 - $delta ) >= $base;
    return $dt2;
}

sub to_recurrence
{
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

    my $r = DateTime::Event::Recurrence->yearly( %param );
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
accepts a "time_zone", a "locale" parameter,
and a "base" parameter.

The "base" parameter is used as a default base datetime 
in the "to_datetime" method. It is also used for validating
inputs to the "set" method. 
There is no default "base".

C<new> without parameters creates a completely undefined datetime:

  my $dti = DateTime::Incomplete->new();

The parameters can be explicitly undefined:

  my $dti = DateTime::Incomplete->new( year => 2003, day => undef );


=item * set

Use this to define or undefine a datetime field:

  $dti->set( month => 12 );
  $dti->set( day => 24 );
  $dti->set( day => undef );

It accepts the same arguments as the C<set()> method in
C<DateTime.pm>.

=item * clone

Creates a new object with the same datetime.


=item * set_time_zone

This method accepts either a time zone object or a string that can be
passed as the "name" parameter to C<< DateTime::TimeZone->new() >>.

Incomplete dates don't know the "local time" concept:
If the new time zone's offset is different from the 
previous time zone,
no local time adjust is made.


=item * year, month, day, hour, minute, second, nanosecond

Return the field value, or C<undef>.

These values can also be accessed using the methods:
mon, day_of_month, mday, min, sec.

=item * has_year, has_month, has_day, has_hour, has_minute, has_second, has_nanosecond, has_time_zone, has_locale

Returns 1 if the value is defined; otherwise it returns 0.


=item * time_zone

This returns the C<DateTime::TimeZone> object for the datetime object,
or C<undef>.


=item * locale

This returns the C<DateTime::Locale> object for the datetime object,
or C<undef>.


=item * datetime, ymd, date, hms, time, iso8601, mdy, dmy

These are equivalent to DateTime stringification methods with the same name, 
except that the undefined fields are replaced by 'xx' or 'xxxx'.


=item * strftime( $format, ... )

This method implements functionality similar to the C<strftime()>
method in C.  However, if given multiple format strings, then it will
return multiple scalars, one for each format string.

See the C<strftime Specifiers> section in C<DateTime> documentation
for a list of all possible format specifiers.

Undefined fields are replaced by 'xx' or 'xxxx'.

The specification C<%s> (epoch) always returns C<xxxxxx>.

=item * week week_year week_number week_of_month day_name day_abbr day_of_week wday dow day_of_year doy quarter day_of_quarter doq weekday_of_month jd mjd is_leap_year ce_year era year_with_era last_day_of_month month_name month_abbr hour_1 hour_12 hour_12_0 fractional_second millisecond microsecond offset time_zone_short_name time_zone_long_name

These are equivalent to DateTime methods with the same name,
except that they will return C<undef> if there is not enough data to compute
the result.

For example, C<is_leap_year> returns C<undef> if there is no year.


=item * is_finite, is_infinite

These methods allow you to distinguish normal datetime objects from
infinite ones.  Infinite datetime objects are documented in
L<DateTime::Infinite|DateTime::Infinite>.

Incomplete dates are not "Infinite".


=item * truncate( to => ... )

This method allows you to define some of the components in
the object to their "zero" values.  The "to" parameter is used to
specify which values to truncate, and it may be one of "year",
"month", "day", "hour", "minute", or "second".  For example, if
"month" is specified, then the day becomes 1, and the hour,
minute, and second all become 0.


=item * from_day_of_year( ... )

This constructor takes the same arguments as can be given to the
C<new()> method, except that it does not accept a "month" or "day"
argument.  Instead, it requires both "year" and "day_of_year".  The
day of year must be between 1 and 366, and 366 is only allowed for
leap years.

It creates a C<DateTime::Incomplete> object with all fields defined.


=item * from_object( object => $object, ... )

This class method can be used to construct a new 
C<DateTime::Incomplete> object from
any object that implements the C<utc_rd_values()> method.  All
C<DateTime::Calendar> modules must implement this method in order to
provide cross-calendar compatibility.  This method accepts a
"locale" parameter.

If the object passed to this method has a C<time_zone()> method, that
is used to set the time zone.  Otherwise UTC is used.

It creates a C<DateTime::Incomplete> object with all fields defined.


=item * from_epoch( ... )

This class method can be used to construct a new 
C<DateTime::Incomplete> object from
an epoch time instead of components.  Just as with the C<new()>
method, it accepts "time_zone" and "locale" parameters.

If the epoch value is not an integer, the part after the decimal will
be converted to nanoseconds.  This is done in order to be compatible
with C<Time::HiRes>.

It creates a C<DateTime::Incomplete> object with all fields defined.


=item * now( ... )

This class method is equivalent to C<DateTime->now>.

It creates a C<DateTime::Incomplete> object with all fields defined.


=item * today( ... )

This class method is equivalent to C<now>, but it leaves
hour, minute, second and nanosecond undefined.


=back


=head1 "DATETIME::INCOMPLETE" METHODS

=over 4

=item * set_base

  $dti->set_base( $dt );

The "base" parameter is used as a default base datetime 
in the "to_datetime" method. It is also used for validating
inputs to the "set" method.

The base object must use the year/month/day system. 
Most calendars use this system: Gregorian (C<DateTime>),
Julian, and others.


=item * base

Returns the C<base> datetime value, or C<undef>.


=item * has_base

Returns 1 if the C<base> value is defined; otherwise it returns 0.


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
  $dtset1 = $dti->to_recurrence;   
  # Christmas recurrence, with _seconds_ resolution

  $dti->set( hour => 0, minute => 0, second => 0, nanosecond => 0 );
  $dtset2 = $dti->to_recurrence;   
  # Christmas recurrence, with days resolution (hour/min/sec = 00:00:00)

This method generates the set of all possible datetimes
that fit into an incomplete datetime definition.

Those recurrences are DateTime::Set objects:

  $dt_next_xmas = $dti->to_recurrence->next( DateTime->today );

Recurrence generation has only been tested with
Gregorian dates so far.

Incomplete dates that have the I<year> defined will
generate finite sets. This kind of sets can take a lot of 
resources (RAM and CPU). 
The following datetime would generate the set of I<all seconds> in 2003:

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
value of a given day is 23:59:59.999999999 (for non leapsecond days).


=back


=head1 DIFFERENCES BETWEEN "DATETIME" AND "DATETIME::INCOMPLETE"

These methods are not implemented in C<DateTime::Incomplete>.
Some may be implemented in next versions:

  epoch
  hires_epoch
  is_dst
  utc_rd_values
  utc_rd_as_seconds
  local_rd_as_seconds

  add_duration, add, subtract_duration, subtract, subtract_datetime

  DefaultLanguage
  compare
  compare_ignore_floating

There are no "Storable" class hooks.


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

