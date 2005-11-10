package Getopt::Long::Descriptive;

use strict;
use Getopt::Long;
use List::Util qw(max first);
use Carp qw(croak);
use Params::Validate qw(:all);

=head1 NAME

Getopt::Long::Descriptive - Getopt::Long with usage text

=head1 VERSION

 0.01

=cut

our $VERSION = '0.01';

=head1 DESCRIPTION

Convenient wrapper for Getopt::Long and program usage output

=head1 SYNOPSIS

  use Getopt::Long::Descriptive;
  my ($opts, $usage) = describe_options($format, @opts, \%arg);

=head1 FORMAT

  $format = "usage: myprog %o myarg...";

C<%o> will be replaced with a list of the short options, as well as the text
"[long options...]" if any have been defined.

Because of this, any literal C<%> symbols will need to be written as C<%%>.

=head1 OPTIONS

Option specifications are the same as in Getopt::Long.  You should pass in an
array of arrayrefs whose first elements are option specs and whose second
elements are descriptions.

  my @opts = ([ "verbose|V" => "be noisy"       ],
              [ "logfile=s" => "file to log to" ]);

Option specifications may have a third hashref argument.  If
present, this configures extra restrictions on the value or
presence of that option.

=head2 Option Constraints

=head3 implies

  implies => 'bar'

  implies => [qw(foo bar)]

  implies => { foo => 1, bar => 2 }

=head3 required

=head3 Params::Validate

In addition, any constraint understood by Params::Validate
may be used.

(Internally, all constraints are translated into
Params::Validate options or callbacks.)

=head1 EXTRA ARGUMENTS

If the last parameter is a hashref, it contains extra arguments to modify the
way C<describe_options> works.  Valid arguments are:

  getopt_conf - an arrayref of strings, passed to Getopt::Long::Configure

=head1 EXPORTED FUNCTIONS

=head2 C<describe_options>

See SYNOPSIS; returns a hashref of option values and an
object that represents the usage statement.

The usage statement has several methods:

=over 4

=item * C<< $usage->text >> returns the usage string

=item * C<< $usage->warn >> prints usage to STDERR

=item * C<< $usage->die >> dies with the usage string

=back

=head2 C<:types>

Any of the Params::Validate type constants (C<SCALAR>, etc.)
can be imported as well.  You can get all of them at once by
importing C<:types>.

=head1 CONFIGURATION

=head2 C<$MungeOptions>

When C<$Getopt::Long::Descriptive::MungeOptions> is true, some munging is done
to make option names more hash-key friendly:

=over 4

=item * All keys are lowercased

=item * C<-> is changed to C<_>

=back

The default is a true value.

=head1 SEE ALSO

L<Getopt::Long>
L<Params::Validate>

=cut

BEGIN {
  require Exporter;
  our @ISA = qw(Exporter);
  our @EXPORT = qw(describe_options);
  our %EXPORT_TAGS = (
    types => $Params::Validate::EXPORT_TAGS{types},
  );
  our @EXPORT_OK = @{$EXPORT_TAGS{types}};
}

my %CONSTRAINT = (
  implies  => \&_mk_implies,
  required => { optional => 0 },
  only_one => \&_mk_only_one,
);

our $MungeOptions = 1;

sub describe_options {
  my $format = shift;
  my $arg    = (ref $_[-1] and ref $_[-1] eq 'HASH') ? pop @_ : {};
  my @opts   = map {
    {(
      spec       => $_->[0],
      desc       => $_->[1],
      constraint => $_->[2] || {},
      name       => _munge((split /\|/, $_->[0])[0]),
    )}
  } @_;
  
  my @go_conf = @{ $arg->{getopt_conf} || $arg->{getopt} || [] };
  if ($arg->{getopt}) {
    warn "describe_options: 'getopt' is deprecated, please use 'getopt_conf' instead\n";
  }

  push @go_conf, "bundling" unless grep { /bundling/i } @go_conf;
   
  my @specs = map { $_->{spec} } @opts;
  my $short = join "", sort {
    lc $a cmp lc $b
  } map {
    (my $s = $_) =~ s/[:=]\w+$//;
    grep /^.$/, split /\|/, $s
  } @specs;
  
  my $long = grep /\b[^|]{2,}/, @specs;

  my %replace = (
    "%" => "%",
    "o" => (join(" ",
                 ($short ? "[-$short]" : ()),
                 ($long  ? "[long options...]" : ())
               )),
  );

  (my $str = $format) =~ s/%(.)/$replace{$1}/ge;
  $str =~ s/\s{2,}/ /g;

  # a spec can grow up to 4 characters in usage output:
  # '-' on short option, ' ' between short and long, '--' on long
  my $length = max(map length(), @specs) + 4;

  my $usage = bless sub {
    my ($as_string) = @_;
    my ($out_fh, $buffer);
    my @tmpopts = @opts;
    if ($as_string) {
      require IO::Scalar;
      $out_fh = IO::Scalar->new( \$buffer );
    } else {
      $out_fh = \*STDERR;
    }

    print {$out_fh} "$str\n";

    while (@tmpopts) {
      my $opt  = shift @tmpopts;
      my $spec = $opt->{spec};
      my $desc = $opt->{desc};
      $spec =~ s/[:=]\w+[%@]?$//;
      $spec = join " ", reverse map { length > 1 ? "--$_" : "-$_" }
                                split /\|/, $spec;
      printf {$out_fh} "\t%-${length}s  %s\n", $spec, $desc;
    }

    return $buffer if $as_string;
  } => "Getopt::Long::Descriptive::Usage";

  Getopt::Long::Configure(@go_conf);

  my %return;
  $usage->die unless GetOptions(\%return, @specs);

  for my $opt (keys %return) {
    my $newopt = _munge($opt);
    next if $newopt eq $opt;
    $return{$newopt} = delete $return{$opt};
  }

  for my $copt (grep { $_->{constraint} } @opts) {
    my $name = $copt->{name};
    my $new  = _validate_with(
      name   => $name,
      params => \%return,
      spec   => $copt->{constraint},
      opts   => \@opts,
    );
    next unless (defined($new) || exists($return{$name}));
    $return{$name} = $new;
  }

  return \%return, $usage;
}

sub _munge {
  my ($opt) = @_;
  return $opt unless $MungeOptions;
  $opt = lc($opt);
  $opt =~ tr/-/_/;
  return $opt;
}

sub _validate_with {
  my (%arg) = validate(@_, {
    name   => 1,
    params => 1,
    spec   => 1,
    opts   => 1,
  });
  my $spec = $arg{spec};
  my %pvspec;
  for my $ct (keys %{$spec}) {
    if ($CONSTRAINT{$ct} and ref $CONSTRAINT{$ct} eq 'CODE') {
      $pvspec{callbacks} ||= {};
      $pvspec{callbacks} = {
        %{$pvspec{callbacks}},
        $CONSTRAINT{$ct}->(
          $arg{name},
          $spec->{$ct},
          $arg{params},
          $arg{opts},
        ),
      };
    } else {
      %pvspec = (
        %pvspec,
        $CONSTRAINT{$ct}
          ? %{$CONSTRAINT{$ct}}
            : ($ct => $spec->{$ct}),
      );
    }
  }

  unless (exists $pvspec{optional}) {
    $pvspec{optional} = 1;
  }

  # we need to implement 'default' by ourselves sometimes
  # because otherwise the implies won't be checked/executed
  # XXX this should be more generic -- we'll probably want
  # other callbacks to always run, too
  if (!defined($arg{params}{$arg{name}})
        && $pvspec{default}
          && $spec->{implies}) {

    $arg{params}{$arg{name}} = delete $pvspec{default};
  }

  #use Data::Dumper;
  #local $Data::Dumper::Terse = 1;
  #local $Data::Dumper::Indent = 0;
  #warn "pvspec = " . Dumper(\%pvspec);
  my %p = validate_with(
    params => [ %{$arg{params}} ],
    spec   => { $arg{name} => \%pvspec },
    allow_extra => 1,
  );
  return $p{$arg{name}};
}

# scalar:   single option = true
# arrayref: multiple options = true
# hashref:  single/multiple options = given values
sub _norm_imply {
  my ($what) = @_;
  return $what
    if ref $what eq 'HASH';

  return { map { $_ => 1 } @$what } 
    if ref $what eq 'ARRAY';

  return { $what => 1 }
    if not ref $what;

  die "can't imply: $what";
}

sub _mk_implies {
  my $name = shift;
  my $what = _norm_imply(shift);
  my $param = shift;
  my $opts  = shift;
  for my $implied (keys %$what) {
    first { $_->{name} eq $implied } @$opts
      or die("option specification for $name implies nonexistent option $implied\n");
  }
  my $whatstr = join(
    ", ", 
    map { "$_=$what->{$_}" }
      keys %$what);
  return "$name implies $whatstr" => sub {
    my ($val) = shift || return 1; 
    while (my ($key, $val) = each %$what) {
      if (exists $param->{$key} and $param->{$key} ne $val) {
        die("option specification for $name implies that $key should be set to $val, "
              . "but it is $param->{$key} already\n");
      }
      $param->{$key} = $val;
    }
    return 1;
  };
}

sub _mk_only_one {
  die "unimplemented";
}

package Getopt::Long::Descriptive::Usage;

use strict;

sub text { shift->(1) }

sub warn { shift->() }

sub die  { die shift->text }

use overload (
  q{""} => "text",
);

=head1 AUTHOR

Hans Dieter Pearcey, C<< <hdp@cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-getopt-long-descriptive@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Getopt-Long-Descriptive>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 COPYRIGHT & LICENSE

Copyright 2005 Hans Dieter Pearcey, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1; # End of Getopt::Long::Descriptive
