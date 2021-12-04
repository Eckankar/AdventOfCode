#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my @input = <STDIN>);

my $window_size = 25;

my @active = @input[0 .. $window_size-1];

my %active = map { $_ => 1 } @active;

for my $i ($window_size .. $#input) {
    my $val = $input[$i];
    my $valid = grep { $active{$val-$_} } @active;

    unless ($valid) {
        say $val;
        exit(0);
    }

    delete $active{ $input[$i - $window_size] };
    $active{$val} = 1;
    @active = (@active[1 .. $#active], $val);
}
