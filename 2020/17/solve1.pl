#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures experimental::smartmatch);

use List::Util qw(uniq);

chomp(my @input = <STDIN>);
@input = map { [split(//, $_)] } @input;

my %state;
for my $y (0 .. scalar @input - 1) {
    for my $x (0 .. scalar $input[0]->@* - 1) {
        $state{"$x;$y;0"} = 1 if $input[$y]->[$x] eq '#';
    }
}

sub cartesian($first = undef, @rest) {
    return ([]) unless defined($first);

    my @rest_product = cartesian(@rest);
    my @result;
    for my $e (@$first) {
        push(@result, map { [$e, @$_] } @rest_product);
    }
    return @result;
}

sub add($p, $q) {
    return [ $p->[0]+$q->[0], $p->[1]+$q->[1], $p->[2]+$q->[2] ];
}

my @neighbor_diffs = grep { ! ($_ ~~ [0, 0, 0]) } cartesian([-1, 0, 1], [-1, 0, 1], [-1, 0, 1]);

sub neighbors($addr) {
    my @coords = split(/;/, $addr);
    return map { join(';', add(\@coords, $_)->@*) } @neighbor_diffs;
}

sub iterate($old_state) {
    my %new_state;

    my %neighbor_counts;
    for my $entry (map { neighbors($_) } keys %$old_state) {
        $neighbor_counts{$entry} += 1;
    }


    for my $entry (uniq(keys %neighbor_counts, keys %$old_state)) {
        if ( ( $old_state->{$entry} && $neighbor_counts{$entry} ~~ [2,3]) ||
             (!$old_state->{$entry} && $neighbor_counts{$entry} == 3)) {
            $new_state{$entry} = 1;
        }
    }

    return %new_state;
}

%state = iterate(\%state) for (1..6);

say scalar %state;
