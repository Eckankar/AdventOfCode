#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my @input = <STDIN>);

sub add($p, $q) {
    return [ $p->[0] + $q->[0], $p->[1] + $q->[1] ];
}

my %flipped;
for my $line (@input) {
    my $pos = [0, 0];

    while (my ($move, $rest) = $line =~ m/^([sn]?[we])(.*)$/) {
        $line = $rest;

        if    ($move eq 'w')  { $pos = add($pos, [ -1,  0]); }
        elsif ($move eq 'e')  { $pos = add($pos, [  1,  0]); }
        elsif ($move eq 'nw') { $pos = add($pos, [  0, -1]); }
        elsif ($move eq 'ne') { $pos = add($pos, [  1, -1]); }
        elsif ($move eq 'sw') { $pos = add($pos, [ -1,  1]); }
        elsif ($move eq 'se') { $pos = add($pos, [  0,  1]); }
    }

    my $pos_str = join(';', @$pos);
    if ($flipped{$pos_str}) {
        delete $flipped{$pos_str};
    } else {
        $flipped{$pos_str} = 1;
    }
}

say scalar %flipped;
