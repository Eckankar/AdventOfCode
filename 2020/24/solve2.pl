#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures experimental::smartmatch);

use List::Util qw(uniq);

chomp(my @input = <STDIN>);

sub add($p, $q) {
    return [ $p->[0] + $q->[0], $p->[1] + $q->[1] ];
}

sub neighbor($pos, $move) {
    if    ($move eq 'w')  { $pos = add($pos, [ -1,  0]); }
    elsif ($move eq 'e')  { $pos = add($pos, [  1,  0]); }
    elsif ($move eq 'nw') { $pos = add($pos, [  0, -1]); }
    elsif ($move eq 'ne') { $pos = add($pos, [  1, -1]); }
    elsif ($move eq 'sw') { $pos = add($pos, [ -1,  1]); }
    elsif ($move eq 'se') { $pos = add($pos, [  0,  1]); }

    return $pos;
}

sub neighbors($pos) {
    return map { neighbor($pos, $_) } (qw(w e nw ne sw se));
}

sub flip($flipped, $pos) {
    my $pos_str = join(';', @$pos);
    if ($flipped->{$pos_str}) {
        delete $flipped->{$pos_str};
    } else {
        $flipped->{$pos_str} = 1;
    }
}

my %flipped;
for my $line (@input) {
    my $pos = [0, 0];

    while (my ($move, $rest) = $line =~ m/^([sn]?[we])(.*)$/) {
        $line = $rest;
        $pos = neighbor($pos, $move);
    }

    flip(\%flipped, $pos);
}

sub iterate($old_flipped) {
    my %flipped;

    my @relevant = uniq(map { join(";", @$_) } map { $_, neighbors($_) } map { [ split(/;/, $_) ] } keys %$old_flipped);

    for my $pos_str (@relevant) {
        my $pos = [ split(/;/, $pos_str) ];

        my @neighbors = map { join(";", @$_) } neighbors($pos);

        if ($old_flipped->{$pos_str}) {
            $flipped{$pos_str} = 1 if (grep { $old_flipped->{$_} } @neighbors) ~~ [1, 2];
        } else {
            $flipped{$pos_str} = 1 if (grep { $old_flipped->{$_} } @neighbors) == 2;
        }
    }

    return %flipped;
}

%flipped = iterate(\%flipped) for (1..100);

say scalar %flipped;

