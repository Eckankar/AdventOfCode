#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my @input = <STDIN>);

my @seatids;

for my $location (@input) {
    my ($row, $seat) = $location =~ m/^([FB]{7})([LR]{3})$/;
    $row  =~ tr/FB/01/;
    $seat =~ tr/LR/01/;

    $row = oct("0b$row");
    $seat = oct("0b$seat");

    my $seatid = $row * 8 + $seat;
    push(@seatids, $seatid);
}

my %taken = map { $_ => 1 } @seatids;
my ($my_seatid) = map { $_ - 1 } grep { ! $taken{$_-1} && $taken{$_-2} } @seatids;

say $my_seatid;
