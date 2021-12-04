#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my @input = <STDIN>);

my $max_seatid = -1;

for my $location (@input) {
    my ($row, $seat) = $location =~ m/^([FB]{7})([LR]{3})$/;
    $row  =~ tr/FB/01/;
    $seat =~ tr/LR/01/;

    $row = oct("0b$row");
    $seat = oct("0b$seat");

    my $seatid = $row * 8 + $seat;
    $max_seatid = $seatid if $seatid > $max_seatid;
}

say $max_seatid;
