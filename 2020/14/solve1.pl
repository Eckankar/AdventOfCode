#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures experimental::smartmatch);
use List::Util qw(sum0);

chomp(my @input = <STDIN>);

my ($mask_zeroes, $mask_ones);
my %memory;

sub update_mask($mask) {
    $mask_zeroes = 2 ** 36 - 1;
    $mask_ones = 0;

    my $i = 36;
    for my $bit (split(//, $mask)) {
        $i -= 1;

        $mask_ones   ^= 2 ** $i if $bit eq '1';
        $mask_zeroes ^= 2 ** $i if $bit eq '0';
    }
}

update_mask( join('', 'X' x 36) );

for my $line (@input) {
    if (my ($mask) = $line =~ m/^mask = ([X01]{36})$/) {
        update_mask($mask);
    } elsif (my ($addr, $val) = $line =~ m/^mem\[(\d+)\] = (\d+)$/) {
        $val |= $mask_ones;
        $val &= $mask_zeroes;

        $memory{$addr} = $val;
    }
}

say sum0( values %memory );
