#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures experimental::smartmatch);
use List::Util qw(sum0);

chomp(my @input = <STDIN>);

my ($mask_ones, @floating);
my %memory;

sub update_mask($mask) {
    $mask_ones = 0;
    @floating = ();

    my $i = 36;
    for my $bit (split(//, $mask)) {
        $i -= 1;

        $mask_ones ^= 2 ** $i if $bit eq '1';
        push(@floating, $i)   if $bit eq 'X';
    }
}

sub apply_floating($addr, $first = undef, @rest) {
    return $addr unless defined($first);

    my $zeroed = $addr & (2 ** 36 - 1 - 2 ** $first);
    my $oned   = $addr | (2 ** $first);

    return (
        apply_floating($zeroed, @rest),
        apply_floating($oned  , @rest),
    );
}

update_mask( join('', 'X' x 36) );

for my $line (@input) {
    if (my ($mask) = $line =~ m/^mask = ([X01]{36})$/) {
        update_mask($mask);
    } elsif (my ($addr, $val) = $line =~ m/^mem\[(\d+)\] = (\d+)$/) {
        $addr |= $mask_ones;

        my @addrs = apply_floating($addr, @floating);

        $memory{$_} = $val for @addrs;
    }
}

say sum0( values %memory );
