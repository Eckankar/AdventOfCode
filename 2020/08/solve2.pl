#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my @input = <STDIN>);

sub run(@program) {
    my $pc = 0;
    my $acc = 0;
    my %seen;

    while ($pc <= $#program) {
        last if $seen{$pc}++;

        my ($op, $val) = $program[$pc] =~ m/^(nop|acc|jmp) \+?(-?\d+)$/;

        if    ($op eq 'acc') { $pc += 1;    $acc += $val; }
        elsif ($op eq 'jmp') { $pc += $val;               }
        elsif ($op eq 'nop') { $pc += 1;                  }
    }

    return { pc => $pc, acc => $acc };
}

for my $i (0 .. $#input) {
    my $line = $input[$i];
    my $result;

    if ($line =~ m/^nop/) {
        $result = run(@input[0 .. $i-1], $line =~ s/nop/jmp/r, @input[$i+1 .. $#input]);
    } elsif ($line =~ m/^jmp/) {
        $result = run(@input[0 .. $i-1], $line =~ s/jmp/nop/r, @input[$i+1 .. $#input]);
    }

    if ($result && $result->{pc} == $#input + 1) {
        say $result->{acc};
        exit(0);
    }

}
