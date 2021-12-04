#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

use File::Temp qw(tempdir);
use IPC::System::Simple qw(systemx);
use Mojo::File;

chomp(my $input = do { local $/; <STDIN> });

my ($grammar, $strings) = split(/\n\s*\n/, $input);

$grammar =~ s/(\d+)/rule$1/g;
$grammar =~ s/:/ :=/g;
$grammar =~ s#"([ab])"#~/$1/#g;

$grammar =~ s/rule8 := rule42/rule8 := rule42+/;

my $rule11rhs = join(" | ", map { "rule42{$_} rule31{$_}" } (1..5));
$grammar =~ s/rule11 := rule42 rule31/rule11 := $rule11rhs/;

my $header = <<'EOH';
start: lines

lines := (rule0 "1" /\n/ | ~/[^\n]*\n/)*
EOH

$strings .= "\n" unless $strings =~ m/\n$/;

my $tempdir = tempdir(CLEANUP => 1, TEMPLATE => '/tmp/AoC_XXXXXXXX');
Mojo::File->new("$tempdir/part2.kex")->spurt($header.$grammar);
Mojo::File->new("$tempdir/lines.txt")->spurt($strings);


chdir($tempdir);
systemx(qw(kexc compile part2.kex --out part2));

my @result = qx(cat lines.txt | ./part2);
say scalar @result;
