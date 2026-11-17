#!/usr/bin/perl
use strict; use warnings; no warnings 'recursion';

local $/;
my @toks = <> =~ /"(?:[^"\\]|\\.)*"|[()]|[^\s()"]+/g;

sub parse {
  my $t = shift @toks;
  return $t unless $t eq '(';
  my @l; push @l, parse() while $toks[0] ne ')'; shift @toks;
  return \@l;
}
sub unparse { my $l = shift; ref $l ? '(' . join(' ', map { unparse($_) } @$l) . ')' : $l }
my @decls = grep { !(ref && $_->[0] eq 'Dtype') } @{parse()};

my %arity;
sub walk {
  my $l = shift; return unless ref $l;
  for my $i (0 .. $#$l - 2) {
    next unless !ref $l->[$i] && $l->[$i] =~ /^(Con|Pcon)$/;
    my $c = $l->[$i + 1];
    next unless ref $c && $c->[0] eq 'SOME' && ref $c->[1] && $c->[1][0] eq 'Short';
    $arity{$c->[1][1]} = ref $l->[$i + 2] ? scalar @{$l->[$i + 2]} : 0;
  }
  walk($_) for @$l;
}
walk($_) for @decls;
my @ctors = sort keys %arity;

my $ty = '(Atapp nil (Short "pg_value"))';
my $dtype = '(Dtype (unk unk) ((nil "pg_value" '
  . join(' ', map { "($_" . (" $ty" x $arity{$_}) . ')' } @ctors) . ')))';

sub print_str { qq{(App Opapp ((Var (Long "TextIO" (Short "print"))) (Lit "$_[0]")))} }
sub seq { my $e = pop; $e = "(Let NONE $_ $e)" for reverse @_; $e }
sub show { qq{(App Opapp ((Var (Short "pg_show")) (Var (Short "$_[0]"))))} }
my @branches = map {
  my $c = $_; (my $name = $c) =~ s/^"(?:.*\.)?|"$//g;
  my @xs = map { "x$_" } 1 .. $arity{$c};
  my $body = @xs ? seq(print_str("($name"), (map { (print_str(' '), show($_)) } @xs), print_str(')'))
                 : print_str($name);
  "((Pcon (SOME (Short $c)) " . (@xs ? '(' . join(' ', map { "\"$_\"" } @xs) . ')' : 'nil') . ") . $body)"
} @ctors;
push @branches, '("v" . ' . print_str('<fun>') . ')';
my $printer = qq{(Dlet (unk unk) "pg_show" (Letrec (("pg_show" "x" Mat (Var (Short "x")) (@branches))) (Var (Short "pg_show"))))};

print '(', join(' ', $dtype, (map { unparse($_) } @decls), $printer, '(Dlet (unk unk) "pg_result" ' . show('main') . ')'), ")\n";
