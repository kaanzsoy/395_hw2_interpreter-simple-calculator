#!/usr/bin/perl
use strict;
use warnings;
use Readonly;

# token types
Readonly my $NUMBER => 'NUMBER';
Readonly my $PLUS => 'PLUS';
Readonly my $MINUS => 'MINUS';
Readonly my $MULTIPLY => 'MULTIPLY';
Readonly my $DIVIDE => 'DIVIDE';
Readonly my $POWER => 'POWER';
Readonly my $LPAREN => 'LPAREN';
Readonly my $RPAREN => 'RPAREN';
Readonly my $EOF => 'EOF';

# represents a single unit in the input (like a number, operator, parenthesis..)
package Token {
    sub new {
        my ($class, $type, $value) = @_;
        my $self = {
            type => $type,
            value => $value
        };
        bless $self, $class;
        return $self;
    }
}

# evaluates the final parsed expression tree
package Interpreter {
    sub new {
        my ($class) = @_;
        my $self = {};
        bless $self, $class;
        return $self;
    }

    sub evaluate {
        my ($self, $expr) = @_;
        return $expr->evaluate();
    }
}

package Expression {
    sub new {
        my ($class) = @_;
        my $self = {};
        bless $self, $class;
        return $self;
    }

    sub evaluate {
        die "evaluate() not implemented";
    }
}

# represents a binary operation (like +, -, *, /, ^)
package BinaryOp {
    our @ISA = qw(Expression);

    sub new {
        my ($class, $left, $op, $right) = @_;
        my $self = {
            left => $left,
            op => $op,
            right => $right
        };
        bless $self, $class;
        return $self;
    }

    # recursively evaluate left and right expressions 
    # and apply the operator 
    sub evaluate {
        my ($self) = @_;
        my $left = $self->{left}->evaluate();
        my $right = $self->{right}->evaluate();

        if ($self->{op} eq '+') {
            return $left + $right;
        } elsif ($self->{op} eq '-') {
            return $left - $right;
        } elsif ($self->{op} eq '*') {
            return $left * $right;
        } elsif ($self->{op} eq '/') {
            if ($right == 0) {
                die "Division by zero error!";
            }
            return $left / $right;
        } elsif ($self->{op} eq '^') {
            return $left ** $right;
        } else {
            die "Invalid operator: " . $self->{op};
        }
    }
}

# Number class
# a literal numeric value
package Number {
    our @ISA = qw(Expression);

    sub new {
        my ($class, $value) = @_;
        my $self = {
            value => $value
        };
        bless $self, $class;
        return $self;
    }

    sub evaluate {
        my ($self) = @_;
        return $self->{value};
    }
}

# converts a list of tokens into an expression tree
package Parser {
    sub new {
        my ($class, $tokens) = @_;
        my $self = {
            tokens => $tokens,
            current => 0
        };
        bless $self, $class;
        return $self;
    }

    sub peek {
        my ($self) = @_;
        return $self->{tokens}[$self->{current}];
    }

    sub advance {
        my ($self) = @_;
        $self->{current}++;
    }

    # handles '+' and '-' 
    sub expression {
        my ($self) = @_;
        my $left = $self->term();

        while ($self->{current} < @{$self->{tokens}}) {
            my $token = $self->peek();
            if ($token->{type} eq $PLUS || $token->{type} eq $MINUS) {
                my $op = $token->{type} eq $PLUS ? '+' : '-';
                $self->advance();
                my $right = $self->term();
                $left = BinaryOp->new($left, $op, $right);
            } else {
                last;
            }
        }

        return $left;
    }

    # handles '*', '/', and '^'
    sub term {
        my ($self) = @_;
        my $left = $self->factor();

        while ($self->{current} < @{$self->{tokens}}) {
            my $token = $self->peek();
            if ($token->{type} eq $MULTIPLY || 
                $token->{type} eq $DIVIDE || 
                $token->{type} eq $POWER) {
                my $op = $token->{type} eq $MULTIPLY ? '*' : 
                        $token->{type} eq $DIVIDE ? '/' : '^';
                $self->advance();
                my $right = $self->factor();
                $left = BinaryOp->new($left, $op, $right);
            } else {
                last;
            }
        }

        return $left;
    }

    # numbers and parenthesis
    sub factor {
        my ($self) = @_;
        my $token = $self->peek();
        $self->advance();

        if ($token->{type} eq $NUMBER) {
            return Number->new($token->{value});
        } elsif ($token->{type} eq $LPAREN) {
            my $expr = $self->expression();
            if ($self->peek()->{type} ne $RPAREN) {
                die "Expected closing parenthesis";
            }
            $self->advance();
            return $expr;
        } else {
            die "Unexpected token: " . $token->{type};
        }
    }
}

# splits raw input text into tokens
package Lexer {
    sub new {
        my ($class, $text) = @_;
        my $self = {
            text => $text,
            pos => 0
        };
        bless $self, $class;
        return $self;
    }

    sub peek {
        my ($self) = @_;
        return $self->{text}[$self->{pos}];
    }

    sub advance {
        my ($self) = @_;
        $self->{pos}++;
    }

    sub skip_whitespace {
        my ($self) = @_;
        while ($self->{pos} < length($self->{text}) && 
               substr($self->{text}, $self->{pos}, 1) =~ /\s/) {
            $self->{pos}++;
        }
    }

    sub read_number {
        my ($self) = @_;
        my $start = $self->{pos};
        while ($self->{pos} < length($self->{text}) && 
               substr($self->{text}, $self->{pos}, 1) =~ /[\d.]/) {
            $self->{pos}++;
        }
        my $num = substr($self->{text}, $start, $self->{pos} - $start);
        return Token->new($NUMBER, $num + 0);
    }

    sub get_next_token {
        my ($self) = @_;
        $self->skip_whitespace();

        if ($self->{pos} >= length($self->{text})) {
            return Token->new($EOF);
        }

        my $c = substr($self->{text}, $self->{pos}, 1);
        $self->{pos}++;

        if ($c =~ /[\d.]/) {
            $self->{pos}--;
            return $self->read_number();
        }

        my %operators = (
            '+' => $PLUS,
            '-' => $MINUS,
            '*' => $MULTIPLY,
            '/' => $DIVIDE,
            '^' => $POWER,
            '(' => $LPAREN,
            ')' => $RPAREN
        );

        if (exists $operators{$c}) {
            return Token->new($operators{$c});
        }

        die "Invalid character: $c";
    }

    # tokenize the entire input string
    sub tokenize {
        my ($self) = @_;
        my @tokens;
        my $token;
        do {
            $token = $self->get_next_token();
            push @tokens, $token;
        } while ($token->{type} ne $EOF);
        return \@tokens;
    }
}

print "Input:\n";
while (my $line = <STDIN>) {
    chomp $line;

    # exit if input is empty
    last if $line eq '';

    my $lexer = Lexer->new($line);
    my $tokens = $lexer->tokenize();
    my $parser = Parser->new($tokens);
    my $interpreter = Interpreter->new();

    eval {
        my $expr = $parser->expression();
        my $result = $interpreter->evaluate($expr);
        print "Output: $result\n";
    } or do {
        print "Error: $@\n";
    };
} 