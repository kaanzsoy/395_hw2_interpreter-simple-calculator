with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Containers.Vectors;
with Ada.Numerics.Elementary_Functions;
with Ada.Exceptions;

use Ada.Text_IO;
use Ada.Float_Text_IO;
use Ada.Strings.Fixed;
use Ada.Strings.Maps;
use Ada.Containers;
use Ada.Numerics.Elementary_Functions;

procedure Calculator is

    type Token_Type is (
        Number,
        Plus,
        Minus,
        Multiply,
        Divide,
        Power,
        LeftParen,
        RightParen,
        EOF
    );


    type Token (Kind : Token_Type := Number) is record
        case Kind is
            when Number =>
                Value : Float;
            when others =>
                null;
        end case;
    end record;


    package Token_Vectors is new Ada.Containers.Vectors
        (Index_Type => Natural, Element_Type => Token);


    type Lexer is record
        Input : String(1 .. 1000);
        Length : Natural;
        Pos : Natural;
    end record;

 
    type Parser is record
        Tokens : Token_Vectors.Vector;
        Current : Natural;
    end record;


    function Expression(P : in out Parser) return Float;
    pragma Inline(Expression);


    function Peek(L : Lexer) return Character is
    begin
        if L.Pos <= L.Length then
            return L.Input(L.Pos);
        else
            return ' ';
        end if;
    end Peek;

    procedure Advance(L : in out Lexer) is
    begin
        L.Pos := L.Pos + 1;
    end Advance;

    procedure Skip_Whitespace(L : in out Lexer) is
    begin
        while L.Pos <= L.Length and then Is_In(L.Input(L.Pos), To_Set(" ")) loop
            L.Pos := L.Pos + 1;
        end loop;
    end Skip_Whitespace;

    function Read_Number(L : in out Lexer) return Token is
        Start : Natural := L.Pos;
        Num_Str : String(1 .. 20);
        Num_Length : Natural := 0;
        Num : Float;
    begin
        while L.Pos <= L.Length and then
              (Is_In(L.Input(L.Pos), To_Set("0123456789."))) loop
            Num_Length := Num_Length + 1;
            Num_Str(Num_Length) := L.Input(L.Pos);
            L.Pos := L.Pos + 1;
        end loop;
        Num := Float'Value(Num_Str(1 .. Num_Length));
        return (Kind => Number, Value => Num);
    end Read_Number;

    function Get_Next_Token(L : in out Lexer) return Token is
        C : Character;
    begin
        Skip_Whitespace(L);

        if L.Pos > L.Length then
            return (Kind => EOF);
        end if;

        C := Peek(L);
        Advance(L);

        case C is
            when '+' => return (Kind => Plus);
            when '-' => return (Kind => Minus);
            when '*' => return (Kind => Multiply);
            when '/' => return (Kind => Divide);
            when '^' => return (Kind => Power);
            when '(' => return (Kind => LeftParen);
            when ')' => return (Kind => RightParen);
            when '0' .. '9' | '.' =>
                L.Pos := L.Pos - 1;
                return Read_Number(L);
            when others =>
                raise Constraint_Error with "Invalid character: " & C;
        end case;
    end Get_Next_Token;

    function Tokenize(L : in out Lexer) return Token_Vectors.Vector is
        Tokens : Token_Vectors.Vector;
        Current_Token : Token;
    begin
        loop
            Current_Token := Get_Next_Token(L);
            Token_Vectors.Append(Tokens, Current_Token);
            exit when Current_Token.Kind = EOF;
        end loop;
        return Tokens;
    end Tokenize;


    function Peek(P : Parser) return Token is
    begin
        return Token_Vectors.Element(P.Tokens, P.Current);
    end Peek;

    procedure Advance(P : in out Parser) is
    begin
        P.Current := P.Current + 1;
    end Advance;

    function Factor(P : in out Parser) return Float is
        Current_Token : Token := Peek(P);
    begin
        Advance(P);

        case Current_Token.Kind is
            when Number =>
                return Current_Token.Value;
            when LeftParen =>
                declare
                    Result : Float := Expression(P);
                begin
                    if Peek(P).Kind /= RightParen then
                        raise Constraint_Error with "Expected closing parenthesis";
                    end if;
                    Advance(P);
                    return Result;
                end;
            when others =>
                raise Constraint_Error with "Unexpected token";
        end case;
    end Factor;

    function Term(P : in out Parser) return Float is
        Left : Float := Factor(P);
        Current_Token : Token;
    begin
        while P.Current < Natural(Token_Vectors.Length(P.Tokens)) loop
            Current_Token := Peek(P);
            case Current_Token.Kind is
                when Multiply | Divide | Power =>
                    declare
                        Op : Token_Type := Current_Token.Kind;
                    begin
                        Advance(P);
                        case Op is
                            when Multiply =>
                                Left := Left * Factor(P);
                            when Divide =>
                                declare
                                    Right : Float := Factor(P);
                                begin
                                    if Right = 0.0 then
                                        raise Constraint_Error with "Division by zero error!";
                                    end if;
                                    Left := Left / Right;
                                end;
                            when Power =>
                                Left := Left ** Factor(P);
                            when others =>
                                null;
                        end case;
                    end;
                when others =>
                    exit;
            end case;
        end loop;
        return Left;
    end Term;

    function Expression(P : in out Parser) return Float is
        Left : Float := Term(P);
        Current_Token : Token;
    begin
        while P.Current < Natural(Token_Vectors.Length(P.Tokens)) loop
            Current_Token := Peek(P);
            case Current_Token.Kind is
                when Plus | Minus =>
                    declare
                        Op : Token_Type := Current_Token.Kind;
                    begin
                        Advance(P);
                        if Op = Plus then
                            Left := Left + Term(P);
                        else
                            Left := Left - Term(P);
                        end if;
                    end;
                when others =>
                    exit;
            end case;
        end loop;
        return Left;
    end Expression;


    Input : String(1 .. 1000);
    Length : Natural;
    L : Lexer;
    P : Parser;
    Result : Float;
begin
    Put_Line("Input:");
    loop
        Get_Line(Input, Length);
        exit when Length = 0;

        L := (Input, Length, 1);
        P.Tokens := Tokenize(L);
        P.Current := 0;

        begin
            Result := Expression(P);
            Put("Output: ");
            Put(Result, Fore => 0, Aft => 0, Exp => 0);
            New_Line;
        exception
            when E : others =>
                Put_Line("Error: " & Ada.Exceptions.Exception_Name(E));
        end;
    end loop;
end Calculator; 