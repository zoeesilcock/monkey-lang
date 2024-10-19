const std = @import("std");
const token = @import("token.zig");

const Token = token.Token;

const Lexer = struct {
    input: []const u8,
    position: u32 = 0,
    read_position: u32 = 0,
    char: u8 = 0,
    arena: std.heap.ArenaAllocator,

    pub fn new(input: []const u8) Lexer {
        const arena = std.heap.ArenaAllocator.init(std.heap.page_allocator); 
        var result = Lexer{ .input = input, .arena = arena };

        result.readChar();

        return result;
    }

    fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.char = 0;
        } else {
            self.char = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peekChar(self: *Lexer) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_position];
        }
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const position = self.position;

        while (isLetter(self.char)) {
            self.readChar();
        }

        return self.input[position..self.position];
    }

    fn readNumber(self: *Lexer) []const u8 {
        const position = self.position;

        while (isDigit(self.char)) {
            self.readChar();
        }

        return self.input[position..self.position];
    }

    pub fn nextToken(self: *Lexer) !Token {
        var result = Token{};

        self.skipWhitespace();

        switch (self.char) {
            '=' => {
                if (self.peekChar() == '=') {
                    const char1 = self.char;
                    self.readChar();
                    result.literal = try self.arena.allocator().dupe(u8, &.{ char1, self.char });
                    result.token_type = token.EQ;
                } else {
                    result = try self.newToken(token.ASSIGN, self.char);
                }
            },
            '+' => { result = try self.newToken(token.PLUS, self.char); },
            '-' => { result = try self.newToken(token.MINUS, self.char); },
            '!' => {
                if (self.peekChar() == '=') {
                    const char1 = self.char;
                    self.readChar();
                    result.literal = try self.arena.allocator().dupe(u8, &.{ char1, self.char });
                    result.token_type = token.NOT_EQ;
                } else {
                    result = try self.newToken(token.BANG, self.char);
                }
            },
            '/' => { result = try self.newToken(token.SLASH, self.char); },
            '*' => { result = try self.newToken(token.ASTERISK, self.char); },
            '<' => { result = try self.newToken(token.LT, self.char); },
            '>' => { result = try self.newToken(token.GT, self.char); },
            ';' => { result = try self.newToken(token.SEMICOLON, self.char); },
            ',' => { result = try self.newToken(token.COMMA, self.char); },
            '(' => { result = try self.newToken(token.LPAREN, self.char); },
            ')' => { result = try self.newToken(token.RPAREN, self.char); },
            '{' => { result = try self.newToken(token.LBRACE, self.char); },
            '}' => { result = try self.newToken(token.RBRACE, self.char); },
            0 => { result = Token{ .token_type = token.EOF, .literal = "" }; },
            else => {
                if (isLetter(self.char)) {
                    result.literal = self.readIdentifier();
                    result.token_type = token.lookupIdent(result.literal);
                    return result;
                } else if (isDigit(self.char)) {
                    result.literal = self.readNumber();
                    result.token_type = token.INT;
                    return result;
                } else {
                    result = try self.newToken(token.ILLEGAL, self.char);
                }
            },
        }

        self.readChar();

        return result;
    }

    fn newToken(self: *Lexer, token_type: token.TokenType, char: u8) !Token {
        return try Token.new(token_type, char, self.arena.allocator());
    }

    fn isLetter(char: u8) bool {
        return ('a' <= char and char <= 'z') or ('A' <= char and char <= 'Z') or char == '_';
    }

    fn isDigit(char: u8) bool {
        return '0' <= char and char <= '9';
    }

    fn skipWhitespace(self: *Lexer) void {
        while (self.char == ' ' or self.char == '\t' or self.char == '\n' or self.char == '\r') {
            self.readChar();
        }
    }

    pub fn deinit(self: *Lexer) void {
        self.arena.deinit();
    }
};

test "next token" {
    const input = 
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\ if (5 < 10) {
        \\    return true;
        \\} else {
        \\    return false;
        \\}
        \\
        \\10 == 10;
        \\10 != 9;
    ;

    var lexer = Lexer.new(input);
    defer lexer.deinit();

    try testTokenEquality(Token{ .token_type = token.LET, .literal = "let" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.IDENT, .literal = "five" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.ASSIGN, .literal = "=" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.INT, .literal = "5" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.SEMICOLON, .literal = ";" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.LET, .literal = "let" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.IDENT, .literal = "ten" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.ASSIGN, .literal = "=" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.INT, .literal = "10" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.SEMICOLON, .literal = ";" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.LET, .literal = "let" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.IDENT, .literal = "add" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.ASSIGN, .literal = "=" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.FUNCTION, .literal = "fn" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.LPAREN, .literal = "(" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.IDENT, .literal = "x" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.COMMA, .literal = "," }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.IDENT, .literal = "y" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.RPAREN, .literal = ")" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.LBRACE, .literal = "{" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.IDENT, .literal = "x" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.PLUS, .literal = "+" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.IDENT, .literal = "y" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.SEMICOLON, .literal = ";" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.RBRACE, .literal = "}" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.SEMICOLON, .literal = ";" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.LET, .literal = "let" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.IDENT, .literal = "result" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.ASSIGN, .literal = "=" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.IDENT, .literal = "add" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.LPAREN, .literal = "(" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.IDENT, .literal = "five" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.COMMA, .literal = "," }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.IDENT, .literal = "ten" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.RPAREN, .literal = ")" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.SEMICOLON, .literal = ";" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.BANG, .literal = "!" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.MINUS, .literal = "-" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.SLASH, .literal = "/" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.ASTERISK, .literal = "*" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.INT, .literal = "5" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.SEMICOLON, .literal = ";" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.INT, .literal = "5" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.LT, .literal = "<" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.INT, .literal = "10" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.GT, .literal = ">" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.INT, .literal = "5" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.SEMICOLON, .literal = ";" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.IF, .literal = "if" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.LPAREN, .literal = "(" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.INT, .literal = "5" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.LT, .literal = "<" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.INT, .literal = "10" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.RPAREN, .literal = ")" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.LBRACE, .literal = "{" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.RETURN, .literal = "return" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.TRUE, .literal = "true" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.SEMICOLON, .literal = ";" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.RBRACE, .literal = "}" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.ELSE, .literal = "else" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.LBRACE, .literal = "{" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.RETURN, .literal = "return" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.FALSE, .literal = "false" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.SEMICOLON, .literal = ";" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.RBRACE, .literal = "}" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.INT, .literal = "10" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.EQ, .literal = "==" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.INT, .literal = "10" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.SEMICOLON, .literal = ";" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.INT, .literal = "10" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.NOT_EQ, .literal = "!=" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.INT, .literal = "9" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.SEMICOLON, .literal = ";" }, try lexer.nextToken());

    try testTokenEquality(Token{ .token_type = token.EOF, .literal = "" }, try lexer.nextToken());
}

fn testTokenEquality(expected: token.Token, actual: token.Token) !void {
    try std.testing.expectEqualSlices(u8, expected.token_type, actual.token_type);
    try std.testing.expectEqualSlices(u8, expected.literal, actual.literal);
}

