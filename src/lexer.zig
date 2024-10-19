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

    pub fn nextToken(self: *Lexer) !Token {
        const result = switch (self.char) {
            '=' => try self.newToken(token.ASSIGN, self.char),
            ';' => try self.newToken(token.SEMICOLON, self.char),
            '(' => try self.newToken(token.LPAREN, self.char),
            ')' => try self.newToken(token.RPAREN, self.char),
            ',' => try self.newToken(token.COMMA, self.char),
            '+' => try self.newToken(token.PLUS, self.char),
            '{' => try self.newToken(token.LBRACE, self.char),
            '}' => try self.newToken(token.RBRACE, self.char),
            else => Token{ .token_type = token.EOF, .literal = "" },
        };

        self.readChar();

        return result;
    }

    fn newToken(self: *Lexer, token_type: token.TokenType, char: u8) !Token {
        return try Token.new(token_type, char, self.arena.allocator());
    }

    pub fn deinit(self: *Lexer) void {
        self.arena.deinit();
    }
};

test "next token" {
    const input = "=+(){},;";

    var lexer = Lexer.new(input);
    defer lexer.deinit();

    try testTokenEquality(Token{ .token_type = token.ASSIGN, .literal = "=" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.PLUS, .literal = "+" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.LPAREN, .literal = "(" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.RPAREN, .literal = ")" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.LBRACE, .literal = "{" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.RBRACE, .literal = "}" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.COMMA, .literal = "," }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.SEMICOLON, .literal = ";" }, try lexer.nextToken());
    try testTokenEquality(Token{ .token_type = token.EOF, .literal = "" }, try lexer.nextToken());
}

fn testTokenEquality(expected: token.Token, actual: token.Token) !void {
    try std.testing.expectEqualSlices(u8, expected.token_type, actual.token_type);
    try std.testing.expectEqualSlices(u8, expected.literal, actual.literal);
}

