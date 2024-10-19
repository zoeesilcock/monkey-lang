const std = @import("std");

pub const TokenType: type = []const u8;

pub const Token = struct {
    token_type: TokenType,
    literal: []const u8,

    pub fn new(token_type: TokenType, char: u8, allocator: std.mem.Allocator) !Token {
        return Token{ .token_type = token_type, .literal = try allocator.dupe(u8, &.{ char }) };
    }

    pub fn deinit(self: *Token, allocator: std.mem.Allocator) void {
        allocator.free(self.literal);
    }
};

// Token types
pub const ILLEGAL = "ILLEGAL";
pub const EOF = "EOF";

// Identifiers + literals
pub const IDENT = "IDENT"; // add, foobar, x, y, ...
pub const INT = "INT";   // 1343456

// Operators
pub const ASSIGN = "=";
pub const PLUS = "+";

// Delimiters
pub const COMMA = ",";
pub const SEMICOLON = ";";

pub const LPAREN = "(";
pub const RPAREN = ")";
pub const LBRACE = "{";
pub const RBRACE = "}";

// Keywords
pub const FUNCTION = "FUNCTION";
pub const LET = "LET";

