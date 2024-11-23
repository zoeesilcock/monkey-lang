const std = @import("std");

pub const TokenType: type = []const u8;

pub const Token = struct {
    token_type: TokenType = undefined,
    literal: []const u8 = undefined,

    pub fn init(token_type: TokenType, char: u8, allocator: std.mem.Allocator) !Token {
        return Token{ .token_type = token_type, .literal = try allocator.dupe(u8, &.{ char }) };
    }

    pub fn deinit(self: *Token, allocator: std.mem.Allocator) void {
        allocator.free(self.literal);
    }

    pub fn format(self: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;

        try writer.print("{{ token_type: \"{s}\", literal: \"{s}\" }}", .{ self.token_type, self.literal });
    }
};

// Token types
pub const ILLEGAL = "ILLEGAL";
pub const EOF = "EOF";

// Identifiers + literals
pub const IDENT = "IDENT"; // add, foobar, x, y, ...
pub const INT = "INT"; // 1343456
pub const STRING = "STRING"; // "foobar"

// Operators
pub const ASSIGN = "=";
pub const PLUS = "+";
pub const MINUS = "-";
pub const BANG = "!";
pub const ASTERISK = "*";
pub const SLASH = "/";

pub const LT = "<";
pub const GT = ">";

pub const EQ = "==";
pub const NOT_EQ = "!=";

// Delimiters
pub const COMMA = ",";
pub const SEMICOLON = ";";
pub const COLON = ";";

pub const LPAREN = "(";
pub const RPAREN = ")";
pub const LBRACE = "{";
pub const RBRACE = "}";
pub const LBRACKET = "[";
pub const RBRACKET = "]";

// Keywords
pub const FUNCTION = "FUNCTION";
pub const LET = "LET";
pub const TRUE = "TRUE";
pub const FALSE = "FALSE";
pub const IF = "IF";
pub const ELSE = "ELSE";
pub const RETURN = "RETURN";

pub fn lookupIdent(ident: []const u8) TokenType {
    var result: TokenType = undefined;

    if (std.mem.eql(u8, "fn", ident)) {
        result = FUNCTION;
    } else if (std.mem.eql(u8, "let", ident)) {
        result = LET;
    } else if (std.mem.eql(u8, "true", ident)) {
        result = TRUE;
    } else if (std.mem.eql(u8, "false", ident)) {
        result = FALSE;
    } else if (std.mem.eql(u8, "if", ident)) {
        result = IF;
    } else if (std.mem.eql(u8, "else", ident)) {
        result = ELSE;
    } else if (std.mem.eql(u8, "return", ident)) {
        result = RETURN;
    } else {
        result = IDENT;
    }

    return result;
}
