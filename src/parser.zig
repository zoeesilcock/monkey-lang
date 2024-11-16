const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const tracer = @import("parser_tracer.zig");

const prefixParseFn = *const fn (self: *Parser) std.mem.Allocator.Error!?ast.Expression;
const infixParseFn = *const fn (self: *Parser, expression: ast.Expression) std.mem.Allocator.Error!?ast.Expression;

const OperatorPrecedence = enum(u32) {
    LOWEST = 0,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
    INDEX,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    temp_allocator: std.mem.Allocator,

    l: *lexer.Lexer,
    errors: [][]const u8,

    cur_token: token.Token = undefined,
    peek_token: token.Token = undefined,

    prefixParseFns: std.StringHashMap(prefixParseFn),
    infixParseFns: std.StringHashMap(infixParseFn),

    precedences: std.StringHashMap(OperatorPrecedence),

    pub fn new(l: *lexer.Lexer, allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator) !Parser {
        var p = Parser{
            .l = l,
            .allocator =  allocator,
            .temp_allocator = temp_allocator,
            .errors = &.{},
            .prefixParseFns = std.StringHashMap(prefixParseFn).init(temp_allocator),
            .infixParseFns = std.StringHashMap(infixParseFn).init(temp_allocator),
            .precedences = std.StringHashMap(OperatorPrecedence).init(temp_allocator),
        };

        try p.precedences.put(token.EQ, .EQUALS);
        try p.precedences.put(token.NOT_EQ, .EQUALS);
        try p.precedences.put(token.LT, .LESSGREATER);
        try p.precedences.put(token.GT, .LESSGREATER);
        try p.precedences.put(token.PLUS, .SUM);
        try p.precedences.put(token.MINUS, .SUM);
        try p.precedences.put(token.SLASH, .PRODUCT);
        try p.precedences.put(token.ASTERISK, .PRODUCT);
        try p.precedences.put(token.LPAREN, .CALL);
        try p.precedences.put(token.LBRACKET, .INDEX);

        try p.registerPrefix(token.IDENT, parseIdentifier);
        try p.registerPrefix(token.INT, parseIntegerLiteral);
        try p.registerPrefix(token.STRING, parseStringLiteral);
        try p.registerPrefix(token.BANG, parsePrefixExpression);
        try p.registerPrefix(token.MINUS, parsePrefixExpression);
        try p.registerPrefix(token.TRUE, parseBooleanLiteral);
        try p.registerPrefix(token.FALSE, parseBooleanLiteral);
        try p.registerPrefix(token.LPAREN, parseGroupedExpression);
        try p.registerPrefix(token.FUNCTION, parseFunctionLiteral);
        try p.registerPrefix(token.IF, parseIfExpression);
        try p.registerPrefix(token.LBRACKET, parseArrayLiteral);

        try p.registerInfix(token.PLUS, parseInfixExpression);
        try p.registerInfix(token.MINUS, parseInfixExpression);
        try p.registerInfix(token.SLASH, parseInfixExpression);
        try p.registerInfix(token.ASTERISK, parseInfixExpression);
        try p.registerInfix(token.EQ, parseInfixExpression);
        try p.registerInfix(token.NOT_EQ, parseInfixExpression);
        try p.registerInfix(token.LT, parseInfixExpression);
        try p.registerInfix(token.GT, parseInfixExpression);

        try p.registerInfix(token.LPAREN, parseCallExpression);
        try p.registerInfix(token.LBRACKET, parseIndexExpression);

        try p.nextToken();
        try p.nextToken();

        return p;
    }

    pub fn getErrors(self: *Parser) [][]const u8 {
        return self.errors;
    }

    pub fn parseProgram(self: *Parser) !ast.Program {
        var program = ast.Program{
            .statements = undefined,
        };

        var statements = std.ArrayList(ast.Statement).init(self.temp_allocator);

        while (!std.mem.eql(u8, self.cur_token.token_type, token.EOF)) {
            if (try self.parseStatement()) |stmt| {
                try statements.append(stmt);
            }

            try self.nextToken();
        }

        program.statements = try statements.toOwnedSlice();

        return program;
    }

    pub fn registerPrefix(self: *Parser, token_type: token.TokenType, parse_fn: prefixParseFn) !void {
        try self.prefixParseFns.put(token_type, parse_fn);
    }

    pub fn registerInfix(self: *Parser, token_type: token.TokenType, parse_fn: infixParseFn) !void {
        try self.infixParseFns.put(token_type, parse_fn);
    }

    fn parseStatement(self: *Parser) !?ast.Statement {
        if (std.mem.eql(u8, self.cur_token.token_type, token.LET)) {
            if (try self.parseLetStatement()) |let_stmt| {
                return ast.Statement.init(let_stmt);
            } else {
                return null;
            }
        } else if (std.mem.eql(u8, self.cur_token.token_type, token.RETURN)) {
            if (try self.parseReturnStatement()) |let_stmt| {
                return ast.Statement.init(let_stmt);
            } else {
                return null;
            }
        } else {
            if (try self.parseExpressionStatement()) |expression_stmt| {
                return ast.Statement.init(expression_stmt);
            } else {
                return null;
            }
        }
    }

    fn parseLetStatement(self: *Parser) !?*ast.LetStatement {
        tracer.trace(@src().fn_name);
        defer tracer.untrace(@src().fn_name);

        const tok = self.cur_token;

        if (!try self.expectPeek(token.IDENT)) {
            return null;
        }

        var name = try self.allocator.create(ast.Identifier);
        name.token = self.cur_token;
        name.value = self.cur_token.literal;

        var stmt: *ast.LetStatement = try self.allocator.create(ast.LetStatement);
        stmt.token = tok;
        stmt.name = name;
        stmt.value = null;

        if (!try self.expectPeek(token.ASSIGN)) {
            return null;
        }

        try self.nextToken();
        stmt.value = try self.parseExpression(.LOWEST);

        if (self.peekTokenIs(token.SEMICOLON)) {
            try self.nextToken();
        }

        return stmt;
    }

    fn parseReturnStatement(self: *Parser) !?*ast.ReturnStatement {
        var stmt: *ast.ReturnStatement = try self.allocator.create(ast.ReturnStatement);
        stmt.token = self.cur_token;

        try self.nextToken();

        stmt.return_value = try self.parseExpression(.LOWEST);

        while (!self.curTokenIs(token.SEMICOLON)) {
            try self.nextToken();
        }

        return stmt;
    }

    fn parseBlockStatement(self: *Parser) !?*ast.BlockStatement {
        var block: *ast.BlockStatement = try self.allocator.create(ast.BlockStatement);
        block.token = self.cur_token;

        try self.nextToken();

        var statements = std.ArrayList(ast.Statement).init(self.allocator);
        while (!self.curTokenIs(token.RBRACE) and !self.curTokenIs(token.EOF)) {
            if (try self.parseStatement()) |stmt| {
                try statements.append(stmt);
            }

            try self.nextToken();
        }

        block.statements = try statements.toOwnedSlice();

        return block;
    }

    fn parseExpressionStatement(self: *Parser) !?*ast.ExpressionStatement {
        tracer.trace(@src().fn_name);
        defer tracer.untrace(@src().fn_name);

        var stmt: ?*ast.ExpressionStatement = null;

        if (try self.parseExpression(.LOWEST)) |expression| {
            stmt = try self.allocator.create(ast.ExpressionStatement);
            stmt.?.token = self.cur_token;
            stmt.?.expression = expression;

            if (self.peekTokenIs(token.SEMICOLON)) {
                try self.nextToken();
            }
        }

        return stmt;
    }

    fn parseExpression(self: *Parser, precedence: OperatorPrecedence) !?ast.Expression {
        tracer.trace(@src().fn_name);
        defer tracer.untrace(@src().fn_name);

        var left_expression: ?ast.Expression = null;

        if (self.prefixParseFns.get(self.cur_token.token_type)) |prefixFn| {
            if (try prefixFn(self)) |expression| {
                left_expression = expression;
            }
        } else {
            try self.noPrefixParseFnError(self.cur_token.token_type);
            return null;
        }

        while (!self.peekTokenIs(token.SEMICOLON) and @intFromEnum(precedence) < self.peekPrecedence()) {
            if (self.infixParseFns.get(self.peek_token.token_type)) |infixFn| {
                try self.nextToken();

                if (try infixFn(self, left_expression.?)) |expression| {
                    left_expression = expression;
                }
            } else {
                return left_expression;
            }
        }

        return left_expression;
    }

    fn parseFunctionParameters(self: *Parser) !?[]*ast.Identifier {
        tracer.trace(@src().fn_name);
        defer tracer.untrace(@src().fn_name);

        var identifiers = std.ArrayList(*ast.Identifier).init(self.allocator);

        if (self.peekTokenIs(token.RPAREN)) {
            try self.nextToken();
            return try identifiers.toOwnedSlice();
        }

        try self.nextToken();

        var identifier: *ast.Identifier = try self.allocator.create(ast.Identifier);
        identifier.token = self.cur_token;
        identifier.value = try self.allocator.dupe(u8, self.cur_token.literal);
        try identifiers.append(identifier);

        while (self.peekTokenIs(token.COMMA)) {
            try self.nextToken();
            try self.nextToken();

            identifier = try self.allocator.create(ast.Identifier);
            identifier.token = self.cur_token;
            identifier.value = try self.allocator.dupe(u8, self.cur_token.literal);
            try identifiers.append(identifier);
        }

        if (!try self.expectPeek(token.RPAREN)) {
            return null;
        }

        return try identifiers.toOwnedSlice();
    }

    fn parseExpressionList(self: *Parser, end: token.TokenType) !?[]ast.Expression {
        tracer.trace(@src().fn_name);
        defer tracer.untrace(@src().fn_name);

        var args = std.ArrayList(ast.Expression).init(self.allocator);

        if (self.peekTokenIs(end)) {
            try self.nextToken();
            return try args.toOwnedSlice();
        }

        try self.nextToken();
        if (try self.parseExpression(.LOWEST)) |expression| {
            try args.append(expression);
        }

        while (self.peekTokenIs(token.COMMA)) {
            try self.nextToken();
            try self.nextToken();

            if (try self.parseExpression(.LOWEST)) |expression| {
                try args.append(expression);
            }
        }

        if (!try self.expectPeek(end)) {
            return null;
        }

        return try args.toOwnedSlice();
    }

    fn noPrefixParseFnError(self: *Parser, token_type: token.TokenType) !void {
        var error_array = try std.ArrayList([]const u8).initCapacity(self.temp_allocator, self.errors.len);
        try error_array.appendSlice(self.errors);
        try error_array.append(try std.fmt.allocPrint(
            self.temp_allocator,
            "no prefix parse function for {s} found\n",
            .{token_type},
        ));
        self.errors = try error_array.toOwnedSlice();
    }

    fn nextToken(self: *Parser) !void {
        self.cur_token = self.peek_token;
        self.peek_token = try self.l.nextToken();
    }

    fn curTokenIs(self: *Parser, t: token.TokenType) bool {
        return std.mem.eql(u8, self.cur_token.token_type, t);
    }

    fn peekTokenIs(self: *Parser, t: token.TokenType) bool {
        return std.mem.eql(u8, self.peek_token.token_type, t);
    }

    fn expectPeek(self: *Parser, t: token.TokenType) !bool {
        if (self.peekTokenIs(t)) {
            try self.nextToken();
            return true;
        }

        try self.peekError(t);
        return false;
    }

    fn peekError(self: *Parser, t: token.TokenType) !void {
        var error_array = try std.ArrayList([]const u8).initCapacity(self.temp_allocator, self.errors.len);
        try error_array.appendSlice(self.errors);

        const message = try std.fmt.allocPrint(
            self.temp_allocator,
            "expected next token to be {s}, got {s} instead\n",
            .{ t, self.peek_token.token_type },
        );
        try error_array.append(message);

        self.errors = try error_array.toOwnedSlice();
    }

    fn peekPrecedence(self: *Parser) u32 {
        if (self.precedences.get(self.peek_token.token_type)) |precedence| {
            return @intFromEnum(precedence);
        }

        return @intFromEnum(OperatorPrecedence.LOWEST);
    }

    fn curPrecedence(self: *Parser) u32 {
        if (self.precedences.get(self.cur_token.token_type)) |precedence| {
            return @intFromEnum(precedence);
        }

        return @intFromEnum(OperatorPrecedence.LOWEST);
    }
};

fn parseIdentifier(self: *Parser) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    var identifier: *ast.Identifier = try self.allocator.create(ast.Identifier);
    identifier.token = self.cur_token;
    identifier.value = try self.allocator.dupe(u8, self.cur_token.literal);

    return ast.Expression.init(identifier);
}

fn parseIntegerLiteral(self: *Parser) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    var expression: ?ast.Expression = null;

    const opt_value: ?i64 = std.fmt.parseInt(i64, self.cur_token.literal, 10) catch null;
    if (opt_value) |value| {
        var lit: *ast.IntegerLiteral = try self.allocator.create(ast.IntegerLiteral);
        lit.token = self.cur_token;
        lit.value = value;
        expression = ast.Expression.init(lit);
    }

    return expression;
}

fn parseStringLiteral(self: *Parser) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    var literal: *ast.StringLiteral = try self.allocator.create(ast.StringLiteral);
    literal.token = self.cur_token;
    literal.value = self.cur_token.literal;

    return ast.Expression.init(literal);
}

fn parseBooleanLiteral(self: *Parser) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    var lit: *ast.BooleanLiteral = try self.allocator.create(ast.BooleanLiteral);
    lit.token = self.cur_token;
    lit.value = self.curTokenIs(token.TRUE);

    return ast.Expression.init(lit);
}

fn parseGroupedExpression(self: *Parser) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    try self.nextToken();

    var expression: ?ast.Expression = try self.parseExpression(.LOWEST);

    if (!try self.expectPeek(token.RPAREN)) {
        expression = null;
    }

    return expression;
}

fn parseIfExpression(self: *Parser) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    var expression: *ast.IfExpression = try self.allocator.create(ast.IfExpression);
    expression.token = self.cur_token;

    if (!try self.expectPeek(token.LPAREN)) {
        return null;
    }

    try self.nextToken();
    expression.condition = try self.parseExpression(.LOWEST);

    if (!try self.expectPeek(token.RPAREN)) {
        return null;
    }

    if (!try self.expectPeek(token.LBRACE)) {
        return null;
    }

    expression.consequence = try self.parseBlockStatement();
    expression.alternative = null;

    if (self.peekTokenIs(token.ELSE)) {
        try self.nextToken();

        if (!try self.expectPeek(token.LBRACE)) {
            return null;
        }

        expression.alternative = try self.parseBlockStatement();
    }

    return ast.Expression.init(expression);
}

fn parseArrayLiteral(self: *Parser) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    var array_literal: *ast.ArrayLiteral = try self.allocator.create(ast.ArrayLiteral);
    array_literal.token = self.cur_token;
    array_literal.elements = try self.parseExpressionList(token.RBRACKET) orelse &.{};

    return ast.Expression.init(array_literal);
}

fn parsePrefixExpression(self: *Parser) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    var expression: *ast.PrefixExpression = try self.allocator.create(ast.PrefixExpression);
    expression.token = self.cur_token;
    expression.operator = try self.allocator.dupe(u8, self.cur_token.literal);

    try self.nextToken();
    expression.right = try self.parseExpression(.PREFIX);

    return ast.Expression.init(expression);
}

fn parseInfixExpression(self: *Parser, left: ast.Expression) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    var expression: *ast.InfixExpression = try self.allocator.create(ast.InfixExpression);
    expression.token = self.cur_token;
    expression.operator = try self.allocator.dupe(u8, self.cur_token.literal);
    expression.left = left;

    const precedence = self.curPrecedence();
    try self.nextToken();
    expression.right = try self.parseExpression(@enumFromInt(precedence));

    return ast.Expression.init(expression);
}

fn parseFunctionLiteral(self: *Parser) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    var lit: *ast.FunctionLiteral = try self.allocator.create(ast.FunctionLiteral);
    lit.token = self.cur_token;
    lit.body = null;

    if (!try self.expectPeek(token.LPAREN)) {
        return null;
    }

    lit.parameters = try self.parseFunctionParameters() orelse &.{};

    if (!try self.expectPeek(token.LBRACE)) {
        return null;
    }

    if (try self.parseBlockStatement()) |body| {
        lit.body = body;
    }

    return ast.Expression.init(lit);
}

fn parseCallExpression(self: *Parser, function: ast.Expression) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    var expression: *ast.CallExpression = try self.allocator.create(ast.CallExpression);
    expression.token = self.cur_token;
    expression.function = function;
    expression.arguments = try self.parseExpressionList(token.RPAREN) orelse &.{};

    return ast.Expression.init(expression);
}

fn parseIndexExpression(self: *Parser, left: ast.Expression) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    var expression: *ast.IndexExpression = try self.allocator.create(ast.IndexExpression);
    expression.token = self.cur_token;
    expression.left = left;

    try self.nextToken();

    expression.index = try self.parseExpression(.LOWEST);

    if (!try self.expectPeek(token.RBRACKET)) {
        return null;
    }

    return ast.Expression.init(expression);
}

const TestSetup = struct {
    parser: Parser,
    lexer: lexer.Lexer,
    program: ast.Program,
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: *TestSetup) void {
        self.lexer.deinit();
        self.arena.deinit();
    }
};

fn setupTestParser(input: []const u8) !TestSetup {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var l = lexer.Lexer.init(input, arena.allocator());
    var p = try Parser.new(&l, arena.allocator(), arena.allocator());
    const program = try p.parseProgram();
    return TestSetup{ .parser = p, .lexer = l, .program = program, .arena = arena };
}

const TestValueTypes = enum {
    int_value,
    bool_value,
    string_value,
};

pub const TestValue = union(TestValueTypes) {
    int_value: i64,
    bool_value: bool,
    string_value: []const u8,
};

fn testLiteralExpression(expected_value: TestValue, expression: ast.Expression) !void {
    switch (expected_value) {
        .int_value => |value| try testIntegerLiteral(value, expression),
        .bool_value => |value| try testBooleanLiteral(value, expression),
        .string_value => |value| try testIdentifier(value, expression),
    }
}

fn testIntegerLiteral(expected_value: i64, integer_literal: ast.Expression) !void {
    const literal: *ast.IntegerLiteral = integer_literal.unwrap(ast.IntegerLiteral);
    try std.testing.expectEqual(expected_value, literal.value);

    var buf: [10]u8 = undefined;
    const expected_value_str = try std.fmt.bufPrint(&buf, "{d}", .{expected_value});
    try std.testing.expectEqualSlices(u8, expected_value_str, literal.tokenLiteral());
}

fn testBooleanLiteral(expected_value: bool, boolean_literal: ast.Expression) !void {
    const literal: *ast.BooleanLiteral = boolean_literal.unwrap(ast.BooleanLiteral);
    try std.testing.expectEqual(expected_value, literal.value);

    var buf: [10]u8 = undefined;
    const expected_value_str = try std.fmt.bufPrint(&buf, "{s}", .{if (expected_value) "true" else "false"});
    try std.testing.expectEqualSlices(u8, expected_value_str, literal.tokenLiteral());
}

test "let statements" {
    try testLetStatement("let x = 5;", "x", .{ .int_value = 5 });
    try testLetStatement("let y = true;", "y", .{ .bool_value = true });
    try testLetStatement("let foobar = y;", "foobar", .{ .string_value = "y" });
}

fn testLetStatement(input: []const u8, expected_identifier: []const u8, expected_value: TestValue) !void {
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt = setup.program.statements[0];
    try std.testing.expectEqualSlices(u8, "let", stmt.tokenLiteral());

    const let_stmt: *const ast.LetStatement = stmt.unwrap(ast.LetStatement);
    try std.testing.expectEqualSlices(u8, expected_identifier, let_stmt.name.value);
    try std.testing.expectEqualSlices(u8, expected_identifier, let_stmt.name.tokenLiteral());

    try testLiteralExpression(expected_value, let_stmt.value.?);
}

test "parser errors for let statements" {
    const input =
        \\let x 5;
        \\let = 10;
        \\let 838383;
    ;

    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 4);
}

fn expectErrors(parser: *Parser, error_count: u32) !void {
    const errors = parser.getErrors();

    if (errors.len != error_count) {
        std.debug.print("parser has {d} unexpected errors\n", .{errors.len});

        for (errors) |err| {
            std.debug.print("parser error: {s}", .{err});
        }
    }

    try std.testing.expectEqual(error_count, errors.len);
}

test "return statements" {
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(3, setup.program.statements.len);

    try testReturnStatement(.{ .int_value = 5 }, setup.program.statements[0]);
    try testReturnStatement(.{ .int_value = 10 }, setup.program.statements[1]);
    try testReturnStatement(.{ .int_value = 993322 }, setup.program.statements[2]);
}

fn testReturnStatement(expected_value: TestValue, s: ast.Statement) !void {
    var return_stmt: *ast.ReturnStatement = s.unwrap(ast.ReturnStatement);
    try std.testing.expectEqualSlices(u8, "return", return_stmt.tokenLiteral());
    try testLiteralExpression(expected_value, return_stmt.return_value.?);
}

test "identifier expression" {
    const input = "foobar;";

    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *ast.ExpressionStatement = setup.program.statements[0].unwrap(ast.ExpressionStatement);
    if (stmt.expression) |expression| {
        try testIdentifier("foobar", expression);
    }
}

fn testIdentifier(expected_value: []const u8, expression: ast.Expression) !void {
    const ident: *ast.Identifier = expression.unwrap(ast.Identifier);

    try std.testing.expectEqualSlices(u8, expected_value, ident.value);
    try std.testing.expectEqualSlices(u8, expected_value, ident.tokenLiteral());
}

test "integer literal expression" {
    const input = "5;";

    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *ast.ExpressionStatement = setup.program.statements[0].unwrap(ast.ExpressionStatement);
    if (stmt.expression) |expression| {
        const literal: *ast.IntegerLiteral = expression.unwrap(ast.IntegerLiteral);
        try std.testing.expectEqual(5, literal.value);
        try std.testing.expectEqualSlices(u8, "5", literal.tokenLiteral());
    }
}

test "boolean literal expression" {
    try testBooleanExpression("true;", true);
    try testBooleanExpression("false;", false);
}

fn testBooleanExpression(input: []const u8, expected_value: bool) !void {
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *ast.ExpressionStatement = setup.program.statements[0].unwrap(ast.ExpressionStatement);
    if (stmt.expression) |expression| {
        const literal: *ast.BooleanLiteral = expression.unwrap(ast.BooleanLiteral);
        try std.testing.expectEqual(expected_value, literal.value);
    }
}

test "prefix expressions" {
    try testPrefixExpression("!5;", "!", .{ .int_value = 5 });
    try testPrefixExpression("-15;", "-", .{ .int_value = 15 });
    try testPrefixExpression("!foobar;", "!", .{ .string_value = "foobar" });
    try testPrefixExpression("-foobar;", "-", .{ .string_value = "foobar" });
    try testPrefixExpression("!true;", "!", .{ .bool_value = true });
    try testPrefixExpression("!false;", "!", .{ .bool_value = false });
}

fn testPrefixExpression(input: []const u8, expected_operator: []const u8, expected_value: TestValue) !void {
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *ast.ExpressionStatement = setup.program.statements[0].unwrap(ast.ExpressionStatement);
    if (stmt.expression) |expression| {
        const prefix_expression: *ast.PrefixExpression = expression.unwrap(ast.PrefixExpression);
        try std.testing.expectEqualSlices(u8, expected_operator, prefix_expression.operator);
        try testLiteralExpression(expected_value, prefix_expression.right.?);
    }
}

test "infix expressions" {
    try testParsingInfixExpression("5 + 5;", .{ .int_value = 5 }, "+", .{ .int_value = 5 });
    try testParsingInfixExpression("5 - 5;", .{ .int_value = 5 }, "-", .{ .int_value = 5 });
    try testParsingInfixExpression("5 * 5;", .{ .int_value = 5 }, "*", .{ .int_value = 5 });
    try testParsingInfixExpression("5 / 5;", .{ .int_value = 5 }, "/", .{ .int_value = 5 });
    try testParsingInfixExpression("5 > 5;", .{ .int_value = 5 }, ">", .{ .int_value = 5 });
    try testParsingInfixExpression("5 < 5;", .{ .int_value = 5 }, "<", .{ .int_value = 5 });
    try testParsingInfixExpression("5 == 5;", .{ .int_value = 5 }, "==", .{ .int_value = 5 });
    try testParsingInfixExpression("5 != 5;", .{ .int_value = 5 }, "!=", .{ .int_value = 5 });
    try testParsingInfixExpression("foobar + barfoo;", .{ .string_value = "foobar" }, "+", .{ .string_value = "barfoo" });
    try testParsingInfixExpression("foobar - barfoo;", .{ .string_value = "foobar" }, "-", .{ .string_value = "barfoo" });
    try testParsingInfixExpression("foobar * barfoo;", .{ .string_value = "foobar" }, "*", .{ .string_value = "barfoo" });
    try testParsingInfixExpression("foobar / barfoo;", .{ .string_value = "foobar" }, "/", .{ .string_value = "barfoo" });
    try testParsingInfixExpression("foobar > barfoo;", .{ .string_value = "foobar" }, ">", .{ .string_value = "barfoo" });
    try testParsingInfixExpression("foobar < barfoo;", .{ .string_value = "foobar" }, "<", .{ .string_value = "barfoo" });
    try testParsingInfixExpression("foobar == barfoo;", .{ .string_value = "foobar" }, "==", .{ .string_value = "barfoo" });
    try testParsingInfixExpression("foobar != barfoo;", .{ .string_value = "foobar" }, "!=", .{ .string_value = "barfoo" });
    try testParsingInfixExpression("true == true", .{ .bool_value = true }, "==", .{ .bool_value = true });
    try testParsingInfixExpression("true != false", .{ .bool_value = true }, "!=", .{ .bool_value = false });
    try testParsingInfixExpression("false == false", .{ .bool_value = false }, "==", .{ .bool_value = false });
}

fn testParsingInfixExpression(input: []const u8, left_value: TestValue, operator: []const u8, right_value: TestValue) !void {
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *ast.ExpressionStatement = setup.program.statements[0].unwrap(ast.ExpressionStatement);
    if (stmt.expression) |expression| {
        try testInfixEpression(left_value, operator, right_value, expression);
    }
}

fn testInfixEpression(left_value: TestValue, operator: []const u8, right_value: TestValue, expression: ast.Expression) !void {
    const infix_expression: *ast.InfixExpression = expression.unwrap(ast.InfixExpression);
    try testLiteralExpression(left_value, infix_expression.left.?);
    try std.testing.expectEqualSlices(u8, operator, infix_expression.operator);
    try testLiteralExpression(right_value, infix_expression.right.?);
}

test "operator precedence parsing" {
    try testOperatorPrecedenceParsing(
        "-a * b",
        "((-a) * b)",
    );
    try testOperatorPrecedenceParsing(
        "!-a",
        "(!(-a))",
    );
    try testOperatorPrecedenceParsing(
        "a + b + c",
        "((a + b) + c)",
    );
    try testOperatorPrecedenceParsing(
        "a + b - c",
        "((a + b) - c)",
    );
    try testOperatorPrecedenceParsing(
        "a * b * c",
        "((a * b) * c)",
    );
    try testOperatorPrecedenceParsing(
        "a * b / c",
        "((a * b) / c)",
    );
    try testOperatorPrecedenceParsing(
        "a + b / c",
        "(a + (b / c))",
    );
    try testOperatorPrecedenceParsing(
        "a + b * c + d / e - f",
        "(((a + (b * c)) + (d / e)) - f)",
    );
    try testOperatorPrecedenceParsing(
        "3 + 4; -5 * 5",
        "(3 + 4)((-5) * 5)",
    );
    try testOperatorPrecedenceParsing(
        "5 > 4 == 3 < 4",
        "((5 > 4) == (3 < 4))",
    );
    try testOperatorPrecedenceParsing(
        "5 < 4 != 3 > 4",
        "((5 < 4) != (3 > 4))",
    );
    try testOperatorPrecedenceParsing(
        "3 + 4 * 5 == 3 * 1 + 4 * 5",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
    );
    try testOperatorPrecedenceParsing(
        "true",
        "true",
    );
    try testOperatorPrecedenceParsing(
        "false",
        "false",
    );
    try testOperatorPrecedenceParsing(
        "3 > 5 == false",
        "((3 > 5) == false)",
    );
    try testOperatorPrecedenceParsing(
        "3 < 5 == true",
        "((3 < 5) == true)",
    );
    try testOperatorPrecedenceParsing(
        "1 + (2 + 3) + 4",
        "((1 + (2 + 3)) + 4)",
    );
    try testOperatorPrecedenceParsing(
        "(5 + 5) * 2",
        "((5 + 5) * 2)",
    );
    try testOperatorPrecedenceParsing(
        "2 / (5 + 5)",
        "(2 / (5 + 5))",
    );
    try testOperatorPrecedenceParsing(
        "(5 + 5) * 2 * (5 + 5)",
        "(((5 + 5) * 2) * (5 + 5))",
    );
    try testOperatorPrecedenceParsing(
        "-(5 + 5)",
        "(-(5 + 5))",
    );
    try testOperatorPrecedenceParsing(
        "!(true == true)",
        "(!(true == true))",
    );
    try testOperatorPrecedenceParsing(
        "a + add(b * c) + d",
        "((a + add((b * c))) + d)",
    );
    try testOperatorPrecedenceParsing(
        "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
        "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
    );
    try testOperatorPrecedenceParsing(
        "add(a + b + c * d / f + g)",
        "add((((a + b) + ((c * d) / f)) + g))",
    );
    try testOperatorPrecedenceParsing(
        "a * [1, 2, 3, 4][b * c] * d",
        "((a * ([1, 2, 3, 4][(b * c)])) * d)",
    );
    try testOperatorPrecedenceParsing(
        "add(a * b[2], b[1], 2 * [1, 2][1])",
        "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
    );
}

fn testOperatorPrecedenceParsing(input: []const u8, expected: []const u8) !void {
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);

    try std.testing.expectEqualSlices(u8, expected, setup.program.string(setup.arena.allocator()));
}

test "if expression" {
    const input = "if (x < y) { x }";
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *const ast.ExpressionStatement = setup.program.statements[0].unwrap(ast.ExpressionStatement);
    const expression: *const ast.IfExpression = stmt.expression.?.unwrap(ast.IfExpression);

    try testInfixEpression(.{ .string_value = "x" }, "<", .{ .string_value = "y" }, expression.condition.?);
    try std.testing.expectEqual(1, expression.consequence.?.statements.len);

    const consequence: *const ast.ExpressionStatement = expression.consequence.?.statements[0].unwrap(ast.ExpressionStatement);
    try testIdentifier("x", consequence.expression.?);

    try std.testing.expectEqual(null, expression.alternative);
}

test "if else expression" {
    const input = "if (x < y) { x } else { y }";
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *const ast.ExpressionStatement = setup.program.statements[0].unwrap(ast.ExpressionStatement);
    const expression: *const ast.IfExpression = stmt.expression.?.unwrap(ast.IfExpression);

    try testInfixEpression(.{ .string_value = "x" }, "<", .{ .string_value = "y" }, expression.condition.?);
    try std.testing.expectEqual(1, expression.consequence.?.statements.len);

    const consequence: *const ast.ExpressionStatement = expression.consequence.?.statements[0].unwrap(ast.ExpressionStatement);
    try testIdentifier("x", consequence.expression.?);

    try std.testing.expectEqual(1, expression.alternative.?.statements.len);
    const alternative: *const ast.ExpressionStatement = expression.alternative.?.statements[0].unwrap(ast.ExpressionStatement);
    try testIdentifier("y", alternative.expression.?);
}

test "function literals" {
    const input = "fn(x, y) { x + y; }";
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *const ast.ExpressionStatement = setup.program.statements[0].unwrap(ast.ExpressionStatement);
    const function_literal: *const ast.FunctionLiteral = stmt.expression.?.unwrap(ast.FunctionLiteral);

    try std.testing.expectEqual(2, function_literal.parameters.len);

    try testLiteralExpression(.{ .string_value = "x" }, ast.Expression.init(function_literal.parameters[0]));
    try testLiteralExpression(.{ .string_value = "y" }, ast.Expression.init(function_literal.parameters[1]));

    try std.testing.expectEqual(1, function_literal.body.?.statements.len);

    const body_stmt: *const ast.ExpressionStatement = function_literal.body.?.statements[0].unwrap(ast.ExpressionStatement);

    try testInfixEpression(.{ .string_value = "x" }, "+", .{ .string_value = "y" }, body_stmt.expression.?);
}

test "function parameters" {
    try testFunctionParameters("fn() {};", &.{});
    try testFunctionParameters("fn(x) {};", &.{"x"});
    try testFunctionParameters("fn(x, y, z) {};", &.{"x", "y", "z"});
}

fn testFunctionParameters(input: []const u8, expected_params: []const []const u8) !void {
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *const ast.ExpressionStatement = setup.program.statements[0].unwrap(ast.ExpressionStatement);
    const function_literal: *const ast.FunctionLiteral = stmt.expression.?.unwrap(ast.FunctionLiteral);

    try std.testing.expectEqual(expected_params.len, function_literal.parameters.len);

    for (expected_params, 0..) |expected_param, i| {
        try testLiteralExpression(.{ .string_value = expected_param }, ast.Expression.init(function_literal.parameters[i]));
    }
}

test "call expressions" {
    const input = "add(1, 2 * 3, 4 + 5);";
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *const ast.ExpressionStatement = setup.program.statements[0].unwrap(ast.ExpressionStatement);
    const call_expression: *ast.CallExpression = stmt.expression.?.unwrap(ast.CallExpression);

    try testIdentifier("add", call_expression.function.?);

    try std.testing.expectEqual(3, call_expression.arguments.len);

    try testLiteralExpression(.{ .int_value = 1 }, call_expression.arguments[0]);
    try testInfixEpression(.{ .int_value = 2 }, "*", .{ .int_value = 3 }, call_expression.arguments[1]);
    try testInfixEpression(.{ .int_value = 4 }, "+", .{ .int_value = 5 }, call_expression.arguments[2]);
}

test "string literal expression" {
    const input = "\"hello world\"";
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *const ast.ExpressionStatement = setup.program.statements[0].unwrap(ast.ExpressionStatement);
    const string_literal: *ast.StringLiteral = stmt.expression.?.unwrap(ast.StringLiteral);
    try std.testing.expectEqualSlices(u8, "hello world", string_literal.value);
}

test "parsing array liteals" {
    const input = "[1, 2 * 2, 3 + 3]";

    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *const ast.ExpressionStatement = setup.program.statements[0].unwrap(ast.ExpressionStatement);

    const array_literal: *ast.ArrayLiteral = stmt.expression.?.unwrap(ast.ArrayLiteral);
    try std.testing.expectEqual(3, array_literal.elements.len);

    try testIntegerLiteral(1, array_literal.elements[0]);
    try testInfixEpression(.{ .int_value = 2 }, "*", .{ .int_value = 2 }, array_literal.elements[1]);
    try testInfixEpression(.{ .int_value = 3 }, "+", .{ .int_value = 3 }, array_literal.elements[2]);
}

test "parsing index expressions" {
    const input = "myArray[1 + 1]";

    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);

    const stmt: *const ast.ExpressionStatement = setup.program.statements[0].unwrap(ast.ExpressionStatement);

    const index_expression: *ast.IndexExpression = stmt.expression.?.unwrap(ast.IndexExpression);
    try testIdentifier("myArray", index_expression.left);
    try testInfixEpression(.{ .int_value = 1 }, "+", .{ .int_value = 1 }, index_expression.index.?);
}
