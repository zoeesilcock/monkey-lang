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
};

const Parser = struct {
    arena: std.heap.ArenaAllocator,

    l: *lexer.Lexer,
    errors: [][]const u8,

    cur_token: token.Token = undefined,
    peek_token: token.Token = undefined,

    prefixParseFns: std.StringHashMap(prefixParseFn),
    infixParseFns: std.StringHashMap(infixParseFn),

    precedences: std.StringHashMap(OperatorPrecedence),

    pub fn new(l: *lexer.Lexer) !Parser {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        var p = Parser{
            .l = l,
            .arena = arena,
            .errors = &.{},
            .prefixParseFns = std.StringHashMap(prefixParseFn).init(arena.allocator()),
            .infixParseFns = std.StringHashMap(infixParseFn).init(arena.allocator()),
            .precedences = std.StringHashMap(OperatorPrecedence).init(arena.allocator()),
        };

        try p.precedences.put(token.EQ, .EQUALS);
        try p.precedences.put(token.NOT_EQ, .EQUALS);
        try p.precedences.put(token.LT, .LESSGREATER);
        try p.precedences.put(token.GT, .LESSGREATER);
        try p.precedences.put(token.PLUS, .SUM);
        try p.precedences.put(token.MINUS, .SUM);
        try p.precedences.put(token.SLASH, .PRODUCT);
        try p.precedences.put(token.ASTERISK, .PRODUCT);

        try p.registerPrefix(token.IDENT, parseIdentifier);
        try p.registerPrefix(token.INT, parseIntegerLiteral);
        try p.registerPrefix(token.BANG, parsePrefixExpression);
        try p.registerPrefix(token.MINUS, parsePrefixExpression);

        try p.registerInfix(token.PLUS, parseInfixExpression);
        try p.registerInfix(token.MINUS, parseInfixExpression);
        try p.registerInfix(token.SLASH, parseInfixExpression);
        try p.registerInfix(token.ASTERISK, parseInfixExpression);
        try p.registerInfix(token.EQ, parseInfixExpression);
        try p.registerInfix(token.NOT_EQ, parseInfixExpression);
        try p.registerInfix(token.LT, parseInfixExpression);
        try p.registerInfix(token.GT, parseInfixExpression);

        try p.nextToken();
        try p.nextToken();

        return p;
    }

    pub fn deinit(self: *Parser, program: *ast.Program) void {
        self.arena.allocator().free(program.statements);
        self.arena.deinit();
    }

    pub fn getErrors(self: *Parser) [][]const u8 {
        return self.errors;
    }

    pub fn parseProgram(self: *Parser) !ast.Program {
        var program = ast.Program{
            .statements = undefined,
        };

        var statements = std.ArrayList(ast.Statement).init(self.arena.allocator());

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
        const tok = self.cur_token;

        if (!try self.expectPeek(token.IDENT)) {
            return null;
        }

        var name = try self.arena.allocator().create(ast.Identifier);
        name.token = self.cur_token;
        name.value = self.cur_token.literal;

        var stmt: *ast.LetStatement = try self.arena.allocator().create(ast.LetStatement);
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
        var stmt: *ast.ReturnStatement = try self.arena.allocator().create(ast.ReturnStatement);
        stmt.token = self.cur_token;
        stmt.return_value = null;

        try self.nextToken();

        while (!self.curTokenIs(token.SEMICOLON)) {
            try self.nextToken();
        }

        return stmt;
    }

    fn parseExpressionStatement(self: *Parser) !?*ast.ExpressionStatement {
        tracer.trace(@src().fn_name);
        defer tracer.untrace(@src().fn_name);

        var stmt: ?*ast.ExpressionStatement = null;

        if (try self.parseExpression(.LOWEST)) |expression| {
            stmt = try self.arena.allocator().create(ast.ExpressionStatement);
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

    fn noPrefixParseFnError(self: *Parser, token_type: token.TokenType) !void {
        var error_array = try std.ArrayList([]const u8).initCapacity(self.arena.allocator(), self.errors.len);
        try error_array.appendSlice(self.errors);
        try error_array.append(try std.fmt.allocPrint(
            self.arena.allocator(),
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
        var error_array = try std.ArrayList([]const u8).initCapacity(self.arena.allocator(), self.errors.len);
        try error_array.appendSlice(self.errors);

        const message = try std.fmt.allocPrint(
            self.arena.allocator(),
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
    var identifier: *ast.Identifier = try self.arena.allocator().create(ast.Identifier);
    identifier.token = self.cur_token;
    identifier.value = try self.arena.allocator().dupe(u8, self.cur_token.literal);

    return ast.Expression.init(identifier);
}

fn parseIntegerLiteral(self: *Parser) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    var expression: ?ast.Expression = null;

    const opt_value: ?i64 = std.fmt.parseInt(i64, self.cur_token.literal, 10) catch null;
    if (opt_value) |value| {
        var lit: *ast.IntegerLiteral = try self.arena.allocator().create(ast.IntegerLiteral);
        lit.token = self.cur_token;
        lit.value = value;
        expression = ast.Expression.init(lit);
    }

    return expression;
}

fn parsePrefixExpression(self: *Parser) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    var expression: *ast.PrefixExpression = try self.arena.allocator().create(ast.PrefixExpression);
    expression.token = self.cur_token;
    expression.operator = try self.arena.allocator().dupe(u8, self.cur_token.literal);

    try self.nextToken();
    expression.right = try self.parseExpression(.PREFIX);

    return ast.Expression.init(expression);
}

fn parseInfixExpression(self: *Parser, left: ast.Expression) std.mem.Allocator.Error!?ast.Expression {
    tracer.trace(@src().fn_name);
    defer tracer.untrace(@src().fn_name);

    var expression: *ast.InfixExpression = try self.arena.allocator().create(ast.InfixExpression);
    expression.token = self.cur_token;
    expression.operator = try self.arena.allocator().dupe(u8, self.cur_token.literal);
    expression.left = left;

    const precedence = self.curPrecedence();
    try self.nextToken();
    expression.right = try self.parseExpression(@enumFromInt(precedence));

    return ast.Expression.init(expression);
}

const TestSetup = struct {
    parser: Parser,
    lexer: lexer.Lexer,
    program: ast.Program,

    pub fn deinit(self: *TestSetup) void {
        self.lexer.deinit();
        self.parser.deinit(&self.program);
    }
};

fn setupTestParser(input: []const u8) !TestSetup {
    var l = lexer.Lexer.new(input);
    var p = try Parser.new(&l);
    const program = try p.parseProgram();
    return TestSetup{ .parser = p, .lexer = l, .program = program };
}

test "let statements" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(3, setup.program.statements.len);

    try testLetStatement("x", setup.program.statements[0]);
    try testLetStatement("y", setup.program.statements[1]);
    try testLetStatement("foobar", setup.program.statements[2]);
}

fn testLetStatement(expected_name: []const u8, s: ast.Statement) !void {
    try std.testing.expectEqualSlices(u8, "let", s.tokenLiteral());

    const let_stmt: *const ast.LetStatement = @ptrCast(@alignCast(s.ptr));
    try std.testing.expectEqualSlices(u8, expected_name, let_stmt.name.value);
    try std.testing.expectEqualSlices(u8, expected_name, let_stmt.name.tokenLiteral());
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
}

fn testReturnStatement(s: ast.Statement) !void {
    const return_stmt: *const ast.ReturnStatement = @ptrCast(@alignCast(s.ptr));
    try std.testing.expectEqualSlices(u8, "return", return_stmt.name.tokenLiteral());
}

test "identifier expression" {
    const input = "foobar;";

    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(setup.program.statements[0].ptr));
    if (stmt.expression) |expression| {
        const ident: *ast.Identifier = @ptrCast(@alignCast(expression.ptr));
        try std.testing.expectEqualSlices(u8, "foobar", ident.value);
        try std.testing.expectEqualSlices(u8, "foobar", ident.tokenLiteral());
    }
}

test "integerer literal expression" {
    const input = "5;";

    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(setup.program.statements[0].ptr));
    if (stmt.expression) |expression| {
        const literal: *ast.IntegerLiteral = @ptrCast(@alignCast(expression.ptr));
        try std.testing.expectEqual(5, literal.value);
        try std.testing.expectEqualSlices(u8, "5", literal.tokenLiteral());
    }
}

test "prefix expressions" {
    try testPrefixExpression("!5;", "!", 5);
    try testPrefixExpression("-15;", "-", 15);
}

fn testPrefixExpression(input: []const u8, expected_operator: []const u8, expected_value: i64) !void {
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(setup.program.statements[0].ptr));
    if (stmt.expression) |expression| {
        const prefix_expression: *ast.PrefixExpression = @ptrCast(@alignCast(expression.ptr));
        try std.testing.expectEqualSlices(u8, expected_operator, prefix_expression.operator);
        try testIntegerLiteral(prefix_expression.right.?, expected_value);
    }
}

fn testIntegerLiteral(integer_literal: ast.Expression, expected_value: i64) !void {
    const literal: *ast.IntegerLiteral = @ptrCast(@alignCast(integer_literal.ptr));
    try std.testing.expectEqual(expected_value, literal.value);

    var buf: [10]u8 = undefined;
    const expected_value_str = try std.fmt.bufPrint(&buf, "{d}", .{expected_value});
    try std.testing.expectEqualSlices(u8, expected_value_str, literal.tokenLiteral());
}

test "infix expressions" {
    try testInfixExpression("5 + 5;", 5, "+", 5);
    try testInfixExpression("5 - 5;", 5, "-", 5);
    try testInfixExpression("5 * 5;", 5, "*", 5);
    try testInfixExpression("5 / 5;", 5, "/", 5);
    try testInfixExpression("5 > 5;", 5, ">", 5);
    try testInfixExpression("5 < 5;", 5, "<", 5);
    try testInfixExpression("5 == 5;", 5, "==", 5);
    try testInfixExpression("5 != 5;", 5, "!=", 5);
}

fn testInfixExpression(input: []const u8, left_value: i64, operator: []const u8, right_value: i64) !void {
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);
    try std.testing.expectEqual(1, setup.program.statements.len);

    const stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(setup.program.statements[0].ptr));
    if (stmt.expression) |expression| {
        const infix_expression: *ast.InfixExpression = @ptrCast(@alignCast(expression.ptr));
        try testIntegerLiteral(infix_expression.left.?, left_value);
        try std.testing.expectEqualSlices(u8, operator, infix_expression.operator);
        try testIntegerLiteral(infix_expression.right.?, right_value);
    }
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
}

fn testOperatorPrecedenceParsing(input: []const u8, expected: []const u8) !void {
    var setup = try setupTestParser(input);
    defer setup.deinit();

    try expectErrors(&setup.parser, 0);

    try std.testing.expectEqualSlices(u8, expected, setup.program.string(setup.parser.arena.allocator()));
}
