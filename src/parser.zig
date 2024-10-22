const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

const Parser = struct {
    l: *lexer.Lexer,
    cur_token: token.Token = undefined,
    peek_token: token.Token = undefined,
    arena: std.heap.ArenaAllocator,
    errors: [][]const u8,

    pub fn new(l: *lexer.Lexer) !Parser {
        const arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        var p = Parser{
            .l = l,
            .arena = arena,
            .errors = &.{},
        };

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
            return null;
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
        stmt.value = undefined;

        if (!try self.expectPeek(token.ASSIGN)) {
            return null;
        }

        while (self.curTokenIs(token.SEMICOLON)) {
            try self.nextToken();
        }

        return stmt;
    }

    fn parseReturnStatement(self: *Parser) !?*ast.ReturnStatement {
        var stmt: *ast.ReturnStatement = try self.arena.allocator().create(ast.ReturnStatement);
        stmt.token = self.cur_token;

        try self.nextToken();

        while (!self.curTokenIs(token.SEMICOLON)) {
            try self.nextToken();
        }
    
        return stmt;
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
};

test "test let statements" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    var l = lexer.Lexer.new(input);
    defer l.deinit();

    var p = try Parser.new(&l);

    var program = try p.parseProgram();
    defer p.deinit(&program);

    try expectErrors(&p, 0);

    try std.testing.expectEqual(program.statements.len, 3);

    try testLetStatement(program.statements[0], "x");
    try testLetStatement(program.statements[1], "y");
    try testLetStatement(program.statements[2], "foobar");
}

fn testLetStatement(s: ast.Statement, expected_name: []const u8) !void {
    try std.testing.expectEqualSlices(u8, s.tokenLiteral(), "let");

    const let_stmt: *const ast.LetStatement = @ptrCast(@alignCast(s.ptr));
    try std.testing.expectEqualSlices(u8, expected_name, let_stmt.name.value);
    try std.testing.expectEqualSlices(u8, expected_name, let_stmt.name.tokenLiteral());
}

test "test parser errors for let statements" {
    const input = 
        \\let x 5;
        \\let = 10;
        \\let 838383;
    ;

    var l = lexer.Lexer.new(input);
    defer l.deinit();

    var p = try Parser.new(&l);

    var program = try p.parseProgram();
    defer p.deinit(&program);

    try expectErrors(&p, 3);
}

fn expectErrors(parser: *Parser, error_count: u32) !void {
    const errors = parser.getErrors();

    if (errors.len != error_count) {
        std.debug.print("parser has {d} unexpected errors\n", .{ errors.len });

        for (errors) |err| {
            std.debug.print("parser error: {s}", .{ err });
        }
    }

    try std.testing.expectEqual(error_count, errors.len);
}

test "test return statements" {
    const input = 
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var l = lexer.Lexer.new(input);
    defer l.deinit();

    var p = try Parser.new(&l);
    var program = try p.parseProgram();
    defer p.deinit(&program);

    try expectErrors(&p, 0);

    try std.testing.expectEqual(3, program.statements.len);
}

fn testReturnStatement(s: ast.Statement) !void {
    const return_stmt: *const ast.ReturnStatement = @ptrCast(@alignCast(s.ptr));
    try std.testing.expectEqualSlices(u8, "return", return_stmt.name.tokenLiteral());
}
