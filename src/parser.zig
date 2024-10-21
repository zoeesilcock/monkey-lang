const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

const Parser = struct {
    l: *lexer.Lexer,
    cur_token: token.Token = undefined,
    peek_token: token.Token = undefined,
    arena: std.heap.ArenaAllocator,

    pub fn new(l: *lexer.Lexer) !Parser {
        const arena = std.heap.ArenaAllocator.init(std.heap.page_allocator); 
        var p = Parser{ .l = l, .arena = arena }; 

        try p.nextToken();
        try p.nextToken();
        
        return p;
    }

    pub fn deinit(self: *Parser, program: *ast.Program) void {
        self.arena.allocator().free(program.statements);
        self.arena.deinit();
    }

    fn nextToken(self: *Parser) !void {
        self.cur_token = self.peek_token;
        self.peek_token = try self.l.nextToken();
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
        } else {
            return null;
        }
    }

    fn parseLetStatement(self: *Parser) !?*ast.LetStatement {
        const tok = self.cur_token;

        if (!try self.expectPeek(token.IDENT)) {
            std.debug.print("no IDENT\n", .{});
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
            std.debug.print("no ASSIGN\n", .{});
            return null;
        }

        while (self.curTokenIs(token.SEMICOLON)) {
            std.debug.print("no SEMICOLON\n", .{});
            try self.nextToken();
        }

        return stmt;
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

        return false;
    }
};

test "test LetStatements" {
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

    try std.testing.expectEqual(program.statements.len, 3);

    try testLetStatement(program.statements[0], "x");
    try testLetStatement(program.statements[1], "y");
    try testLetStatement(program.statements[2], "foobar");
}

fn testLetStatement(s: ast.Statement, name: []const u8) !void {
    try std.testing.expectEqualSlices(u8, s.tokenLiteral(), "let");

    const let_stmt: *const ast.LetStatement = @ptrCast(@alignCast(s.ptr));
    try std.testing.expectEqualSlices(u8, let_stmt.name.value, name);
    try std.testing.expectEqualSlices(u8, let_stmt.name.tokenLiteral(), name);
}
