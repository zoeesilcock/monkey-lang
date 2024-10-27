const std = @import("std");
const token = @import("token.zig");

pub const Program = struct {
    statements: []const Statement,

    pub fn tokenLiteral(self: *Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenliteral();
        } else {
            return "";
        }
    }

    pub fn string(self: *const Program, allocator: std.mem.Allocator) []const u8 {
        var out: []u8 = "";

        for (self.statements) |stmt| {
            out = std.mem.concat(allocator, u8, &.{ 
                out,
                stmt.string(allocator),
            }) catch "";
        }

        return out;
    }
};

test "Program.string" {
    const program = Program{
        .statements = &.{
            Statement.init(@constCast(&LetStatement{
                .token = token.Token{ .token_type = token.LET, .literal = "let" },
                .name = @constCast(&Identifier{
                    .token = token.Token{ .token_type = token.IDENT, .literal = "myVar" },
                    .value = "myVar",
                }),
                .value = Expression.init(@constCast(&Identifier{
                    .token = token.Token{ .token_type = token.IDENT, .literal = "anotherVar" },
                    .value = "anotherVar",
                })),
            })),
        },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const test_string = program.string(arena.allocator());
    defer arena.deinit();

    try std.testing.expectEqualSlices(u8, "let myVar = anotherVar;", test_string);
}

pub const Statement = struct {
    ptr: *anyopaque,
    vtab: *const VTab,

    const VTab = struct {
        tokenLiteral: *const fn (ptr: *anyopaque) []const u8,
        statementNode: *const fn (ptr: *anyopaque) void,
        string: *const fn (ptr: *anyopaque, allocator: std.mem.Allocator) []const u8,
    };

    pub fn tokenLiteral(self: Statement) []const u8 {
        return self.vtab.tokenLiteral(self.ptr);
    }

    pub fn statementNode(self: Statement) void {
        self.vtab.statementNode(self.ptr);
    }

    pub fn string(self: Statement, allocator: std.mem.Allocator) []const u8 {
        return self.vtab.string(self.ptr, allocator);
    }

    pub fn init(obj: anytype) Statement {
        const Ptr = @TypeOf(obj);
        const PtrInfo = @typeInfo(Ptr);

        std.debug.assert(PtrInfo == .Pointer);
        std.debug.assert(PtrInfo.Pointer.size == .One);
        std.debug.assert(@typeInfo(PtrInfo.Pointer.child) == .Struct);

        const impl = struct {
            fn tokenLiteral(ptr: *anyopaque) []const u8 {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return self.tokenLiteral();
            }
            fn statementNode(ptr: *anyopaque) void {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                self.statementNode();
            }
            fn string(ptr: *anyopaque, allocator: std.mem.Allocator) []const u8 {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return self.string(allocator);
            }
        };

        return .{
            .ptr = obj,
            .vtab = &.{
                .tokenLiteral = impl.tokenLiteral,
                .statementNode = impl.statementNode,
                .string = impl.string,
            },
        };
    }
};

pub const LetStatement = struct {
    token: token.Token,
    name: *Identifier,
    value: ?Expression,

    pub fn tokenLiteral(self: *LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn statementNode(self: *LetStatement) void {
        _ = self;
    }

    pub fn string(self: *LetStatement, allocator: std.mem.Allocator) []const u8 {
        var out: []u8 = "";

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            self.tokenLiteral(),
            " ",
            self.name.string(allocator),
            " = ",
            if (self.value) |value| value.string(allocator) else "",
            ";",
        }) catch "";

        return out;
    }
};

pub const ReturnStatement = struct {
    token: token.Token,
    return_value: ?Expression,

    pub fn tokenLiteral(self: *ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn statementNode(self: *ReturnStatement) void {
        _ = self;
    }

    pub fn string(self: *ReturnStatement, allocator: std.mem.Allocator) []const u8 {
        var out: []u8 = "";

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            self.tokenLiteral(),
            " ",
            if (self.return_value) |return_value| return_value.string(allocator) else "",
            ";",
        }) catch "";

        return out;
    }
};

pub const ExpressionStatement = struct {
    token: token.Token,
    expression: ?Expression,

    pub fn tokenLiteral(self: *ExpressionStatement) []const u8 {
        if (self.expression) |expression| {
            return expression.tokenLiteral();
        }

        return "";
    }

    pub fn statementNode(self: *ExpressionStatement) void {
        _ = self;
    }

    pub fn string(self: *ExpressionStatement, allocator: std.mem.Allocator) []const u8 {
        if (self.expression) |expression| {
            return expression.string(allocator);
        }

        return "";
    }
};

pub const Expression = struct {
    ptr: *anyopaque,
    vtab: *const VTab,

    const VTab = struct {
        tokenLiteral: *const fn(ptr: *anyopaque) []const u8,
        expressionNode: *const fn (ptr: *anyopaque) void,
        string: *const fn (ptr: *anyopaque, allocator: std.mem.Allocator) []const u8,
    };

    pub fn tokenLiteral(self: Expression) []const u8 {
        return self.vtab.tokenLiteral(self.ptr);
    }

    pub fn expressionNode(self: Expression) void {
        self.vtab.expressionNode(self.ptr);
    }

    pub fn string(self: Expression, allocator: std.mem.Allocator) []const u8 {
        return self.vtab.string(self.ptr, allocator);
    }

    pub fn init(obj: anytype) Expression {
        const Ptr = @TypeOf(obj);
        const PtrInfo = @typeInfo(Ptr);

        std.debug.assert(PtrInfo == .Pointer);
        std.debug.assert(PtrInfo.Pointer.size == .One);
        std.debug.assert(@typeInfo(PtrInfo.Pointer.child) == .Struct);

        const impl = struct {
            fn tokenLiteral(ptr: *anyopaque) []const u8 {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return self.tokenLiteral();
            }
            fn expressionNode(ptr: *anyopaque) void {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                self.expressionNode();
            }
            fn string(ptr: *anyopaque, allocator: std.mem.Allocator) []const u8 {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return self.string(allocator);
            }
        };

        return .{
            .ptr = obj,
            .vtab = &.{
                .tokenLiteral = impl.tokenLiteral,
                .expressionNode = impl.expressionNode,
                .string = impl.string,
            },
        };
    }
};

pub const Identifier = struct {
    token: token.Token,
    value: []const u8,

    pub fn tokenLiteral(self: *Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn expressionNode(self: Identifier) void {
        _ = self;
    }

    pub fn string(self: *Identifier, allocator: std.mem.Allocator) []const u8 {
        _ = allocator;
        return self.value;
    }
};

pub const IntegerLiteral = struct {
    token: token.Token,
    value: i64,

    pub fn tokenLiteral(self: *IntegerLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn expressionNode(self: IntegerLiteral) void {
        _ = self;
    }

    pub fn string(self: *IntegerLiteral, allocator: std.mem.Allocator) []const u8 {
        _ = allocator;
        return self.token.literal;
    }
};

pub const PrefixExpression = struct {
    token: token.Token,
    operator: []const u8,
    right: ?Expression,

    pub fn tokenLiteral(self: *PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn expressionNode(self: PrefixExpression) void {
        _ = self;
    }

    pub fn string(self: *PrefixExpression, allocator: std.mem.Allocator) []const u8 {
        var out: []u8 = "";

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            "(",
            self.operator,
            if (self.right) |right_expression| right_expression.string(allocator) else "",
            ")",
        }) catch "";

        return out;
    }
};

pub const InfixExpression = struct {
    token: token.Token,
    left: ?Expression,
    operator: []const u8,
    right: ?Expression,

    pub fn tokenLiteral(self: *InfixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn expressionNode(self: InfixExpression) void {
        _ = self;
    }

    pub fn string(self: *InfixExpression, allocator: std.mem.Allocator) []const u8 {
        var out: []u8 = "";

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            "(",
            if (self.left) |left_expression| left_expression.string(allocator) else "",
            " ",
            self.operator,
            " ",
            if (self.right) |right_expression| right_expression.string(allocator) else "",
            ")",
        }) catch "";

        return out;
    }
};
