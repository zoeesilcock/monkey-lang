const std = @import("std");
const token = @import("token.zig");

const NodeType = enum {
    Program,
    Statement,
    Expression,
    LetStatement,
    ReturnStatement,
    ExpressionStatement,
    BlockStatement,
    Identifier,
    IntegerLiteral,
    BooleanLiteral,
    FunctionLiteral,
    ArrayLiteral,
    StringLiteral,
    PrefixExpression,
    InfixExpression,
    IfExpression,
    CallExpression,
    IndexExpression,
};

pub const Node = struct {
    ptr: *anyopaque,
    node_type: NodeType,
    vtab: *const VTab,

    const VTab = struct {
        tokenLiteral: *const fn(ptr: *anyopaque) []const u8,
        string: *const fn (ptr: *anyopaque, allocator: std.mem.Allocator) []const u8,
    };

    pub fn tokenLiteral(self: Node) []const u8 {
        return self.vtab.tokenLiteral(self.ptr);
    }

    pub fn string(self: Node, allocator: std.mem.Allocator) []const u8 {
        return self.vtab.string(self.ptr, allocator);
    }

    pub fn unwrap(self: Node, comptime T: type) *T {
        return @ptrCast(@alignCast(self.ptr));
    }

    pub fn init(obj: anytype) Node {
        const Ptr = @TypeOf(obj);
        const PtrInfo = @typeInfo(Ptr);

        std.debug.assert(PtrInfo == .Pointer);
        std.debug.assert(PtrInfo.Pointer.size == .One);
        std.debug.assert(@typeInfo(PtrInfo.Pointer.child) == .Struct);

        const node_type: NodeType = switch(Ptr) {
            *Program => NodeType.Program,
            *Statement => NodeType.Statement,
            *Expression => NodeType.Expression,
            *LetStatement => NodeType.LetStatement,
            *ReturnStatement => NodeType.ReturnStatement,
            *ExpressionStatement => NodeType.ExpressionStatement,
            *BlockStatement => NodeType.BlockStatement,
            *Identifier => NodeType.Identifier,
            *IntegerLiteral => NodeType.IntegerLiteral,
            *BooleanLiteral => NodeType.BooleanLiteral,
            *FunctionLiteral => NodeType.FunctionLiteral,
            *ArrayLiteral => NodeType.ArrayLiteral,
            *StringLiteral => NodeType.StringLiteral,
            *PrefixExpression => NodeType.PrefixExpression,
            *InfixExpression => NodeType.InfixExpression,
            *IfExpression => NodeType.IfExpression,
            *CallExpression => NodeType.CallExpression,
            *IndexExpression => NodeType.IndexExpression,
            else => {
                std.debug.print("Unsupported Node type: {?}\n", .{ Ptr });
                unreachable;
            },
        };

        const impl = struct {
            fn tokenLiteral(ptr: *anyopaque) []const u8 {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return self.tokenLiteral();
            }
            fn string(ptr: *anyopaque, allocator: std.mem.Allocator) []const u8 {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return self.string(allocator);
            }
        };

        return .{
            .ptr = obj,
            .node_type = node_type,
            .vtab = &.{
                .tokenLiteral = impl.tokenLiteral,
                .string = impl.string,
            },
        };
    }
};

pub const Program = struct {
    statements: []const Statement,

    pub fn tokenLiteral(self: *Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
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

const StatementType = enum {
    LetStatement,
    ReturnStatement,
    ExpressionStatement,
    BlockStatement,
};

pub const Statement = struct {
    ptr: *anyopaque,
    statement_type: StatementType,
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

    pub fn unwrap(self: Statement, comptime T: type) *T {
        return @ptrCast(@alignCast(self.ptr));
    }
 
    pub fn init(obj: anytype) Statement {
        const Ptr = @TypeOf(obj);
        const PtrInfo = @typeInfo(Ptr);

        std.debug.assert(PtrInfo == .Pointer);
        std.debug.assert(PtrInfo.Pointer.size == .One);
        std.debug.assert(@typeInfo(PtrInfo.Pointer.child) == .Struct);

        const statement_type: StatementType = switch(Ptr) {
            *LetStatement => StatementType.LetStatement,
            *ReturnStatement => StatementType.ReturnStatement,
            *ExpressionStatement => StatementType.ExpressionStatement,
            *BlockStatement => StatementType.BlockStatement,
            else => {
                std.debug.print("Unsupported Statement type: {?}\n", .{ Ptr });
                unreachable;
            },
        };

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
            .statement_type = statement_type,
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

pub const BlockStatement = struct {
    token: token.Token,
    statements: []const Statement,

    pub fn tokenLiteral(self: *BlockStatement) []const u8 {
        return self.token.literal;
    }

    pub fn statementNode(self: *BlockStatement) void {
        _ = self;
    }

    pub fn string(self: *BlockStatement, allocator: std.mem.Allocator) []const u8 {
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

pub const ExpressionType = enum {
    Identifier,
    IntegerLiteral,
    BooleanLiteral,
    ArrayLiteral,
    FunctionLiteral,
    StringLiteral,
    PrefixExpression,
    InfixExpression,
    IfExpression,
    CallExpression,
    IndexExpression,
};

pub const Expression = struct {
    ptr: *anyopaque,
    expression_type: ExpressionType,
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

    pub fn unwrap(self: Expression, comptime T: type) *T {
        return @ptrCast(@alignCast(self.ptr));
    }

    pub fn init(obj: anytype) Expression {
        const Ptr = @TypeOf(obj);
        const PtrInfo = @typeInfo(Ptr);

        std.debug.assert(PtrInfo == .Pointer);
        std.debug.assert(PtrInfo.Pointer.size == .One);
        std.debug.assert(@typeInfo(PtrInfo.Pointer.child) == .Struct);

        const expression_type: ExpressionType = switch(Ptr) {
            *Identifier => ExpressionType.Identifier,
            *IntegerLiteral => ExpressionType.IntegerLiteral,
            *BooleanLiteral => ExpressionType.BooleanLiteral,
            *FunctionLiteral => ExpressionType.FunctionLiteral,
            *ArrayLiteral => ExpressionType.ArrayLiteral,
            *StringLiteral => ExpressionType.StringLiteral,
            *PrefixExpression => ExpressionType.PrefixExpression,
            *InfixExpression => ExpressionType.InfixExpression,
            *IfExpression => ExpressionType.IfExpression,
            *CallExpression => ExpressionType.CallExpression,
            *IndexExpression => ExpressionType.IndexExpression,
            else => {
                std.debug.print("Unsupported Expression type: {?}\n", .{ Ptr });
                unreachable;
            },
        };

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
            .expression_type = expression_type,
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

pub const BooleanLiteral = struct {
    token: token.Token,
    value: bool,

    pub fn tokenLiteral(self: *BooleanLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn expressionNode(self: BooleanLiteral) void {
        _ = self;
    }

    pub fn string(self: *BooleanLiteral, allocator: std.mem.Allocator) []const u8 {
        _ = allocator;
        return self.token.literal;
    }
};

pub const FunctionLiteral = struct {
    token: token.Token,
    parameters: []*Identifier,
    body: ?*BlockStatement,

    pub fn tokenLiteral(self: *FunctionLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn expressionNode(self: FunctionLiteral) void {
        _ = self;
    }

    pub fn string(self: *FunctionLiteral, allocator: std.mem.Allocator) []const u8 {
        var out: []u8 = "";

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            "(",
        }) catch "";

        for (self.parameters, 0..) |param, i| {
            out = std.mem.concat(allocator, u8, &.{ 
                out,
                param.string(allocator),
                if (i < self.parameters.len - 1) ", " else "",
            }) catch "";
        }

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            ")",
        }) catch "";

        if (self.body) |body| {
            out = std.mem.concat(allocator, u8, &.{ 
                out,
                body.string(allocator),
            }) catch "";
        }

        return out;
    }
};

pub const StringLiteral = struct {
    token: token.Token,
    value: []const u8,

    pub fn tokenLiteral(self: *StringLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn expressionNode(self: StringLiteral) void {
        _ = self;
    }

    pub fn string(self: *StringLiteral, allocator: std.mem.Allocator) []const u8 {
        _ = allocator;
        return self.token.literal;
    }
};

pub const ArrayLiteral = struct {
    token: token.Token,
    elements: []Expression,

    pub fn tokenLiteral(self: *ArrayLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn expressionNode(self: ArrayLiteral) void {
        _ = self;
    }

    pub fn string(self: *ArrayLiteral, allocator: std.mem.Allocator) []const u8 {
        var out: []u8 = "";

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            "[",
        }) catch "";

        for (self.elements, 0..) |element, i| {
            out = std.mem.concat(allocator, u8, &.{ 
                out,
                element.string(allocator),
                if (i < self.elements.len - 1) ", " else "",
            }) catch "";
        }

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            "]",
        }) catch "";

        return out;
    }
};

pub const IndexExpression = struct {
    token: token.Token,
    left: Expression,
    index: ?Expression,

    pub fn tokenLiteral(self: *IndexExpression) []const u8 {
        return self.token.literal;
    }

    pub fn expressionNode(self: IndexExpression) void {
        _ = self;
    }

    pub fn string(self: *IndexExpression, allocator: std.mem.Allocator) []const u8 {
        var out: []u8 = "";

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            "(",
            self.left.string(allocator),
            "[",
            self.index.?.string(allocator),
            "])",
        }) catch "";

        return out;
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

pub const IfExpression = struct {
    token: token.Token,
    condition: ?Expression,
    consequence: ?*BlockStatement,
    alternative: ?*BlockStatement,

    pub fn tokenLiteral(self: *IfExpression) []const u8 {
        return self.token.literal;
    }

    pub fn expressionNode(self: IfExpression) void {
        _ = self;
    }

    pub fn string(self: *IfExpression, allocator: std.mem.Allocator) []const u8 {
        var out: []u8 = "";

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            "if",
            if (self.condition) |condition| condition.string(allocator) else "",
            " ",
            if (self.consequence) |consequence| consequence.string(allocator) else "",
            " ",
        }) catch "";

        if (self.alternative) |alternative| {
            out = std.mem.concat(allocator, u8, &.{ 
                out,
                "else",
                alternative.string(allocator),
            }) catch "";
        }

        return out;
    }
};

pub const CallExpression = struct {
    token: token.Token,
    function: ?Expression,
    arguments: []Expression,

    pub fn tokenLiteral(self: *CallExpression) []const u8 {
        return self.token.literal;
    }

    pub fn expressionNode(self: CallExpression) void {
        _ = self;
    }

    pub fn string(self: *CallExpression, allocator: std.mem.Allocator) []const u8 {
        var out: []u8 = "";

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            if (self.function) |function| function.string(allocator) else "",
            "(",
        }) catch "";

        for (self.arguments, 0..) |arg, i| {
            out = std.mem.concat(allocator, u8, &.{ 
                out,
                arg.string(allocator),
                if (i < self.arguments.len - 1) ", " else "",
            }) catch "";
        }

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            ")",
        }) catch "";

        return out;
    }
};
