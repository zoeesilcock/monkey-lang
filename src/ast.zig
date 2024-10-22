const std = @import("std");
const token = @import("token.zig");

pub const Program = struct {
    statements: []Statement,

    pub fn tokenLiteral(self: *Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenliteral();
        } else {
            return "";
        }
    }
};

pub const Statement = struct {
    ptr: *anyopaque,
    vtab: *const VTab,

    const VTab = struct {
        tokenLiteral: *const fn (ptr: *anyopaque) []const u8,
        statementNode: *const fn (ptr: *anyopaque) void,
    };

    pub fn tokenLiteral(self: Statement) []const u8 {
        return self.vtab.tokenLiteral(self.ptr);
    }

    pub fn statementNode(self: Statement) void {
        self.vtab.statementNode(self.ptr);
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
        };

        return .{
            .ptr = obj,
            .vtab = &.{
                .tokenLiteral = impl.tokenLiteral,
                .statementNode = impl.statementNode,
            },
        };
    }
};

pub const LetStatement = struct {
    token: token.Token,
    name: *Identifier,
    value: Expression,

    pub fn tokenLiteral(self: *LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn statementNode(self: *LetStatement) void {
        _ = self;
    }
};

pub const ReturnStatement = struct {
    token: token.Token,
    name: *Identifier,
    return_value: Expression,

    pub fn tokenLiteral(self: *ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn statementNode(self: *ReturnStatement) void {
        _ = self;
    }
};

pub const Identifier = struct {
    token: token.Token,
    value: []const u8,

    pub fn tokenLiteral(self: *Identifier) []const u8 {
        return self.token.literal;
    }
};

pub const Expression = struct {
    ptr: *anyopaque,
    vtab: *const VTab,

    const VTab = struct {
        tokenLiteral: *const fn(ptr: *anyopaque) []const u8,
        expressionNode: *const fn (ptr: *anyopaque) void,
    };

    pub fn tokenLiteral(self: Expression) []const u8 {
        return self.vtab.tokenLiteral(self.ptr);
    }

    pub fn expressionNode(self: Expression) void {
        self.vtab.expressionNode(self.ptr);
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
        };

        return .{
            .ptr = obj,
            .vtab = &.{
                .tokenLiteral = impl.tokenLiteral,
                .expressionNode = impl.expressionNode,
            },
        };
    }
};

