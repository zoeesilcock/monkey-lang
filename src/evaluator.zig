const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const object = @import("object.zig");
const parser = @import("parser.zig");

const NULL = &object.Null{};
const TRUE = &object.Boolean{ .value = true };
const FALSE = &object.Boolean{ .value = false };

pub fn eval(node: ast.Node) ?object.Object {
    switch (node.node_type) {
        .Program => {
            const program: *ast.Program = node.unwrap(ast.Program);
            return evalStatements(program.statements);
        },
        .ExpressionStatement => {
            const stmt: *ast.ExpressionStatement = node.unwrap(ast.ExpressionStatement);
            return evalExpression(stmt.expression);
        },
        .IntegerLiteral => {
            var integer = object.Integer{ .value = node.unwrap(ast.IntegerLiteral).value };
            return object.Object.init(&integer);
        },
        .BooleanLiteral => {
            const boolean: *object.Boolean = @constCast(nativeBoolToBooleanObject(node.unwrap(ast.BooleanLiteral).value));
            return object.Object.init(boolean);
        },
        else => {
            std.debug.print("Unexpected Node type: {?}\n", .{ node.node_type });
            return null;
        }
    }
}

fn evalExpression(opt_expression: ?ast.Expression) ?object.Object {
    var result: ?object.Object = null;

    if (opt_expression) |*expression| {
        switch (expression.expression_type) {
            .IntegerLiteral => result = eval(ast.Node.init(expression.unwrap(ast.IntegerLiteral))),
            .BooleanLiteral => result = eval(ast.Node.init(expression.unwrap(ast.BooleanLiteral))),
            else => unreachable,
        }
    }

    return result;
}

fn evalStatements(stmts: []const ast.Statement) ?object.Object {
    var result: ?object.Object = null;

    for (stmts) |stmt| {
        switch (stmt.statement_type) {
            .ExpressionStatement => result = eval(ast.Node.init(stmt.unwrap(ast.ExpressionStatement))),
            else => unreachable,
        }
    }

    return result;
}

fn nativeBoolToBooleanObject(input: bool) *const object.Boolean {
    return if (input) TRUE else FALSE;
}

test "eval integer expression" {
    try testEvalInteger("5", 5);
    try testEvalInteger("10", 10);
}

fn testEvalInteger(input: []const u8, expected_value: i64) !void {
    if (try testEval(input)) |evaluated| {
        try testIntegerObject(evaluated, expected_value);
    } else {
        unreachable;
    }
}

fn testEval(input: []const u8) !?object.Object {
    var l = lexer.Lexer.new(input);
    var p = try parser.Parser.new(&l);
    var program = try p.parseProgram();

    return eval(ast.Node.init(&program));
}

fn testIntegerObject(obj: object.Object, expected_value: i64) !void {
    try std.testing.expectEqual(obj.inner_type, .Integer);

    const integer: *object.Integer = obj.unwrap(object.Integer);

    try std.testing.expectEqual(expected_value, integer.value);
}

test "eval boolean expression" {
    try testEvalBoolean("true", true);
    try testEvalBoolean("false", false);
}

fn testEvalBoolean(input: []const u8, expected_value: bool) !void {
    if (try testEval(input)) |evaluated| {
        try testBooleanObject(evaluated, expected_value);
    } else {
        unreachable;
    }
}

fn testBooleanObject(obj: object.Object, expected_value: bool) !void {
    try std.testing.expectEqual(obj.inner_type, .Boolean);

    const boolean: *object.Boolean = obj.unwrap(object.Boolean);

    try std.testing.expectEqual(expected_value, boolean.value);
}
