const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const object = @import("object.zig");
const parser = @import("parser.zig");

fn eval(node: ast.Node) ?object.Object {
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
            .IntegerLiteral => {
                const e: *ast.IntegerLiteral = expression.unwrap(ast.IntegerLiteral);
                result = eval(ast.Node.init(e));
            },
            else => unreachable,
        }
    }

    return result;
}

fn evalStatements(stmts: []const ast.Statement) ?object.Object {
    var result: ?object.Object = null;

    for (stmts) |stmt| {
        switch (stmt.statement_type) {
            .ExpressionStatement => {
                const s = stmt.unwrap(ast.ExpressionStatement);
                result = eval(ast.Node.init(s));
            },
            else => unreachable,
        }
    }

    return result;
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
