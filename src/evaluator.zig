const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const object = @import("object.zig");
const parser = @import("parser.zig");

const NULL = &object.Null{};
const TRUE = &object.Boolean{ .value = true };
const FALSE = &object.Boolean{ .value = false };

pub fn eval(node: ast.Node, allocator: std.mem.Allocator) std.mem.Allocator.Error!?object.Object {
    switch (node.node_type) {
        .Program => {
            const program: *ast.Program = node.unwrap(ast.Program);
            return try evalStatements(program.statements, allocator);
        },
        .ExpressionStatement => {
            const stmt: *ast.ExpressionStatement = node.unwrap(ast.ExpressionStatement);
            return evalExpression(stmt.expression, allocator);
        },
        .IntegerLiteral => {
            var integer: *object.Integer = try allocator.create(object.Integer);
            integer.value = node.unwrap(ast.IntegerLiteral).value;
            return object.Object.init(integer);
        },
        .BooleanLiteral => {
            const boolean: *object.Boolean = @constCast(nativeBoolToBooleanObject(node.unwrap(ast.BooleanLiteral).value));
            return object.Object.init(boolean);
        },
        .PrefixExpression => {
            const expression: *ast.PrefixExpression = node.unwrap(ast.PrefixExpression);

            if (expression.right) |right| {
                return try evalPrefixExpression(expression.operator, try evalExpression(right, allocator), allocator);
            }

            return null;
        },
        .InfixExpression => {
            const expression: *ast.InfixExpression = node.unwrap(ast.InfixExpression);

            if (expression.left) |left| {
                if (expression.right) |right| {
                    return try evalInfixExpression(
                        expression.operator,
                        try evalExpression(left, allocator),
                        try evalExpression(right, allocator),
                        allocator,
                    );
                }
            }

            return null;
        },
        else => {
            std.debug.print("Unexpected Node type: {?}\n", .{node.node_type});
            return null;
        },
    }
}

fn evalExpression(opt_expression: ?ast.Expression, allocator: std.mem.Allocator) !?object.Object {
    var result: ?object.Object = null;

    if (opt_expression) |*expression| {
        switch (expression.expression_type) {
            .IntegerLiteral => result = try eval(ast.Node.init(expression.unwrap(ast.IntegerLiteral)), allocator),
            .BooleanLiteral => result = try eval(ast.Node.init(expression.unwrap(ast.BooleanLiteral)), allocator),
            .PrefixExpression => result = try eval(ast.Node.init(expression.unwrap(ast.PrefixExpression)), allocator),
            .InfixExpression => result = try eval(ast.Node.init(expression.unwrap(ast.InfixExpression)), allocator),
            else => {
                std.debug.print("Unexpected expression type in evalExpression: {?}\n", .{expression.expression_type});
                unreachable;
            },
        }
    }

    return result;
}

fn evalStatements(stmts: []const ast.Statement, allocator: std.mem.Allocator) !?object.Object {
    var result: ?object.Object = null;

    for (stmts) |stmt| {
        switch (stmt.statement_type) {
            .ExpressionStatement => result = try eval(ast.Node.init(stmt.unwrap(ast.ExpressionStatement)), allocator),
            else => unreachable,
        }
    }

    return result;
}

fn evalPrefixExpression(operator: []const u8, opt_right: ?object.Object, allocator: std.mem.Allocator) !?object.Object {
    var result: ?object.Object = null;

    if (opt_right) |right| {
        if (std.mem.eql(u8, operator, "!")) {
            result = evalBangOperatorExpression(right);
        } else if (std.mem.eql(u8, operator, "-")) {
            result = try evalMinusPrefixOperatorExpression(right, allocator);
        }
    }

    return result;
}

fn evalBangOperatorExpression(right: object.Object) ?object.Object {
    var result: ?object.Object = null;
    var result_boolean: *object.Boolean = undefined;

    switch (right.inner_type) {
        .Boolean => {
            const boolean: *object.Boolean = right.unwrap(object.Boolean);
            result_boolean = if (boolean.value) @constCast(FALSE) else @constCast(TRUE);
        },
        .Null => result_boolean = @constCast(TRUE),
        else => result_boolean = @constCast(FALSE),
    }

    result = object.Object.init(result_boolean);

    return result;
}

fn evalMinusPrefixOperatorExpression(right: object.Object, allocator: std.mem.Allocator) !?object.Object {
    var result: ?object.Object = null;

    if (right.inner_type == .Integer) {
        const integer: *object.Integer = right.unwrap(object.Integer);
        var result_integer: *object.Integer = try allocator.create(object.Integer);
        result_integer.value = -integer.value;
        result = object.Object.init(result_integer);
    }

    return result;
}

fn evalInfixExpression(
    operator: []const u8,
    opt_left: ?object.Object,
    opt_right: ?object.Object,
    allocator: std.mem.Allocator,
) !?object.Object {
    var result: ?object.Object = null;

    if (opt_left) |left| {
        if (opt_right) |right| {
            if (left.inner_type == .Integer and right.inner_type == .Integer) {
                result = try evalIntegerInfixExpression(operator, left, right, allocator);
            }
        }
    }

    return result;
}

fn evalIntegerInfixExpression(
    operator: []const u8,
    left: object.Object,
    right: object.Object,
    allocator: std.mem.Allocator,
) !?object.Object {
    var result: ?object.Object = null;
    var result_integer: ?*object.Integer = null;
    var result_boolean: ?*object.Boolean = null;

    const left_value: i64 = left.unwrap(object.Integer).value;
    const right_value: i64 = right.unwrap(object.Integer).value;

    if (std.mem.eql(u8, operator, "+")) {
        result_integer = try allocator.create(object.Integer);
        result_integer.?.value = left_value + right_value;
    } else if (std.mem.eql(u8, operator, "-")) {
        result_integer = try allocator.create(object.Integer);
        result_integer.?.value = left_value - right_value;
    } else if (std.mem.eql(u8, operator, "*")) {
        result_integer = try allocator.create(object.Integer);
        result_integer.?.value = left_value * right_value;
    } else if (std.mem.eql(u8, operator, "/")) {
        result_integer = try allocator.create(object.Integer);
        result_integer.?.value = @divFloor(left_value, right_value);
    } else if (std.mem.eql(u8, operator, "<")) {
        result_boolean = @constCast(nativeBoolToBooleanObject(left_value < right_value));
    } else if (std.mem.eql(u8, operator, ">")) {
        result_boolean = @constCast(nativeBoolToBooleanObject(left_value > right_value));
    } else if (std.mem.eql(u8, operator, "==")) {
        result_boolean = @constCast(nativeBoolToBooleanObject(left_value == right_value));
    } else if (std.mem.eql(u8, operator, "!=")) {
        result_boolean = @constCast(nativeBoolToBooleanObject(left_value != right_value));
    }

    if (result_integer) |integer| {
        result = object.Object.init(integer);
    } else if (result_boolean) |boolean| {
        result = object.Object.init(boolean);
    }

    return result;
}

fn nativeBoolToBooleanObject(input: bool) *const object.Boolean {
    return if (input) TRUE else FALSE;
}

test "eval integer expression" {
    try testEvalInteger("5", 5);
    try testEvalInteger("10", 10);
    try testEvalInteger("-5", -5);
    try testEvalInteger("-10", -10);
    try testEvalInteger("5 + 5 + 5 + 5 - 10", 10);
    try testEvalInteger("2 * 2 * 2 * 2 * 2", 32);
    try testEvalInteger("-50 + 100 + -50", 0);
    try testEvalInteger("5 * 2 + 10", 20);
    try testEvalInteger("5 + 2 * 10", 25);
    try testEvalInteger("20 + 2 * -10", 0);
    try testEvalInteger("50 / 2 * 2 + 10", 60);
    try testEvalInteger("2 * (5 + 10)", 30);
    try testEvalInteger("3 * 3 * 3 + 10", 37);
    try testEvalInteger("3 * (3 * 3) + 10", 37);
    try testEvalInteger("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50);
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

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    return try eval(ast.Node.init(&program), arena.allocator());
}

fn testIntegerObject(obj: object.Object, expected_value: i64) !void {
    try std.testing.expectEqual(obj.inner_type, .Integer);

    const integer: *object.Integer = obj.unwrap(object.Integer);

    try std.testing.expectEqual(expected_value, integer.value);
}

test "eval boolean expression" {
    try testEvalBoolean("true", true);
    try testEvalBoolean("false", false);
    try testEvalBoolean("1 < 2", true);
    try testEvalBoolean("1 > 2", false);
    try testEvalBoolean("1 < 1", false);
    try testEvalBoolean("1 > 1", false);
    try testEvalBoolean("1 == 1", true);
    try testEvalBoolean("1 != 1", false);
    try testEvalBoolean("1 == 2", false);
    try testEvalBoolean("1 != 2", true);
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

test "bang operator" {
    try testBangOperator("!true", false);
    try testBangOperator("!false", true);
    try testBangOperator("!5", false);
    try testBangOperator("!!true", true);
    try testBangOperator("!!false", false);
    try testBangOperator("!!5", true);
}

fn testBangOperator(input: []const u8, expected_value: bool) !void {
    if (try testEval(input)) |evaluated| {
        try testBooleanObject(evaluated, expected_value);
    } else {
        unreachable;
    }
}
