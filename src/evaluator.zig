const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const object = @import("object.zig");
const parser = @import("parser.zig");

const NULL = &object.Null{};
const TRUE = &object.Boolean{ .value = true };
const FALSE = &object.Boolean{ .value = false };

fn newError(comptime format: []const u8, args: anytype, allocator: std.mem.Allocator) !object.Object {
    var result: *object.Error = try allocator.create(object.Error);
    result.message = try std.fmt.allocPrint(allocator, format, args);
    return object.Object.init(result);
}

fn isError(opt_object: ?object.Object) bool {
    var result = false;

    if (opt_object) |obj| {
        result = std.mem.eql(u8, obj.objectType(), object.ERROR_OBJ);
    }

    return result;
}

pub fn eval(node: ast.Node, allocator: std.mem.Allocator) std.mem.Allocator.Error!?object.Object {
    switch (node.node_type) {
        .Program => {
            const program: *ast.Program = node.unwrap(ast.Program);
            return try evalProgram(program, allocator);
        },
        .BlockStatement => {
            const block: *ast.BlockStatement = node.unwrap(ast.BlockStatement);
            return try evalBlockStatement(block, allocator);
        },
        .ExpressionStatement => {
            const stmt: *ast.ExpressionStatement = node.unwrap(ast.ExpressionStatement);
            return evalExpression(stmt.expression, allocator);
        },
        .ReturnStatement => {
            const return_stmt: *ast.ReturnStatement = node.unwrap(ast.ReturnStatement);
            if (return_stmt.return_value) |return_value| {
                const opt_value = try evalExpression(return_value, allocator);

                if (isError(opt_value)) {
                    return opt_value;
                }

                if (opt_value) |value| {
                    var result: *object.ReturnValue = try allocator.create(object.ReturnValue);
                    result.value = value;
                    return object.Object.init(result);
                }
            }

            return null;
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
            const right = try evalExpression(expression.right, allocator);

            if (isError(right)) {
                return right;
            }

            return try evalPrefixExpression(expression.operator, right, allocator);
        },
        .InfixExpression => {
            const expression: *ast.InfixExpression = node.unwrap(ast.InfixExpression);
            const right = try evalExpression(expression.right, allocator);
            const left = try evalExpression(expression.left, allocator);

            if (isError(left)) {
                return left;
            }

            if (isError(right)) {
                return right;
            }

            return try evalInfixExpression(expression.operator, left, right, allocator);
        },
        .IfExpression => {
            const if_expression: *ast.IfExpression = node.unwrap(ast.IfExpression);
            return try evalIfExpression(if_expression, allocator);
        },
        else => {
            std.debug.print("Unexpected Node type: {?}\n", .{node.node_type});
            return null;
        },
    }
}

fn evalProgram(program: *ast.Program, allocator: std.mem.Allocator) !?object.Object {
    var result: ?object.Object = null;

    for (program.statements) |*stmt| {
        if (try evalStatement(stmt, allocator)) |evaluated| {
            result = evaluated;

            if (evaluated.inner_type == object.ObjectInnerType.ReturnValue) {
                const return_value: *object.ReturnValue = evaluated.unwrap(object.ReturnValue);
                return return_value.value;
            } else if (evaluated.inner_type == object.ObjectInnerType.Error) {
                return evaluated;
            }
        }
    }

    return result;
}

fn evalBlockStatement(block: *ast.BlockStatement, allocator: std.mem.Allocator) !?object.Object {
    var result: ?object.Object = null;

    for (block.statements) |*stmt| {
        result = try evalStatement(stmt, allocator);

        if (result) |res| {
            if (std.mem.eql(u8, res.objectType(), object.RETURN_VALUE_OBJ) or
                std.mem.eql(u8, res.objectType(), object.ERROR_OBJ)
            ) {
                return result;
            }
        }
    }

    return result;
}

fn evalStatement(stmt: *const ast.Statement, allocator: std.mem.Allocator) !?object.Object {
    var result: ?object.Object = null;

    switch (stmt.statement_type) {
        .ExpressionStatement => result = try eval(ast.Node.init(stmt.unwrap(ast.ExpressionStatement)), allocator),
        .ReturnStatement => result = try eval(ast.Node.init(stmt.unwrap(ast.ReturnStatement)), allocator),
        else => {
            std.debug.print("Unexpected statement type in evalStatements: {?}\n", .{stmt.statement_type});
            unreachable;
        },
    }

    return result;
}

fn evalExpression(opt_expression: ?ast.Expression, allocator: std.mem.Allocator) !?object.Object {
    var result: ?object.Object = null;

    if (opt_expression) |*expression| {
        switch (expression.expression_type) {
            .IntegerLiteral => result = try eval(ast.Node.init(expression.unwrap(ast.IntegerLiteral)), allocator),
            .BooleanLiteral => result = try eval(ast.Node.init(expression.unwrap(ast.BooleanLiteral)), allocator),
            .PrefixExpression => result = try eval(ast.Node.init(expression.unwrap(ast.PrefixExpression)), allocator),
            .InfixExpression => result = try eval(ast.Node.init(expression.unwrap(ast.InfixExpression)), allocator),
            .IfExpression => result = try eval(ast.Node.init(expression.unwrap(ast.IfExpression)), allocator),
            else => {
                std.debug.print("Unexpected expression type in evalExpression: {?}\n", .{expression.expression_type});
                unreachable;
            },
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
        } else {
            return try newError("unknown operator: {s}{s}", .{ operator, right.objectType() }, allocator);
        }
    }

    return result;
}

fn evalBangOperatorExpression(right: object.Object) ?object.Object {
    var result: ?object.Object = null;
    var result_boolean: *object.Boolean = undefined;

    switch (right.inner_type) {
        .Boolean => {
            result_boolean = if (@intFromPtr(right.ptr) == @intFromPtr(TRUE)) @constCast(FALSE) else @constCast(TRUE);
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
    } else {
        return try newError("unknown operator: -{s}", .{ right.objectType() }, allocator);
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

    if (opt_left) |*left| {
        if (opt_right) |*right| {
            if (!std.mem.eql(u8, left.objectType(), right.objectType())) {
                return try newError("type mismatch: {s} {s} {s}", .{ left.objectType(), operator, right.objectType() }, allocator);
            }

            if (left.inner_type == .Integer and right.inner_type == .Integer) {
                result = try evalIntegerInfixExpression(operator, left, right, allocator);
            } else if (std.mem.eql(u8, operator, "==")) {
                const boolean: *object.Boolean = @constCast(nativeBoolToBooleanObject(left.ptr == right.ptr));
                result = object.Object.init(boolean);
            } else if (std.mem.eql(u8, operator, "!=")) {
                const boolean: *object.Boolean = @constCast(nativeBoolToBooleanObject(left.ptr != right.ptr));
                result = object.Object.init(boolean);
            } else {
                return try newError("unknown operator: {s} {s} {s}", .{ left.objectType(), operator, right.objectType() }, allocator);
            }
        }
    }

    return result;
}

fn evalIntegerInfixExpression(
    operator: []const u8,
    left: *const object.Object,
    right: *const object.Object,
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
    } else {
        return try newError("unknown operator: {s} {s} {s}", .{ left.objectType(), operator, right.objectType() }, allocator);
    }

    if (result_integer) |integer| {
        result = object.Object.init(integer);
    } else if (result_boolean) |boolean| {
        result = object.Object.init(boolean);
    }

    return result;
}

fn evalIfExpression(if_expression: *ast.IfExpression, allocator: std.mem.Allocator) !?object.Object {
    var result: ?object.Object = null;
    const opt_condition = try evalExpression(if_expression.condition, allocator);

    if (isError(opt_condition)) {
        return opt_condition;
    }

    if (opt_condition) |condition| {
        if (isTruthy(condition)) {
            if (if_expression.consequence) |consequence| {
                result = try eval(ast.Node.init(consequence), allocator);
            }
        } else {
            if (if_expression.alternative) |alternative| {
                result = try eval(ast.Node.init(alternative), allocator);
            }
        }
    }

    return result;
}

fn isTruthy(obj: object.Object) bool {
    if (@intFromPtr(obj.ptr) == @intFromPtr(NULL)) {
        return false;
    } else if (@intFromPtr(obj.ptr) == @intFromPtr(TRUE)) {
        return true;
    } else if (@intFromPtr(obj.ptr) == @intFromPtr(FALSE)) {
        return false;
    } else {
        return true;
    }
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
    try testEvalBoolean("true == true", true);
    try testEvalBoolean("false == false", true);
    try testEvalBoolean("true == false", false);
    try testEvalBoolean("true != false", true);
    try testEvalBoolean("false != true", true);
    try testEvalBoolean("(1 < 2) == true", true);
    try testEvalBoolean("(1 < 2) == false", false);
    try testEvalBoolean("(1 > 2) == true", false);
    try testEvalBoolean("(1 > 2) == false", true);
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

test "if/else expressions" {
    try testIfElseExpression("if (true) { 10 }", .{ .int_value = 10 });
    try testIfElseExpression("if (false) { 10 }", null);
    try testIfElseExpression("if (1) { 10 }", .{ .int_value = 10 });
    try testIfElseExpression("if (1 < 2) { 10 }", .{ .int_value = 10 });
    try testIfElseExpression("if (1 > 2) { 10 }", null);
    try testIfElseExpression("if (1 > 2) { 10 } else { 20 }", .{ .int_value = 20 });
    try testIfElseExpression("if (1 < 2) { 10 } else { 20 }", .{ .int_value = 10 });
}

fn testIfElseExpression(input: []const u8, expected_value: ?parser.TestValue) !void {
    if (try testEval(input)) |evaluated| {
        switch (evaluated.inner_type) {
            .Integer => try testIntegerObject(evaluated, expected_value.?.int_value),
            else => unreachable,
        }
    } else {
        try std.testing.expectEqual(null, expected_value);
    }
}

test "return statements" {
    try testReturnStatement("return 10;", .{ .int_value = 10 });
    try testReturnStatement("return 10; 9;", .{ .int_value = 10 });
    try testReturnStatement("return 2 * 5; 9;", .{ .int_value = 10 });
    try testReturnStatement("9; return 2 * 5; 9;", .{ .int_value = 10 });
    try testReturnStatement(
        \\if (10 > 1) {
        \\  if (10 > 1) {
        \\    return 10;
        \\  }
        \\
        \\  return 1;
        \\}
    , .{ .int_value = 10 });
}

fn testReturnStatement(input: []const u8, expected_value: parser.TestValue) !void {
    if (try testEval(input)) |evaluated| {
        switch (evaluated.inner_type) {
            .Integer => try testIntegerObject(evaluated, expected_value.int_value),
            else => {
                std.debug.print("no integer object returned\n", .{});
                unreachable;
            }
        }
    } else {
        unreachable;
    }
}

test "error handling" {
    try testErrorHandling(
        "5 + true;",
        "type mismatch: INTEGER + BOOLEAN",
    );
    try testErrorHandling(
        "5 + true; 5;",
        "type mismatch: INTEGER + BOOLEAN",
    );
    try testErrorHandling(
        "-true",
        "unknown operator: -BOOLEAN",
    );
    try testErrorHandling(
        "true + false;",
        "unknown operator: BOOLEAN + BOOLEAN",
    );
    try testErrorHandling(
        "5; true + false; 5",
        "unknown operator: BOOLEAN + BOOLEAN",
    );
    try testErrorHandling(
        "if (10 > 1) { true + false; }",
        "unknown operator: BOOLEAN + BOOLEAN",
    );
    try testErrorHandling(
        \\if (10 > 1) {
        \\  if (10 > 1) {
        \\   return true + false;
        \\ }
        \\
        \\  return 1;
        \\}
        ,
        "unknown operator: BOOLEAN + BOOLEAN",
    );
}

fn testErrorHandling(input: []const u8, expected_error: []const u8) !void {
    if (try testEval(input)) |evaluated| {
        switch (evaluated.inner_type) {
            .Error => try testErrorObject(evaluated, expected_error),
            else => {
                std.debug.print("no error object returned\n", .{});
                unreachable;
            }
        }
    } else {
        unreachable;
    }
}

fn testErrorObject(obj: object.Object, expected_message: []const u8) !void {
    try std.testing.expectEqual(obj.inner_type, .Error);

    const error_object: *object.Error = obj.unwrap(object.Error);

    try std.testing.expectEqualSlices(u8, expected_message, error_object.message);
}
