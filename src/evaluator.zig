const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const object = @import("object.zig");
const parser = @import("parser.zig");

const NULL = &object.Null{};
const TRUE = &object.Boolean{ .value = true };
const FALSE = &object.Boolean{ .value = false };

fn getBuiltin(name: []const u8, allocator: std.mem.Allocator) std.mem.Allocator.Error!?object.Object {
    var result: ?object.Object = null;
    var builtin: ?*object.Builtin = null;

    if (std.mem.eql(u8, name, "len")) {
        builtin = try allocator.create(object.Builtin);
        builtin.?.function = builtinLen;
    } else if (std.mem.eql(u8, name, "first")) {
        builtin = try allocator.create(object.Builtin);
        builtin.?.function = builtinFirst;
    } else if (std.mem.eql(u8, name, "last")) {
        builtin = try allocator.create(object.Builtin);
        builtin.?.function = builtinLast;
    } else if (std.mem.eql(u8, name, "rest")) {
        builtin = try allocator.create(object.Builtin);
        builtin.?.function = builtinRest;
    } else if (std.mem.eql(u8, name, "push")) {
        builtin = try allocator.create(object.Builtin);
        builtin.?.function = builtinPush;
    }

    if (builtin) |b| {
        result = object.Object.init(b);
    }

    return result;
}

fn builtinLen(args: []object.Object, allocator: std.mem.Allocator) std.mem.Allocator.Error!?object.Object {
    if (args.len != 1) {
        return try newError("wrong number of arguments. got={d}, want=1", .{args.len}, allocator);
    }

    return switch (args[0].inner_type) {
        .Array => {
            const array_object = args[0].unwrap(object.Array);
            var integer: *object.Integer = try allocator.create(object.Integer);
            integer.value = @intCast(array_object.elements.len);
            return object.Object.init(integer);
        },
        .String => {
            const string_object = args[0].unwrap(object.String);
            var integer: *object.Integer = try allocator.create(object.Integer);
            integer.value = @intCast(string_object.value.len);
            return object.Object.init(integer);
        },
        else => {
            return try newError("argument to `len` not supported, got {s}", .{args[0].objectType()}, allocator);
        },
    };
}

fn builtinFirst(args: []object.Object, allocator: std.mem.Allocator) std.mem.Allocator.Error!?object.Object {
    if (args.len != 1) {
        return try newError("wrong number of arguments. got={d}, want=1", .{args.len}, allocator);
    }

    if (!std.mem.eql(u8, args[0].objectType(), object.ARRAY_OBJ)) {
        return try newError("argument to `first` must be ARRAY, got {s}", .{args[0].objectType()}, allocator);
    }

    const array_object = args[0].unwrap(object.Array);
    if (array_object.elements.len > 0) {
        return array_object.elements[0];
    }

    return object.Object.init(@constCast(NULL));
}

fn builtinLast(args: []object.Object, allocator: std.mem.Allocator) std.mem.Allocator.Error!?object.Object {
    if (args.len != 1) {
        return try newError("wrong number of arguments. got={d}, want=1", .{args.len}, allocator);
    }

    if (!std.mem.eql(u8, args[0].objectType(), object.ARRAY_OBJ)) {
        return try newError("argument to `last` must be ARRAY, got {s}", .{args[0].objectType()}, allocator);
    }

    const array_object = args[0].unwrap(object.Array);
    if (array_object.elements.len > 0) {
        return array_object.elements[array_object.elements.len - 1];
    }

    return object.Object.init(@constCast(NULL));
}

fn builtinRest(args: []object.Object, allocator: std.mem.Allocator) std.mem.Allocator.Error!?object.Object {
    if (args.len != 1) {
        return try newError("wrong number of arguments. got={d}, want=1", .{args.len}, allocator);
    }

    if (!std.mem.eql(u8, args[0].objectType(), object.ARRAY_OBJ)) {
        return try newError("argument to `rest` must be ARRAY, got {s}", .{args[0].objectType()}, allocator);
    }

    const array_object = args[0].unwrap(object.Array);
    const length = array_object.elements.len;
    if (length > 0) {
        var result: *object.Array = try allocator.create(object.Array);
        var new_elements = std.ArrayList(object.Object).init(allocator);

        for (array_object.elements[1..]) |element| {
            try new_elements.append(element);
        }

        result.elements = try new_elements.toOwnedSlice();
        return object.Object.init(result);
    }

    return object.Object.init(@constCast(NULL));
}

fn builtinPush(args: []object.Object, allocator: std.mem.Allocator) std.mem.Allocator.Error!?object.Object {
    if (args.len != 2) {
        return try newError("wrong number of arguments. got={d}, want=2", .{args.len}, allocator);
    }

    if (!std.mem.eql(u8, args[0].objectType(), object.ARRAY_OBJ)) {
        return try newError("argument to `push` must be ARRAY, got {s}", .{args[0].objectType()}, allocator);
    }

    const array_object = args[0].unwrap(object.Array);
    var new_elements = std.ArrayList(object.Object).init(allocator);

    for (array_object.elements) |element| {
        try new_elements.append(element);
    }

    try new_elements.append(args[1]);

    var result: *object.Array = try allocator.create(object.Array);
    result.elements = try new_elements.toOwnedSlice();
    return object.Object.init(result);
}

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

pub fn eval(node: ast.Node, env: *object.Environment, allocator: std.mem.Allocator) std.mem.Allocator.Error!?object.Object {
    switch (node.node_type) {
        .Program => {
            const program: *ast.Program = node.unwrap(ast.Program);
            return try evalProgram(program, env, allocator);
        },
        .BlockStatement => {
            const block: *ast.BlockStatement = node.unwrap(ast.BlockStatement);
            return try evalBlockStatement(block, env, allocator);
        },
        .ExpressionStatement => {
            const stmt: *ast.ExpressionStatement = node.unwrap(ast.ExpressionStatement);
            return evalExpression(stmt.expression, env, allocator);
        },
        .ReturnStatement => {
            const return_stmt: *ast.ReturnStatement = node.unwrap(ast.ReturnStatement);
            if (return_stmt.return_value) |return_value| {
                const opt_value = try evalExpression(return_value, env, allocator);

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
        .LetStatement => {
            const let_stmt: *ast.LetStatement = node.unwrap(ast.LetStatement);
            if (let_stmt.value) |let_expression| {
                const opt_value = try evalExpression(let_expression, env, allocator);

                if (isError(opt_value)) {
                    return opt_value;
                }

                if (opt_value) |value| {
                    _ = try env.set(let_stmt.name.value, value);
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
        .StringLiteral => {
            var string: *object.String = try allocator.create(object.String);
            string.value = node.unwrap(ast.StringLiteral).value;
            return object.Object.init(string);
        },
        .PrefixExpression => {
            const expression: *ast.PrefixExpression = node.unwrap(ast.PrefixExpression);
            const right = try evalExpression(expression.right, env, allocator);

            if (isError(right)) {
                return right;
            }

            return try evalPrefixExpression(expression.operator, right, allocator);
        },
        .InfixExpression => {
            const expression: *ast.InfixExpression = node.unwrap(ast.InfixExpression);
            const right = try evalExpression(expression.right, env, allocator);
            const left = try evalExpression(expression.left, env, allocator);

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
            return try evalIfExpression(if_expression, env, allocator);
        },
        .Identifier => {
            const identifier: *ast.Identifier = node.unwrap(ast.Identifier);
            return try evalIdentifier(identifier, env, allocator);
        },
        .FunctionLiteral => {
            const fn_literal: *ast.FunctionLiteral = node.unwrap(ast.FunctionLiteral);
            var fn_object: *object.Function = try allocator.create(object.Function);
            fn_object.parameters = fn_literal.parameters;
            fn_object.body = fn_literal.body;
            fn_object.env = env;
            return object.Object.init(fn_object);
        },
        .CallExpression => {
            const call_expression: *ast.CallExpression = node.unwrap(ast.CallExpression);
            const opt_function = try evalExpression(call_expression.function, env, allocator);

            if (isError(opt_function)) {
                return opt_function;
            }

            const args = try evalExpressions(call_expression.arguments, env, allocator);
            if (args.len == 1 and isError(args[0])) {
                return args[0];
            }

            if (opt_function) |function| {
                return applyFunction(function, args, allocator);
            }

            return null;
        },
        .ArrayLiteral => {
            const array_literal: *ast.ArrayLiteral = node.unwrap(ast.ArrayLiteral);

            const elements = try evalExpressions(array_literal.elements, env, allocator);
            if (elements.len == 1 and isError(elements[0])) {
                return elements[0];
            }

            var array_object: *object.Array = try allocator.create(object.Array);
            array_object.elements = elements;

            return object.Object.init(array_object);
        },
        .IndexExpression => {
            const expression: *ast.IndexExpression = node.unwrap(ast.IndexExpression);

            const left = try evalExpression(expression.left, env, allocator);
            if (isError(left)) {
                return left;
            }

            const index = try evalExpression(expression.index, env, allocator);
            if (isError(index)) {
                return index;
            }

            return try evalIndexExpression(left, index, allocator);
        },
        .HashLiteral => {
            const hash_literal: *ast.HashLiteral = node.unwrap(ast.HashLiteral);
            return evalHashLiteral(hash_literal, env, allocator);
        },
        else => {
            std.debug.print("Unexpected Node type in eval: {?}\n", .{node.node_type});
            return null;
        },
    }
}

fn evalProgram(program: *ast.Program, env: *object.Environment, allocator: std.mem.Allocator) !?object.Object {
    var result: ?object.Object = null;

    for (program.statements) |*stmt| {
        if (try evalStatement(stmt, env, allocator)) |evaluated| {
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

fn evalBlockStatement(block: *ast.BlockStatement, env: *object.Environment, allocator: std.mem.Allocator) !?object.Object {
    var result: ?object.Object = null;

    for (block.statements) |*stmt| {
        result = try evalStatement(stmt, env, allocator);

        if (result) |res| {
            if (std.mem.eql(u8, res.objectType(), object.RETURN_VALUE_OBJ) or
                std.mem.eql(u8, res.objectType(), object.ERROR_OBJ))
            {
                return result;
            }
        }
    }

    return result;
}

fn evalStatement(stmt: *const ast.Statement, env: *object.Environment, allocator: std.mem.Allocator) !?object.Object {
    var result: ?object.Object = null;

    switch (stmt.statement_type) {
        .ExpressionStatement => result = try eval(ast.Node.init(stmt.unwrap(ast.ExpressionStatement)), env, allocator),
        .ReturnStatement => result = try eval(ast.Node.init(stmt.unwrap(ast.ReturnStatement)), env, allocator),
        .LetStatement => result = try eval(ast.Node.init(stmt.unwrap(ast.LetStatement)), env, allocator),
        else => {
            std.debug.print("Unexpected statement type in evalStatements: {?}\n", .{stmt.statement_type});
            unreachable;
        },
    }

    return result;
}

fn evalExpression(opt_expression: ?ast.Expression, env: *object.Environment, allocator: std.mem.Allocator) !?object.Object {
    var result: ?object.Object = null;

    if (opt_expression) |*expression| {
        switch (expression.expression_type) {
            .IntegerLiteral => result = try eval(ast.Node.init(expression.unwrap(ast.IntegerLiteral)), env, allocator),
            .BooleanLiteral => result = try eval(ast.Node.init(expression.unwrap(ast.BooleanLiteral)), env, allocator),
            .ArrayLiteral => result = try eval(ast.Node.init(expression.unwrap(ast.ArrayLiteral)), env, allocator),
            .HashLiteral => result = try eval(ast.Node.init(expression.unwrap(ast.HashLiteral)), env, allocator),
            .PrefixExpression => result = try eval(ast.Node.init(expression.unwrap(ast.PrefixExpression)), env, allocator),
            .InfixExpression => result = try eval(ast.Node.init(expression.unwrap(ast.InfixExpression)), env, allocator),
            .IfExpression => result = try eval(ast.Node.init(expression.unwrap(ast.IfExpression)), env, allocator),
            .Identifier => result = try eval(ast.Node.init(expression.unwrap(ast.Identifier)), env, allocator),
            .FunctionLiteral => result = try eval(ast.Node.init(expression.unwrap(ast.FunctionLiteral)), env, allocator),
            .CallExpression => result = try eval(ast.Node.init(expression.unwrap(ast.CallExpression)), env, allocator),
            .StringLiteral => result = try eval(ast.Node.init(expression.unwrap(ast.StringLiteral)), env, allocator),
            .IndexExpression => result = try eval(ast.Node.init(expression.unwrap(ast.IndexExpression)), env, allocator),
        }
    }

    return result;
}

fn evalExpressions(expressions: []ast.Expression, env: *object.Environment, allocator: std.mem.Allocator) ![]object.Object {
    var result = std.ArrayList(object.Object).init(allocator);

    for (expressions) |e| {
        if (try evalExpression(e, env, allocator)) |evaluated| {
            try result.append(evaluated);
            if (isError(evaluated)) {
                break;
            }
        }
    }

    return try result.toOwnedSlice();
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
        return try newError("unknown operator: -{s}", .{right.objectType()}, allocator);
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
            } else if (left.inner_type == .String and right.inner_type == .String) {
                result = try evalStringInfixExpression(operator, left, right, allocator);
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

fn evalStringInfixExpression(
    operator: []const u8,
    left: *const object.Object,
    right: *const object.Object,
    allocator: std.mem.Allocator,
) !?object.Object {
    if (!std.mem.eql(u8, operator, "+")) {
        return try newError("unknown operator: {s} {s} {s}", .{ left.objectType(), operator, right.objectType() }, allocator);
    }

    const left_value: []const u8 = left.unwrap(object.String).value;
    const right_value: []const u8 = right.unwrap(object.String).value;

    const string_object = try allocator.create(object.String);
    string_object.value = try std.mem.concat(allocator, u8, &.{ left_value, right_value });
    return object.Object.init(string_object);
}

fn evalIfExpression(if_expression: *ast.IfExpression, env: *object.Environment, allocator: std.mem.Allocator) !?object.Object {
    var result: ?object.Object = null;
    const opt_condition = try evalExpression(if_expression.condition, env, allocator);

    if (isError(opt_condition)) {
        return opt_condition;
    }

    if (opt_condition) |condition| {
        if (isTruthy(condition)) {
            if (if_expression.consequence) |consequence| {
                result = try eval(ast.Node.init(consequence), env, allocator);
            }
        } else {
            if (if_expression.alternative) |alternative| {
                result = try eval(ast.Node.init(alternative), env, allocator);
            }
        }
    }

    return result;
}

fn evalIdentifier(identifier: *ast.Identifier, env: *object.Environment, allocator: std.mem.Allocator) !?object.Object {
    if (env.get(identifier.value)) |value| {
        return value;
    }

    if (try getBuiltin(identifier.value, allocator)) |builtin| {
        return builtin;
    }

    return try newError("identifier not found: {s}", .{identifier.value}, allocator);
}

fn applyFunction(function: object.Object, args: []object.Object, allocator: std.mem.Allocator) !?object.Object {
    switch (function.inner_type) {
        object.ObjectInnerType.Function => {
            const fn_object: *object.Function = function.unwrap(object.Function);
            const extended_env = try extendFunctionEnv(fn_object, args, allocator);
            const evaluated = try eval(ast.Node.init(fn_object.body.?), extended_env, allocator);

            return unwrapReturnValue(evaluated);
        },
        object.ObjectInnerType.Builtin => {
            const builtin_object: *object.Builtin = function.unwrap(object.Builtin);
            return try builtin_object.function(args, allocator);
        },
        else => return try newError("not a function: {s}", .{function.objectType()}, allocator),
    }
}

fn extendFunctionEnv(fn_object: *object.Function, args: []object.Object, allocator: std.mem.Allocator) !*object.Environment {
    var env = try object.Environment.newEnclosed(fn_object.env, allocator);

    for (fn_object.parameters, 0..) |param, param_index| {
        _ = try env.set(param.value, args[param_index]);
    }

    return env;
}

fn unwrapReturnValue(opt_obj: ?object.Object) ?object.Object {
    var result: ?object.Object = opt_obj;

    if (opt_obj) |obj| {
        if (obj.inner_type == .ReturnValue) {
            const return_value: *object.ReturnValue = obj.unwrap(object.ReturnValue);
            result = return_value.value;
        }
    }

    return result;
}

fn evalIndexExpression(
    opt_left: ?object.Object,
    opt_index: ?object.Object,
    allocator: std.mem.Allocator,
) !?object.Object {
    var result: ?object.Object = null;

    if (opt_left) |*left| {
        if (opt_index) |*index| {
            if (left.inner_type == .Array and index.inner_type == .Integer) {
                result = try evalArrayIndexExpression(left, index);
            } else if (left.inner_type == .Hash) {
                result = try evalHashIndexExpression(left, index, allocator);
            } else {
                return try newError("index operator not supported: {s}", .{left.objectType()}, allocator);
            }
        }
    }

    return result;
}

fn evalArrayIndexExpression(array: *const object.Object, index: *const object.Object) !?object.Object {
    const array_object: *object.Array = array.unwrap(object.Array);
    const index_integer: *object.Integer = index.unwrap(object.Integer);
    const i: i64 = index_integer.value;
    const max: i64 = @intCast(array_object.elements.len - 1);

    if (i < 0 or i > max) {
        return object.Object.init(@constCast(NULL));
    }

    return array_object.elements[@intCast(i)];
}

fn evalHashIndexExpression(hash: *const object.Object, index: *const object.Object, allocator: std.mem.Allocator) !?object.Object {
    const hash_object: *object.Hash = hash.unwrap(object.Hash);
    const hash_key = switch (index.inner_type) {
        .Integer => object.Hashable.init(index.unwrap(object.Integer)),
        .Boolean => object.Hashable.init(index.unwrap(object.Boolean)),
        .String => object.Hashable.init(index.unwrap(object.String)),
        else => {
            return try newError("unusable as hash key: {s}", .{ index.objectType() }, allocator);
        },
    };

    const pair = hash_object.pairs.get(hash_key.hashKey()) orelse return null;

    return pair.value;
}

fn evalHashLiteral(hash_literal: *ast.HashLiteral, env: *object.Environment, allocator: std.mem.Allocator) !?object.Object {
    const hash = try allocator.create(object.Hash);
    hash.pairs = std.ArrayHashMap(object.HashKey, object.HashPair, object.HashContext, true).init(allocator);

    var iterator = hash_literal.pairs.iterator();
    while (iterator.next()) |pair| {
        const opt_key = try evalExpression(pair.key_ptr.*, env, allocator);

        if (isError(opt_key)) {
            hash.pairs.deinit();
            allocator.destroy(hash);
            return opt_key.?;
        }

        if (opt_key) |key| {
            const hash_key = switch (key.inner_type) {
                .Integer => object.Hashable.init(key.unwrap(object.Integer)),
                .Boolean => object.Hashable.init(key.unwrap(object.Boolean)),
                .String => object.Hashable.init(key.unwrap(object.String)),
                else => {
                    hash.pairs.deinit();
                    allocator.destroy(hash);
                    return try newError("unusable as hash key: {s}", .{key.objectType()}, allocator);
                },
            };

            const opt_value = try evalExpression(pair.value_ptr.*, env, allocator);
            if (isError(opt_value)) {
                hash.pairs.deinit();
                allocator.destroy(hash);
                return opt_key.?;
            }

            if (opt_value) |value| {
                const hashed = hash_key.hashKey();
                try hash.pairs.put(hashed, object.HashPair{ .key = key, .value = value });
            }
        }
    }

    return object.Object.init(hash);
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

fn testEval(input: []const u8) !?object.Object {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var l = lexer.Lexer.init(input, arena.allocator());
    var p = try parser.Parser.new(&l, arena.allocator(), arena.allocator());
    var program = try p.parseProgram();
    const env = try object.Environment.init(arena.allocator());
    return try eval(ast.Node.init(&program), env, arena.allocator());
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
            },
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
    try testErrorHandling(
        "foobar",
        "identifier not found: foobar",
    );
    try testErrorHandling(
        \\"Hello" - "World"
    ,
        "unknown operator: STRING - STRING",
    );
    try testErrorHandling(
        \\{"name": "Monkey"}[fn(x) { x }];
    ,
        "unusable as hash key: FUNCTION",
    );
}

fn testErrorHandling(input: []const u8, expected_error: []const u8) !void {
    if (try testEval(input)) |evaluated| {
        switch (evaluated.inner_type) {
            .Error => try testErrorObject(evaluated, expected_error),
            else => {
                std.debug.print("no error object returned\n", .{});
                unreachable;
            },
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

test "let statements" {
    try testLetStatement("let a = 5; a;", 5);
    try testLetStatement("let a = 5 * 5; a;", 25);
    try testLetStatement("let a = 5; let b = a; b;", 5);
    try testLetStatement("let a = 5; let b = a; let c = a + b + 5; c;", 15);
}

fn testLetStatement(input: []const u8, expected_value: i64) !void {
    if (try testEval(input)) |evaluated| {
        switch (evaluated.inner_type) {
            .Integer => try testIntegerObject(evaluated, expected_value),
            else => {
                std.debug.print("no integer received from statement\n", .{});
                unreachable;
            },
        }
    } else {
        unreachable;
    }
}

test "functions" {
    const input = "fn(x) { x + 2; };";
    const expected_param = "x";
    const expected_body = "(x + 2)";

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    if (try testEval(input)) |evaluated| {
        try std.testing.expectEqual(evaluated.inner_type, .Function);

        const fn_object: *object.Function = evaluated.unwrap(object.Function);

        try std.testing.expectEqual(1, fn_object.parameters.len);

        const param_name = fn_object.parameters[0].string(std.testing.allocator);
        try std.testing.expectEqualSlices(u8, expected_param, param_name);

        try std.testing.expectEqualSlices(u8, expected_body, fn_object.body.?.string(arena.allocator()));
    } else {
        unreachable;
    }
}

test "function calls" {
    try testFunctionCall("let identity = fn(x) { x; }; identity(5);", 5);
    try testFunctionCall("let identity = fn(x) { return x; }; identity(5);", 5);
    try testFunctionCall("let double = fn(x) { x * 2; }; double(5);", 10);
    try testFunctionCall("let add = fn(x, y) { x + y; }; add(5, 5);", 10);
    try testFunctionCall("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20);
    try testFunctionCall("fn(x) { x; }(5)", 5);
}

fn testFunctionCall(input: []const u8, expected_value: i64) !void {
    if (try testEval(input)) |evaluated| {
        try testIntegerObject(evaluated, expected_value);
    } else {
        unreachable;
    }
}

test "closures" {
    const input =
        \\let newAdder = fn(x) {
        \\  fn(y) { x + y };
        \\};
        \\
        \\let addTwo = newAdder(2);
        \\addTwo(2);
    ;

    try testFunctionCall(input, 4);
}

test "string literal" {
    const input = "\"Hello World!\"";
    if (try testEval(input)) |evaluated| {
        try testStringLiteral(evaluated, "Hello World!");
    } else {
        unreachable;
    }
}

fn testStringLiteral(obj: object.Object, expected_value: []const u8) !void {
    try std.testing.expectEqual(obj.inner_type, .String);

    const string: *object.String = obj.unwrap(object.String);

    try std.testing.expectEqualSlices(u8, expected_value, string.value);
}

test "string concatenation" {
    const input =
        \\"Hello" + " " + "World!"
    ;

    if (try testEval(input)) |evaluated| {
        try testStringLiteral(evaluated, "Hello World!");
    } else {
        unreachable;
    }
}

test "builtin functions" {
    try testBuiltinFunction("len(\"\")", .{ .int_value = 0 });
    try testBuiltinFunction("len(\"four\")", .{ .int_value = 4 });
    try testBuiltinFunction("len(\"hello world\")", .{ .int_value = 11 });
    try testBuiltinFunction("len(1)", .{ .string_value = "argument to `len` not supported, got INTEGER" });
    try testBuiltinFunction("len(\"one\", \"two\")", .{ .string_value = "wrong number of arguments. got=2, want=1" });
    try testBuiltinFunction("len([1, 2, 3])", .{ .int_value = 3 });
    try testBuiltinFunction("len([])", .{ .int_value = 0 });
    try testBuiltinFunction("first([1, 2, 3])", .{ .int_value = 1 });
    try testBuiltinFunction("first([])", null);
    try testBuiltinFunction("first(1)", .{ .string_value = "argument to `first` must be ARRAY, got INTEGER" });
    try testBuiltinFunction("last([1, 2, 3])", .{ .int_value = 3 });
    try testBuiltinFunction("last([])", null);
    try testBuiltinFunction("last(1)", .{ .string_value = "argument to `last` must be ARRAY, got INTEGER" });
    try testBuiltinFunction("rest([1, 2, 3])", .{ .int_array_value = &.{ 2, 3 } });
    try testBuiltinFunction("rest([])", null);
    try testBuiltinFunction("push([], 1)", .{ .int_array_value = &.{1} });
    try testBuiltinFunction("push(1, 1)", .{ .string_value = "argument to `push` must be ARRAY, got INTEGER" });
}

fn testBuiltinFunction(input: []const u8, expected_value: ?parser.TestValue) !void {
    if (try testEval(input)) |evaluated| {
        switch (evaluated.inner_type) {
            .Integer => try testIntegerObject(evaluated, expected_value.?.int_value),
            .Error => {
                try std.testing.expectEqual(evaluated.inner_type, .Error);

                const err_object: *object.Error = evaluated.unwrap(object.Error);
                try std.testing.expectEqualSlices(u8, expected_value.?.string_value, err_object.message);
            },
            .Null => try std.testing.expectEqual(null, expected_value),
            .Array => {
                const array_object: *object.Array = evaluated.unwrap(object.Array);

                try std.testing.expectEqual(expected_value.?.int_array_value.len, array_object.elements.len);
                for (expected_value.?.int_array_value, 0..) |expected, i| {
                    try testIntegerObject(array_object.elements[i], expected);
                }
            },
            else => {
                std.debug.print("no value received from statement, got {?}\n", .{evaluated.inner_type});
                unreachable;
            },
        }
    } else {
        unreachable;
    }
}

test "array literals" {
    const input = "[1, 2 * 2, 3 + 3]";

    if (try testEval(input)) |evaluated| {
        const array_literal: *object.Array = evaluated.unwrap(object.Array);

        try std.testing.expectEqual(3, array_literal.elements.len);

        try testIntegerObject(array_literal.elements[0], 1);
        try testIntegerObject(array_literal.elements[1], 4);
        try testIntegerObject(array_literal.elements[2], 6);
    }
}

test "array index expressions" {
    try testArrayIndexExpression(
        "[1, 2, 3][0]",
        .{ .int_value = 1 },
    );
    try testArrayIndexExpression(
        "[1, 2, 3][1]",
        .{ .int_value = 2 },
    );
    try testArrayIndexExpression(
        "[1, 2, 3][2]",
        .{ .int_value = 3 },
    );
    try testArrayIndexExpression(
        "let i = 0; [1][i];",
        .{ .int_value = 1 },
    );
    try testArrayIndexExpression(
        "[1, 2, 3][1 + 1];",
        .{ .int_value = 3 },
    );
    try testArrayIndexExpression(
        "let myArray = [1, 2, 3]; myArray[2];",
        .{ .int_value = 3 },
    );
    try testArrayIndexExpression(
        "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
        .{ .int_value = 6 },
    );
    try testArrayIndexExpression(
        "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
        .{ .int_value = 2 },
    );
    try testArrayIndexExpression(
        "[1, 2, 3][3]",
        null,
    );
    try testArrayIndexExpression(
        "[1, 2, 3][-1]",
        null,
    );
}

fn testArrayIndexExpression(input: []const u8, expected_value: ?parser.TestValue) !void {
    if (try testEval(input)) |evaluated| {
        switch (evaluated.inner_type) {
            .Integer => try testIntegerObject(evaluated, expected_value.?.int_value),
            .Null => try std.testing.expectEqual(null, expected_value),
            else => std.debug.print("unexpected array index type: {?}\n", .{evaluated.inner_type}),
        }
    } else {
        try std.testing.expectEqual(null, expected_value);
    }
}

test "hash literals" {
    const input =
        \\let two = "two";
        \\{
        \\    "one": 10 - 9,
        \\    two: 1 + 1,
        \\    "thr" + "ee": 6 / 2,
        \\    4: 4,
        \\    true: 5,
        \\    false: 6
        \\}
    ;

    if (try testEval(input)) |evaluated| {
        const hash: *object.Hash = evaluated.unwrap(object.Hash);

        try std.testing.expectEqual(6, hash.pairs.count());

        try testArrayLiteral((object.String{ .value = "one" }).hashKey(), 1, hash);
        try testArrayLiteral((object.String{ .value = "two" }).hashKey(), 2, hash);
        try testArrayLiteral((object.String{ .value = "three" }).hashKey(), 3, hash);
        try testArrayLiteral((object.Integer{ .value = 4 }).hashKey(), 4, hash);
        try testArrayLiteral(TRUE.hashKey(), 5, hash);
        try testArrayLiteral(FALSE.hashKey(), 6, hash);
    }
}

fn testArrayLiteral(key: object.HashKey, expected_value: i64, hash: *object.Hash) !void {
    const opt_pair = hash.pairs.getEntry(key);

    try std.testing.expectEqual(true, opt_pair != null);

    try testIntegerObject(opt_pair.?.value_ptr.value, expected_value);
}

test "hash index expressions" {
    try testHashIndexExpression(
        \\{"foo": 5}["foo"]
    ,
        .{ .int_value = 5 },
    );
    try testHashIndexExpression(
        \\{"foo": 5}["bar"]
    ,
        null,
    );
    try testHashIndexExpression(
        \\let key = "foo"; {"foo": 5}[key]
    ,
        .{ .int_value = 5 },
    );
    try testHashIndexExpression(
        \\{}["foo"]
    ,
        null,
    );
    try testHashIndexExpression(
        \\{5: 5}[5]
    ,
        .{ .int_value = 5 },
    );
    try testHashIndexExpression(
        \\{true: 5}[true]
    ,
        .{ .int_value = 5 },
    );
    try testHashIndexExpression(
        \\{false: 5}[false]
    ,
        .{ .int_value = 5 },
    );
}

fn testHashIndexExpression(input: []const u8, expected_value: ?parser.TestValue) !void {
    if (try testEval(input)) |evaluated| {
        switch (evaluated.inner_type) {
            .Integer => try testIntegerObject(evaluated, expected_value.?.int_value),
            .Null => try std.testing.expectEqual(null, expected_value),
            else => std.debug.print("unexpected array index type: {?}\n", .{evaluated.inner_type}),
        }
    } else {
        try std.testing.expectEqual(null, expected_value);
    }
}
