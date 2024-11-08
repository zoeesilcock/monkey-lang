const std = @import("std");

pub const ObjectType: type = []const u8;

pub const INTEGER_OBJ: ObjectType = "INTEGER";
pub const BOOLEAN_OBJ: ObjectType = "BOOLEAN";
pub const NULL_OBJ: ObjectType = "NULL";
pub const RETURN_VALUE_OBJ = "RETURN_VALUE";
pub const ERROR_OBJ = "ERROR_OBJ";

pub const ObjectInnerType = enum {
    Integer,
    Boolean,
    Null,
    ReturnValue,
    Error,
};

pub const Object = struct {
    ptr: *anyopaque,
    inner_type: ObjectInnerType,
    vtab: *const VTab,

    const VTab = struct {
        objectType: *const fn(ptr: *anyopaque) ObjectType,
        inspect: *const fn (ptr: *anyopaque, allocator: std.mem.Allocator) []const u8,
    };

    pub fn objectType(self: Object) ObjectType {
        return self.vtab.objectType(self.ptr);
    }

    pub fn inspect(self: Object, allocator: std.mem.Allocator) []const u8 {
        return self.vtab.inspect(self.ptr, allocator);
    }

    pub fn unwrap(self: Object, comptime T: type) *T {
        return @ptrCast(@alignCast(self.ptr));
    }

    pub fn init(obj: anytype) Object {
        const Ptr = @TypeOf(obj);
        const PtrInfo = @typeInfo(Ptr);

        std.debug.assert(PtrInfo == .Pointer);
        std.debug.assert(PtrInfo.Pointer.size == .One);
        std.debug.assert(@typeInfo(PtrInfo.Pointer.child) == .Struct);

        const inner_type: ObjectInnerType = switch(Ptr) {
            *Integer => ObjectInnerType.Integer,
            *Boolean => ObjectInnerType.Boolean,
            *Null => ObjectInnerType.Null,
            *ReturnValue => ObjectInnerType.ReturnValue,
            *Error => ObjectInnerType.Error,
            else => {
                std.debug.print("Unsupported Object type: {?}\n", .{ Ptr });
                unreachable;
            },
        };

        const impl = struct {
            fn objectType(ptr: *anyopaque) ObjectType {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return self.objectType();
            }
            fn inspect(ptr: *anyopaque, allocator: std.mem.Allocator) []const u8 {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return self.inspect(allocator);
            }
        };

        return .{
            .ptr = obj,
            .inner_type = inner_type,
            .vtab = &.{
                .objectType = impl.objectType,
                .inspect = impl.inspect,
            },
        };
    }
};

pub const Integer = struct {
    value: i64,

    pub fn objectType(self: Integer) ObjectType {
        _ = self;
        return INTEGER_OBJ;
    }

    pub fn inspect(self: Integer, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{d}", .{ self.value }) catch "";
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn objectType(self: Boolean) ObjectType {
        _ = self;
        return BOOLEAN_OBJ;
    }

    pub fn inspect(self: Boolean, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s}", .{ if (self.value) "true" else "false" }) catch "";
    }
};

pub const Null = struct {
    pub fn objectType(self: Null) ObjectType {
        _ = self;
        return NULL_OBJ;
    }

    pub fn inspect(self: Null, allocator: std.mem.Allocator) []const u8 {
        _ = self;
        _ = allocator;
        return "null";
    }
};

pub const ReturnValue = struct {
    value: Object,

    pub fn objectType(self: ReturnValue) ObjectType {
        _ = self;
        return RETURN_VALUE_OBJ;
    }

    pub fn inspect(self: ReturnValue, allocator: std.mem.Allocator) []const u8 {
        return self.value.inspect(allocator);
    }
};

pub const Error = struct {
    message: []const u8,

    pub fn objectType(self: Error) ObjectType {
        _ = self;
        return ERROR_OBJ;
    }

    pub fn inspect(self: Error, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "ERROR: {s}", .{ self.message }) catch "";
    }
};
