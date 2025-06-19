const std = @import("std");

pub const Error = error{ScanError};
const makeError = @import("main.zig").makeError;

pub const TokenType = enum {
    COMMA,
    COLUMN,
    DOT,
    MINUS,
    PLUS,
    ASTERISK,
    EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    BANG,
    BANG_EQUAL,
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_SQUARED_BRACKET,
    RIGHT_SQUARED_BRACKET,
    LEFT_ARROW,
    RIGHT_ARROW,
    START_PROC,
    END_PROC,
    START_FUNC,
    END_FUNC,
    DO,
    END_DO,
    IF,
    THEN,
    ELSE,
    END_IF,
    WHILE,
    FOR,
    HIGH,
    LOW,
    AND,
    OR,
    NOT,
    DIV,
    MOD,
    RETURN,
    STRUCTURE,
    TYPE,
    VOID,
    STRING,
    CHAR,
    INT,
    FLOAT,
    BOOLEAN,
    TRUE,
    FALSE,
    NIL,
    DATA,
    RESULT,
    IDENTIFIER,
    EOF,
};

const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ .START_PROC, "debproc" },
    .{ .END_PROC, "finproc" },
    .{ .START_FUNC, "debfonc" },
    .{ .END_FUNC, "finfonc" },
    .{ .DO, "faire" },
    .{ .END_DO, "finfaire" },
    .{ .IF, "si" },
    .{ .THEN, "alors" },
    .{ .ELSE, "sinon" },
    .{ .END_IF, "finsi" },
    .{ .WHILE, "tantque" },
    .{ .FOR, "pour" },
    .{ .HIGH, "haut" },
    .{ .LOW, "bas" },
    .{ .AND, "et" },
    .{ .OU, "ou" },
    .{ .NOT, "non" },
    .{ .DIV, "div" },
    .{ .MOD, "mod" },
    .{ .RETURN, "retourner" },
    .{ .STRUCTURE, "structure" },
    .{ .TYPE, "type" },
    .{ .VOID, "vide" },
    .{ .STRING, "chaine" },
    .{ .CHAR, "caratère" },
    .{ .INT, "entier" },
    .{ .FLOAT, "réel" },
    .{ .BOOLEAN, "booléen" },
    .{ .TRUE, "vrai" },
    .{ .FALSE, "faux" },
    .{ .NIL, "nil" },
    .{ .DATA, "d" },
    .{ .RESULT, "r" },
});

pub const Literal = union(enum) {
    string: []const u8,
    char: u8,
    integer: u64,
    float: f64,
    boolean: bool,
};

pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    literal: ?Literal,
    line: u16,

    pub fn create(token_type: TokenType, lexeme: []const u8, literal: ?Literal, line: u16) Token {
        return .{
            .token_type = token_type,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
        };
    }
};

pub const Scanner = struct {
    source: []const u8,
    allocator: std.mem.Allocator,
    tokens: std.ArrayList(Token),
    start: u64,
    current: u64,
    line: u16,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Scanner {
        return .{
            .source = source,
            .allocator = allocator,
            .tokens = std.ArrayList(Token).init(allocator),
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    pub fn deinit(self: *Scanner) void {
        self.tokens.deinit();
    }

    pub fn scanTokens(self: *Scanner) ![]const Token {
        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken();
        }

        try self.tokens.append(Token{ .lexeme = "", .token_type = .EOF, .line = self.line, .literal = null });
        return self.tokens.items;
    }

    fn scanToken(self: *Scanner) !void {
        const c = self.advance();

        try switch (c) {
            ',' => self.addToken(.COMMA, null),
            ';' => self.addToken(.COLUMN, null),
            '.' => self.addToken(.DOT, null),
            '-' => self.addToken(.MINUS, null),
            '+' => self.addToken(.PLUS, null),
            '*' => self.addToken(.ASTERISK, null),
            '=' => self.addToken(.EQUAL, null),
            '<' => self.addToken(if (self.match('=')) .LESS_EQUAL else .LESS, null),
            '>' => self.addToken(if (self.match('=')) .GREATER_EQUAL else .GREATER, null),
            '!' => self.addToken(if (self.match('=')) .BANG_EQUAL else .BANG, null),
            '(' => self.addToken(.LEFT_PAREN, null),
            ')' => self.addToken(.RIGHT_PAREN, null),
            '[' => self.addToken(.LEFT_SQUARED_BRACKET, null),
            ']' => self.addToken(.RIGHT_SQUARED_BRACKET, null),
            ' ', '\t', '\r' => return,
            '\n' => {
                self.line += 1;
                return;
            },
            '\'' => self.char(),
            '"' => self.string(),
            else => unreachable,
        };
    }

    fn addToken(self: *Scanner, token_type: TokenType, literal: ?Literal) !void {
        const text = self.source[self.start..self.current];
        try self.tokens.append(Token.create(token_type, text, literal, self.line));
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *Scanner) u8 {
        if (!self.isAtEnd()) self.current += 1;
        return self.source[self.current - 1];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0 else return self.source[self.current];
    }

    fn char(self: *Scanner) !void {
        if (!self.isAtEnd()) {
            if (self.peek() != '\'') {
                _ = self.advance();

                if (self.isAtEnd()) {
                    try makeError(
                        self.line,
                        &[_]u8{self.source[self.current - 1]},
                        &[_][]const u8{"Caractère non terminé."},
                    );
                    return error.ScanError;
                }
                if (self.peek() != '\'') {
                    try makeError(
                        self.line,
                        &[_]u8{self.source[self.current - 1]},
                        &[_][]const u8{"Caractère non valide."},
                    );
                    return error.ScanError;
                } else {
                    try self.addToken(.CHAR, .{ .char = self.source[self.current - 1] });
                    _ = self.advance();
                    return;
                }
            } else {
                try self.addToken(.CHAR, .{
                    .char = 170,
                });
                _ = self.advance();
                return;
            }
        } else {
            try makeError(
                self.line,
                &[_]u8{self.source[self.current - 1]},
                &[_][]const u8{"Caractère non valide."},
            );
            return error.ScanError;
        }
    }

    fn string(self: *Scanner) !void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            try makeError(self.line, &[_]u8{self.source[self.current - 1]}, &[_][]const u8{"Chaine non terminée."});
            return error.ScanError;
        }

        _ = self.advance();

        const value = self.source[self.start + 1 .. self.current - 1];
        try self.addToken(.STRING, .{ .string = value });
    }
};

const testing = std.testing;

test "Scanner: void input" {
    const allocator = testing.allocator;
    var scanner = Scanner.init(allocator, "");
    defer scanner.deinit();

    const tokens = try scanner.scanTokens();

    try testing.expectEqual(@as(usize, 1), tokens.len);
    try testing.expectEqual(TokenType.EOF, tokens[0].token_type);
}

test "Scanner: simple tokens" {
    const allocator = testing.allocator;
    var scanner = Scanner.init(allocator, ",;.-+*=[]()");
    defer scanner.deinit();

    const tokens = try scanner.scanTokens();

    try testing.expectEqual(@as(usize, 12), tokens.len);
    try testing.expectEqual(TokenType.COMMA, tokens[0].token_type);
    try testing.expectEqual(TokenType.COLUMN, tokens[1].token_type);
    try testing.expectEqual(TokenType.DOT, tokens[2].token_type);
    try testing.expectEqual(TokenType.MINUS, tokens[3].token_type);
    try testing.expectEqual(TokenType.PLUS, tokens[4].token_type);
    try testing.expectEqual(TokenType.ASTERISK, tokens[5].token_type);
    try testing.expectEqual(TokenType.EQUAL, tokens[6].token_type);
    try testing.expectEqual(TokenType.LEFT_SQUARED_BRACKET, tokens[7].token_type);
    try testing.expectEqual(TokenType.RIGHT_SQUARED_BRACKET, tokens[8].token_type);
    try testing.expectEqual(TokenType.LEFT_PAREN, tokens[9].token_type);
    try testing.expectEqual(TokenType.RIGHT_PAREN, tokens[10].token_type);
    try testing.expectEqual(TokenType.EOF, tokens[11].token_type);
}

test "Scanner: multiple symbol tokens" {
    const allocator = testing.allocator;
    var scanner = Scanner.init(allocator, ">=<=!==!>");
    defer scanner.deinit();

    const tokens = try scanner.scanTokens();

    try testing.expectEqual(@as(usize, 7), tokens.len);
    try testing.expectEqual(TokenType.GREATER_EQUAL, tokens[0].token_type);
    try testing.expectEqual(TokenType.LESS_EQUAL, tokens[1].token_type);
    try testing.expectEqual(TokenType.BANG_EQUAL, tokens[2].token_type);
    try testing.expectEqual(TokenType.EQUAL, tokens[3].token_type);
    try testing.expectEqual(TokenType.BANG, tokens[4].token_type);
    try testing.expectEqual(TokenType.GREATER, tokens[5].token_type);
    try testing.expectEqual(TokenType.EOF, tokens[6].token_type);
}

test "Scanner: skipped character" {
    const allocator = testing.allocator;

    {
        const test_cases = [_]struct { input: []const u8, expected_type: TokenType }{
            .{ .input = " ", .expected_type = TokenType.EOF },
            .{ .input = "\t", .expected_type = TokenType.EOF },
            .{ .input = "\r", .expected_type = TokenType.EOF },
            .{ .input = "\t\r ", .expected_type = TokenType.EOF },
        };

        for (test_cases) |tc| {
            var scanner = Scanner.init(allocator, tc.input);
            defer scanner.deinit();

            const tokens = try scanner.scanTokens();

            try testing.expectEqual(@as(usize, 1), tokens.len);
            try testing.expectEqual(tc.expected_type, tokens[0].token_type);
        }
    }

    {
        var scanner = Scanner.init(allocator, "\r<=\t! =");
        defer scanner.deinit();

        const tokens = try scanner.scanTokens();

        try testing.expectEqual(@as(usize, 4), tokens.len);
        try testing.expectEqual(TokenType.LESS_EQUAL, tokens[0].token_type);
        try testing.expectEqual(TokenType.BANG, tokens[1].token_type);
        try testing.expectEqual(TokenType.EQUAL, tokens[2].token_type);
        try testing.expectEqual(TokenType.EOF, tokens[3].token_type);
    }
}

test "Scanner: character literal" {
    const allocator = testing.allocator;

    {
        var scanner = Scanner.init(allocator, "'a'';''1'");
        defer scanner.deinit();

        const tokens = try scanner.scanTokens();

        const expectations = [_]struct { type: TokenType, value: u8 }{
            .{ .type = TokenType.CHAR, .value = 'a' },
            .{ .type = TokenType.CHAR, .value = ';' },
            .{ .type = TokenType.CHAR, .value = '1' },
        };

        for (expectations, 0..) |expected, i| {
            try testing.expectEqual(expected.type, tokens[i].token_type);
            try testing.expectEqual(expected.value, tokens[i].literal.?.char);
        }
    }

    {
        const test_cases = [_]struct {
            source: []const u8,
            expected_type: TokenType,
            expected_literal_value: u8,
        }{
            .{ .source = "''", .expected_type = TokenType.CHAR, .expected_literal_value = 170 },
            .{ .source = "' '", .expected_type = TokenType.CHAR, .expected_literal_value = ' ' },
        };

        for (test_cases) |tc| {
            var scanner = Scanner.init(allocator, tc.source);
            defer scanner.deinit();

            const tokens = try scanner.scanTokens();

            try testing.expectEqual(@as(usize, 2), tokens.len);
            try testing.expectEqual(tc.expected_type, tokens[0].token_type);
            try testing.expectEqual(tc.expected_literal_value, tokens[0].literal.?.char);
            try testing.expectEqual(TokenType.EOF, tokens[1].token_type);
        }
    }

    {
        const sources = [_][]const u8{
            "'",
            "'a",
            "'ab'",
        };

        for (sources) |source| {
            var scanner = Scanner.init(allocator, source);
            defer scanner.deinit();

            try testing.expectError(error.ScanError, scanner.scanTokens());
        }
    }
}

test "Scanner: string literal" {
    const allocator = testing.allocator;

    {
        const test_cases = [_]struct {
            source: []const u8,
            expected_type: TokenType,
            expected_literal_value: []const u8,
        }{
            .{ .source = "\"\"", .expected_type = TokenType.STRING, .expected_literal_value = "" },
            .{ .source = "\"a\"", .expected_type = TokenType.STRING, .expected_literal_value = "a" },
            .{ .source = "\"abcd\"", .expected_type = TokenType.STRING, .expected_literal_value = "abcd" },
            .{ .source = "\"';,\"", .expected_type = TokenType.STRING, .expected_literal_value = "';," },
        };

        for (test_cases) |tc| {
            var scanner = Scanner.init(allocator, tc.source);
            defer scanner.deinit();

            const tokens = try scanner.scanTokens();

            try testing.expectEqual(TokenType.STRING, tokens[0].token_type);
            try testing.expectEqualStrings(tc.expected_literal_value, tokens[0].literal.?.string);
        }
    }

    {
        var scanner = Scanner.init(allocator, "\"ab\"\"cd\"");
        defer scanner.deinit();

        const tokens = try scanner.scanTokens();

        try testing.expectEqual(@as(usize, 3), tokens.len);
        try testing.expectEqual(TokenType.STRING, tokens[0].token_type);
        try testing.expectEqualStrings("ab", tokens[0].literal.?.string);
        try testing.expectEqual(TokenType.STRING, tokens[1].token_type);
        try testing.expectEqualStrings("cd", tokens[1].literal.?.string);
    }

    {
        const source = "\"\nabcd\n\"";
        const expected_literal_value =
            \\
            \\abcd
            \\
        ;

        var scanner = Scanner.init(allocator, source);
        defer scanner.deinit();

        const tokens = try scanner.scanTokens();

        try testing.expectEqual(TokenType.STRING, tokens[0].token_type);
        try testing.expectEqualStrings(expected_literal_value, tokens[0].literal.?.string);
    }

    {
        const source = "\"aeijoiefj";
        var scanner = Scanner.init(allocator, source);
        defer scanner.deinit();

        try testing.expectError(error.ScanError, scanner.scanTokens());
    }
}
