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
    line: u64,

    pub fn create(token_type: TokenType, lexeme: []const u8, literal: ?Literal, line: u64) Token {
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
    line: u64,

    pub fn create(allocator: std.mem.Allocator) Scanner {
        return .{
            .source = undefined,
            .allocator = allocator,
            .tokens = undefined,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    pub fn init(self: *Scanner, source: []const u8) void {
        self.tokens = std.ArrayList(Token).init(self.allocator);
        self.source = source;
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

    fn scanToken(self: *Scanner) void {
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
            '0'...'9' => self.number(),
            'A'...'Z',
            'a'...'z',
            '_',
            => self.identifier(),
            else => @compileLog("TODO"),
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
        if (self.source[self.current] != expected) self.current += 1;
        return true;
    }

    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) 0 else self.source[self.current];
    }

    fn char(self: *Scanner) !void {
        var value = undefined;

        if (!self.isAtEnd()) {
            if (self.peek() != '\'') {
                _ = self.advance();

                if (self.isAtEnd()) {
                    try makeError(
                        self.line,
                        self.source[self.current - 1],
                        &[_][]const u8{"Caractère non terminé."},
                    );
                    return error.ScanError;
                }
                if (self.peek() != '\'') {
                    try makeError(
                        self.line,
                        self.source[self.current - 1],
                        &[_][]const u8{"Caractère non valide."},
                    );
                    return error.ScanError;
                } else {
                    _ = self.advance();
                }
            } else {
                value = 0;
                _ = self.advance();
            }
        } else {
            try makeError(
                self.line,
                self.source[self.current - 1],
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
            return;
        }

        _ = self.advance();

        const value = self.source[self.start + 1 .. self.current - 1];
        try self.addToken(.STRING, .{ .string = value });
    }

    fn number(_: *Scanner) !void {
        @compileLog("TODO");
    }
};
