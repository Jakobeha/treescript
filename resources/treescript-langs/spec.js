"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var nodes = {
    ArrayPattern: [
        "elements",
    ],
    RestElement: [
        "argument",
    ],
    AssignmentPattern: [
        "left",
        "right",
    ],
    ObjectPattern: [
        "properties",
    ],
    ThisExpression: [],
    Identifier: [
        "name",
    ],
    Literal: [
        "value",
        "raw",
        "regex?",
    ],
    ArrayExpression: [
        "elements",
    ],
    ObjectExpression: [
        "properties",
    ],
    Property: [
        "key",
        "computed",
        "value",
        "kind",
        "method",
        "shorthand",
    ],
    FunctionExpression: [
        "id",
        "params",
        "body",
        "generator",
        "async",
        "expression",
    ],
    ArrowFunctionExpression: [
        "id",
        "params",
        "body",
        "generator",
        "async",
        "expression",
    ],
    ClassExpression: [
        "id",
        "superClass",
        "body",
    ],
    ClassBody: [
        "body",
    ],
    MethodDefinition: [
        "key",
        "computed",
        "value",
        "kind",
        "static",
    ],
    TaggedTemplateExpression: [
        "readonly tag",
        "readonly quasi",
    ],
    TemplateElement: [
        "value",
        "tail",
    ],
    TemplateLiteral: [
        "quasis",
        "expressions",
    ],
    MemberExpression: [
        "computed",
        "object",
        "property",
    ],
    Super: [],
    MetaProperty: [
        "meta",
        "property",
    ],
    CallExpression: [
        "callee",
        "arguments",
    ],
    NewExpression: [
        "callee",
        "arguments",
    ],
    Import: [],
    SpreadElement: [
        "argument",
    ],
    UpdateExpression: [
        "operator",
        "argument",
        "prefix",
    ],
    AwaitExpression: [
        "argument",
    ],
    UnaryExpression: [
        "operator",
        "argument",
        "prefix",
    ],
    BinaryExpression: [
        "operator",
        "left",
        "right",
    ],
    LogicalExpression: [
        "operator",
        "left",
        "right",
    ],
    ConditionalExpression: [
        "test",
        "consequent",
        "alternate",
    ],
    YieldExpression: [
        "argument",
        "delegate",
    ],
    AssignmentExpression: [
        "operator",
        "left",
        "right",
    ],
    SequenceExpression: [
        "expressions",
    ],
    BlockStatement: [
        "body",
    ],
    BreakStatement: [
        "label",
    ],
    ClassDeclaration: [
        "id",
        "superClass",
        "body",
    ],
    ContinueStatement: [
        "label",
    ],
    DebuggerStatement: [],
    DoWhileStatement: [
        "body",
        "test",
    ],
    EmptyStatement: [],
    ExpressionStatement: [
        "expression",
        "directive?",
    ],
    ForStatement: [
        "init",
        "test",
        "update",
        "body",
    ],
    ForInStatement: [
        "left",
        "right",
        "body",
        "each",
    ],
    ForOfStatement: [
        "left",
        "right",
        "body",
    ],
    FunctionDeclaration: [
        "id",
        "params",
        "body",
        "generator",
        "async",
        "expression",
    ],
    IfStatement: [
        "test",
        "consequent",
        "alternate?",
    ],
    LabeledStatement: [
        "label",
        "body",
    ],
    ReturnStatement: [
        "argument",
    ],
    SwitchStatement: [
        "discriminant",
        "cases",
    ],
    SwitchCase: [
        "test",
        "consequent",
    ],
    ThrowStatement: [
        "argument",
    ],
    TryStatement: [
        "block",
        "handler",
        "finalizer",
    ],
    CatchClause: [
        "param",
        "body",
    ],
    VariableDeclaration: [
        "declarations",
        "kind",
    ],
    VariableDeclarator: [
        "id",
        "init",
    ],
    WhileStatement: [
        "test",
        "body",
    ],
    WithStatement: [
        "object",
        "body",
    ],
    Program: [
        "sourceType",
        "body",
    ],
    ImportDeclaration: [
        "specifiers",
        "source",
    ],
    ImportSpecifier: [
        "local",
        "imported?",
    ],
    ImportDefaultSpecifier: [
        "local",
        "imported?",
    ],
    ImportNamespaceSpecifier: [
        "local",
        "imported?",
    ],
    ExportAllDeclaration: [
        "source",
    ],
    ExportDefaultDeclaration: [
        "declaration",
    ],
    ExportNamedDeclaration: [
        "declaration",
        "specifiers",
        "source",
    ],
    ExportSpecifier: [
        "exported",
        "local",
    ],
};
exports.default = {
    nodes: nodes
};
