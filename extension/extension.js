const vscode = require("vscode");
const fs = require("fs");
const path = require("path");

const KEYWORDS = [
  "def",
  "return",
  "if",
  "then",
  "else",
  "for",
  "do",
  "end",
  "repeat",
  "while",
  "case",
  "is",
  "import",
  "as",
  "let",
  "adt",
  "iterable",
  "iterative",
  "fmap",
  "foreign",
  "input",
  "writefile",
  "true",
  "false",
];

const BUILTIN_FUNCTIONS = [
  "map",
  "filter",
  "foldl",
  "foldr",
  "sum",
  "product",
  "reverse",
  "take",
  "drop",
  "head",
  "tail",
  "isEmpty",
  "any",
  "all",
  "find",
  "zip",
  "zipWith",
  "range",
  "min",
  "max",
  "print",
  "typeOf",
  "length",
  "to_int",
  "index_of",
];

const SNIPPETS = [
  {
    label: "import ... as ...",
    insertText: "import ${1:module/path} as ${2:alias}",
    detail: "Import alias",
  },
  {
    label: "case-is",
    insertText: "case ${1:value} is ${2:Pattern} = ${3:expr} | _ = ${4:expr} end",
    detail: "Case expression",
  },
  {
    label: "adt iterative",
    insertText: "adt iterative ${1:Type} = ${2:Ctor} ${3:FieldType}",
    detail: "Iterative ADT",
  },
];

const moduleIndexCache = new Map();

function activate(context) {
  const diagnostics = vscode.languages.createDiagnosticCollection("bern");
  context.subscriptions.push(diagnostics);

  context.subscriptions.push(
    vscode.languages.registerCompletionItemProvider(
      "bern",
      {
        provideCompletionItems(document, position) {
          return provideCompletionItems(document, position);
        },
      },
      ":"
    )
  );

  context.subscriptions.push(
    vscode.languages.registerHoverProvider("bern", {
      provideHover(document, position) {
        const range = document.getWordRangeAtPosition(position);
        if (!range) {
          return undefined;
        }
        const word = document.getText(range);
        if (!KEYWORDS.includes(word)) {
          return undefined;
        }
        return new vscode.Hover(`Bern keyword: \`${word}\``);
      },
    })
  );

  context.subscriptions.push(
    vscode.languages.registerDefinitionProvider("bern", {
      provideDefinition(document, position) {
        return provideDefinition(document, position);
      },
    })
  );

  context.subscriptions.push(
    vscode.languages.registerReferenceProvider("bern", {
      provideReferences(document, position, options) {
        return provideReferences(document, position, options);
      },
    })
  );

  context.subscriptions.push(
    vscode.languages.registerDocumentSymbolProvider("bern", {
      provideDocumentSymbols(document) {
        return provideDocumentSymbols(document);
      },
    })
  );

  const refreshDiagnostics = (document) => {
    if (document.languageId !== "bern") {
      return;
    }
    updateDiagnostics(document, diagnostics);
  };

  if (vscode.window.activeTextEditor) {
    refreshDiagnostics(vscode.window.activeTextEditor.document);
  }

  context.subscriptions.push(
    vscode.workspace.onDidOpenTextDocument(refreshDiagnostics)
  );
  context.subscriptions.push(
    vscode.workspace.onDidChangeTextDocument((event) =>
      refreshDiagnostics(event.document)
    )
  );
  context.subscriptions.push(
    vscode.workspace.onDidSaveTextDocument(refreshDiagnostics)
  );
  context.subscriptions.push(
    vscode.workspace.onDidCloseTextDocument((document) =>
      diagnostics.delete(document.uri)
    )
  );
}

function deactivate() {}

function provideCompletionItems(document, position) {
  const context = buildDocumentContext(document);
  const linePrefix = document
    .lineAt(position.line)
    .text.slice(0, position.character);
  const qualifiedMatch = linePrefix.match(
    /([A-Za-z_][A-Za-z0-9_\/]*)\:([A-Za-z_][A-Za-z0-9_]*)?$/
  );

  if (qualifiedMatch) {
    const qualifier = qualifiedMatch[1];
    const moduleIndex = context.importedIndicesByQualifier.get(qualifier);
    if (!moduleIndex) {
      return [];
    }
    return moduleIndex.symbols.map((symbol) => {
      const item = new vscode.CompletionItem(
        symbol,
        vscode.CompletionItemKind.Function
      );
      item.detail = `Imported from ${qualifier}`;
      return item;
    });
  }

  const byLabel = new Map();
  const push = (item) => {
    if (!byLabel.has(item.label)) {
      byLabel.set(item.label, item);
    }
  };

  for (const keyword of KEYWORDS) {
    const item = new vscode.CompletionItem(
      keyword,
      vscode.CompletionItemKind.Keyword
    );
    item.detail = "Bern keyword";
    push(item);
  }

  for (const builtin of BUILTIN_FUNCTIONS) {
    const item = new vscode.CompletionItem(
      builtin,
      vscode.CompletionItemKind.Function
    );
    item.detail = "Core function";
    push(item);
  }

  for (const snippet of SNIPPETS) {
    const item = new vscode.CompletionItem(
      snippet.label,
      vscode.CompletionItemKind.Snippet
    );
    item.insertText = new vscode.SnippetString(snippet.insertText);
    item.detail = snippet.detail;
    push(item);
  }

  for (const symbol of context.localIndex.symbols) {
    const item = new vscode.CompletionItem(
      symbol,
      vscode.CompletionItemKind.Variable
    );
    item.detail = "Local symbol";
    push(item);
  }

  for (const [symbol, owners] of context.symbolOwners.entries()) {
    if (owners.size === 1) {
      const owner = Array.from(owners)[0];
      const item = new vscode.CompletionItem(
        symbol,
        vscode.CompletionItemKind.Function
      );
      item.detail = `Imported from ${owner}`;
      push(item);
      continue;
    }
    for (const owner of owners) {
      const item = new vscode.CompletionItem(
        `${owner}:${symbol}`,
        vscode.CompletionItemKind.Function
      );
      item.detail = `Ambiguous symbol '${symbol}'`;
      push(item);
    }
  }

  return Array.from(byLabel.values());
}

function provideDefinition(document, position) {
  const token = getSymbolTokenAtPosition(document, position);
  if (!token) {
    return undefined;
  }

  const context = buildDocumentContext(document);
  if (token.qualifier) {
    const moduleIndex = context.importedIndicesByQualifier.get(token.qualifier);
    if (!moduleIndex) {
      return undefined;
    }
    const defs = moduleIndex.definitions.get(token.symbol) || [];
    return defs.map((entry) => entry.location);
  }

  const localDefs = context.localIndex.definitions.get(token.symbol) || [];
  if (localDefs.length > 0) {
    return localDefs.map((entry) => entry.location);
  }

  const owners = context.symbolOwners.get(token.symbol);
  if (!owners || owners.size === 0) {
    return undefined;
  }

  const locations = [];
  for (const owner of owners) {
    const moduleIndex = context.importedIndicesByQualifier.get(owner);
    if (!moduleIndex) {
      continue;
    }
    const defs = moduleIndex.definitions.get(token.symbol) || [];
    for (const def of defs) {
      locations.push(def.location);
    }
  }

  return locations.length > 0 ? locations : undefined;
}

function provideReferences(document, position, options) {
  const token = getSymbolTokenAtPosition(document, position);
  if (!token) {
    return [];
  }

  if (token.qualifier) {
    return findQualifiedReferences(document, token.qualifier, token.symbol);
  }

  const references = findUnqualifiedReferences(document, token.symbol);
  if (options.includeDeclaration) {
    return references;
  }

  const localIndex = extractModuleIndex(document.getText(), document.uri);
  const declarations = localIndex.definitions.get(token.symbol) || [];
  const declarationKeys = new Set(
    declarations.map((entry) => locationKey(entry.location))
  );

  return references.filter(
    (location) => !declarationKeys.has(locationKey(location))
  );
}

function provideDocumentSymbols(document) {
  const localIndex = extractModuleIndex(document.getText(), document.uri);
  const symbols = [];

  for (const [name, defs] of localIndex.definitions.entries()) {
    if (defs.length === 0) {
      continue;
    }
    const first = defs[0];
    const symbol = new vscode.DocumentSymbol(
      name,
      first.kind,
      toVscodeSymbolKind(first.kind),
      first.location.range,
      first.location.range
    );
    symbols.push(symbol);
  }

  symbols.sort((a, b) => a.range.start.line - b.range.start.line);
  return symbols;
}

function updateDiagnostics(document, diagnostics) {
  const source = document.getText();
  const context = buildDocumentContext(document);
  const allDiagnostics = [];

  const ambiguousCalls = /\b([A-Za-z_][A-Za-z0-9_]*)\s*\(/g;
  let callMatch = ambiguousCalls.exec(source);
  while (callMatch) {
    const name = callMatch[1];
    const owners = context.symbolOwners.get(name);
    const index = callMatch.index;
    const before = source.slice(Math.max(0, index - 20), index);

    if (owners && owners.size > 1 && !/\:\s*$/.test(before) && !isDefCall(source, index)) {
      const ownerList = Array.from(owners).map((owner) => `${owner}:${name}(...)`);
      const start = document.positionAt(index);
      const end = document.positionAt(index + name.length);
      const range = new vscode.Range(start, end);
      allDiagnostics.push(
        new vscode.Diagnostic(
          range,
          `Ambiguous imported symbol '${name}'. Use ${ownerList.join(" or ")}.`,
          vscode.DiagnosticSeverity.Warning
        )
      );
    }

    callMatch = ambiguousCalls.exec(source);
  }

  diagnostics.set(document.uri, allDiagnostics);
}

function isDefCall(source, index) {
  const lineStart = source.lastIndexOf("\n", index - 1) + 1;
  const prefix = source.slice(lineStart, index);
  return /^\s*def\s+$/.test(prefix);
}

function buildDocumentContext(document) {
  const source = document.getText();
  const imports = parseImports(source);
  const localIndex = extractModuleIndex(source, document.uri);
  const symbolOwners = new Map();
  const importedIndicesByQualifier = new Map();
  const importsByQualifier = new Map();

  for (const imp of imports) {
    const modulePath = resolveModulePath(imp.moduleName, document);
    if (!modulePath) {
      continue;
    }

    const moduleIndex = readModuleIndex(modulePath);
    importedIndicesByQualifier.set(imp.qualifier, moduleIndex);
    importsByQualifier.set(imp.qualifier, imp.moduleName);

    for (const symbol of moduleIndex.symbols) {
      if (!symbolOwners.has(symbol)) {
        symbolOwners.set(symbol, new Set());
      }
      symbolOwners.get(symbol).add(imp.qualifier);
    }
  }

  return {
    localIndex,
    importsByQualifier,
    importedIndicesByQualifier,
    symbolOwners,
  };
}

function parseImports(source) {
  const imports = [];
  const importRegex =
    /^\s*import\s+([A-Za-z_][A-Za-z0-9_\/]*)(?:\s+as\s+([A-Za-z_][A-Za-z0-9_]*))?/gm;
  let match = importRegex.exec(source);
  while (match) {
    imports.push({
      moduleName: match[1],
      qualifier: match[2] || match[1],
    });
    match = importRegex.exec(source);
  }
  return imports;
}

function extractModuleIndex(source, uri) {
  const definitions = new Map();
  const symbols = new Set();
  const lineOffsets = buildLineOffsets(source);

  const addDefinition = (name, kind, startOffset, endOffset) => {
    if (!name || KEYWORDS.includes(name)) {
      return;
    }
    symbols.add(name);
    const location = createLocation(uri, lineOffsets, startOffset, endOffset);
    if (!definitions.has(name)) {
      definitions.set(name, []);
    }
    definitions.get(name).push({ location, kind });
  };

  const addFromRegex = (regex, kind) => {
    let match = regex.exec(source);
    while (match) {
      const name = match[1];
      const relStart = match[0].indexOf(name);
      if (relStart >= 0) {
        const start = match.index + relStart;
        addDefinition(name, kind, start, start + name.length);
      }
      match = regex.exec(source);
    }
  };

  addFromRegex(/^\s*def\s+([A-Za-z_][A-Za-z0-9_]*)\b/gm, "function");
  addFromRegex(/^\s*foreign\s+([A-Za-z_][A-Za-z0-9_]*)\b/gm, "foreign");
  addFromRegex(/^\s*([A-Za-z_][A-Za-z0-9_]*)\s*(?::=|=)/gm, "variable");

  const adtRegex =
    /^\s*adt(?:\s+(?:iterable|iterative))?\s+([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.+)$/gm;
  let adtMatch = adtRegex.exec(source);
  while (adtMatch) {
    const typeName = adtMatch[1];
    const rhs = adtMatch[2];
    const full = adtMatch[0];
    const relTypeStart = full.indexOf(typeName);
    if (relTypeStart >= 0) {
      const typeStart = adtMatch.index + relTypeStart;
      addDefinition(typeName, "type", typeStart, typeStart + typeName.length);
    }

    const rhsAbsStart = adtMatch.index + full.indexOf(rhs);
    const parts = rhs.split("|");
    let searchFrom = 0;
    for (const part of parts) {
      const partIndex = rhs.indexOf(part, searchFrom);
      if (partIndex === -1) {
        continue;
      }
      searchFrom = partIndex + part.length;
      const trimmedLeft = part.match(/^\s*/);
      const trimLen = trimmedLeft ? trimmedLeft[0].length : 0;
      const ctorPart = part.slice(trimLen);
      const ctorMatch = ctorPart.match(/^([A-Z][A-Za-z0-9_]*)/);
      if (!ctorMatch) {
        continue;
      }
      const ctorName = ctorMatch[1];
      const ctorStart = rhsAbsStart + partIndex + trimLen;
      addDefinition(
        ctorName,
        "constructor",
        ctorStart,
        ctorStart + ctorName.length
      );
    }

    adtMatch = adtRegex.exec(source);
  }

  return {
    symbols: Array.from(symbols.values()),
    definitions,
  };
}

function resolveModulePath(moduleName, document) {
  const folder = vscode.workspace.getWorkspaceFolder(document.uri);
  if (!folder) {
    return null;
  }

  const root = folder.uri.fsPath;
  const localFile = path.join(root, `${moduleName}.brn`);
  const libFile = path.join(root, "lib", `${moduleName}.brn`);
  const relativeFile = path.join(path.dirname(document.uri.fsPath), `${moduleName}.brn`);

  const candidates = [localFile, libFile, relativeFile];
  for (const candidate of candidates) {
    if (fs.existsSync(candidate)) {
      return candidate;
    }
  }
  return null;
}

function readModuleIndex(modulePath) {
  try {
    const stat = fs.statSync(modulePath);
    const cacheHit = moduleIndexCache.get(modulePath);
    if (cacheHit && cacheHit.mtimeMs === stat.mtimeMs) {
      return cacheHit.index;
    }

    const text = fs.readFileSync(modulePath, "utf8");
    const uri = vscode.Uri.file(modulePath);
    const index = extractModuleIndex(text, uri);
    moduleIndexCache.set(modulePath, { mtimeMs: stat.mtimeMs, index });
    return index;
  } catch {
    return { symbols: [], definitions: new Map() };
  }
}

function getSymbolTokenAtPosition(document, position) {
  const range = document.getWordRangeAtPosition(
    position,
    /[A-Za-z_][A-Za-z0-9_]*/
  );
  if (!range) {
    return null;
  }

  const symbol = document.getText(range);
  const lineText = document.lineAt(position.line).text;
  const prefix = lineText.slice(0, range.start.character);
  const qualifierMatch = prefix.match(/([A-Za-z_][A-Za-z0-9_\/]*)\s*:\s*$/);

  return {
    symbol,
    qualifier: qualifierMatch ? qualifierMatch[1] : null,
    range,
  };
}

function findQualifiedReferences(document, qualifier, symbol) {
  const source = document.getText();
  const lineOffsets = buildLineOffsets(source);
  const regex = new RegExp(
    `\\b${escapeRegExp(qualifier)}\\s*:\\s*${escapeRegExp(symbol)}\\b`,
    "g"
  );
  const refs = [];

  let match = regex.exec(source);
  while (match) {
    const symbolOffset = match.index + match[0].lastIndexOf(symbol);
    refs.push(
      createLocation(
        document.uri,
        lineOffsets,
        symbolOffset,
        symbolOffset + symbol.length
      )
    );
    match = regex.exec(source);
  }
  return refs;
}

function findUnqualifiedReferences(document, symbol) {
  const source = document.getText();
  const lineOffsets = buildLineOffsets(source);
  const regex = new RegExp(`\\b${escapeRegExp(symbol)}\\b`, "g");
  const refs = [];

  let match = regex.exec(source);
  while (match) {
    const before = source.slice(Math.max(0, match.index - 20), match.index);
    if (!/\:\s*$/.test(before)) {
      refs.push(
        createLocation(
          document.uri,
          lineOffsets,
          match.index,
          match.index + symbol.length
        )
      );
    }
    match = regex.exec(source);
  }
  return refs;
}

function buildLineOffsets(source) {
  const offsets = [0];
  for (let i = 0; i < source.length; i += 1) {
    if (source[i] === "\n") {
      offsets.push(i + 1);
    }
  }
  return offsets;
}

function offsetToPosition(offset, lineOffsets) {
  let low = 0;
  let high = lineOffsets.length - 1;
  while (low <= high) {
    const mid = Math.floor((low + high) / 2);
    if (lineOffsets[mid] <= offset) {
      if (mid === lineOffsets.length - 1 || lineOffsets[mid + 1] > offset) {
        return new vscode.Position(mid, offset - lineOffsets[mid]);
      }
      low = mid + 1;
    } else {
      high = mid - 1;
    }
  }
  return new vscode.Position(0, offset);
}

function createLocation(uri, lineOffsets, startOffset, endOffset) {
  const start = offsetToPosition(startOffset, lineOffsets);
  const end = offsetToPosition(endOffset, lineOffsets);
  return new vscode.Location(uri, new vscode.Range(start, end));
}

function toVscodeSymbolKind(kind) {
  if (kind === "function" || kind === "foreign") {
    return vscode.SymbolKind.Function;
  }
  if (kind === "variable") {
    return vscode.SymbolKind.Variable;
  }
  if (kind === "type") {
    return vscode.SymbolKind.Class;
  }
  if (kind === "constructor") {
    return vscode.SymbolKind.Constructor;
  }
  return vscode.SymbolKind.Variable;
}

function locationKey(location) {
  return `${location.uri.toString()}:${location.range.start.line}:${location.range.start.character}`;
}

function escapeRegExp(text) {
  return text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

module.exports = {
  activate,
  deactivate,
};
