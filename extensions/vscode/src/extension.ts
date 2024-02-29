import * as vscode from "vscode";
import { highlights } from "@kyanite/highlights";

export function activate(context: vscode.ExtensionContext) {
  const semanticTokensProvider =
    vscode.languages.registerDocumentSemanticTokensProvider(
      selector,
      provider,
      legend
    );
  context.subscriptions.push(semanticTokensProvider);
}

const tokenTypes = ["keyword"];
const tokenModifiers: string[] = [];
const legend = new vscode.SemanticTokensLegend(tokenTypes, tokenModifiers);

const provider: vscode.DocumentSemanticTokensProvider = {
  provideDocumentSemanticTokens(
    document: vscode.TextDocument
  ): vscode.ProviderResult<vscode.SemanticTokens> {
    const tokensBuilder = new vscode.SemanticTokensBuilder(legend);
    const tokens = highlights({
      code: document.getText(),
      filename: document.fileName,
    });
    for (const token of tokens) {
      const span = token.span;
      const range = new vscode.Range(
        new vscode.Position(span.line, span.start),
        new vscode.Position(span.line, span.end)
      );
      const tokenType = `${token.kind
        .slice(0, 1)
        .toLowerCase()}${token.kind.slice(1)}`;
      const tokenModifiers = token.modifiers.map((modifier) =>
        modifier.toLowerCase()
      );
      tokensBuilder.push(range, tokenType, tokenModifiers);
    }
    return tokensBuilder.build();
  },
};

const selector = { language: "kyanite", scheme: "file" };

export function deactivate() {}
