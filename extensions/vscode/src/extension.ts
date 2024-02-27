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
      console.log(token);
      const span = token.span;
      const range = new vscode.Range(
        new vscode.Position(span.line, span.start),
        new vscode.Position(span.line, span.end)
      );
      tokensBuilder.push(range, token.kind, token.modifiers);
    }
    return tokensBuilder.build();
  },
};

const selector = { language: "kyanite", scheme: "file" };

export function deactivate() {}
