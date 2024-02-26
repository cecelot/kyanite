import * as vscode from "vscode";

export function activate(context: vscode.ExtensionContext) {
  console.log("kyanite-vscode: extension activated");

  const disposable = vscode.commands.registerCommand(
    "kyanite-vscode.helloWorld",
    () => {
      vscode.window.showInformationMessage("Hello World from kyanite-vscode!");
    }
  );

  context.subscriptions.push(disposable);
}

export function deactivate() {}
