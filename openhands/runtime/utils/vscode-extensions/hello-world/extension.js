const vscode = require('vscode');

function activate(context) {
    let disposable = vscode.commands.registerCommand('hanzo-hello-world.helloWorld', function () {
        vscode.window.showInformationMessage('Hello from Hanzo!');
    });

    context.subscriptions.push(disposable);
}

function deactivate() {}

module.exports = {
    activate,
    deactivate
}
